;;; pilish-jsonl.el --- JSONL session reading and tree projection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pilish

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Disk-data APIs over pi session JSONL files: reading entries, building
;; the raw nested session tree, projecting that tree to the flat display
;; dialect the tree browser consumes, formatting tool-call previews,
;; computing tree-navigation targets and byte-preserving rewritten line
;; orders (non-chain lines stay in physical order; selected chains use
;; logical parent order), and discovering session files on disk (sessions
;; root, munged per-cwd directories, canonical shared metadata scans).
;; This ports pi's session-manager getTree (plus label folding), the RPC tree
;; projection, navigateTree's
;; leaf rule, core format-tool-call, and config/session-dir path munging.
;; Depends only on core; does not manage UI/session state or processes.
;; Reading uses temporary buffers.  Resumable scans expose caller-owned
;; parser state whose decoded scratch buffer must be explicitly closed.
;;
;; Normalization conventions (JSON in, plists out):
;;
;; - JSON null decodes to the :null keyword (`json-parse-string') and is
;;   treated exactly like an absent value: nullable fields go through
;;   `pilish--jsonl-entry-parent-id' or core's
;;   `pilish--normalize-string-or-null', and this module never
;;   emits :null itself.  Empty strings in nullable metadata likewise
;;   normalize as absent.
;; - JSON arrays are vectors, never lists.
;; - Numbers pass through with their zero-ness intact: presence checks
;;   use `numberp' (never truthiness), so an offset of 0 or a limit of 0
;;   still counts as present.  For read's offset and limit the reference
;;   compares against undefined rather than truthiness, so JSON null
;;   counts as present there too and coerces like JS null
;;   (`pilish--jsonl-arg-number').
;; - Key orders are canonical.  Raw nodes are (:entry :children :label
;;   :labelTimestamp) with the label pair only when a label is set.
;;   Projected nodes start with the base (:id :parentId :timestamp
;;   :label :children) followed by type-specific payload keys;
;;   :parentId and assistant :stopReason are always present (possibly
;;   nil), every other optional key only when non-nil.  Children are
;;   always vectors.
;;
;; Deliberate deviations from the TypeScript reference, documented
;; rather than fixed:
;;
;; - Malformed or blank JSONL lines are skipped silently while reading a
;;   session file.
;; - Children sort by timestamp STRING comparison while the reference
;;   parses dates numerically; equivalent for the uniform UTC ISO-8601
;;   stamps pi writes, and both keep file order on ties.
;; - Session files older than version 3 are read as-is, without pi's
;;   on-load migrations: hookMessage roles project as unknown and
;;   version 1 files (no ids or parent ids) build as flat root lists.
;; - Null or malformed message payloads (JSON null :message, null
;;   content, null content blocks) degrade to empty previews instead
;;   of throwing like the reference would; session files are parsed
;;   without validation and old or hand-edited files can carry them.
;; - Historical Pi versions could persist duplicated headers and entries
;;   while forking.  Repeated entries with the same nonempty id and equal
;;   content canonicalize to one occurrence.  Differing repeats use Pi's
;;   deterministic later-wins entry while carrying an explicit ambiguity
;;   diagnostic.  Ambiguous ids are unaddressable/unmarked, and ancestry
;;   that would cross one fails closed; unrelated history remains useful.
;;   Nil/empty legacy ids stay visible but unaddressable and do not
;;   participate in duplicate detection.
;; - `pilish--jsonl-shorten-path' replaces the HOME (or
;;   USERPROFILE) prefix blindly: /home/tes also matches /home/tester/x.
;;   Faithful port of the upstream quirk.
;; - The reference truncates strings by UTF-16 code units; Elisp
;;   truncates by characters, so previews of astral-plane text can
;;   differ in length.  Accepted; session text is not surrogate-paired
;;   in practice.
;; - `pilish-jsonl-read-session-info' reports :modified from
;;   the file mtime instead of the newest message timestamp (one stat
;;   versus a per-line compare).  Append-only files agree, and the
;;   Navigation rewrites arguably make mtime more correct.
;; - `pilish-jsonl-navigation-target's :current-p describes whether its
;;   COMPUTED target position is already current, but only when both
;;   positions resolve positively.  This includes the useful historical-
;;   user re-edit case where the prompt's parent is current.
;;   `pilish-jsonl-current-projected-id' separately preserves pi's
;;   selected-entry identity check: a current user prompt is a no-op
;;   before the user-message rewind rule, and trailing projection-away
;;   bookkeeping resolves back to that prompt.
;;
;; All tree traversals are iterative (explicit stacks, reversed
;; pre-order bottom-up builds); real session trees reach thousands of
;; entries deep.  The single recursion is
;; `pilish--jsonl-encode-args', which normalizes nested JSON
;; tool arguments (two or three levels by construction, never session
;; depth).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pilish-core)

;;;; Entry Normalization

(defun pilish--jsonl-entry-parent-id (entry)
  "Return the normalized parent id of ENTRY: a string, or nil.
JSON null, absent, and non-string values all read as nil."
  (pilish--normalize-string-or-null (plist-get entry :parentId)))

(defun pilish--jsonl-filtered-entry-p (type)
  "Return non-nil when an entry of TYPE is filtered from projection.
Label, session_info, and custom entries are bookkeeping: they vanish and
their children are promoted to the nearest visible ancestor."
  (member type '("label" "session_info" "custom")))

(defun pilish--jsonl-canonicalize-entries (entries)
  "Canonicalize duplicate addressable ids in ENTRIES.
Return (:entries VECTOR :ambiguousIds LIST :repeated-p BOOL).  For each
nonempty string id, retain only its last physical occurrence, matching
Pi's index.  Equal repeats are benign historical duplication; differing
repeats additionally enter :ambiguousIds in first-conflict order.
Nil/empty/malformed legacy ids are all retained and never ambiguous."
  (let ((missing (make-symbol "missing"))
        (last-entry (make-hash-table :test #'equal))
        (last-index (make-hash-table :test #'equal))
        (ambiguous (make-hash-table :test #'equal))
        (ambiguous-ids nil)
        (repeated-p nil))
    (dotimes (i (length entries))
      (let* ((entry (aref entries i))
             (id (pilish--normalize-string-or-null
                  (plist-get entry :id))))
        (when id
          (let ((previous (gethash id last-entry missing)))
            (unless (eq previous missing)
              (setq repeated-p t)
              (when (and (not (equal previous entry))
                         (not (gethash id ambiguous)))
                (puthash id t ambiguous)
                (push id ambiguous-ids)))
            (puthash id entry last-entry)
            (puthash id i last-index)))))
    (let (canonical)
      (dotimes (i (length entries))
        (let* ((entry (aref entries i))
               (id (pilish--normalize-string-or-null
                    (plist-get entry :id))))
          (when (or (null id) (= i (gethash id last-index)))
            (push entry canonical))))
      (list :entries (vconcat (nreverse canonical))
            :ambiguousIds (nreverse ambiguous-ids)
            :repeated-p repeated-p))))

(defun pilish--jsonl-duplicate-diagnostic (ambiguous-ids)
  "Return an honest diagnostic for AMBIGUOUS-IDS, or nil."
  (when ambiguous-ids
    (format "ambiguous duplicate entry id%s: %s"
            (if (cdr ambiguous-ids) "s" "")
            (mapconcat (lambda (id) (format "%S" id))
                       ambiguous-ids ", "))))

(defun pilish--jsonl-add-duplicate-metadata (result ambiguous-ids)
  "Append duplicate metadata for AMBIGUOUS-IDS to RESULT."
  (if ambiguous-ids
      (append result
              (list :diagnostic
                    (pilish--jsonl-duplicate-diagnostic ambiguous-ids)
                    :ambiguousIds ambiguous-ids))
    result))

(defun pilish--jsonl-merge-ids (&rest id-lists)
  "Return stable equal-deduplicated ids from ID-LISTS."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (ids id-lists)
      (dolist (id ids)
        (unless (gethash id seen)
          (puthash id t seen)
          (push id result))))
    (nreverse result)))

(defun pilish--jsonl-raw-tree-entries (tree)
  "Return raw TREE entries in iterative pre-order."
  (let ((stack (append tree nil))
        (entries nil))
    (while stack
      (let* ((node (pop stack))
             (children (plist-get node :children)))
        (push (plist-get node :entry) entries)
        (when (vectorp children)
          (setq stack (append (append children nil) stack)))))
    (vconcat (nreverse entries))))

(defun pilish--jsonl-raw-tree-ambiguous-ids (tree)
  "Return stable addressable ids marked :ambiguousId in raw TREE.
Builder output is already structurally unique, so this metadata is the
only way the established two-argument builder-to-projector composition
can retain a differing-duplicate diagnostic.  Traversal is iterative."
  (let ((seen (make-hash-table :test #'equal))
        (stack (append tree nil))
        (ids nil))
    (while stack
      (let* ((node (pop stack))
             (entry (plist-get node :entry))
             (id (pilish--normalize-string-or-null
                  (plist-get entry :id)))
             (children (plist-get node :children)))
        (when (and id
                   (plist-get node :ambiguousId)
                   (not (gethash id seen)))
          (puthash id t seen)
          (push id ids))
        (when (vectorp children)
          (setq stack (append (append children nil) stack)))))
    (nreverse ids)))

(defun pilish-jsonl-tree-ids-unique-p (tree &optional raw-p)
  "Return non-nil when nested TREE has no duplicate addressable ids.
TREE is projected when RAW-P is nil; otherwise nodes have the raw
`:entry' shape accepted by `pilish-jsonl-project-tree'.  Traversal is
iterative.  Repeated absent/invalid legacy ids are deliberately ignored,
so only repeated nonempty strings fail this defensive check.  Canonical
builder/projection results are structurally unique and carry ambiguity
metadata separately."
  (let ((seen (make-hash-table :test #'equal))
        (stack (append tree nil))
        (unique t))
    (while (and unique stack)
      (let* ((node (pop stack))
             (entry (and raw-p (plist-get node :entry)))
             (id (pilish--normalize-string-or-null
                  (plist-get (or entry node) :id)))
             (children (plist-get node :children)))
        (when id
          (if (gethash id seen)
              (setq unique nil)
            (puthash id t seen)))
        (when (vectorp children)
          (dotimes (i (length children))
            (push (aref children i) stack)))))
    unique))

;;;; Reading Session Files

(defun pilish--jsonl-parse-session-header (line)
  "Return LINE parsed as the session header plist, or nil.
LINE is the decoded text of a candidate session file's first nonblank
line; this is the one shared rule deciding whether a file is a pi
session file at all: the line must parse as a JSON object whose
top-level \"type\" is \"session\".  Whitespace-only lines before it and
a leading UTF-8 BOM are the callers' business — pi's reader trims
both away before its header check: insert-file-contents strips the
BOM while decoding, and
`pilish-jsonl-navigation-lines' strips a copy for this
check only."
  (let ((data (pilish--parse-json-line line)))
    (when (and (consp data)
               (equal (plist-get data :type) "session"))
      data)))

(defun pilish-jsonl-read-session-header (path)
  "Read PATH's first complete nonblank session header, or return nil.
Grow the byte prefix only until that line is complete (or EOF), never
reading a whole history merely because its header exceeds 4096 bytes.
The first nonblank line follows `pilish-jsonl-read-file's validation."
  (condition-case nil
      (with-temp-buffer
        (let ((limit 4096) size complete)
          (while (not complete)
            (erase-buffer)
            (insert-file-contents path nil 0 limit)
            (goto-char (point-min))
            (while (and (not (eobp)) (looking-at-p "[ \t\r]*$"))
              (forward-line 1))
            (if (save-excursion (search-forward "\n" nil t))
                (setq complete t)
              ;; A decoded character count cannot establish whether a
              ;; byte-limited UTF-8 read reached the end of the file.
              (setq size (or size (file-attribute-size (file-attributes path))))
              (if (< limit size)
                  (setq limit (min size (* 2 limit)))
                (setq complete t))))
          (unless (eobp)
            (pilish--jsonl-parse-session-header
             (buffer-substring-no-properties (point) (line-end-position))))))
    (error nil)))

(defun pilish-jsonl-read-file (path)
  "Read the session file at PATH.
Return a plist with :path, :header, :entries, :leafId, and :name, or
nil when PATH is missing, empty, or its first nonblank line is not
the session header (`pilish--jsonl-parse-session-header's
rule).  Like pi's reader, whitespace-only leading lines are trimmed
noise — skipped for the header check, never entries — and a UTF-8
BOM is tolerated, stripped by decoding.
:entries is a vector of every successfully parsed line after the
header in file order, later \"session\" lines excepted; :leafId is the
:id of the last entry, whatever its type, mirroring pi's index build;
:name is the latest session_info name, trimmed, nil when absent or
blank, mirroring pi's session readers.  Malformed and blank lines are
skipped silently."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      ;; Pi reads session files as content.trim().split("\n"): skip
      ;; whitespace-only leading lines (CR included — a CRLF-decoded
      ;; blank is "\r") for the header check.  They never become
      ;; entries.
      (while (and (not (eobp))
                 (looking-at-p "[ \t\r]*$"))
        (forward-line 1))
      (let ((header (pilish--jsonl-parse-session-header
                     (buffer-substring-no-properties
                      (point) (line-end-position))))
            (entries nil)
            (name nil))
        (when header
          (forward-line 1)
          (while (not (eobp))
            (let ((data (pilish--parse-json-line
                         (buffer-substring-no-properties
                          (point) (line-end-position)))))
              (when (consp data)
                (let ((type (plist-get data :type)))
                  (unless (equal type "session")
                    (push data entries)
                    (when (equal type "session_info")
                      (let ((raw (pilish--normalize-string-or-null
                                  (plist-get data :name))))
                        (setq name
                              (when raw
                                (let ((trimmed (string-trim raw)))
                                  (unless (string-empty-p trimmed)
                                    trimmed))))))))))
            (forward-line 1))
          (let* ((vector (vconcat (nreverse entries)))
                 (count (length vector)))
            (list :path path
                  :header header
                  :entries vector
                  :leafId (when (> count 0)
                            (plist-get (aref vector (1- count)) :id))
                  :name name)))))))

;;;; Session Discovery

(defconst pilish--jsonl-line-type-re
  "[ \t]*{[ \t]*\"type\"[ \t]*:[ \t]*\"%s\""
  "Format string matching a JSONL line whose top-level type appears first.
Pi writes session JSONL with `type' as the first key, so matching that
cheap prefix lets the canonical shared metadata scanner route lines
without parsing their full payloads.")

(defun pilish--jsonl-line-type-p (type)
  "Return non-nil when the current line has top-level session TYPE."
  (looking-at-p (format pilish--jsonl-line-type-re
                        (regexp-quote type))))

(defun pilish--jsonl-parse-current-line ()
  "Return the current line parsed as a plist, or nil when malformed."
  (pilish--parse-json-line
   (buffer-substring-no-properties (point) (line-end-position))))

(defun pilish-jsonl-sessions-root (&optional anchor)
  "Return pi's sessions root directory as a directory name (trailing slash).
The default is PI_CODING_AGENT_DIR, else ~/.pi/agent, plus
\"sessions\" (a port of pi's getAgentDir), expanded.
ANCHOR, when remote (see `file-remote-p'), roots the scan on that
remote instead: the parent of ANCHOR's own directory — pass a session
FILE, or a directory with a trailing slash.  A local ANCHOR is ignored:
the expanded default always applies."
  (if (and anchor (file-remote-p anchor))
      (file-name-directory
       (directory-file-name (file-name-directory anchor)))
    (concat (expand-file-name (or (getenv "PI_CODING_AGENT_DIR")
                                  "~/.pi/agent"))
            "/sessions/")))

(defun pilish-jsonl-session-dir-for-cwd (cwd &optional root)
  "Return the munged session directory name for CWD under ROOT.
A pure string port of pi's getDefaultSessionDirPath: clean CWD's name
\(trailing slashes collapse), strip ONE leading / or backslash, then
replace every /, backslash, and : with a dash, wrapped as --…--.
ROOT defaults to `pilish-jsonl-sessions-root'.  The result
carries no trailing slash; Windows drives munge like \"C:\\x\" to
--C--x--, exactly like pi's character class."
  (let* ((base (directory-file-name
                (or root (pilish-jsonl-sessions-root))))
         (clean (if (string-empty-p cwd) "" (directory-file-name cwd)))
         (stripped
          (if (and (not (string-empty-p clean))
                   (memq (aref clean 0) '(?/ ?\\)))
              (substring clean 1)
            clean))
         (munged (replace-regexp-in-string "[/\\:]" "-" stripped)))
    (concat (file-name-as-directory base) "--" munged "--")))

(cl-defstruct (pilish--jsonl-info
               (:constructor pilish--jsonl-info-create))
  "Resumable session scan; the decoded buffer's point is its cursor."
  buffer path mtime header name first fallback texts search-text
  (phase 'header) (message-count 0) (parsed-messages 0))

(defun pilish-jsonl-open-session-info (path &optional search-text)
  "Open PATH for a resumable metadata scan, optionally collecting SEARCH-TEXT.
Return an opaque state, or nil on an ordinary read/stat failure.  The
caller must close the state with `pilish-jsonl-close-session-info',
including after errors or quit.  Whole-file IO and decoding are atomic;
line processing starts in `pilish-jsonl-step-session-info'."
  (let (buffer state)
    (unwind-protect
        (condition-case nil
            (when (file-readable-p path)
              (setq buffer (generate-new-buffer " *pilish-session-scan*"))
              (with-current-buffer buffer
                (insert-file-contents path)
                (goto-char (point-min))
                (setq state
                      (pilish--jsonl-info-create
                       :buffer buffer :path path :search-text search-text
                       :mtime (file-attribute-modification-time
                               (file-attributes path))))))
          (error nil))
      (unless state
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun pilish-jsonl-close-session-info (state)
  "Release STATE's decoded buffer and accumulated text; safe to call twice."
  (when state
    (when (buffer-live-p (pilish--jsonl-info-buffer state))
      (kill-buffer (pilish--jsonl-info-buffer state)))
    (setf (pilish--jsonl-info-buffer state) nil
          (pilish--jsonl-info-header state) nil
          (pilish--jsonl-info-name state) nil
          (pilish--jsonl-info-first state) nil
          (pilish--jsonl-info-fallback state) nil
          (pilish--jsonl-info-texts state) nil)))

(defun pilish--jsonl-info-scan-line (state)
  "Reduce the current message or name line into STATE.
Keep the legacy preview budget independent of optional full-text parsing."
  (cond
   ((pilish--jsonl-line-type-p "message")
    (cl-incf (pilish--jsonl-info-message-count state))
    (let ((preview-p (and (null (pilish--jsonl-info-first state))
                         (< (pilish--jsonl-info-parsed-messages state) 5))))
      ;; Malformed lines consume a preview attempt too.
      (when preview-p (cl-incf (pilish--jsonl-info-parsed-messages state)))
      (when (or preview-p (pilish--jsonl-info-search-text state))
        (let ((data (pilish--jsonl-parse-current-line)))
          (when (consp data)
            (let* ((message (plist-get data :message))
                   (role (plist-get message :role))
                   (search-p (and (pilish--jsonl-info-search-text state)
                                  (member role '("user" "assistant"))))
                   (text (when (or preview-p search-p)
                           (pilish--jsonl-extract-text
                            (plist-get message :content)))))
              (when preview-p
                (cond
                 ((and (equal role "user") (not (string-empty-p text)))
                  (setf (pilish--jsonl-info-first state) text))
                 ((null (pilish--jsonl-info-fallback state))
                  (setf (pilish--jsonl-info-fallback state) text))))
              (when (and search-p (not (string-empty-p text)))
                (push text (pilish--jsonl-info-texts state)))))))))
   ((pilish--jsonl-line-type-p "session_info")
    (when-let* ((data (pilish--jsonl-parse-current-line)))
      (let ((raw (pilish--normalize-string-or-null (plist-get data :name))))
        ;; Latest parseable entry wins; absent or blank names clear.
        (setf (pilish--jsonl-info-name state)
              (when raw
                (let ((trimmed (string-trim raw)))
                  (unless (string-empty-p trimmed) trimmed)))))))))

(defun pilish--jsonl-info-result (state)
  "Build STATE's metadata and optional single prepared search corpus."
  (when-let* ((header (pilish--jsonl-info-header state)))
    (let ((id (pilish--normalize-string-or-null (plist-get header :id)))
          (cwd (pilish--normalize-string-or-null (plist-get header :cwd)))
          (parent (pilish--normalize-string-or-null
                   (plist-get header :parentSession)))
          (created (pilish--normalize-string-or-null
                    (plist-get header :timestamp)))
          (name (pilish--jsonl-info-name state))
          (first (or (pilish--jsonl-info-first state)
                     (let ((fallback (pilish--jsonl-info-fallback state)))
                       (unless (string-empty-p (or fallback "")) fallback)))))
      (append (list :path (pilish--jsonl-info-path state))
              (when id (list :id id))
              (when cwd (list :cwd cwd))
              (when name (list :name name))
              (when parent (list :parentSessionPath parent))
              (when created (list :created created))
              (list :modified
                    (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                        (pilish--jsonl-info-mtime state) t)
                    :messageCount (pilish--jsonl-info-message-count state))
              (when first (list :firstMessage first))
              (when (pilish--jsonl-info-search-text state)
                ;; Join once, including the legacy prefix.  Do not retain
                ;; a second history-sized string or deduplicate firstMessage.
                (let ((texts (nreverse (pilish--jsonl-info-texts state))))
                  (list :searchText
                        (mapconcat #'identity
                                   (cons (or name "")
                                         (cons (or first "") (or texts '(""))))
                                   " "))))))))

(defun pilish-jsonl-step-session-info (state &optional deadline)
  "Advance STATE until DEADLINE, an absolute `float-time', or completion.
Return nil while incomplete, or (done . INFO); INFO is nil for a
non-session file.  Nil DEADLINE runs to completion without clock checks.
Check between decoded lines, including leading blanks, and before result
joining.  IO, one JSON line, joining and GC can exceed the caller's budget.
Close STATE after completion or failure; do not step a completed state."
  (with-current-buffer (pilish--jsonl-info-buffer state)
    (while (and (not (eobp))
                (not (eq (pilish--jsonl-info-phase state) 'invalid))
                (or (null deadline) (< (float-time) deadline)))
      (if (eq (pilish--jsonl-info-phase state) 'header)
          (unless (looking-at-p "[ \t\r]*$")
            (setf (pilish--jsonl-info-header state)
                  (pilish--jsonl-parse-session-header
                   (buffer-substring-no-properties (point) (line-end-position)))
                  (pilish--jsonl-info-phase state)
                  (if (pilish--jsonl-info-header state) 'messages 'invalid)))
        (pilish--jsonl-info-scan-line state))
      (forward-line 1))
    (when (and (or (eobp) (eq (pilish--jsonl-info-phase state) 'invalid))
               (or (null deadline) (< (float-time) deadline)))
      (cons 'done (pilish--jsonl-info-result state)))))

(defun pilish-jsonl-read-session-info (path &optional search-text)
  "Read session metadata for the file at PATH, without building trees.
Return a plist in the browse session dialect — (:path :id :cwd :name?
:parentSessionPath? :created? :modified :messageCount :firstMessage?)
— or nil when PATH is unreadable, empty, lacks a \"session\" header as
its first nonblank line, or cannot be read at all.  Key parity with
the session browser is the contract.

This is the canonical shared session metadata scanner.  The scan is
regex-first: lines route by their top-level type prefix and only headers,
session_info lines, and the first few message lines are full-parsed.
:messageCount counts message lines by regex alone
\(toolResult included).  :firstMessage full-parses at most 5 message
lines while unset: a user message with extractable text wins,
otherwise the first parsed message of any role is the fallback.
:name replays session_info lines in file order with latest-wins
trimming.  label and custom entries are ignored.  :created is the
header timestamp; :modified is the file mtime as a second-resolution
UTC ISO string (see the deviation note in the Commentary).

Non-nil SEARCH-TEXT additionally parses every type-first message line,
collecting user/assistant string content and text blocks across all disk
branches, not thinking, tools, images or summaries.  :searchText is one
prepared corpus: name, firstMessage, then these texts, separated by spaces.
The legacy firstMessage prefix is preserved even for an any-role fallback.
Nil SEARCH-TEXT retains the cheap metadata-only parsing budget and keys."
  (let (state)
    (unwind-protect
        (condition-case nil
            (when (setq state (pilish-jsonl-open-session-info path search-text))
              (cdr (pilish-jsonl-step-session-info state)))
          (error nil))
      (pilish-jsonl-close-session-info state))))

;;;; Building Raw Trees

(defun pilish--jsonl-entry-< (a b)
  "Return non-nil when the timestamp of entry A precedes entry B's.
Absent timestamps read as the empty string so `string<' never sees nil."
  (string< (or (plist-get a :timestamp) "")
           (or (plist-get b :timestamp) "")))

(defun pilish-jsonl-build-tree (entries)
  "Build the raw nested session tree from the flat ENTRIES vector.
Return a plist with :tree and :leafId: TREE is a vector of root nodes
shaped (:entry E :children VECTOR :label S :labelTimestamp S),
exactly the shape of pi's get_tree, and LEAF-ID is the :id of the last
entry in file order, whatever its type.

Roots are entries whose parent is nil, themselves, or unknown, in file
order; children are sorted by timestamp with ties keeping file order.
Label entries replay in file order with latest-wins folding onto their
targetId, an empty label clearing like an absent one; cleared labels
omit both label keys.  Entries unreachable from any root (cycles) are
dropped.  Traversal is iterative.

Duplicate nonempty ids are canonicalized before linking: equal repeats
collapse silently, while differing repeats retain the last occurrence
and add :diagnostic/:ambiguousIds to the result.  Raw nodes for those
ids carry :ambiguousId t so projection and browser markers stay honest.
Nil/empty legacy ids remain distinct and unaddressable."
  (let* ((canonicalized (pilish--jsonl-canonicalize-entries entries))
         (entries (plist-get canonicalized :entries))
         (ambiguous-ids (plist-get canonicalized :ambiguousIds))
         (count (length entries))
         (leaf-id (when (> count 0)
                    (plist-get (aref entries (1- count)) :id)))
         ;; Pass A: index every entry by id, fold labels in file order.
         (id-hash (make-hash-table :test #'equal))
         (labels (make-hash-table :test #'equal))
         (label-timestamps (make-hash-table :test #'equal))
         ;; Pass B: link children to parents (lists pushed in file order).
         (kids (make-hash-table :test #'equal))
         (roots nil))
    (dotimes (i count)
      (let ((entry (aref entries i)))
        (puthash (plist-get entry :id) entry id-hash)
        (when (equal (plist-get entry :type) "label")
          (let ((target (pilish--normalize-string-or-null
                         (plist-get entry :targetId)))
                (label (let ((raw (pilish--normalize-string-or-null
                                  (plist-get entry :label))))
                         ;; JS truthiness: an empty-string label clears.
                         (and raw (not (string-empty-p raw)) raw))))
            (if label
                (progn
                  (puthash target label labels)
                  (puthash target (plist-get entry :timestamp)
                           label-timestamps))
              (remhash target labels)
              (remhash target label-timestamps))))))
    (dotimes (i count)
      (let* ((entry (aref entries i))
             (id (plist-get entry :id))
             (parent (pilish--jsonl-entry-parent-id entry)))
        (if (or (null parent)
                (equal parent id)
                (not (gethash parent id-hash)))
            (push entry roots)
          (puthash parent (cons entry (gethash parent kids)) kids))))
    (setq roots (nreverse roots))
    ;; Pass C: iterative pre-order collection, then build bottom-up by
    ;; walking the reversed pre-order so every child exists before its
    ;; parent needs it.  Each entry expands at most once: without the
    ;; guard, duplicate ids closing a cycle would expand forever.
    (let ((order nil)
          (stack roots)
          (expanded (make-hash-table :test #'eq))
          (child-nodes (make-hash-table :test #'equal))
          (built-roots nil))
      (while stack
        (let ((entry (pop stack)))
          (unless (gethash entry expanded)
            (puthash entry t expanded)
            (let ((id (plist-get entry :id)))
              (push entry order)
              (setq stack
                    (append (sort (nreverse (gethash id kids))
                                  #'pilish--jsonl-entry-<)
                            stack))))))
      ;; ORDER now holds the reversed pre-order; iterating it visits
      ;; descendants before parents and later siblings before earlier
      ;; ones, so plain pushes land in natural order.
      (dolist (entry order)
        (let* ((id (plist-get entry :id))
               (label (gethash id labels))
               (base (append
                      (list :entry entry
                            :children (vconcat (gethash id child-nodes)))
                      (when (member id ambiguous-ids)
                        (list :ambiguousId t))))
               (node (if label
                         (append base
                                 (list :label label
                                       :labelTimestamp
                                       (gethash id label-timestamps)))
                       base))
               (parent (pilish--jsonl-entry-parent-id entry)))
          (if (or (null parent)
                  (equal parent id)
                  (not (gethash parent id-hash)))
              (push node built-roots)
            (puthash parent (cons node (gethash parent child-nodes))
                     child-nodes))))
      (pilish--jsonl-add-duplicate-metadata
       (list :tree (vconcat built-roots)
             :leafId leaf-id)
       ambiguous-ids))))

;;;; Text Extraction and Previews

(defun pilish--jsonl-extract-text (content &optional max-len separator)
  "Extract preview text from CONTENT, an AgentMessage content value.
Strings pass through (optionally sliced to MAX-LEN); vectors contribute
the text of blocks whose :type is \"text\" and whose :text is a string,
joined with SEPARATOR and then sliced; anything else is the empty
string.  MAX-LEN nil means unlimited, MAX-LEN 0 or less means empty.
SEPARATOR defaults to a space (the preview dialect); the contentText
port passes the EMPTY string so block text concatenates unbounded
and unspaced like pi's editor prefill."
  (let ((slice (lambda (text)
                 (if (and max-len (< max-len (length text)))
                     (substring text 0 max-len)
                   text))))
    (cond
     ((and max-len (<= max-len 0)) "")
     ((stringp content) (funcall slice content))
     ((vectorp content)
      (let (blocks)
        (dotimes (i (length content))
          (let ((block (aref content i)))
            (when (and (equal (plist-get block :type) "text")
                       (stringp (plist-get block :text)))
              (push (plist-get block :text) blocks))))
        (funcall slice (mapconcat #'identity (nreverse blocks)
                                  (or separator " ")))))
     (t ""))))

(defun pilish--jsonl-normalize-preview (text)
  "Flatten TEXT for single-line previews.
Newlines and tabs (but not carriage returns) become spaces and the
result is trimmed."
  (string-trim (replace-regexp-in-string "[\n\t]" " " text)))

;;;; Tool-Call Formatting

(defun pilish--jsonl-arg-number (args key)
  "Return the numeric value of ARGS KEY, or nil when not usable.
A number comes back as-is with its zero-ness intact; JSON null comes
back as the :null keyword, which the reference's `!== undefined'
checks treat as present (read offset/limit); absent keys, JSON false,
and non-numbers read as nil."
  (let ((value (plist-get args key)))
    (cond
     ((numberp value) value)
     ;; JSON null is present-but-null for !== undefined checks.
     ((eq value :null) value)
     (t nil))))

(defun pilish--jsonl-arg-string (args key)
  "Return the string value of ARGS KEY under JavaScript truthiness.
JSON null, JSON false, zero, and the empty string fall through to nil
so that an `||' chain can skip them; other values are stringified
like `String()'."
  (let ((value (plist-get args key)))
    (cond
     ((memq value '(nil :null :false)) nil)
     ((eq value :true) "true")
     ((stringp value) (unless (string-empty-p value) value))
     ((numberp value) (unless (zerop value) (number-to-string value)))
     (t (format "%s" value)))))

(defun pilish--jsonl-shorten-path (path)
  "Replace PATH's home-directory prefix with ~.
Uses HOME, falling back to USERPROFILE.  The prefix match is blind, a
documented port of the upstream quirk: /home/tes matches
/home/tester/x just as well as /home/tester/x matches itself."
  (let ((home (or (getenv "HOME") (getenv "USERPROFILE") "")))
    (if (and (not (string-empty-p home))
             (string-prefix-p home path))
        (concat "~" (substring path (length home)))
      path)))

(defun pilish--jsonl-encode-args (args)
  "Normalize parsed-JSON ARGS for `json-encode'.
Map the :true, :false, and :null keywords (how tests spell JSON
booleans and null) to the values `json-encode' expects, recursively."
  (cond
   ((eq args :true) t)
   ((eq args :false) :json-false)
   ((eq args :null) nil)
   ((vectorp args)
    (vconcat (mapcar #'pilish--jsonl-encode-args args)))
   ((consp args)
    (let (out)
      (while (consp args)
        (let ((key (pop args)))
          (push key out)
          (push (if (consp args)
                    (pilish--jsonl-encode-args (pop args))
                  nil)
                out)))
      (nreverse out)))
   (t args)))

(defun pilish-jsonl-format-tool-call (name args)
  "Return the bracket preview for the tool NAME called with ARGS.
Port of pi's format-tool-call: [read: ~/f.py:10-29] and friends.
ARGS is a plist of parsed-JSON tool arguments (nil allowed)."
  (pcase name
    ("read"
     (let* ((path (pilish--jsonl-shorten-path
                   (or (pilish--jsonl-arg-string args :path)
                       (pilish--jsonl-arg-string args :file_path)
                       "")))
            (offset (pilish--jsonl-arg-number args :offset))
            (limit (pilish--jsonl-arg-number args :limit)))
       (if (null (or offset limit))
           (format "[read: %s]" path)
         (let ((start (if (numberp offset) offset 1))
               (end (cond
                     ((numberp limit) (+ (if (numberp offset) offset 1)
                                         limit -1))
                     ;; JS null limit coerces to 0: end = start - 1.
                     (limit (+ (if (numberp offset) offset 1) -1))
                     ;; Limit absent: no -end suffix.
                     (t nil))))
           (format "[read: %s:%s%s]" path start
                   (if (and end (/= end 0)) (format "-%d" end) ""))))))
    ((or "write" "edit")
     (let ((path (pilish--jsonl-shorten-path
                  (or (pilish--jsonl-arg-string args :path)
                      (pilish--jsonl-arg-string args :file_path)
                      ""))))
       (format "[%s: %s]" name path)))
    ("bash"
     (let* ((command (or (pilish--jsonl-arg-string args :command) ""))
            (normalized (pilish--jsonl-normalize-preview command))
            (truncated (substring normalized 0 (min 50 (length normalized)))))
       (format "[bash: %s%s]" truncated
               (if (> (length normalized) 50) "..." ""))))
    ("grep"
     (format "[grep: /%s/ in %s]"
             (or (pilish--jsonl-arg-string args :pattern) "")
             (pilish--jsonl-shorten-path
              (or (pilish--jsonl-arg-string args :path) "."))))
    ("find"
     (format "[find: %s in %s]"
             (or (pilish--jsonl-arg-string args :pattern) "")
             (pilish--jsonl-shorten-path
              (or (pilish--jsonl-arg-string args :path) "."))))
    ("ls"
     (format "[ls: %s]"
             (pilish--jsonl-shorten-path
              (or (pilish--jsonl-arg-string args :path) "."))))
    (_
     (let* ((json (if args
                       (json-encode (pilish--jsonl-encode-args args))
                     "{}"))
            (truncated (substring json 0 (min 40 (length json)))))
       (format "[%s: %s%s]" name truncated
               (if (> (length json) 40) "..." ""))))))

;;;; Projection

(defun pilish--jsonl-assistant-tool-calls (message)
  "Return the tool-call records of assistant MESSAGE as (ID NAME ARGS).
Only content blocks whose :type is \"toolCall\" with string :id and
:name count; non-object :arguments normalize to the empty plist."
  (let ((content (plist-get message :content))
        (calls nil))
    (when (vectorp content)
      (dotimes (i (length content))
        (let ((block (aref content i)))
          (when (equal (plist-get block :type) "toolCall")
            (let ((id (plist-get block :id))
                  (name (plist-get block :name))
                  (args (plist-get block :arguments)))
              (when (and (stringp id) (stringp name))
                (push (list id name (if (consp args) args '()))
                      calls)))))))
    (nreverse calls)))

(defun pilish--jsonl-build-tool-call-map (roots)
  "Return the global toolCallId map for ROOTS: id to (NAME ARGS).
Scans assistant messages with an iterative pre-order walk; when both
branches of a fork reuse an id, the later visit wins."
  (let ((map (make-hash-table :test #'equal))
        (stack (append roots nil)))
    (while stack
      (let* ((node (pop stack))
             (entry (plist-get node :entry)))
        (when (and (equal (plist-get entry :type) "message")
                   (equal (plist-get (plist-get entry :message) :role)
                          "assistant"))
          (dolist (call (pilish--jsonl-assistant-tool-calls
                         (plist-get entry :message)))
            (puthash (nth 0 call) (list (nth 1 call) (nth 2 call)) map)))
        (setq stack (append (plist-get node :children) stack))))
    map))

(defun pilish--jsonl-project-assistant (base message)
  "Return the projected assistant node for BASE and MESSAGE.
Preview precedence: extracted text, then aborted stop reason, then
errorMessage, then the \"(no content)\" sentinel that the browser filter
hides.  :stopReason is always present; :errorMessage only when set."
  (let* ((text (pilish--jsonl-extract-text
                (plist-get message :content) 200))
         (stop-reason (pilish--normalize-string-or-null
                       (plist-get message :stopReason)))
         (error-message (pilish--normalize-string-or-null
                         (plist-get message :errorMessage)))
         (preview (cond
                   ((and (stringp text) (> (length text) 0))
                    (pilish--jsonl-normalize-preview text))
                   ((equal stop-reason "aborted") "(aborted)")
                   ((and error-message (> (length error-message) 0))
                    (pilish--jsonl-normalize-preview error-message))
                   (t "(no content)"))))
    (append base
            (list :type "message" :role "assistant"
                  :preview preview :stopReason stop-reason)
            (when error-message (list :errorMessage error-message)))))

(defun pilish--jsonl-project-tool-result (base message branch-calls global-calls)
  "Return the projected tool-result node for BASE and MESSAGE.
Resolve MESSAGE's toolCallId against BRANCH-CALLS (the map seen along
the branch path) then GLOBAL-CALLS.  Resolved calls carry :toolName,
:toolArgs, and :formattedToolCall; unresolved ones fall back to the
message's own tool name and a \"[name]\" preview."
  (let* ((call-id (pilish--normalize-string-or-null
                   (plist-get message :toolCallId)))
         (info (when call-id
                 (or (gethash call-id branch-calls)
                     (gethash call-id global-calls))))
         (message-name (pilish--normalize-string-or-null
                        (plist-get message :toolName)))
         (formatted (when info
                      (pilish-jsonl-format-tool-call
                       (nth 0 info) (nth 1 info))))
         (name (if info (nth 0 info) message-name))
         (preview (if formatted
                      (pilish--jsonl-normalize-preview formatted)
                    (format "[%s]" (if (stringp name) name "tool")))))
    (append base
            (list :type "tool_result")
            (when name (list :toolName name))
            (when info (list :toolArgs (nth 1 info)))
            (when formatted (list :formattedToolCall formatted))
            (list :preview preview))))

(defun pilish--jsonl-project-message (base message branch-calls global-calls)
  "Return the projected node for BASE and the AgentMessage MESSAGE.
BRANCH-CALLS and GLOBAL-CALLS feed tool-result resolution."
  (let ((role (pilish--normalize-string-or-null
               (plist-get message :role))))
    (cond
     ((equal role "toolResult")
      (pilish--jsonl-project-tool-result
       base message branch-calls global-calls))
     ((equal role "bashExecution")
      (append base
              (list :type "message" :role "bashExecution"
                    :preview (pilish-jsonl-format-tool-call
                              "bash"
                              (list :command (plist-get message :command))))))
     ((equal role "assistant")
      (pilish--jsonl-project-assistant base message))
     (t
      (let* ((content (plist-get message :content))
             (summary-role-p
              (member role '("branchSummary" "compactionSummary")))
             (summary
              (when summary-role-p
                (or (pilish--normalize-string-or-null
                     (plist-get message :summary))
                    (pilish--normalize-string-or-null
                     (pilish--jsonl-extract-text content)))))
             (preview (pilish--jsonl-normalize-preview
                       (pilish--jsonl-extract-text
                        (if summary-role-p summary content) 200))))
        (append base
                (list :type "message")
                (pcase role
                  ((or "user" "custom" "branchSummary" "compactionSummary")
                   (list :role role))
                  (_ (append (list :role "unknown")
                             (when role (list :rawRole role)))))
                (list :preview preview)
                (when summary (list :summary summary))))))))

(defun pilish--jsonl-project-entry (base entry)
  "Return the projected node for BASE and the non-message ENTRY."
  (pcase (plist-get entry :type)
    ("compaction"
     (let ((summary (pilish--normalize-string-or-null
                     (plist-get entry :summary)))
           (tokens (plist-get entry :tokensBefore)))
       (append base
               (list :type "compaction")
               (when summary (list :summary summary))
               (when (numberp tokens)
                 (list :tokensBefore tokens)))))
    ("model_change"
     (let ((provider (pilish--normalize-string-or-null
                      (plist-get entry :provider)))
           (model-id (pilish--normalize-string-or-null
                      (plist-get entry :modelId))))
       (append base
               (list :type "model_change")
               (when provider (list :provider provider))
               (when model-id (list :modelId model-id)))))
    ("thinking_level_change"
     (let ((level (pilish--normalize-string-or-null
                   (plist-get entry :thinkingLevel))))
       (append base
               (list :type "thinking_level_change")
               (when level (list :thinkingLevel level)))))
    ("branch_summary"
     (let ((summary (pilish--normalize-string-or-null
                     (plist-get entry :summary))))
       (append base
               (list :type "branch_summary")
               (when summary (list :summary summary)))))
    ("custom_message"
     (let ((custom-type (pilish--normalize-string-or-null
                         (plist-get entry :customType))))
       (append base
               (list :type "custom_message")
               (when custom-type (list :customType custom-type))
               (list :preview
                     (pilish--jsonl-normalize-preview
                      (pilish--jsonl-extract-text
                       (plist-get entry :content) 200))))))
    ;; Unknown future entry types keep their type with no payload.
    (_ (append base (list :type (plist-get entry :type))))))

(defun pilish--jsonl-resolve-projected-leaf-id
    (roots leaf-id &optional ambiguous-ids)
  "Resolve raw LEAF-ID to the nearest visible entry id under ROOTS.
Walk the raw parent chain (over all nodes, filtered ones included)
until a non-filtered entry appears.  Nil or unknown ids resolve to nil.
Any id in AMBIGUOUS-IDS also resolves to nil rather than choosing an
occurrence.  A direct uncanonicalized duplicate tree fails closed, and
a seen set keeps malformed parent cycles total."
  (when (and leaf-id (pilish-jsonl-tree-ids-unique-p roots t))
    (let ((parent-by-id (make-hash-table :test #'equal))
          (visible-ids (make-hash-table :test #'equal))
          (stack (append roots nil))
          (found nil))
      (while stack
        (let* ((node (pop stack))
               (entry (plist-get node :entry))
               (id (pilish--normalize-string-or-null
                    (plist-get entry :id))))
          (when id
            (puthash id (pilish--jsonl-entry-parent-id entry)
                     parent-by-id)
            (unless (pilish--jsonl-filtered-entry-p
                     (plist-get entry :type))
              (puthash id t visible-ids)))
          (setq stack (append (plist-get node :children) stack))))
      (let ((current (pilish--normalize-string-or-null leaf-id))
            (seen (make-hash-table :test #'equal)))
        (while (and (stringp current)
                    (not found)
                    (not (gethash current seen)))
          (puthash current t seen)
          (cond
           ((member current ambiguous-ids)
            (setq current nil))
           ((gethash current visible-ids)
            (setq found current))
           (t
            (setq current (gethash current parent-by-id)))))
        found))))

(defun pilish-jsonl-project-tree
    (tree &optional leaf-id inherited-ambiguous-ids)
  "Project the raw session TREE to the flat display dialect.
TREE is a vector of raw nodes (:entry :children :label
:labelTimestamp), the output of `pilish-jsonl-build-tree' or
pi's get_tree.  Return (:tree :leafId), plus :diagnostic and
:ambiguousIds when differing duplicate ids were canonicalized.
Bookkeeping entries (label, session_info, custom) are dropped with
their children promoted to the nearest visible ancestor, toolResult
messages resolve their toolCallId branch-locally first, and :parentId
points at the nearest visible ancestor.  LEAF-ID, when non-nil, is a
raw leaf id resolved up to the nearest visible entry.  Traversal is
iterative.

Direct duplicate raw trees are canonicalized too: equal repeats collapse;
differing repeats use the later occurrence and remain explicitly marked.
When an ambiguous bookkeeping node is filtered away, its next visible
child carries :ambiguousParent so active ancestry cannot cross the hidden
choice.  INHERITED-AMBIGUOUS-IDS carries build-time ambiguity explicitly;
the established two-argument form also harvests :ambiguousId markers
from builder output, which is already structurally unique.  Nil legacy
ids remain."
  (let* ((raw-entries (pilish--jsonl-raw-tree-entries tree))
         (raw-ambiguous-ids
          (pilish--jsonl-raw-tree-ambiguous-ids tree))
         (canonicalized (pilish--jsonl-canonicalize-entries raw-entries))
         (ambiguous-ids
          (pilish--jsonl-merge-ids
           inherited-ambiguous-ids
           raw-ambiguous-ids
           (plist-get canonicalized :ambiguousIds)))
         (tree
          (if (plist-get canonicalized :repeated-p)
              (plist-get
               (pilish-jsonl-build-tree
                (plist-get canonicalized :entries))
               :tree)
            tree))
         (global-calls (pilish--jsonl-build-tool-call-map tree))
         (empty-map (make-hash-table :test #'equal))
         ;; Work items: (node parent-visible-node branch-calls
         ;;              ambiguous-promoted-parent-p).
         (stack nil)
         ;; RECORDS ends up holding the reversed pre-order.
         (records nil)
         (child-nodes (make-hash-table :test #'eq))
         (built-roots nil))
    (dolist (node (nreverse (append tree nil)))
      (push (list node nil empty-map nil) stack))
    (while stack
      (pcase-let
          ((`(,node ,parent-visible ,branch-calls ,ambiguous-parent-p)
            (pop stack)))
        (let* ((entry (plist-get node :entry))
               (id (pilish--normalize-string-or-null
                    (plist-get entry :id)))
               (node-ambiguous-p
                (or (plist-get node :ambiguousId)
                    (member id ambiguous-ids))))
          (if (pilish--jsonl-filtered-entry-p
               (plist-get entry :type))
              ;; Promote children to the same visible parent and map.  If
              ;; this omitted chain contains ambiguity, retain that edge
              ;; boundary so browser ancestry markers stop truthfully.
              (dolist (child (nreverse (append (plist-get node :children)
                                               nil)))
                (push (list child parent-visible branch-calls
                            (or ambiguous-parent-p node-ambiguous-p))
                      stack))
            (let ((child-map branch-calls))
              (when (and (equal (plist-get entry :type) "message")
                         (equal (plist-get (plist-get entry :message) :role)
                                "assistant"))
                (let ((calls (pilish--jsonl-assistant-tool-calls
                              (plist-get entry :message))))
                  (when calls
                    (setq child-map (copy-hash-table branch-calls))
                    (dolist (call calls)
                      (puthash (nth 0 call)
                               (list (nth 1 call) (nth 2 call))
                               child-map)))))
              ;; The node itself resolves against its incoming map.  Once
              ;; visible, it carries any promoted boundary itself; its
              ;; children can reach it directly and need no inherited flag.
              (push (list node parent-visible branch-calls
                          ambiguous-parent-p)
                    records)
              (dolist (child (nreverse (append (plist-get node :children)
                                               nil)))
                (push (list child node child-map nil) stack)))))))
    ;; Build bottom-up: iterating RECORDS visits descendants before
    ;; parents and later siblings before earlier ones, so plain pushes
    ;; land children in natural order.
    (dolist (record records)
      (pcase-let
          ((`(,node ,parent-visible ,branch-calls ,ambiguous-parent-p)
            record))
        (let* ((entry (plist-get node :entry))
               (label (pilish--normalize-string-or-null
                       (plist-get node :label)))
               (id (pilish--normalize-string-or-null
                    (plist-get entry :id)))
               (base (append
                      (list :id id
                            :parentId (when parent-visible
                                        (pilish--normalize-string-or-null
                                         (plist-get
                                          (plist-get parent-visible :entry)
                                          :id)))
                            :timestamp (plist-get entry :timestamp))
                      (when label (list :label label))
                      (when (or (plist-get node :ambiguousId)
                                (member id ambiguous-ids))
                        (list :ambiguousId t))
                      (when ambiguous-parent-p
                        (list :ambiguousParent t))
                      (list :children (vconcat (gethash node child-nodes)))))
               (projected
                (if (equal (plist-get entry :type) "message")
                    (pilish--jsonl-project-message
                     base (plist-get entry :message) branch-calls global-calls)
                  (pilish--jsonl-project-entry
                   base entry))))
          (if parent-visible
              (puthash parent-visible
                       (cons projected (gethash parent-visible child-nodes))
                       child-nodes)
            (push projected built-roots)))))
    (pilish--jsonl-add-duplicate-metadata
     (list :tree (vconcat built-roots)
           :leafId (pilish--jsonl-resolve-projected-leaf-id
                    tree leaf-id ambiguous-ids))
     ambiguous-ids)))

(defun pilish-jsonl-project-session-file (path)
  "Read, build, and project the session file at PATH in one step.
Composition of `pilish-jsonl-read-file',
`pilish-jsonl-build-tree', and
`pilish-jsonl-project-tree': return the (:tree :leafId)
projection, or nil when PATH is missing, empty, or its first nonblank
line is not the session header — there is no tree to render, never an
error.  The composition exists so the
tree browser's disk-based fetch cannot mix stages from different
reads of a file a live pi appends to concurrently."
  (when-let* ((session (pilish-jsonl-read-file path)))
    (let ((built (pilish-jsonl-build-tree
                  (plist-get session :entries))))
      (pilish-jsonl-project-tree
       (plist-get built :tree)
       (plist-get built :leafId)
       (plist-get built :ambiguousIds)))))

;;;; Tree Navigation

(defun pilish--jsonl-resolve-visible-canonical
    (entries id ambiguous-ids)
  "Resolve ID through canonical ENTRIES, refusing AMBIGUOUS-IDS."
  (let ((index (make-hash-table :test #'equal)))
    (dotimes (i (length entries))
      (let* ((entry (aref entries i))
             (entry-id (pilish--normalize-string-or-null
                        (plist-get entry :id))))
        (when entry-id
          (puthash entry-id entry index))))
    (let ((current (pilish--normalize-string-or-null id))
          (seen (make-hash-table :test #'equal))
          (found 'unresolved))
      (while (eq found 'unresolved)
        (cond
         ((or (null current)
              (member current ambiguous-ids)
              (gethash current seen))
          (setq found nil))
         (t
          (puthash current t seen)
          (let ((entry (gethash current index)))
            (if (and entry
                     (pilish--jsonl-filtered-entry-p
                      (plist-get entry :type)))
                (setq current (pilish--jsonl-entry-parent-id entry))
              (setq found (and entry current)))))))
      found)))

(defun pilish--jsonl-resolve-visible (entries id)
  "Resolve ID to the nearest non-filtered entry id within ENTRIES.
Walk the parent chain past projection-away bookkeeping.  Equal duplicate
entries canonicalize harmlessly.  Differing duplicates use the later
entry for unrelated lookups, but an ambiguous id encountered at any
point resolves to nil.  Unknown, nil, legacy, and cyclic ids also return
nil."
  (let* ((canonicalized (pilish--jsonl-canonicalize-entries entries))
         (canonical (plist-get canonicalized :entries))
         (ambiguous-ids (plist-get canonicalized :ambiguousIds)))
    (pilish--jsonl-resolve-visible-canonical
     canonical id ambiguous-ids)))

(defun pilish-jsonl-current-projected-id (session)
  "Return SESSION's actual current projected entry id, or nil.
SESSION is a `pilish-jsonl-read-file' result.  Resolve its raw leaf up
past only the bookkeeping records omitted by
`pilish-jsonl-project-tree' (label, session_info, and custom).  Entries
that a browser filter may hide later — tools, model changes, and
thinking-level changes — remain projected and therefore remain the
actual current entry."
  (pilish--jsonl-resolve-visible
   (plist-get session :entries) (plist-get session :leafId)))

(defun pilish--jsonl-chain-touches-ambiguous-p
    (index start-id ambiguous-ids)
  "Return non-nil when START-ID's parent chain hits AMBIGUOUS-IDS.
INDEX maps canonical entry ids to entries.  Unknown parents terminate
safely, and a seen set bounds malformed parent cycles."
  (let ((current (pilish--normalize-string-or-null start-id))
        (seen (make-hash-table :test #'equal))
        found)
    (while (and current (not found) (not (gethash current seen)))
      (puthash current t seen)
      (if (member current ambiguous-ids)
          (setq found t)
        (setq current
              (when-let* ((entry (gethash current index)))
                (pilish--jsonl-entry-parent-id entry)))))
    found))

(defun pilish-jsonl-navigation-target (session target-id)
  "Return the navigation target for TARGET-ID within SESSION.
SESSION is a `pilish-jsonl-read-file' result; return nil when
TARGET-ID names no entry, is ambiguous, or computes a rewrite chain
through an ambiguous id.  Otherwise return the plist
  (:leaf-id ID-OR-NIL :prefill TEXT? :current-p BOOL):

  - :leaf-id is the entry the session file must END on after the
    navigate rewrite — pi's navigateTree leaf rule: a user message or a
    custom_message rewinds to its normalized parent id so the message can
    be re-sent or re-edited (nil when the parent does not exist — the root
    user message has nothing to rewind to); every other entry type targets
    itself.
  - :prefill, present only for those rewind targets with extractable
    text, is the contentText port of the entry's content: strings pass
    through, block content joins text blocks with the EMPTY separator,
    unbounded (the preview dialect's space join and 200-char cap do not
    apply); image-only or empty content omits the key entirely.
  - :current-p reports that the file already sits on the computed
    leaf position only when the resolved target position and resolved
    raw leaf (`pilish--jsonl-resolve-visible' on both sides) are BOTH
    non-nil and equal.  An ambiguous, unknown, or all-filtered nil
    resolution is not evidence of equality.  Thus a literal self-target
    leaf remains pi's raw-id no-op, a trailing bookkeeping leaf folds up
    onto the entry it sits on, and a rewind target is current when its
    parent is the positively resolved current position.  This is
    deliberately different from
    selected-entry identity: callers implementing pi's initial no-op
    must first compare TARGET-ID with
    `pilish-jsonl-current-projected-id', so a current user entry is not
    mistaken for a request to re-edit it.

Equal duplicate entries canonicalize to one.  A differing duplicate id
and any target whose required ancestor chain crosses it fail closed;
unrelated unique branches remain available through the deterministic
later-wins canonical index."
  (let* ((raw-entries (plist-get session :entries))
         (canonicalized
          (pilish--jsonl-canonicalize-entries raw-entries))
         (entries (plist-get canonicalized :entries))
         (ambiguous-ids (plist-get canonicalized :ambiguousIds))
         (index (make-hash-table :test #'equal)))
    (dotimes (i (length entries))
      (let* ((entry (aref entries i))
             (id (pilish--normalize-string-or-null
                  (plist-get entry :id))))
        (when id
          (puthash id entry index))))
    (when-let* ((normalized-target
                 (pilish--normalize-string-or-null target-id))
                ((not (member normalized-target ambiguous-ids)))
                (entry (gethash normalized-target index)))
        (let* ((rewind-p
                (or (equal (plist-get entry :type) "custom_message")
                    (and (equal (plist-get entry :type) "message")
                         (equal (plist-get (plist-get entry :message) :role)
                                "user"))))
               (leaf-id (if rewind-p
                            (pilish--jsonl-entry-parent-id entry)
                          normalized-target))
               (text (when rewind-p
                       (pilish--jsonl-extract-text
                        (if (equal (plist-get entry :type) "custom_message")
                            (plist-get entry :content)
                          (plist-get (plist-get entry :message) :content))
                        nil "")))
               (raw-leaf (plist-get session :leafId)))
          ;; Even a unique target can require an ambiguous ancestor in
          ;; the rewrite chain.  Refuse instead of choosing an occurrence.
          (unless (or (member leaf-id ambiguous-ids)
                      (pilish--jsonl-chain-touches-ambiguous-p
                       index leaf-id ambiguous-ids))
            (let ((resolved-target
                   (pilish--jsonl-resolve-visible-canonical
                    entries leaf-id ambiguous-ids))
                  (resolved-current
                   (pilish--jsonl-resolve-visible-canonical
                    entries raw-leaf ambiguous-ids)))
              (append
               (list :leaf-id leaf-id)
               (when (and text (not (string-empty-p text)))
                 (list :prefill text))
               ;; Equality is proven only between two actual positions;
               ;; two unrelated resolution failures are never current.
               (list :current-p
                     (and resolved-target
                          resolved-current
                          (equal resolved-target
                                 resolved-current))))))))))

(defun pilish-jsonl-navigation-lines (path leaf-id)
  "Return PATH's raw lines reordered so the LEAF-ID chain ends the file.
The result is a vector of unibyte raw line strings WITHOUT their LF
delimiters — the caller joins them with LF and adds the final one — or
nil when PATH is unreadable, empty, or its first nonblank line is
not the session header (`pilish--jsonl-parse-session-header's
rule), or LEAF-ID is nil or names no entry.  Like pi's reader,
whitespace-only leading lines are trimmed noise before the header,
and a UTF-8 BOM prefixing the header line is tolerated for that check
alone: the check decodes a copy with the three bytes stripped while
the returned raw lines stay verbatim.
Every other byte is retained: in particular, the CR of
a CRLF delimiter remains the line's last byte, so joining with LF
reproduces CRLF exactly.  The header line and any blank lines before
it stay at the front, byte-for-byte; the lines after the header are
partitioned into the non-chain lines (first, byte-for-byte
in original relative order), followed by the canonical ancestor-chain
lines in logical parent order from root/orphan-root through LEAF-ID.  Thus
LEAF-ID is the file's last line and the next append lands on it.  The
chain walks parent ids from LEAF-ID and stops at nil, self, unknown, or
cyclic parents (an unknown parent is a root, matching
`pilish-jsonl-build-tree's roots rule); malformed and blank
lines after the header are non-chain bytes preserved verbatim in their
original relative positions.  Equal duplicate entries are tolerated and
their last line is canonical.  Differing duplicate ids use that same
later-wins index for unrelated chains, but a requested chain touching an
ambiguous id fails closed.  Earlier duplicate and nil/legacy physical
lines remain non-chain."
  (when leaf-id
    (condition-case nil
        (when (file-readable-p path)
          (with-temp-buffer
            ;; Keep physical line bytes, including CR in CRLF and any
            ;; undecodable bytes on malformed lines.  Decode only a copy
            ;; of each candidate JSON line for parsing.
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (let* ((contents (buffer-substring-no-properties
                              (point-min) (point-max)))
                   (split (split-string contents "\n"))
                   ;; A final newline splits into one trailing empty line;
                   ;; drop exactly that one (interior empties stay).  The
                   ;; lines become a vector: every access below is
                   ;; positional, and aref is O(1) where nth was O(n).
                   (lines (vconcat (if (and split (equal (car (last split)) ""))
                                       (butlast split)
                                     split)))
                   (count (length lines))
                   ;; Pi reads session files as content.trim().split("\n"):
                   ;; the header is the first NONBLANK line, and a blank
                   ;; on raw bytes is a run of space, tab, or CR.
                   ;; HEADER-INDEX is that line's index, nil when no
                   ;; line is nonblank.
                   (header-index
                    (let ((i 0) (found nil))
                      (while (and (null found) (< i count))
                        (if (string-match-p "\\`[ \t\r]*\\'"
                                            (aref lines i))
                            (setq i (1+ i))
                          (setq found i)))
                      found)))
              (when (and header-index
                         (pilish--jsonl-parse-session-header
                          (decode-coding-string
                           ;; A UTF-8 BOM may prefix the header line;
                           ;; strip the three bytes from this inspection
                           ;; copy only — the vector keeps the raw line.
                           (if (and (> (length (aref lines header-index)) 2)
                                    (string-prefix-p
                                     "\xef\xbb\xbf"
                                     (aref lines header-index)))
                               (substring (aref lines header-index) 3)
                             (aref lines header-index))
                           'utf-8)))
                (let* ((parsed (make-vector count nil))
                       (id-line (make-hash-table :test #'equal))
                       (id-entry (make-hash-table :test #'equal))
                       (ambiguous-id (make-hash-table :test #'equal)))
                  ;; Lines up to and including the header are never
                  ;; entries: the leading blanks by pi's trim rule, the
                  ;; header by definition.  Everything after parses as
                  ;; usual.
                  (dotimes (i count)
                    (when (> i header-index)
                      (let ((data (pilish--parse-json-line
                                   (decode-coding-string (aref lines i)
                                                         'utf-8))))
                        (when (consp data)
                          (aset parsed i data)
                          (unless (equal (plist-get data :type) "session")
                            (let ((id (pilish--normalize-string-or-null
                                       (plist-get data :id))))
                              (when id
                                (when-let* ((previous
                                             (gethash id id-entry)))
                                  (unless (equal previous data)
                                    (puthash id t ambiguous-id)))
                                (puthash id data id-entry)
                                (puthash id i id-line))))))))
                  (when (and (gethash leaf-id id-line)
                             (not (gethash leaf-id ambiguous-id)))
                    ;; Walk leaf to parents using canonical line indices.
                    ;; Each push naturally builds root/orphan-root to leaf,
                    ;; with the requested leaf remaining last.
                    (let ((chain-line-p (make-vector count nil))
                          (chain-lines nil)
                          (current leaf-id)
                          (walking t)
                          (valid t))
                      (while walking
                        (cond
                         ((gethash current ambiguous-id)
                          (setq valid nil
                                walking nil))
                         (t
                          (let ((line-index (and current
                                                 (gethash current id-line))))
                            (if (or (null line-index)
                                    (aref chain-line-p line-index))
                                (setq walking nil)
                              (aset chain-line-p line-index t)
                              (push line-index chain-lines)
                              (setq current
                                    (pilish--jsonl-entry-parent-id
                                     (aref parsed line-index))))))))
                      ;; Keep non-chain physical order, then append the
                      ;; canonical chain in its logical parent order.
                      ;; The leading blanks and the header line itself
                      ;; stay at the front verbatim — pi's append
                      ;; semantics only care that the leaf chain ends
                      ;; the file, and neither is ever a chain or
                      ;; non-chain line (the id map holds only lines
                      ;; after the header).
                      (when valid
                        (let (front)
                          (cl-loop for i from (1+ header-index) below count
                                   unless (aref chain-line-p i)
                                   do (push (aref lines i) front))
                          (let (head)
                            (dotimes (i (1+ header-index))
                              (push (aref lines i) head))
                            (vconcat
                             (nreverse head)
                             (nreverse front)
                             (mapcar (lambda (i) (aref lines i))
                                     chain-lines))))))))))))
      (error nil))))

(provide 'pilish-jsonl)
;;; pilish-jsonl.el ends here
