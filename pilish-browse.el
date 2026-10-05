;;; pilish-browse.el --- Session and tree browser -*- lexical-binding: t; -*-

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

;; Session and tree browsing for Pilish.
;;
;; Provides two read-only, refreshable, keyboard-driven buffers:
;;   - Session Browser: find, filter, switch, rename, and delete sessions
;;   - Tree Browser: continue from a selected turn and label nodes (like /tree)
;;
;; The two browsers display different graphs.  The tree browser moves
;; the active path inside ONE session file, while the session browser's
;; Threaded view shows fork families: SEPARATE session files whose
;; headers point back at a parent (the /fork and /clone commands).
;;
;; Session data comes from time-sliced scans of JSONL files on disk,
;; and conversation trees are projected from the linked chat's JSONL
;; session file.  Browsing does not need a live pi process, but the tree
;; shows the last persisted turn, so it can lag an in-flight turn;
;; refresh manually with `g'.  Tree labels are appended out-of-band and
;; fold back into every disk read.
;;
;; Session switching uses menu.el's guarded resume flow.  Renaming the
;; linked current session asks pi to append session_info; renaming any
;; other session appends session_info out-of-band.  Tree navigation
;; guards the linked session, process, and loaded file; computes the
;; target from fresh JSONL data; atomically rewrites an ordinary local
;; session file so the target's ancestor chain ends it (write-temp +
;; rename — the rename is the only step that touches the file); resumes
;; the rewritten file through menu; prefills the input buffer with the
;; target's re-edit text; and dismisses the browser window once the
;; transition settles.  Navigation does not auto-reopen the browser
;; (refresh with `g'); rewinding before the first message points at the
;; chat's fork command instead.

;;; Code:

(require 'pilish-core)
(require 'pilish-ui)
(require 'pilish-jsonl)
(require 'cl-lib)
(require 'cursor-sensor)
(require 'ucs-normalize)
(require 'magit-section)
(require 'transient)

;; Forward declarations for functions in other modules (avoid circular deps)
(declare-function pilish-set-session-name "pilish-menu" (name))
(declare-function pilish--resume-selected-session "pilish-menu"
                  (proc chat-buf selected-path))
(declare-function pilish--session-transition-ready-p "pilish-menu"
                  (chat-buf action))
(declare-function pilish--session-file-cwd-or-error "pilish-menu"
                  (path))
(declare-function pilish--session-list-directory "pilish-menu"
                  (&optional chat-buf))

;;;; Response Parsers

(defun pilish--parse-tree (response)
  "Parse a `get_tree' RESPONSE into a tree data plist.
Returns plist with :tree (vector) and :leafId (string), or nil on failure.
Currently unused by the disk-based tree seam (`--browse-load-tree'
reads the session file directly); reserved for a future RPC fallback
and as the projected-shape fixture loader for tests."
  (when (eq (plist-get response :success) t)
    (plist-get response :data)))

;;;; Session Display Helpers

(defun pilish--collapse-whitespace (str)
  "Collapse whitespace (including newlines) in STR to a single space."
  (replace-regexp-in-string "[\n\r\t ]+" " " str))

(defun pilish--first-nonempty-line (str)
  "Return the first non-empty line from STR.
Skips leading blank lines.  Returns empty string if STR is empty
or contains only whitespace."
  (if (or (null str) (string-empty-p str))
      ""
    (let ((lines (split-string str "\n")))
      (or (cl-find-if (lambda (l) (not (string-empty-p (string-trim l)))) lines)
          ""))))

(defun pilish--session-display-name (session)
  "Return display name for SESSION plist.
Prefers :name, falls back to :firstMessage, then \"[empty session]\".
Newlines and excess whitespace are collapsed to single spaces."
  (let ((raw (or (pilish--normalize-string-or-null
                  (plist-get session :name))
                 (pilish--normalize-string-or-null
                  (plist-get session :firstMessage)))))
    (if raw
        (pilish--collapse-whitespace raw)
      "[empty session]")))

(defun pilish--session-unsafe-cwd-p (string)
  "Return non-nil when STRING has unsafe display characters in it.
Rejected by Unicode general category — Cc, Cf, Zl, Zp — rather than
hand-listed ranges: control characters (a NUL can signal file
operations; a newline would forge a row), format characters (bidi
overrides and isolates, zero-width marks, the Arabic letter mark,
the byte-order mark — all of which visually reorder or hide text),
and the line and paragraph separators.  Ordinary whitespace (Zs)
and printable text stay allowed."
  (cl-some (lambda (c)
             (memq (get-char-code-property c 'general-category)
                   '(Cc Cf Zl Zp)))
           string))

(defun pilish--session-cwd-parts (localname)
  "Return (KIND COMPONENTS) lexically parsing LOCALNAME, or nil.
KIND is `posix', `windows', or `unc'.  A POSIX localname splits on /
only — a backslash is an ordinary Unix filename character — and dot
segments resolve; the root is the empty component list, and leading
separators beyond one collapse (///a is /a).  Windows drive
spellings — C:/x, C:\\x, and the drive root C:/ — parse as `windows'
with the upcased drive as the first component, so slash and
backslash spellings of one directory share an identity and the drive
disambiguates colliding basenames; a bare C: is drive-relative, not
a project directory, and is rejected.  A UNC spelling — exactly two
leading separators, forward or back, followed by nonempty server and
share components — parses as `unc' with those components as anchors,
never aliasing a rooted POSIX path that merely has the same
components.  Repeated separators after the introducer collapse, so
//server//share/app is an accepted alias of //server/share/app;
fewer than two nonempty anchors and dot or dot-dot anchors reject.
Dot-dot resolves only in tails: the POSIX root, a Windows drive, and
a UNC server/share are anchors that dot-dot never pops.  Anything
else yields nil.  Pure string arithmetic, no filesystem access."
  (let ((resolve
         ;; Resolve dot components, popping only above ANCHOR-COUNT
         ;; leading components.
         (lambda (comps anchor-count)
           (let ((out nil))
             (dolist (c comps)
               (cond ((or (string-empty-p c) (string= c ".")))
                     ((string= c "..")
                      (when (> (length out) anchor-count) (pop out)))
                     (t (push c out))))
             (nreverse out)))))
    (cond
     ((or (string-empty-p localname) (string-prefix-p "~" localname))
      nil)
     ;; Windows drive: letter, colon, separator; the bare drive is
     ;; drive-relative and rejected.
     ((string-match-p "\\`[A-Za-z]:[\\\\/]" localname)
      (let* ((comps (funcall resolve (split-string localname "[\\\\/]") 1))
             (drive (upcase (substring localname 0 2))))
        (list 'windows (cons drive (cdr comps)))))
     ;; UNC: exactly two leading separators, either kind, and the
     ;; third character is not another separator.  Repeated later
     ;; separators collapse, but server and share must still provide
     ;; two nonempty, non-dot anchors — malformed anchors are rejected
     ;; rather than resolved into an alias of another share.
     ((and (or (string-prefix-p "//" localname)
               (string-prefix-p "\\\\" localname))
           (not (string-match-p "\\`[\\\\/]\\{3\\}" localname)))
      ;; Omit nulls: the two leading separators must not become
      ;; empty anchor components.
      (let* ((raw (split-string localname "[\\\\/]" t))
             (server (nth 0 raw))
             (share (nth 1 raw)))
        (if (and server share
                 (not (member server '("." "..")))
                 (not (member share '("." ".."))))
            (list 'unc (funcall resolve raw 2))
          ;; Malformed or missing anchors reject: this spelling names
          ;; no trustworthy project and must not alias another share.
          nil)))
     ((string-prefix-p "/" localname)
      (list 'posix (funcall resolve (split-string localname "/") 0)))
     (t nil))))

(defun pilish--session-zero-width-char-p (char)
  "Return non-nil when CHAR occupies no display column.
This conservative display-identity check catches combining and
Default_Ignorable_Code_Point characters such as the combining
grapheme joiner without rejecting them from legitimate cwd metadata."
  (let ((width (char-width char)))
    (or (null width) (<= width 0))))

(defun pilish--session-route-method-p (string)
  "Return non-nil when STRING has TRAMP's default method grammar.
A method is the default marker `-' or at least two alphanumeric
characters.  This mirrors Emacs 30's `tramp-method-regexp' as plain
lexical validation without loading or consulting TRAMP."
  (or (equal string "-")
      (string-match-p "\\`[[:alnum:]]\\{2,\\}\\'" string)))

(defun pilish--session-route-user-p (string)
  "Return non-nil when STRING has TRAMP's default user grammar.
Users are nonempty and exclude slash, colon, pipe, and blank
characters.  Notably, @ and # are legal inside a user."
  (string-match-p "\\`[^/:|[:blank:]]+\\'" string))

(defun pilish--session-route-plain-host-p (string)
  "Return non-nil when STRING has TRAMP's unbracketed host grammar."
  (string-match-p "\\`[%._[:alnum:]-]+\\'" string))

(defun pilish--session-route-ipv6-host-p (string)
  "Return non-nil when STRING is a TRAMP-style bracketed IPv6 host.
TRAMP intentionally uses a lexical, somewhat loose IPv6 spelling:
one or more colon-terminated alphanumeric groups followed by
alphanumerics or dots.  Requiring that shape rejects arbitrary text
inside brackets while retaining IPv4-mapped forms."
  (and (> (length string) 2)
       (eq (aref string 0) ?\[)
       (eq (aref string (1- (length string))) ?\])
       (string-match-p
        "\\`\\(?:[[:alnum:]]*:\\)+[.[:alnum:]]*\\'"
        (substring string 1 -1))))

(defun pilish--session-route-port-p (string)
  "Return non-nil when STRING is a nonempty numeric TRAMP port."
  (string-match-p "\\`[[:digit:]]+\\'" string))

(defun pilish--session-route-host (route)
  "Return ROUTE's effective final-hop host, parsed wholly and lexically.
Every pipe-separated hop follows Emacs 30's default TRAMP lexical
method/user/host/port grammar.  User syntax is greedy through the last
@, allowing names such as user@example.com and user#tag; only a # in
the remaining host spelling introduces a numeric port.  The first hop
must name an explicit host.  Each later hostless hop may inherit the
nearest prior explicit one, as in /ssh:b|sudo:: and
/ssh:u@b#22|sudo:root@:.  An explicit later host replaces it, and a
bracketed IPv6 literal remains bracketed.

Malformed hops, a hostless first hop, controls, bidi/format text, and
zero-width/default-invisible characters reject the entire route.
Thus /ssh:: never obtains identity from machine-local TRAMP defaults.
No file-name handler, TRAMP function/configuration, filesystem, or
remote IO is consulted."
  (when (and (stringp route)
             (string-prefix-p "/" route)
             (string-suffix-p ":" route)
             (> (length route) 2)
             (not (pilish--session-unsafe-cwd-p route))
             (not (cl-some #'pilish--session-zero-width-char-p route)))
    (let ((hops (split-string (substring route 1 -1) "|" nil))
          effective-host)
      (catch 'invalid
        (when (or (null hops) (member "" hops))
          (throw 'invalid nil))
        (dolist (hop hops)
          (unless (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" hop)
            (throw 'invalid nil))
          (let* ((method (match-string 1 hop))
                 (endpoint (match-string 2 hop))
                 ;; TRAMP's user regexp admits @, so its greedy match
                 ;; effectively leaves the last @ as the delimiter.
                 (at (cl-position ?@ endpoint :from-end t))
                 (user (and at (substring endpoint 0 at)))
                 (host-port (if at (substring endpoint (1+ at)) endpoint))
                 (hash (cl-position ?# host-port))
                 (host (if hash (substring host-port 0 hash) host-port))
                 (port (and hash (substring host-port (1+ hash)))))
            (unless (and (pilish--session-route-method-p method)
                         (or (null user)
                             (pilish--session-route-user-p user))
                         ;; A port belongs to the host suffix, never to
                         ;; the already-separated user spelling.
                         (or (null port)
                             (and (not (string-empty-p host))
                                  (pilish--session-route-port-p port)))
                         (or (string-empty-p host)
                             (pilish--session-route-plain-host-p host)
                             (pilish--session-route-ipv6-host-p host)))
              (throw 'invalid nil))
            (if (string-empty-p host)
                (unless effective-host
                  ;; The first host cannot come from ambient TRAMP
                  ;; defaults, even if a later hop names one.
                  (throw 'invalid nil))
              (setq effective-host host))))
        effective-host))))

(defun pilish--session-syntactic-route (path)
  "Return PATH's complete TRAMP-looking route using string syntax only.
The route ends before an absolute/tilde localname or at end of PATH.
No file-name handler or TRAMP configuration is consulted."
  (when (and (stringp path)
             (string-match
              "\\`\\(/[^/\n]+:\\)\\(?:/\\|~\\|\\'\\)" path))
    (match-string 1 path)))

(defun pilish--session-project-spec-lexical (session)
  "Return SESSION's lexical (IDENTITY HOST COMPONENTS), or nil.
See `pilish--session-project-spec' for the full stored/canonical
contract.  This helper performs pure string validation only."
  (let* ((cwd (pilish--normalize-string-or-null (plist-get session :cwd)))
         (route (pilish--session-syntactic-route
                 (plist-get session :path)))
         (cwd-route (and cwd (pilish--session-syntactic-route cwd)))
         (cwd-route-looking-p
          (and cwd (string-match-p "\\`/[^/:]+:" cwd)))
         (host (and route (pilish--session-route-host route))))
    (when (and cwd
               (not (pilish--session-unsafe-cwd-p cwd))
               ;; A cwd carrying its own route must agree with the
               ;; file's; a malformed TRAMP-looking prefix is not a
               ;; local POSIX component and must not bypass this check.
               (or cwd-route (not (and route cwd-route-looking-p)))
               (or (null cwd-route) (equal cwd-route route))
               ;; A malformed or context-defaulted route cannot
               ;; identify a project.
               (or (null route) host))
      (let* ((localname (if cwd-route
                            (substring cwd (length cwd-route))
                          cwd))
             (parts (and (not (string-empty-p localname))
                         (pilish--session-cwd-parts localname))))
        (when parts
          (let* ((kind (nth 0 parts))
                 (components (nth 1 parts))
                 (joined (mapconcat #'identity components "/"))
                 (path (pcase kind
                         ('posix (concat "/" joined))
                         ('unc (concat "//" joined))
                         ;; The drive component already carries the
                         ;; colon; a rooted POSIX spelling of the same
                         ;; text keeps its leading slash.
                         (_ joined))))
            (list (concat route path) host components)))))))

(defun pilish--session-ordinary-local-cwd-p (cwd)
  "Return non-nil when raw CWD is safe for local canonicalization.
Only a validated single-slash POSIX spelling qualifies.  UNC names,
Windows drives, complete remote routes, and ambiguous first components
such as /ssh:h:relative remain lexical and cannot activate a file-name
handler."
  (and (stringp cwd)
       (not (pilish--session-unsafe-cwd-p cwd))
       (string-prefix-p "/" cwd)
       (not (string-prefix-p "//" cwd))
       (not (string-match-p "\\`/[^/:]+:" cwd))))

(defun pilish--session-local-file-truename (path)
  "Return local `file-truename' for PATH with all handlers inhibited.
PATH has already passed `pilish--session-ordinary-local-cwd-p'.
Binding `file-name-handler-alist' to nil creates the scan-time local
filesystem boundary: arbitrary handlers cannot perform hidden IO or
turn a local project identity into a remote spelling."
  (let ((file-name-handler-alist nil))
    (file-truename path)))

(defun pilish--session-canonical-project-spec (session)
  "Return SESSION's project spec with a canonical local POSIX cwd.
Lexically valid remote routes, UNC names, Windows drives, and
TRAMP-looking ambiguous local spellings remain pure strings and never
reach `file-truename'.  For a validated ordinary local raw cwd,
`file-truename' runs with file-name handlers inhibited *before* lexical
dot-segment collapse, preserving kernel path-walk semantics across a
symlink followed by `..'.  The result must itself remain an ordinary
local POSIX spelling; a failure or nonlocal/route-looking result keeps
the validated lexical spec."
  (let* ((cwd (pilish--normalize-string-or-null
               (plist-get session :cwd)))
         (session-route (pilish--session-syntactic-route
                         (plist-get session :path)))
         (cwd-route (and cwd (pilish--session-syntactic-route cwd)))
         ;; Resolve the untouched raw cwd first.  The lexical spec is
         ;; deliberately computed afterward so no prior dot collapse
         ;; can change symlink/.. path-walk semantics.
         (canonical
          (and (null session-route)
               (null cwd-route)
               (pilish--session-ordinary-local-cwd-p cwd)
               (condition-case nil
                   (pilish--session-local-file-truename cwd)
                 (error nil))))
         (spec (pilish--session-project-spec-lexical session)))
    (when spec
      (if (not (pilish--session-ordinary-local-cwd-p canonical))
          spec
        (pcase (pilish--session-cwd-parts canonical)
          (`(posix ,components)
           (list (concat "/" (mapconcat #'identity components "/"))
                 nil components))
          (_ spec))))))

(defun pilish--session-project-spec (session)
  "Return (IDENTITY HOST COMPONENTS) for SESSION's project, or nil.
An enriched scan item carries `:canonicalProjectSpec': local POSIX
cwd symlinks were resolved during the scan with lexical fallback,
while remote routes, UNC names, and Windows drives stayed lexical.
A direct/un-enriched item is parsed lexically without filesystem or
TRAMP configuration access.

IDENTITY combines the complete validated TRAMP route, when remote,
with normalized cwd components.  HOST is the route's effective
final-hop host (explicit or inherited).  Empty/relative/remote-home
localnames, route disagreement, a malformed or context-defaulted
route, and display-unsafe metadata yield nil.  See
`pilish--session-cwd-parts', `pilish--session-route-host', and
`pilish--session-canonical-project-spec'."
  (or (plist-get session :canonicalProjectSpec)
      (pilish--session-project-spec-lexical session)))

(defconst pilish--session-project-token-width 16
  "Display width of the project token field on All-projects rows.
The token and the two-column live field stay inside this bound plus
two columns, so identity and live status remain visible in a
32-column body no matter how deep a fork connector, how long a
common prefix, or how wide a project name is.")

(defconst pilish--session-project-placeholder "?"
  "Placeholder token for a row with no usable project.
Shown on All-projects rows whose recorded cwd was rejected or
absent, so the field stays reserved and truthful rather than
silently shifting the row's layout.")

(defun pilish--session-project-label (host components depth)
  "Return the unbounded project label for HOST and COMPONENTS at DEPTH.
DEPTH trailing components are shown — the whole path when DEPTH
exhausts COMPONENTS — or `/` for the root; a remote project prefixes
its effective HOST and a colon."
  (let ((path (if components
                  (mapconcat #'identity (last components depth) "/")
                "/")))
    (if host (concat host ":" path) path)))

(defun pilish--session-pad-display (string width)
  "Left-justify STRING with spaces to exactly display WIDTH columns.
Padding is display-width aware, so tokens containing wide characters
still align the following field."
  (concat string (make-string (max 0 (- width (string-width string)))
                              ?\s)))

(defun pilish--session-project-display-key (label)
  "Return LABEL's compatibility-normalized exact display-field key.
NFKC makes canonical and compatibility-equivalent readable labels
share one allocation group — for example, ASCII SPACE and EN SPACE
inside otherwise identical text.  Exact fixed-width padding preserves
established trailing-space collision handling."
  (pilish--session-pad-display
   (ucs-normalize-NFKC-string label)
   pilish--session-project-token-width))

(defun pilish--session-blank-glyph-char-p (char)
  "Return non-nil when CHAR's Unicode name denotes a blank glyph.
General category and column width do not identify BRAILLE PATTERN
BLANK or the Hangul filler family.  A conservative Unicode-name policy
catches names ending in BLANK or FILLER without maintaining fragile
code-point ranges or classifying visible symbols merely containing the
word elsewhere in their name."
  (when-let* ((name (get-char-code-property char 'name)))
    (and (stringp name)
         (string-match-p "\\(?:BLANK\\|FILLER\\)\\'" name))))

(defun pilish--session-project-natural-label-unsafe-p (label)
  "Return non-nil when LABEL must not become a natural token.
Whitespace-only labels and Unicode BLANK/FILLER glyphs can render as
an apparently empty field.  Any zero-width character can hide a
distinct legal cwd spelling.  These labels move to the ordinal-front
generated namespace instead of having their legal identities rejected."
  (or (and (not (string-empty-p label))
           (cl-every
            (lambda (char)
              (eq (get-char-code-property char 'general-category) 'Zs))
            label))
      (cl-some #'pilish--session-blank-glyph-char-p label)
      (cl-some #'pilish--session-zero-width-char-p label)))

(defun pilish--session-ordinal36 (n width)
  "Return N as `#' plus WIDTH base-36 digits, zero-padded.
The leading `#' reserves the generated namespace: no readable label
can spell a generated token, so the two never collide.  A number
too large for WIDTH signals instead of silently truncating."
  (unless (< 0 n (expt 36 width))
    (error "Ordinal %d exceeds width %d" n width))
  (let ((digits ""))
    (dotimes (_ width)
      (setq digits (format "%c%s"
                           (aref "0123456789abcdefghijklmnopqrstuvwxyz"
                                 (mod n 36))
                           digits)
            n (/ n 36)))
    (concat "#" digits)))

(defun pilish--session-ordinal-width (count)
  "Return the base-36 digit width covering COUNT ordinals.
Integer arithmetic throughout, so exact powers of 36 roll over
cleanly: 35 needs one digit, the 36th needs two."
  (let ((width 1) (capacity 36))
    ;; Ordinals are one-based, so WIDTH digits cover only
    ;; 1..(36^WIDTH - 1); the exact power needs another digit.
    (while (>= count capacity)
      (cl-incf width)
      (setq capacity (* capacity 36)))
    width))

(defun pilish--session-project-fields (items &optional ellipsis)
  "Return a hash table mapping session key to its All-projects token.
Tokens are built from every project in ITEMS — the full loaded set,
not the filtered rows — so a query never silently relabels the rows
it leaves behind.  Rows whose cwd was rejected map to the
placeholder instead; the field is always reserved.  When ELLIPSIS is
non-nil, it marks any generated-tail truncation; ordinary rows retain
their established compact field when it is nil.

Allocation is deterministic and exact.  Identities are deduplicated
and sorted once, each grows the shortest readable label that
distinguishes it (see `pilish--session-project-label'), and all
candidates are grouped by their NFKC-normalized exact padded display
field (see `pilish--session-project-display-key').  A group yields a
natural token only when it holds exactly one candidate whose label
fits the token width, is not blank-looking or zero-width-bearing,
and spells neither the reserved `#' prefix nor the placeholder's own
field.  Thus trailing-space, canonical-Unicode, and compatibility-Unicode
equivalents move together to the generated class; blank/filler and
zero-width-bearing identities move there even alone; and a project
literally named `?' cannot impersonate
the placeholder.  Every other identity — over width, reserved,
display-unsafe, or field-colliding, with its whole group — gets a
generated token `#ORD tail': fixed-width
base-36 ordinals over the sorted class (width scaled to the class
size by `pilish--session-ordinal-width'), plus the label's basename
truncated to the remaining display columns (marked by ELLIPSIS when
non-nil).  Ordinals lead, so generated
fields are pairwise unique and never collide with naturals; the
shape holds for any practical archive (15 ordinal columns cover
36^15 - 1 positive ordinals)."
  (let* ((info (make-hash-table :test 'equal))
         (table (make-hash-table :test 'equal))
         ;; Session key -> spec, computed once per item.
         (key-specs
          (let ((ks nil))
            (dolist (item items)
              (let ((spec (pilish--session-project-spec item)))
                (when spec
                  ;; Rejected cwds contribute no identity; their rows
                  ;; map to the placeholder below.
                  (puthash (nth 0 spec)
                           (list (nth 1 spec) (nth 2 spec))
                           info))
                (push (cons (pilish--session-item-key item) spec) ks)))
            ks)))
    (let* ((identities (sort (hash-table-keys info) #'string<))
           (depth (make-hash-table :test 'equal))
           (label
            (lambda (id)
              (pilish--session-project-label
               (nth 0 (gethash id info))
               (nth 1 (gethash id info))
               (gethash id depth)))))
      ;; Grow the shortest distinguishing label per identity.
      (dolist (id identities)
        (puthash id (min 1 (length (nth 1 (gethash id info))))
                 depth))
      (catch 'stable
        (while t
          (let ((groups (make-hash-table :test 'equal))
                (extended nil))
            (dolist (id identities)
              (push id (gethash (funcall label id) groups)))
            (maphash
             (lambda (_ ids)
               (when (> (length ids) 1)
                 (dolist (id ids)
                   (when (< (gethash id depth)
                            (length (nth 1 (gethash id info))))
                     (puthash id (1+ (gethash id depth)) depth)
                     (setq extended t)))))
             groups)
            (unless extended (throw 'stable nil)))))
      ;; One grouping by normalized exact display field decides
      ;; natural vs generated, placeholder field included as reserved.
      (let* ((placeholder-key
              (pilish--session-project-display-key
               pilish--session-project-placeholder))
             (by-field (make-hash-table :test 'equal))
             (natural (make-hash-table :test 'equal))
             (generated nil))
        (dolist (id identities)
          (let* ((lab (funcall label id))
                 (key (pilish--session-project-display-key lab)))
            (push id (gethash key by-field))))
        (puthash placeholder-key
                 (cons '(reserved . placeholder)
                       (gethash placeholder-key by-field))
                 by-field)
        (dolist (id identities)
          (let* ((lab (funcall label id))
                 (key (pilish--session-project-display-key lab))
                 (candidates (gethash key by-field)))
            (if (and (= (length candidates) 1)
                     (<= (string-width lab)
                         pilish--session-project-token-width)
                     (not (pilish--session-project-natural-label-unsafe-p
                           lab))
                     ;; Reserve the generated prefix by compatibility
                     ;; appearance too (for example SMALL NUMBER SIGN).
                     (not (string-prefix-p
                           "#" (ucs-normalize-NFKC-string lab))))
                (puthash id lab natural)
              (push id generated))))
        ;; Generated tokens: ordinal width scaled to the class size,
        ;; tail truncated to the columns the ordinal leaves.
        (let* ((class (sort (copy-sequence generated) #'string<))
               (width (pilish--session-ordinal-width (length class)))
               (tail-width (max 0 (- pilish--session-project-token-width
                                     width 2)))
               (ordinal 0))
          (dolist (id class)
            (cl-incf ordinal)
            (puthash id
                     (concat (pilish--session-ordinal36 ordinal width)
                             (if (> tail-width 0)
                                 (concat " "
                                         (truncate-string-to-width
                                          (or (car (last
                                                    (split-string
                                                     (funcall label id) "/")))
                                              "/")
                                          tail-width 0 nil ellipsis))
                               ""))
                     natural)))
        ;; Map every item's session key to its project's token, or the
        ;; placeholder when the recorded cwd was rejected.
        (dolist (entry key-specs)
          (puthash (car entry)
                   (if (cdr entry)
                       (gethash (nth 0 (cdr entry)) natural
                                pilish--session-project-placeholder)
                     pilish--session-project-placeholder)
                   table))
        table))))

(defun pilish--propertize-face (string face)
  "Propertize STRING with both `face' and `font-lock-face' set to FACE.
This follows Magit's convention to survive fontification."
  (propertize string 'face face 'font-lock-face face))

(defun pilish--make-margin-overlay (string)
  "Create a right-margin overlay on the current line displaying STRING.
The overlay uses `evaporate' so it auto-removes when the buffer text
is deleted (e.g., during erase-and-rewrite refresh).
STRING defaults to a single space if nil."
  (save-excursion
    (forward-line (if (bolp) -1 0))
    (let ((o (make-overlay (1+ (point)) (line-end-position) nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize "o" 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

(defconst pilish--session-margin-width 20
  "Right margin width for the session browser.
Accommodates: count (4 digits + \" msgs \") + age (2 + 1 + 7) + padding.
4 + 5 + 10 = 19, plus 1 char left padding = 20.")

(defconst pilish--tree-margin-width 16
  "Right margin width for the tree browser.
Accommodates: \"[\" + 12-char label + \"]\" + padding = 16.")

(defvar-local pilish--browse-margin-width nil
  "Right margin width for the current browse buffer.
Set by the derived mode; used by the window-configuration hook.")

(defun pilish--browse-set-window-margins (width &optional window)
  "Set right margin to WIDTH on WINDOW (default: selected window).
Preserves any existing left margin."
  (let ((win (or window (selected-window))))
    (when (window-live-p win)
      (set-window-margins win (car (window-margins win)) width))))

(defun pilish--browse-apply-margins ()
  "Re-apply right margins for the current browse buffer.
Reads width from `pilish--browse-margin-width'.
Intended as a `window-configuration-change-hook' callback."
  (when pilish--browse-margin-width
    (pilish--browse-set-window-margins
     pilish--browse-margin-width)))

;;;; Margin Age Formatting

(defconst pilish--age-spec
  '(("year"   31557600)
    ("month"   2629800)
    ("week"     604800)
    ("day"       86400)
    ("hour"       3600)
    ("minute"       60)
    ("second"        1))
  "Time units and their durations in seconds.
Used for margin age display in browse buffers.")

(defun pilish--margin-age (seconds)
  "Convert SECONDS to a (COUNT . UNIT) pair.
Returns the largest unit where COUNT >= 1, or (0 . \"second\") for zero."
  (let ((result (cons 0 "second")))
    (cl-loop for (unit secs) in pilish--age-spec
             when (>= seconds secs)
             do (setq result (cons (floor (/ (float seconds) secs)) unit))
             and return nil)
    result))

(defconst pilish--margin-age-unit-width
  (apply #'max (mapcar (lambda (s) (length (concat (car s) "s")))
                       pilish--age-spec))
  "Width of the longest pluralized unit name (\"minutes\" = 7).")

(defconst pilish--margin-age-format
  (format "%%2d %%-%ds" pilish--margin-age-unit-width)
  "Format string for margin age: \"%2d %-7s\".")

(defun pilish--format-margin-age (seconds)
  "Format SECONDS as a magit-log–style aligned age string.
Format: \"%2d %-Ns\" where N is the longest pluralized unit width.
Example: \" 5 minutes\", \" 1 hour   \", \"10 days   \"."
  (let* ((pair (pilish--margin-age seconds))
         (count (car pair))
         (unit (cdr pair))
         (unit-str (if (= count 1) unit (concat unit "s"))))
    (format pilish--margin-age-format count unit-str)))

(defun pilish--format-margin-age-from-iso (iso-timestamp)
  "Format ISO-TIMESTAMP as a margin age string.
Returns nil on invalid input."
  (condition-case nil
      (let* ((time (date-to-time iso-timestamp))
             (diff (floor (float-time (time-subtract (current-time) time)))))
        (pilish--format-margin-age (max 0 diff)))
    (error nil)))

;;;; Tree Helpers

(defun pilish--active-path-ids (tree leaf-id)
  "Compute the total set of string node IDs on TREE's active path.
LEAF-ID is the current leaf node ID.  Return a hash table mapping
active node IDs to t.  A malformed tree with duplicate addressable ids
fails closed to an empty path: no occurrence can truthfully receive an
active/current marker.  A canonical node carrying :ambiguousId is a
hard ancestry boundary: omit it and stop, because ancestors reached only
by choosing one conflicting occurrence are not truthful either.  A
unique node carrying :ambiguousParent is marked itself, then stops before
the parent; projection uses that flag when a filtered ambiguous record
was promoted away.  A seen set bounds malformed unique-id parent cycles.
Nil/legacy ids are never markers."
  (let ((result (make-hash-table :test 'equal))
        (leaf-id (pilish--normalize-string-or-null leaf-id)))
    (when (and leaf-id (pilish-jsonl-tree-ids-unique-p tree))
      ;; Build the parent-id lookup from the finite nested tree.
      (let ((parent-map (make-hash-table :test 'equal))
            (ambiguous (make-hash-table :test 'equal))
            (ambiguous-parent (make-hash-table :test 'equal))
            (stack (append tree nil)))
        (while stack
          (let* ((node (pop stack))
                 (parent-id (pilish--normalize-string-or-null
                             (plist-get node :id)))
                 (children (plist-get node :children)))
            (when parent-id
              (when (plist-get node :ambiguousId)
                (puthash parent-id t ambiguous))
              (when (plist-get node :ambiguousParent)
                (puthash parent-id t ambiguous-parent)))
            (when (vectorp children)
              (dotimes (i (length children))
                (let* ((child (aref children i))
                       (child-id (pilish--normalize-string-or-null
                                  (plist-get child :id))))
                  (when child-id
                    (puthash child-id parent-id parent-map))
                  (push child stack))))))
        ;; Walk from leaf to root.  Malformed declared parent links can
        ;; still induce a cycle, so mark each addressable id once.
        (let ((current leaf-id)
              (seen (make-hash-table :test 'equal)))
          (while (and current (not (gethash current seen)))
            (puthash current t seen)
            (if (gethash current ambiguous)
                (setq current nil)
              (puthash current t result)
              (setq current
                    (unless (gethash current ambiguous-parent)
                      (gethash current parent-map))))))))
    result))

;;;; Tree Filter Predicates

(defconst pilish--empty-assistant-preview "(no content)"
  "Preview string the RPC projection sets for assistant messages with no text.
Used as a heuristic to detect tool-dispatch-only assistant messages.")

(defun pilish--browse-node-empty-assistant-p (node)
  "Return non-nil if NODE is an empty assistant message.
Empty assistants have no text content — typically tool-dispatch messages
containing only toolCall blocks.  Detected via the preview string heuristic.
Aborted or errored messages are NOT considered empty."
  (let ((type (plist-get node :type))
        (role (plist-get node :role)))
    (and (equal type "message")
         (equal role "assistant")
         (let ((preview (or (plist-get node :preview) "")))
           (or (string-empty-p preview)
               (equal preview pilish--empty-assistant-preview)))
         (not (equal (plist-get node :stopReason) "aborted"))
         (not (plist-get node :errorMessage)))))

(defun pilish--browse-node-visible-p (node filter-mode)
  "Return non-nil if NODE should be visible under FILTER-MODE.
FILTER-MODE is one of: `default', `no-tools', `user-only',
`labeled-only', `all'.
NODE is a tree node plist.

Filtering is two-phase (matching TUI tree-selector.ts:282-311):
  Phase 1 — universal pre-filter: empty assistant messages are always
            hidden regardless of mode (unless aborted or carrying an
            error message).
  Phase 2 — mode-specific filter: each mode defines additional rules."
  (if (pilish--browse-node-empty-assistant-p node)
      ;; Phase 1: universal pre-filter — empty assistants always hidden
      nil
    ;; Phase 2: mode-specific filter.  Settings-like rows — model and
    ;; thinking changes, plus the leading system prompt (projected so
    ;; parent chains resolve onto it, but hidden like settings noise;
    ;; the TUI shows the system row even under its default filter) —
    ;; share one predicate, mirroring pi's isSettingsEntry structure.
    (let* ((type (plist-get node :type))
           (role (plist-get node :role))
           (settings-like-p
            (or (member type '("model_change" "thinking_level_change"))
                (and (equal type "message") (equal role "system")))))
      (pcase filter-mode
        ('all t)
        ('labeled-only
         (and (plist-get node :label) t))
        ('user-only
         (and (equal type "message") (equal role "user")))
        ('no-tools
         (and (not settings-like-p)
              (not (equal type "tool_result"))))
        (_ ;; `default'
         (not settings-like-p))))))

;;;; Client-Side Search/Filter

(defun pilish--matches-filter-p (text tokens)
  "Return non-nil if TEXT matches all regexp TOKENS.
Each whitespace-separated token is a regexp.
All tokens must match for the entry to be included."
  (or (null tokens)
      (cl-every (lambda (tok) (string-match-p tok text)) tokens)))

(defconst pilish--tree-semantic-tool-preview-names
  '("read" "write" "edit" "bash" "grep" "find" "ls")
  "Tool names whose projected previews contain selected semantic fields.
Other tool previews can contain arbitrary JSON arguments, which tree
search deliberately excludes.")

(defun pilish--tree-node-searchable-text (node)
  "Return the searchable semantic text of projected tree NODE.
The corpus includes the known display text, label, type and role,
summary, tool name, and model/thinking metadata.  Projected message
previews contain text blocks only, so image and thinking payloads never
enter the corpus.  Raw `:toolArgs' are never serialized; a custom tool's
JSON-formatted call preview is excluded as well."
  (let* ((type (plist-get node :type))
         (role (plist-get node :role))
         (tool-name (plist-get node :toolName))
         (tool-preview-safe-p
          (or (not (equal type "tool_result"))
              (not (plist-member node :toolArgs))
              (member tool-name pilish--tree-semantic-tool-preview-names)))
         (parts nil))
    (dolist (value
             (append
              (list (plist-get node :label)
                    type
                    (and (stringp type)
                         (replace-regexp-in-string "_" " " type))
                    role
                    (and (stringp role)
                         (replace-regexp-in-string "_" " " role))
                    (plist-get node :rawRole)
                    (plist-get node :customType)
                    tool-name
                    (plist-get node :summary)
                    (plist-get node :errorMessage)
                    (plist-get node :stopReason)
                    (plist-get node :provider)
                    (plist-get node :modelId)
                    (plist-get node :thinkingLevel))
              (when tool-preview-safe-p
                (list (plist-get node :preview)
                      (pilish--tree-node-preview node)))))
      (when (stringp value)
        (push value parts)))
    (mapconcat #'identity (nreverse parts) " ")))

;;;; Tree Flattening for Display

(defun pilish--flatten-tree-for-display
    (tree leaf-id filter-mode &optional search-tokens published-values)
  "Return TREE's visible nodes as (NODE DEPTH PREFIX) rows.
LEAF-ID identifies the current leaf for active-branch-first ordering.
FILTER-MODE controls browser filtering, and SEARCH-TOKENS are the
all-regexp-token query applied to each node's semantic text.

DEPTH is ancestry depth in the final displayed topology.  It deliberately
is not inferred from PREFIX: a one-child chain has no connector glyphs but
still has parent/child structure.  When PUBLISHED-VALUES is a hash table,
record every addressable, unambiguous, universally displayable string id,
including ids hidden by the current filter or search.  The browser uses that
set to prune fold state only when nodes leave the published tree, not when
they temporarily leave the display.

Filtering and search happen before topology is derived.  Hidden nodes'
children attach to their nearest visible ancestor; visible roots,
sibling connectors, depth, and ancestor gutters therefore represent
only the final visible set.  Both passes use explicit stacks so deeply
nested conversations remain safe."
  (let ((active-ids (pilish--active-path-ids tree leaf-id))
        (visible-children (make-hash-table :test 'eq))
        (stack nil))
    ;; First pass: retain visible nodes and group them under the nearest
    ;; visible ancestor.  Stack items are (NODE NEAREST-VISIBLE-PARENT).
    (dolist (root (reverse (append tree nil)))
      (push (list root nil) stack))
    (while stack
      (pcase-let ((`(,node ,visible-parent) (pop stack)))
        (when (and published-values
                   (not (plist-get node :ambiguousId))
                   ;; Universal pre-filter exclusions are not published
                   ;; browser ids under any view.
                   (pilish--browse-node-visible-p node 'all))
          (when-let* ((id (pilish--normalize-string-or-null
                           (plist-get node :id))))
            (puthash id t published-values)))
        (let* ((visible-p
                (and (pilish--browse-node-visible-p node filter-mode)
                     (or (null search-tokens)
                         (pilish--matches-filter-p
                          (pilish--tree-node-searchable-text node)
                          search-tokens))))
               (next-parent (if visible-p node visible-parent))
               (children (plist-get node :children))
               (child-list (and (vectorp children)
                                (append children nil)))
               (ordered-children
                (if (> (length child-list) 1)
                    (pilish--sort-active-first child-list active-ids)
                  child-list)))
          (when visible-p
            (puthash visible-parent
                     (cons node (gethash visible-parent visible-children))
                     visible-children))
          (dolist (child (reverse ordered-children))
            (push (list child next-parent) stack)))))
    ;; Consing during pre-order built every sibling list backwards.
    (maphash (lambda (parent children)
               (puthash parent (nreverse children) visible-children))
             visible-children)
    (pilish--flatten-visible-tree visible-children)))

(defun pilish--flatten-visible-tree (visible-children)
  "Flatten VISIBLE-CHILDREN with freshly derived visual topology.
VISIBLE-CHILDREN maps each visible parent node (or nil for a visible
root) to its ordered visible children.  Return (NODE INDENT PREFIX)
rows without consulting any hidden node."
  (let ((stack nil)
        (result nil))
    ;; Multiple visible roots are independent roots, not siblings under
    ;; an implied displayed parent, so they never receive connectors.
    (dolist (root (reverse (gethash nil visible-children)))
      (push (list root 0 nil nil t) stack))
    ;; Stack items: (NODE INDENT GUTTERS BRANCH-CHILD-P LAST-P).
    (while stack
      (pcase-let ((`(,node ,indent ,gutters ,branch-child-p ,last-p)
                   (pop stack)))
        (let* ((connector
                (when branch-child-p (if last-p "└─ " "├─ ")))
               (prefix (concat (apply #'concat gutters)
                               (or connector "")))
               (children (gethash node visible-children))
               (child-count (length children))
               (children-branch-p (> child-count 1))
               ;; Logical ancestry always advances, including a
               ;; connector-free one-child chain.  PREFIX remains a
               ;; separate visual concern.
               (child-indent (1+ indent))
               (child-gutters
                (if branch-child-p
                    (append gutters (list (if last-p "   " "│  ")))
                  gutters))
               (index (1- child-count)))
          (push (list node indent prefix) result)
          ;; Reverse push preserves each visible sibling's active-first
          ;; order when the LIFO stack is consumed.
          (dolist (child (reverse children))
            (push (list child child-indent child-gutters
                        children-branch-p
                        (= index (1- child-count)))
                  stack)
            (setq index (1- index))))))
    (nreverse result)))

(defun pilish--sort-active-first (children active-ids)
  "Sort CHILDREN so the subtree containing an active node comes first.
ACTIVE-IDS is the hash table of active path node IDs."
  (let ((active nil)
        (inactive nil))
    (dolist (child children)
      (if (pilish--subtree-contains-active-p child active-ids)
          (push child active)
        (push child inactive)))
    (append (nreverse active) (nreverse inactive))))

(defun pilish--subtree-contains-active-p (node active-ids)
  "Return non-nil if NODE or any descendant is in ACTIVE-IDS.
Uses iterative DFS to avoid stack overflow on deep trees."
  (let ((stack (list node)))
    (cl-block found
      (while stack
        (let* ((n (pop stack))
               (children (plist-get n :children)))
          (when (gethash (plist-get n :id) active-ids)
            (cl-return-from found t))
          (when (vectorp children)
            (dotimes (i (length children))
              (push (aref children i) stack)))))
      nil)))

;;;; Session View/Filter/Threading

(defconst pilish--session-view-modes
  '(threaded recent messages)
  "Available session browser views, in cycle order.
`threaded' is Threaded (fork families), `recent' is Recent activity,
and `messages' is Most messages — see
`pilish--session-view-label' for the user-facing names.")

(defun pilish--session-view-next (current)
  "Return the view after CURRENT in the cycle."
  (let ((modes pilish--session-view-modes))
    (or (cadr (member current modes))
        (car modes))))

(defun pilish--session-view-label (view)
  "Return the user-facing label for the session browser VIEW.
Threaded is a hierarchy change, not a sort, so the label spells out
what it groups; the message-count view counts records and ranks
nothing, so it is never called relevance or Fuzzy."
  (pcase view
    ('threaded "Threaded (fork families)")
    ('recent "Recent activity")
    ('messages "Most messages")))

(defun pilish--session-scope-label (scope)
  "Return the user-facing label for the session browser SCOPE."
  (pcase scope
    ('all "All projects")
    ('current "This project")))

(defun pilish--session-sort-items (items view)
  "Order session ITEMS for the flat views of VIEW.
`recent' (Recent activity) orders by the session file's mtime,
descending.  `messages' (Most messages) orders by the count of
persisted message records — tool-result records included —
descending.  `threaded' returns ITEMS as-is; the Threaded (fork
families) view arranges rows during rendering (see
`pilish--session-thread-items'), and a query flattens it to
newest-first rows.
Missing mtimes are the oldest known value, matching
the subtree activity fallback; ties fall back to canonical identity
so scan order never leaks."
  (pcase view
    ('recent
     (sort (copy-sequence items)
           (lambda (a b)
             (let ((ma (or (plist-get a :modified) ""))
                   (mb (or (plist-get b :modified) "")))
               (if (not (equal ma mb))
                   (string> ma mb)
                 (string< (or (plist-get a :canonicalPath)
                              (plist-get a :path) "")
                          (or (plist-get b :canonicalPath)
                              (plist-get b :path) "")))))))
    ('messages
     (sort (copy-sequence items)
           (lambda (a b)
             ;; Equal message counts fall back to canonical identity
             ;; so scan order never leaks, matching Recent.
             (let ((ca (or (plist-get a :messageCount) 0))
                   (cb (or (plist-get b :messageCount) 0)))
               (if (/= ca cb)
                   (> ca cb)
                 (string< (or (plist-get a :canonicalPath)
                              (plist-get a :path) "")
                          (or (plist-get b :canonicalPath)
                              (plist-get b :path) "")))))))
    (_ items)))

(defun pilish--canonical-session-path (path &optional memo anchor)
  "Return PATH's canonical spelling for session family identity.
Remote spellings keep their full TRAMP route as the identity — never
resolved, so no remote filesystem is contacted and distinct routes
stay distinct identities.  Local spellings resolve symlinks along
the whole path through `file-truename', so alias spellings of one
file — a symlinked sessions directory or a symlinked session file
itself — denote one family.  A spelling `file-truename' cannot
resolve, such as a symlink cycle, keeps its lexical form instead of
signaling — like pi's realpath fallback.  MEMO, when non-nil, is a
hash table caching canonical spellings across calls; ANCHOR, when
non-nil, anchors relative spellings as
`pilish--route-preserving-expand-file-name' does."
  (when (stringp path)
    (let ((expanded (pilish--route-preserving-expand-file-name path anchor)))
      (if (pilish--remote-prefix-for-path expanded)
          expanded
        (let ((memo (or memo (make-hash-table :test 'equal))))
          (or (gethash expanded memo)
              (puthash expanded
                       (condition-case nil
                           (file-truename expanded)
                         (error expanded))
                       memo)))))))

(defun pilish--session-item-key (item &optional memo)
  "Return ITEM's canonical identity key.
A present `:canonicalPath' (see `pilish--session-enrich-item') wins,
even when nil; otherwise the key is computed from `:path' with MEMO,
as direct callers rendering hand-built items still do."
  (if (plist-member item :canonicalPath)
      (plist-get item :canonicalPath)
    (pilish--canonical-session-path (plist-get item :path) memo)))

(defconst pilish--unresolved-parent-session 'unresolved
  "Sentinel `:canonicalParentSession' for unresolvable fork headers.
Truthiness stops the render-time fallback from recomputing a path
that already failed during the scan; it matches no item identity, so
the fork renders as an ordinary root.")

(defun pilish--session-parent-anchor (item)
  "Return the anchor for ITEM's relative fork-header spellings.
Pi resolves relative paths against its process working directory —
recorded as the session header's `cwd', which pi's own fork code
writes (`cwd: this.cwd') and `resolvePath' defaults to — so that
recorded directory is the deterministic anchor, but only when it is
absolute: pi records an absolute working directory, and a relative
value would otherwise expand against the ambient buffer.  When the
header lacks a usable `cwd', the child session file's own directory
anchors instead — route-preserving, so a multi-hop TRAMP child
keeps its complete route — and pi's fork flow creates the fork next
to its parent.  Nil when neither yields a stable anchor."
  (let ((cwd (plist-get item :cwd)))
    (if (and (stringp cwd) (file-name-absolute-p cwd))
        cwd
      (pilish--route-preserving-file-name-directory
       (plist-get item :path)))))

(defun pilish--session-enrich-item (item)
  "Return ITEM with its canonical identities resolved for rendering.
Computes `:canonicalPath', `:canonicalProjectSpec', and, for forks,
`:canonicalParentSession' (see `pilish--thread-parent-identity').
Local session paths and POSIX project cwds resolve symlinks with
lexical fallback; project routes are validated syntactically, and
remote/UNC/Windows project spellings never reach `file-truename'.
Thus family, project-token, and live-marker rendering needs no
archive-sized or remote canonicalization later — only the few live
process session paths canonicalize locally per render.

Enrichment runs inside scan slices under cancellation and quit
handling.  An ordinary resolution failure degrades just that
relationship instead of discarding the file or aborting the scan:
an unresolvable fork becomes an orphan, while session and project
paths keep their safe lexical spellings."
  (let* ((key (condition-case nil
                  (pilish--canonical-session-path (plist-get item :path))
                (error (plist-get item :path))))
         (project-spec
          (condition-case nil
              (pilish--session-canonical-project-spec item)
            (error (pilish--session-project-spec-lexical item))))
         (parent-path (plist-get item :parentSessionPath)))
    (append item
            (list :canonicalPath key)
            (when project-spec
              (list :canonicalProjectSpec project-spec))
            (when parent-path
              (list :canonicalParentSession
                    (or (condition-case nil
                            (pilish--thread-parent-identity
                             parent-path key nil
                             (pilish--session-parent-anchor item))
                          (error pilish--unresolved-parent-session))
                        ;; No stable anchor is as unresolvable as an
                        ;; error: keep the sentinel so renders never
                        ;; recompute against the ambient buffer.
                        pilish--unresolved-parent-session))))))

(defconst pilish--session-canonicalization-stale
  (make-symbol "pilish-session-canonicalization-stale")
  "Sentinel returned when generation-guarded item canonicalization is stale.")

(defun pilish--session-canonicalize-items (items &optional buf generation)
  "Return ITEMS deduplicated by canonical identity, one per session.
Items carrying stored keys (see `pilish--session-enrich-item') dedupe
by pure hash lookups; hand-built items without keys get
`:canonicalPath' computed here.  Items with equivalent identities —
two discovered spellings of one file, such as symlink aliases —
collapse to the first spelling, so all views render one row per
session.  Input plists are never mutated.

When BUF and GENERATION are non-nil, check ownership immediately before
and after each callback-capable key computation.  Return
`pilish--session-canonicalization-stale' rather than a partial result if
a newer fetch takes ownership."
  (let ((memo (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (result nil)
        (guarded (and buf generation)))
    (catch 'stale
      (dolist (item items)
        (when (and guarded
                   (not (pilish--session-browser-generation-current-p
                         buf generation)))
          (throw 'stale pilish--session-canonicalization-stale))
        (let* ((stored-p (plist-member item :canonicalPath))
               (key (if stored-p
                        (plist-get item :canonicalPath)
                      (pilish--canonical-session-path
                       (plist-get item :path) memo)))
               (entry (if stored-p item
                        (append item (list :canonicalPath key)))))
          (when (and guarded
                     (not (pilish--session-browser-generation-current-p
                           buf generation)))
            (throw 'stale pilish--session-canonicalization-stale))
          (unless (gethash key seen)
            (puthash key t seen)
            (push entry result))))
      (nreverse result))))

(defun pilish--thread-parent-identity (parent-path child-key &optional memo anchor)
  "Return PARENT-PATH's canonical identity resolved against CHILD-KEY.
A spelling with its own TRAMP route is its own identity; a prefix-free
spelling — what pi records in a fork header — is anchored to the
child's own route first, so a shared local name can never attach to
another route's item and an all-remote family never touches the local
filesystem.  Relative spellings expand against ANCHOR — the child's
recorded working directory (see `pilish--session-parent-anchor'), as
pi resolves them — never the ambient buffer; a relative spelling
with no absolute anchor has no stable identity and resolves to nil.
MEMO caches local canonicalization."
  (when (stringp parent-path)
    (if (and (not (file-name-absolute-p parent-path))
             (not (and (stringp anchor)
                       (file-name-absolute-p anchor))))
        nil
      (let* ((expanded (pilish--route-preserving-expand-file-name
                        parent-path anchor))
             (child-prefix (pilish--remote-prefix-for-path child-key)))
        (cond
         ((pilish--remote-prefix-for-path expanded) expanded)
         (child-prefix (concat child-prefix expanded))
         (t (pilish--canonical-session-path parent-path memo anchor)))))))

(defun pilish--session-thread-items (items &optional buf generation)
  "Arrange ITEMS into fork-family rows while BUF owns GENERATION.
Return a list of (ITEM PREFIX DEPTH) rows.  DEPTH is fork-family
ancestry depth; it stays structural even when connector glyphs change.
ITEMS carry one row per canonical identity, as
`pilish--session-canonicalize-items' ensures.
Families use `:parentSessionPath' only when the parent is present; a
missing or filtered parent leaves the fork as a root.  Roots and sibling
subtrees sort by latest subtree activity, newest first, then canonical
path.  PREFIX carries the `├─'/`└─' branch and ancestor `│' gutters.

BUF and GENERATION are optional for non-render callers.  When supplied,
ownership is checked before and immediately after each key or parent
canonicalization, so reentrant cancellation does not continue through
later hand-built items."
  (let ((memo (make-hash-table :test 'equal))
        (nodes nil)
        (by-key (make-hash-table :test 'equal))
        (children-of (make-hash-table :test 'equal))
        (roots nil)
        (current-p
         (lambda ()
           (or (null generation)
               (pilish--session-browser-generation-current-p
                buf generation)))))
    (catch 'stale
      ;; Resolve each key once.  Render-prepared items make this a pure
      ;; lookup, while direct callers retain guarded fallback behavior.
      (dolist (item items)
        (unless (funcall current-p) (throw 'stale nil))
        (let ((key (pilish--session-item-key item memo)))
          (unless (funcall current-p) (throw 'stale nil))
          (push (list :item item :key key) nodes)))
      (setq nodes (nreverse nodes))
      (dolist (node nodes)
        (puthash (plist-get node :key) node by-key))
      (dolist (node nodes)
        (unless (funcall current-p) (throw 'stale nil))
        (let* ((item (plist-get node :item))
               (parent-key
                (or (plist-get item :canonicalParentSession)
                    (pilish--thread-parent-identity
                     (plist-get item :parentSessionPath)
                     (plist-get node :key) memo
                     (pilish--session-parent-anchor item)))))
          (unless (funcall current-p) (throw 'stale nil))
          (let ((parent (and parent-key (gethash parent-key by-key))))
            (if (and parent
                     ;; A self-reference would otherwise recurse forever.
                     (not (equal (plist-get parent :key)
                                 (plist-get node :key))))
                (puthash
                 (plist-get parent :key)
                 (cons node
                       (gethash (plist-get parent :key) children-of))
                 children-of)
              (push node roots)))))
      (dolist (root roots)
        (pilish--thread-node-activity root children-of))
      (let ((result nil))
        (dolist (root (pilish--thread-sorted-nodes roots))
          (push (list (plist-get root :item) "" 0) result)
          (setq result (pilish--thread-collect-children
                        root children-of nil result 0)))
        (nreverse result)))))

(defun pilish--thread-node-activity (node children-of)
  "Return and cache in NODE the latest `:modified' in NODE's subtree.
CHILDREN-OF maps a canonical parent key to its child nodes.  Activity
is the newest modification anywhere under the family, not the
parent's own mtime, so a recently used fork promotes its whole
family.  The explicit postorder stack keeps deeply forked families
safe; malformed parent cycles are bounded by an eq seen set."
  (or (plist-get node :activity)
      (let ((stack (list (list node nil)))
            (seen (make-hash-table :test #'eq)))
        (puthash node t seen)
        (while stack
          (pcase-let ((`(,current ,finish-p) (pop stack)))
            (if finish-p
                (let ((latest (or (plist-get (plist-get current :item)
                                              :modified)
                                  "")))
                  (dolist (child (gethash (plist-get current :key)
                                          children-of))
                    (let ((child-latest
                           (or (plist-get child :activity)
                               (plist-get (plist-get child :item) :modified)
                               "")))
                      (when (string> child-latest latest)
                        (setq latest child-latest))))
                  (plist-put current :activity latest))
              (push (list current t) stack)
              (dolist (child (gethash (plist-get current :key) children-of))
                (unless (or (plist-get child :activity)
                            (gethash child seen))
                  (puthash child t seen)
                  (push (list child nil) stack))))))
        (or (plist-get node :activity) ""))))

(defun pilish--thread-sorted-nodes (nodes)
  "Return NODES ordered by subtree activity descending.
Equal activity falls back to canonical path ascending, so the row
order never depends on scan order."
  (sort (copy-sequence nodes)
        (lambda (a b)
          (let ((ma (or (plist-get a :activity) ""))
                (mb (or (plist-get b :activity) "")))
            (if (not (equal ma mb))
                (string> ma mb)
              (string< (or (plist-get a :key) "")
                       (or (plist-get b :key) "")))))))

(defun pilish--thread-collect-children
    (node children-of ancestors result &optional depth)
  "Collect NODE's sorted children and their descendants onto RESULT.
CHILDREN-OF maps a canonical parent key to its child nodes.
ANCESTORS lists, oldest level first, whether each ancestor level
above the children continues below them.  DEPTH is NODE's structural
fork-family depth and defaults to zero.  Children sort by
`pilish--thread-sorted-nodes' and render after their parent with
connectors from `pilish--thread-prefix'.  An explicit preorder stack
keeps arbitrarily deep families off the Lisp call stack."
  (let (stack)
    (cl-labels
        ((queue-children
          (parent gutters parent-depth)
          (let* ((kids (pilish--thread-sorted-nodes
                        (gethash (plist-get parent :key) children-of)))
                 (count (length kids))
                 (index 0)
                 tasks)
            ;; TASKS is built backwards, then pushed backwards onto STACK,
            ;; leaving the first sorted child at the top.
            (dolist (kid kids)
              (setq index (1+ index))
              (let ((last-p (= index count)))
                (push (list kid
                            (pilish--thread-prefix gutters last-p)
                            (append gutters (list (not last-p)))
                            (1+ parent-depth))
                      tasks)))
            (dolist (task tasks)
              (push task stack)))))
      (queue-children node ancestors (or depth 0))
      (while stack
        (pcase-let ((`(,current ,prefix ,gutters ,current-depth)
                     (pop stack)))
          (push (list (plist-get current :item) prefix current-depth)
                result)
          (queue-children current gutters current-depth))))
    result))

(defun pilish--thread-prefix (ancestors last-p)
  "Return the Threaded-view connector for a row below the top level.
LAST-P non-nil means the row is its parent's last child and gets the
terminating `└─' branch; earlier siblings get `├─'.  Each true element
of ANCESTORS contributes a `│' gutter for an ancestor level whose
subtree continues past this row."
  (concat (mapconcat (lambda (continues) (if continues "│ " "  "))
                     ancestors "")
          (if last-p "└─ " "├─ ")))

(defun pilish--session-filter-named (items)
  "Filter ITEMS to only those with a name."
  (cl-remove-if-not (lambda (item)
                      (pilish--normalize-string-or-null
                       (plist-get item :name)))
                    items))

(defun pilish--session-filter-search (items tokens)
  "Filter ITEMS by search TOKENS.
Match prepared :searchText, or name/first-message text for metadata items."
  (if (null tokens)
      items
    (cl-remove-if-not
     (lambda (item)
       (let ((text (or (plist-get item :searchText)
                       (concat (or (plist-get item :name) "") " "
                               (or (plist-get item :firstMessage) "") " "
                               (or (plist-get item :allMessagesText) "")))))
         (pilish--matches-filter-p text tokens)))
     items)))

;;;; Time-Based Section Headers

(defun pilish--session-date-minus-one-day (time)
  "Return TIME's local calendar date one day earlier, as \"YYYY-MM-DD\".
The day decrements on the decoded local calendar and the result is
encoded at noon — never inside a daylight-saving transition — so a
stale numeric offset carried over from TIME cannot shift the target
date across midnight: encoding 00:30 the night after a spring-forward
transition with the current offset lands an hour early and turns
March 29 into March 28."
  (let ((decoded (decode-time time)))
    (setf (nth 0 decoded) 0
          (nth 1 decoded) 0
          (nth 2 decoded) 12
          (nth 3 decoded) (1- (nth 3 decoded)))
    (format-time-string "%Y-%m-%d" (encode-time decoded))))

(defun pilish--session-time-group (iso-timestamp &optional now)
  "Return the calendar time-group label for ISO-TIMESTAMP.
Groups: \"Future\", \"Today\", \"Yesterday\", \"This Week\",
\"Older\" — calendar semantics, not rolling durations.  \"Today\" is
the current local calendar date and \"Yesterday\" the date before
it (both via `pilish--session-date-minus-one-day'); \"This Week\" is
the rest of the current Monday-start week (ISO week and year match); a
late-night session therefore reads \"Yesterday\" from midnight onward,
and last week's Friday reads \"Older\" on Monday.  A later calendar
date — clock skew on the writer — reads \"Future\", which newest-first
sorting places as one contiguous leading group; without it, future
rows would repeat the \"This Week\" heading between Today and
Yesterday.  NOW defaults to the current time; renderers capture it
once per render and tests pin it.  Invalid timestamps read as
\"Older\"."
  (condition-case nil
      (let ((time (date-to-time iso-timestamp))
            (now (or now (current-time))))
        (cond
         ((string> (format-time-string "%Y-%m-%d" time)
                   (format-time-string "%Y-%m-%d" now))
          "Future")
         ((equal (format-time-string "%Y-%m-%d" time)
                 (format-time-string "%Y-%m-%d" now))
          "Today")
         ((equal (format-time-string "%Y-%m-%d" time)
                 (pilish--session-date-minus-one-day now))
          "Yesterday")
         ((equal (format-time-string "%G-W%V" time)
                 (format-time-string "%G-W%V" now))
          "This Week")
         (t "Older")))
    (error "Older")))

;;;; Section Classes

(defclass pilish-session-section (magit-section)
  ((keymap :initform 'pilish-session-section-map))
  "Section class for a session entry in the session browser.")

;;;; Flat-Row Folding

(defvar-local pilish--browse-fold-state nil
  "Fold state keyed by the canonical value of a rendered section.
Only folded values are present.  Session paths, Recent group labels,
and unambiguous string tree-node ids are canonical fold targets;
legacy and ambiguous tree rows are deliberately excluded.")

(defvar-local pilish--browse-fold-rows nil
  "Preorder flat-row metadata for the current completed render.")

(defvar-local pilish--browse-fold-row-by-section nil
  "Eq hash table from current Magit section objects to flat-row metadata.")

(defvar-local pilish--browse-fold-overlays nil
  "Invisible overlays implementing folds in the current render.")

(defun pilish--browse-fold-state-table ()
  "Return the current buffer's persistent fold-state table."
  (or pilish--browse-fold-state
      (setq pilish--browse-fold-state
            (make-hash-table :test #'equal))))

(defun pilish--browse-folded-p (value)
  "Return non-nil when canonical section VALUE is folded."
  (and (stringp value)
       (gethash value (pilish--browse-fold-state-table))))

(defun pilish--browse-publish-fold-values (values)
  "Publish canonical VALUES and prune fold state absent from them.
VALUES is an equal hash table built from the complete successful source
snapshot, not merely its current filter/search result.  Consequently a
value can disappear from the display and return with its fold intact,
while a value removed from the published data is forgotten."
  (let ((state (pilish--browse-fold-state-table)) stale)
    (maphash (lambda (value _folded)
               (unless (gethash value values)
                 (push value stale)))
             state)
    (dolist (value stale)
      (remhash value state))))

(defun pilish--browse-delete-fold-overlays ()
  "Delete only the flat-fold overlays owned by the current buffer."
  (mapc #'delete-overlay pilish--browse-fold-overlays)
  (setq pilish--browse-fold-overlays nil))

(defun pilish--browse-begin-fold-render ()
  "Reset transaction-local flat-row folding metadata before insertion."
  (pilish--browse-delete-fold-overlays)
  (setq pilish--browse-fold-rows nil
        pilish--browse-fold-row-by-section (make-hash-table :test #'eq)))

(defun pilish--browse-register-fold-row
    (section value depth surface foldable &optional indicator-offset)
  "Register one flat renderer row for post-insert folding.
SECTION is its flat Magit section, VALUE its existing canonical section
value, and DEPTH its ancestry depth in the displayed topology.  SURFACE
is one of `tree', `threaded', `recent', or `flat'.  FOLDABLE says the
already-computed next row is a descendant.  INDICATOR-OFFSET is its
character offset after any connector prefix.  A non-string VALUE remains
visible and participates in topology, but is never a fold target."
  (push (list :section section
              :value value
              :depth depth
              :surface surface
              :foldable (and foldable (stringp value))
              :indicator-offset (or indicator-offset 0)
              :index nil
              :end-index nil
              :parent nil
              :fold-parent nil
              :unit-root nil
              :body-start nil
              :end nil)
        pilish--browse-fold-rows))

(defun pilish--browse-fold-indicator (value)
  "Return a non-color fold indicator for canonical section VALUE."
  (propertize (if (pilish--browse-folded-p value) "▸ " "▾ ")
              'pilish-browse-fold-indicator value))

(defun pilish--browse-finish-fold-render ()
  "Derive flat extents and apply folds after all row insertion.
The stack pass is linear.  It uses the depth already carried by the
conversation-tree and threaded render rows, or the explicit Recent
heading/child depths; it never scans a source subtree."
  (setq pilish--browse-fold-rows (nreverse pilish--browse-fold-rows))
  (let ((stack nil)
        (index 0)
        (count (length pilish--browse-fold-rows)))
    (dolist (row pilish--browse-fold-rows)
      (let ((depth (plist-get row :depth)))
        (while (and stack
                    (>= (plist-get (car stack) :depth) depth))
          (setf (plist-get (car stack) :end-index) index)
          (pop stack))
        (let ((parent (car stack)))
          (setf (plist-get row :index) index
                (plist-get row :parent) parent
                (plist-get row :fold-parent)
                (cond
                 ((null parent) nil)
                 ((plist-get parent :foldable) parent)
                 (t (plist-get parent :fold-parent)))
                (plist-get row :unit-root)
                (and (> depth 0)
                     (memq (plist-get row :surface)
                           '(threaded recent))
                     (or (plist-get parent :unit-root) parent))))
        (puthash (plist-get row :section) row
                 pilish--browse-fold-row-by-section)
        (push row stack)
        (setq index (1+ index))))
    (dolist (row stack)
      (setf (plist-get row :end-index) count))
    ;; Convert once so extent endpoints stay O(1) even for a deep chain.
    (let ((rows (vconcat pilish--browse-fold-rows)))
      (dolist (row pilish--browse-fold-rows)
        (when (plist-get row :foldable)
          (let* ((row-index (plist-get row :index))
                 (end-index (plist-get row :end-index))
                 (body-start
                  (oref (plist-get (aref rows (1+ row-index)) :section)
                        start))
                 (end (if (< end-index count)
                          (oref (plist-get (aref rows end-index) :section)
                                start)
                        (point-max))))
            (setf (plist-get row :body-start) body-start
                  (plist-get row :end) end)))))
  (pilish--browse-apply-folds)))

(defun pilish--browse-apply-folds ()
  "Rebuild invisible overlays and truthful indicators from fold state.
Only maximal effective folds get overlays.  Nested folded state remains
recorded, so expanding an outer row reveals a still-folded child header,
but deep fold-all renders do not accumulate overlapping overlays."
  (pilish--browse-delete-fold-overlays)
  (let ((inhibit-read-only t)
        (covered-until 0))
    (dolist (row pilish--browse-fold-rows)
      (when (plist-get row :foldable)
        (let* ((value (plist-get row :value))
               (folded (pilish--browse-folded-p value))
               (start (+ (oref (plist-get row :section) start)
                         (plist-get row :indicator-offset))))
          ;; `subst-char-in-region' changes one glyph in place: positions,
          ;; Magit's section properties, and its insertion-type markers stay
          ;; unchanged, while `buffer-string' and redisplay both tell the
          ;; truth about the current state.
          (when (equal (get-text-property
                        start 'pilish-browse-fold-indicator)
                       value)
            (remove-text-properties start (1+ start) '(display nil))
            (let ((glyph (string-to-char (if folded "▸" "▾"))))
              (unless (= (char-after start) glyph)
                (subst-char-in-region start (1+ start)
                                      (char-after start) glyph t))))
          (when (and folded
                     (>= (plist-get row :index) covered-until))
            (let ((beg (plist-get row :body-start))
                  (end (plist-get row :end)))
              (when (< beg end)
                (let ((overlay (make-overlay beg end nil nil t)))
                  (overlay-put overlay 'evaporate t)
                  (overlay-put overlay 'invisible 'pilish-browse-fold)
                  (overlay-put overlay 'cursor-intangible t)
                  (overlay-put overlay 'pilish-browse-fold-header row)
                  (push overlay pilish--browse-fold-overlays)))
              (setq covered-until (plist-get row :end-index)))))))
    (setq pilish--browse-fold-overlays
          (nreverse pilish--browse-fold-overlays))))

(defun pilish--browse-fold-overlay-at (position)
  "Return the effective Pilish fold overlay hiding POSITION, or nil."
  (let (best)
    (dolist (overlay (overlays-at position))
      (when (overlay-get overlay 'pilish-browse-fold-header)
        (when (or (null best)
                  (< (overlay-start overlay) (overlay-start best))
                  (and (= (overlay-start overlay) (overlay-start best))
                       (> (overlay-end overlay) (overlay-end best))))
          (setq best overlay))))
    best))

(defun pilish--browse-repair-folded-position (position)
  "Return a visible position for POSITION under current folds.
A hidden descendant is promoted to the header whose effective overlay
hides it.  Other kinds of invisibility are left untouched."
  (if-let* ((overlay (pilish--browse-fold-overlay-at position))
            (row (overlay-get overlay 'pilish-browse-fold-header)))
      (oref (plist-get row :section) start)
    position))

(defun pilish--browse-repair-folded-points ()
  "Move buffer and window points out of folded row extents."
  (goto-char (pilish--browse-repair-folded-position (point)))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-point
     window
     (pilish--browse-repair-folded-position (window-point window)))))

(defun pilish--browse-visible-section-at-point ()
  "Return the visible current section selected at point, or nil.
A section must still own its row start after a rerender.  Check that
start, not just point: Magit's context-menu selection can name a hidden
row while point is elsewhere.  Redisplay repairs cursors, but commands
can run before that repair."
  (when-let* ((section (magit-current-section))
              (start (oref section start))
              ((eq section (magit-section-at start)))
              ((not (invisible-p start))))
    section))

(defun pilish--browse-current-fold-row ()
  "Return current flat-row metadata, or nil outside a browser row."
  (and pilish--browse-fold-row-by-section
       (gethash (magit-current-section)
                pilish--browse-fold-row-by-section)))

(defun pilish--browse-fold-target-at-point ()
  "Return the foldable row selected by the documented point rule.
A row with displayed descendants selects itself.  A leaf selects its
nearest displayed foldable ancestor or containing unit."
  (when-let* ((row (pilish--browse-current-fold-row)))
    (if (plist-get row :foldable)
        row
      (plist-get row :fold-parent))))

(defun pilish--browse-refresh-fold-display ()
  "Apply current fold state and keep all displayed points visible."
  (pilish--browse-apply-folds)
  (pilish--browse-repair-folded-points)
  (force-mode-line-update))

(defun pilish-browse-toggle-fold ()
  "Toggle the foldable row at point without changing section nesting.
A row that has currently displayed descendants folds itself.  On a
leaf row, toggle its nearest displayed foldable ancestor or containing
unit.  Flat query and Most-messages rows have no such target."
  (interactive)
  (if-let* ((row (pilish--browse-fold-target-at-point)))
      (let* ((value (plist-get row :value))
             (state (pilish--browse-fold-state-table)))
        (if (gethash value state)
            (remhash value state)
          (puthash value t state))
        (pilish--browse-refresh-fold-display))
    (user-error "No foldable row at point")))

(defun pilish-browse-fold-all (&optional unfold)
  "Fold every displayed top-level unit, or UNFOLD every saved fold.
Without a prefix argument, collapse each outermost currently foldable
conversation root, fork family, or Recent group in one overlay pass.
With a prefix argument, clear all fold state, including temporarily
filtered values."
  (interactive "P")
  (let ((state (pilish--browse-fold-state-table)))
    (if unfold
        (clrhash state)
      (dolist (row pilish--browse-fold-rows)
        (when (and (plist-get row :foldable)
                   (null (plist-get row :fold-parent)))
          (puthash (plist-get row :value) t state))))
    (pilish--browse-refresh-fold-display)))

(defun pilish-browse-goto-parent-row ()
  "Go to the current row's visible parent or containing unit.
Conversation-tree rows use their nearest displayed parent.  Threaded
session rows go to their family root, and Recent rows go to their time
group heading.  Flat session/query rows have no parent."
  (interactive)
  (let* ((row (pilish--browse-current-fold-row))
         (surface (and row (plist-get row :surface)))
         (target
          (pcase surface
            ('tree (plist-get row :parent))
            ((or 'threaded 'recent) (plist-get row :unit-root))
            (_ nil))))
    (if target
        (magit-section-goto (plist-get target :section))
      (user-error "No parent row"))))

(defun pilish--browse-skip-folded-section (_section)
  "Keep Magit's section motions out of invisible flat rows.
Installed buffer-locally on `magit-section-movement-hook'.  Native
Magit section commands and bindings remain unchanged; after one lands
inside an effective fold, jump directly across its overlay rather than
walking every hidden row."
  (when-let* ((overlay (pilish--browse-fold-overlay-at (point)))
              (row (overlay-get overlay 'pilish-browse-fold-header)))
    (cond
     ((memq this-command
            '(magit-section-forward magit-section-forward-sibling))
      (let ((end (overlay-end overlay)))
        (if (< end (point-max))
            (goto-char end)
          (goto-char (oref (plist-get row :section) start))
          (user-error "No next visible section"))))
     (t
      ;; Backward motions, and any future Magit motion that reaches an
      ;; invisible row, repair to the visible owning header.
      (goto-char (oref (plist-get row :section) start))))))

;;;; Keymaps

(defvar pilish-browse-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    ;; These commands describe recursive Magit section bodies.  Pilish's
    ;; identity sections stay deliberately flat, so retaining them would be
    ;; inert or misleading.  Copying (rather than parenting) lets nil remove
    ;; the inherited bindings completely.
    (dolist (key '("C-c TAB" "C-<tab>" "M-<tab>"
                   "1" "2" "3" "4"
                   "M-1" "M-2" "M-3" "M-4"
                   "<left-fringe> <mouse-1>"
                   "<left-fringe> <mouse-2>"))
      (define-key map (kbd key) nil))
    (define-key map (kbd "TAB") #'pilish-browse-toggle-fold)
    (define-key map [tab] #'pilish-browse-toggle-fold)
    (define-key map (kbd "<backtab>") #'pilish-browse-fold-all)
    (define-key map [backtab] #'pilish-browse-fold-all)
    (define-key map (kbd "^") #'pilish-browse-goto-parent-row)
    (define-key map (kbd "g") #'pilish-browse-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Base keymap for Pilish browse modes.")

(defvar pilish-session-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pilish-browse-mode-map)
    (define-key map (kbd "s") #'pilish-session-browser-cycle-view)
    (define-key map (kbd "f") #'pilish-session-browser-toggle-named)
    (define-key map (kbd "/") #'pilish-session-browser-search)
    (define-key map (kbd "t") #'pilish-session-browser-toggle-scope)
    (define-key map (kbd "r") #'pilish-session-browser-rename)
    (define-key map (kbd "d") #'pilish-session-browser-delete)
    (define-key map (kbd "RET") #'pilish-session-browser-switch)
    (define-key map (kbd "?") #'pilish-session-browser-dispatch)
    (define-key map (kbd "h") #'pilish-session-browser-dispatch)
    map)
  "Keymap for the session browser.")

(defvar pilish-session-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pilish-session-browser-switch)
    map)
  "Keymap for session sections (text property on each session line).")

;;;; Session Browser Default Options

(defcustom pilish-session-browser-default-scope 'current
  "Initial scope of a newly created session browser buffer.
`current' lists only this project's sessions; `all' lists every
project under the sessions root.

The value initializes browser buffers when they are created, and
again whenever the browser major mode is explicitly re-run.  An
existing browser buffer — including one hidden with
\\[quit-window] and reopened — otherwise keeps its locally changed
state, so scope toggles stay local to that buffer."
  :type '(choice (const :tag "This project" current)
                 (const :tag "All projects" all))
  :group 'pilish)

(defcustom pilish-session-browser-default-view 'threaded
  "Initial view of a newly created session browser buffer.
Views differ in hierarchy and ordering:

- `threaded' — Threaded (fork families): sessions grouped into fork
  families through the `parentSession' header link that /fork and
  /clone write; each parent renders before its descendants, and roots
  and siblings are ordered by the latest activity (file mtime)
  anywhere in the family, newest first.  A search query flattens the
  view to newest-first rows: flat results must not draw ancestry
  between a partial set of matches.
- `recent' — Recent activity: flat rows ordered by the session
  file's modification time, newest first.
- `messages' — Most messages: flat rows ordered by the number of
  persisted message records, tool-result records included, highest
  first.

The value initializes browser buffers when they are created, and
again whenever the browser major mode is explicitly re-run.  An
existing browser buffer — including one hidden with
\\[quit-window] and reopened — otherwise keeps its locally changed
state, so cycling views with `s' stays local to that buffer."
  :type '(choice (const :tag "Threaded (fork families)" threaded)
                 (const :tag "Recent activity" recent)
                 (const :tag "Most messages" messages))
  :group 'pilish)

(defcustom pilish-session-browser-default-named-only nil
  "Whether new session browser buffers start showing named sessions only.

The value initializes browser buffers when they are created, and
again whenever the browser major mode is explicitly re-run.  An
existing browser buffer — including one hidden with
\\[quit-window] and reopened — otherwise keeps its locally changed
state, so toggling it with `f' stays local to that buffer."
  :type 'boolean
  :group 'pilish)

;;;; Buffer-Local State

(defvar-local pilish--session-browser-scope 'current
  "Scope for session listing: `current' (this project) or `all'.
New browser buffers start from
`pilish-session-browser-default-scope'.")

(defvar-local pilish--session-browser-view 'threaded
  "Session view: `threaded', `recent', or `messages'.
New browser buffers start from
`pilish-session-browser-default-view'.")

(defvar-local pilish--session-browser-named-only nil
  "When non-nil, show only named sessions.
New browser buffers start from
`pilish-session-browser-default-named-only'.")

(defvar-local pilish--session-browser-items nil
  "Session items from the last `--browse-load-sessions' callback.")

(defvar-local pilish--session-browser-items-scope nil
  "Scope that owns `pilish--session-browser-items', or nil before a scan.
During a different-scope load the header suppresses this stale
snapshot's total instead of relabeling it as the requested scope.")

(defvar-local pilish--session-browser-search-query nil
  "Current search query string, or nil.")

(defvar-local pilish--session-browser-search-tokens nil
  "Parsed search tokens from `pilish--session-browser-search-query'.")

(defvar-local pilish--session-browser-loading nil
  "Non-nil while a fetch is in progress.")

(defvar-local pilish--session-browser-fetch-anchor nil
  "Point anchor carried across the in-flight fetch cycle.
Set to the anchor captured at fetch start.  A refresh issued while
loading captures no anchor of its own (the loading render already
destroyed the sections), so it reuses this one instead.  Cleared when
the cycle's final render runs.")

(defvar-local pilish--session-browser-error nil
  "Error message string from last fetch, or nil on success.")

(defvar-local pilish--session-browser-fetch-token 0
  "Generation counter for session-browser fetches.
The browser fetch cycle claims its generation before rendering and
passes it to `pilish--browse-load-sessions'.  Direct loader callers that
omit a generation claim one at the loader seam.  Superseded work is
dropped by comparing its captured token with the buffer's current one.
The permanent local binding lets mode reinitialization advance rather
than reset the counter, so an older callback can never regain ownership.")

(put 'pilish--session-browser-fetch-token 'permanent-local t)

(defvar-local pilish--session-browser-rendering-p nil
  "Non-nil while a session rerender owns the buffer transaction.")

(defvar-local pilish--session-browser-pending-rerender nil
  "Newest session rerender deferred by an active render transaction.
The value is an argument list for `pilish--session-browser-rerender'.")

(defun pilish--session-browser-ensure-render-current (buf generation)
  "Abort session insertion unless BUF still owns GENERATION."
  (unless (pilish--session-browser-generation-current-p buf generation)
    (throw 'pilish--session-browser-stale-render nil)))

;;;; Session Browser Dispatch Transient

(defun pilish--session-dispatch-heading ()
  "Return heading string for the session browser dispatch transient.
Shows current scope, view, and named-only state — the same state
`pilish--session-browser-header-line' formats for the
header-line, using the same user-facing labels
\(`pilish--session-scope-label', `pilish--session-view-label').
Transient evaluates group descriptions in the invoking browser
buffer \(`transient-with-shadowed-buffer' inside
`transient--insert-group'), so these buffer-local reads see the
browser's state on the real rendering path."
  (mapconcat #'identity
             (append (list (format "scope:%s"
                                   (pilish--session-scope-label
                                    pilish--session-browser-scope))
                           (format "view:%s"
                                   (pilish--session-view-label
                                    pilish--session-browser-view)))
                     (and pilish--session-browser-named-only
                          '("named-only")))
             " │ "))

(transient-define-prefix pilish-session-browser-dispatch ()
  "Session browser help."
  [:description pilish--session-dispatch-heading
   ["Actions"
    ("RET" "switch" pilish-session-browser-switch)
    ("r" "rename" pilish-session-browser-rename)
    ("d" "delete" pilish-session-browser-delete)
    ("g" "refresh" pilish-browse-refresh)
    ("q" "quit" quit-window)]
   ["View & Filter"
    ("s" "cycle view" pilish-session-browser-cycle-view)
    ("f" "named only" pilish-session-browser-toggle-named)
    ("t" "toggle scope" pilish-session-browser-toggle-scope)
    ("/" "search" pilish-session-browser-search)]
   ["Navigate & Fold"
    ("TAB" "toggle row/nearest containing fold" pilish-browse-toggle-fold)
    ("<backtab>" "fold all (prefix: unfold)" pilish-browse-fold-all)
    ("^" "family root/group heading" pilish-browse-goto-parent-row)]])

;;;; Faces

(defface pilish-session-name
  '((t :weight bold))
  "Face for session names in the session browser."
  :group 'pilish)

(defface pilish-session-message-count
  '((t :inherit shadow))
  "Face for message counts in the session browser."
  :group 'pilish)

(defface pilish-session-age
  '((t :inherit shadow))
  "Face for relative age in the session browser margin."
  :group 'pilish)

(defface pilish-session-cwd
  '((t :inherit shadow :slant italic))
  "Face for the project label leading All-projects session rows."
  :group 'pilish)

(defface pilish-session-thread-connector
  '((t :inherit shadow))
  "Face for threading connectors (├─, └─) in the session browser."
  :group 'pilish)

(defface pilish-session-group-header
  '((t :inherit magit-section-heading))
  "Face for time-group headers (Today, Yesterday, etc.)."
  :group 'pilish)

(defface pilish-session-live
  '((t :inherit success :weight bold))
  "Face for live-session markers in the session browser."
  :group 'pilish)

;;;; Major Modes

(define-derived-mode pilish-browse-mode magit-section-mode
  "Pi-Browse"
  "Base mode for Pilish browse buffers.
Uses Magit's flat section navigation plus Pilish's explicit row folding."
  :group 'pilish
  (setq pilish--browse-fold-state (make-hash-table :test #'equal)
        pilish--browse-fold-rows nil
        pilish--browse-fold-row-by-section (make-hash-table :test #'eq)
        pilish--browse-fold-overlays nil)
  (add-to-invisibility-spec 'pilish-browse-fold)
  (cursor-intangible-mode 1)
  (add-hook 'magit-section-movement-hook
            #'pilish--browse-skip-folded-section nil t))

(define-derived-mode pilish-session-browser-mode
  pilish-browse-mode "Pi-Sessions"
  "Major mode for browsing pi sessions.
\\{pilish-session-browser-mode-map}
Buffers start from the `pilish-session-browser-default-scope',
`-view', and `-named-only' options; an existing browser buffer keeps
its toggled state when hidden and reopened, and re-running this mode
re-initializes from the current defaults."
  :group 'pilish
  ;; `kill-all-local-variables' preserves permanent locals.  Establish
  ;; generation zero on first entry; on every later mode entry, advance
  ;; the surviving counter before hooks or new work can run.  Thus a
  ;; callback captured before reinitialization is immediately obsolete.
  (if (local-variable-p 'pilish--session-browser-fetch-token)
      (cl-incf pilish--session-browser-fetch-token)
    (setq-local pilish--session-browser-fetch-token 0))
  ;; Initial state from the default options, set in the mode body so
  ;; mode hooks observe the configured values and their overrides
  ;; survive (idiomatic major-mode initialization).  Entry points reuse
  ;; an existing buffer without re-running the mode, so a hidden and
  ;; reopened browser keeps its toggled state.
  (setq pilish--session-browser-scope
        pilish-session-browser-default-scope
        pilish--session-browser-view
        pilish-session-browser-default-view
        pilish--session-browser-named-only
        pilish-session-browser-default-named-only)
  (setq-local header-line-format
              '(:eval (pilish--session-browser-header-line)))
  (setq pilish--browse-margin-width
        pilish--session-margin-width)
  (setq-local right-margin-width pilish--session-margin-width)
  (add-hook 'window-configuration-change-hook
            #'pilish--browse-apply-margins nil t))

;;;; Buffer Management

(defun pilish--session-browser-buffer-name (dir)
  "Return session browser buffer name for DIR."
  (format "*pilish-sessions:%s*"
          (pilish--route-preserving-abbreviate-file-name dir)))

(defun pilish--get-or-create-session-browser (dir)
  "Get or create session browser buffer for DIR.
A newly created buffer's mode initialization starts from
`pilish-session-browser-default-scope',
`pilish-session-browser-default-view', and
`pilish-session-browser-default-named-only'; an existing buffer is
returned unchanged (the mode never re-runs), so hiding with
\\[quit-window] and reopening keeps its locally changed state until
the buffer is killed or the browser major mode is explicitly
re-run."
  (let* ((name (pilish--session-browser-buffer-name dir))
         (buf (get-buffer name)))
    (or buf
        (with-current-buffer (generate-new-buffer name)
          (setq default-directory dir)
          (pilish-session-browser-mode)
          (current-buffer)))))

;;;; Rendering

(defun pilish--session-browser-empty-line (kind)
  "Return the actionable empty-state lines for KIND, `scan' or `filter'.
KIND `scan' covers a scope that listed no session files at all;
`filter' covers rows that existed until named-only filtering or the
search query removed them all.  Reading the same buffer-local state
the header line shows, each line names a widening action with its
real keybinding: \\[pilish-session-browser-toggle-scope] switches
scope, \\[pilish-session-browser-toggle-named] clears named-only,
and an empty \\[pilish-session-browser-search] input clears the
query.  Each hint is its own short line, key first, so the recovery
action survives the 32 usable columns of a 52-column terminal with
the 20-column right margin — one long sentence would truncate
before its first key.  Precedence under `filter' follows recency of
narrowing — the query first, then named-only, then the scope switch
— with at most two hints; an empty All-projects scan has no wider
scope to suggest and stays a single plain line."
  (pcase kind
    ('scan
     (if (eq pilish--session-browser-scope 'all)
         "No sessions found."
       (substitute-command-keys
        "No sessions in this project.\n\\[pilish-session-browser-toggle-scope] to list all projects")))
    ('filter
     (cond
      (pilish--session-browser-search-query
       (concat
        (substitute-command-keys
         "No matching sessions.\n\\[pilish-session-browser-search] to clear the query")
        (cond
         (pilish--session-browser-named-only
          (substitute-command-keys
           "\n\\[pilish-session-browser-toggle-named] to show all names"))
         ((eq pilish--session-browser-scope 'current)
          (substitute-command-keys
           "\n\\[pilish-session-browser-toggle-scope] to search all projects"))
         (t ""))))
      (pilish--session-browser-named-only
       (concat
        (substitute-command-keys
         "No named sessions.\n\\[pilish-session-browser-toggle-named] to show all names")
        (when (eq pilish--session-browser-scope 'current)
          (substitute-command-keys
           "\n\\[pilish-session-browser-toggle-scope] to list all projects"))))
      (t "No matching sessions.")))))

(defun pilish--session-browser-generation-current-p (buf generation)
  "Return non-nil when BUF still owns session GENERATION.
BUF must still be a session browser: the permanent generation binding
also survives a switch to another major mode, where no old browser work
may publish."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (and (derived-mode-p 'pilish-session-browser-mode)
              (eq generation pilish--session-browser-fetch-token)))))

(defun pilish--session-browser-prepare-render-items (items buf generation)
  "Return `(t . ITEMS)' with one stored key each, or nil when stale.
Each returned item is a render-only copy whose leading
`:canonicalPath' is computed exactly once.  Direct hand-built items
can dispatch a file-name handler during that computation, so BUF's
ownership of GENERATION is checked immediately before and after every
key.  Downstream project,
thread, and row preparation can then reuse the stored key without
re-running callback-capable canonicalization."
  (let ((memo (make-hash-table :test 'equal))
        (prepared nil))
    (catch 'stale
      (dolist (item items)
        (unless (pilish--session-browser-generation-current-p buf generation)
          (throw 'stale nil))
        (let ((key (pilish--session-item-key item memo)))
          (unless (pilish--session-browser-generation-current-p
                   buf generation)
            (throw 'stale nil))
          ;; Put the render key first even when ITEM already carries one;
          ;; `plist-get' and `pilish--session-item-key' then reuse exactly
          ;; the value whose callback boundary was checked above.
          (push (append (list :canonicalPath key) item) prepared)))
      (cons t (nreverse prepared)))))

(defun pilish--session-fold-published-values (items now)
  "Return fold-value set for the complete prepared session ITEMS at NOW.
Session keys and every Recent time-group label represented by the source
snapshot are included, regardless of the current view, filter, or query."
  (let ((values (make-hash-table :test #'equal)))
    (dolist (item items)
      (when-let* ((key (plist-get item :canonicalPath))
                  ((stringp key)))
        (puthash key t values))
      (puthash (pilish--session-time-group
                (plist-get item :modified) now)
               t values))
    values))

(defun pilish--session-browser-render (buf)
  "Render the session browser in BUF from its buffer-local state."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
           (generation pilish--session-browser-fetch-token)
           (raw-items pilish--session-browser-items)
           ;; Status states render no rows and therefore need no keys.
           (prepared
            (if (or pilish--session-browser-loading
                    pilish--session-browser-error
                    (null raw-items))
                (cons t raw-items)
              (pilish--session-browser-prepare-render-items
               raw-items buf generation)))
           (items (cdr prepared))
           (filtered
            (and prepared
                 (not pilish--session-browser-loading)
                 (not pilish--session-browser-error)
                 items
                 (pilish--session-filter-search
                  (if pilish--session-browser-named-only
                      (pilish--session-filter-named items)
                    items)
                  pilish--session-browser-search-tokens)))
           ;; Loading/error/empty states need no live markers.  A live
           ;; path handler may start a newer fetch, so every later stage
           ;; is gated by the captured generation.
           (live-paths
            (and filtered
                 (pilish--session-browser-generation-current-p
                  buf generation)
                 (pilish--browse-live-session-paths)))
           (row-kind
            (cond
             ((and (eq pilish--session-browser-view 'threaded)
                   (null pilish--session-browser-search-tokens))
              'threaded)
             ((eq pilish--session-browser-view 'recent) 'recent)
             (t 'flat)))
           ;; Token allocation uses the full prepared snapshot, not the
           ;; filtered rows, and reuses each precomputed key.
           (fields
            (and filtered
                 (pilish--session-browser-generation-current-p
                  buf generation)
                 (eq pilish--session-browser-scope 'all)
                 (pilish--session-project-fields items)))
           ;; Complete callback-capable row preparation before opening a
           ;; Magit section.  In particular, hand-built fork metadata may
           ;; canonicalize a parent while threading.
           (rows
            (and filtered
                 (pilish--session-browser-generation-current-p
                  buf generation)
                 (pcase row-kind
                   ('threaded
                    (pilish--session-thread-items
                     filtered buf generation))
                   ('recent (pilish--session-sort-items filtered 'recent))
                   (_ (pilish--session-sort-items
                       filtered
                       ;; A queried Threaded view is a flat newest-first
                       ;; result set, with no implied family connectors.
                       (if (eq pilish--session-browser-view 'threaded)
                           'recent
                         pilish--session-browser-view))))))
           ;; Capture the calendar clock once for both the persistent
           ;; Recent-group universe and the rendered Recent boundaries.
           (render-now
            (and prepared
                 (not pilish--session-browser-loading)
                 (not pilish--session-browser-error)
                 (current-time)))
           (published-values
            (and render-now
                 (pilish--session-fold-published-values items render-now))))
      (pilish--session-browser-ensure-render-current buf generation)
      (pilish--browse-begin-fold-render)
      (when (and prepared
                 (pilish--session-browser-generation-current-p
                  buf generation))
        (magit-insert-section (root)
          ;; A visibility hook can request a newer fetch.  Its render is
          ;; queued until this transaction unwinds; stop before inserting
          ;; any row owned by the superseded generation.
          (pilish--session-browser-ensure-render-current buf generation)
          (cond
           (pilish--session-browser-loading
            (insert (pilish--propertize-face
                     "Loading sessions..."
                     'pilish-activity-phase)
                    "\n"))
           (pilish--session-browser-error
            (insert (pilish--propertize-face
                     (format "Error: %s\n" pilish--session-browser-error)
                     'error)))
           ((null items)
            (insert (pilish--session-browser-empty-line 'scan) "\n"))
           ((null filtered)
            (insert (pilish--session-browser-empty-line 'filter) "\n"))
           ((eq row-kind 'threaded)
            (pilish--session-browser-render-threaded
             rows fields live-paths buf generation))
           ((eq row-kind 'recent)
            (pilish--session-browser-render-recent
             rows fields live-paths buf generation render-now))
           (t
            (pilish--session-browser-render-flat
             rows fields live-paths buf generation)))))
        (pilish--session-browser-ensure-render-current buf generation)
        ;; Publish only after insertion still owns its generation.  A
        ;; reentrant newer fetch must not let this obsolete render prune
        ;; persistent fold state before it stops.
        (when published-values
          (pilish--browse-publish-fold-values published-values))
        (pilish--browse-finish-fold-render)
        (pilish--session-browser-ensure-render-current buf generation))))

(defun pilish--session-browser-render-flat
    (items fields live-paths buf generation)
  "Render prepared ITEMS as flat rows while BUF owns GENERATION.
FIELDS maps precomputed session keys to bounded project tokens;
LIVE-PATHS marks live sessions."
  (dolist (item items)
    (when (pilish--session-browser-generation-current-p buf generation)
      (pilish--session-browser-insert-session
       item (plist-get item :canonicalPath) nil fields live-paths
       buf generation 0 'flat nil))))

(defun pilish--session-browser-render-threaded
    (rows fields live-paths buf generation)
  "Render prepared threaded ROWS while BUF owns GENERATION.
Each row is an (ITEM PREFIX DEPTH) entry already produced before the
outer Magit section opened.  FIELDS and LIVE-PATHS provide project and
live context."
  (cl-loop for tail on rows
           for entry = (car tail)
           for next = (cadr tail)
           while (pilish--session-browser-generation-current-p
                  buf generation)
           do
           (let* ((item (nth 0 entry))
                  (depth (nth 2 entry))
                  (foldable (and next (> (nth 2 next) depth))))
             (pilish--session-browser-insert-session
              item (plist-get item :canonicalPath) (nth 1 entry)
              fields live-paths buf generation
              depth 'threaded foldable))))

(defun pilish--session-browser-render-recent
    (items fields live-paths buf generation now)
  "Render prepared, recency-sorted ITEMS at NOW while BUF owns GENERATION.
NOW is captured once by the outer render, so a render that crosses
midnight groups every row against the same calendar day.  FIELDS maps
session keys to project tokens; LIVE-PATHS marks live sessions."
  (let ((last-group nil))
    (dolist (item items)
      (when (pilish--session-browser-generation-current-p buf generation)
        (let ((group (pilish--session-time-group
                      (plist-get item :modified) now)))
          (unless (equal group last-group)
            (when (pilish--session-browser-generation-current-p
                   buf generation)
              (magit-insert-section group-section (time-group group)
                (pilish--session-browser-ensure-render-current
                 buf generation)
                (pilish--browse-register-fold-row
                 group-section group 0 'recent t)
                (magit-insert-heading
                  (concat
                   (pilish--browse-fold-indicator group)
                   (pilish--propertize-face
                    group 'pilish-session-group-header))))
              (setq last-group group)))
          (pilish--session-browser-insert-session
           item (plist-get item :canonicalPath) nil fields live-paths
           buf generation 1 'recent nil))))))

(defun pilish--session-browser-insert-session
    (session key prefix fields live-paths buf generation
             &optional depth surface foldable)
  "Insert prepared SESSION with KEY and PREFIX while BUF owns GENERATION.
KEY was computed once by
`pilish--session-browser-prepare-render-items', so insertion never
repeats callback-capable canonicalization.  It remains the Magit
section identity, preserving point across alias spelling changes;
actions resolve the raw retained spelling through
`pilish--session-browser-path-at-point'.
PREFIX is the Threaded-view connector — the empty string for a family
root, gutters and a branch for descendants.  A nil PREFIX renders a
flat row, where a forked session gets a \"fork:\" prefix instead.
In All projects scope the row leads with its bounded project token
looked up in FIELDS by SESSION's key (see
`pilish--session-project-fields' — the placeholder token keeps the
field reserved when the recorded cwd was rejected), padded to the
fixed token width, followed by a two-column live field, and only
then the unbounded connector and title — so identity and live
status survive any connector depth, common prefix, or long title in
a narrow window.  This-project rows keep the compact connector-first
shape because the scope already fixes the project.  When SESSION's
key is a key of LIVE-PATHS (see
`pilish--browse-live-session-paths' — only Pilish processes in this
Emacs, never other Emacs instances or system-wide pi processes),
prepend a live-session marker.
DEPTH, SURFACE, and FOLDABLE carry the already prepared flat folding
shape.  Message count and age are rendered as a right-margin overlay."
  (when (pilish--session-browser-generation-current-p buf generation)
    (let* ((name (pilish--session-display-name session))
         (count (or (plist-get session :messageCount) 0))
         (modified (plist-get session :modified))
         (is-fork (plist-get session :parentSessionPath))
         (live-p (gethash key live-paths))
         (token (and fields (gethash key fields)))
         (fold-indicator
          (and foldable (pilish--browse-fold-indicator key)))
         (display-prefix
          (cond
           (prefix
            ;; Keep every connector in its original column.  A fold glyph
            ;; follows this row's complete connector instead of shifting
            ;; descendant gutters to the right.
            (concat
             (pilish--propertize-face
              prefix 'pilish-session-thread-connector)
             fold-indicator))
           (is-fork
            (pilish--propertize-face
             "fork: " 'pilish-session-thread-connector))
           (t "")))
         (heading
          (concat
           (if token
               ;; All projects: the bounded token field, the fixed
               ;; two-column live field, and only then unbounded
               ;; connector and title — identity and live status
               ;; survive any depth, prefix, or long title.
               (concat (pilish--propertize-face
                        (pilish--session-pad-display
                         token pilish--session-project-token-width)
                        'pilish-session-cwd)
                       (if live-p
                           (pilish--propertize-face
                            "\u25cf " 'pilish-session-live)
                         "  ")
                       display-prefix)
             ;; This project: the established compact shape.
             (concat display-prefix
                     (when live-p
                       (pilish--propertize-face
                        "\u25cf " 'pilish-session-live))))
           (pilish--propertize-face
            name 'pilish-session-name)))
         (margin-str (concat
                      (pilish--propertize-face
                       (format "%4d msgs " count)
                       'pilish-session-message-count)
                      (pilish--propertize-face
                       (or (pilish--format-margin-age-from-iso modified)
                           (format (format "%%%ds"
                                           (+ 3 pilish--margin-age-unit-width))
                                   "?"))
                       'pilish-session-age))))
      ;; Check immediately before insertion as preparation above may run
      ;; user-advised display/time code even though the key is already pure.
      (when (pilish--session-browser-generation-current-p buf generation)
        (magit-insert-section session-section (session key)
          (pilish--session-browser-ensure-render-current buf generation)
          (pilish--browse-register-fold-row
           session-section key (or depth 0) (or surface 'flat) foldable
           (and foldable
                (text-property-any
                 0 (length heading)
                 'pilish-browse-fold-indicator key heading)))
          (magit-insert-heading heading)
          (pilish--make-margin-overlay margin-str))))))

;;;; Header-Line

(defun pilish--session-browser-header-line ()
  "Return header-line string for the session browser.
Shows the scope, view, named-only, query, and the session count in
scope — the same state `pilish--session-dispatch-heading' formats
for the transient, using the same user-facing labels.  The count is
labeled `total' because it counts scanned sessions in scope, not
the rows surviving the current query and named-only filter.  A total
is shown only for an error-free snapshot owned by the displayed scope.
A different scope's stale snapshot, a failed scan, and initial/unowned
state are unconfirmed and hide the total."
  (let* ((scope pilish--session-browser-scope)
         (view pilish--session-browser-view)
         (named pilish--session-browser-named-only)
         (query pilish--session-browser-search-query)
         (count-visible-p
          (and (not pilish--session-browser-error)
               (eq pilish--session-browser-items-scope scope)))
         (count (length (or pilish--session-browser-items '()))))
    (mapconcat #'identity
               (append (list (format "Sessions [%s]"
                                     (pilish--session-scope-label scope))
                             (format "view:%s"
                                     (pilish--session-view-label view)))
                       (and named '("named-only"))
                       (and query (list (format "/%s" query)))
                       (and count-visible-p
                            (list (format "(%d total)" count)))
                       (list (pilish--propertize-face "?:help" 'shadow)))
               " │ ")))

;;;; Session Browser Interactive Commands

(defun pilish-session-browser-cycle-view ()
  "Cycle the session browser view.
Threaded (fork families), Recent activity, and Most messages —
see `pilish--session-view-label' for what each view shows."
  (interactive)
  (setq pilish--session-browser-view
        (pilish--session-view-next pilish--session-browser-view))
  (pilish--session-browser-rerender)
  (message "Pi: View: %s"
           (pilish--session-view-label pilish--session-browser-view)))

(define-obsolete-function-alias 'pilish-session-browser-cycle-sort
  'pilish-session-browser-cycle-view "3.2.0")

(defun pilish-session-browser-toggle-named ()
  "Toggle named-only filter in the session browser."
  (interactive)
  (setq pilish--session-browser-named-only
        (not pilish--session-browser-named-only))
  (pilish--session-browser-rerender)
  (message "Pi: Named-only: %s"
           (if pilish--session-browser-named-only "on" "off")))

(defun pilish-session-browser-toggle-scope ()
  "Toggle the scope between this project and all projects."
  (interactive)
  (setq pilish--session-browser-scope
        (if (eq pilish--session-browser-scope 'all)
            'current 'all))
  (pilish--session-browser-fetch-and-render)
  (message "Pi: Scope: %s"
           (pilish--session-scope-label pilish--session-browser-scope)))

(defun pilish-session-browser-search ()
  "Search names, first messages and all user/assistant text on disk.
Whitespace-separated regexp tokens must all match.  Text on inactive
branches is included; thinking, tools, images and summaries are not added.
The legacy first-message fallback can still match text from any role.
A blank or whitespace-only query clears the filter."
  (interactive)
  (let ((query (string-trim
                (read-string "Filter (regexp tokens): "
                             pilish--session-browser-search-query)))
        (need-rerender t))
    (if (string-empty-p query)
        (setq pilish--session-browser-search-query nil
              pilish--session-browser-search-tokens nil)
      ;; Validate regexp tokens
      (condition-case err
          (let ((tokens (split-string query)))
            (dolist (tok tokens)
              (string-match-p tok ""))
            (setq pilish--session-browser-search-query query
                  pilish--session-browser-search-tokens tokens))
        (invalid-regexp
         (message "Pi: Invalid regexp: %s" (error-message-string err))
         (setq need-rerender nil))))
    (when need-rerender
      (pilish--session-browser-rerender))))

(defun pilish--session-browser-item-at-point ()
  "Return the loaded session item represented at point, or nil.
Visible session sections carry canonical identity keys; resolve that key
in `pilish--session-browser-items', the complete loaded snapshot rather
than the currently visible filtered rows."
  (when-let* ((section (pilish--browse-visible-section-at-point))
              ((object-of-class-p section 'pilish-session-section)))
    (cl-find (oref section value) pilish--session-browser-items
             :key #'pilish--session-item-key :test #'equal)))

(defun pilish--session-browser-path-at-point ()
  "Return the file path of the visible session at point, or nil.
Sections carry canonical identities; the displayed row's raw retained
spelling is returned so actions (switch, rename, delete) act on the
path the user selected.  Identity-sensitive guards and relationships
canonicalize separately."
  (when-let* ((section (pilish--browse-visible-section-at-point))
              ((object-of-class-p section 'pilish-session-section)))
    (let ((key (oref section value)))
      (or (plist-get (pilish--session-browser-item-at-point) :path)
          key))))

(defun pilish-session-browser-switch ()
  "Switch to the session at point."
  (interactive)
  (if-let* ((path (pilish--session-browser-path-at-point)))
      (pilish--browse-switch-session path)
    (message "Pi: No session at point")))

(defun pilish--browse-live-session-paths ()
  "Return a hash table of session identities open in live Pilish processes.
Keys are canonical identities (`pilish--canonical-session-path'):
symlink alias spellings of one file unify, and remote spellings keep
their complete TRAMP route, so a final-hop-only spelling of a
multi-hop session is not cross-marked.  Rows compare their stored
`:canonicalPath' through `pilish--session-item-key'.  The table
reflects only the frontend's own process state — sessions opened in
other Emacs instances or outside Pilish are not marked — and is
empty with no live process, so disk browsing stays fully usable
offline.  The interactive guards keep their stricter per-path checks
in `pilish--browse-live-session-chat-buffer'."
  (let ((paths (make-hash-table :test 'equal)))
    (dolist (proc (process-list))
      (when (pilish--session-live-process-p proc)
        (let ((chat-buf (process-get proc 'pilish-chat-buffer)))
          (when (buffer-live-p chat-buf)
            (with-current-buffer chat-buf
              (let ((current (plist-get pilish--state :session-file)))
                (when (stringp current)
                  (puthash (pilish--canonical-session-path current)
                           t paths))))))))
    paths))

(defun pilish--browse-live-session-chat-buffer (path)
  "Return the chat buffer of a live Pilish process using PATH, or nil."
  (cl-loop for proc in (process-list)
           for chat-buf = (process-get proc 'pilish-chat-buffer)
           when (and (pilish--session-live-process-p proc)
                     (buffer-live-p chat-buf)
                     (pilish--browse-session-file-matches-p chat-buf path))
           return chat-buf))

(defun pilish--browse-ensure-session-closed (path)
  "Signal a `user-error' when a live Pilish process is using PATH."
  (when-let* ((chat-buf (pilish--browse-live-session-chat-buffer path)))
    (user-error "Session is open in %s — close it first"
                (buffer-name chat-buf))))

(defun pilish--session-delete-safe-text (text)
  "Return TEXT without characters that can forge a delete prompt.
The human session/project helpers remain the source of the wording.
After composing ordinary decomposed characters, this final display
boundary replaces control, bidi/format, line/paragraph separator, and
remaining zero-width characters with the replacement character."
  (let* ((plain (substring-no-properties (if (stringp text) text "")))
         (normalized
          (condition-case nil
              (ucs-normalize-NFC-string plain)
            (error plain))))
    (mapconcat
     (lambda (char)
       (if (or (memq (get-char-code-property char 'general-category)
                     '(Cc Cf Zl Zp))
               (pilish--session-zero-width-char-p char))
           "\ufffd"
         (char-to-string char)))
     normalized "")))

(defconst pilish--session-delete-name-width 50
  "Maximum display width of a session name in a delete prompt.")

(defconst pilish--session-delete-project-width 24
  "Maximum display width of a project token in a delete prompt.")

(defconst pilish--session-delete-child-name-width 26
  "Maximum display width of each child name in a delete prompt.")

(defconst pilish--session-delete-prompt-max-width 400
  "Upper display-width bound for a session deletion prompt.
This covers the longest fixed wording including the live-check limit,
three bounded child names, the target identity, and finite child counts.")

(defun pilish--session-delete-prompt-component (text width)
  "Return sanitized TEXT quoted within display WIDTH, ellipsizing if needed.
The ellipsis is part of the bound.  Quoting happens before truncation,
so user-controlled quote and backslash escapes cannot expand the final
component beyond WIDTH."
  (let* ((printed (prin1-to-string
                   (pilish--session-delete-safe-text text)))
         ;; Strip the printer's balanced ASCII quotes, bound the escaped
         ;; contents, then restore quotes so truncation stays readable.
         (contents (substring printed 1 -1)))
    (concat "\""
            (truncate-string-to-width
             contents (- width 2) 0 nil "…")
            "\"")))

(defun pilish--session-delete-project-context (session items)
  "Return SESSION's safe project token within ITEMS, or \"unknown\".
Reuse `pilish--session-project-fields', including its validated project
identity, shortest distinguishing labels, generated collision tokens,
and malformed-metadata placeholder.  ITEMS is the browser's complete
loaded snapshot, so filtering and either scope cannot silently change
that context; an unusable placeholder never falls back to the archive
path."
  (or
   (condition-case nil
       (let* ((fields (pilish--session-project-fields items "…"))
              (token (gethash (pilish--session-item-key session) fields)))
         (unless (or (null token)
                     (equal token pilish--session-project-placeholder))
           (pilish--session-delete-safe-text token)))
     (error nil))
   "unknown"))

(defun pilish--session-direct-child-items (parent items)
  "Return PARENT's known direct child sessions in ITEMS.
Both sides use the canonical family identity established by
`pilish--session-item-key' and `pilish--thread-parent-identity'.
Equivalent path aliases therefore match and duplicate child aliases
count once.  Grandchildren are deliberately absent: deleting a
session does not cascade, and direct children become roots only when
the parent no longer appears elsewhere in the archive."
  (let* ((memo (make-hash-table :test 'equal))
         (parent-key
          (condition-case nil
              (pilish--session-item-key parent memo)
            (error nil)))
         (seen (make-hash-table :test 'equal))
         children)
    (when (stringp parent-key)
      (dolist (item items)
        (condition-case nil
            (let* ((key (pilish--session-item-key item memo))
                   (parent-identity
                    (or (plist-get item :canonicalParentSession)
                        (pilish--thread-parent-identity
                         (plist-get item :parentSessionPath) key memo
                         (pilish--session-parent-anchor item)))))
              (when (and (stringp key)
                         (equal parent-identity parent-key)
                         (not (equal key parent-key))
                         (not (gethash key seen)))
                (puthash key t seen)
                (push item children)))
          ;; One malformed item cannot suppress warnings for the rest
          ;; of the already loaded snapshot.
          (error nil))))
    (nreverse children)))

(defun pilish--session-delete-child-warning (children)
  "Return the delete-prompt warning for direct CHILDREN, or nil.
The count and non-cascading/root consequence are always explicit.
At most three names are included, each display-bounded; when there are
more, do not format names that the prompt will omit."
  (when children
    (let* ((count (length children))
           (name-list
            (when (<= count 3)
              (format
               " (%s)"
               (mapconcat
                #'identity
                (sort
                 (mapcar
                  (lambda (item)
                    (pilish--session-delete-prompt-component
                     (pilish--session-display-name item)
                     pilish--session-delete-child-name-width))
                  children)
                 #'string<)
                ", ")))))
      (format (concat " %d direct child session%s%s are not deleted;"
                      " they become roots if their parent leaves the archive.")
              count (if (= count 1) "" "s") (or name-list "")))))

(defun pilish--session-delete-prompt (session items children trash-p)
  "Return contextual confirmation for deleting SESSION from ITEMS.
CHILDREN are its known direct forks.  TRASH-P selects wording that
exactly matches the optional trash argument later passed to
`delete-file'.  Policy leads the prompt; every metadata component is
sanitized, display-bounded, and truthfully ellipsized."
  (let ((name
         (pilish--session-delete-prompt-component
          (pilish--session-display-name session)
          pilish--session-delete-name-width))
        (project
         (pilish--session-delete-prompt-component
          (pilish--session-delete-project-context session items)
          pilish--session-delete-project-width)))
    (concat
     (if trash-p
         "Move session file to trash"
       "Permanently delete session file")
     " in project " project " — session " name "."
     (pilish--session-delete-child-warning children)
     " Other Emacs instances or external processes are not checked."
     " Continue? ")))

(defun pilish--session-delete-file-state (path)
  "Observe PATH's modification time, byte size, and file identity, or nil.
Ignore access time and request uncached metadata from remote handlers."
  (let ((remote-file-name-inhibit-cache t))
    (when-let* ((attributes (file-attributes path)))
      (list (file-attribute-modification-time attributes)
            (file-attribute-size attributes)
            (file-attribute-file-identifier attributes)))))

(defun pilish-session-browser-delete ()
  "Delete the session at point after contextual confirmation.
The prompt leads with whether Emacs will move the file to trash or
permanently delete it, then identifies the display-bounded human
session name and project and reports known direct child forks from the
full loaded snapshot.  Children are not cascade-deleted; they appear
as roots if the parent no longer appears elsewhere in the archive,
while deeper descendants retain their own parents.  Live/child matching
uses canonical identity, but `delete-file' receives the selected raw
pathname, preserving symlink and file-handler semantics.

Refuse a session used by a live Pilish process before prompting and
check that identity again after confirmation.  Also recompute the
selected pathname's canonical identity and compare its file's modification
time, size, and identity with their pre-prompt values.  Reject observed
changes or unavailable metadata.  This is not locking: idle external
writers are undetectable, and a writer can still change the file after
the final check.  The prompt discloses that live detection covers only
Pilish processes in this Emacs, not another Emacs or an external process.
Cancellation and a signaled `delete-file' leave the browser snapshot
unrefreshed; success refreshes through the existing scan path."
  (interactive)
  (if-let* ((session (pilish--session-browser-item-at-point))
            (raw-path (pilish--session-browser-path-at-point)))
      (let* ((trash-p (and delete-by-moving-to-trash t))
             ;; Matching is canonical, but destructive action is not:
             ;; `delete-file' must receive the raw retained pathname so
             ;; symlinks and file-name handlers keep Emacs semantics.
             (identity (or (condition-case nil
                               (pilish--session-item-key session)
                             (error nil))
                           raw-path))
             (children nil)
             (name
              (pilish--session-delete-prompt-component
               (pilish--session-display-name session)
               pilish--session-delete-name-width)))
        ;; This first identity check must precede the prompt.
        (pilish--browse-ensure-session-closed identity)
        (setq children
              (pilish--session-direct-child-items
               session pilish--session-browser-items))
        ;; Local canonical identity follows the selected symlink to the
        ;; session file; `file-attributes' on the raw link would not.
        (let ((file-state (pilish--session-delete-file-state identity)))
          (when (y-or-n-p
                 (pilish--session-delete-prompt
                  session pilish--session-browser-items children trash-p))
            ;; A process can open the identity while confirmation is active.
            (pilish--browse-ensure-session-closed identity)
            (unless (and file-state
                         (equal identity
                                (condition-case nil
                                    (pilish--canonical-session-path raw-path)
                                  (error nil)))
                         (equal file-state
                                (pilish--session-delete-file-state identity)))
              (user-error "Selected session changed while awaiting confirmation"))
            ;; Keep the policy named by the prompt stable even if Lisp run
            ;; from the minibuffer changed the global option meanwhile.
            (let ((delete-by-moving-to-trash trash-p))
              (delete-file raw-path trash-p))
            (pilish--session-browser-fetch-and-render)
            (if trash-p
                (message "Pi: Moved %s to trash" name)
              (message "Pi: Permanently deleted %s" name)))))
    (message "Pi: No session at point")))

(defun pilish--browse-clean-session-name (name)
  "Return NAME cleaned for a session_info append.
CR/LF runs collapse to single spaces, then surrounding whitespace
trims (pi's appendSessionInfo order)."
  (string-trim (replace-regexp-in-string "[\r\n]+" " " name)))

(defun pilish--browse-last-entry-id-in-buffer ()
  "Return the id of the last parseable non-header line in the current buffer.
Blank or malformed trailing lines are skipped: pi's loader ignores them,
so parenting an append to one would detach it from the conversation (the
leaf walk would stop at a null parent).  The header ends the search — a
header-only file reads as nil."
  (goto-char (point-max))
  (catch 'done
    (while t
      (skip-chars-backward " \t\r\n")
      (if (bobp)
          (throw 'done nil)
        (let* ((bol (line-beginning-position))
               (data (and (> (point) bol)
                          (pilish--parse-json-line
                           (buffer-substring-no-properties bol (point))))))
          (cond
           ((and (consp data) (equal (plist-get data :type) "session"))
            (throw 'done nil))
           ((consp data)
            (throw 'done (pilish--normalize-string-or-null
                          (plist-get data :id))))
           ;; Blank or malformed line: step over it and retry.
           (t (goto-char bol))))))))

(defun pilish--browse-session-file-state (path)
  "Re-read session file PATH fresh and describe its tail for an append.
Return a plist (:last-id ID-OR-NIL :ids HASH :newline-p BOOL), or nil when
PATH is missing or unreadable.  :last-id is the id of the last parseable
non-header line (see `pilish--browse-last-entry-id-in-buffer').
:ids holds every id-shaped string in the file — a superset of pi's entry
index — for fresh-id collision checks.  :newline-p reports whether the
file already ends in a newline.  The whole file is inserted once, fresh:
a live pi process appends concurrently, so the state must reflect the
physical file, never cached browser data."
  (condition-case nil
      (when (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (let ((ids (make-hash-table :test #'equal)))
            (goto-char (point-min))
            (while (re-search-forward
                    "\"id\"[ \t]*:[ \t]*\"\\([^\"]+\\)\"" nil t)
              (puthash (match-string-no-properties 1) t ids))
            (list :last-id (pilish--browse-last-entry-id-in-buffer)
                  :ids ids
                  :newline-p (and (> (point-max) (point-min))
                                  (eq (char-before (point-max)) ?\n))))))
    (error nil)))

(defun pilish--browse-fresh-entry-id (ids)
  "Return a random 8-hex entry id absent from IDS, like pi's generateId.
Collision checking is not optional: pi keys entries by id and walks
parent chains without a cycle guard, so a colliding id can wedge its
loader on the next session load.  After 100 failed tries fall back to a
wider 16-hex id, mirroring pi's full-UUID fallback."
  (cl-loop repeat 100
           for id = (format "%08x" (random #x100000000))
           when (not (gethash id ids))
           return id
           finally return (format "%08x%08x"
                                  (random #x100000000)
                                  (random #x100000000))))

(defun pilish--browse-append-session-entry (path type payload action)
  "Append one out-of-band entry of TYPE with PAYLOAD to session file PATH.
ACTION names the operation for messages (\"rename\", \"label\").
Mirrors pi's appenders without a live process.  The file is re-read
immediately before the append via
`pilish--browse-session-file-state' so :parentId is the id of
the current last parseable line (never the browser's cached leaf),
which keeps the entry from orphaning the context on the next load, and
the fresh id is checked against every id already in the file (pi's
loader cannot tolerate duplicates — see
`pilish--browse-fresh-entry-id').  When the file does not end
in a newline a separator is inserted first — bytes are never glued
onto a partial line.  PAYLOAD's pairs are encoded verbatim after the
shared :type/:id/:parentId/:timestamp head, so an omitted key stays
omitted (clearing a label relies on that: the load-time fold treats an
absent :label as cleared).

The file is NEVER created: pi's _persist appends per-entry once the
file exists, and its full-rewrite path is an exclusive create ('wx')
that only runs when the file never existed — creating it here would
crash pi's next flush with EEXIST.  So this helper only ever appends
to an existing readable file.  Races: a concurrent pi append between
the read and the write turns our line into a benign sibling (name
resolution is file-order latest-wins and projection filters these
bookkeeping entries), and stale-parent orphans are impossible by
construction.  A session live elsewhere sees the change on its next
state refresh.  Return non-nil when the line was appended; nil (with a
message) when PATH is unreadable."
  (if-let* ((state (pilish--browse-session-file-state path)))
      (let ((line (json-encode
                   (append (list :type type
                                 :id (pilish--browse-fresh-entry-id
                                      (plist-get state :ids))
                                 :parentId (plist-get state :last-id)
                                 :timestamp (format-time-string
                                             "%Y-%m-%dT%H:%M:%S.%3NZ"
                                             (current-time) t))
                           payload))))
        (let ((coding-system-for-write 'utf-8))
          (write-region
           (concat (unless (plist-get state :newline-p) "\n") line "\n")
           nil path 'append))
        t)
    (message "Pi: Cannot %s: session file is unreadable: %s" action path)
    nil))

(defun pilish--browse-append-session-info (path name)
  "Append a session_info entry naming session file PATH to NAME, out-of-band.
Thin wrapper over `pilish--browse-append-session-entry' with
the session_info payload (:name NAME); see there for the freshness,
race, and never-create contract.  Return non-nil when the line was
appended."
  (pilish--browse-append-session-entry
   path "session_info" (list :name name) "rename"))

(defun pilish-session-browser-rename ()
  "Rename the session at point.
Prompt once; empty or whitespace-only input cancels with a message
\(names cannot be cleared, matching the TUI).  Dispatch on
current-vs-other session (see
`pilish--browse-session-file-matches-p'):
  - Current session: `pilish-set-session-name' RPC, then
    refresh.  Known benign race: the refresh may beat pi's
    session_info flush, leaving a stale name visible until
    \\[pilish-browse-refresh].
  - Other session: `pilish--browse-append-session-info'
    appends out-of-band, then refreshes; an unreadable file cancels
    with a message and no refresh."
  (interactive)
  (if-let* ((path (pilish--session-browser-path-at-point)))
      (let* ((item (cl-find path pilish--session-browser-items
                            :key (lambda (it) (plist-get it :path))
                            :test #'equal))
             (existing (and item
                            (pilish--normalize-string-or-null
                             (plist-get item :name))))
             (input (read-string "Rename session: " (or existing "")))
             (clean (pilish--browse-clean-session-name input)))
        (if (string-empty-p clean)
            (message "Pi: Rename cancelled")
          (if (pilish--browse-session-file-matches-p
               (pilish--get-chat-buffer) path)
              (progn
                (pilish-set-session-name clean)
                (pilish--session-browser-fetch-and-render))
            (when (pilish--browse-append-session-info path clean)
              (pilish--session-browser-fetch-and-render)))))
    (message "Pi: No session at point")))

;;;; Point-Preserving Rerender

(defun pilish--browse-capture-point-anchor ()
  "Return the point anchor (IDENT . OFFSET) for the section at point.
IDENT is the section's `magit-section-ident'; OFFSET is the distance
from the section start.  Return nil when point sits on no content
section: either there is no section at point, or only the root
section, which every render recreates over the whole buffer and which
carries no identity (a loading-state render produces nothing else).
Used to both capture point before a render and to carry a position
across a fetch cycle whose intermediate renders destroy sections."
  (let ((section (magit-current-section)))
    (when (and section (not (eq section magit-root-section)))
      (cons (magit-section-ident section)
            (- (point) (oref section start))))))

(defun pilish--browse-rerender-preserving-point
    (buf render-fn &optional fallback missing-section-fn ignore-current-anchor)
  "Erase BUF, render via RENDER-FN, restore point by section identity.
The section at point is captured as a `magit-section-ident' before the
erase; after rendering, point moves to that section's start (plus the
captured intra-section column offset).  Falls back to `point-min' when
the section no longer exists (e.g. filtered away or state change).

FALLBACK, when non-nil, is a `(IDENT . OFFSET)' anchor (from
`pilish--browse-capture-point-anchor') captured before an
intermediate render destroyed the sections point sat on — the fetch
cycle renders a loading state before the final render.  It is used
only when the point-local capture yields no anchor, so a plain
rerender (no FALLBACK) behaves exactly as before.

MISSING-SECTION-FN, when non-nil, is called after rendering with the
chosen anchor when its exact section is absent (and also when there
was no anchor).  It may return another Magit section to select.  This
keeps tree-specific ancestor/active-path policy inside the tree
browser while retaining this function as the one point-restoration
mechanism for both browsers.

When IGNORE-CURRENT-ANCHOR is non-nil, do not capture the section under
point before erasing; only FALLBACK may provide an anchor.  The tree
browser uses this when another session file takes ownership of a reused
buffer, so a shared node id from the old file cannot survive its loading
render and override fresh active-leaf orientation.

After the restore, every live window displaying BUF is synced to the
restored position: `erase-buffer' clamps all displaying windows to
bob and `goto-char' moves only the buffer's own point, so without
`set-window-point' a pane whose window is not selected keeps showing
point-at-top (same idiom as `pilish--with-scroll-preservation'
in ui.el).  The sync covers exact, resolved, and point-min restore
paths."
  (with-current-buffer buf
    (let* ((anchor (or (and (not ignore-current-anchor)
                            (pilish--browse-capture-point-anchor))
                       fallback))
           (ident (car anchor))
           (offset (cdr anchor)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall render-fn buf)
        (let ((new (or (and ident (magit-get-section ident))
                       (and missing-section-fn
                            (funcall missing-section-fn anchor)))))
          (if new
              (let ((start (oref new start))
                    (end (oref new end)))
                (goto-char start)
                (when (and (integerp offset) (> offset 0))
                  (forward-char
                   (min offset (1- (- (or end (point-max)) start))))))
            (goto-char (point-min)))
          ;; Restoration by identity must not reveal a user's fold.  If
          ;; the exact or fallback section is inside an invisible extent,
          ;; promote point to the visible fold header before any window is
          ;; synchronized.
          (goto-char (pilish--browse-repair-folded-position (point)))
          ;; `erase-buffer' clamped every displaying window's point to
          ;; bob; the `goto-char' above moved only the buffer's own
          ;; point.  Sync all live windows displaying BUF — the list
          ;; spans live frames and yields only live windows (same
          ;; idiom as `pilish--with-scroll-preservation').
          (dolist (w (get-buffer-window-list buf nil t))
            (set-window-point w (point)))
          (force-mode-line-update))))))

;;;; Fetch and Render

(defun pilish--session-browser-fetch-and-render ()
  "Fetch sessions and re-render the session browser.
Sessions are read from disk, so no live pi process is required.

The point anchor is captured before the loading-state render — that
render destroys the session sections, so without carrying the anchor
across the cycle the final render would find nothing under point to
restore (E2E defect A4).  A refresh issued while another fetch is
still loading finds no sections to capture and reuses the in-flight
cycle's anchor (see `pilish--session-browser-fetch-anchor')."
  (let* ((buf (current-buffer))
         ;; Claim the generation before rendering: rendering can invoke
         ;; file-name handlers through live-session identity lookup, and
         ;; a reentrant newer fetch must remain newer after this one resumes.
         (token (setq pilish--session-browser-fetch-token
                      (1+ pilish--session-browser-fetch-token)))
         (scope pilish--session-browser-scope)
         (anchor (or (pilish--browse-capture-point-anchor)
                     ;; Mid-flight refresh: the loading render already
                     ;; destroyed the sections under point, so carry
                     ;; the anchor the in-flight cycle captured.
                     (and pilish--session-browser-loading
                          pilish--session-browser-fetch-anchor))))
    (setq pilish--session-browser-loading t
          pilish--session-browser-fetch-anchor anchor)
    ;; Loading-state render: default point behavior (nothing to keep).
    (pilish--session-browser-rerender)
    (pilish--browse-load-sessions
     scope
     (lambda (items error)
       (when (buffer-live-p buf)
         (pilish--session-browser-apply-scan
          buf items error anchor scope token)))
     token)))

(defun pilish--session-browser-apply-scan
    (buf items error anchor &optional scope generation)
  "Store a completed scan's ITEMS and ERROR in BUF, then rerender.
ITEMS pass through `pilish--session-canonicalize-items' — one keyed
row per session identity — so every view and query transition renders
each session once.  ANCHOR is the pre-fetch point anchor handed to
`pilish--session-browser-rerender'.  SCOPE owns this snapshot; direct
legacy callers may omit it to use BUF's current scope.  When GENERATION
is non-nil, hand-built item canonicalization and publication occur only
while BUF still owns it; reentrant newer fetches retain their state."
  (with-current-buffer buf
    (when (or (null generation)
              (pilish--session-browser-generation-current-p
               buf generation))
      (let ((canonical
             (pilish--session-canonicalize-items
              items (and generation buf) generation)))
        (when (and (not (eq canonical
                            pilish--session-canonicalization-stale))
                   (or (null generation)
                       (pilish--session-browser-generation-current-p
                        buf generation)))
          (setq pilish--session-browser-loading nil
                pilish--session-browser-fetch-anchor nil
                pilish--session-browser-error error
                pilish--session-browser-items canonical
                pilish--session-browser-items-scope
                (or scope pilish--session-browser-scope))
          (pilish--session-browser-rerender anchor))))))

(defun pilish--session-browser-rerender (&optional fallback)
  "Re-render the session browser from local state, preserving point.
FALLBACK is a pre-fetch `(IDENT . OFFSET)' anchor handed to
`pilish--browse-rerender-preserving-point' for the fetch cycle's final
render.

Only one render transaction runs in the buffer.  A reentrant request from
Magit's callback-capable visibility hook replaces the pending request;
the active generation aborts immediately after that hook, unwinds its
dynamic section parent, and only then paints the newest request cleanly."
  (if pilish--session-browser-rendering-p
      ;; A one-element list remains non-nil when FALLBACK itself is nil.
      (setq pilish--session-browser-pending-rerender (list fallback))
    (setq pilish--session-browser-rendering-p t)
    (unwind-protect
        (let ((generation pilish--session-browser-fetch-token))
          (catch 'pilish--session-browser-stale-render
            (pilish--session-browser-ensure-render-current
             (current-buffer) generation)
            (pilish--browse-rerender-preserving-point
             (current-buffer) #'pilish--session-browser-render fallback)
            (pilish--session-browser-ensure-render-current
             (current-buffer) generation)))
      (setq pilish--session-browser-rendering-p nil)
      (when-let* ((pending pilish--session-browser-pending-rerender))
        (setq pilish--session-browser-pending-rerender nil)
        (apply #'pilish--session-browser-rerender pending)))))

;;;; Tree Browser Section Classes and Keymaps

(defclass pilish-tree-node-section (magit-section)
  ((keymap :initform 'pilish-tree-node-section-map))
  "Section class for a tree node in the tree browser.")

(defun pilish--register-section-types ()
  "Register browse section classes in `magit--section-type-alist'.
Wrapped because that alist is a private Magit internal; if Magit ever
changes the mechanism, this is the single place to adapt.

Unregistered types (e.g. the `time-group' headings, which are not
interactive) silently fall back to the plain `magit-section' section class;
that is fine for display-only sections."
  (setf (alist-get 'session magit--section-type-alist)
        'pilish-session-section)
  (setf (alist-get 'tree-node magit--section-type-alist)
        'pilish-tree-node-section))
(pilish--register-section-types)

(defvar pilish-tree-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pilish-browse-mode-map)
    (define-key map (kbd "f") #'pilish-tree-browser-cycle-filter)
    (define-key map (kbd "l") #'pilish-tree-browser-set-label)
    (define-key map (kbd "/") #'pilish-tree-browser-search)
    (define-key map (kbd "RET") #'pilish-tree-browser-navigate)
    (define-key map (kbd "?") #'pilish-tree-browser-dispatch)
    (define-key map (kbd "h") #'pilish-tree-browser-dispatch)
    map)
  "Keymap for the tree browser.")

(defvar pilish-tree-node-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pilish-tree-browser-navigate)
    (define-key map (kbd "l") #'pilish-tree-browser-set-label)
    map)
  "Keymap for tree node sections.")

;;;; Tree Browser Default Options

(defcustom pilish-tree-browser-default-filter 'no-tools
  "Initial filter of a newly created tree browser buffer.
Filters select which entries the projected tree shows:

- `default'       projected entries except model/thinking changes and
                  the system prompt;
- `no-tools'      `default' without tool results;
- `user-only'     user messages only;
- `labeled-only'  labeled nodes only;
- `all'           all projected/displayable content.

Projection removes raw label, session_info, custom, usage, and
context_edit bookkeeping entries before these filters run, promoting
their children to the nearest displayable ancestor.  Thus `all' does
not restore raw bookkeeping.  In every filter, empty tool-dispatch
assistant messages stay hidden unless aborted or carrying an error
message (see `pilish--browse-node-visible-p').

The value initializes browser buffers when they are created, and
again whenever the browser major mode is explicitly re-run.  An
existing browser buffer — including one hidden with
\\[quit-window] and reopened — otherwise keeps its locally changed
state, so cycling filters with `f' stays local to that buffer."
  :type '(choice (const :tag "No tool results" no-tools)
                 (const :tag "Default" default)
                 (const :tag "User messages only" user-only)
                 (const :tag "Labeled nodes only" labeled-only)
                 (const :tag "All projected/displayable content" all))
  :group 'pilish)

;;;; Tree Browser State

(defvar-local pilish--tree-browser-filter 'no-tools
  "Current filter mode of the tree browser.
One of `no-tools', `default', `user-only', `labeled-only', `all'.
New browser buffers start from
`pilish-tree-browser-default-filter'.")

(defvar-local pilish--tree-browser-tree nil
  "Projected tree from the last `--browse-load-tree' callback.
Vector of root nodes in the browse node dialect.")

(defvar-local pilish--tree-browser-leaf-id nil
  "Current leaf node ID from the tree response.")

(defvar-local pilish--tree-browser-visible-count 0
  "Count of visible entries from the last render.
Cached to avoid re-flattening the tree in the header-line.")

(defvar-local pilish--tree-browser-search-query nil
  "Current search query string, or nil.")

(defvar-local pilish--tree-browser-search-tokens nil
  "Parsed search tokens.")

(defvar-local pilish--tree-browser-loading nil
  "Non-nil while a fetch is in progress.")

(defvar-local pilish--tree-browser-fetch-anchor nil
  "Point anchor carried across the in-flight fetch cycle.
Same lifecycle as `pilish--session-browser-fetch-anchor': set
from the anchor captured at fetch start, reused by a refresh issued
while loading, cleared when the cycle's final render runs.")

(defvar-local pilish--tree-browser-fetch-lineage nil
  "Old selected-node lineage carried across the in-flight fetch cycle.
The list runs from the selected id toward its old root.  It is captured
before the loading render and before a replacement tree is installed,
then cleared with `pilish--tree-browser-fetch-anchor'.")

(defvar-local pilish--tree-browser-point-anchor nil
  "Last meaningful tree-node point anchor.
Unlike the fetch anchor, this survives a render with no matching rows,
so clearing a search can recover the selected section identity.  A
successful render that selects an ancestor or active-path fallback
updates it to that actual selection.")

(defvar-local pilish--tree-browser-point-lineage nil
  "Old lineage belonging to `pilish--tree-browser-point-anchor'.
This survives empty search/error renders with the anchor.  Missing-node
resolution consumes this captured lineage; it never reconstructs the
selected node's ancestry from a replacement tree.")

(defvar-local pilish--tree-browser-state-file nil
  "Session file that owns this buffer's orientation and point anchors.
A fetch for a different file resets these states before its loading
render, even when both files contain identical projected node ids.")

(defvar-local pilish--tree-browser-point-oriented-p nil
  "Non-nil after this browser has rendered its first tree snapshot.
The first completed render deliberately selects the active projected
leaf or nearest visible ancestor.  Later renders preserve the user's
section instead and use path-aware fallback only if it disappears.
Loading and error renders do not set this flag.")

(defvar-local pilish--tree-browser-error nil
  "Error message string from the last tree fetch, or nil on success.
Rendered as an error state with a zero visible count.")

(defvar-local pilish--tree-browser-diagnostic nil
  "Non-fatal tree diagnostic from the last successful fetch.
Unlike `pilish--tree-browser-error', this warning renders above useful
canonical rows and keeps the loaded-file navigation guard armed.")

(defvar-local pilish--tree-browser-rendering-p nil
  "Non-nil while any tree rerender owns the buffer transaction.")

(defvar-local pilish--tree-browser-pending-load nil
  "Newest completed load deferred by a reentrant tree render.
The value is the argument list for `pilish--tree-browser-apply-load'.")

(defvar-local pilish--tree-browser-pending-rerender nil
  "Newest ordinary rerender deferred by an active tree render.
A completed pending load takes precedence because it publishes newer
state; otherwise this stores arguments for `pilish--tree-browser-rerender'.")

(defvar pilish--tree-browser-render-generation nil
  "Dynamically bound generation fencing one tree render.")

(defvar pilish--tree-browser-render-owner nil
  "Dynamically bound session-file owner fencing one tree render.")

(defvar-local pilish--tree-browser-fetch-token 0
  "Generation counter for tree-browser fetches.
The browser fetch cycle claims its generation before rendering and
passes it to `pilish--browse-load-tree', exactly like the session-side
cycle.  Direct loader callers that omit a generation claim one at the
loader seam.  Deferred or yielding reads validate both their captured
generation and, for browser-owned fetches, session-file owner before
publishing.  The permanent local binding lets mode reinitialization
advance rather than reset the counter, independently of owner state.")

(put 'pilish--tree-browser-fetch-token 'permanent-local t)

(defun pilish--tree-browser-generation-current-p
    (buf generation &optional owner check-owner-p)
  "Return non-nil when BUF still owns tree GENERATION and OWNER.
BUF must still be a tree browser because its permanent generation also
survives unrelated major modes.  When CHECK-OWNER-P is non-nil, OWNER
must also equal BUF's `pilish--tree-browser-state-file'.  Direct loader
callers use generation ownership alone; full browser fetches pass the
session-file owner they claimed before their loading render."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (and (derived-mode-p 'pilish-tree-browser-mode)
              (eq generation pilish--tree-browser-fetch-token)
              (or (not check-owner-p)
                  (equal owner pilish--tree-browser-state-file))))))

(defun pilish--tree-browser-render-current-p ()
  "Return non-nil when the dynamically fenced render still owns state."
  (or (null pilish--tree-browser-render-generation)
      (pilish--tree-browser-generation-current-p
       (current-buffer)
       pilish--tree-browser-render-generation
       pilish--tree-browser-render-owner t)))

(defun pilish--tree-browser-ensure-render-current ()
  "Abort the current tree render when ownership was superseded."
  (unless (pilish--tree-browser-render-current-p)
    (throw 'pilish--tree-browser-stale-render nil)))

(defvar-local pilish--tree-browser-loaded-file nil
  "Session file the current tree was loaded from, or nil on error states.
Set, when a fetch succeeds, to the file the FETCH read (resolved at
fetch start — a chat session switch mid-read cannot retarget the
labeler onto a tree the browser is not showing);
`pilish--browse-set-label' compares against it to refuse
labeling a session the chat has since left.  Orientation/anchor
ownership is tracked separately by `pilish--tree-browser-state-file'
so a transient read error does not make a retry look like a file switch.")

(defvar pilish--tree-browser-resolution-lineage nil
  "Dynamically bound old selected lineage for one tree rerender.
`pilish--tree-browser-missing-section' consults this only after exact
section restoration fails.")

(defconst pilish--tree-filter-modes
  '(no-tools default user-only labeled-only all)
  "Available filter modes for the tree browser, in cycle order.")

;;;; Tree Browser Dispatch Transient

(defun pilish--tree-dispatch-heading ()
  "Return heading string for the tree browser dispatch transient.
Shows current filter mode.
Sibling of `pilish--tree-browser-header-line' — both
format the same state variables for different contexts.  Transient
evaluates group descriptions in the invoking browser buffer (see
`transient-with-shadowed-buffer' inside `transient--insert-group'),
so this buffer-local read sees the browser's state on the real
rendering path."
  (format "filter:%s" pilish--tree-browser-filter))

(transient-define-prefix pilish-tree-browser-dispatch ()
  "Tree browser help."
  [:description pilish--tree-dispatch-heading
   ["Actions"
    ("RET" "continue from selected turn" pilish-tree-browser-navigate)
    ("l" "label" pilish-tree-browser-set-label)
    ("g" "refresh" pilish-browse-refresh)
    ("q" "quit" quit-window)]
   ["Navigate & Fold"
    ("TAB" "toggle row/nearest ancestor fold" pilish-browse-toggle-fold)
    ("<backtab>" "fold all (prefix: unfold)" pilish-browse-fold-all)
    ("^" "visible parent row" pilish-browse-goto-parent-row)]
   ["Filter"
    ("f" "cycle filter" pilish-tree-browser-cycle-filter)
    ("d" "default: hide model/thinking changes"
     pilish--tree-browser-filter-default)
    ("n" "no-tools: default without tool results"
     pilish--tree-browser-filter-no-tools)
    ("u" "user-only: user messages"
     pilish--tree-browser-filter-user-only)
    ("L" "labeled-only: labeled nodes"
     pilish--tree-browser-filter-labeled-only)
    ("a" "all projected/displayable content"
     pilish--tree-browser-filter-all)
    ("/" "search semantic text" pilish-tree-browser-search)]])

;;;; Tree Browser Faces

(defface pilish-tree-user
  '((t :inherit font-lock-keyword-face))
  "Face for user messages in the tree browser."
  :group 'pilish)

(defface pilish-tree-assistant
  '((t :inherit font-lock-string-face))
  "Face for assistant messages in the tree browser."
  :group 'pilish)

(defface pilish-tree-tool
  '((t :inherit shadow))
  "Face for tool results in the tree browser."
  :group 'pilish)

(defface pilish-tree-compaction
  '((t :inherit shadow :slant italic))
  "Face for compaction entries in the tree browser."
  :group 'pilish)

(defface pilish-tree-summary
  '((t :inherit warning))
  "Face for branch summaries in the tree browser."
  :group 'pilish)

(defface pilish-tree-active
  '((t :weight bold))
  "Face for current-entry and active-path markers in the tree browser."
  :group 'pilish)

(defface pilish-tree-label
  '((t :inherit success :weight bold))
  "Face for node labels in the tree browser."
  :group 'pilish)

(defface pilish-tree-connector
  '((t :inherit shadow))
  "Face for tree connectors (├─, └─, │) in the tree browser."
  :group 'pilish)

;;;; Tree Browser Mode

(define-derived-mode pilish-tree-browser-mode
  pilish-browse-mode "Pi-Tree"
  "Major mode for browsing pi conversation tree.
\\{pilish-tree-browser-mode-map}
Buffers start from `pilish-tree-browser-default-filter'; an existing
browser buffer keeps its chosen filter when hidden and reopened, and
re-running this mode re-initializes from the current default."
  :group 'pilish
  ;; Preserve and advance the generation across the parent's
  ;; `kill-all-local-variables', invalidating every pre-reset callback.
  ;; Other tree state, including its file owner, still resets normally.
  (if (local-variable-p 'pilish--tree-browser-fetch-token)
      (cl-incf pilish--tree-browser-fetch-token)
    (setq-local pilish--tree-browser-fetch-token 0))
  ;; Initial filter from the default option, before hooks run (see
  ;; `pilish-session-browser-mode' for the timing rationale).
  (setq pilish--tree-browser-filter
        pilish-tree-browser-default-filter)
  (setq-local header-line-format
              '(:eval (pilish--tree-browser-header-line)))
  (setq pilish--browse-margin-width
        pilish--tree-margin-width)
  (setq-local right-margin-width pilish--tree-margin-width)
  (add-hook 'window-configuration-change-hook
            #'pilish--browse-apply-margins nil t))

;;;; Tree Browser Buffer Management

(defun pilish--tree-browser-buffer-name (dir)
  "Return tree browser buffer name for DIR."
  (format "*pilish-tree:%s*"
          (pilish--route-preserving-abbreviate-file-name dir)))

(defun pilish--get-or-create-tree-browser (dir)
  "Get or create tree browser buffer for DIR.
A newly created buffer's mode initialization starts from
`pilish-tree-browser-default-filter'; an existing buffer is returned
unchanged (the mode never re-runs), so hiding with
\\[quit-window] and reopening keeps its locally chosen filter until
the buffer is killed or the browser major mode is explicitly re-run."
  (let* ((name (pilish--tree-browser-buffer-name dir))
         (buf (get-buffer name)))
    (or buf
        (with-current-buffer (generate-new-buffer name)
          (setq default-directory dir)
          (pilish-tree-browser-mode)
          (current-buffer)))))

;;;; Tree Node Formatting

(defun pilish--tree-node-face (node)
  "Return the face for NODE based on its type and role."
  (let ((type (plist-get node :type))
        (role (plist-get node :role)))
    (pcase type
      ("message"
       (pcase role
         ("user" 'pilish-tree-user)
         ("assistant" 'pilish-tree-assistant)
         ("branchSummary" 'pilish-tree-summary)
         ("compactionSummary" 'pilish-tree-compaction)
         (_ 'default)))
      ("tool_result" 'pilish-tree-tool)
      ("compaction" 'pilish-tree-compaction)
      ("branch_summary" 'pilish-tree-summary)
      ("model_change" 'shadow)
      ("thinking_level_change" 'shadow)
      (_ 'default))))

(defun pilish--tree-node-type-label (node)
  "Return a short type label for NODE."
  (let ((type (plist-get node :type))
        (role (plist-get node :role)))
    (pcase type
      ("message"
       (pcase role
         ("user" "you")
         ("assistant" "ast")
         ("branchSummary" "sum")
         ("compactionSummary" "cmp")
         ("bashExecution" "sh")
         (_ role)))
      ("tool_result"
       (or (plist-get node :toolName) "tool"))
      ("compaction" "compact")
      ("branch_summary" "summary")
      ("model_change" "model")
      ("thinking_level_change" "think")
      (_ type))))

(defun pilish--tree-strip-bracket-preview (node)
  "Return preview text for NODE with bracket wrappers stripped.
The upstream `formatToolCall' wraps previews as `[name: args]'.  Since
the type-label column already identifies the tool, the wrapper is
redundant.  Prefers `formattedToolCall' over `preview'."
  (let ((text (or (plist-get node :formattedToolCall)
                  (plist-get node :preview)
                  "")))
    (cond
     ;; [name: content] → content
     ((string-match "^\\[.+?: \\(.*\\)\\]$" text)
      (match-string 1 text))
     ;; [name] (no args) → empty
     ((string-match "^\\[.+\\]$" text)
      "")
     ;; Plain text → as-is
     (t text))))

(defun pilish--tree-node-preview (node)
  "Return preview text for NODE."
  (let ((type (plist-get node :type)))
    (pcase type
      ("compaction"
       (format "compacted (%s tokens)"
               (pilish--format-tokens-compact
                (or (plist-get node :tokensBefore) 0))))
      ("branch_summary"
       (pilish--first-nonempty-line
        (or (plist-get node :summary) "")))
      ("model_change"
       (format "%s/%s" (plist-get node :provider) (plist-get node :modelId)))
      ("thinking_level_change"
       (or (plist-get node :thinkingLevel) ""))
      ("tool_result"
       (pilish--tree-strip-bracket-preview node))
      ("message"
       (if (equal (plist-get node :role) "bashExecution")
           (pilish--tree-strip-bracket-preview node)
         (or (plist-get node :preview) "")))
      (_ (or (plist-get node :preview) "")))))

(defun pilish--tree-format-node-line (node is-active &optional is-current)
  "Format a single NODE into a display string.
IS-ACTIVE is non-nil if the node is on the active path.  IS-CURRENT
marks the actual projected leaf, rather than merely its nearest
visible ancestor.  The ASCII marker column is separate from tree
connectors: `@' means current, `*' means an active ancestor, and a
blank means inactive.  A label remains in the right margin and also
appears inline so keyboard navigation and search expose the same text."
  (let* ((face (pilish--tree-node-face node))
         (type-label (pilish--tree-node-type-label node))
         (preview (pilish--tree-node-preview node))
         (label (plist-get node :label))
         (marker (cond
                  (is-current
                   (pilish--propertize-face "@ " 'pilish-tree-active))
                  (is-active
                   (pilish--propertize-face "* " 'pilish-tree-active))
                  (t "  ")))
         (type-str (pilish--propertize-face
                    (format "%-7s" type-label) face))
         (label-str
          (if label
              (concat
               (pilish--propertize-face
                (format "[%s]"
                        (replace-regexp-in-string
                         "[\n\t]" " " (format "%s" label)))
                'pilish-tree-label)
               " ")
            ""))
         (preview-str (pilish--propertize-face preview face)))
    (concat marker type-str " " label-str preview-str)))

;;;; Tree Browser Point Orientation

(defun pilish--tree-anchor-node-id (anchor)
  "Return the tree-node id encoded in point ANCHOR, or nil.
ANCHOR has the `(IDENT . OFFSET)' shape returned by
`pilish--browse-capture-point-anchor'."
  (cl-loop for component in (car-safe anchor)
           when (and (consp component)
                     (eq (car component) 'tree-node))
           return (cdr component)))

(defun pilish--tree-parent-index (tree)
  "Return a hash table mapping string ids in projected TREE to parent ids.
Traversal is iterative so browser orientation remains safe for deep
conversation trees.  Nil/empty legacy ids are omitted.  A duplicate
addressable id fails closed to an empty index.  Canonical :ambiguousId
and :ambiguousParent nodes are indexed as roots so point fallback cannot
cross an uncertain ancestry edge.  `pilish--tree-path-to-root' guards
unique-id parent cycles."
  (let ((parents (make-hash-table :test #'equal))
        (stack (append tree nil)))
    (when (pilish-jsonl-tree-ids-unique-p tree)
      (while stack
        (let* ((node (pop stack))
               (id (pilish--normalize-string-or-null
                    (plist-get node :id)))
               (children (plist-get node :children)))
          (when id
            (puthash id
                     (unless (or (plist-get node :ambiguousId)
                                 (plist-get node :ambiguousParent))
                       (pilish--normalize-string-or-null
                        (plist-get node :parentId)))
                     parents))
          (when (vectorp children)
            (dotimes (i (length children))
              (push (aref children i) stack))))))
    parents))

(defun pilish--tree-path-to-root (id parents)
  "Return IDs from ID through its ancestors according to PARENTS.
PARENTS is the hash table from `pilish--tree-parent-index'.  Unknown
IDs return nil.  A seen set makes malformed projected cycles total."
  (let ((missing (make-symbol "missing"))
        (seen (make-hash-table :test #'equal))
        (current (pilish--normalize-string-or-null id))
        (result nil))
    (while (and current
                (not (gethash current seen))
                (not (eq (gethash current parents missing) missing)))
      (puthash current t seen)
      (push current result)
      (setq current (gethash current parents)))
    (nreverse result)))

(defun pilish--tree-rendered-section-index ()
  "Return `(BY-ID . FIRST)' for rendered tree-node sections.
BY-ID maps addressable string node ids to Magit section objects; FIRST
is the first tree-node section even when it is an unaddressable legacy
row.  The walk also supports future nested sections and is iterative
for deep trees."
  (let ((by-id (make-hash-table :test #'equal))
        (first nil)
        (stack nil))
    (when magit-root-section
      (dolist (child (reverse (oref magit-root-section children)))
        (push child stack)))
    (while stack
      (let ((section (pop stack)))
        (when (eq (oref section type) 'tree-node)
          (when-let* ((id (pilish--normalize-string-or-null
                           (oref section value))))
            (puthash id section by-id))
          (unless first (setq first section)))
        (dolist (child (reverse (oref section children)))
          (push child stack))))
    (cons by-id first)))

(defun pilish--tree-first-section-on-path (ids sections)
  "Return the first rendered section for IDS from SECTIONS, or nil."
  (cl-loop for id in ids
           for section = (gethash id sections)
           when section return section))

(defun pilish--tree-anchor-lineage (anchor tree)
  "Return ANCHOR's node lineage in TREE from selected id toward root.
Nil means ANCHOR is not an addressable tree node or its id is absent.
Call this before replacing TREE; missing-selection resolution must use
the old ancestry, not infer ancestry from the replacement snapshot."
  (when-let* ((id (pilish--tree-anchor-node-id anchor)))
    (pilish--tree-path-to-root id (pilish--tree-parent-index tree))))

(defun pilish--tree-browser-missing-section (_anchor)
  "Resolve a missing section after a tree-browser render.
On later snapshots, first use the selected node's lineage captured
from the OLD tree before filtering or replacement.  Never reconstruct
that ancestry from the new tree: a vanished sibling must fall back to
its surviving old parent before the new active leaf.  On the first
snapshot (or after that lineage has no rendered survivor), select the
new active projected leaf or nearest visible ancestor.  The first
rendered row is the deterministic final fallback."
  (let* ((parents (pilish--tree-parent-index pilish--tree-browser-tree))
         (rendered (pilish--tree-rendered-section-index))
         (sections (car rendered))
         (first (cdr rendered))
         (selected-path
          (and pilish--tree-browser-point-oriented-p
               pilish--tree-browser-resolution-lineage))
         (active-path
          (pilish--tree-path-to-root pilish--tree-browser-leaf-id parents)))
    (or (pilish--tree-first-section-on-path selected-path sections)
        (pilish--tree-first-section-on-path active-path sections)
        first)))

;;;; Tree Browser Rendering

(defun pilish--tree-browser-render (buf)
  "Render the tree browser in BUF from its buffer-local state.
A completed-load render dynamically fences its generation and owner;
checks before and immediately after Magit's visibility-hook seam abort
obsolete insertion when a newer load lands reentrantly."
  (with-current-buffer buf
    (pilish--tree-browser-ensure-render-current)
    (pilish--browse-begin-fold-render)
    (let* ((inhibit-read-only t)
           (tree pilish--tree-browser-tree)
           (leaf-id (pilish--normalize-string-or-null
                     pilish--tree-browser-leaf-id))
           (filter pilish--tree-browser-filter)
           (diagnostic pilish--tree-browser-diagnostic)
           (unique-ids-p (pilish-jsonl-tree-ids-unique-p tree))
           (published-values nil))
      (magit-insert-section (root)
        ;; `magit-insert-section' ran its visibility hook before entering
        ;; this body.  That hook can yield and complete a newer fetch.
        (pilish--tree-browser-ensure-render-current)
        (when (and diagnostic
                   (not pilish--tree-browser-loading)
                   (not pilish--tree-browser-error))
          (insert (pilish--propertize-face
                   (format "Warning: %s\n" diagnostic)
                   'warning)))
        (cond
         (pilish--tree-browser-loading
          (setq pilish--tree-browser-visible-count 0)
          (insert (pilish--propertize-face
                   "Loading tree..."
                   'pilish-activity-phase)
                  "\n"))
         (pilish--tree-browser-error
          (setq pilish--tree-browser-visible-count 0)
          (insert (pilish--propertize-face
                   (format "Error: %s\n" pilish--tree-browser-error)
                   'error)))
         ((not unique-ids-p)
          ;; Fail closed: bare duplicate ids cannot provide truthful
          ;; markers, section identities, or RET targets.  Nil-id legacy
          ;; rows remain permitted by `pilish-jsonl-tree-ids-unique-p'.
          (setq pilish--tree-browser-visible-count 0)
          (insert "Malformed conversation tree: duplicate entry ids.\n"))
         ((or (null tree) (= (length tree) 0))
          (setq pilish--tree-browser-visible-count 0
                published-values (make-hash-table :test #'equal))
          (insert "No conversation tree.\n"))
         (t
          (let* ((source-values (make-hash-table :test #'equal))
                 (flat (pilish--flatten-tree-for-display
                        tree leaf-id filter
                        pilish--tree-browser-search-tokens
                        source-values))
                 (active-ids (pilish--active-path-ids tree leaf-id))
                 (visible flat))
            (setq published-values source-values)
            (setq pilish--tree-browser-visible-count
                  (length visible))
            (if (null visible)
                (insert "No matching entries.\n")
              (cl-loop for tail on visible
                       for entry = (car tail)
                       for next = (cadr tail)
                       do
                (pilish--tree-browser-ensure-render-current)
                (let* ((node (nth 0 entry))
                       (depth (nth 1 entry))
                       (prefix (nth 2 entry))
                       (node-id (pilish--normalize-string-or-null
                                 (plist-get node :id)))
                       (ambiguous-p (plist-get node :ambiguousId))
                       ;; Keep a stable, non-string Magit identity for the
                       ;; one canonical display row.  The ambiguous bare id
                       ;; itself must not become a section/navigation target.
                       (section-value
                        (if ambiguous-p
                            (cons 'ambiguous-id node-id)
                          node-id))
                       (foldable
                        (and next (> (nth 1 next) depth)
                             (stringp section-value)))
                       ;; Legacy or ambiguous rows have no truthful
                       ;; occurrence identity and never receive @ or *.
                       (is-active (and node-id
                                       (not ambiguous-p)
                                       (gethash node-id active-ids)))
                       (is-current (and node-id leaf-id
                                        (not ambiguous-p)
                                        (equal node-id leaf-id)))
                       (prefix-str (pilish--propertize-face
                                    prefix
                                    'pilish-tree-connector))
                       ;; Marker (two characters), the padded type, then one
                       ;; separator.  %-7s is a minimum width, so use the
                       ;; actual string length for custom tool/type names.
                       (indicator-index
                        (+ 3
                           (length
                            (format "%-7s"
                                    (pilish--tree-node-type-label node)))))
                       (line (pilish--tree-format-node-line
                              node is-active is-current)))
                  (magit-insert-section node-section
                      (tree-node section-value)
                    ;; Fence the exact seam from the adversarial repro:
                    ;; the section visibility hook has just returned.
                    (pilish--tree-browser-ensure-render-current)
                    (pilish--browse-register-fold-row
                     node-section section-value depth 'tree foldable
                     (+ (length prefix) indicator-index))
                    (magit-insert-heading
                      ;; Preserve the established connector, @/* marker, and
                      ;; seven-column type label.  The fold glyph follows
                      ;; those discovery columns and precedes the preview.
                      (if foldable
                          (concat prefix-str
                                  (substring line 0 indicator-index)
                                  (pilish--browse-fold-indicator
                                   section-value)
                                  (substring line indicator-index))
                        (concat prefix-str line)))
                    (when-let* ((label (plist-get node :label)))
                      ;; 3 = "[" + "]" + 1 char padding
                      (let ((truncated
                             (pilish--truncate-string
                              label
                              (- pilish--tree-margin-width 3))))
                        (pilish--make-margin-overlay
                         (pilish--propertize-face
                          (format "[%s]" truncated)
                          'pilish-tree-label))))))))))))
      (pilish--tree-browser-ensure-render-current)
      ;; Commit both source membership and visibility only after every flat
      ;; section finished insertion and this render still owns its fenced
      ;; generation.  Generic point restoration runs after this function.
      (when published-values
        (pilish--browse-publish-fold-values published-values))
      (pilish--browse-finish-fold-render)
      (pilish--tree-browser-ensure-render-current))))

;;;; Tree Browser Header-Line

(defun pilish--tree-browser-header-line ()
  "Return header-line string for the tree browser.
Uses cached visible count from the last render to avoid redundant
tree flattening on every redisplay cycle."
  (let* ((filter pilish--tree-browser-filter)
         (query pilish--tree-browser-search-query)
         (total pilish--tree-browser-visible-count))
    (mapconcat #'identity
               (append (list (format "Tree [%s]" filter)
                             (format "(%d)" total))
                       (and query (list (format "/%s" query)))
                       (list (pilish--propertize-face
                              "@ current, * active path" 'shadow)
                             (pilish--propertize-face "?:help" 'shadow)))
               " │ ")))

;;;; Tree Browser Interactive Commands

(defun pilish-tree-browser-set-filter (filter)
  "Set the tree browser to FILTER and re-render it.
FILTER is one of the five symbols in `pilish--tree-filter-modes'."
  (interactive
   (list
    (intern
     (completing-read
      "Tree filter: "
      (mapcar #'symbol-name pilish--tree-filter-modes)
      nil t nil nil (symbol-name pilish--tree-browser-filter)))))
  (unless (memq filter pilish--tree-filter-modes)
    (user-error "Unknown tree filter: %s" filter))
  (setq pilish--tree-browser-filter filter)
  (pilish--tree-browser-rerender)
  (message "Pi: Filter: %s" filter))

(defun pilish--tree-browser-filter-default ()
  "Select the tree browser's `default' filter."
  (interactive)
  (pilish-tree-browser-set-filter 'default))

(defun pilish--tree-browser-filter-no-tools ()
  "Select the tree browser's `no-tools' filter."
  (interactive)
  (pilish-tree-browser-set-filter 'no-tools))

(defun pilish--tree-browser-filter-user-only ()
  "Select the tree browser's `user-only' filter."
  (interactive)
  (pilish-tree-browser-set-filter 'user-only))

(defun pilish--tree-browser-filter-labeled-only ()
  "Select the tree browser's `labeled-only' filter."
  (interactive)
  (pilish-tree-browser-set-filter 'labeled-only))

(defun pilish--tree-browser-filter-all ()
  "Select the tree browser's `all' filter."
  (interactive)
  (pilish-tree-browser-set-filter 'all))

(defun pilish-tree-browser-cycle-filter ()
  "Cycle the tree browser filter mode."
  (interactive)
  (let* ((modes pilish--tree-filter-modes)
         (current pilish--tree-browser-filter)
         (next (or (cadr (member current modes)) (car modes))))
    (pilish-tree-browser-set-filter next)))

(defun pilish-tree-browser-search ()
  "Set or clear the semantic-text search in the tree browser.
Whitespace-separated regexp tokens must all match one projected node."
  (interactive)
  (let ((query (read-string "Filter (regexp tokens): "
                            pilish--tree-browser-search-query))
        (need-rerender t))
    (if (string-empty-p query)
        (setq pilish--tree-browser-search-query nil
              pilish--tree-browser-search-tokens nil)
      (condition-case err
          (let ((tokens (split-string query)))
            (dolist (tok tokens)
              (string-match-p tok ""))
            (setq pilish--tree-browser-search-query query
                  pilish--tree-browser-search-tokens tokens))
        (invalid-regexp
         (message "Pi: Invalid regexp: %s" (error-message-string err))
         (setq need-rerender nil))))
    (when need-rerender
      (pilish--tree-browser-rerender))))

(defun pilish-tree-browser-navigate ()
  "Continue the live conversation from the selected turn.
Selecting the actual current projected entry is a no-op.  Projected
legacy and ambiguous rows are visible but cannot truthfully name a
continuation target."
  (interactive)
  (let* ((section (pilish--browse-visible-section-at-point))
         (value (and section (oref section value))))
    (cond
     ((or (null section)
          (not (eq (oref section type) 'tree-node)))
      (message "Pi: No tree node at point"))
     ((eq (car-safe value) 'ambiguous-id)
      (message
       "Pi: Cannot continue from ambiguous duplicate entry id: %s"
       (cdr value)))
     ((not (stringp value))
      (message
       (concat
        "Pi: Cannot continue from selected turn: legacy entry has no id; "
        "open it with pi once to migrate, then refresh with g")))
     (t
      (pilish--browse-navigate value)))))

(defun pilish-tree-browser-set-label ()
  "Set or clear a label on the visible addressable tree node at point."
  (interactive)
  (when-let* ((section (pilish--browse-visible-section-at-point))
              (node-id (oref section value))
              ((stringp node-id)))
    (let* ((current-label (when pilish--tree-browser-tree
                            (pilish--tree-find-label
                             pilish--tree-browser-tree node-id)))
           (new-label (read-string
                       (if current-label
                           (format "Label (current: %s, empty to clear): "
                                   current-label)
                         "Label: ")
                       current-label))
           (label (if (string-empty-p (string-trim new-label)) nil new-label)))
      (pilish--browse-set-label node-id label))))

(defun pilish--tree-find-label (tree node-id)
  "Find the label for NODE-ID in TREE.
Returns the label string or nil."
  (let ((stack (append tree nil))
        (result nil))
    (while (and stack (not result))
      (let* ((node (pop stack))
             (children (plist-get node :children)))
        (when (equal (plist-get node :id) node-id)
          (setq result (plist-get node :label)))
        (when (vectorp children)
          (dotimes (i (length children))
            (push (aref children i) stack)))))
    result))

(defun pilish--tree-find-node (tree node-id)
  "Find the projected node with :id NODE-ID in TREE; nil when absent.
Iterative pre-order walk; used by
`pilish--browse-navigate-message' for the success preview."
  (let ((stack (append tree nil))
        (found nil))
    (while (and stack (not found))
      (let ((node (pop stack)))
        (if (equal (plist-get node :id) node-id)
            (setq found node)
          (setq stack (append (append (plist-get node :children) nil)
                              stack)))))
    found))

(defun pilish--tree-node-with-label (node label)
  "Return a copy of projected NODE carrying :label LABEL, or no label.
LABEL nil removes the label pair entirely — the load-time fold treats
an absent label as cleared, and projection only emits the pair when a
label is set, so a patched node stays shape-identical to a fresh read.
Only NODE's own plist is copied; the :children vector is shared as-is,
keeping the copy O(1) in tree size — rebuilding the spine above NODE
is `pilish--tree-apply-label's business.  A newly set :label
pair goes right after :timestamp, the canonical projected position."
  (let ((out nil)
        (replaced nil))
    (while (consp node)
      (let ((key (pop node)))
        (if (eq key :label)
            ;; Drop the old pair wherever it sits.
            (when (consp node) (pop node))
          (setq out (nconc out
                           (list key (if (consp node) (pop node) nil))))
          (when (and (eq key :timestamp) label)
            (setq out (nconc out (list :label label))
                  replaced t)))))
    (if (or replaced (not label))
        out
      ;; No :timestamp anchor (not a projected node): append at the end.
      (nconc out (list :label label)))))

(defun pilish--tree-apply-label (tree node-id label)
  "Patch TREE so the projected node with NODE-ID carries LABEL, or none.
Return a fresh tree vector: the root-to-node spine is rebuilt with
fresh plists and child vectors, with the patched node
\\(`pilish--tree-node-with-label'\\) `aset' into each container,
while every unvisited subtree is shared — the patch costs O(path
length), not O(tree size).  Return nil when NODE-ID is not in TREE.
Both the search and the rebuild are iterative, so deep chains cannot
overflow the Lisp stack."
  (when (vectorp tree)
    (let* ((root-frame (cons tree 0))
           (stack (list root-frame))
           ;; PATH holds the reversed (CONTAINER . INDEX) frames of the
           ;; current DFS chain; once the target is found it is the
           ;; deepest-first root-to-node path.
           (path nil)
           (found nil))
      (catch 'exited
        (while stack
          (let* ((frame (car stack))
                 (vec (car frame))
                 (idx (cdr frame)))
            (if (>= idx (length vec))
                (progn
                  (pop stack)
                  ;; This frame's owner node is done; leave its path
                  ;; entry too (the root frame has no owner above it).
                  (unless (eq frame root-frame) (pop path)))
              (setcdr frame (1+ idx))
              (let* ((node (aref vec idx))
                     (children (plist-get node :children))
                     (descend (and (vectorp children)
                                   (> (length children) 0))))
                (push (cons vec idx) path)
                (when (equal (plist-get node :id) node-id)
                  (setq found t)
                  (throw 'exited t))
                (if descend
                    (push (cons children 0) stack)
                  (pop path))))))
        nil)
      (when found
        (let* ((target-frame (car path))
               (target (aref (car target-frame) (cdr target-frame)))
               (patched (pilish--tree-node-with-label target label))
               (result nil)
               (frames path))
          (while frames
            (let* ((frame (car frames))
                   (vec (copy-sequence (car frame))))
              (aset vec (cdr frame) patched)
              (if (cdr frames)
                  ;; The node at the next frame up owns VEC as its
                  ;; :children: hand it a fresh plist pointing there.
                  (let* ((owner-frame (cadr frames))
                         (owner (aref (car owner-frame)
                                      (cdr owner-frame))))
                    (setq patched (plist-put (copy-sequence owner)
                                             :children vec)))
                (setq result vec)))
            (setq frames (cdr frames)))
          result)))))

;;;; Disk-Backed Data Layer Seams

(defun pilish--browse-current-session-directory ()
  "Return the session directory for the \"current\" scope, or nil.
Resolution order: the optional menu-supplied session list directory
when menu.el is loaded, then the munged stable session directory of the
linked chat buffer — rooted on that directory's own host when remote —
then the munged project directory; the last works with no session at
all.  Signals when resolution itself fails."
  (or (and (fboundp 'pilish--session-list-directory)
           (pilish--session-list-directory))
      (when-let* ((chat-buf pilish--chat-buffer))
        (and (buffer-live-p chat-buf)
             (let ((cwd (pilish--chat-session-directory chat-buf)))
               (pilish-jsonl-session-dir-for-cwd
                cwd (pilish-jsonl-sessions-root cwd)))))
      (pilish-jsonl-session-dir-for-cwd
       (pilish--session-directory))))

(defun pilish--browse-project-directory ()
  "Return the linked chat's stable cwd, or the browser's project directory."
  (if (buffer-live-p pilish--chat-buffer)
      (pilish--route-preserving-expand-file-name
       (pilish--chat-session-directory pilish--chat-buffer))
    (pilish--session-directory)))

(defun pilish--browse-flat-session-directory-p (dir)
  "Return non-nil when DIR is a flat custom session directory.
Local default storage has one exact path for the linked chat's cwd;
a custom sessionDir may have any name.  Remote storage still uses the
munged-name heuristic because its configured sessions root is unknown."
  (if (file-remote-p dir)
      (not (string-match-p "\\`--.*--\\'"
                           (file-name-nondirectory (directory-file-name dir))))
    (not (equal (directory-file-name (expand-file-name dir))
                (directory-file-name
                 (expand-file-name
                  (pilish-jsonl-session-dir-for-cwd
                   (pilish--browse-project-directory))))))))

(defun pilish--browse-session-directories (scope &optional buf token)
  "Return the session directories for SCOPE while BUF owns TOKEN.
`current' resolves the session file's directory.  For flat custom
storage, `all' scans that same directory; otherwise it lists root-level
munged --…-- directories under the sessions root, excluding sidecars
and non-munged directories.  Missing roots read as empty; resolution
errors signal as before.  BUF and TOKEN are optional for direct callers;
when present, ownership is checked before and immediately after each
handler-dispatching directory operation."
  (let ((current-p
         (lambda ()
           (or (null token)
               (pilish--session-browser-generation-current-p buf token)))))
    (when (funcall current-p)
      (let ((cur (pilish--browse-current-session-directory)))
        (cond
         ((not (funcall current-p)))
         ((or (not (eq scope 'all))
              (and cur (pilish--browse-flat-session-directory-p cur)))
          (and (funcall current-p) (list cur)))
         ((funcall current-p)
          (let ((root (if cur
                          (pilish-jsonl-sessions-root
                           (file-name-as-directory cur))
                        (pilish-jsonl-sessions-root))))
            ;; Root construction can itself dispatch a handler; never
            ;; enter the root listing after it supersedes this token.
            (when (funcall current-p)
              (let ((candidates
                     (condition-case nil
                         (directory-files root t "\\`--")
                       (error nil)))
                    (result nil))
                (when (funcall current-p)
                  (catch 'stale
                    (dolist (dir candidates)
                      (unless (funcall current-p) (throw 'stale nil))
                      (let ((directory-p (file-directory-p dir)))
                        (unless (funcall current-p) (throw 'stale nil))
                        (when directory-p (push dir result))))
                    (nreverse result))))))))))))

(defun pilish--browse-session-files (dirs &optional buf token)
  "Return every \\.jsonl file directly inside DIRS, in listing order.
Unreadable or missing directories are skipped.  When BUF and TOKEN are
non-nil, check generation ownership immediately before and after each
`directory-files' call; cancellation during one directory prevents all
later directory listings and returns nil."
  (let ((result nil))
    (catch 'stale
      (dolist (dir dirs)
        (when (and token
                   (not (pilish--session-browser-generation-current-p
                         buf token)))
          (throw 'stale nil))
        (let ((files (condition-case nil
                         (directory-files dir t "\\.jsonl\\'")
                       (error nil))))
          (when (and token
                     (not (pilish--session-browser-generation-current-p
                           buf token)))
            (throw 'stale nil))
          (setq result (nconc result files))))
      result)))

(defun pilish--browse-session-scan-current-p (buf token)
  "Return non-nil if BUF still owns the session scan generation TOKEN."
  (pilish--session-browser-generation-current-p buf token))

(defun pilish--browse-scan-session-files
    (buf token files items callback &optional state include-file)
  "Advance FILES and accumulated ITEMS for BUF's generation TOKEN.
STATE is the optional in-progress JSONL scan for the first file.
INCLUDE-FILE, when non-nil, tests each file before its full scan.  Each
slice shares a 10 ms deadline across opens and line processing.  A
positive-delay continuation allows a command-loop turn; it is not a
latency guarantee.  Whole-file IO, individual records, joining, GC, and
final synchronous filtering/rendering can exceed the budget.

At most one file state is retained by a pending continuation.  Ownership
is checked before and immediately after every callback-capable open,
read, and enrichment operation, including INCLUDE-FILE, and before the
next file.  Cancellation observed in a running slice closes the current
state during unwind, then touches no remaining file, schedules no
continuation, and invokes no callback.  Completion, errors, and quit
likewise close resources before CALLBACK receives (ITEMS ERROR).
Hiding the browser with q does not cancel a current generation."
  (let (transferred finished failure cancelled)
    (unwind-protect
        (when (pilish--browse-session-scan-current-p buf token)
          (condition-case err
              (let ((deadline (+ (float-time) 0.010))
                    yield)
                (while (and files
                            (not yield)
                            (not cancelled)
                            (pilish--browse-session-scan-current-p buf token)
                            (< (float-time) deadline))
                  (let (result)
                    (condition-case nil
                        (progn
                          (unless state
                            (let ((include
                                   (or (null include-file)
                                       (and (pilish--browse-session-scan-current-p
                                             buf token)
                                            (funcall include-file
                                                     (car files))))))
                              (unless (pilish--browse-session-scan-current-p
                                       buf token)
                                (setq cancelled t))
                              (when (and include (not cancelled))
                                (setq state
                                      (pilish-jsonl-open-session-info
                                       (car files) t))
                                (unless (pilish--browse-session-scan-current-p
                                         buf token)
                                  (setq cancelled t)))))
                          (when (and state (not cancelled))
                            (if (pilish--browse-session-scan-current-p
                                 buf token)
                                (setq result
                                      (pilish-jsonl-step-session-info
                                       state deadline))
                              (setq cancelled t))
                            (unless (pilish--browse-session-scan-current-p
                                     buf token)
                              (setq cancelled t))))
                      ;; Ordinary file failures skip just this file.  Quit
                      ;; instead reaches the scan-level interruption handler.
                      (error (setq result '(done))))
                    ;; An error handler itself may have reentered Lisp.
                    (unless (pilish--browse-session-scan-current-p buf token)
                      (setq cancelled t))
                    (unless cancelled
                      (if (or (null state) result)
                          (let (enriched)
                            ;; Identity enrichment can dispatch local file
                            ;; handlers.  Publish it only if ownership
                            ;; survives the complete operation.
                            (when (cdr result)
                              (if (pilish--browse-session-scan-current-p
                                   buf token)
                                  (setq enriched
                                        (pilish--session-enrich-item
                                         (cdr result)))
                                (setq cancelled t))
                              (unless (pilish--browse-session-scan-current-p
                                       buf token)
                                (setq cancelled t)))
                            (unless cancelled
                              (when enriched (push enriched items))
                              ;; Close a completed state before advancing;
                              ;; stale in-progress state is closed by unwind.
                              (if (pilish--browse-session-scan-current-p
                                   buf token)
                                  (progn
                                    (pilish-jsonl-close-session-info state)
                                    (setq state nil)
                                    (if (pilish--browse-session-scan-current-p
                                         buf token)
                                        (setq files (cdr files))
                                      (setq cancelled t)))
                                (setq cancelled t))))
                        (setq yield t)))))
                (when (pilish--browse-session-scan-current-p buf token)
                  (if files
                      (progn
                        (run-at-time 0.001 nil
                                     #'pilish--browse-scan-session-files
                                     buf token files items callback state
                                     include-file)
                        (setq transferred t))
                    (setq finished t))))
            (quit (setq failure "Session scan was interrupted"))
            (error (setq failure (format "Session scan failed: %s"
                                         (error-message-string err))))))
      (unless transferred (pilish-jsonl-close-session-info state)))
    ;; Outside handlers and after resource cleanup: a signaling consumer
    ;; must not be called again as an error callback.
    (when (pilish--browse-session-scan-current-p buf token)
      (cond (failure (funcall callback nil failure))
            (finished (funcall callback (nreverse items) nil))))))

(defun pilish--browse-load-sessions (scope callback &optional generation)
  "Load session items for SCOPE, then call CALLBACK with (ITEMS ERROR).
ITEMS is a list of session plists in the browse session dialect:
\(:path :id :cwd :name? :parentSessionPath? :created :modified
:messageCount :firstMessage :searchText) — the optional search-corpus
output of `pilish-jsonl-read-session-info' — each enriched during the
scan with `:canonicalPath', `:canonicalProjectSpec', and, for forks,
`:canonicalParentSession' (see `pilish--session-enrich-item'), so
downstream renders need no archive-sized or remote canonicalization —
only the few live-process session paths canonicalize locally per
render.  ERROR is an error
string or nil.  SCOPE is `current' (one project directory or the
current project's sessions in flat custom storage) or `all' (every
munged directory under the default sessions root, or the flat custom
directory).  The scan is
chunked (see `pilish--browse-scan-session-files') and shows a loading
state throughout.  A request that remains current reports exactly
once; a superseded request stops at the next guarded directory/file
boundary and is dropped without a callback.  Scope setup failures
surface synchronously as \"Cannot list sessions: …\", or \"Session scan
was interrupted\" for quit, only while the request owns its generation.
A resolver that reentrantly starts another fetch cannot publish its
now-stale failure.  GENERATION, when supplied by the browser fetch
cycle, was claimed before its loading render so render-time reentrancy
cannot reverse request order.  Direct seam callers omit it and claim a
new generation here."
  (let* ((buf (current-buffer))
         (token
          (or generation
              (setq pilish--session-browser-fetch-token
                    (1+ pilish--session-browser-fetch-token)))))
    ;; A fetch can be superseded before it even reaches this seam when
    ;; its loading render reenters Lisp.  Do no directory IO in that case.
    (when (pilish--browse-session-scan-current-p buf token)
      (let (dirs files flat-current project-id failure)
        ;; Resolve the scope and its identity under the same ownership and
        ;; error boundary as directory IO.  Capture the identity before
        ;; scheduling slices, which may run in an unrelated buffer.
        (condition-case err
            (progn
              (setq dirs (pilish--browse-session-directories scope buf token))
              (when (and (pilish--browse-session-scan-current-p buf token)
                         (eq scope 'current) (car dirs))
                (setq flat-current
                      (pilish--browse-flat-session-directory-p (car dirs))))
              (when (and flat-current
                         (pilish--browse-session-scan-current-p buf token))
                (setq project-id
                      (car (pilish--session-canonical-project-spec
                            (list :cwd (pilish--browse-project-directory)
                                  :path (car dirs))))))
              (when (pilish--browse-session-scan-current-p buf token)
                (setq files (pilish--browse-session-files dirs buf token))))
          (quit (setq failure "Session scan was interrupted"))
          (error
           (setq failure (format "Cannot list sessions: %s"
                                 (error-message-string err)))))
        (cond
         ((not (pilish--browse-session-scan-current-p buf token)))
         (failure
          (funcall callback nil failure))
         (t
          (run-at-time
           0 nil #'pilish--browse-scan-session-files buf token files nil
           (if flat-current
               (lambda (items error)
                 (funcall callback
                          (cl-remove-if-not
                           (lambda (item)
                             (and project-id
                                  (equal project-id
                                         (car (pilish--session-project-spec
                                               item)))))
                           items)
                          error))
             callback)
           nil
           (when flat-current
             (lambda (file)
               (when-let* ((header (pilish-jsonl-read-session-header file))
                           (cwd (plist-get header :cwd)))
                 (and project-id
                      (equal project-id
                             (car (pilish--session-canonical-project-spec
                                   (list :path file :cwd cwd)))))))))))))))

(defun pilish--tree-browser-chat-session-file ()
  "Return the linked chat buffer's current session file, or nil.
A live `pilish--chat-buffer' link supplies the normalized
:session-file from its state plist (populated at startup via get_state
--apply-state-response and TRAMP-prefixed for Emacs).  The file persists
after process death, so tree fetches and labels keep working with no
live pi process.  Nil covers both a dead link and a chat whose session
file does not exist yet (it is created on the first assistant reply)."
  (when-let* ((chat-buf pilish--chat-buffer))
    (when (buffer-live-p chat-buf)
      (with-current-buffer chat-buf
        (when (plistp pilish--state)
          (pilish--normalize-string-or-null
           (plist-get pilish--state :session-file)))))))

(cl-defun pilish--browse-load-tree
    (callback &optional (path (pilish--tree-browser-chat-session-file))
              generation)
  "Load PATH's conversation tree, then call CALLBACK with (TREE LEAF-ID MESSAGE).
TREE is a vector of projected root nodes in the browse node dialect,
LEAF-ID the projected leaf entry id, and MESSAGE an error string or
nil on success — the same shape as the session side's (ITEMS ERROR)
callback.  The third MESSAGE argument lets callers render precise
error states instead of a generic \"no tree\".  On a successful
canonicalization with differing duplicate ids, CALLBACK receives a
fourth DIAGNOSTIC string; callbacks on ordinary/error paths retain the
three-argument contract.

The tree comes from the linked chat's session file on DISK —
`pilish-jsonl-project-session-file' — never an RPC get_tree.
Labels appended out-of-band fold back on every disk read, navigation
rewrites this same file, and no process is needed — the state
:session-file key persists after process death.  The tree shows
the last PERSISTED turn, so it lags an in-flight turn; refresh
manually with \\[pilish-browse-refresh].

PATH defaults to the linked chat's current session file for direct
seam callers.  The browser fetch passes the path and GENERATION it
claimed before its loading paint, so render-time reentrancy cannot
reverse request order or retarget the read away from the file that
owns that fetch's orientation and anchors.  Direct seam callers omit
GENERATION and claim a new one here, mirroring
`pilish--browse-load-sessions'.

A missing chat link reports the link error and a link with no session
file yet — or one naming a path that does not exist — reports the
not-yet error, both synchronously while the request still owns its
generation.  Otherwise the read itself is deferred through
`(run-at-time 0 ...)' so the caller's forced redisplay can paint the
loading state first (see `pilish--tree-browser-fetch-and-render':
Emacs runs due 0-timers before redisplaying, so without the forced
paint a single timer hop starves the loading render entirely); then
it runs as one blocking read (a 53 MB worst-case session reads in
~0.6 s, typical files in tens of milliseconds; chunking would
complicate the pure jsonl reader for no UI gain).

File handlers can reenter Emacs during `file-exists-p' or the disk
projection.  Ownership is therefore checked immediately after each
callback-capable boundary and again by the browser callback before
publication.  Superseded work is dropped without calling CALLBACK.
The read remains wrapped in `condition-case': a nil or garbage read
reports MESSAGE \"Session file is unreadable or not a pi session file:
PATH\" instead of signaling.  A `quit' during the blocking read reports
\"Session file read was interrupted: PATH\" — `quit' is not an `error',
so without its own handler the
callback would never run and the browser would sit on its loading
state forever."
  (let* ((buf (current-buffer))
         (browser-owned-p (not (null generation)))
         (token
          (or generation
              (setq pilish--tree-browser-fetch-token
                    (1+ pilish--tree-browser-fetch-token))))
         (linked (and pilish--chat-buffer
                      (buffer-live-p pilish--chat-buffer))))
    (cl-labels
        ((current-p ()
           (pilish--tree-browser-generation-current-p
            buf token path browser-owned-p))
         (report (tree leaf-id message &optional diagnostic)
           (when (current-p)
             (if diagnostic
                 (funcall callback tree leaf-id message diagnostic)
               (funcall callback tree leaf-id message)))))
      ;; A browser fetch may already have been superseded by a reentrant
      ;; loading render before reaching this seam.  Do no file I/O then.
      (when (current-p)
        (cond
         ((not linked)
          (report
           nil nil
           "No linked pi chat session (open the tree browser from a pi chat)"))
         ((not path)
          (report
           nil nil
           "No session file yet — it is created on the first assistant reply"))
         (t
          (let ((exists nil)
                (existence-failure nil))
            (condition-case nil
                (setq exists (file-exists-p path))
              (quit
               (setq existence-failure
                     (format "Session file read was interrupted: %s" path)))
              (error
               (setq existence-failure
                     (format
                      (concat "Session file is unreadable or not "
                              "a pi session file: %s")
                      path))))
            ;; `file-exists-p' can dispatch a yielding file handler.
            (when (current-p)
              (cond
               (existence-failure
                (report nil nil existence-failure))
               ((not exists)
                (report
                 nil nil
                 "No session file yet — it is created on the first assistant reply"))
               (t
                (run-at-time
                 0 nil
                 (lambda ()
                   (when (current-p)
                     (let ((tree nil)
                           (leaf-id nil)
                           (diagnostic nil)
                           (failure nil))
                       (condition-case nil
                           (let ((result
                                  (pilish-jsonl-project-session-file path)))
                             (if result
                                 (setq tree (plist-get result :tree)
                                       leaf-id (plist-get result :leafId)
                                       diagnostic
                                       (plist-get result :diagnostic))
                               (setq failure
                                     (format
                                      (concat
                                       "Session file is unreadable or not "
                                       "a pi session file: %s")
                                      path))))
                         (quit
                          (setq failure
                                (format
                                 "Session file read was interrupted: %s"
                                 path)))
                         (error
                          (setq failure
                                (format
                                 (concat
                                  "Session file is unreadable or not "
                                  "a pi session file: %s")
                                 path))))
                       ;; The projection can yield through a file handler.
                       ;; Validate immediately after it before callback.
                       (report tree leaf-id failure diagnostic)))))))))))))))

(defun pilish--browse-session-file-matches-p (chat-buf path)
  "Return non-nil when CHAT-BUF's current session file is PATH.
Compares canonical identities (`pilish--canonical-session-path',
anchored at CHAT-BUF so relative session files resolve like the chat
does): symlink alias spellings of one file match, and remote
spellings keep their complete TRAMP route, so a final-hop-only
spelling of a multi-hop session is a different file.  For LOCAL
spellings only, `file-equal-p' adds the same-inode fallback that
hardlinked names need; remote spellings are never queried with
`file-equal-p'."
  (and (buffer-live-p chat-buf)
       (with-current-buffer chat-buf
         (let ((current (plist-get pilish--state :session-file)))
           (and (stringp current)
                (let ((a (pilish--canonical-session-path
                          current nil default-directory))
                      (b (pilish--canonical-session-path path)))
                  (or (equal a b)
                      (and (not (pilish--remote-prefix-for-path current))
                           (not (pilish--remote-prefix-for-path path))
                           (file-equal-p current path)))))))))

(defun pilish--browse-transition-refused-p (chat-buf action)
  "Return non-nil when CHAT-BUF must refuse to start ACTION now.
One gate for both halves of the transition guard, shared by
`pilish--browse-switch-session' and
`pilish--browse-navigate': an ACTIVE session transition
refuses with \"Pi: Cannot ACTION while switching sessions\" — the
status stays idle during the transition latch, so
`pilish--session-transition-ready-p' cannot see it, and a
second concurrent switch or navigate would race the first — and
otherwise the ready guard runs with ACTION (reporting its own refusal
when it returns nil)."
  (if (pilish--session-transition-active-p chat-buf)
      (progn
        (message "Pi: Cannot %s while switching sessions" action)
        t)
    (not (pilish--session-transition-ready-p chat-buf action))))

(defun pilish--browse-switch-session (path)
  "Switch the linked chat session to session file PATH.
Guards, in order: a live linked chat buffer (else `user-error'), a live
pi process (else `user-error'), and no session transition in flight —
`pilish--browse-transition-refused-p' gates both an active
transition, which keeps the status idle (without the explicit check a
second RET would race the first switch), and the ready guard, which
reports its own refusal and returns quietly.  Delegation is
`pilish--resume-selected-session' (PROC CHAT-BUF PATH); its
synchronous `user-error's (bad cwd, duplicate open) surface in the
browser.  Afterwards `pilish--browse-quit-when-settled' waits
out the transition and dismisses the browser window only when the chat
landed on PATH; a failed switch leaves the browser open (menu already
messaged).  No extra refresh logic: \\[pilish-browse-refresh]
re-derives the \"current\" scope live and the entry point opens a fresh
browser per project directory."
  (let ((chat-buf pilish--chat-buffer))
    (unless (and chat-buf (buffer-live-p chat-buf))
      (user-error "No pi session to switch to"))
    (let ((proc (buffer-local-value 'pilish--process chat-buf)))
      (unless (pilish--session-live-process-p proc)
        (user-error "Pi process is not running"))
      (unless (pilish--browse-transition-refused-p chat-buf "switch")
        (pilish--resume-selected-session proc chat-buf path)
        (pilish--browse-quit-when-settled
         chat-buf (selected-window) path)))))

(defun pilish--browse-poll-settled (chat-buf win path deadline)
  "Polling body of `pilish--browse-quit-when-settled'.
CHAT-BUF, WIN, and PATH are the parent's arguments; give up silently
once DEADLINE (an absolute time) has passed.  A dead CHAT-BUF also ends
the poll quietly — there is nothing left to wait for, and probing a
killed buffer would signal inside the timer."
  (if (not (buffer-live-p chat-buf))
      nil
    (if (pilish--session-transition-active-p chat-buf)
        (when (time-less-p (current-time) deadline)
          (run-at-time 0.05 nil #'pilish--browse-poll-settled
                       chat-buf win path deadline))
      (when (and (pilish--browse-session-file-matches-p chat-buf path)
                 (window-live-p win)
                 (with-current-buffer (window-buffer win)
                   (derived-mode-p 'pilish-session-browser-mode
                                   'pilish-tree-browser-mode)))
        (quit-window nil win)))))

(defun pilish--browse-quit-when-settled (chat-buf win path)
  "Wait out CHAT-BUF's session transition, then dismiss the browser window.
Polls `pilish--session-transition-active-p' every 0.05 s with
a 30 s timeout.  Once settled, WIN is quit ONLY when the chat state's
:session-file matches PATH — a failed switch already messaged via the
menu, so a mismatch silently leaves the browser open — and only when
WIN still shows a pi browse buffer (session OR tree browser, via
`derived-mode-p'): a dead or repurposed window (the buffer was killed
mid-poll) is left alone so `quit-window' can never close whatever
replaced it."
  (pilish--browse-poll-settled
   chat-buf win path (time-add (current-time) 30)))

(defun pilish--browse-confirm-draft-replacement
    (input-buf &optional approved-draft)
  "Decide whether navigation may replace INPUT-BUF's current draft.
APPROVED-DRAFT is a nonempty snapshot accepted before a fresh guarded
navigation pass.  Return `(ready . SNAPSHOT)' when replacement may run
without yielding, `(revalidate . SNAPSHOT)' after an accepted prompt,
or nil on refusal.  SNAPSHOT is nil for a blank image-free draft.
When the draft changes while `y-or-n-p' is active, ask about a new
nonempty draft; becoming blank still returns `revalidate' so stale
navigation state observed before the prompt is never committed."
  (let ((draft (pilish--input-draft-nonempty-snapshot input-buf)))
    (cond
     ((null draft)
      (cons 'ready nil))
     ((equal draft approved-draft)
      (cons 'ready approved-draft))
     ((not (y-or-n-p
            "Replace the unsent draft and continue from selected turn? "))
      nil)
     (t
      (let ((replacement
             (pilish--input-draft-nonempty-snapshot input-buf)))
        (cond
         ((equal draft replacement)
          (cons 'revalidate draft))
         ((null replacement)
          (cons 'revalidate nil))
         (t
          (pilish--browse-confirm-draft-replacement input-buf))))))))

(defun pilish--browse-draft-replacement-still-safe-p (input-buf approval)
  "Return non-nil if APPROVAL still permits replacing INPUT-BUF's draft.
A now-blank draft is always safe to replace.  A nonempty draft must be
the exact text/image value previously approved."
  (let ((draft (pilish--input-draft-nonempty-snapshot input-buf)))
    (or (null draft)
        (equal draft approval))))

(defun pilish--browse-navigate (node-id)
  "Continue the live conversation from projected tree node NODE-ID.
An addressable id identical to `pilish--tree-browser-leaf-id' is the
actual current entry in the unique-id disk projection already loaded by
this browser.  Return a concise no-op message for it BEFORE consulting
any linked process,
streaming/busy/transition guard, fresh disk state, draft, prefill,
rewrite, resume, or settle seam.  Otherwise run the guarded continuation
flow in `pilish--browse-navigate-noncurrent'."
  (let* ((addressable-node-id
          (pilish--normalize-string-or-null node-id))
         (addressable-leaf-id
          (pilish--normalize-string-or-null
           pilish--tree-browser-leaf-id))
         (node (and addressable-node-id
                    (pilish--tree-find-node
                     pilish--tree-browser-tree addressable-node-id))))
    (if (and addressable-node-id
             addressable-leaf-id
             node
             (not (plist-get node :ambiguousId))
             ;; Duplicate ids have no truthful occurrence-level identity;
             ;; fail through to the authoritative disk APIs instead of
             ;; manufacturing a cached-id no-op.
             (pilish-jsonl-tree-ids-unique-p pilish--tree-browser-tree)
             (equal addressable-node-id addressable-leaf-id))
        (message "Pi: Already at current position")
      (pilish--browse-navigate-noncurrent node-id))))

(defun pilish--browse-navigation-owner-current-p (owner)
  "Return non-nil when OWNER still names this browser, chat, and file."
  (let ((browser-buf (plist-get owner :browser-buffer))
        (chat-buf (plist-get owner :chat-buffer))
        (path (plist-get owner :session-file)))
    (and (eq (current-buffer) browser-buf)
         (derived-mode-p 'pilish-tree-browser-mode)
         (eq pilish--chat-buffer chat-buf)
         (buffer-live-p chat-buf)
         (equal pilish--tree-browser-loaded-file path)
         (equal (pilish--tree-browser-chat-session-file) path))))

(defun pilish--browse-navigation-owner-changed ()
  "Report that draft confirmation outlived its navigation owner."
  (message
   (concat
    "Pi: Cannot continue from selected turn: tree changed "
    "during draft confirmation")))

(defun pilish--browse-navigate-noncurrent
    (node-id &optional approved-draft navigation-owner)
  "Continue from non-current tree node NODE-ID.
APPROVED-DRAFT and NAVIGATION-OWNER are internal state carried only
across the post-confirmation revalidation pass.  Initial callers omit
them.  Revalidation always returns to the original browser buffer and
aborts if that buffer, its linked chat, or its session file changed."
  (if (null navigation-owner)
      (pilish--browse-navigate-noncurrent-owned node-id approved-draft)
    (let ((browser-buf (plist-get navigation-owner :browser-buffer)))
      (if (not (buffer-live-p browser-buf))
          (pilish--browse-navigation-owner-changed)
        (with-current-buffer browser-buf
          (if (pilish--browse-navigation-owner-current-p navigation-owner)
              (pilish--browse-navigate-noncurrent-owned
               node-id approved-draft navigation-owner)
            (pilish--browse-navigation-owner-changed)))))))

(defun pilish--browse-navigate-noncurrent-owned
    (node-id &optional approved-draft navigation-owner)
  "Continue from non-current tree node NODE-ID under NAVIGATION-OWNER.
APPROVED-DRAFT is an ephemeral text/image snapshot accepted before a
recursive revalidation pass.  NAVIGATION-OWNER is captured before the
first prompt and normally starts nil.
Guard → rewrite → switch → reload → prefill, mirroring pi's
navigateTree without a navigate RPC.  An interactive draft answer is
bound to the browser buffer, linked chat buffer, and session file
captured before the prompt.  Revalidation aborts if any owner changed
or died instead of resolving NODE-ID in a newly current tree:

 1. a live linked chat buffer, else `user-error' \"No pi session to
    continue from selected turn\";
 2. the loaded-file guard (as `pilish--browse-set-label'):
    a fresh session-file resolution that is nil messages \"Pi: Cannot
    continue from selected turn: no session file\", one that disagrees with
    `pilish--tree-browser-loaded-file' messages \"Pi: Session
    changed since the tree was loaded — refresh with g\" (the rewrite
    would have to pick one of two files);
 3. a live pi process, else `user-error' \"Pi process is not running\";
 4. no in-flight session transition (the first half of
    `pilish--browse-transition-refused-p') —
    `--session-transition-ready-p' cannot see one (status stays idle
    during the latch), and a second RET during a switch would race it;
 5. `pilish--session-transition-ready-p' with the action
    \"continue from selected turn\" (the second half; reports its own refusal);
 6. a FRESH `pilish-jsonl-read-file' — the browser's cached
    tree can lag the file — else the unreadable message;
 7. a versioned header: version 1 files (no ids) refuse with the
    migrate hint;
 8. `pilish-jsonl-navigation-target': unknown node ids refuse with
    the refresh hint;
 9. selected identity against `pilish-jsonl-current-projected-id': the
    actual current projected entry just messages \"Pi: Already at
    current position\".  This check precedes the user-message rewind
    rule, so a current root/user prompt — including one followed by
    projected-away bookkeeping — performs no write, switch, prefill,
    settle wait, or draft replacement;
10. a nil :leaf-id on a HISTORICAL root user message refuses with the
    fork hint — the chat's fork command does that job;
11. navigation target :current-p handles the distinct historical-user
    re-edit case where its parent and the current leaf positively resolve
    to the same position (two unresolved nils never qualify): a nonempty
    text/image draft first gets a targeted replacement prompt;
    acceptance recursively re-runs every guard and disk/target lookup,
    then restores :prefill, messages success, and schedules settle, but
    performs no write or switch.  A target without :prefill only reports
    that it is already at the current position and cannot prompt because
    it replaces no draft;
12. the resume cwd pre-flight (`--session-file-cwd-or-error') runs
    BEFORE any write so its `user-error's surface before the file
    changes;
13. `pilish-jsonl-navigation-lines' resolves the rewrite bytes; an
    unreadable result stops without prompting or replacing the draft;
14. when the rewrite path would replace a nonempty text/image draft,
    its targeted confirmation runs before the file rewrite or resume;
    acceptance recursively re-runs steps 1–13, so bytes and guards
    observed before the interactive prompt are never committed;
15. the local atomic rewrite (`--browse-rewrite-session-file') — the
    closing rename is the ONLY call that touches the session file;
    pre-commit local failure messages and stops byte-identically;
16. `pilish--resume-selected-session' (PROC CHAT-BUF PATH) —
    a same-path switch is legal, so the switch rides the normal
    choreography including the transition latch and history reload;
17. the input prefill runs immediately after the resume RPC is
    scheduled (the latch blocks sending until the switch settles),
    against the input buffer captured from the chat BEFORE the RPC.
    If yielding rewrite/resume work exposed a newer nonempty draft,
    preserve it and skip the prefill;
18. \"Pi: Continued from selected turn: PREVIEW\" from the cached tree
    (`--tree-find-node', `--tree-node-preview', truncated to 60;
    \"Pi: Continued from selected turn\" without a preview), then
    `pilish--browse-quit-when-settled';
19. no auto-reopen of the browser — refresh with `g'.

On ordinary local files this guard/read/rewrite path is synchronous
apart from the optional draft confirmation; an accepted answer starts a
fresh guarded pass.  The ready guard idles the linked pi process but
cannot exclude another pi instance or external writer; a writer between
the final authoritative line read and rename can lose its change.  TRAMP
file handlers may yield, and their rename need not be atomic.  These
residual risks are accepted here rather than inventing cross-module
writer coordination."
  (let ((chat-buf pilish--chat-buffer))
    (unless (and chat-buf (buffer-live-p chat-buf))
      (user-error "No pi session to continue from selected turn"))
    ;; The input buffer is captured BEFORE the RPC: the chat may retarget
    ;; buffers during the switch (step 16).
    (let* ((input-buf (buffer-local-value 'pilish--input-buffer
                                          chat-buf))
           (path (pilish--tree-browser-chat-session-file))
           (navigation-owner
            (or navigation-owner
                (list :browser-buffer (current-buffer)
                      :chat-buffer chat-buf
                      :session-file path))))
      (cond
       ((null path)
        (message "Pi: Cannot continue from selected turn: no session file"))
       ((not (equal pilish--tree-browser-loaded-file path))
        (message "Pi: Session changed since the tree was loaded — refresh with g"))
       (t
        (let ((proc (buffer-local-value 'pilish--process chat-buf)))
          (unless (pilish--session-live-process-p proc)
            (user-error "Pi process is not running"))
          (cond
           ((pilish--browse-transition-refused-p
             chat-buf "continue from selected turn")
            nil)
           (t
            (let ((session (pilish-jsonl-read-file path)))
              (cond
               ((null session)
                (message
                 (concat
                  "Pi: Cannot continue from selected turn: session file "
                  "is unreadable or not a pi session file: %s")
                 path))
               ((not (plist-get (plist-get session :header) :version))
                (message
                 (concat
                  "Pi: Cannot continue from selected turn: session file uses "
                  "an old format; open it with pi once to migrate, then refresh with g")))
               (t
                (let ((target (pilish-jsonl-navigation-target
                               session node-id)))
                  (cond
                   ((null target)
                    (message
                     "Pi: Cannot continue from selected turn: no such tree node — refresh with g"))
                   ;; Pi checks the selected entry identity before its
                   ;; user-message rewind rule.  Do the same against the
                   ;; resolved projected leaf: trailing label/session-info/
                   ;; custom records cannot turn RET on the current user
                   ;; prompt into a historical re-edit that rewrites the
                   ;; file and replaces the draft.
                   ((equal node-id
                           (pilish-jsonl-current-projected-id session))
                    (message "Pi: Already at current position"))
                   ((null (plist-get target :leaf-id))
                    (message
                     (concat
                      "Pi: Cannot continue from selected turn: it has no parent; "
                      "fork it from the chat instead")))
                   ((plist-get target :current-p)
                    (if (not (plist-get target :prefill))
                        (message "Pi: Already at current position")
                      (let ((decision
                             (pilish--browse-confirm-draft-replacement
                              input-buf approved-draft)))
                        (pcase (car-safe decision)
                          ('revalidate
                           ;; `y-or-n-p' yields.  Re-run every live/file/target
                           ;; guard before acting on the accepted snapshot.
                           (pilish--browse-navigate-noncurrent
                            node-id (cdr decision) navigation-owner))
                          ('ready
                           (pilish--browse-prefill-input
                            input-buf (plist-get target :prefill))
                           (pilish--browse-navigate-message node-id)
                           (pilish--browse-quit-when-settled
                            chat-buf (selected-window) path))))))
                   (t
                    (condition-case err
                        (pilish--session-file-cwd-or-error path)
                      ;; Re-signal the guard's own wording before any
                      ;; write happens.
                      (user-error (signal (car err) (cdr err))))
                    (let ((lines (pilish-jsonl-navigation-lines
                                  path (plist-get target :leaf-id))))
                      (if (null lines)
                          (message
                           (concat
                            "Pi: Cannot continue from selected turn: session file "
                            "is unreadable or not a pi session file: %s")
                           path)
                        (let ((decision
                               (pilish--browse-confirm-draft-replacement
                                input-buf approved-draft)))
                          (pcase (car-safe decision)
                            ('revalidate
                             ;; Revalidate after the interactive answer; never
                             ;; commit lines computed before the prompt.
                             (pilish--browse-navigate-noncurrent
                              node-id (cdr decision) navigation-owner))
                            ('ready
                             (when (pilish--browse-rewrite-session-file
                                    path lines)
                               (pilish--resume-selected-session
                                proc chat-buf path)
                               (let ((replace-draft-p
                                      (pilish--browse-draft-replacement-still-safe-p
                                       input-buf (cdr decision))))
                                 (when replace-draft-p
                                   (pilish--browse-prefill-input
                                    input-buf (plist-get target :prefill)))
                                 (pilish--browse-navigate-message node-id)
                                 (unless replace-draft-p
                                   (message
                                    (concat
                                     "Pi: Continued from selected turn; kept newer "
                                     "input draft (prefill skipped)"))))
                               (pilish--browse-quit-when-settled
                                chat-buf (selected-window) path)))))))))))))))))))))

(defun pilish--browse-navigate-message (node-id)
  "Message successful continuation from NODE-ID using the cached tree.
The preview comes from `pilish--tree-find-node' and
`pilish--tree-node-preview' over the browser's cached tree
with no refetch, truncated to 60; without a preview use the bare
\"Pi: Continued from selected turn\"."
  (let* ((node (when (vectorp pilish--tree-browser-tree)
                 (pilish--tree-find-node
                  pilish--tree-browser-tree node-id)))
         (preview (if node (pilish--tree-node-preview node) "")))
    (if (and (stringp preview) (not (string-empty-p preview)))
        (message "Pi: Continued from selected turn: %s"
                 (pilish--truncate-string preview 60))
      (message "Pi: Continued from selected turn"))))

(defun pilish--browse-rewrite-session-file (path lines)
  "Atomically replace the session file at PATH with LINES.
LINES is the `pilish-jsonl-navigation-lines' vector; the
joined bytes gain one final LF.  The ONLY call that touches PATH is
the closing `rename-file': bytes land without coding or end-of-line
conversion in a sibling temp file (`.pi-nav-…' in PATH's directory)
that carries PATH's modes best-effort, then replace PATH.  On ordinary
local files the sibling is on the same filesystem and rename is the
atomic commit; pi holds no persistent descriptor on session files, so
its next append/read opens the replacement.  On TRAMP a handler may
degrade rename to copy+delete: replacement can be visible in pieces
and a remote failure cannot promise a byte-identical original.  This
is accepted because in-place writing is strictly worse.

The rewrite reorders complete raw lines only.  It always adds one final
LF; CR bytes returned for CRLF lines remain in place, so an all-CRLF
file with its final delimiter stays all-CRLF.  A file missing its
trailing newline gains LF (or CRLF when its final raw line ends in CR).
The ready guard only idles the linked pi process: an independent
append/change between the
fresh line read and local rename can still be lost.  `unwind-protect'
removes the temp on an error or quit before commit.  Thus on ordinary
local files pre-commit failures leave PATH byte-identical and no temp
behind; errors report \"Pi: Could not continue from selected turn: …\"
and return nil, while quits
propagate after cleanup.  Success returns non-nil."
  (let* ((contents (concat (mapconcat #'identity (append lines nil) "\n")
                           "\n"))
         (tmp (make-temp-name
               (concat (file-name-directory path) ".pi-nav-")))
         (swapped nil))
    (condition-case err
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'no-conversion))
                ;; VISIT 0: no "Wrote file" message, no lockfile — and
                ;; the target is TMP, never PATH.  LINES are unibyte raw
                ;; file lines, so no coding or EOL conversion is allowed.
                (write-region contents nil tmp nil 0))
              (ignore-errors
                (set-file-modes tmp (file-modes path)))
              (rename-file tmp path t)
              (setq swapped t))
          (unless swapped
            (ignore-errors (delete-file tmp))))
      (error
       (message "Pi: Could not continue from selected turn: %s"
                (error-message-string err))
       nil))))

(defun pilish--browse-prefill-input (input-buf text)
  "Replace INPUT-BUF's draft with TEXT; nil TEXT still erases.
Navigation confirms first when that replacement would discard nonempty
text or an attached prompt image.  This function runs immediately after
the resume RPC is scheduled — the transition latch blocks sending until
the switch settles, so the text cannot leak into the outgoing session.
Failures are non-fatal."
  (when (buffer-live-p input-buf)
    (condition-case err
        (pilish--replace-input-draft input-buf text)
      (error
       (message "Pi: Failed to prefill prompt - %s"
                (error-message-string err))))))

(defun pilish--browse-set-label (node-id label)
  "Set LABEL (string, or nil to clear) on tree node NODE-ID.
Appends a `label' entry to the linked chat's session file out-of-band
via `pilish--browse-append-session-entry' — pi's
appendLabelChange shape, with the :label key omitted entirely on a
clear (the load-time fold treats an absent or empty label as
cleared).  The append also makes the label entry the file's new raw
leaf; the next fetch's projected leaf still resolves up to the last
visible entry, so the active path does not move.  Instead of
re-reading the file, the cached projected tree is patched locally
\\(`pilish--tree-apply-label'\\) and re-rendered: section
identity is the node id, which labeling never changes, so point
survives.  Report \"Pi: Label set to LABEL\" or \"Pi: Label cleared\".

Guards: no resolvable session file messages \"Pi: Cannot label: no
session file\" (nothing is written anywhere — session files are
append-only and must never be created here); a fresh resolution that
disagrees with `pilish--tree-browser-loaded-file' (the chat
switched sessions behind the browser's back) messages \"Pi: Session
changed since the tree was loaded — refresh with g\" and appends
nothing — writing into the old file would still be harmless for pi (a
benign sibling), but the browser would then show a label its tree no
longer reflects."
  (let ((path (pilish--tree-browser-chat-session-file)))
    (cond
     ((not path)
      (message "Pi: Cannot label: no session file"))
     ((not (equal pilish--tree-browser-loaded-file path))
      (message "Pi: Session changed since the tree was loaded — refresh with g"))
     (t
      (when (pilish--browse-append-session-entry
             path "label"
             (append (list :targetId node-id)
                     (when label (list :label label)))
             "label")
        (setq pilish--tree-browser-tree
              (or (pilish--tree-apply-label
                   pilish--tree-browser-tree node-id label)
                  pilish--tree-browser-tree))
        (pilish--tree-browser-rerender)
        (if label
            (message "Pi: Label set to %s" label)
          (message "Pi: Label cleared")))))))

;;;; Tree Browser Fetch and Render

(defun pilish--tree-browser-apply-load
    (buf tree leaf-id message anchor lineage owner generation
         &optional diagnostic)
  "Publish one completed tree load in BUF while it still owns it.
TREE, LEAF-ID, MESSAGE, and optional DIAGNOSTIC are the loader result.
ANCHOR and LINEAGE belong to the pre-fetch selection.  OWNER and
GENERATION were claimed at fetch entry before the loading render.
Ownership is checked at callback entry and immediately before state
publication, matching the session-side apply pattern.

Rendering is one buffer-local transaction.  A newer apply that lands
from Magit's callback-capable visibility hook is queued instead of
nesting another section tree.  Generation checks inside the active
render abort its resumed insertion and all post-render orientation;
the unwind then applies only the newest queued owner, whose clean render
erases any partial obsolete text."
  (when (pilish--tree-browser-generation-current-p
         buf generation owner t)
    (with-current-buffer buf
      (when (pilish--tree-browser-generation-current-p
             buf generation owner t)
        (if pilish--tree-browser-rendering-p
            (setq pilish--tree-browser-pending-load
                  (list buf tree leaf-id message anchor lineage
                        owner generation diagnostic))
          ;; No callback-capable boundary exists between this final check,
          ;; publication, and the rerender acquiring its transaction lock.
          (setq pilish--tree-browser-loading nil
                pilish--tree-browser-fetch-anchor nil
                pilish--tree-browser-fetch-lineage nil
                pilish--tree-browser-error message
                pilish--tree-browser-diagnostic diagnostic
                pilish--tree-browser-tree tree
                pilish--tree-browser-leaf-id leaf-id
                pilish--tree-browser-loaded-file
                (and (not message) owner))
          (pilish--tree-browser-rerender
           anchor lineage nil generation owner))))))

(defun pilish--tree-browser-fetch-and-render ()
  "Fetch tree and re-render the tree browser.
The tree is read from the linked chat's session file on disk, so no
live pi process is required.  The callback reports (TREE LEAF-ID
MESSAGE): a non-nil MESSAGE renders as an error state with a zero
visible count.  On success, `pilish--tree-browser-loaded-file'
records the file resolved before the deferred read, so a chat session
switch mid-read leaves the label and continuation guards comparing
against the tree actually displayed; it is nil on error states.

The point anchor AND its old selected-node lineage are captured before
the loading render and before the callback replaces the tree.  A
missing selected node can therefore resolve to its nearest surviving
OLD ancestor instead of inventing ancestry from the replacement tree.
A refresh issued while another fetch is loading reuses both in-flight
values.

`pilish--tree-browser-state-file' owns orientation and anchors.  A
fetch for another file resets them and suppresses capture from the old
sections during the loading render; shared ids across session files
therefore cannot override fresh active-leaf orientation.  A same-file
refresh retains the ordinary point-preservation behavior.

The loading state is painted EXPLICITLY: the deferred read is a
single `(run-at-time 0 ...)' hop, and Emacs runs due 0-timers before
redisplaying, so without the forced `redisplay' here the loading
render would never become visible (the chunked session scan yields
to redisplay between its slices; one timer hop never does)."
  (let* ((buf (current-buffer))
         ;; Claim generation before resolving/painting anything: either
         ;; step can reenter Lisp, and a newer fetch must stay newer when
         ;; this invocation resumes (the session fetch has the same rule).
         (token (setq pilish--tree-browser-fetch-token
                      (1+ pilish--tree-browser-fetch-token)))
         (loaded (pilish--tree-browser-chat-session-file))
         (new-owner-p
          (not (equal loaded pilish--tree-browser-state-file)))
         (captured (and (not new-owner-p)
                        (pilish--browse-capture-point-anchor)))
         (anchor
          (and (not new-owner-p)
               (or captured
                   ;; Mid-flight refresh: the loading render already
                   ;; destroyed the sections under point.
                   (and pilish--tree-browser-loading
                        pilish--tree-browser-fetch-anchor)
                   pilish--tree-browser-point-anchor)))
         (lineage
          (and (not new-owner-p)
               (cond
                (captured
                 (pilish--tree-anchor-lineage
                  captured pilish--tree-browser-tree))
                (pilish--tree-browser-loading
                 pilish--tree-browser-fetch-lineage)
                (t pilish--tree-browser-point-lineage)))))
    (when new-owner-p
      ;; Node ids are canonical only within one session file.  A reused
      ;; browser must not transfer folds to coincidentally equal ids owned
      ;; by another file.
      (setq pilish--browse-fold-state (make-hash-table :test #'equal)
            pilish--tree-browser-point-oriented-p nil
            pilish--tree-browser-point-anchor nil
            pilish--tree-browser-point-lineage nil
            pilish--tree-browser-fetch-anchor nil
            pilish--tree-browser-fetch-lineage nil))
    (setq pilish--tree-browser-state-file loaded
          pilish--tree-browser-loading t
          pilish--tree-browser-fetch-anchor anchor
          pilish--tree-browser-fetch-lineage lineage)
    ;; On a file switch, the old sections are still in the buffer here;
    ;; explicitly suppress their capture while painting loading state.
    (pilish--tree-browser-rerender nil nil new-owner-p)
    ;; Paint it before scheduling the read — see docstring.
    (redisplay)
    (pilish--browse-load-tree
     (lambda (tree leaf-id message &optional diagnostic)
       ;; The timer may call back in any current buffer.  The apply helper
       ;; returns to BUF and validates generation + file ownership twice.
       (pilish--tree-browser-apply-load
        buf tree leaf-id message anchor lineage loaded token diagnostic))
     loaded token)))

(defun pilish--tree-browser-rerender
    (&optional fallback fallback-lineage ignore-current-anchor
               generation owner)
  "Re-render the tree browser from local state, preserving point.
FALLBACK is a pre-fetch `(IDENT . OFFSET)' anchor and
FALLBACK-LINEAGE is its ancestry captured from the old tree.  A fresh
tree snapshot instead orients to its active projected leaf (or nearest
visible ancestor).  Subsequent renders preserve the selected section;
if it disappears, the resolver tries the captured old lineage before
the new active path and deterministic first-row fallback.

When IGNORE-CURRENT-ANCHOR is non-nil, neither this function nor the
generic renderer captures the section currently under point.  A new
session-file owner uses that mode for its loading render.  GENERATION
and OWNER, when supplied by a completed load, fence actual Magit
insertion and every post-render orientation/point write.  Every other
render snapshots the current generation and owner, so loading,
filtering, and search renders have the same protection.

Only one render transaction runs per buffer.  Reentrant rerenders and
completed loads retain only their newest request.  On unwind, a current
completed load wins; otherwise the newest ordinary rerender repaints.
An obsolete transaction throws to its local boundary first, ensuring a
successor always starts from a clean erase instead of a mixed section
tree."
  (if pilish--tree-browser-rendering-p
      (setq pilish--tree-browser-pending-rerender
            (list fallback fallback-lineage ignore-current-anchor
                  generation owner))
    (setq pilish--tree-browser-rendering-p t)
    (unwind-protect
        (let ((pilish--tree-browser-render-generation
               (or generation pilish--tree-browser-fetch-token))
              (pilish--tree-browser-render-owner
               (if generation owner pilish--tree-browser-state-file)))
          (pilish--tree-browser-rerender-transaction
           fallback fallback-lineage ignore-current-anchor))
      (setq pilish--tree-browser-rendering-p nil)
      (let ((pending-load pilish--tree-browser-pending-load)
            (pending-rerender pilish--tree-browser-pending-rerender))
        (setq pilish--tree-browser-pending-load nil
              pilish--tree-browser-pending-rerender nil)
        (if (and pending-load
                 (pilish--tree-browser-generation-current-p
                  (nth 0 pending-load) (nth 7 pending-load)
                  (nth 6 pending-load) t))
            (apply #'pilish--tree-browser-apply-load pending-load)
          (when pending-rerender
            (apply #'pilish--tree-browser-rerender pending-rerender)))))))

(defun pilish--tree-browser-rerender-transaction
    (fallback fallback-lineage ignore-current-anchor)
  "Perform one fenced tree render using FALLBACK and FALLBACK-LINEAGE.
IGNORE-CURRENT-ANCHOR has the meaning documented by
`pilish--tree-browser-rerender'.  The wrapper owns serialization and
dynamically binds the generation/owner checked at each render seam."
  (catch 'pilish--tree-browser-stale-render
    (pilish--tree-browser-ensure-render-current)
    (let* ((captured (and (not ignore-current-anchor)
                          (pilish--browse-capture-point-anchor)))
           (anchor (or captured
                       fallback
                       pilish--tree-browser-point-anchor))
           (lineage
            (cond
             (captured
              (pilish--tree-anchor-lineage
               captured pilish--tree-browser-tree))
             ;; A fetch fallback belongs to the OLD tree.  Even when its
             ;; captured lineage is nil, never reconstruct it from the
             ;; replacement snapshot.
             (fallback fallback-lineage)
             ((equal anchor pilish--tree-browser-point-anchor)
              pilish--tree-browser-point-lineage))))
      ;; Remember the user's last addressable node before a loading or
      ;; empty render destroys all node sections, together with ancestry
      ;; from the tree that still contains that node.
      (when (stringp (pilish--tree-anchor-node-id anchor))
        (setq pilish--tree-browser-point-anchor anchor
              pilish--tree-browser-point-lineage lineage))
      (let ((pilish--tree-browser-resolution-lineage lineage))
        (pilish--browse-rerender-preserving-point
         (current-buffer) #'pilish--tree-browser-render anchor
         ;; Loading/error renders have no node sections; do not walk a
         ;; potentially huge cached tree merely to rediscover that fact.
         (and (not pilish--tree-browser-loading)
              (not pilish--tree-browser-error)
              pilish--tree-browser-tree
              #'pilish--tree-browser-missing-section)
         ignore-current-anchor))
      (pilish--tree-browser-ensure-render-current)
      ;; Loading/error placeholders are not a first tree snapshot.  Once
      ;; a real snapshot has rendered, ordinary refreshes must never
      ;; reapply initial active-leaf orientation over a user's selection.
      (unless (or pilish--tree-browser-loading
                  pilish--tree-browser-error
                  (null pilish--tree-browser-tree))
        (setq pilish--tree-browser-point-oriented-p t))
      (pilish--tree-browser-ensure-render-current)
      ;; Record the section actually chosen by exact identity, old
      ;; ancestor, active path, or fallback.  With no addressable rows,
      ;; retain the old anchor so clearing a search can recover it.
      (when-let* ((selected (pilish--browse-capture-point-anchor))
                  ((stringp (pilish--tree-anchor-node-id selected))))
        (pilish--tree-browser-ensure-render-current)
        (setq pilish--tree-browser-point-anchor selected
              pilish--tree-browser-point-lineage
              (pilish--tree-anchor-lineage
               selected pilish--tree-browser-tree))))))

;;;; Tree Browser Refresh Integration

(defun pilish-browse-refresh ()
  "Refresh the current browse buffer from disk."
  (interactive)
  (cond
   ((derived-mode-p 'pilish-session-browser-mode)
    (pilish--session-browser-fetch-and-render))
   ((derived-mode-p 'pilish-tree-browser-mode)
    (pilish--tree-browser-fetch-and-render))
   (t (message "Pi: Not in a browse buffer"))))

;;;; Entry Points

;;;###autoload
(defun pilish-session-browser ()
  "Open the session browser for the current project.
A new browser starts from `pilish-session-browser-default-scope',
`pilish-session-browser-default-view', and
`pilish-session-browser-default-named-only'; an existing (even
hidden) browser buffer is reused with its current state.  Re-running
the browser major mode re-initializes from the current defaults."
  (interactive)
  (let* ((dir (pilish--session-directory))
         (new-p (not (get-buffer
                      (pilish--session-browser-buffer-name dir))))
         (buf (pilish--get-or-create-session-browser dir)))
    ;; Link to the chat session.  Only the chat buffer is cached here;
    ;; `pilish--get-process' resolves the process live.
    (when-let* ((chat-buf (pilish--get-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer buf
          (setq pilish--chat-buffer chat-buf))))
    (pop-to-buffer buf)
    (pilish--browse-apply-margins)
    (pilish--session-browser-fetch-and-render)
    (when new-p
      (message "Pi: Press ? for available commands"))))

;;;###autoload
(defun pilish-tree-browser ()
  "Open the tree browser for the current session.
Guard first: the tree browser reads the linked chat's session file
from disk, so without a live chat session there is nothing to browse
— signal `user-error' \"No pi session to browse\" BEFORE creating any
browser buffer (a buffer with no link would only render the link
error forever).  A new browser starts from
`pilish-tree-browser-default-filter'; an existing (even hidden)
browser buffer is reused with its current state.  Re-running the
browser major mode re-initializes from the current defaults."
  (interactive)
  (let ((chat-buf (pilish--get-chat-buffer)))
    (unless (and chat-buf (buffer-live-p chat-buf))
      (user-error "No pi session to browse")))
  (let* ((dir (pilish--session-directory))
         (new-p (not (get-buffer
                      (pilish--tree-browser-buffer-name dir))))
         (buf (pilish--get-or-create-tree-browser dir)))
    ;; Link to the chat session.  Only the chat buffer is cached here;
    ;; the session file is resolved live from its state on every fetch.
    (when-let* ((chat-buf (pilish--get-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer buf
          (setq pilish--chat-buffer chat-buf))))
    (pop-to-buffer buf)
    (pilish--browse-apply-margins)
    (pilish--tree-browser-fetch-and-render)
    (when new-p
      (message "Pi: Press ? for available commands"))))

(provide 'pilish-browse)
;;; pilish-browse.el ends here
