;;; pilish-jsonl-test.el --- Tests for JSONL session module -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for pilish-jsonl.el: JSONL session reading, raw
;; session-tree building, RPC-style display projection, and tool-call
;; preview formatting.
;;
;; Three golden fixtures under test/fixtures/ form the oracle and are
;; mutually consistent by construction (same session, same ids):
;;
;;   browse-session.jsonl   - realistic session file (header + 35 entries)
;;                            covering the full matrix: fork with a shared
;;                            toolCallId in both branches, label
;;                            set/clear/re-set, session_info x2, custom
;;                            entries (one with promoted children),
;;                            custom_message x2 (string and block content,
;;                            one with display false), aborted/no-content/
;;                            errorMessage assistants, compaction,
;;                            branch_summary, model_change,
;;                            thinking_level_change, bashExecution, user
;;                            string and block content, assistant with
;;                            read+bash+default-json tool calls plus an
;;                            unresolvable toolCallId, sibling timestamps
;;                            deliberately out of file order, and a final
;;                            label line as the raw leaf.
;;   browse-raw.json        - expected build-tree output (raw nested
;;                            :entry/:children/:label nodes, exactly the
;;                            shape of pi's get_tree before projection)
;;   browse-projected.json  - expected project-tree output (filtered nodes
;;                            gone, children promoted, hand-computed
;;                            previews)
;;
;; Golden comparisons canonicalize both sides through a JSON round-trip
;; (json-encode, then json-parse-string).  That makes the comparison
;; tolerant of nil-versus-:null and list-versus-vector representation on
;; the implementation side, but it still pins key order and key presence.
;;
;; Internal helper signatures pinned by the unit tables below; the green
;; implementation must provide them under the `pilish--jsonl-'
;; prefix:
;;
;;   (pilish--jsonl-extract-text CONTENT &optional MAX-LEN)
;;   (pilish--jsonl-normalize-preview TEXT)
;;   (pilish--jsonl-shorten-path PATH)
;;   (pilish--jsonl-arg-number ARGS KEY)

;;; Code:

(require 'ert)
(require 'json)
(require 'pilish-jsonl)
(require 'pilish-test-common)

;;;; Helpers

(defconst pilish-test--jsonl-session-file "browse-session.jsonl"
  "Golden session fixture for the jsonl module.")

(defun pilish-test--jsonl-session-path ()
  "Return the absolute path of the golden session fixture."
  (expand-file-name pilish-test--jsonl-session-file
                    pilish-test--fixture-dir))

(defconst pilish-test--jsonl-v087-session-file "browse-session-v087.jsonl"
  "Golden pi 0.87-shaped session fixture for the jsonl module.

A linear session written by pi 0.86/0.87 (still version 3): a leading
system-role message with sections/toolsAdded, an assistant tool
call round-trip whose toolResult carries `content' (0.85 files use
`output'), and the new `usage' and `context_edit' record kinds around
a self-referential (retain-none) compaction with a systemMessage blob.
Unlike the 0.85 golden above it has no derived projection goldens;
the tests below pin its projection directly.")

(defun pilish-test--jsonl-v087-session-path ()
  "Return the absolute path of the pi 0.87-shaped session fixture."
  (expand-file-name pilish-test--jsonl-v087-session-file
                    pilish-test--fixture-dir))

(defun pilish-test--write-jsonl (path lines)
  "Write LINES (entry plists, header first) to PATH, one JSON per line.
Nil plist values encode as JSON null, as in real session files.  The
keywords :null, :true, and :false (how tests spell JSON literals)
encode as their JSON counterparts."
  (with-temp-file path
    (dolist (line lines)
      (insert (json-encode (pilish-test--jsonl-literalize line))
              "\n"))))

(defun pilish-test--jsonl-normalize (data)
  "Normalize plist tree DATA recursively for JSON encoding.
`json-encode' stringifies the keywords :null, :true, and :false when
they appear as object values, so map them to their encodable
counterparts first."
  (cond
   ((eq data :null) nil)
   ((eq data :true) t)
   ((eq data :false) 'false)
   ((vectorp data)
    (vconcat (mapcar #'pilish-test--jsonl-normalize data)))
   ((consp data)
    (let (out)
      (while (consp data)
        (let ((key (pop data)))
          (push key out)
          (push (if (consp data)
                    (pilish-test--jsonl-normalize (pop data))
                  nil)
                out)))
      (nreverse out)))
   (t data)))

(defun pilish-test--jsonl-literalize (data)
  "Recursively map JSON-literal keywords in plist tree DATA to the
values `json-encode' serializes as null, true, and false."
  (cond
   ((eq data :null) nil)
   ((eq data :true) t)
   ((eq data :false) :json-false)
   ((vectorp data)
    (vconcat (mapcar #'pilish-test--jsonl-literalize data)))
   ((consp data)
    (let (out)
      (while (consp data)
        (let ((key (pop data)))
          (push key out)
          (push (if (consp data)
                    (pilish-test--jsonl-literalize (pop data))
                  nil)
                out)))
      (nreverse out)))
   (t data)))

(defun pilish-test--jsonl-canonicalize (data)
  "Canonicalize plist tree DATA through a JSON round-trip."
  (json-parse-string
   (json-encode (pilish-test--jsonl-normalize data))
   :object-type 'plist))

(defun pilish-test--jsonl-fixture-canonical (filename)
  "Read JSON fixture FILENAME and canonicalize it like actual results."
  (pilish-test--jsonl-canonicalize
   (pilish-test--read-json-fixture filename)))

(defun pilish-test--jsonl-build-lines (lines)
  "Write LINES (header first) to a temp file and build the raw tree.
Returns the (:tree ... :leafId ...) plist from `build-tree'."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-build"))
         (path (expand-file-name "session.jsonl" dir))
         (session (progn (pilish-test--write-jsonl path lines)
                         (pilish-jsonl-read-file path))))
    (pilish-jsonl-build-tree (plist-get session :entries))))

(defun pilish-test--jsonl-project-lines (lines)
  "Write LINES (header first) to a temp file, then read, build, project.
Returns the (:tree ... :leafId ...) plist from `project-tree'."
  (pcase-let ((`(,raw-tree ,raw-leaf)
               (pcase (pilish-test--jsonl-build-lines lines)
                 ((map (:tree rt) (:leafId rl)) (list rt rl)))))
    (pilish-jsonl-project-tree raw-tree raw-leaf)))

(defun pilish-test--jsonl-find (tree id)
  "Find the projected node with :id ID in TREE; nil when absent.
Traversal is iterative."
  (let ((stack (append tree nil))
        (found nil))
    (while (and stack (not found))
      (let ((node (pop stack)))
        (if (equal (plist-get node :id) id)
            (setq found node)
          (setq stack (append (append (plist-get node :children) nil)
                              stack)))))
    found))

(defun pilish-test--jsonl-find-raw (tree id)
  "Find the raw node whose :entry has :id ID in TREE; nil when absent."
  (let ((stack (append tree nil))
        (found nil))
    (while (and stack (not found))
      (let ((node (pop stack)))
        (if (equal (plist-get (plist-get node :entry) :id) id)
            (setq found node)
          (setq stack (append (append (plist-get node :children) nil)
                              stack)))))
    found))

(defconst pilish-test--jsonl-header
  '(:type "session" :version 3 :id "5eed0000-0000-4000-8000-000000000000"
    :timestamp "2026-03-02T10:00:00.000Z" :cwd "/tmp/pi-jsonl-test")
  "Reusable session header for small in-test sessions.")

(defun pilish-test--jsonl-entry (type id parent second &rest payload)
  "Return an entry plist of TYPE, ID, and PARENT (string or nil).
SECOND is the seconds component of the 2026-03-02T10:00:00Z timestamp;
PAYLOAD is the plist tail (:message, :targetId, ...)."
  (append (list :type type :id id :parentId parent
                :timestamp (format "2026-03-02T10:%02d:%02d.000Z"
                                   (/ second 60) (% second 60)))
          payload))

(defun pilish-test--jsonl-msg (id parent second message)
  "Return a message entry whose message plist is MESSAGE."
  (pilish-test--jsonl-entry "message" id parent second
                                     :message message))

(defun pilish-test--jsonl-asst (id parent second &rest props)
  "Return an assistant message entry with message plist PROPS."
  (pilish-test--jsonl-msg
   id parent second (append '(:role "assistant") props)))

;;;; Golden round-trip tests

(ert-deftest pilish-test-jsonl-build-tree-golden ()
  "build-tree over the golden session matches browse-raw.json."
  (let* ((session (pilish-jsonl-read-file
                   (pilish-test--jsonl-session-path)))
         (built (pilish-jsonl-build-tree
                 (plist-get session :entries))))
    (should (equal (pilish-test--jsonl-canonicalize built)
                   (pilish-test--jsonl-fixture-canonical
                    "browse-raw.json")))))

(ert-deftest pilish-test-jsonl-project-tree-golden ()
  "project-tree over browse-raw.json matches browse-projected.json."
  (let* ((raw (pilish-test--read-json-fixture "browse-raw.json"))
         (result (pilish-jsonl-project-tree
                  (plist-get raw :tree) (plist-get raw :leafId))))
    (should (equal (pilish-test--jsonl-canonicalize result)
                   (pilish-test--jsonl-fixture-canonical
                    "browse-projected.json")))))

(ert-deftest pilish-test-jsonl-round-trip-golden ()
  "read-file, build-tree, and project-tree chained match the projection."
  (let* ((session (pilish-jsonl-read-file
                   (pilish-test--jsonl-session-path)))
         (built (pilish-jsonl-build-tree
                 (plist-get session :entries)))
         (result (pilish-jsonl-project-tree
                  (plist-get built :tree) (plist-get built :leafId))))
    (should (equal (pilish-test--jsonl-canonicalize result)
                   (pilish-test--jsonl-fixture-canonical
                    "browse-projected.json")))))

(ert-deftest pilish-test-jsonl-project-session-file-golden ()
  "project-session-file is the read→build→project composition over PATH.
  Over the golden session file it returns exactly the chained golden
  (browse-projected.json); a missing or headerless file reads as nil —
  there is no tree to render, not an error."
  (let* ((path (pilish-test--jsonl-session-path))
         (direct (pilish-jsonl-project-session-file path)))
    (should direct)
    (should (equal (pilish-test--jsonl-canonicalize direct)
                   (pilish-test--jsonl-fixture-canonical
                    "browse-projected.json"))))
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-psf"))
         (headerless (expand-file-name "headerless.jsonl" dir)))
    (pilish-test--write-jsonl
     headerless
     (list (pilish-test--jsonl-msg
            "h1" nil 0 '(:role "user" :content "decoy"))))
    (should-not (pilish-jsonl-project-session-file headerless))
    (should-not (pilish-jsonl-project-session-file
                 (expand-file-name "missing.jsonl" dir)))))

;;;; pi 0.87 session shape (browse-session-v087.jsonl)

(ert-deftest pilish-test-jsonl-v087-fixture-projection ()
  "Projecting the pi 0.87 fixture tolerates the new record kinds.
`usage' and `context_edit' entries vanish from the projection with
their children promoted to the nearest visible ancestor, so the
compaction lands directly on the final assistant.  A trailing usage
leaf folds the same way: the raw leaf id is the usage entry, but the
projected leaf and `pilish-jsonl-current-projected-id' both stay on
the final user message.  The leading system-role message projects
with its fixed preview — never via the unknown-role escape hatch —
and remains a real parent in the chain.  A thinking-only assistant
keeps the empty-content sentinel preview, and the 0.87-style
toolResult (`content' rather than `output') still resolves its
toolCallId."
  (let* ((result (pilish-jsonl-project-session-file
                  (pilish-test--jsonl-v087-session-path)))
         (tree (plist-get result :tree))
         (session (pilish-jsonl-read-file
                   (pilish-test--jsonl-v087-session-path))))
    (should result)
    (should-not (plist-get result :diagnostic))
    ;; Bookkeeping kinds vanish entirely; their ids are unaddressable,
    ;; trailing usage leaf included.
    (should-not (pilish-test--jsonl-find tree "8b92c5d6"))
    (should-not (pilish-test--jsonl-find tree "9ca3d7e8"))
    (should-not (pilish-test--jsonl-find tree "c7d8e1f3"))
    ;; The system prompt projects with its fixed role and preview.
    (let ((system (pilish-test--jsonl-find tree "3c4d5e6f")))
      (should system)
      (should (equal (plist-get system :type) "message"))
      (should (equal (plist-get system :role) "system"))
      (should (equal (plist-get system :preview) "system prompt"))
      (should (equal (plist-get system :parentId) "2b3c4d5e"))
      ;; The explicit system clause, not the unknown-role escape hatch.
      (should-not (plist-member system :rawRole)))
    ;; The system node is a real parent in the chain.
    (should (equal (plist-get (pilish-test--jsonl-find tree "4d5e6f70")
                              :parentId)
                   "3c4d5e6f"))
    ;; Children of the filtered chain promote past both records.
    (let ((compaction (pilish-test--jsonl-find tree "a1b4e9f2")))
      (should compaction)
      (should (equal (plist-get compaction :parentId) "7a81b3c4")))
    ;; Thinking blocks never reach previews; the tool round-trip
    ;; resolves exactly like the 0.85 shapes.
    (should (equal (plist-get (pilish-test--jsonl-find tree "5e6f7081")
                              :preview)
                   "(no content)"))
    (should (equal (plist-get (pilish-test--jsonl-find tree "6f708192")
                              :preview)
                   "[bash: echo ok]"))
    ;; A trailing usage leaf folds onto the last projected entry.
    (should (equal (plist-get session :leafId) "c7d8e1f3"))
    (should (equal (plist-get result :leafId) "b5c6f083"))
    (should (equal (pilish-jsonl-current-projected-id session)
                   "b5c6f083"))))

(ert-deftest pilish-test-jsonl-v087-compaction-tolerance ()
  "The 0.87 compaction record projects through summary/tokensBefore only.
A self-referential firstKeptEntryId (retain-none) and the systemMessage
blob are new payload Pilish does not consume: neither breaks the
projection nor leaks into the projected node."
  (let* ((result (pilish-jsonl-project-session-file
                  (pilish-test--jsonl-v087-session-path)))
         (compaction (pilish-test--jsonl-find
                      (plist-get result :tree) "a1b4e9f2")))
    (should compaction)
    (should (equal (plist-get compaction :type) "compaction"))
    (should (equal (plist-get compaction :summary)
                   "Summary of earlier turns"))
    (should (equal (plist-get compaction :tokensBefore) 1234))
    ;; The self-reference and the system blob stay out of the dialect.
    (should-not (plist-get compaction :firstKeptEntryId))
    (should-not (plist-get compaction :systemMessage))
    ;; No error surfaced as a diagnostic either.
    (should-not (plist-get result :diagnostic))))

(ert-deftest pilish-test-jsonl-v087-read-session-info ()
  "Session metadata over the 0.87 fixture counts and resolves sanely.
:messageCount counts message lines by regex with the leading system
message included and the new record kinds ignored; :firstMessage is
the first USER text, so the empty system content cannot win."
  (let ((info (pilish-jsonl-read-session-info
               (pilish-test--jsonl-v087-session-path))))
    (should info)
    (should (equal (plist-get info :id)
                   "6d4c2f18-9a3b-4e57-8c21-2f5a9d0b7e64"))
    (should (equal (plist-get info :cwd) "/home/daniel/co/pilish"))
    (should (equal (plist-get info :created) "2026-09-25T22:28:55.488Z"))
    ;; system, user, assistant, toolResult, assistant, user.
    (should (equal (plist-get info :messageCount) 6))
    (should (equal (plist-get info :firstMessage) "hello"))))

(ert-deftest pilish-test-jsonl-v087-navigation ()
  "Navigation over the 0.87 fixture rewinds across the compaction.
The final user message targets its parentId — the compaction entry,
which under the existing self-target rule would keep its own id — and
current-projected-id resolves to the unfiltered final user message,
not the usage/context_edit records that precede it."
  (let* ((session (pilish-jsonl-read-file
                   (pilish-test--jsonl-v087-session-path))))
    (should (equal (pilish-jsonl-navigation-target session "b5c6f083")
                   (list :leaf-id "a1b4e9f2"
                         :prefill "continue from the summary"
                         :current-p nil)))
    ;; Non-user entries target themselves; the compaction is no
    ;; exception even with its self-referential firstKeptEntryId.
    (should (equal (pilish-jsonl-navigation-target session "a1b4e9f2")
                   (list :leaf-id "a1b4e9f2" :current-p nil)))
    (should (equal (pilish-jsonl-current-projected-id session)
                   "b5c6f083"))))

;;;; read-file

(ert-deftest pilish-test-jsonl-nested-summary-fields-preserved ()
  "Reading v3 summaries preserves bounded raw facts, not invented child output."
  ;; Dropping optional message fields or decoding previews as arguments loses
  ;; the persistence boundary on which live/history presentation depends.
  (let* ((path (expand-file-name "nested-tools-session.jsonl" pilish-test--fixture-dir))
         (session (pilish-jsonl-read-file path))
         (entries (plist-get session :entries))
         (messages (seq-map (lambda (entry) (plist-get entry :message)) entries))
         (results (seq-filter (lambda (message) (equal (plist-get message :role) "toolResult"))
                              messages))
         (parent (car results))
         (nested (plist-get parent :nestedCalls))
         (calls (plist-get nested :calls))
         (details (plist-get (plist-get parent :details) :calls)))
    (should (= 3 (plist-get (plist-get session :header) :version)))
    (should (equal (mapcar (lambda (message) (plist-get message :toolCallId)) results)
                   '("p" "g" "d" "old")))
    (should (equal nested
                   '(:calls [(:id "p/1" :name "read" :arguments (:path "/tmp/CHILD-READ")
                              :status "ok" :durationMs 12)
                             (:id "p/2" :name "bash" :argumentsBytes 9000
                              :status "error" :durationMs 25 :error "Command exited with code 3")
                             (:id "p/3" :name "read" :arguments (:path "CHILD-LATE")
                              :status "unfinished")]
                     :complete :false)))
    (should-not (plist-member (aref calls 1) :arguments))
    (should-not (seq-some (lambda (call) (plist-member call :parentToolCallId)) calls))
    (should (equal (plist-get (aref details 0) :args) "{\"path\":\"/tmp/CHILD-READ\"}"))
    ;; Pi's 200-character preview keeps 197 JSON characters plus "...".
    (should (equal (plist-get (aref details 1) :args)
                   (concat "{\"command\":\"printf 'CHILD-ERROR'; " (make-string 163 ?x) "...")))
    (should (equal (plist-get (aref details 2) :id) "p/models.classify/1"))
    (should (= 0.002 (plist-get (aref details 2) :cost)))
    ;; Every line is an ordinary linear v3 entry.  No nested execution entries
    ;; are persisted or needed to redisplay the parent-only results.
    (dotimes (index (length entries))
      (should (equal (plist-get (aref entries index) :parentId)
                     (if (= index 0) :null (plist-get (aref entries (1- index)) :id)))))))

(ert-deftest pilish-test-jsonl-read-file-missing ()
  "A nonexistent path reads as nil."
  (should-not (pilish-jsonl-read-file
               "/nonexistent/pi-jsonl/no-such-file.jsonl")))

(ert-deftest pilish-test-jsonl-read-file-empty-or-headerless ()
  "Empty files and files whose first nonblank line is not the session
header read as nil."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-bad"))
         (empty (expand-file-name "empty.jsonl" dir))
         (garbage (expand-file-name "garbage.jsonl" dir)))
    (with-temp-file empty)
    (with-temp-file garbage
      (insert "{\"type\":\"message\",\"id\":\"g1\"\n"
              "this is not json\n"
              "\n"))
    (should-not (pilish-jsonl-read-file empty))
    (should-not (pilish-jsonl-read-file garbage))))

(ert-deftest pilish-test-jsonl-read-file-header-only ()
  "A header-only session reads as an empty entry vector with nil leaf."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-hdr"))
         (path (expand-file-name "hdr.jsonl" dir))
         (data (progn (pilish-test--write-jsonl
                       path (list pilish-test--jsonl-header))
                      (pilish-jsonl-read-file path))))
    (should data)
    (should (equal (plist-get data :path) path))
    (should (equal (plist-get (plist-get data :header) :cwd)
                   "/tmp/pi-jsonl-test"))
    (should (equal (plist-get data :entries) []))
    (should-not (plist-get data :leafId))
    (should-not (plist-get data :name))))

(ert-deftest pilish-test-jsonl-read-file-fixture-shape ()
  "File order, leaf id, and session name for the golden session."
  (let* ((data (pilish-jsonl-read-file
                (pilish-test--jsonl-session-path)))
         (entries (plist-get data :entries)))
    (should (= (length entries) 35))
    ;; First entry in file order is the branch-B head: the fixture models
    ;; a session file after a navigation rewrite, so line order and tree
    ;; order deliberately disagree.
    (should (equal (plist-get (aref entries 0) :id) "aa000019"))
    ;; The leaf is the id of the last line, whatever its type.
    (should (equal (plist-get data :leafId) "aa000023"))
    ;; The latest session_info entry wins.
    (should (equal (plist-get data :name) "Build fixed"))))

(ert-deftest pilish-test-jsonl-read-file-skips-bad-lines ()
  "Malformed and blank lines are skipped silently."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-skip"))
         (path (expand-file-name "bad.jsonl" dir))
         (data (progn
                 (with-temp-file path
                   (insert (json-encode pilish-test--jsonl-header) "\n"
                           "{not json at all\n"
                           "\n"
                           "{\"type\":\"message\",\"id\":\"trunc\"\n"
                           (json-encode '(:type "message" :id "ok1" :parentId nil
                                          :timestamp "2026-03-02T10:00:01.000Z"
                                          :message (:role "user" :content "hi")))
                           "\n"))
                 (pilish-jsonl-read-file path))))
    (should data)
    (should (= (length (plist-get data :entries)) 1))
    (should (equal (plist-get data :leafId) "ok1"))))

;;;; The first-nonblank session-header rule

(ert-deftest pilish-test-jsonl-session-header-first-nonblank-line-rule ()
  "THE session header is the first NONBLANK line, uniformly.
Like pi's reader, which trims leading whitespace before the header
check, blank leading lines are noise: skipped by the header check,
never entries, and kept verbatim at the front of a navigation
rewrite.  A junk first nonblank line — even with a valid header
right after it — still makes the file a non-session file: read-file,
read-session-info, and navigation-lines all return nil."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-line0"))
         (blank (expand-file-name "blank-first.jsonl" dir))
         (junk (expand-file-name "junk-first.jsonl" dir))
         (header-line (json-encode pilish-test--jsonl-header))
         (u1-line (json-encode '(:type "message" :id "u1" :parentId nil
                                 :timestamp "2026-03-02T10:00:00.000Z"
                                 :message (:role "user" :content "root"))))
         (x1-line (json-encode '(:type "message" :id "x1" :parentId nil
                                 :timestamp "2026-03-02T10:00:01.000Z"
                                 :message (:role "user" :content "side"))))
         (u2-line (json-encode '(:type "message" :id "u2" :parentId "u1"
                                 :timestamp "2026-03-02T10:00:02.000Z"
                                 :message (:role "user" :content "leaf")))))
    ;; A CRLF blank line, a plain blank one, and an indented one lead.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "\r\n" "\n" "  \n"
              header-line "\n" u1-line "\n" x1-line "\n" u2-line "\n")
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) blank nil 0)))
    ;; read-file: the blanks vanish; entries and leaf read on.
    (let ((session (pilish-jsonl-read-file blank)))
      (should session)
      (should (equal (plist-get (plist-get session :header) :cwd)
                     "/tmp/pi-jsonl-test"))
      (should (= (length (plist-get session :entries)) 3))
      (should (equal (plist-get session :leafId) "u2")))
    ;; The bounded probe uses that same first-nonblank rule.
    (should (equal (plist-get (pilish-jsonl-read-session-header blank) :cwd)
                   "/tmp/pi-jsonl-test"))
    ;; read-session-info: same header rule.
    (let ((info (pilish-jsonl-read-session-info blank)))
      (should info)
      (should (= (plist-get info :messageCount) 3)))
    ;; navigation-lines: the blanks and header stay at the front
    ;; verbatim; the rewrite happens behind them (u2's chain ends the
    ;; file, x1 leads as non-chain) with the byte multiset intact.
    (let ((lines (pilish-jsonl-navigation-lines blank "u2")))
      (should lines)
      (should (= (length lines) 7))
      (should (equal (aref lines 0) "\r"))
      (should (equal (aref lines 1) ""))
      (should (equal (aref lines 2) "  "))
      (should (equal (aref lines 3) header-line))
      (should (equal (aref lines 4) x1-line))
      (should (equal (aref lines 5) u1-line))
      (should (equal (aref lines 6) u2-line))
      (should (equal (sort (append lines nil) #'string<)
                     (sort (list "\r" "" "  " header-line u1-line
                                 x1-line u2-line)
                           #'string<))))
    ;; A junk first nonblank line is fatal even with a header after it.
    (with-temp-file junk
      (insert "{not json at all\n" header-line "\n" u1-line "\n"))
    (should-not (pilish-jsonl-read-file junk))
    (should-not (pilish-jsonl-read-session-info junk))
    (should-not (pilish-jsonl-read-session-header junk))
    ;; The leaf id exists in the would-be entries, so a nil here can
    ;; only come from the header rule.
    (should-not (pilish-jsonl-navigation-lines junk "u1"))))

(ert-deftest pilish-test-jsonl-session-header-bom-tolerated ()
  "A UTF-8 BOM prefixing the header line is tolerated uniformly:
read-file and read-session-info decode it away, and navigation-lines
accepts the header while keeping that line's bytes — BOM included —
verbatim, so the joined output stays byte-identical modulo line
reordering."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-bom"))
         (path (expand-file-name "bom.jsonl" dir))
         (header-line (json-encode pilish-test--jsonl-header))
         (u1-line (json-encode '(:type "message" :id "u1" :parentId nil
                                 :timestamp "2026-03-02T10:00:00.000Z"
                                 :message (:role "user" :content "root"))))
         (u2-line (json-encode '(:type "message" :id "u2" :parentId "u1"
                                 :timestamp "2026-03-02T10:00:01.000Z"
                                 :message (:role "user" :content "leaf"))))
         (bom-header-line (string-to-unibyte
                           (concat "\xef\xbb\xbf" header-line))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert 239 187 191)
      (insert header-line "\n" u1-line "\n" u2-line "\n")
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) path nil 0)))
    ;; read-file: insert-file-contents strips the BOM, so the header
    ;; line is clean and both entries parse.
    (let ((session (pilish-jsonl-read-file path)))
      (should session)
      (should (equal (plist-get (plist-get session :header) :cwd)
                     "/tmp/pi-jsonl-test"))
      (should (= (length (plist-get session :entries)) 2)))
    ;; The bounded probe also decodes away the BOM.
    (should (equal (plist-get (pilish-jsonl-read-session-header path) :cwd)
                   "/tmp/pi-jsonl-test"))
    ;; read-session-info: same header rule.
    (should (equal (plist-get (pilish-jsonl-read-session-info path)
                              :messageCount)
                   2))
    ;; navigation-lines: header accepted, its line keeps the BOM
    ;; bytes, and the reordered multiset matches the original bytes
    ;; exactly.
    (let ((lines (pilish-jsonl-navigation-lines path "u2")))
      (should lines)
      (should (= (length lines) 3))
      (should (equal (aref lines 0) bom-header-line))
      (should (equal (aref lines 2) u2-line))
      (should (equal (sort (append lines nil) #'string<)
                     (sort (list bom-header-line u1-line u2-line)
                           #'string<))))))

;;;; build-tree: label folding, roots, sibling sorting

(ert-deftest pilish-test-jsonl-label-folding ()
  "Labels replay in file order: set, clear, re-set; latest wins."
  (let* ((built (pilish-test--jsonl-build-lines
                 (list pilish-test--jsonl-header
                       (pilish-test--jsonl-msg
                        "u1" nil 0 '(:role "user" :content "root"))
                       (pilish-test--jsonl-entry
                        "label" "l1" "u1" 1 :targetId "u1" :label "first")
                       (pilish-test--jsonl-entry
                        "label" "l2" "l1" 2 :targetId "u1")
                       (pilish-test--jsonl-entry
                        "label" "l3" "l2" 3 :targetId "u1" :label "final"))))
         (tree (plist-get built :tree))
         (root (aref tree 0)))
    (should (equal (plist-get root :label) "final"))
    (should (equal (plist-get root :labelTimestamp)
                   "2026-03-02T10:00:03.000Z"))
    ;; Label entries are ordinary raw-tree nodes chained under the target.
    (let ((chain (plist-get root :children)))
      (should (= (length chain) 1))
      (should (equal (plist-get (plist-get (aref chain 0) :entry) :id) "l1"))
      (should (equal (plist-get built :leafId) "l3")))))

(ert-deftest pilish-test-jsonl-label-clear-without-reset ()
  "A cleared label stays cleared; the :label key is omitted entirely."
  (let* ((built (pilish-test--jsonl-build-lines
                 (list pilish-test--jsonl-header
                       (pilish-test--jsonl-msg
                        "u1" nil 0 '(:role "user" :content "root"))
                       (pilish-test--jsonl-msg
                        "u2" "u1" 1 '(:role "assistant" :content "node"))
                       (pilish-test--jsonl-entry
                        "label" "l1" "u2" 2 :targetId "u2" :label "temp")
                       (pilish-test--jsonl-entry
                        "label" "l2" "l1" 3 :targetId "u2"))))
         (u2 (pilish-test--jsonl-find-raw
              (plist-get built :tree) "u2")))
    (should u2)
    (should-not (plist-member u2 :label))
    (should-not (plist-member u2 :labelTimestamp))))

(ert-deftest pilish-test-jsonl-label-empty-string-clears ()
  "An empty-string label clears like an absent one (JS truthiness)."
  (let* ((built (pilish-test--jsonl-build-lines
                 (list pilish-test--jsonl-header
                       (pilish-test--jsonl-msg
                        "u1" nil 0 '(:role "user" :content "root"))
                       (pilish-test--jsonl-entry
                        "label" "l1" "u1" 1 :targetId "u1" :label "keep")
                       (pilish-test--jsonl-entry
                        "label" "l2" "l1" 2 :targetId "u1" :label ""))))
         (u1 (pilish-test--jsonl-find-raw
              (plist-get built :tree) "u1")))
    (should u1)
    (should-not (plist-member u1 :label))
    (should-not (plist-member u1 :labelTimestamp))))

(ert-deftest pilish-test-jsonl-read-file-session-name-trim ()
  "Session names trim; a blank latest name clears to nil."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-name"))
         (padded (expand-file-name "padded.jsonl" dir))
         (cleared (expand-file-name "cleared.jsonl" dir)))
    (pilish-test--write-jsonl
     padded (list pilish-test--jsonl-header
                  (pilish-test--jsonl-entry
                   "session_info" "s1" nil 0 :name "  Build hunt  ")))
    (should (equal (plist-get (pilish-jsonl-read-file padded) :name)
                   "Build hunt"))
    (pilish-test--write-jsonl
     cleared (list pilish-test--jsonl-header
                   (pilish-test--jsonl-entry
                    "session_info" "s1" nil 0 :name "Build")
                   (pilish-test--jsonl-entry
                    "session_info" "s2" "s1" 1 :name "   ")))
    (should-not (plist-get (pilish-jsonl-read-file cleared) :name))))

(ert-deftest pilish-test-jsonl-roots-and-cycles ()
  "Null, self, and missing parents are roots; cycle nodes are dropped."
  (let* ((built (pilish-test--jsonl-build-lines
                 (list pilish-test--jsonl-header
                       (pilish-test--jsonl-msg
                        "r1" nil 0 '(:role "user" :content "root"))
                       (pilish-test--jsonl-msg
                        "c1" "r1" 1 '(:role "user" :content "child"))
                       (pilish-test--jsonl-msg
                        "s1" "s1" 2 '(:role "user" :content "self parent"))
                       (pilish-test--jsonl-msg
                        "o1" "missing" 3 '(:role "user" :content "orphan"))
                       (pilish-test--jsonl-msg
                        "x1" "x2" 4 '(:role "user" :content "cycle a"))
                       (pilish-test--jsonl-msg
                        "x2" "x1" 5 '(:role "user" :content "cycle b")))))
         (tree (plist-get built :tree)))
    (should (equal (mapcar (lambda (node)
                             (plist-get (plist-get node :entry) :id))
                           (append tree nil))
                   '("r1" "s1" "o1")))
    ;; The cycle pair is unreachable and dropped entirely.
    (should-not (pilish-test--jsonl-find-raw tree "x1"))
    (should-not (pilish-test--jsonl-find-raw tree "x2"))
    ;; leafId still mirrors the physical last line, cycles or not.
    (should (equal (plist-get built :leafId) "x2"))
    ;; Projection keeps root order: roots seed the stack in natural
    ;; order, so a multi-root tree projects in file order.
    (should (equal (mapcar (lambda (node) (plist-get node :id))
                           (append (plist-get
                                    (pilish-jsonl-project-tree tree)
                                    :tree)
                                   nil))
                   '("r1" "s1" "o1")))))

(ert-deftest pilish-test-jsonl-sibling-sort-and-stable-ties ()
  "Children sort by timestamp; ties keep file order."
  (let* ((built (pilish-test--jsonl-build-lines
                 (list pilish-test--jsonl-header
                       (pilish-test--jsonl-msg
                        "p" nil 0 '(:role "user" :content "parent"))
                       (pilish-test--jsonl-msg
                        "late" "p" 30 '(:role "user" :content "file first"))
                       (pilish-test--jsonl-msg
                        "early" "p" 10 '(:role "user" :content "ts first"))
                       (pilish-test--jsonl-msg
                        "tie1" "p" 20 '(:role "user" :content "tie a"))
                       (pilish-test--jsonl-msg
                        "tie2" "p" 20 '(:role "user" :content "tie b")))))
         (parent (aref (plist-get built :tree) 0))
         (kids (mapcar (lambda (node)
                         (plist-get (plist-get node :entry) :id))
                       (append (plist-get parent :children) nil))))
    (should (equal kids '("early" "tie1" "tie2" "late")))))

;;;; project-tree: roles, previews, leaf resolution

(ert-deftest pilish-test-jsonl-role-mapping ()
  "Known roles pass through; anything else maps to unknown with rawRole."
  (let* ((result (pilish-test--jsonl-project-lines
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "hi"))
                        (pilish-test--jsonl-msg
                         "c1" "u1" 1 '(:role "custom" :content "custom text"))
                        (pilish-test--jsonl-msg
                         "b1" "c1" 2 '(:role "branchSummary"
                                        :content "branch summary text"))
                        (pilish-test--jsonl-msg
                         "k1" "b1" 3 '(:role "compactionSummary"
                                        :content "compaction text"))
                        (pilish-test--jsonl-msg
                         "w1" "k1" 4 '(:role "weird" :content "odd text")))))
         (tree (plist-get result :tree)))
    (should (equal (plist-get (pilish-test--jsonl-find tree "u1")
                              :role)
                   "user"))
    (should (equal (plist-get (pilish-test--jsonl-find tree "c1")
                              :role)
                   "custom"))
    (should (equal (plist-get (pilish-test--jsonl-find tree "b1")
                              :role)
                   "branchSummary"))
    (should (equal (plist-get (pilish-test--jsonl-find tree "k1")
                              :role)
                   "compactionSummary"))
    (let ((weird (pilish-test--jsonl-find tree "w1")))
      (should (equal (plist-get weird :role) "unknown"))
      (should (equal (plist-get weird :rawRole) "weird"))
      (should (equal (plist-get weird :preview) "odd text")))))

(ert-deftest pilish-test-jsonl-summary-message-roles-retain-full-text ()
  "Legacy summary-message projection keeps text beyond its preview."
  (let* ((text (concat (make-string 220 ?x) " full-summary-needle"))
         (result (pilish-test--jsonl-project-lines
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "b1" nil 0
                         (list :role "branchSummary" :summary text))
                        (pilish-test--jsonl-msg
                         "c1" "b1" 1
                         (list :role "compactionSummary" :summary text)))))
         (tree (plist-get result :tree)))
    (dolist (id '("b1" "c1"))
      (let ((node (pilish-test--jsonl-find tree id)))
        (should (= (length (plist-get node :preview)) 200))
        (should (equal (plist-get node :summary) text))))))

(ert-deftest pilish-test-jsonl-compaction-projects-summary ()
  "Compaction projection retains its semantic summary for tree search."
  (let* ((result (pilish-test--jsonl-project-lines
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-entry
                         "compaction" "c1" nil 0
                         :summary "Retained compaction summary"
                         :tokensBefore 4096))))
         (node (pilish-test--jsonl-find (plist-get result :tree) "c1")))
    (should (equal (plist-get node :summary)
                   "Retained compaction summary"))
    (should (= (plist-get node :tokensBefore) 4096))))

(ert-deftest pilish-test-jsonl-malformed-payloads-degrade ()
  "Null messages, null content, and null blocks degrade, never crash.
pi parses session files without validation; old or hand-edited files
can carry them.  The reference throws here; we project empties."
  (let* ((result (pilish-test--jsonl-project-lines
                  (list pilish-test--jsonl-header
                        ;; "message": null.
                        (pilish-test--jsonl-entry
                         "message" "m1" nil 0 :message :null)
                        (pilish-test--jsonl-msg
                         "m2" "m1" 1 '(:role "user" :content :null))
                        (pilish-test--jsonl-msg
                         "m3" "m2" 2 '(:role "user"
                                        :content [:null
                                                  (:type "text"
                                                   :text "after null")]))
                        (pilish-test--jsonl-msg
                         "m4" "m3" 3 '(:role "assistant" :content :null)))))
         (tree (plist-get result :tree)))
    (let ((m1 (pilish-test--jsonl-find tree "m1")))
      (should (equal (plist-get m1 :role) "unknown"))
      (should-not (plist-get m1 :rawRole))
      (should (equal (plist-get m1 :preview) "")))
    (should (equal (plist-get (pilish-test--jsonl-find tree "m2")
                              :preview)
                   ""))
    (should (equal (plist-get (pilish-test--jsonl-find tree "m3")
                              :preview)
                   "after null"))
    (let ((m4 (pilish-test--jsonl-find tree "m4")))
      (should (equal (plist-get m4 :preview) "(no content)"))
      (should (plist-member m4 :stopReason)))))

(ert-deftest pilish-test-jsonl-assistant-preview-fallbacks ()
  "Assistant preview precedence: text, aborted, errorMessage, no content."
  (let* ((result (pilish-test--jsonl-project-lines
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-asst
                         "a1" nil 0 :content "Text wins over aborted."
                         :stopReason "aborted")
                        (pilish-test--jsonl-asst
                         "a2" "a1" 1 :content "" :stopReason "end_turn")
                        (pilish-test--jsonl-asst
                         "a3" "a2" 2 :content " \n\t ")
                        (pilish-test--jsonl-asst
                         "a4" "a3" 3 :content [] :stopReason "aborted")
                        (pilish-test--jsonl-asst
                         "a5" "a4" 4 :content [] :stopReason "error"
                         :errorMessage "boom:\nbad")
                        (pilish-test--jsonl-asst
                         "a6" "a5" 5 :content []))))
         (tree (plist-get result :tree)))
    ;; Non-empty text beats every fallback.
    (let ((a1 (pilish-test--jsonl-find tree "a1")))
      (should (equal (plist-get a1 :preview) "Text wins over aborted."))
      (should (equal (plist-get a1 :stopReason) "aborted")))
    ;; Empty-string content is falsy: the fallback chain applies.
    (let ((a2 (pilish-test--jsonl-find tree "a2")))
      (should (equal (plist-get a2 :preview) "(no content)"))
      (should (equal (plist-get a2 :stopReason) "end_turn")))
    ;; Whitespace-only content is truthy: preview is the empty string,
    ;; not a fallback.
    (should (equal (plist-get (pilish-test--jsonl-find tree "a3")
                              :preview)
                   ""))
    (should (equal (plist-get (pilish-test--jsonl-find tree "a4")
                              :preview)
                   "(aborted)"))
    ;; errorMessage previews normalized, without the 200 cap.
    (let ((a5 (pilish-test--jsonl-find tree "a5")))
      (should (equal (plist-get a5 :preview) "boom: bad"))
      (should (equal (plist-get a5 :errorMessage) "boom:\nbad"))
      (should (equal (plist-get a5 :stopReason) "error")))
    ;; stopReason is always present on assistant nodes, even when nil.
    (let ((a6 (pilish-test--jsonl-find tree "a6")))
      (should (equal (plist-get a6 :preview) "(no content)"))
      (should (plist-member a6 :stopReason))
      (should-not (plist-member a6 :errorMessage)))))

(ert-deftest pilish-test-jsonl-fork-branch-scoped-resolution ()
  "A shared toolCallId resolves per branch, beating the global map."
  (let* ((session (pilish-jsonl-read-file
                   (pilish-test--jsonl-session-path)))
         (built (pilish-jsonl-build-tree
                 (plist-get session :entries)))
         (result (pilish-jsonl-project-tree
                  (plist-get built :tree) (plist-get built :leafId)))
         (tree (plist-get result :tree))
         (ra (pilish-test--jsonl-find tree "aa00000a"))
         (rb (pilish-test--jsonl-find tree "aa00001a")))
    ;; Both branches call tc04; each toolResult resolves against the
    ;; assistant on its own branch path.
    (should (equal (plist-get ra :formattedToolCall)
                   "[read: /srv/demo/scripts/build.sh:1-40]"))
    (should (equal (plist-get ra :toolName) "read"))
    (should (equal (plist-get rb :formattedToolCall)
                   "[bash: grep -rn 'error' /var/log/demo.log | head -20]"))
    (should (equal (plist-get rb :toolName) "bash"))
    (should-not (equal (plist-get ra :formattedToolCall)
                       (plist-get rb :formattedToolCall)))))

(ert-deftest pilish-test-jsonl-branch-map-sibling-isolation ()
  "Tool-call maps stay branch-scoped: a sibling's redefinition never
leaks into another sibling's tool results, and the global map (where
the redefinition also wins) never overrides the branch one."
  (let* ((result (pilish-test--jsonl-project-lines
                  (list pilish-test--jsonl-header
                        ;; a1 calls tc09 with read /a.py.
                        (pilish-test--jsonl-msg
                         "a1" nil 0
                         '(:role "assistant"
                           :content [(:type "toolCall" :id "tc09"
                                            :name "read"
                                            :arguments (:path "/a.py"))]
                           :stopReason "tool_calls"))
                        ;; x1, a1's first child, redefines tc09 with bash.
                        (pilish-test--jsonl-msg
                         "x1" "a1" 1
                         '(:role "assistant"
                           :content [(:type "toolCall" :id "tc09"
                                            :name "bash"
                                            :arguments (:command "echo hi"))]
                           :stopReason "tool_calls"))
                        ;; x1's own result resolves the redefinition.
                        (pilish-test--jsonl-msg
                         "t2" "x1" 2 '(:role "toolResult" :toolCallId "tc09"))
                        ;; t1, a1's second child, must see a1's original,
                        ;; not x1's and not the global map's bash.
                        (pilish-test--jsonl-msg
                         "t1" "a1" 3 '(:role "toolResult" :toolCallId "tc09")))))
         (tree (plist-get result :tree)))
    (let ((t1 (pilish-test--jsonl-find tree "t1")))
      (should (equal (plist-get t1 :formattedToolCall) "[read: /a.py]"))
      (should (equal (plist-get t1 :toolName) "read")))
    (should (equal (plist-get (pilish-test--jsonl-find tree "t2")
                              :formattedToolCall)
                   "[bash: echo hi]"))))

(ert-deftest pilish-test-jsonl-projected-leaf-resolution ()
  "Projected leaf walks raw parents to the nearest visible entry."
  (let* ((raw (pilish-test--read-json-fixture "browse-raw.json"))
         (tree (plist-get raw :tree)))
    ;; nil and unknown leaf ids resolve to nil.
    (should-not (plist-get (pilish-jsonl-project-tree tree)
                           :leafId))
    (should-not (plist-get (pilish-jsonl-project-tree tree "deadbeef")
                           :leafId))
    ;; The raw leaf is a label entry: walk up the bookkeeping chain.
    (should (equal (plist-get (pilish-jsonl-project-tree
                               tree (plist-get raw :leafId))
                              :leafId)
                   "aa000018"))
    ;; A filtered session_info leaf walks up the same way.
    (should (equal (plist-get (pilish-jsonl-project-tree
                               tree "aa000022")
                              :leafId)
                   "aa000018"))))

(ert-deftest pilish-test-jsonl-preview-truncation ()
  "Previews slice content to 200 chars before normalization."
  (let* ((s199 (make-string 199 ?a))
         (s200 (make-string 200 ?a))
         (s201 (make-string 201 ?a))
         (result (pilish-test--jsonl-project-lines
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u199" nil 0 `(:role "user" :content ,s199))
                        (pilish-test--jsonl-msg
                         "u200" "u199" 1 `(:role "user" :content ,s200))
                        (pilish-test--jsonl-msg
                         "u201" "u200" 2 `(:role "user" :content ,s201))
                        (pilish-test--jsonl-msg
                         "ujoin" "u201" 3
                         `(:role "user"
                           :content [(:type "text" :text ,(make-string 150 ?x))
                                     (:type "text" :text ,(make-string 150 ?y))]))
                        (pilish-test--jsonl-msg
                         "utrim" "ujoin" 4
                         `(:role "user"
                           :content ,(concat "keep" (make-string 250 ?\s)))))))
         (tree (plist-get result :tree)))
    (should (equal (plist-get (pilish-test--jsonl-find tree "u199")
                              :preview)
                   s199))
    (should (equal (plist-get (pilish-test--jsonl-find tree "u200")
                              :preview)
                   s200))
    (should (equal (plist-get (pilish-test--jsonl-find tree "u201")
                              :preview)
                   s200))
    ;; Blocks join with a space first, then slice.
    (should (equal (plist-get (pilish-test--jsonl-find tree "ujoin")
                              :preview)
                   (concat (make-string 150 ?x) " " (make-string 49 ?y))))
    ;; Slice happens before trimming, so trailing padding disappears.
    (should (equal (plist-get (pilish-test--jsonl-find tree "utrim")
                              :preview)
                   "keep"))))

;;;; format-tool-call unit tables

(ert-deftest pilish-test-jsonl-format-tool-call-read ()
  "read previews: shortening, offset/limit arithmetic, falsy ends."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/home/tester/proj/a.py"
                            :offset 10 :limit 20))
                   "[read: ~/proj/a.py:10-29]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 7))
                   "[read: /tmp/x.py:7]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :limit 20))
                   "[read: /tmp/x.py:1-20]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py"))
                   "[read: /tmp/x.py]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:file_path "/tmp/b.py"))
                   "[read: /tmp/b.py]"))
    ;; Empty-string paths fall through to file_path.
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "" :file_path "/tmp/c.py"))
                   "[read: /tmp/c.py]"))
    ;; JSON null normalizes to absent.
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path :null :file_path "/tmp/z.py"))
                   "[read: /tmp/z.py]"))
    ;; JSON null is present-but-null: null !== undefined triggers the
    ;; suffix; offset ?? 1 and null-limit arithmetic coerce to 1 and 0.
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset :null))
                   "[read: /tmp/x.py:1]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :limit :null))
                   "[read: /tmp/x.py:1]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 10 :limit :null))
                   "[read: /tmp/x.py:10-9]"))
    (should (equal (pilish-jsonl-format-tool-call "read" '())
                   "[read: ]"))
    ;; offset 0 stays 0 (explicit zero survives the ?? 1 default).
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 0 :limit 5))
                   "[read: /tmp/x.py:0-4]"))
    ;; limit 0 with an offset: end = start - 1, ported as-is.
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 3 :limit 0))
                   "[read: /tmp/x.py:3-2]"))
    ;; limit 0 alone: end 0 is JS-falsy, so no -end suffix.
    (should (equal (pilish-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :limit 0))
                   "[read: /tmp/x.py:1]"))))

(ert-deftest pilish-test-jsonl-format-tool-call-write-edit ()
  "write and edit previews shorten their paths."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pilish-jsonl-format-tool-call
                    "write" '(:path "/home/tester/w.txt"))
                   "[write: ~/w.txt]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "edit" '(:file_path "/tmp/e.py"))
                   "[edit: /tmp/e.py]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "write" '(:path "" :file_path ""))
                   "[write: ]"))))

(ert-deftest pilish-test-jsonl-format-tool-call-bash ()
  "bash previews normalize whitespace and truncate at 50 chars."
  (let ((process-environment '("HOME=/home/tester"))
        (cmd50 (make-string 50 ?a)))
    (should (equal (pilish-jsonl-format-tool-call
                    "bash" (list :command cmd50))
                   (concat "[bash: " cmd50 "]")))
    (should (equal (pilish-jsonl-format-tool-call
                    "bash" (list :command (concat cmd50 "Z")))
                   (concat "[bash: " cmd50 "...]")))
    (should (equal (pilish-jsonl-format-tool-call
                    "bash" '(:command "  ls\t-l\nfoo  "))
                   "[bash: ls -l foo]"))
    ;; Empty or absent command falls through to the empty string.
    (should (equal (pilish-jsonl-format-tool-call
                    "bash" '(:command ""))
                   "[bash: ]"))
    (should (equal (pilish-jsonl-format-tool-call "bash" '())
                   "[bash: ]"))))

(ert-deftest pilish-test-jsonl-format-tool-call-search ()
  "grep, find, and ls previews with path defaults."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pilish-jsonl-format-tool-call
                    "grep" '(:pattern "TODO" :path "/home/tester/src"))
                   "[grep: /TODO/ in ~/src]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "grep" '(:pattern "TODO"))
                   "[grep: /TODO/ in .]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "grep" '(:path "/x"))
                   "[grep: // in /x]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "find" '(:pattern "*.el" :path "/tmp"))
                   "[find: *.el in /tmp]"))
    (should (equal (pilish-jsonl-format-tool-call "find" '())
                   "[find:  in .]"))
    (should (equal (pilish-jsonl-format-tool-call
                    "ls" '(:path "/tmp"))
                   "[ls: /tmp]"))
    (should (equal (pilish-jsonl-format-tool-call "ls" '())
                   "[ls: .]"))))

(ert-deftest pilish-test-jsonl-format-tool-call-default-json ()
  "Unknown tools fall back to compact JSON args with a 40-char cap."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pilish-jsonl-format-tool-call
                    "list_issues" '(:repo "acme/api" :state "open"))
                   "[list_issues: {\"repo\":\"acme/api\",\"state\":\"open\"}]"))
    (let* ((args40 (list :k (make-string 32 ?x)))
           (json40 (json-encode args40))
           (args41 (list :k (make-string 33 ?x)))
           (json41 (json-encode args41)))
      (should (= (length json40) 40))
      (should (= (length json41) 41))
      (should (equal (pilish-jsonl-format-tool-call "toolx" args40)
                     (concat "[toolx: " json40 "]")))
      (should (equal (pilish-jsonl-format-tool-call "toolx" args41)
                     (concat "[toolx: " (substring json41 0 40) "...]"))))
    (should (equal (pilish-jsonl-format-tool-call
                    "flag_tool" '(:n 0 :ok :true))
                   "[flag_tool: {\"n\":0,\"ok\":true}]"))))

;;;; Internal helper unit tables

(ert-deftest pilish-test-jsonl-shorten-path ()
  "HOME (then USERPROFILE) prefixes collapse to ~, blindly."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pilish--jsonl-shorten-path "/home/tester/x/el")
                   "~/x/el"))
    (should (equal (pilish--jsonl-shorten-path "/opt/etc/conf")
                   "/opt/etc/conf"))
    ;; Blind prefix replacement, ported as-is (do not "fix").
    (let ((process-environment '("HOME=/home/tes")))
      (should (equal (pilish--jsonl-shorten-path "/home/tester/x")
                     "~ter/x")))
    ;; USERPROFILE applies when HOME is unset.
    (let ((process-environment '("USERPROFILE=/up")))
      (should (equal (pilish--jsonl-shorten-path "/up/f.txt")
                     "~/f.txt"))
      (should (equal (pilish--jsonl-shorten-path "/var/log/x")
                     "/var/log/x")))
    ;; With neither variable set, nothing is shortened.
    (let ((process-environment '()))
      (should (equal (pilish--jsonl-shorten-path "/up/f.txt")
                     "/up/f.txt")))))

(ert-deftest pilish-test-jsonl-extract-text ()
  "Strings pass through; block vectors join text blocks with spaces."
  (should (equal (pilish--jsonl-extract-text "hello world" 5)
                 "hello"))
  (should (equal (pilish--jsonl-extract-text "hello") "hello"))
  ;; Slicing clamps at the end of the string.
  (should (equal (pilish--jsonl-extract-text "plain" 200) "plain"))
  (should (equal (pilish--jsonl-extract-text "abc" 0) ""))
  (should (equal (pilish--jsonl-extract-text nil) ""))
  (should (equal (pilish--jsonl-extract-text "") ""))
  (should (equal (pilish--jsonl-extract-text (vector)) ""))
  ;; Only text blocks with string text contribute; join with spaces.
  (should (equal (pilish--jsonl-extract-text
                  (vector '(:type "text" :text "a")
                          '(:type "image")
                          '(:type "text" :text "b")))
                 "a b"))
  (should (equal (pilish--jsonl-extract-text
                  (vector '(:type "text" :text 5)))
                 ""))
  ;; Joining happens before slicing.
  (should (equal (pilish--jsonl-extract-text
                  (vector '(:type "text" :text "abcdef")) 4)
                 "abcd")))

(ert-deftest pilish-test-jsonl-normalize-preview ()
  "Newlines and tabs become spaces and the result is trimmed; \\r stays."
  (should (equal (pilish--jsonl-normalize-preview "a\nb\tc") "a b c"))
  (should (equal (pilish--jsonl-normalize-preview "  x  ") "x"))
  (should (equal (pilish--jsonl-normalize-preview "a\rb") "a\rb"))
  (should (equal (pilish--jsonl-normalize-preview "\n\t x \t\n") "x")))

(ert-deftest pilish-test-jsonl-arg-number ()
  "Numeric arg lookup: zero counts as present; JSON null reads as
present-but-null; strings do not."
  (should (equal (pilish--jsonl-arg-number '(:offset 0) :offset) 0))
  (should (equal (pilish--jsonl-arg-number '(:limit 0) :limit) 0))
  (should (equal (pilish--jsonl-arg-number '(:offset 5) :offset) 5))
  (should (eq (pilish--jsonl-arg-number '(:offset :null) :offset)
              :null))
  (should-not (pilish--jsonl-arg-number '() :offset))
  (should-not (pilish--jsonl-arg-number '(:offset "5") :offset))
  (should-not (pilish--jsonl-arg-number '(:limit 5) :offset)))

(ert-deftest pilish-test-jsonl-identical-duplicate-entries-canonicalize ()
  "A historical Pi duplicate segment canonicalizes without data loss.
Old Pi fork files can contain a repeated header and byte-equivalent
entries.  Identical nonempty-id repeats denote one occurrence: root and
answer remain navigable, the answer remains current, and no ambiguity
diagnostic is emitted."
  (let* ((root (pilish-test--jsonl-msg
                "root" nil 0 '(:role "user" :content "question")))
         (answer (pilish-test--jsonl-msg
                  "answer" "root" 1
                  '(:role "assistant" :content "answer")))
         (session
          (pilish-test--jsonl-session-at
           (list pilish-test--jsonl-header
                 root
                 ;; The historical bug repeated both header and entries.
                 (copy-tree pilish-test--jsonl-header)
                 (copy-tree root)
                 answer)))
         (entries (plist-get session :entries))
         (built (pilish-jsonl-build-tree entries))
         (projected (pilish-jsonl-project-session-file
                     (plist-get session :path))))
    (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                           (append entries nil))
                   '("root" "root" "answer")))
    (should (= (length (plist-get built :tree)) 1))
    (should-not (plist-get built :diagnostic))
    (should (equal (plist-get projected :leafId) "answer"))
    (should-not (plist-get projected :diagnostic))
    (should (pilish-test--jsonl-find (plist-get projected :tree) "root"))
    (should (pilish-test--jsonl-find (plist-get projected :tree) "answer"))
    (should (pilish-jsonl-navigation-target session "root"))
    (should (pilish-jsonl-navigation-target session "answer"))))

(ert-deftest pilish-test-jsonl-duplicate-id-cycle-terminates ()
  "Differing duplicate ids diagnose ambiguity without hiding safe data.
The later `dup1' occurrence wins canonically and closes a malformed
parent cycle with `mid'; that cyclic component remains bounded/dropped,
while unrelated `safe' stays displayable and navigable.  The ambiguous
id itself is never a navigation or current-position target."
  (let* ((session
          (pilish-test--jsonl-session-at
           (list pilish-test--jsonl-header
                 (pilish-test--jsonl-msg
                  "dup1" nil 0 '(:role "user" :content "first dup"))
                 (pilish-test--jsonl-msg
                  "mid" "dup1" 1 '(:role "user" :content "middle"))
                 (pilish-test--jsonl-msg
                  "unsafe-child" "dup1" 2
                  '(:role "assistant" :content "ambiguous ancestry"))
                 (pilish-test--jsonl-msg
                  "safe" nil 3 '(:role "assistant" :content "safe turn"))
                 ;; Same id, different content/parent: genuinely ambiguous.
                 (pilish-test--jsonl-msg
                  "dup1" "mid" 4 '(:role "user" :content "later dup")))))
         (built (pilish-jsonl-build-tree (plist-get session :entries)))
         (projected (pilish-jsonl-project-session-file
                     (plist-get session :path))))
    (should (equal (plist-get built :ambiguousIds) '("dup1")))
    (should (string-match-p "dup1" (plist-get built :diagnostic)))
    (should (pilish-test--jsonl-find-raw
             (plist-get built :tree) "safe"))
    (should (equal (plist-get projected :ambiguousIds) '("dup1")))
    (should (string-match-p "dup1" (plist-get projected :diagnostic)))
    (should (pilish-test--jsonl-find (plist-get projected :tree) "safe"))
    (should-not (pilish-jsonl-current-projected-id session))
    (should-not (pilish-jsonl-navigation-target session "dup1"))
    ;; `mid' rewinds to ambiguous dup1; the assistant child would target
    ;; itself, but its rewrite chain still has to choose a dup1 occurrence.
    (should-not (pilish-jsonl-navigation-target session "mid"))
    (should-not (pilish-jsonl-navigation-target session "unsafe-child"))
    (should (pilish-jsonl-navigation-target session "safe"))
    (should-not (pilish-jsonl-navigation-lines
                 (plist-get session :path) "dup1"))
    (should-not (pilish-jsonl-navigation-lines
                 (plist-get session :path) "mid"))
    (should-not (pilish-jsonl-navigation-lines
                 (plist-get session :path) "unsafe-child"))
    (should (pilish-jsonl-navigation-lines
             (plist-get session :path) "safe"))))

(ert-deftest pilish-test-jsonl-projected-leaf-duplicate-cycle-is-bounded ()
  "Direct raw projection diagnoses a finite differing-duplicate cycle.
The nested value is finite but its ids imply dup → mid → dup, and the
visible root and leaf are distinct `dup' occurrences.  Bound hash reads
so a regression fails rather than hanging; the ambiguous leaf resolves
to nil and the diagnostic survives even when the cycle leaves no rows."
  (let* ((tree
          (vector
           (list :entry
                 '(:type "message" :id "dup" :parentId "mid"
                   :timestamp "2026-01-01T00:00:00Z"
                   :message (:role "user" :content "root dup"))
                 :children
                 (vector
                  (list :entry
                        '(:type "custom" :id "mid" :parentId "dup"
                          :timestamp "2026-01-01T00:00:01Z")
                        :children
                        (vector
                         (list :entry
                               '(:type "message" :id "dup" :parentId "mid"
                                 :timestamp "2026-01-01T00:00:02Z"
                                 :message (:role "assistant"
                                           :content "leaf dup"))
                               :children (vector))))))))
         (real-gethash (symbol-function 'gethash))
         (reads 0)
         resolved)
    (cl-letf (((symbol-function 'gethash)
               (lambda (key table &optional default)
                 (cl-incf reads)
                 (when (> reads 64)
                   (ert-fail "projected-leaf parent cycle did not terminate"))
                 (funcall real-gethash key table default))))
      (setq resolved
            (pilish--jsonl-resolve-projected-leaf-id tree "dup")))
    (should-not resolved)
    (let ((projected (pilish-jsonl-project-tree tree "dup")))
      (should (equal (plist-get projected :tree) []))
      (should-not (plist-get projected :leafId))
      (should (equal (plist-get projected :ambiguousIds) '("dup")))
      (should (string-match-p "dup" (plist-get projected :diagnostic))))))

(ert-deftest pilish-test-jsonl-project-tree-harvests-built-ambiguity ()
  "The two-argument builder-to-projector API preserves ambiguity.
`pilish-jsonl-build-tree' has already canonicalized the duplicate, so
projection cannot rediscover it by scanning entries.  It must harvest
the surviving raw node's :ambiguousId marker: the filtered ambiguous
leaf cannot resolve to its apparently-current visible parent, and the
warning remains present without an explicit third argument."
  (let* ((entries
          [(:type "message" :id "u" :timestamp "2026-01-01T00:00:00Z"
            :message (:role "user" :content "visible parent"))
           (:type "custom" :id "d" :parentId "u"
            :timestamp "2026-01-01T00:00:01Z" :data 1)
           (:type "custom" :id "d" :parentId "u"
            :timestamp "2026-01-01T00:00:02Z" :data 2)])
         (built (pilish-jsonl-build-tree entries))
         ;; Deliberately exercise the established two-argument form.
         (projected (pilish-jsonl-project-tree
                     (plist-get built :tree) (plist-get built :leafId)))
         (raw-d (pilish-test--jsonl-find-raw
                 (plist-get built :tree) "d")))
    (should (plist-get raw-d :ambiguousId))
    (should (equal (mapcar (lambda (node) (plist-get node :id))
                           (append (plist-get projected :tree) nil))
                   '("u")))
    (should-not (plist-get projected :leafId))
    (should (equal (plist-get projected :ambiguousIds) '("d")))
    (should (string-match-p "d" (plist-get projected :diagnostic)))))

(ert-deftest pilish-test-jsonl-filtered-duplicate-preserves-marker-boundary ()
  "Projection records ambiguity even when the duplicate row is filtered.
A unique visible child remains current and displayable, but
`:ambiguousParent' tells the browser not to claim the promoted canonical
parent as active.  Continuation also refuses the ambiguous raw chain."
  (let* ((session
          (pilish-test--jsonl-session-at
           (list pilish-test--jsonl-header
                 (pilish-test--jsonl-msg
                  "top" nil 0 '(:role "user" :content "top"))
                 (pilish-test--jsonl-entry
                  "custom" "dup-custom" "top" 1 :customType "first")
                 (pilish-test--jsonl-entry
                  "custom" "dup-custom" "top" 2 :customType "later")
                 (pilish-test--jsonl-msg
                  "leaf" "dup-custom" 3
                  '(:role "assistant" :content "unique leaf")))))
         (projected (pilish-jsonl-project-session-file
                     (plist-get session :path)))
         (tree (plist-get projected :tree))
         (leaf (pilish-test--jsonl-find tree "leaf")))
    (should (equal (plist-get projected :ambiguousIds) '("dup-custom")))
    (should (equal (plist-get projected :leafId) "leaf"))
    (should (plist-get leaf :ambiguousParent))
    (should-not (pilish-jsonl-navigation-target session "leaf"))))

(ert-deftest pilish-test-jsonl-duplicate-policy-preserves-nil-and-valid-ids ()
  "Fail-closed duplicate detection ignores absent legacy ids.
Repeated nil/empty ids remain visible but unaddressable beside one valid
id; only repeated nonempty string ids make a session ambiguous."
  (let* ((entries
          [(:type "message" :id "" :timestamp "2026-01-01T00:00:00Z"
            :message (:role "user" :content "legacy empty one"))
           (:type "message" :timestamp "2026-01-01T00:00:01Z"
            :message (:role "user" :content "legacy absent one"))
           (:type "message" :id "valid" :timestamp "2026-01-01T00:00:02Z"
            :message (:role "assistant" :content "addressable"))
           (:type "message" :id "" :timestamp "2026-01-01T00:00:03Z"
            :message (:role "user" :content "legacy empty two"))
           (:type "message" :timestamp "2026-01-01T00:00:04Z"
            :message (:role "user" :content "legacy absent two"))])
         (built (pilish-jsonl-build-tree entries))
         (projected (pilish-jsonl-project-tree
                     (plist-get built :tree) (plist-get built :leafId)))
         (empty-leaf (pilish-jsonl-project-tree
                      (plist-get built :tree) ""))
         (session (list :entries entries :leafId "")))
    (should (= (length (plist-get built :tree)) 5))
    (should (= (length (plist-get projected :tree)) 5))
    (should (equal (mapcar (lambda (node) (plist-get node :id))
                           (append (plist-get projected :tree) nil))
                   '(nil nil "valid" nil nil)))
    (should-not (plist-get projected :leafId))
    (should-not (plist-get empty-leaf :leafId))
    (should-not (pilish-jsonl-current-projected-id session))
    (should-not (pilish-jsonl-navigation-target session ""))))

;;;; Deep chain

(ert-deftest pilish-test-jsonl-deep-chain ()
  "A 2500-entry chain with a forked tip builds and projects iteratively."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-deep"))
         (path (expand-file-name "deep.jsonl" dir))
         (lines (list pilish-test--jsonl-header)))
    (dotimes (i 2500)
      (push (pilish-test--jsonl-msg
             (format "dc%04d" i)
             (if (zerop i) nil (format "dc%04d" (1- i)))
             i
             `(:role "user" :content ,(format "tick %d" i)))
            lines))
    ;; The fork branches off near the tip with a later timestamp and is
    ;; appended last, so it provides the leaf.
    (push (pilish-test--jsonl-msg
           "df0000" "dc2498" 2999 '(:role "user" :content "fork"))
          lines)
    (push (pilish-test--jsonl-msg
           "df0001" "df0000" 3000 '(:role "user" :content "fork tip"))
          lines)
    (pilish-test--write-jsonl path (nreverse lines))
    (let* ((session (pilish-jsonl-read-file path))
           (built (pilish-jsonl-build-tree
                   (plist-get session :entries)))
           (result (pilish-jsonl-project-tree
                    (plist-get built :tree) (plist-get built :leafId)))
           (tree (plist-get result :tree)))
      (should (equal (plist-get built :leafId) "df0001"))
      (should (equal (plist-get result :leafId) "df0001"))
      (should (equal (plist-get (aref tree 0) :id) "dc0000"))
      ;; Walk the single-child spine down to the fork point.
      (let ((node (aref tree 0))
            (hops 0))
        (while (not (equal (plist-get node :id) "dc2498"))
          (setq node (aref (plist-get node :children) 0)
                hops (1+ hops)))
        (should (= hops 2498))
        (let ((kids (plist-get node :children)))
          (should (= (length kids) 2))
          ;; Timestamp order: chain tip first, fork second.
          (should (equal (plist-get (aref kids 0) :id) "dc2499"))
          (should (equal (plist-get (aref kids 1) :id) "df0000"))
          (should (= (length (plist-get (aref kids 0) :children)) 0))
          (should (equal (plist-get (aref (plist-get (aref kids 1) :children) 0)
                                   :id)
                         "df0001"))))
      ;; Every one of the 2502 entries survives projection.
      (let ((count 0)
            (stack (append tree nil)))
        (while stack
          (let ((node (pop stack)))
            (setq count (1+ count))
            (setq stack (append (append (plist-get node :children) nil)
                                stack))))
        (should (= count 2502))))))

;;;; Session Discovery

(ert-deftest pilish-test-jsonl-session-dir-for-cwd ()
  "session-dir-for-cwd munges a cwd into pi's --…-- directory name.
Mirrors pi's getDefaultSessionDirPath: clean the directory name, strip
ONE leading / or \\, then replace every /, \\, and : with a dash, and
wrap the result in --…-- under ROOT (default: sessions-root)."
  ;; Unix path under an explicit root.
  (should (equal (expand-file-name
                  (pilish-jsonl-session-dir-for-cwd
                   "/home/daniel/co/pi" "/r/sessions"))
                 "/r/sessions/--home-daniel-co-pi--"))
  ;; Root defaults to sessions-root (PI_CODING_AGENT_DIR honored).
  (let ((process-environment '("PI_CODING_AGENT_DIR=/tmp/fake-root")))
    (should (equal (expand-file-name
                    (pilish-jsonl-session-dir-for-cwd "/a/b"))
                   "/tmp/fake-root/sessions/--a-b--")))
  ;; Windows drive letters: each :, /, and \ becomes a dash, exactly
  ;; like pi's [/\\:] replaceAll (see the deviation note in the plan:
  ;; pi itself produces --C--x--, not --C-x--).
  (should (equal (expand-file-name
                  (pilish-jsonl-session-dir-for-cwd
                   "C:\\x" "/r/sessions"))
                 "/r/sessions/--C--x--"))
  ;; A trailing slash is cleaned first.
  (should (equal (expand-file-name
                  (pilish-jsonl-session-dir-for-cwd
                   "/home/x/y/" "/r/sessions"))
                 "/r/sessions/--home-x-y--"))
  ;; A slash-only cwd munges to the empty string between the dashes.
  (should (equal (expand-file-name
                  (pilish-jsonl-session-dir-for-cwd
                   "/" "/r/sessions"))
                 "/r/sessions/----"))
  ;; Colons munge like slashes (real pi replaces all three characters).
  (should (equal (expand-file-name
                  (pilish-jsonl-session-dir-for-cwd
                   "/a:b/c" "/r/sessions"))
                 "/r/sessions/--a-b-c--")))

(ert-deftest pilish-test-jsonl-sessions-root ()
  "sessions-root expands the agent dir, always with a trailing slash.
The default is ~/.pi/agent/sessions (mirroring pi's getAgentDir);
PI_CODING_AGENT_DIR overrides it.  Remote anchors move the root onto
the remote (the parent of the anchor file's directory); local anchors
are ignored."
  ;; Default: expanded ~/.pi/agent/sessions with a trailing slash.
  (let ((process-environment '("HOME=/tmp/fake-home")))
    (should (equal (pilish-jsonl-sessions-root)
                   "/tmp/fake-home/.pi/agent/sessions/")))
  ;; PI_CODING_AGENT_DIR replaces the default agent dir.
  (let ((process-environment '("HOME=/tmp/fake-home"
                               "PI_CODING_AGENT_DIR=/tmp/agent-dir")))
    (should (equal (pilish-jsonl-sessions-root)
                   "/tmp/agent-dir/sessions/")))
  ;; A remote session-file anchor roots the scan on that remote: the
  ;; parent of the anchor's own directory.
  (should (equal (pilish-jsonl-sessions-root
                  "/ssh:fake:/home/u/.pi/agent/sessions/--home-u-proj--/s.jsonl")
                 "/ssh:fake:/home/u/.pi/agent/sessions/"))
  ;; A local anchor is ignored; the expanded default still applies.
  (let ((process-environment '("HOME=/tmp/fake-home")))
    (should (equal (pilish-jsonl-sessions-root "/local/anchor.jsonl")
                   "/tmp/fake-home/.pi/agent/sessions/"))))

(ert-deftest pilish-test-jsonl-read-session-info-fields ()
  "read-session-info returns the exact browse session dialect plist.
Key parity with the session browser is the contract: :path :id :cwd
:name (latest session_info wins, trimmed) :parentSessionPath :created
(header timestamp) :modified (mtime as UTC ISO-8601) :messageCount
(regex count, toolResult included) :firstMessage (first user text within
the five-message parse budget).  label and custom entries are ignored."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-info"))
         (path (expand-file-name "session.jsonl" dir))
         (mtime (encode-time 45 31 14 2 3 2026)))
    (pilish-test--write-jsonl
     path
     (list `(:type "session" :version 3 :id "aaaabbbb-cccc-4ddd-8eee-000000000001"
             :timestamp "2026-03-02T10:00:00.000Z" :cwd "/tmp/proj"
             :parentSession "/elsewhere/root.jsonl")
           (pilish-test--jsonl-msg
            "m1" nil 0 '(:role "user"
                          :content [(:type "text" :text "hello")
                                    (:type "text" :text "world")]))
           (pilish-test--jsonl-msg
            "m2" "m1" 1 '(:role "assistant" :content "interim answer"))
           (pilish-test--jsonl-msg
            "m3" "m2" 2 '(:role "toolResult" :toolCallId "tc1"
                           :toolName "read"))
           (pilish-test--jsonl-entry
            "label" "l1" "m3" 3 :targetId "m1" :label "ignored")
           (pilish-test--jsonl-entry
            "custom" "c1" "l1" 4 :customType "decoy")
           (pilish-test--jsonl-entry
            "session_info" "s1" "c1" 5 :name "First")
           (pilish-test--jsonl-entry
            "session_info" "s2" "s1" 6 :name "  Second  ")))
    (set-file-times path mtime)
    (should (equal (pilish-jsonl-read-session-info path)
                   (list :path path
                         :id "aaaabbbb-cccc-4ddd-8eee-000000000001"
                         :cwd "/tmp/proj"
                         :name "Second"
                         :parentSessionPath "/elsewhere/root.jsonl"
                         :created "2026-03-02T10:00:00.000Z"
                         :modified (format-time-string
                                    "%Y-%m-%dT%H:%M:%SZ" mtime t)
                         :messageCount 3
                         :firstMessage "hello world")))))

(ert-deftest pilish-test-jsonl-read-session-header-prefix ()
  "The header probe grows its byte prefix, never reading the history."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-header-prefix"))
         (short (expand-file-name "short.jsonl" dir))
         (long (expand-file-name "long.jsonl" dir))
         (blank (expand-file-name "blank.jsonl" dir))
         (unterminated (expand-file-name "unterminated.jsonl" dir))
         (invalid (expand-file-name "invalid.jsonl" dir))
         (original (symbol-function 'insert-file-contents))
         (reads nil))
    (unwind-protect
        (progn
          (with-temp-file short
            (insert (json-encode '(:type "session" :cwd "/tmp/a"))
                    "\n" (make-string 10000 ?x)))
          (with-temp-file long
            (insert (json-encode
                     (list :type "session" :cwd "/tmp/b"
                           :id (make-string 4500 ?x))) "\n"
                    (make-string 10000 ?x)))
          (with-temp-file blank
            (insert (make-string 4300 ?\s) "\n"
                    (json-encode '(:type "session" :cwd "/tmp/c")) "\n"
                    (make-string 10000 ?x)))
          (with-temp-file unterminated
            (insert (json-encode
                     (list :type "session" :cwd "/tmp/d"
                           :id (make-string 4500 ?x)))))
          (with-temp-file invalid
            (insert "not json\n"
                    (json-encode '(:type "session" :cwd "/tmp/a")) "\n"))
          (cl-letf (((symbol-function 'insert-file-contents)
                     (lambda (path &rest args)
                       (push (cons path args) reads)
                       (apply original path args))))
            (should (equal (plist-get (pilish-jsonl-read-session-header short)
                                      :cwd)
                           "/tmp/a"))
            (should (equal reads (list (list short nil 0 4096))))
            (setq reads nil)
            (should (equal (plist-get (pilish-jsonl-read-session-header long)
                                      :cwd)
                           "/tmp/b"))
            (should (equal (nreverse reads)
                           (list (list long nil 0 4096)
                                 (list long nil 0 8192))))
            (setq reads nil)
            (should (equal (plist-get (pilish-jsonl-read-session-header blank)
                                      :cwd)
                           "/tmp/c"))
            (should (equal (nreverse reads)
                           (list (list blank nil 0 4096)
                                 (list blank nil 0 8192))))
            (setq reads nil)
            (should (equal (plist-get
                            (pilish-jsonl-read-session-header unterminated)
                            :cwd)
                           "/tmp/d"))
            (should (equal (nreverse reads)
                           (list (list unterminated nil 0 4096)
                                 (list unterminated nil 0
                                       (file-attribute-size
                                        (file-attributes unterminated))))))
            (should-not (pilish-jsonl-read-session-header invalid))
            (should-not (pilish-jsonl-read-session-header
                         (expand-file-name "missing.jsonl" dir)))))
      (delete-directory dir t))))

(ert-deftest pilish-test-jsonl-read-session-header-long-utf8-bounded ()
  "Probe complete UTF-8 headers without reading their large histories."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-utf8-header-"))
         (header (json-encode (list :type "session" :cwd "/tmp/utf8"
                                    :id (make-string 1500 ?界))))
         (original (symbol-function 'insert-file-contents)))
    (unwind-protect
        (progn
          (should (> (string-bytes header) 4096))
          (should (< (length header) 4096))
          (dolist (leading-blanks '(nil t))
            (let* ((path (expand-file-name
                          (if leading-blanks "blank.jsonl" "header.jsonl")
                          dir))
                   (prefix (if leading-blanks
                               (concat (make-string 4200 ?\s) "\n")
                             ""))
                   reads)
              (with-temp-file path
                (insert prefix header "\n" (make-string 200000 ?x)))
              (should (equal (plist-get (pilish-jsonl-read-session-info path)
                                            :cwd)
                             "/tmp/utf8"))
              (cl-letf (((symbol-function 'insert-file-contents)
                         (lambda (file &rest args)
                           (push (cons file args) reads)
                           (apply original file args))))
                (should (equal (plist-get (pilish-jsonl-read-session-header path)
                                              :cwd)
                               "/tmp/utf8")))
              (should reads)
              (should (cl-every (lambda (read)
                                  (and (equal (car read) path)
                                       (numberp (nth 3 read))
                                       (<= (nth 3 read) 16384)
                                       (< (nth 3 read)
                                          (file-attribute-size
                                           (file-attributes path)))))
                                reads)))))
      (delete-directory dir t))))

(ert-deftest pilish-test-jsonl-read-session-info-keeps-name-before-malformed-tail ()
  "A malformed session_info tail does not clear the latest parseable name."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-name-tail"))
         (path (expand-file-name "session.jsonl" dir)))
    (unwind-protect
        (progn
          (pilish-test--write-jsonl
           path
           (list pilish-test--jsonl-header
                 (pilish-test--jsonl-entry
                  "session_info" "s1" nil 0 :name "Keep me")))
          (with-temp-buffer
            (insert "{\"type\":\"session_info\",\"id\":\"torn\"\n")
            (write-region (point-min) (point-max) path t 'silent))
          (should (equal (plist-get
                          (pilish-jsonl-read-session-info path)
                          :name)
                         "Keep me")))
      (delete-directory dir t))))

(ert-deftest pilish-test-jsonl-read-session-info-fallbacks ()
  "read-session-info degrades on budget misses and malformed files.
The firstMessage budget full-parses at most 5 message lines: a user
message inside the budget wins, otherwise the first parsed message of
any role is the fallback.  Files without a session header as their
first nonblank line, empty files, and unreadable files all read as
nil.  No session_info means no :name key at all."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-fb"))
         (late-user (expand-file-name "late-user.jsonl" dir))
         (budget (expand-file-name "budget.jsonl" dir))
         (no-header (expand-file-name "no-header.jsonl" dir))
         (garbage (expand-file-name "garbage.jsonl" dir))
         (empty (expand-file-name "empty.jsonl" dir))
         (unnamed (expand-file-name "unnamed.jsonl" dir)))
    ;; A user message within the 5-line budget beats earlier non-user
    ;; messages.
    (pilish-test--write-jsonl
     late-user
     (list pilish-test--jsonl-header
           (pilish-test--jsonl-asst
            "a1" nil 0 :content "assistant speaks first")
           (pilish-test--jsonl-msg
            "u1" "a1" 1 '(:role "user" :content "pick me"))))
    (should (equal (plist-get (pilish-jsonl-read-session-info
                               late-user)
                              :firstMessage)
                   "pick me"))
    ;; Six non-user messages exhaust the budget; the any-role fallback
    ;; supplies the first message, while messageCount still counts all
    ;; message lines by regex.
    (let ((lines (list pilish-test--jsonl-header)))
      (dotimes (i 6)
        (push (pilish-test--jsonl-asst
               (format "b%d" (1+ i)) (if (zerop i) nil (format "b%d" i))
               i :content (format "m%d" (1+ i)))
              lines))
      (push (pilish-test--jsonl-msg
             "u9" "b6" 10 '(:role "user" :content "too late"))
            lines)
      (pilish-test--write-jsonl budget (nreverse lines)))
    (let ((info (pilish-jsonl-read-session-info budget)))
      (should (equal (plist-get info :firstMessage) "m1"))
      (should (= (plist-get info :messageCount) 7)))
    ;; No session header line: nil regardless of the entries.
    (pilish-test--write-jsonl
     no-header
     (list (pilish-test--jsonl-msg
            "x1" nil 0 '(:role "user" :content "orphaned"))))
    (should-not (pilish-jsonl-read-session-info no-header))
    ;; Junk as the first nonblank line (a well-formed header follows
    ;; on the next line): the file is not a session file.
    (with-temp-file garbage
      (insert "{not json at all\n"
              (json-encode pilish-test--jsonl-header) "\n"))
    (should-not (pilish-jsonl-read-session-info garbage))
    ;; Empty file: nil.
    (with-temp-file empty)
    (should-not (pilish-jsonl-read-session-info empty))
    ;; Unreadable file: nil (the scan skips it silently).  Running as
    ;; root can always read it, so skip the probe there.
    (unless (zerop (user-uid))
      (let ((unreadable (expand-file-name "unreadable.jsonl" dir)))
        (pilish-test--write-jsonl
         unreadable (list pilish-test--jsonl-header))
        (set-file-modes unreadable #o000)
        (unwind-protect
            (should-not (pilish-jsonl-read-session-info unreadable))
          (set-file-modes unreadable #o600))))
    ;; No session_info entry: the :name key is absent entirely.
    (pilish-test--write-jsonl
     unnamed
     (list pilish-test--jsonl-header
           (pilish-test--jsonl-msg
            "u1" nil 0 '(:role "user" :content "hello"))))
    (should-not (plist-member (pilish-jsonl-read-session-info unnamed)
                              :name))))

(ert-deftest pilish-test-jsonl-read-session-info-modified-iso ()
  ":modified is a second-resolution UTC ISO string that round-trips
through `date-to-time' and orders lexicographically like time."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-iso"))
         (older (expand-file-name "older.jsonl" dir))
         (newer (expand-file-name "newer.jsonl" dir))
         (t1 (encode-time 10 30 12 2 3 2026))
         (t2 (time-add t1 90)))
    (pilish-test--write-jsonl
     older (list pilish-test--jsonl-header))
    (pilish-test--write-jsonl
     newer (list pilish-test--jsonl-header))
    (set-file-times older t1)
    (set-file-times newer t2)
    (let* ((mod-a (plist-get (pilish-jsonl-read-session-info older)
                             :modified))
           (mod-b (plist-get (pilish-jsonl-read-session-info newer)
                             :modified)))
      (dolist (m (list mod-a mod-b))
        (should (string-match-p
                 "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'"
                 m)))
      ;; Round trip back to the statted second.
      (should (time-equal-p (date-to-time mod-a) t1))
      (should (time-equal-p (date-to-time mod-b) t2))
      ;; Lexicographic order matches chronological order.
      (should (string< mod-a mod-b))
      (should (string> mod-b mod-a)))))

;;;; Optional Session Search Corpus

(ert-deftest pilish-test-jsonl-session-search-corpus-scope-and-metadata ()
  "Opt-in search includes text on all branches, never hidden payloads."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-search"))
         (path (expand-file-name "session.jsonl" dir)))
    (pilish-test--write-jsonl
     path
     (list '(:type "session" :id "headerneedle" :cwd "/metadata-needle")
           '(:type "message" :id "u1" :parentId nil
             :message (:role "user" :content "opening"))
           '(:type "message" :id "a1" :parentId "u1"
             :message (:role "assistant" :content "inactive alpha"))
           '(:type "message" :id "u2" :parentId "u1"
             :message (:role "user"
                       :content [(:type "text" :text "active")
                                 (:type "image" :data "imageneedle" :text "imagetextneedle")
                                 (:type "text" :text "beta")]))
           '(:type "message" :id "a2" :parentId "u2"
             :message (:role "assistant"
                       :content [(:type "thinking" :thinking "thinkingneedle")
                                 (:type "toolCall" :name "toolnameneedle"
                                  :arguments (:text "argsneedle"))
                                 (:type "text" :text "omega")
                                 (:type "text" :text "")
                                 (:type "text" :text 42)]))
           '(:type "message" :message (:role "toolResult" :content "toolneedle"))
           '(:type "message" :message (:role "custom" :content "customroleneedle"))
           '(:type "message" :message (:role "system" :content "systemneedle"))
           '(:type "message" :message (:role "user" :content nil))
           '(:type "branch_summary" :summary "branchneedle")
           '(:type "compaction" :summary "compactneedle")
           '(:type "custom" :message (:role "user" :content "customentryneedle"))
           '(:type "label" :label "labelneedle")
           '(:type "session_info" :name " Name ")))
    ;; Canonical type-first routing remains intentional, and malformed late
    ;; message/name lines neither lose prior text nor clear the latest name.
    (with-temp-buffer
      (insert "{\"id\":\"noncanonical\",\"type\":\"message\",\"message\":{\"role\":\"user\",\"content\":\"orderneedle\"}}\n"
              "{\"type\":\"message\",broken\n"
              "{\"type\":\"session_info\",broken\n")
      (write-region (point-min) (point-max) path t 'silent))
    (let* ((metadata (pilish-jsonl-read-session-info path))
           (full (pilish-jsonl-read-session-info path t))
           (without-corpus (copy-sequence full)))
      (cl-remf without-corpus :searchText)
      (should (equal without-corpus metadata))
      (should-not (plist-member metadata :searchText))
      (should-not (plist-member full :allMessagesText))
      (should (equal (plist-get full :searchText)
                     "Name opening opening inactive alpha active beta omega ")))))

(ert-deftest pilish-test-jsonl-session-search-unicode-eof-and-decoding ()
  "Full search preserves Unicode and embedded whitespace through decoded EOF."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-search-utf8"))
         (path (expand-file-name "session.jsonl" dir)))
    (dolist (coding '(utf-8-unix utf-8-dos utf-8-with-signature-dos))
      (let ((coding-system-for-write coding))
        (with-temp-file path
          (insert " \t\n"
                  (json-encode pilish-test--jsonl-header) "\n"
                  (json-encode '(:type "message" :message (:role "user" :content "first")))
                  "\n"
                  ;; Deliberately no final newline, regardless of encoding.
                  (json-encode '(:type "message"
                                 :message (:role "assistant" :content "finál\n東京\t🚀"))))))
      (should (equal (plist-get (pilish-jsonl-read-session-info path t) :searchText)
                     " first first finál\n東京\t🚀")))))

(ert-deftest pilish-test-jsonl-session-search-default-preview-budget ()
  "Default metadata parses only the legacy preview budget and name records."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-search-budget"))
         (path (expand-file-name "session.jsonl" dir))
         (original (symbol-function 'pilish--jsonl-parse-current-line))
         (parsed 0))
    (with-temp-file path
      (insert (json-encode pilish-test--jsonl-header) "\n"
              "{\"type\":\"message\",broken\n")
      (dotimes (_ 4)
        (insert (json-encode '(:type "message" :message (:role "toolResult" :content "legacy fallback"))) "\n"))
      (dotimes (_ 20)
        (insert (json-encode '(:type "message" :message (:role "user" :content "late user"))) "\n"))
      (insert (json-encode '(:type "session_info" :name " Old ")) "\n"
              (json-encode '(:type "session_info" :name "  ")) "\n"))
    (cl-letf (((symbol-function 'pilish--jsonl-parse-current-line)
               (lambda () (cl-incf parsed) (funcall original))))
      (let ((metadata (pilish-jsonl-read-session-info path)))
        (should (= parsed 7))
        (should (= (plist-get metadata :messageCount) 25))
        (should (equal (plist-get metadata :firstMessage) "legacy fallback"))
        (should-not (plist-member metadata :name))
        (should-not (plist-member metadata :searchText))))))

(ert-deftest pilish-test-jsonl-session-search-legacy-empty-fallback ()
  "Full extraction doesn't overwrite the first-five any-role preview choice."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-search-fallback"))
         (path (expand-file-name "session.jsonl" dir)))
    (dolist (first '("" "legacy tool"))
      (pilish-test--write-jsonl
       path
       (append (list pilish-test--jsonl-header
                     `(:type "message" :message (:role "toolResult" :content ,first)))
               (make-list 4 '(:type "message" :message (:role "toolResult" :content "excluded later tool")))
               (list '(:type "message" :message (:role "user" :content "late user")))))
      (let* ((metadata (pilish-jsonl-read-session-info path))
             (full (pilish-jsonl-read-session-info path t))
             (without-corpus (copy-sequence full)))
        (cl-remf without-corpus :searchText)
        (should (equal metadata without-corpus))
        (should (equal (plist-get full :searchText) (concat " " first " late user")))
        (should (equal (plist-get full :firstMessage) (unless (equal first "") first)))))))

(ert-deftest pilish-test-jsonl-session-search-invalid-and-unreadable-files ()
  "Opt-in reading skips bad headers and read failures, like metadata-only."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-search-invalid"))
         (path (expand-file-name "session.jsonl" dir)))
    (dolist (contents '("" "garbage\n{\"type\":\"session\"}\n"
                        "{\"type\":\"message\"}\n"))
      (with-temp-file path (insert contents))
      (should-not (pilish-jsonl-read-session-info path t)))
    (should-not (pilish-jsonl-read-session-info (expand-file-name "missing.jsonl" dir) t))
    (pilish-test--write-jsonl path (list pilish-test--jsonl-header))
    (let (scan-buffer)
      (cl-letf (((symbol-function 'insert-file-contents)
                 (lambda (&rest _)
                   (setq scan-buffer (current-buffer))
                   (signal 'file-error '("read failed")))))
        (should-not (pilish-jsonl-read-session-info path t)))
      (should (bufferp scan-buffer))
      (should-not (buffer-live-p scan-buffer)))))

;;;; Navigation

(defun pilish-test--jsonl-line-string (line)
  "Return the raw JSON string `--write-jsonl' writes for LINE."
  (json-encode (pilish-test--jsonl-literalize line)))

(defun pilish-test--jsonl-session-at (lines)
  "Write LINES (header first) to a temp file and return the session.
The `pilish-jsonl-read-file' result is what
`pilish-jsonl-navigation-target' consumes."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-nav"))
         (path (expand-file-name "session.jsonl" dir)))
    (pilish-test--write-jsonl path lines)
    (pilish-jsonl-read-file path)))

(ert-deftest pilish-test-jsonl-navigation-target-user-message ()
  "A user-message target rewinds the leaf to its parent with the
message text as prefill (pi's re-edit rule).  Prefill is the
contentText port: strings pass through; block content joins with the
EMPTY separator, unbounded — the preview dialect's space join and
200-char cap do not apply.  An empty, image-only, or null content
omits :prefill entirely; the rewind to the parent still applies."
  (let* ((long (make-string 300 ?z))
         (session (pilish-test--jsonl-session-at
                   (list pilish-test--jsonl-header
                         (pilish-test--jsonl-msg
                          "u1" nil 0 '(:role "user" :content "root"))
                         (pilish-test--jsonl-msg
                          "u2" "u1" 1 '(:role "user" :content "hello there"))
                         (pilish-test--jsonl-msg
                          "u3" "u2" 2 '(:role "user"
                                        :content [(:type "text" :text "alpha")
                                                  (:type "text" :text "beta")]))
                         (pilish-test--jsonl-msg
                          "u4" "u3" 3 `(:role "user" :content ,long))
                         (pilish-test--jsonl-msg
                          "u5" "u4" 4 '(:role "user" :content ""))
                         (pilish-test--jsonl-msg
                          "u6" "u5" 5 '(:role "user"
                                        :content [(:type "image")]))
                         (pilish-test--jsonl-msg
                          "u7" "u6" 6 '(:role "user" :content :null))))))
    ;; String content: the leaf is the parent id, the prefill the text.
    (should (equal (pilish-jsonl-navigation-target session "u2")
                   (list :leaf-id "u1" :prefill "hello there" :current-p nil)))
    ;; Block content joins with the empty string, not a space.
    (should (equal (pilish-jsonl-navigation-target session "u3")
                   (list :leaf-id "u2" :prefill "alphabeta" :current-p nil)))
    ;; Unbounded: no preview-style 200-char cap.
    (should (equal (pilish-jsonl-navigation-target session "u4")
                   (list :leaf-id "u3" :prefill long :current-p nil)))
    ;; Empty, image-only, and null contents omit :prefill entirely.
    (should (equal (pilish-jsonl-navigation-target session "u5")
                   (list :leaf-id "u4" :current-p nil)))
    (should (equal (pilish-jsonl-navigation-target session "u6")
                   (list :leaf-id "u5" :current-p nil)))
    (should (equal (pilish-jsonl-navigation-target session "u7")
                   (list :leaf-id "u6" :current-p nil)))))

(ert-deftest pilish-test-jsonl-navigation-target-custom-message ()
  "A custom_message target rewinds like a user message: the leaf is
its parent id and the prefill is the contentText of the entry's own
:content (string passthrough; blocks joined with the empty string).
Empty content omits :prefill; :customType never becomes the prefill."
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "root"))
                        (pilish-test--jsonl-entry
                         "custom_message" "c1" "u1" 1
                         :customType "notice" :content "re-edit me")
                        (pilish-test--jsonl-entry
                         "custom_message" "c2" "c1" 2
                         :customType "notice"
                         :content [(:type "text" :text "part one")
                                   (:type "text" :text "part two")])
                        (pilish-test--jsonl-entry
                         "custom_message" "c3" "c2" 3
                         :customType "notice" :content "")))))
    (should (equal (pilish-jsonl-navigation-target session "c1")
                   (list :leaf-id "u1" :prefill "re-edit me" :current-p nil)))
    (should (equal (pilish-jsonl-navigation-target session "c2")
                   (list :leaf-id "c1" :prefill "part onepart two"
                         :current-p nil)))
    ;; Empty content: rewind to the parent, no :prefill key at all.
    (should (equal (pilish-jsonl-navigation-target session "c3")
                   (list :leaf-id "c2" :current-p nil)))))

(ert-deftest pilish-test-jsonl-navigation-target-non-user-self-leaf ()
  "Every other entry type targets ITSELF: assistant, toolResult,
compaction, model_change, thinking_level_change, and branch_summary
all keep their own id as the leaf, and none carries :prefill.  The
literal raw leaf (bs1) is current, matching pi's raw-id no-op and the
resolved-position truth table; earlier self targets are not."
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "root"))
                        (pilish-test--jsonl-asst
                         "a1" "u1" 1 :content "reply" :stopReason "end_turn")
                        (pilish-test--jsonl-msg
                         "t1" "a1" 2 '(:role "toolResult" :toolCallId "tc1"))
                        (pilish-test--jsonl-entry
                         "compaction" "k1" "t1" 3 :tokensBefore 4096)
                        (pilish-test--jsonl-entry
                         "model_change" "mo1" "k1" 4
                         :provider "anthropic" :modelId "claude")
                        (pilish-test--jsonl-entry
                         "thinking_level_change" "th1" "mo1" 5
                         :thinkingLevel "high")
                        (pilish-test--jsonl-entry
                         "branch_summary" "bs1" "th1" 6 :summary "explored")))))
    (dolist (id '("a1" "t1" "k1" "mo1" "th1" "bs1"))
      (should (equal (pilish-jsonl-navigation-target session id)
                     (list :leaf-id id :current-p (equal id "bs1")))))))

(ert-deftest pilish-test-jsonl-navigation-target-unknown-nil ()
  "An id no entry carries reads as nil — no target, no signal."
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "root"))))))
    (should-not (pilish-jsonl-navigation-target session "deadbeef"))))

(ert-deftest pilish-test-jsonl-current-projected-id ()
  "The current projected id skips only projection-away bookkeeping.
A user followed by label/session-info/custom records remains the
current projected entry.  Model and thinking changes are displayable
projected entries, so the last of those remains current even when a
browser filter later hides it."
  (let ((bookkeeping
         (pilish-test--jsonl-session-at
          (list pilish-test--jsonl-header
                (pilish-test--jsonl-msg
                 "u1" nil 0 '(:role "user" :content "current"))
                (pilish-test--jsonl-entry
                 "label" "l1" "u1" 1 :targetId "u1" :label "tag")
                (pilish-test--jsonl-entry
                 "session_info" "s1" "l1" 2 :name "named")
                (pilish-test--jsonl-entry
                 "custom" "c1" "s1" 3 :customType "meta" :data '(:ok t)))))
        (settings
         (pilish-test--jsonl-session-at
          (list pilish-test--jsonl-header
                (pilish-test--jsonl-msg
                 "u1" nil 0 '(:role "user" :content "prompt"))
                (pilish-test--jsonl-entry
                 "model_change" "m1" "u1" 1
                 :provider "test" :modelId "model")
                (pilish-test--jsonl-entry
                 "thinking_level_change" "h1" "m1" 2
                 :thinkingLevel "high")))))
    (should (equal (pilish-jsonl-current-projected-id bookkeeping) "u1"))
    (should (equal (pilish-jsonl-current-projected-id settings) "h1"))))

(ert-deftest pilish-test-jsonl-navigation-rewinds-onto-system-parent ()
  "A rewound file whose leaf is the system entry stays addressable.
Re-editing the first user message targets its system parent (the
rewind rule), and that position is CURRENT: the system entry is
projected — not projection-filtered — so
`pilish-jsonl-current-projected-id' resolves the file's raw leaf to it
instead of nil, keeping the :current-p equality check honest."
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        ;; Post-rewrite order: the user/assistant chain
                        ;; leads, the targeted system leaf ends the file.
                        (pilish-test--jsonl-msg
                         "u1" "sys1" 1 '(:role "user" :content "hello"))
                        (pilish-test--jsonl-msg
                         "a1" "u1" 2 '(:role "assistant" :content "reply"))
                        (pilish-test--jsonl-msg
                         "sys1" nil 0 '(:role "system" :content ""))))))
    (should (equal (pilish-jsonl-current-projected-id session) "sys1"))
    (should (equal (pilish-jsonl-navigation-target session "u1")
                   (list :leaf-id "sys1" :prefill "hello" :current-p t)))))

(ert-deftest pilish-test-jsonl-navigation-target-current-position ()
  ":current-p compares positively RESOLVED positions.
A trailing filtered leaf (label here) resolves up, so targeting the
visible entry it sits on is current; a target on another branch is not;
a file already rewound makes the same user message current again AND
still prefills (re-edit the same prompt).  Two unresolved nil positions
are not proof of equality."
  ;; Trailing label child: targeting the entry it resolves to is current.
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "root"))
                        (pilish-test--jsonl-asst
                         "a1" "u1" 1 :content "reply" :stopReason "end_turn")
                        (pilish-test--jsonl-entry
                         "label" "l1" "a1" 2 :targetId "u1" :label "tag")))))
    (should (equal (pilish-jsonl-navigation-target session "a1")
                   (list :leaf-id "a1" :current-p t))))
  ;; A target on another branch is not current.
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "root"))
                        (pilish-test--jsonl-asst
                         "a1" "u1" 1 :content "active" :stopReason "end_turn")
                        (pilish-test--jsonl-asst
                         "b1" "u1" 2 :content "abandoned"
                         :stopReason "end_turn")
                        (pilish-test--jsonl-msg
                         "u2" "a1" 3 '(:role "user" :content "leaf"))))))
    (should (equal (pilish-jsonl-navigation-target session "b1")
                   (list :leaf-id "b1" :current-p nil))))
  ;; Re-edit again: the file already sits on the parent (a previous
  ;; navigate put u1 last), so the same user message is current AND
  ;; still carries its prefill.
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-msg
                         "u2" "u1" 1 '(:role "user" :content "try the other way"))
                        (pilish-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "root"))))))
    (should (equal (pilish-jsonl-navigation-target session "u2")
                   (list :leaf-id "u1" :prefill "try the other way"
                         :current-p t))))
  ;; nil is unresolved, not a position: nil == nil cannot prove current.
  (let ((session (pilish-test--jsonl-session-at
                  (list pilish-test--jsonl-header
                        (pilish-test--jsonl-entry
                         "label" "l1" nil 0 :targetId "l1" :label "only")))))
    (should (equal (pilish-jsonl-navigation-target session "l1")
                   (list :leaf-id "l1" :current-p nil)))))

(ert-deftest pilish-test-jsonl-navigation-current-needs-proven-equality ()
  "Ambiguous or unresolved positions cannot manufacture :current-p.
A unique user target rewinds to a root bookkeeping parent that has no
visible resolution, while the raw current leaf is a differing duplicate
and therefore also unresolved.  These distinct nil resolutions must not
compare current.  Conversely, a unique current leaf does not make a
request whose target ancestry is ambiguous safe: that target fails
closed instead of claiming equality."
  ;; Astra's exact false-no-op shape: both resolvers return nil for
  ;; different reasons, but the unique user target remains navigable.
  (let* ((session
          (pilish-test--jsonl-session-at
           (list pilish-test--jsonl-header
                 (pilish-test--jsonl-entry
                  "custom" "meta" nil 0 :customType "root-meta")
                 (pilish-test--jsonl-msg
                  "u" "meta" 1 '(:role "user" :content "safe prompt"))
                 (pilish-test--jsonl-msg
                  "dup" nil 2 '(:role "assistant" :content "first"))
                 (pilish-test--jsonl-msg
                  "dup" nil 3 '(:role "assistant" :content "later")))))
         (target (pilish-jsonl-navigation-target session "u")))
    (should-not (pilish-jsonl-current-projected-id session))
    (should (equal target
                   (list :leaf-id "meta" :prefill "safe prompt"
                         :current-p nil))))
  ;; The current position is provably unique, but the selected user would
  ;; rewind through a differing duplicate parent and must fail closed.
  (let ((session
         (pilish-test--jsonl-session-at
          (list pilish-test--jsonl-header
                (pilish-test--jsonl-entry
                 "custom" "dup-parent" nil 0 :customType "first")
                (pilish-test--jsonl-entry
                 "custom" "dup-parent" nil 1 :customType "later")
                (pilish-test--jsonl-msg
                 "u" "dup-parent" 2 '(:role "user" :content "unsafe"))
                (pilish-test--jsonl-msg
                 "current" nil 3
                 '(:role "assistant" :content "current"))))))
    (should (equal (pilish-jsonl-current-projected-id session) "current"))
    (should-not (pilish-jsonl-navigation-target session "u"))))

(ert-deftest pilish-test-jsonl-navigation-lines-chain-to-end ()
  "navigation-lines reorders for a rewrite: the header line stays
first, the
leaf's ancestor chain moves to the end (the leaf itself last), and
every other line keeps its original relative order ahead of it.
Mid-chain bookkeeping entries (label, session_info) are ordinary
chain nodes and travel with the chain; a second root and an
off-branch sibling lead."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-navord"))
         (path (expand-file-name "session.jsonl" dir))
         (header pilish-test--jsonl-header)
         (lines
          (list header
                (pilish-test--jsonl-msg
                 "u1" nil 0 '(:role "user" :content "root"))
                (pilish-test--jsonl-msg
                 "x1" nil 1 '(:role "user" :content "second root"))
                (pilish-test--jsonl-msg
                 "b1" "u1" 2 '(:role "user" :content "sibling"))
                (pilish-test--jsonl-entry
                 "label" "l1" "u1" 3 :targetId "u1" :label "tag")
                (pilish-test--jsonl-entry
                 "session_info" "s1" "l1" 4 :name "Named")
                (pilish-test--jsonl-msg
                 "a1" "s1" 5 '(:role "assistant" :content "reply"))
                (pilish-test--jsonl-msg
                 "u2" "a1" 6 '(:role "user" :content "leaf")))))
    (pilish-test--write-jsonl path lines)
    ;; Chain from u2: u2 a1 s1 l1 u1 — the non-chain x1 and b1 lead.
    (should (equal (pilish-jsonl-navigation-lines path "u2")
                   (vconcat
                    (mapcar #'pilish-test--jsonl-line-string
                            (list (nth 0 lines) (nth 2 lines) (nth 3 lines)
                                  (nth 1 lines) (nth 4 lines) (nth 5 lines)
                                  (nth 6 lines) (nth 7 lines))))))))

(ert-deftest pilish-test-jsonl-navigation-lines-consecutive-cross-branch-navigation ()
  "Consecutive cross-branch rewrites leave the newly requested leaf last.
The first rewrite moves the shared root behind the other branch in
physical order; the second must still order its chain logically rather
than finish on that shared root."
  (let* ((dir (pilish-test--make-temp-directory
               "pi-jsonl-nav-cross-branch"))
         (path (expand-file-name "session.jsonl" dir))
         (header pilish-test--jsonl-header)
         (root (pilish-test--jsonl-msg
                "root" nil 0 '(:role "user" :content "root")))
         (a1 (pilish-test--jsonl-asst
              "a1" "root" 1 :content "branch A reply" :stopReason "end_turn"))
         (a2 (pilish-test--jsonl-msg
              "a2" "a1" 2 '(:role "user" :content "branch A leaf")))
         (b1 (pilish-test--jsonl-asst
              "b1" "root" 3 :content "branch B reply" :stopReason "end_turn"))
         (b2 (pilish-test--jsonl-msg
              "b2" "b1" 4 '(:role "user" :content "branch B leaf")))
         (fixture (list header root a1 a2 b1 b2)))
    ;; The valid branched file initially ends at b2.
    (pilish-test--write-jsonl path fixture)
    (let* ((original
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally path)
              (vconcat
               (butlast
                (split-string
                 (buffer-substring-no-properties (point-min) (point-max))
                 "\n")))))
           (original-count (length original))
           (original-multiset (sort (append original nil) #'string<))
           (expected-second
            (vector (aref original 0) ; header
                    (aref original 2) ; non-chain a1
                    (aref original 3) ; non-chain a2
                    (aref original 1) ; chain root
                    (aref original 4) ; chain b1
                    (aref original 5))) ; requested leaf b2
           (first (pilish-jsonl-navigation-lines path "a2")))
      ;; Rewrite A preserves every physical line and ends at a2.
      (should (= (length first) original-count))
      (should (equal (sort (append first nil) #'string<)
                     original-multiset))
      (should (equal (aref first (1- (length first)))
                     (aref original 3)))
      ;; Reproduce the atomic writer's bytes: returned lines joined by LF,
      ;; plus exactly one final LF, then navigate across to branch B.
      (let ((coding-system-for-write 'no-conversion))
        (write-region
         (concat (mapconcat #'identity (append first nil) "\n") "\n")
         nil path nil 0))
      (let ((second (pilish-jsonl-navigation-lines path "b2")))
        ;; Rewrite B has the same byte multiset and count, but must now end
        ;; at b2 rather than the physically later shared root.
        (should (= (length second) original-count))
        (should (equal (sort (append second nil) #'string<)
                       original-multiset))
        (should (equal (aref second (1- (length second)))
                       (aref original 5)))
        (should (equal second expected-second))))))

(ert-deftest pilish-test-jsonl-navigation-lines-preserve-malformed-and-blank ()
  "Malformed and blank lines are non-chain BYTES: kept verbatim in
the leading partition, in their original relative order.  The result
lines carry no trailing newline — the caller joins and terminates."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-navml"))
         (path (expand-file-name "torn.jsonl" dir))
         (header-line (json-encode pilish-test--jsonl-header))
         (u1-line (json-encode
                   (list :type "message" :id "u1" :parentId nil
                         :timestamp "2026-03-02T10:00:00.000Z"
                         :message '(:role "user" :content "root"))))
         (u2-line (json-encode
                   (list :type "message" :id "u2" :parentId "u1"
                         :timestamp "2026-03-02T10:00:01.000Z"
                         :message '(:role "user" :content "leaf"))))
         (garbage "{\"type\":\"message\",\"id\":\"torn\",\"paren"))
    (with-temp-file path
      (insert header-line "\n"
              u1-line "\n"
              garbage "\n"
              "\n"
              u2-line "\n"))
    (should (equal (pilish-jsonl-navigation-lines path "u2")
                   (vector header-line garbage "" u1-line u2-line)))))

(ert-deftest pilish-test-jsonl-navigation-lines-nil-cases ()
  "Missing, empty, and headerless files, and unknown or nil leaf ids,
all read as nil — there is no reorder to express."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-navnil"))
         (missing (expand-file-name "missing.jsonl" dir))
         (empty (expand-file-name "empty.jsonl" dir))
         (headerless (expand-file-name "headerless.jsonl" dir))
         (valid (expand-file-name "valid.jsonl" dir)))
    (with-temp-file empty)
    (with-temp-file headerless
      (insert "{\"type\":\"message\",\"id\":\"u1\",\"parentId\":null,"
              "\"timestamp\":\"2026-03-02T10:00:00.000Z\","
              "\"message\":{\"role\":\"user\",\"content\":\"decoy\"}}\n"))
    (pilish-test--write-jsonl
     valid
     (list pilish-test--jsonl-header
           (pilish-test--jsonl-msg
            "u1" nil 0 '(:role "user" :content "root"))))
    (should-not (pilish-jsonl-navigation-lines missing "u1"))
    (should-not (pilish-jsonl-navigation-lines empty "u1"))
    (should-not (pilish-jsonl-navigation-lines headerless "u1"))
    (should-not (pilish-jsonl-navigation-lines valid "deadbeef"))
    (should-not (pilish-jsonl-navigation-lines valid nil))))

(ert-deftest pilish-test-jsonl-navigation-lines-identity ()
  "When the chain already ends the file, the partition is the
identity: a plain chain with its leaf last, and a chain whose last
line is a trailing label child of the visible leaf, both come back
unchanged."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-navid"))
         (plain (expand-file-name "plain.jsonl" dir))
         (plain-lines
          (list pilish-test--jsonl-header
                (pilish-test--jsonl-msg
                 "u1" nil 0 '(:role "user" :content "root"))
                (pilish-test--jsonl-asst
                 "a1" "u1" 1 :content "reply" :stopReason "end_turn")
                (pilish-test--jsonl-msg
                 "u2" "a1" 2 '(:role "user" :content "leaf")))))
    (pilish-test--write-jsonl plain plain-lines)
    (should (equal (pilish-jsonl-navigation-lines plain "u2")
                   (vconcat (mapcar #'pilish-test--jsonl-line-string
                                    plain-lines)))))
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-navlab"))
         (labeled (expand-file-name "labeled.jsonl" dir))
         (labeled-lines
          (list pilish-test--jsonl-header
                (pilish-test--jsonl-msg
                 "u1" nil 0 '(:role "user" :content "root"))
                (pilish-test--jsonl-msg
                 "u2" "u1" 1 '(:role "user" :content "leaf"))
                (pilish-test--jsonl-entry
                 "label" "l1" "u2" 2 :targetId "u1" :label "tag"))))
    (pilish-test--write-jsonl labeled labeled-lines)
    (should (equal (pilish-jsonl-navigation-lines labeled "l1")
                   (vconcat (mapcar #'pilish-test--jsonl-line-string
                                    labeled-lines))))))

(ert-deftest pilish-test-jsonl-navigation-lines-large-chain-linear ()
  "navigation-lines over a 100k-line chain answers correctly and fast.
Line 0 stays fixed, the off-chain side entries keep their physical
order ahead of the chain, the requested leaf ends the file, and the
whole call finishes inside a generous wall-clock budget that the
quadratic nth-per-access version measured 16s+ against."
  (let* ((dir (pilish-test--make-temp-directory "pi-jsonl-navbig"))
         (path (expand-file-name "big.jsonl" dir))
         (count 100000)
         (header-line (json-encode pilish-test--jsonl-header))
         (side1-line (json-encode '(:type "message" :id "side1" :parentId nil
                                    :timestamp "2026-03-02T10:00:00.000Z"
                                    :message (:role "user" :content "first side"))))
         (side2-line (json-encode '(:type "message" :id "side2" :parentId nil
                                    :timestamp "2026-03-02T10:00:01.000Z"
                                    :message (:role "user" :content "second side"))))
         (line-format "{\"type\":\"message\",\"id\":\"n%06d\",\"parentId\":%s}"))
    ;; Generate the file as raw bytes in a unibyte buffer; formatting
    ;; 100k minimal entries into a string list is fast enough here.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert header-line "\n" side1-line "\n" side2-line "\n")
      (dotimes (i count)
        (insert (format line-format i
                        (if (zerop i)
                            "null"
                          (format "\"n%06d\"" (1- i))))
                "\n"))
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) path nil 0)))
    (let* ((start (float-time))
           (lines (pilish-jsonl-navigation-lines
                   path (format "n%06d" (1- count))))
           (elapsed (- (float-time) start)))
      (should (= (length lines) (+ count 3)))
      (should (equal (aref lines 0) header-line))
      ;; Non-chain lines keep their physical relative order.
      (should (equal (aref lines 1) side1-line))
      (should (equal (aref lines 2) side2-line))
      ;; The requested leaf is the file's new last line.
      (should (equal (aref lines (1- (length lines)))
                     (format line-format (1- count)
                             (format "\"n%06d\"" (- count 2)))))
      ;; Wall-clock tripwire, generous for CI variance: the quadratic
      ;; version needs 16s+ on this file while the linear one stays
      ;; well under the budget.  No tighter timing is asserted.
      (should (< elapsed 10)))))

(provide 'pilish-jsonl-test)
;;; pilish-jsonl-test.el ends here
