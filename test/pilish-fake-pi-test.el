;;; pilish-fake-pi-test.el --- Black-box tests for fake pi harness -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise the Python fake-pi harness as a real subprocess over
;; stdin/stdout.  They intentionally avoid poking Python internals so the fake
;; stays accountable to the JSONL RPC contract.

;;; Code:

(require 'ert)
(require 'pilish)
(require 'pilish-jsonl)
(require 'pilish-test-common)
(require 'seq)

(defconst pilish-fake-pi-test--timeout 5
  "Timeout in seconds for fake-pi black-box tests.")

(defun pilish-fake-pi-test--process-filter (proc output)
  "Capture JSONL OUTPUT from fake-pi PROC."
  (let* ((partial (or (process-get proc 'fake-pi-partial) ""))
         (result (pilish--accumulate-lines partial output))
         (lines (car result))
         (objects (process-get proc 'fake-pi-objects))
         (invalid (process-get proc 'fake-pi-invalid-lines))
         (new-objects nil)
         (new-invalid nil))
    (process-put proc 'fake-pi-raw-output
                 (concat (or (process-get proc 'fake-pi-raw-output) "") output))
    (process-put proc 'fake-pi-partial (cdr result))
    (dolist (line lines)
      (if-let ((json (pilish--parse-json-line line)))
          (push json new-objects)
        (push line new-invalid)))
    (process-put proc 'fake-pi-objects (nconc objects (nreverse new-objects)))
    (process-put proc 'fake-pi-invalid-lines (nconc invalid (nreverse new-invalid)))))

(defun pilish-fake-pi-test--start-process (scenario &optional extra-args)
  "Start fake-pi for SCENARIO with optional EXTRA-ARGS."
  (let ((proc (make-process
               :name (format "fake-pi-test-%s" scenario)
               :command (append (pilish-test-fake-pi-executable)
                                (list "--mode" "rpc")
                                (pilish-test-fake-pi-extra-args scenario extra-args))
               :connection-type 'pipe
               :coding 'utf-8-unix
               :filter #'pilish-fake-pi-test--process-filter
               :noquery t)))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun pilish-fake-pi-test--stop-process (proc)
  "Stop fake-pi PROC gracefully, with a bounded forced-kill fallback."
  (when (processp proc)
    (set-process-query-on-exit-flag proc nil)
    (when (process-live-p proc)
      ;; EOF lets the harness run its finally block and remove its temporary
      ;; session root.  Force termination only if graceful shutdown wedges.
      (ignore-errors (process-send-eof proc))
      (unless (pilish-test-wait-until
               (lambda () (not (process-live-p proc))) 2 0.01 proc)
        (delete-process proc)))))

(defmacro pilish-fake-pi-test-with-process (spec &rest body)
  "Bind PROC to a fake-pi process for SPEC, run BODY, then clean up.
SPEC is (PROC SCENARIO &rest EXTRA-ARGS)."
  (declare (indent 1) (debug t))
  (let ((proc (nth 0 spec))
        (scenario (nth 1 spec))
        (extra-args (nthcdr 2 spec)))
    `(let ((,proc (pilish-fake-pi-test--start-process ,scenario (list ,@extra-args))))
       (unwind-protect
           (progn ,@body)
         (pilish-fake-pi-test--stop-process ,proc)))))

(defun pilish-fake-pi-test--send (proc command)
  "Send COMMAND plist to fake-pi PROC."
  (process-send-string proc (pilish--encode-command command)))

(defun pilish-fake-pi-test--pop-object (proc &optional timeout)
  "Pop the next parsed JSON object from PROC within TIMEOUT seconds."
  (unless (pilish-test-wait-until
           (lambda () (process-get proc 'fake-pi-objects))
           (or timeout pilish-fake-pi-test--timeout)
           0.01
           proc)
    (ert-fail
     (format "Timed out waiting for fake-pi output\nraw=%S\ninvalid=%S"
             (process-get proc 'fake-pi-raw-output)
             (process-get proc 'fake-pi-invalid-lines))))
  (let* ((objects (process-get proc 'fake-pi-objects))
         (next (car objects)))
    (process-put proc 'fake-pi-objects (cdr objects))
    next))

(defun pilish-fake-pi-test--collect-until (proc predicate &optional timeout)
  "Collect objects from PROC until PREDICATE returns non-nil for the latest one."
  (let* ((items nil)
         (limit (or timeout pilish-fake-pi-test--timeout))
         (deadline (+ (float-time) limit))
         done)
    (while (not done)
      (let* ((remaining (- deadline (float-time)))
             (item (pilish-fake-pi-test--pop-object
                    proc (max 0.0 remaining))))
        (push item items)
        (setq done (funcall predicate item))))
    (nreverse items)))

(ert-deftest pilish-fake-pi-test-collect-until-spends-one-timeout-budget ()
  "Repeated reads should spend one timeout budget instead of resetting it."
  (let ((timeouts nil)
        (items '((:type "message_start") (:type "agent_end"))))
    (cl-letf (((symbol-function 'pilish-fake-pi-test--pop-object)
               (lambda (_proc timeout)
                 (push timeout timeouts)
                 (sleep-for 0.01)
                 (pop items))))
      (pilish-fake-pi-test--collect-until
       :ignored
       (lambda (item) (equal (plist-get item :type) "agent_end"))
       1.0))
    (setq timeouts (nreverse timeouts))
    (should (= (length timeouts) 2))
    (should (> (car timeouts) (cadr timeouts)))))

(defun pilish-fake-pi-test--event-types (objects)
  "Return the :type fields from OBJECTS."
  (mapcar (lambda (obj) (plist-get obj :type)) objects))

(defun pilish-fake-pi-test--events-of-type (objects type)
  "Return events from OBJECTS whose top-level type is TYPE."
  (seq-filter (lambda (obj) (equal (plist-get obj :type) type)) objects))

(defun pilish-fake-pi-test--message-events (objects type role)
  "Return TYPE message events from OBJECTS whose message has ROLE."
  (seq-filter
   (lambda (obj)
     (and (equal (plist-get obj :type) type)
          (equal (plist-get (plist-get obj :message) :role) role)))
   objects))

(defun pilish-fake-pi-test--message-updates (objects type)
  "Return message updates from OBJECTS whose nested event has TYPE."
  (seq-filter
   (lambda (obj)
     (and (equal (plist-get obj :type) "message_update")
          (equal (plist-get (plist-get obj :assistantMessageEvent) :type) type)))
   objects))

(defun pilish-fake-pi-test--rpc (proc command &optional timeout)
  "Send COMMAND to PROC and return its correlated response within TIMEOUT."
  (pilish-fake-pi-test--send proc command)
  (let ((response (pilish-fake-pi-test--pop-object proc timeout)))
    (unless (and (equal (plist-get response :type) "response")
                 (equal (plist-get response :command)
                        (plist-get command :type)))
      (ert-fail (format "Unexpected fake-pi response for %S: %S"
                        command response)))
    (when (plist-member command :id)
      (unless (equal (plist-get response :id) (plist-get command :id))
        (ert-fail (format "Fake-pi response lost request id for %S: %S"
                          command response))))
    response))

(defun pilish-fake-pi-test--read-jsonl-file (path)
  "Parse every nonblank JSONL record in PATH into a vector of plists."
  (unless (file-readable-p path)
    (ert-fail (format "JSONL file is not readable: %S" path)))
  (with-temp-buffer
    (insert-file-contents path)
    (let ((line-number 0)
          (records nil))
      (goto-char (point-min))
      (while (not (eobp))
        (setq line-number (1+ line-number))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string-empty-p (string-trim line))
            (condition-case err
                (push (json-parse-string line
                                         :object-type 'plist
                                         :array-type 'array
                                         :null-object :null
                                         :false-object :false)
                      records)
              (error
               (ert-fail
                (format "Invalid JSONL at %s:%d: %s"
                        path line-number (error-message-string err)))))))
        (forward-line 1))
      (vconcat (nreverse records)))))

(defun pilish-fake-pi-test--file-bytes (path)
  "Return the literal bytes of the session file at PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun pilish-fake-pi-test--write-jsonl-file (path records)
  "Write RECORDS as strict JSONL to PATH."
  (with-temp-file path
    (dolist (record records)
      (insert (json-serialize record
                              :null-object :null
                              :false-object :false)
              "\n"))))

(defun pilish-fake-pi-test--canonical-json (value)
  "Return an order-insensitive canonical representation of JSON VALUE."
  (cond
   ((vectorp value)
    (cons :array
          (mapcar #'pilish-fake-pi-test--canonical-json
                  (append value nil))))
   ((and (consp value) (keywordp (car value)))
    (let ((cursor value)
          (pairs nil))
      (while cursor
        (unless (and (consp cursor) (consp (cdr cursor)))
          (ert-fail (format "Malformed JSON plist in test expectation: %S"
                            value)))
        (push (cons (car cursor)
                    (pilish-fake-pi-test--canonical-json
                     (cadr cursor)))
              pairs)
        (setq cursor (cddr cursor)))
      (cons :object
            (sort pairs
                  (lambda (a b)
                    (string< (symbol-name (car a))
                             (symbol-name (car b))))))))
   (t value)))

(defun pilish-fake-pi-test--json-equal-p (a b)
  "Return non-nil when JSON-shaped values A and B are semantically equal."
  (equal (pilish-fake-pi-test--canonical-json a)
         (pilish-fake-pi-test--canonical-json b)))

(defun pilish-fake-pi-test--assert-assistant-roundtrip (proc assistants)
  "Assert live ASSISTANTS match PROC's inspected and persisted payloads."
  (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_messages")))
         (state (pilish-fake-pi-test--rpc proc '(:type "get_state")))
         (records (pilish-fake-pi-test--read-jsonl-file
                   (plist-get (plist-get state :data) :sessionFile))))
    (should (eq (plist-get response :success) t))
    (should (eq (plist-get state :success) t))
    (dolist (messages (list (plist-get (plist-get response :data) :messages)
                           (seq-map (lambda (record) (plist-get record :message))
                                    records)))
      (should
       (pilish-fake-pi-test--json-equal-p
        (vconcat (seq-filter (lambda (message)
                              (equal (plist-get message :role) "assistant"))
                            messages))
        (vconcat assistants))))))

(defun pilish-fake-pi-test--iso-timestamp-p (value)
  "Return non-nil when VALUE is a strict UTC ISO timestamp."
  (and (stringp value)
       (string-match-p
        "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}Z\\'"
        value)
       (condition-case nil
           (progn (date-to-time value) t)
         (error nil))))

(defun pilish-fake-pi-test--iso-to-ms (timestamp)
  "Convert ISO TIMESTAMP to Unix milliseconds."
  (truncate (* 1000 (float-time (date-to-time timestamp)))))

(defun pilish-fake-pi-test--assert-v3-header (header)
  "Assert that HEADER has Pi's materialized v3 session shape."
  (should (equal (plist-get header :type) "session"))
  (should (numberp (plist-get header :version)))
  (should (= (plist-get header :version) 3))
  (should (stringp (plist-get header :id)))
  (should (pilish-fake-pi-test--iso-timestamp-p
           (plist-get header :timestamp)))
  (let ((cwd (plist-get header :cwd)))
    (should (stringp cwd))
    (should (file-name-absolute-p cwd))
    (should (file-directory-p cwd))))

(defun pilish-fake-pi-test--assert-entry-base (entry)
  "Assert that nonheader session ENTRY has the required v3 base fields."
  (should (stringp (plist-get entry :id)))
  (should (plist-member entry :parentId))
  (let ((parent-id (plist-get entry :parentId)))
    (should (or (stringp parent-id)
                (pilish--json-null-p parent-id))))
  (should (pilish-fake-pi-test--iso-timestamp-p
           (plist-get entry :timestamp))))

(defun pilish-fake-pi-test--entry-by-id (entries id)
  "Return the entry in vector ENTRIES whose id is ID."
  (seq-find (lambda (entry) (equal (plist-get entry :id) id)) entries))

(defun pilish-fake-pi-test--assert-valid-v3-records
    (header entries)
  "Assert that HEADER and append-ordered ENTRIES form a valid v3 file."
  (pilish-fake-pi-test--assert-v3-header header)
  (let ((seen (make-hash-table :test #'equal)))
    (dotimes (i (length entries))
      (let* ((entry (aref entries i))
             (id (plist-get entry :id)))
        (pilish-fake-pi-test--assert-entry-base entry)
        (should-not (gethash id seen))
        (puthash id t seen)))))

(defun pilish-fake-pi-test--tree-nodes (tree)
  "Return every node in TREE using iterative preorder traversal."
  (let ((pending (append tree nil))
        (nodes nil))
    (while pending
      (let* ((node (pop pending))
             (children (plist-get node :children)))
        (push node nodes)
        (setq pending (append (append children nil) pending))))
    (nreverse nodes)))

(defun pilish-fake-pi-test--zero-usage ()
  "Return a complete zero-valued Pi usage object."
  '(:input 0 :output 0 :cacheRead 0 :cacheWrite 0 :totalTokens 0
    :cost (:input 0 :output 0 :cacheRead 0 :cacheWrite 0 :total 0)))

(defun pilish-fake-pi-test--assistant-message (text timestamp)
  "Return a valid persisted assistant message containing TEXT at TIMESTAMP."
  (list :role "assistant"
        :content (vector (list :type "text" :text text))
        :api "fake-api"
        :provider "fake"
        :model "fake-model"
        :usage (pilish-fake-pi-test--zero-usage)
        :stopReason "stop"
        :timestamp timestamp))

(defun pilish-fake-pi-test--user-message (text timestamp)
  "Return a valid persisted user message containing TEXT at TIMESTAMP."
  (list :role "user"
        :content (vector (list :type "text" :text text))
        :timestamp timestamp))

(defun pilish-fake-pi-test--write-branched-v3-session (directory)
  "Write the Phase 5 branched v3 target under DIRECTORY and describe it."
  (let* ((path (expand-file-name "phase5-branched-target.jsonl" directory))
         (cwd (directory-file-name (expand-file-name directory)))
         (session-id "11111111-2222-4333-8444-555555555555")
         (root-user-id "10000001")
         (root-assistant-id "10000002")
         (active-user-id "10000003")
         (abandoned-user-id "10000004")
         (abandoned-assistant-id "10000005")
         (branch-summary-id "10000006")
         (custom-message-id "10000007")
         (old-label-id "10000008")
         (latest-label-id "10000009")
         (session-info-id "1000000a")
         (compaction-id "1000000b")
         (post-assistant-id "1000000c")
         (orphan-id "20000001")
         (thinking-level-id "20000002")
         (clear-label-set-id "20000003")
         (clear-label-id "20000004")
         (branch-summary "BRANCH SUMMARY: abandoned experiment recorded.")
         (custom-content "CUSTOM ACTIVE CONTEXT")
         (compaction-summary "COMPACTION SUMMARY: root exchange condensed.")
         (session-name "Phase 5 Branched Target")
         (latest-label "Latest active checkpoint")
         (header
          (list :type "session" :version 3 :id session-id
                :timestamp "2026-02-03T04:05:00.000Z" :cwd cwd))
         ;; The active sibling is physically first but has the later timestamp.
         ;; get_tree must therefore place the abandoned sibling first.
         (entries
          (vector
           (list :type "message" :id root-user-id :parentId :null
                 :timestamp "2026-02-03T04:05:01.000Z"
                 :message (pilish-fake-pi-test--user-message
                           "ROOT USER CONTENT" 1770091501000))
           (list :type "message" :id root-assistant-id
                 :parentId root-user-id
                 :timestamp "2026-02-03T04:05:02.000Z"
                 :message (pilish-fake-pi-test--assistant-message
                           "ROOT ASSISTANT CONTENT" 1770091502000))
           (list :type "message" :id active-user-id
                 :parentId root-assistant-id
                 :timestamp "2026-02-03T04:05:06.000Z"
                 :message (pilish-fake-pi-test--user-message
                           "ACTIVE RETAINED USER CONTENT" 1770091506000))
           (list :type "message" :id abandoned-user-id
                 :parentId root-assistant-id
                 :timestamp "2026-02-03T04:05:04.000Z"
                 :message (pilish-fake-pi-test--user-message
                           "ABANDONED USER CONTENT" 1770091504000))
           (list :type "message" :id abandoned-assistant-id
                 :parentId abandoned-user-id
                 :timestamp "2026-02-03T04:05:05.000Z"
                 :message (pilish-fake-pi-test--assistant-message
                           "ABANDONED ASSISTANT CONTENT" 1770091505000))
           (list :type "custom" :id orphan-id
                 :parentId "missing-parent"
                 :timestamp "2026-02-03T04:05:03.500Z"
                 :customType "orphan-bookkeeping")
           ;; A real Pi 0.84.2 resume appends the current thinking level when
           ;; the active branch has none.  Include one so the target remains
           ;; byte-for-byte stable under both Pi and the fake.
           (list :type "thinking_level_change" :id thinking-level-id
                 :parentId active-user-id
                 :timestamp "2026-02-03T04:05:06.500Z"
                 :thinkingLevel "off")
           (list :type "branch_summary" :id branch-summary-id
                 :parentId thinking-level-id
                 :timestamp "2026-02-03T04:05:07.000Z"
                 :fromId abandoned-assistant-id :summary branch-summary)
           (list :type "custom_message" :id custom-message-id
                 :parentId branch-summary-id
                 :timestamp "2026-02-03T04:05:08.000Z"
                 :customType "phase-five-test" :content custom-content
                 :display t :details '(:origin "fake-rpc-red"))
           (list :type "label" :id old-label-id
                 :parentId custom-message-id
                 :timestamp "2026-02-03T04:05:09.000Z"
                 :targetId active-user-id :label "Earlier checkpoint")
           (list :type "label" :id latest-label-id
                 :parentId old-label-id
                 :timestamp "2026-02-03T04:05:10.000Z"
                 :targetId active-user-id :label latest-label)
           (list :type "label" :id clear-label-set-id
                 :parentId latest-label-id
                 :timestamp "2026-02-03T04:05:10.250Z"
                 :targetId root-assistant-id :label "Temporary root label")
           (list :type "label" :id clear-label-id
                 :parentId clear-label-set-id
                 :timestamp "2026-02-03T04:05:10.500Z"
                 :targetId root-assistant-id)
           (list :type "session_info" :id session-info-id
                 :parentId clear-label-id
                 :timestamp "2026-02-03T04:05:11.000Z"
                 :name session-name)
           (list :type "compaction" :id compaction-id
                 :parentId session-info-id
                 :timestamp "2026-02-03T04:05:12.000Z"
                 :summary compaction-summary
                 :firstKeptEntryId active-user-id :tokensBefore 4321)
           (list :type "message" :id post-assistant-id
                 :parentId compaction-id
                 :timestamp "2026-02-03T04:05:13.000Z"
                 :message (pilish-fake-pi-test--assistant-message
                           "POST-COMPACTION ASSISTANT CONTENT"
                           1770091513000)))))
    (pilish-fake-pi-test--write-jsonl-file
     path (cons header (append entries nil)))
    ;; Parse the actual bytes back so raw-entry expectations use precisely the
    ;; same JSON dialect as subprocess responses.
    (let* ((records (pilish-fake-pi-test--read-jsonl-file path))
           (parsed-entries (seq-subseq records 1))
           (active-entry (pilish-fake-pi-test--entry-by-id
                          parsed-entries active-user-id))
           (post-entry (pilish-fake-pi-test--entry-by-id
                        parsed-entries post-assistant-id))
           (expected-messages
            (vector
             (list :role "compactionSummary" :summary compaction-summary
                   :tokensBefore 4321
                   :timestamp
                   (pilish-fake-pi-test--iso-to-ms
                    "2026-02-03T04:05:12.000Z"))
             (plist-get active-entry :message)
             (list :role "branchSummary" :summary branch-summary
                   :fromId abandoned-assistant-id
                   :timestamp
                   (pilish-fake-pi-test--iso-to-ms
                    "2026-02-03T04:05:07.000Z"))
             (list :role "custom" :customType "phase-five-test"
                   :content custom-content :display t
                   :details '(:origin "fake-rpc-red")
                   :timestamp
                   (pilish-fake-pi-test--iso-to-ms
                    "2026-02-03T04:05:08.000Z"))
             (plist-get post-entry :message))))
      (pilish-fake-pi-test--assert-valid-v3-records
       (aref records 0) parsed-entries)
      (list :path path
            :entries parsed-entries
            :expected-messages expected-messages
            :session-id session-id
            :session-name session-name
            :branch-summary branch-summary
            :latest-label latest-label
            :latest-label-timestamp "2026-02-03T04:05:10.000Z"
            :root-user-id root-user-id
            :root-assistant-id root-assistant-id
            :active-user-id active-user-id
            :abandoned-user-id abandoned-user-id
            :abandoned-assistant-id abandoned-assistant-id
            :orphan-id orphan-id
            :post-assistant-id post-assistant-id))))

(defun pilish-fake-pi-test--logged-input-types-after-switch (records)
  "Return input command types in log RECORDS from the first switch onward."
  (let ((started nil)
        (types nil))
    (dotimes (i (length records))
      (let* ((record (aref records i))
             (payload (plist-get record :payload))
             (type (plist-get payload :type)))
        (when (and (equal (plist-get record :direction) "in")
                   (stringp type))
          (when (equal type "switch_session")
            (setq started t))
          (when started
            (push type types)))))
    (nreverse types)))

(defun pilish-fake-pi-test--wait-or-fail
    (proc predicate description &optional timeout)
  "Wait for PREDICATE with PROC, or fail clearly with DESCRIPTION."
  (unless (pilish-test-wait-until
           predicate
           (or timeout pilish-fake-pi-test--timeout)
           0.01
           proc)
    (ert-fail (format "Timed out waiting for %s (process status: %S)"
                      description (and (processp proc) (process-status proc))))))

(defun pilish-fake-pi-test--run-cli (&rest args)
  "Run fake-pi with ARGS and return `(:exit-code N :output STRING)'."
  (let ((command
         (concat
          (mapconcat #'shell-quote-argument
                     (append (pilish-test-fake-pi-executable) args)
                     " ")
          " 2>&1")))
    (with-temp-buffer
      (list :exit-code (call-process-shell-command command nil (current-buffer) nil)
            :output (buffer-string)))))

(defmacro pilish-fake-pi-test-with-session (spec &rest body)
  "Create a real Pilish session against fake-pi, then run BODY.
SPEC is (SESSION SCENARIO &rest EXTRA-ARGS)."
  (declare (indent 1) (debug t))
  (let ((session (nth 0 spec))
        (scenario (nth 1 spec))
        (extra-args (nthcdr 2 spec)))
    `(let* ((default-directory "/tmp/")
            (pilish-executable
             (pilish-test-fake-pi-executable))
            (pilish-extra-args
             (pilish-test-fake-pi-extra-args ,scenario (list ,@extra-args)))
            (,session nil))
       (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                 ((symbol-function 'pilish--display-buffers) #'ignore))
         (unwind-protect
             (progn
               (pilish)
               (let ((chat-name (pilish-test--chat-buffer-name default-directory)))
                 (should
                  (pilish-test-wait-until
                   (lambda ()
                     (let* ((chat-buf (get-buffer chat-name))
                            (input-buf (and chat-buf
                                            (with-current-buffer chat-buf
                                              pilish--input-buffer)))
                            (proc (and chat-buf
                                       (with-current-buffer chat-buf
                                         pilish--process))))
                       (and (buffer-live-p chat-buf)
                            (buffer-live-p input-buf)
                            (process-live-p proc))))
                   pilish-fake-pi-test--timeout
                   0.01))
                 (let* ((chat-buf (get-buffer chat-name))
                        (input-buf (with-current-buffer chat-buf
                                     pilish--input-buffer))
                        (proc (with-current-buffer chat-buf
                                pilish--process)))
                   (setq ,session (list :chat-buffer chat-buf
                                        :input-buffer input-buf
                                        :process proc))
                   ,@body)))
           (let* ((original-chat
                   (get-buffer
                    (pilish-test--chat-buffer-name default-directory)))
                  (chat-buf (or (plist-get ,session :chat-buffer)
                                original-chat))
                  (input-buf (plist-get ,session :input-buffer))
                  (proc (or (plist-get ,session :process)
                            (and (buffer-live-p chat-buf)
                                 (buffer-local-value
                                  'pilish--process chat-buf)))))
             (pilish-fake-pi-test--stop-process proc)
             ;; A resume can rename and retarget both buffers, so clean up the
             ;; captured objects rather than relying only on their startup names.
             (pilish-test--kill-live-buffers input-buf chat-buf)
             (when (and (buffer-live-p original-chat)
                        (not (eq original-chat chat-buf)))
               (pilish-test--kill-live-buffers original-chat)))
           (pilish-test--kill-session-buffers default-directory))))))

(ert-deftest pilish-fake-pi-test-get-state-handles-split-jsonl-record ()
  "get_state survives a deliberately split JSONL response."
  (pilish-fake-pi-test-with-process
      (proc "prompt-lifecycle" "--split-response" "get_state:24")
    (pilish-fake-pi-test--send proc '(:type "get_state"))
    (let* ((response (pilish-fake-pi-test--pop-object proc))
           (data (plist-get response :data))
           (session-file (plist-get data :sessionFile)))
      (should (equal (plist-get response :type) "response"))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "get_state"))
      (should (stringp session-file))
      (should-not (file-exists-p session-file)))))

(ert-deftest pilish-fake-pi-test-requires-newline-before-eof ()
  "EOF alone must not act as an implicit JSONL record delimiter."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (process-send-string proc "{\"type\":\"get_state\"}")
    (process-send-eof proc)
    (should
     (pilish-test-wait-until
      (lambda () (not (process-live-p proc)))
      pilish-fake-pi-test--timeout
      0.01))
    (should-not (process-get proc 'fake-pi-objects))))

(ert-deftest pilish-fake-pi-test-cli-rejects-unsupported-mode ()
  "The fake should fail fast when asked to run in an unsupported mode."
  (let* ((result (pilish-fake-pi-test--run-cli
                  "--mode" "interactive"
                  "--scenario" "prompt-lifecycle"))
         (output (plist-get result :output)))
    (should-not (eq (plist-get result :exit-code) 0))
    (should (string-match-p "invalid choice" output))
    (should (string-match-p "interactive" output))))

(ert-deftest pilish-fake-pi-test-cli-reports-missing-scenario-cleanly ()
  "The fake should name a missing scenario instead of showing a traceback."
  (let* ((result (pilish-fake-pi-test--run-cli "--scenario" "does-not-exist"))
         (output (plist-get result :output)))
    (should-not (eq (plist-get result :exit-code) 0))
    (should (string-match-p "scenario not found: does-not-exist" output))
    (should-not (string-match-p "Traceback" output))))

(ert-deftest pilish-fake-pi-test-cleans-session-root-on-exit ()
  "The fake removes its temporary session directory when the process exits."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send proc '(:type "get_state"))
    (let* ((response (pilish-fake-pi-test--pop-object proc))
           (session-file (plist-get (plist-get response :data) :sessionFile))
           (session-root (directory-file-name (file-name-directory session-file))))
      (should (file-directory-p session-root))
      (process-send-eof proc)
      (should
       (pilish-test-wait-until
        (lambda () (not (process-live-p proc)))
        pilish-fake-pi-test--timeout
        0.01))
      (should-not (file-exists-p session-root)))))

(ert-deftest pilish-fake-pi-test-get-commands-returns-configured-commands ()
  "get_commands returns the scenario's slash-command list." 
  (pilish-fake-pi-test-with-process (proc "extension-confirm")
    (pilish-fake-pi-test--send proc '(:type "get_commands"))
    (let* ((response (pilish-fake-pi-test--pop-object proc))
           (commands (plist-get (plist-get response :data) :commands))
           (first (aref commands 0)))
      (should (eq (plist-get response :success) t))
      (should (vectorp commands))
      (should (equal (plist-get first :name) "test-confirm"))
      (should (equal (plist-get first :source) "extension")))))

(ert-deftest pilish-fake-pi-test-1-0-command-and-thinking-metadata ()
  "Discovery preserves builtin sourceInfo; assistant thinking survives all views."
  (pilish-fake-pi-test-with-process (proc "input-dispositions")
    (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_commands")))
           (mcp (seq-find
                 (lambda (command) (equal (plist-get command :name) "mcp"))
                 (plist-get (plist-get response :data) :commands)))
           (source-info (plist-get mcp :sourceInfo)))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get mcp :source) "extension"))
      (should (equal (plist-get source-info :path) "builtin:mcp"))
      (should (equal (plist-get source-info :origin) "top-level"))
      (should (equal (plist-get source-info :source) "builtin"))
      (should (equal (plist-get source-info :scope) "temporary")))
    (should (eq (plist-get (pilish-fake-pi-test--rpc
                           proc '(:type "set_thinking_level" :level "high"))
                          :success)
                t))
    (should (eq (plist-get (pilish-fake-pi-test--rpc
                           proc '(:type "prompt" :message "thinking metadata"))
                          :success)
                t))
    (let* ((events (pilish-fake-pi-test--collect-until
                    proc (lambda (object)
                           (equal (plist-get object :type) "agent_settled"))))
           (ends (pilish-fake-pi-test--message-events
                  events "message_end" "assistant"))
           (assistant (plist-get (car ends) :message))
           (agent-end (car (pilish-fake-pi-test--events-of-type
                            events "agent_end"))))
      (should (= (length ends) 1))
      (should (equal (plist-get assistant :stopReason) "stop"))
      (should (equal (plist-get assistant :thinkingLevel) "high"))
      (should (equal (plist-get agent-end :messages) (vector assistant)))
      (pilish-fake-pi-test--assert-assistant-roundtrip proc (list assistant)))))

(ert-deftest pilish-fake-pi-test-set-model-and-thinking-level-update-state ()
  "set_model and set_thinking_level change subsequent get_state responses."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send
     proc '(:type "set_model" :provider "fake-provider" :modelId "fake-large"))
    (let ((model-response (pilish-fake-pi-test--pop-object proc)))
      (should (eq (plist-get model-response :success) t))
      (should (equal (plist-get (plist-get model-response :data) :id) "fake-large")))
    (pilish-fake-pi-test--send proc '(:type "set_thinking_level" :level "high"))
    (should (eq (plist-get (pilish-fake-pi-test--pop-object proc) :success) t))
    (pilish-fake-pi-test--send proc '(:type "get_state"))
    (let* ((state (pilish-fake-pi-test--pop-object proc))
           (data (plist-get state :data))
           (model (plist-get data :model)))
      (should (equal (plist-get model :provider) "fake-provider"))
      (should (equal (plist-get model :id) "fake-large"))
      (should (equal (plist-get data :thinkingLevel) "high")))))

(ert-deftest pilish-fake-pi-test-first-user-materializes-session ()
  "The first user flushes the header and buffered name before any assistant ends."
  (let ((scenario-dir (make-temp-file "pilish-fake-pi-first-user-" t)))
    (unwind-protect
        (progn
          (pilish-fake-pi-test--write-jsonl-file
           (expand-file-name "first-user.json" scenario-dir)
           '((:description "First-user persistence without a completed assistant"
              :commands []
              :prompt (:type "text_stream" :assistant_text "Not completed"
                       :chunk_count 2 :delay_ms 10000 :echo_user t))))
          (pilish-fake-pi-test-with-process
              (proc "first-user" "--scenario-dir" scenario-dir)
            (let* ((state (pilish-fake-pi-test--rpc proc '(:type "get_state")))
                   (data (plist-get state :data))
                   (session-file (plist-get data :sessionFile)))
              (should (stringp session-file))
              (should-not (file-exists-p session-file))
              (should (eq (plist-get (pilish-fake-pi-test--rpc
                                     proc '(:type "set_session_name"
                                            :name "Named before conversation"))
                                    :success)
                          t))
              (let ((named (plist-get (pilish-fake-pi-test--rpc
                                      proc '(:type "get_state"))
                                     :data)))
                (should (equal (plist-get named :sessionFile) session-file))
                (should (equal (plist-get named :sessionName)
                               "Named before conversation"))
                (should-not (file-exists-p session-file)))
              (should (eq (plist-get (pilish-fake-pi-test--rpc
                                     proc '(:type "prompt" :message "Keep this user"))
                                    :success)
                          t))
              (let* ((events (pilish-fake-pi-test--collect-until
                              proc (lambda (event)
                                     (and (equal (plist-get event :type) "message_end")
                                          (equal (plist-get (plist-get event :message)
                                                            :role)
                                                 "user")))))
                     (records (pilish-fake-pi-test--read-jsonl-file session-file))
                     (entries (seq-subseq records 1)))
                (should-not (pilish-fake-pi-test--message-events
                             events "message_end" "assistant"))
                (should (= (length records) 3))
                (pilish-fake-pi-test--assert-valid-v3-records
                 (aref records 0) entries)
                (should (equal (plist-get (aref records 0) :id)
                               (plist-get data :sessionId)))
                (should (equal (mapcar (lambda (entry) (plist-get entry :type))
                                       (append entries nil))
                               '("session_info" "message")))
                (should (equal (plist-get (aref entries 0) :name)
                               "Named before conversation"))
                (should (equal (plist-get (aref entries 1) :parentId)
                               (plist-get (aref entries 0) :id)))
                (should (equal (plist-get (plist-get (aref entries 1) :message) :role)
                               "user"))
                (should (equal (plist-get (plist-get (aref entries 1) :message) :content)
                               [(:type "text" :text "Keep this user")]))
                (pilish-fake-pi-test--send
                 proc '(:id "abort-first-user" :type "abort"))
                (let* ((abort-events
                        (pilish-fake-pi-test--collect-until
                         proc (lambda (event)
                                (equal (plist-get event :id) "abort-first-user"))))
                       (response (car (last abort-events))))
                  (should (eq (plist-get response :success) t))
                  (should-not (pilish-fake-pi-test--message-events
                               abort-events "message_end" "assistant")))
                (should (equal (pilish-fake-pi-test--read-jsonl-file session-file)
                               records))))))
      (delete-directory scenario-dir t))))

(ert-deftest pilish-fake-pi-test-setup-only-does-not-materialize-session ()
  "Naming and custom-only messages update memory without creating a session file."
  (pilish-fake-pi-test-with-process (proc "extension-message")
    (let* ((state (pilish-fake-pi-test--rpc proc '(:type "get_state")))
           (session-file (plist-get (plist-get state :data) :sessionFile)))
      (should (stringp session-file))
      (should-not (file-exists-p session-file))
      (should (eq (plist-get (pilish-fake-pi-test--rpc
                             proc '(:type "set_session_name" :name "Setup only"))
                            :success)
                  t))
      (let ((named (plist-get (pilish-fake-pi-test--rpc proc '(:type "get_state"))
                             :data)))
        (should (equal (plist-get named :sessionName) "Setup only"))
        (should (= (plist-get named :messageCount) 0))
        (should-not (file-exists-p session-file)))
      (pilish-fake-pi-test--send
       proc '(:id "custom-only" :type "prompt" :message "/test-message"))
      (let ((events (pilish-fake-pi-test--collect-until
                     proc (lambda (event)
                            (equal (plist-get event :id) "custom-only")))))
        (should (eq (plist-get (car (last events)) :success) t)))
      (let* ((after (plist-get (pilish-fake-pi-test--rpc proc '(:type "get_state"))
                              :data))
             (entries-response (pilish-fake-pi-test--rpc proc '(:type "get_entries")))
             (entries (plist-get (plist-get entries-response :data) :entries)))
        (should (equal (plist-get after :sessionFile) session-file))
        (should (equal (plist-get after :sessionName) "Setup only"))
        (should (= (plist-get after :messageCount) 1))
        (should (equal (mapcar (lambda (entry) (plist-get entry :type))
                               (append entries nil))
                       '("session_info" "custom_message")))
        (should (equal (plist-get (aref entries 1) :content)
                       "Test message from extension"))
        (should-not (file-exists-p session-file))))))

(ert-deftest pilish-fake-pi-test-generated-session-is-valid-v3-with-entry-rpcs ()
  "A normal fake prompt persists valid v3 entries and exposes their raw IDs."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (let ((prompt "phase five generated session"))
      (let ((response
             (pilish-fake-pi-test--rpc
              proc (list :id "generated-prompt" :type "prompt"
                         :message prompt))))
        (should (eq (plist-get response :success) t)))
      (pilish-fake-pi-test--collect-until
       proc (lambda (object) (equal (plist-get object :type) "agent_settled")))
      (let* ((state-response
              (pilish-fake-pi-test--rpc
               proc '(:id "generated-state" :type "get_state")))
             (state (plist-get state-response :data))
             (session-file (plist-get state :sessionFile))
             (records
              (pilish-fake-pi-test--read-jsonl-file session-file))
             (header (and (> (length records) 0) (aref records 0)))
             (entries (seq-subseq records 1))
             (fork-response
              (pilish-fake-pi-test--rpc
               proc '(:id "generated-forks" :type "get_fork_messages")))
             (entries-response
              (pilish-fake-pi-test--rpc
               proc '(:id "generated-entries" :type "get_entries"))))
        (should (eq (plist-get state-response :success) t))
        (should (file-exists-p session-file))
        (should (= (length entries) 2))
        (pilish-fake-pi-test--assert-v3-header header)
        (should (equal (plist-get header :id) (plist-get state :sessionId)))
        (let ((previous-id nil))
          (dotimes (i (length entries))
            (let* ((entry (aref entries i))
                   (parent-id (plist-get entry :parentId)))
              (pilish-fake-pi-test--assert-entry-base entry)
              (if (= i 0)
                  (should (pilish--json-null-p parent-id))
                (should (equal parent-id previous-id)))
              (setq previous-id (plist-get entry :id)))))
        (let* ((user-entry
                (seq-find
                 (lambda (entry)
                   (and (equal (plist-get entry :type) "message")
                        (equal (plist-get (plist-get entry :message) :role)
                               "user")))
                 entries))
               (fork-messages
                (plist-get (plist-get fork-response :data) :messages)))
          (should user-entry)
          (should (eq (plist-get fork-response :success) t))
          (should
           (pilish-fake-pi-test--json-equal-p
            fork-messages
            (vector (list :entryId (plist-get user-entry :id)
                          :text prompt)))))
        (should (eq (plist-get entries-response :success) t))
        (let ((data (plist-get entries-response :data)))
          (should (equal (plist-get data :entries) entries))
          (should (equal (plist-get data :leafId)
                         (plist-get (aref entries (1- (length entries)))
                                    :id))))))))

(ert-deftest pilish-fake-pi-test-switch-session-roundtrips-branched-v3 ()
  "A switched branched v3 file preserves raw tree and projected history contracts."
  (let ((target-dir
         (file-name-as-directory
          (make-temp-file "pilish-fake-pi-branched-" t))))
    (unwind-protect
        (let* ((fixture
                (pilish-fake-pi-test--write-branched-v3-session
                 target-dir))
               (path (plist-get fixture :path))
               (entries (plist-get fixture :entries))
               (active-id (plist-get fixture :active-user-id))
               (leaf-id (plist-get fixture :post-assistant-id)))
          (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
            (should (eq (plist-get (pilish-fake-pi-test--rpc
                                   proc '(:type "set_thinking_level" :level "high"))
                                  :success)
                        t))
            (let ((switch-response
                   (pilish-fake-pi-test--rpc
                    proc (list :id "branched-switch"
                               :type "switch_session"
                               :sessionPath path))))
              (should
               (pilish-fake-pi-test--json-equal-p
                switch-response
                '(:id "branched-switch" :type "response"
                  :command "switch_session" :success t
                  :data (:cancelled :false)))))
            (let* ((state-response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "branched-state" :type "get_state")))
                   (state (plist-get state-response :data)))
              (should (eq (plist-get state-response :success) t))
              (should (equal (plist-get state :sessionFile) path))
              (should (equal (plist-get state :sessionId)
                             (plist-get fixture :session-id)))
              (should (equal (plist-get state :sessionName)
                             (plist-get fixture :session-name)))
              (should (= (plist-get state :messageCount)
                         (length (plist-get fixture :expected-messages)))))
            (let* ((response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "branched-entries" :type "get_entries")))
                   (data (plist-get response :data)))
              (should (eq (plist-get response :success) t))
              (should (equal (plist-get data :entries) entries))
              (should (equal (plist-get data :leafId) leaf-id)))
            (let* ((response
                    (pilish-fake-pi-test--rpc
                     proc (list :id "branched-since" :type "get_entries"
                                :since active-id)))
                   (data (plist-get response :data)))
              (should (eq (plist-get response :success) t))
              ;; ACTIVE-ID is the third physical entry.  The abandoned sibling
              ;; follows it on disk even though it sorts above it in the tree.
              (should (equal (plist-get data :entries)
                             (seq-subseq entries 3)))
              (should (equal (plist-get data :leafId) leaf-id)))
            (let* ((response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "branched-tree" :type "get_tree")))
                   (data (plist-get response :data))
                   (tree (plist-get data :tree))
                   (nodes (pilish-fake-pi-test--tree-nodes tree)))
              (should (eq (plist-get response :success) t))
              (should (equal (plist-get data :leafId) leaf-id))
              (should (= (length tree) 2))
              (should
               (equal
                (mapcar (lambda (root)
                          (plist-get (plist-get root :entry) :id))
                        (append tree nil))
                (list (plist-get fixture :root-user-id)
                      (plist-get fixture :orphan-id))))
              (should (= (length nodes) (length entries)))
              ;; Every raw bookkeeping entry remains a first-class tree node.
              (dotimes (i (length entries))
                (let* ((entry (aref entries i))
                       (node
                        (seq-find
                         (lambda (candidate)
                           (equal (plist-get
                                   (plist-get candidate :entry) :id)
                                  (plist-get entry :id)))
                         nodes)))
                  (should node)
                  (should
                   (pilish-fake-pi-test--json-equal-p
                    (plist-get node :entry) entry))))
              (dolist (type '("branch_summary" "custom" "custom_message"
                              "thinking_level_change" "label" "session_info"
                              "compaction"))
                (should
                 (seq-find
                  (lambda (node)
                    (equal (plist-get (plist-get node :entry) :type) type))
                  nodes)))
              (should
               (= 4
                  (length
                   (seq-filter
                    (lambda (node)
                      (equal (plist-get (plist-get node :entry) :type)
                             "label"))
                    nodes))))
              (let* ((root-assistant-node
                      (seq-find
                       (lambda (node)
                         (equal (plist-get (plist-get node :entry) :id)
                                (plist-get fixture :root-assistant-id)))
                       nodes))
                     (child-ids
                      (mapcar
                       (lambda (node)
                         (plist-get (plist-get node :entry) :id))
                       (append (plist-get root-assistant-node :children) nil))))
                (should
                 (equal child-ids
                        (list (plist-get fixture :abandoned-user-id)
                              active-id))))
              (let ((active-node
                     (seq-find
                      (lambda (node)
                        (equal (plist-get (plist-get node :entry) :id)
                               active-id))
                      nodes)))
                (should (equal (plist-get active-node :label)
                               (plist-get fixture :latest-label)))
                (should (equal (plist-get active-node :labelTimestamp)
                               (plist-get fixture
                                          :latest-label-timestamp))))
              (let ((cleared-node
                     (seq-find
                      (lambda (node)
                        (equal (plist-get (plist-get node :entry) :id)
                               (plist-get fixture :root-assistant-id)))
                      nodes)))
                (should-not (plist-member cleared-node :label))
                (should-not (plist-member cleared-node :labelTimestamp))))
            (let* ((response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "branched-messages" :type "get_messages")))
                   (messages
                    (plist-get (plist-get response :data) :messages)))
              (should (eq (plist-get response :success) t))
              (should
               (equal (mapcar (lambda (message) (plist-get message :role))
                              (append messages nil))
                      '("compactionSummary" "user" "branchSummary"
                        "custom" "assistant")))
              (should
               (pilish-fake-pi-test--json-equal-p
                messages (plist-get fixture :expected-messages)))
              ;; Older disk assistants do not inherit the current level.
              (should-not (plist-member (aref messages 4) :thinkingLevel))
              (let ((printed (prin1-to-string messages)))
                (should-not (string-match-p "ABANDONED USER CONTENT" printed))
                (should-not
                 (string-match-p "ABANDONED ASSISTANT CONTENT" printed))))
            (let* ((response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "branched-forks"
                            :type "get_fork_messages")))
                   (messages
                    (plist-get (plist-get response :data) :messages)))
              (should (eq (plist-get response :success) t))
              (should
               (pilish-fake-pi-test--json-equal-p
                messages
                (vector
                 (list :entryId (plist-get fixture :root-user-id)
                       :text "ROOT USER CONTENT")
                 (list :entryId active-id
                       :text "ACTIVE RETAINED USER CONTENT")
                 (list :entryId (plist-get fixture :abandoned-user-id)
                       :text "ABANDONED USER CONTENT")))))
            (let ((response
                   (pilish-fake-pi-test--rpc
                    proc '(:id "rename-after-switch"
                           :type "set_session_name"
                           :name "  Renamed\r\nafter switch  "))))
              (should (eq (plist-get response :success) t)))
            (let* ((response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "entries-after-rename" :type "get_entries")))
                   (data (plist-get response :data))
                   (renamed-entries (plist-get data :entries))
                   (name-entry (aref renamed-entries
                                     (1- (length renamed-entries)))))
              (should (= (length renamed-entries) (1+ (length entries))))
              (should (equal (plist-get name-entry :type) "session_info"))
              (should (equal (plist-get name-entry :parentId) leaf-id))
              (should (equal (plist-get name-entry :name)
                             "Renamed after switch"))
              (should (equal (plist-get data :leafId)
                             (plist-get name-entry :id))))
            (let* ((state-response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "state-after-rename" :type "get_state")))
                   (state (plist-get state-response :data)))
              (should (equal (plist-get state :sessionName)
                             "Renamed after switch"))
              ;; session_info advances the raw leaf but projects no message.
              (should (= (plist-get state :messageCount)
                         (length (plist-get fixture :expected-messages)))))))
      (delete-directory target-dir t))))

(ert-deftest pilish-fake-pi-test-switch-failures-are-transactional-and-missing-initializes ()
  "Invalid switches preserve state; a missing absolute target becomes empty v3."
  (let* ((target-dir
          (file-name-as-directory
           (make-temp-file "pilish-fake-pi-switch-" t)))
         (malformed-path (expand-file-name "malformed.jsonl" target-dir))
         (missing-path (expand-file-name "new-empty.jsonl" target-dir)))
    (unwind-protect
        (progn
          (with-temp-file malformed-path
            (insert "this is not a pi session\n"))
          (pilish-fake-pi-test-with-process
              (proc "extension-confirm" "--extension-timeout-ms" "0")
            (pilish-fake-pi-test--send
             proc '(:id "blocking-prompt" :type "prompt"
                    :message "/test-confirm"))
            (should (equal (plist-get
                            (pilish-fake-pi-test--pop-object proc)
                            :type)
                           "extension_ui_request"))
            (let* ((baseline-response
                    (pilish-fake-pi-test--rpc
                     proc '(:id "baseline-state" :type "get_state")))
                   (baseline (plist-get baseline-response :data))
                   (invalid-targets
                    (list (cons "non-string" 17)
                          (cons "directory" target-dir)
                          (cons "malformed" malformed-path))))
              (should (eq (plist-get baseline-response :success) t))
              (should (eq (plist-get baseline :isStreaming) :false))
              (dolist (case invalid-targets)
                (let* ((label (car case))
                       (response
                        (pilish-fake-pi-test--rpc
                         proc (list :id (concat "invalid-" label)
                                    :type "switch_session"
                                    :sessionPath (cdr case)))))
                  (should (pilish--json-false-p
                           (plist-get response :success)))
                  (should (stringp (plist-get response :error)))
                  (should-not (plist-member response :data))
                  (let ((after
                         (pilish-fake-pi-test--rpc
                          proc (list :id (concat "state-after-" label)
                                     :type "get_state"))))
                    (should (eq (plist-get after :success) t))
                    ;; Validation failure neither retargets nor stops the
                    ;; extension worker waiting for its response.
                    (should
                     (pilish-fake-pi-test--json-equal-p
                      (plist-get after :data) baseline)))))
              (should-not (file-exists-p missing-path))
              (pilish-fake-pi-test--send
               proc (list :id "missing-switch"
                          :type "switch_session"
                          :sessionPath missing-path))
              (let* ((events
                      (pilish-fake-pi-test--collect-until
                       proc
                       (lambda (event)
                         (and (equal (plist-get event :type) "response")
                              (equal (plist-get event :id) "missing-switch")))))
                     (response (car (last events)))
                     (prompt-ack
                      (seq-find (lambda (event)
                                  (and (equal (plist-get event :type) "response")
                                       (equal (plist-get event :id) "blocking-prompt")))
                                events)))
                (should (equal (plist-get (plist-get prompt-ack :data) :disposition)
                               "handled"))
                (should (< (seq-position events prompt-ack #'eq)
                           (seq-position events response #'eq)))
                (should-not (seq-intersection
                             (pilish-fake-pi-test--event-types events)
                             '("agent_start" "agent_end" "agent_settled") #'equal))
                (should
                 (pilish-fake-pi-test--json-equal-p
                  response
                  '(:id "missing-switch" :type "response"
                    :command "switch_session" :success t
                    :data (:cancelled :false)))))
              (should (file-exists-p missing-path))
              (let* ((records
                      (pilish-fake-pi-test--read-jsonl-file
                       missing-path))
                     (header (and (= (length records) 1)
                                  (aref records 0)))
                     (state-response
                      (pilish-fake-pi-test--rpc
                       proc '(:id "missing-state" :type "get_state")))
                     (state (plist-get state-response :data)))
                (should (= (length records) 1))
                (pilish-fake-pi-test--assert-v3-header header)
                (should (eq (plist-get state-response :success) t))
                (should (equal (plist-get state :sessionFile) missing-path))
                (should (equal (plist-get state :sessionId)
                               (plist-get header :id)))
                (should (= (plist-get state :messageCount) 0))
                (should (eq (plist-get state :isStreaming) :false))
                (should-not (plist-member state :sessionName)))
              (let ((entries-response
                     (pilish-fake-pi-test--rpc
                      proc '(:id "empty-entries" :type "get_entries")))
                    (tree-response
                     (pilish-fake-pi-test--rpc
                      proc '(:id "empty-tree" :type "get_tree")))
                    (messages-response
                     (pilish-fake-pi-test--rpc
                      proc '(:id "empty-messages" :type "get_messages"))))
                (should
                 (pilish-fake-pi-test--json-equal-p
                  entries-response
                  '(:id "empty-entries" :type "response"
                    :command "get_entries" :success t
                    :data (:entries [] :leafId :null))))
                (should
                 (pilish-fake-pi-test--json-equal-p
                  tree-response
                  '(:id "empty-tree" :type "response"
                    :command "get_tree" :success t
                    :data (:tree [] :leafId :null))))
                (should
                 (pilish-fake-pi-test--json-equal-p
                  messages-response
                  '(:id "empty-messages" :type "response"
                    :command "get_messages" :success t
                    :data (:messages []))))))))
      (delete-directory target-dir t))))

(ert-deftest pilish-fake-pi-test-resume-selected-session-full-contract ()
  "The retained resume choreography settles and renders fake target history."
  (let* ((target-dir
          (file-name-as-directory
           (make-temp-file "pilish-fake-pi-resume-" t)))
         (fixture
          (pilish-fake-pi-test--write-branched-v3-session target-dir))
         (target-path (plist-get fixture :path))
         (log-file (expand-file-name "fake-rpc.log" target-dir)))
    (unwind-protect
        (pilish-fake-pi-test-with-session
            (session "extension-confirm" "--log-file" log-file)
          (let* ((chat-buf (plist-get session :chat-buffer))
                 (proc (plist-get session :process)))
            (pilish-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (and (plist-get pilish--state :session-id)
                      (not (pilish--session-transition-active-p))
                      (seq-find
                       (lambda (command)
                         (equal (plist-get command :name) "test-confirm"))
                       pilish--commands))))
             "initial fake state and commands")
            ;; Make the post-switch command refresh observable rather than
            ;; inheriting the startup command cache.
            (with-current-buffer chat-buf
              (setq pilish--commands nil)
              (pilish--resume-selected-session
               proc chat-buf target-path))
            (pilish-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (not (pilish--session-transition-active-p))))
             "resume transition settlement")
            (let* ((log-records
                    (pilish-fake-pi-test--read-jsonl-file log-file))
                   (types
                    (pilish-fake-pi-test--logged-input-types-after-switch
                     log-records))
                   (required
                    '("switch_session" "get_state" "get_messages"
                      "get_commands"))
                   (observed
                    (seq-filter (lambda (type) (member type required)) types)))
              ;; Ignore get_session_stats: history refresh may request it for
              ;; the header, but these four retained calls are the transition.
              (should (equal observed required)))
            (pilish-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (seq-find
                  (lambda (command)
                    (equal (plist-get command :name) "test-confirm"))
                  pilish--commands)))
             "post-resume get_commands refresh" 2)
            (with-current-buffer chat-buf
              (should (equal (plist-get pilish--state :session-id)
                             (plist-get fixture :session-id)))
              (should (equal (plist-get pilish--state :session-file)
                             target-path))
              (should (= (plist-get pilish--state :message-count)
                         (length (plist-get fixture :expected-messages))))
              (should (equal pilish--session-name
                             (plist-get fixture :session-name)))
              (should
               (pilish-fake-pi-test--json-equal-p
                pilish--canonical-messages
                (plist-get fixture :expected-messages)))
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (heading-pos
                      (string-match "^Branch Summary · [^\n]+\n=+\n" text))
                     (summary-pos
                      (string-match
                       (regexp-quote (plist-get fixture :branch-summary))
                       text))
                     (assistant-pos
                      (string-match "POST-COMPACTION ASSISTANT CONTENT" text)))
                (should heading-pos)
                (should summary-pos)
                (should assistant-pos)
                (should (< heading-pos summary-pos assistant-pos))
                (should
                 (= 1
                    (pilish-test--count-matches
                     "^Branch Summary · " text)))))))
      (delete-directory target-dir t))))

(ert-deftest pilish-fake-pi-test-session-starts-through-emacs-seam ()
  "The fake works through `pilish' startup and rendering paths." 
  (pilish-fake-pi-test-with-session (session "prompt-lifecycle")
    (let* ((chat-buf (plist-get session :chat-buffer))
           (input-buf (plist-get session :input-buffer)))
      (with-current-buffer input-buf
        (erase-buffer)
        (insert "hello seam")
        (pilish-send))
      (should
       (pilish-test-wait-until
        (lambda ()
          (with-current-buffer chat-buf
            (string-match-p "Fake reply for: hello seam"
                            (buffer-string))))
        pilish-fake-pi-test--timeout
        0.01
        (plist-get session :process)))
      (with-current-buffer chat-buf
        (should (file-exists-p (plist-get pilish--state :session-file)))))))

(ert-deftest pilish-fake-pi-test-dispositions-through-emacs-seam ()
  "Real frontend input honors handled prompts and handled/queued steering."
  (pilish-fake-pi-test-with-session
      (session "input-dispositions" "--pre-start-delay-ms" "100")
    (let ((chat (plist-get session :chat-buffer))
          (input (plist-get session :input-buffer))
          (proc (plist-get session :process)) notices)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) notices))))
        (pilish-fake-pi-test--wait-or-fail
         proc (lambda () (with-current-buffer chat (plist-get pilish--state :model)))
         "initial model state")
        (with-current-buffer input (insert "consume without a run") (pilish-send))
        (pilish-fake-pi-test--wait-or-fail
         proc (lambda () (with-current-buffer chat
                           (and (eq pilish--status 'idle) (not pilish--prompt-wait))))
         "handled prompt acceptance")
        (with-current-buffer chat
          (should (equal pilish--activity-phase "idle"))
          (should-not pilish--local-user-message)
          (should-not pilish--prompt-start-timer)
          (should-not (string-match-p "consume without a run\\|^You · " (buffer-string))))
        (with-current-buffer input (insert "initial turn") (pilish-send))
        (pilish-fake-pi-test--wait-or-fail
         proc (lambda () (with-current-buffer chat
                           (and (eq pilish--status 'streaming)
                                (string-match-p "^initial turn$" (buffer-string)))))
         "ordinary prompt start and authoritative user echo")
        (setq notices nil)
        (with-current-buffer input
          (insert "consume without a run")
          (pilish-queue-steering)
          (should (string-empty-p (buffer-string))))
        (should-not notices)
        (pilish-fake-pi-test--wait-or-fail
         proc (lambda () (member "Pi: Steering handled by extension" notices))
         "handled steering acknowledgment")
        (with-current-buffer chat
          (should (eq pilish--status 'streaming))
          (should-not (string-match-p "consume without a run" (buffer-string)))
          (should (= 1 (pilish-test--count-matches "^You · " (buffer-string)))))
        (setq notices nil)
        (with-current-buffer input
          (insert "queued steering")
          (pilish-queue-steering)
          (should (string-empty-p (buffer-string)))
          (should (= 4 (ring-length pilish--input-ring)))
          (insert "newer draft"))
        (should-not notices)
        (pilish-fake-pi-test--wait-or-fail
         proc (lambda () (member "Pi: Steering acknowledged as queued" notices))
         "queued steering acknowledgment")
        ;; The bounded fake does not publish steering queue snapshots.  Its
        ;; acknowledgment must not manufacture an input-header queue count.
        (should-not (pilish--process-queue-snapshot proc))
        (with-current-buffer input
          (should-not (string-match-p " queued [0-9]+" (pilish--header-line-string))))
        (pilish-fake-pi-test--wait-or-fail
         proc (lambda () (with-current-buffer chat
                           (and (eq pilish--status 'idle)
                                (not pilish--prompt-wait)
                                (string-match-p "Steered fake reply for: queued steering"
                                                (buffer-string)))))
         "queued steering echo and settlement")
        (with-current-buffer chat
          (should (= 1 (pilish-test--count-matches "^initial turn$" (buffer-string))))
          (should (= 1 (pilish-test--count-matches "^queued steering$" (buffer-string))))
          (should (= 2 (pilish-test--count-matches "^You · " (buffer-string))))
          (should-not pilish--followup-queue)
          (should-not pilish--local-user-message))
        (with-current-buffer input
          (should (equal (buffer-string) "newer draft"))
          (should (= 4 (ring-length pilish--input-ring))))
        (should (= 1 (cl-count "Pi: Steering acknowledged as queued" notices :test #'equal)))
        (let* ((response (pilish--rpc-sync proc '(:type "get_messages")
                                           pilish-fake-pi-test--timeout))
               (users (seq-filter (lambda (msg) (equal (plist-get msg :role) "user"))
                                  (append (plist-get (plist-get response :data) :messages) nil))))
          (should (eq (plist-get response :success) t))
          (should (equal (mapcar (lambda (msg)
                                  (plist-get (aref (plist-get msg :content) 0) :text))
                                users)
                         '("initial turn" "queued steering"))))))))

(ert-deftest pilish-fake-pi-test-prompt-image-persists-canonical-content ()
  "A UI-attached PNG survives the fake prompt and canonical history contract."
  (let* ((dir (make-temp-file "pilish-fake-pi-image-" t))
         (path (pilish-test--write-prompt-image
                (expand-file-name "pixel.png" dir) 'png))
         (data (pilish-test--prompt-image-base64 'png))
         (text "Describe this fake-contract pixel"))
    (unwind-protect
        (pilish-fake-pi-test-with-session
            (session "prompt-lifecycle")
          (let ((chat-buf (plist-get session :chat-buffer))
                (input-buf (plist-get session :input-buffer))
                (proc (plist-get session :process)))
            (pilish-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (pilish--model-supports-image-input-p chat-buf))
             "vision model state")
            (with-current-buffer input-buf
              (erase-buffer)
              (insert text)
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (&rest _) path)))
                (call-interactively #'pilish-attach-image))
              (delete-file path)
              (pilish-send))
            (pilish-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (and (eq pilish--status 'idle)
                      (not (pilish--prompt-start-wait-active-p))
                      (string-match-p "Fake reply for:" (buffer-string)))))
             "image prompt settlement")
            (with-current-buffer chat-buf
              (should (string-match-p "Image: image/png" (buffer-string))))
            (let* ((response
                    (pilish--rpc-sync
                     proc '(:type "get_messages")
                     pilish-fake-pi-test--timeout))
                   (messages (plist-get (plist-get response :data) :messages))
                   (user (seq-find
                          (lambda (message)
                            (equal (plist-get message :role) "user"))
                          (append messages nil))))
              (should (eq (plist-get response :success) t))
              (should
               (equal (plist-get user :content)
                      (vector (list :type "text" :text text)
                              (list :type "image" :data data
                                    :mimeType "image/png")))))))
      (delete-directory dir t))))

(ert-deftest pilish-fake-pi-test-extension-confirm-displays-through-emacs-seam ()
  "An extension confirm round-trip renders the follow-up message in chat." 
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
    (pilish-fake-pi-test-with-session
        (session "extension-confirm" "--extension-timeout-ms" "500")
      (let* ((chat-buf (plist-get session :chat-buffer))
             (input-buf (plist-get session :input-buffer)))
        (with-current-buffer input-buf
          (erase-buffer)
          (insert "/test-confirm")
          (pilish-send))
        (should
         (pilish-test-wait-until
          (lambda ()
            (with-current-buffer chat-buf
              (string-match-p "CONFIRMED" (buffer-string))))
          pilish-fake-pi-test--timeout
          0.01
          (plist-get session :process)))))))

(ert-deftest pilish-fake-pi-test-custom-message-command-emits-visible-message ()
  "A custom command emits its message before handled acceptance, without a run."
  (pilish-fake-pi-test-with-process (proc "extension-message")
    (pilish-fake-pi-test--send
     proc '(:id "custom" :type "prompt" :message "/test-message"))
    (let* ((events (pilish-fake-pi-test--collect-until
                    proc (lambda (obj)
                           (and (equal (plist-get obj :type) "response")
                                (equal (plist-get obj :id) "custom")))))
           (response (car (last events)))
           (start (car events))
           (end (cadr events))
           (message (plist-get start :message)))
      (should (equal (pilish-fake-pi-test--event-types events)
                     '("message_start" "message_end" "response")))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt"))
      (should (equal (plist-get (plist-get response :data) :disposition)
                     "handled"))
      (should (equal (plist-get message :role) "custom"))
      (should (eq (plist-get message :display) t))
      (should (equal (plist-get message :content) "Test message from extension"))
      (should (equal (plist-get end :message) message)))
    (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_entries")))
           (entries (plist-get (plist-get response :data) :entries)))
      (should (= (length entries) 1))
      (should (equal (plist-get (aref entries 0) :type) "custom_message")))
    (let ((response (pilish-fake-pi-test--rpc proc '(:type "get_fork_messages"))))
      (should (equal (plist-get (plist-get response :data) :messages) [])))))

(ert-deftest pilish-fake-pi-test-custom-noop-command-skips-message-events ()
  "A no-op command reports handled without messages, a run, or persistence."
  (pilish-fake-pi-test-with-process (proc "extension-noop")
    (pilish-fake-pi-test--send
     proc '(:id "noop" :type "prompt" :message "/test-noop"))
    (let* ((events (pilish-fake-pi-test--collect-until
                    proc (lambda (obj)
                           (and (equal (plist-get obj :type) "response")
                                (equal (plist-get obj :id) "noop")))))
           (response (car (last events))))
      (should (equal (pilish-fake-pi-test--event-types events) '("response")))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt"))
      (should (equal (plist-get (plist-get response :data) :disposition)
                     "handled")))
    (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_state")))
           (state (plist-get response :data)))
      (should (eq (plist-get state :isStreaming) :false))
      (should (= (plist-get state :messageCount) 0))
      (should (= (plist-get state :pendingMessageCount) 0)))
    (let ((response (pilish-fake-pi-test--rpc proc '(:type "get_entries"))))
      (should (equal (plist-get (plist-get response :data) :entries) [])))))

(ert-deftest pilish-fake-pi-test-input-dispositions ()
  "Handled input bypasses a run; busy raw prompts need explicit steering."
  (pilish-fake-pi-test-with-process
      (proc "input-dispositions" "--pre-start-delay-ms" "1000")
    (cl-labels ((exchange (command)
                 (pilish-fake-pi-test--send proc command)
                 (pilish-fake-pi-test--collect-until
                  proc (lambda (obj)
                         (and (equal (plist-get obj :type) "response")
                              (equal (plist-get obj :id)
                                     (plist-get command :id)))))))
      (let* ((events (exchange '(:id "handled" :type "prompt"
                                :message "consume without a run")))
             (handled (car (last events)))
             (types (pilish-fake-pi-test--event-types events)))
        (should (eq (plist-get handled :success) t))
        (should (equal (plist-get (plist-get handled :data) :disposition)
                       "handled"))
        (should (equal types '("response")))
        (should-not (seq-intersection
                     types '("agent_start" "agent_end" "agent_settled") #'equal)))
      (let ((response (pilish-fake-pi-test--rpc proc '(:type "get_entries"))))
        (should (equal (plist-get (plist-get response :data) :entries) [])))
      (let ((normal (car (last (exchange '(:id "normal" :type "prompt"
                                          :message "initial turn"))))))
        (should (eq (plist-get normal :success) t))
        (should (equal (plist-get (plist-get normal :data) :disposition)
                       "started")))
      (let* ((events (exchange '(:id "busy-handled" :type "prompt"
                                :message "consume without a run")))
             (handled (car (last events))))
        (should (equal (pilish-fake-pi-test--event-types events) '("response")))
        (should (eq (plist-get handled :success) t))
        (should (equal (plist-get (plist-get handled :data) :disposition)
                       "handled")))
      (let ((rejected (car (last (exchange '(:id "busy" :type "prompt"
                                            :message "must not queue"))))))
        (should (eq (plist-get rejected :success) :false))
        (should-not (plist-member rejected :data))
        (should (string-match-p "streamingBehavior" (plist-get rejected :error))))
      (let ((rejected (car (last (exchange '(:id "follow-up" :type "prompt"
                                            :message "unsupported queue"
                                            :streamingBehavior "followUp"))))))
        (should (eq (plist-get rejected :success) :false))
        (should (string-match-p "followUp" (plist-get rejected :error))))
      (let ((queued (car (last (exchange '(:id "queued" :type "prompt"
                                          :message "raw queued turn"
                                          :streamingBehavior "steer"))))))
        (should (eq (plist-get queued :success) t))
        (should (equal (plist-get (plist-get queued :data) :disposition)
                       "queued")))
      (let ((state (car (last (exchange '(:id "queued-state" :type "get_state"))))))
        (should (= (plist-get (plist-get state :data) :pendingMessageCount) 1)))
      ;; A user append must not reset the still-pending slot's count.
      (pilish-fake-pi-test--collect-until
       proc (lambda (obj)
              (and (equal (plist-get obj :type) "message_end")
                   (equal (plist-get (plist-get obj :message) :role) "user"))))
      (let ((state (car (last (exchange '(:id "user-state" :type "get_state"))))))
        (should (= (plist-get (plist-get state :data) :pendingMessageCount) 1)))
      (pilish-fake-pi-test--collect-until
       proc (lambda (obj)
              (and (equal (plist-get obj :type) "message_end")
                   (equal (plist-get (plist-get obj :message) :role) "user"))))
      (let ((state (car (last (exchange '(:id "taken-state" :type "get_state"))))))
        (should (= (plist-get (plist-get state :data) :pendingMessageCount) 0)))
      (pilish-fake-pi-test--collect-until
       proc (lambda (obj) (equal (plist-get obj :type) "agent_settled")))
      (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_fork_messages")))
             (messages (plist-get (plist-get response :data) :messages)))
        (should (equal (mapcar (lambda (message) (plist-get message :text))
                               (append messages nil))
                       '("initial turn" "raw queued turn")))))))

(ert-deftest pilish-fake-pi-test-dialog-ack-follows-response ()
  "A dialog waits for its answer and custom output before handled acceptance."
  (pilish-fake-pi-test-with-process
      (proc "extension-confirm" "--extension-timeout-ms" "0")
    (pilish-fake-pi-test--send
     proc '(:id "dialog" :type "prompt" :message "/test-confirm"))
    (let ((request (pilish-fake-pi-test--pop-object proc)))
      (should (equal (plist-get request :type) "extension_ui_request"))
      (should (equal (plist-get request :method) "confirm"))
      (should-not (plist-member request :timeout))
      (should-not (pilish-test-wait-until
                   (lambda () (process-get proc 'fake-pi-objects)) 0.2 0.01 proc))
      (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_state")))
             (state (plist-get response :data)))
        (should (eq (plist-get state :isStreaming) :false))
        (should (= (plist-get state :messageCount) 0)))
      ;; This bounded double has one worker, even though a dialog is not a run.
      (let ((overlap (pilish-fake-pi-test--rpc
                      proc '(:id "overlap" :type "prompt" :message "/test-confirm"))))
        (should (eq (plist-get overlap :success) :false)))
      (pilish-fake-pi-test--send
       proc (list :type "extension_ui_response"
                  :id (plist-get request :id) :confirmed t))
      (let* ((events (pilish-fake-pi-test--collect-until
                      proc (lambda (obj)
                             (and (equal (plist-get obj :type) "response")
                                  (equal (plist-get obj :id) "dialog")))))
             (dialog-ack (car (last events)))
             (types (pilish-fake-pi-test--event-types (cons request events))))
        (should (equal types '("extension_ui_request" "message_start"
                               "message_end" "response")))
        (should-not (seq-intersection
                     types '("agent_start" "agent_end" "agent_settled") #'equal))
        (should (= (length (pilish-fake-pi-test--events-of-type events "response"))
                   1))
        (should (equal (plist-get (plist-get (cadr events) :message) :content)
                       "CONFIRMED"))
        (should (eq (plist-get dialog-ack :success) t))
        (should (equal (plist-get dialog-ack :command) "prompt"))
        (should (equal (plist-get (plist-get dialog-ack :data) :disposition)
                       "handled")))
      (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_entries")))
             (entries (plist-get (plist-get response :data) :entries)))
        (should (= (length entries) 1))
        (should (equal (plist-get (aref entries 0) :type) "custom_message")))
      (let ((response (pilish-fake-pi-test--rpc proc '(:type "get_fork_messages"))))
        (should (equal (plist-get (plist-get response :data) :messages) []))))))

(ert-deftest pilish-fake-pi-test-handled-steer-does-not-queue ()
  "Handled steering neither fills nor replaces the pending steering slot."
  (pilish-fake-pi-test-with-process
      (proc "input-dispositions" "--pre-start-delay-ms" "1000")
    (cl-labels ((exchange (command)
                 (pilish-fake-pi-test--send proc command)
                 (pilish-fake-pi-test--collect-until
                  proc (lambda (obj)
                         (and (equal (plist-get obj :type) "response")
                              (equal (plist-get obj :id)
                                     (plist-get command :id)))))))
      (let* ((events (exchange '(:id "idle-handled" :type "steer"
                                :message "consume without a run")))
             (handled (car (last events))))
        (should (equal (pilish-fake-pi-test--event-types events) '("response")))
        (should (eq (plist-get handled :success) t))
        (should (equal (plist-get (plist-get handled :data) :disposition)
                       "handled")))
      (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_state")))
             (state (plist-get response :data)))
        (should (= (plist-get state :pendingMessageCount) 0))
        (should (= (plist-get state :messageCount) 0)))
      (let ((normal (car (last (exchange '(:id "normal" :type "prompt"
                                          :message "initial turn"))))))
        (should (equal (plist-get (plist-get normal :data) :disposition) "started")))
      (let ((queued (car (last (exchange '(:id "steer" :type "steer"
                                          :message "keep queued turn"))))))
        (should (eq (plist-get queued :success) t))
        (should (equal (plist-get (plist-get queued :data) :disposition) "queued")))
      (let* ((events (exchange '(:id "busy-handled" :type "steer"
                                :message "consume without a run")))
             (handled (car (last events))))
        (should (equal (pilish-fake-pi-test--event-types events) '("response")))
        (should (eq (plist-get handled :success) t))
        (should (equal (plist-get (plist-get handled :data) :disposition)
                       "handled")))
      (let ((state (car (last (exchange '(:id "queued-state" :type "get_state"))))))
        (should (= (plist-get (plist-get state :data) :pendingMessageCount) 1)))
      (let* ((events (exchange '(:id "clear" :type "clear_queue")))
             (response (car (last events))))
        (should (equal (pilish-fake-pi-test--event-types events)
                       '("queue_update" "response")))
        (should (equal (plist-get response :data)
                       '(:steering ["keep queued turn"] :followUp []))))
      (let ((state (car (last (exchange '(:id "cleared-state" :type "get_state"))))))
        (should (= (plist-get (plist-get state :data) :pendingMessageCount) 0)))
      (let ((queued (car (last (exchange '(:id "steer-again" :type "steer"
                                          :message "keep queued turn"))))))
        (should (equal (plist-get (plist-get queued :data) :disposition) "queued")))
      (let* ((events (pilish-fake-pi-test--collect-until
                      proc (lambda (obj) (equal (plist-get obj :type) "agent_settled"))))
             (users (pilish-fake-pi-test--message-events events "message_end" "user")))
        (should (equal
                 (mapcar (lambda (event)
                           (plist-get (aref (plist-get (plist-get event :message) :content) 0)
                                      :text))
                         users)
                 '("initial turn" "keep queued turn"))))
      (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_state")))
             (state (plist-get response :data)))
        (should (= (plist-get state :pendingMessageCount) 0)))
      (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_fork_messages")))
             (messages (plist-get (plist-get response :data) :messages)))
        (should (equal (mapcar (lambda (message) (plist-get message :text))
                               (append messages nil))
                       '("initial turn" "keep queued turn")))))))

(ert-deftest pilish-fake-pi-test-lifecycle-emits-settled-after-final-run ()
  "The fake backend emits agent_settled after agent_end and is then idle."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "finish this run"))
    (let ((events (pilish-fake-pi-test--collect-until
                   proc (lambda (obj) (equal (plist-get obj :type) "agent_end")))))
      (should (member "agent_start" (pilish-fake-pi-test--event-types events)))
      (should (pilish-test-wait-until
               (lambda () (process-get proc 'fake-pi-objects))
               pilish-test-short-wait 0.01 proc))
      (should (equal (plist-get (pilish-fake-pi-test--pop-object proc) :type)
                     "agent_settled"))
      (let* ((response (pilish-fake-pi-test--rpc proc '(:type "get_state")))
             (data (plist-get response :data)))
        (should (eq (plist-get response :success) t))
        (should (eq (plist-get data :isStreaming) :false))
        (should (eq (plist-get data :isCompacting) :false))))))

(ert-deftest pilish-fake-pi-test-prompt-response-precedes-stream-events ()
  "prompt returns success first, then streams lifecycle events."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "hello fake pi"))
    (let* ((response (pilish-fake-pi-test--pop-object proc))
           (events (pilish-fake-pi-test--collect-until
                    proc
                    (lambda (obj) (equal (plist-get obj :type) "agent_settled"))))
           (assistant-start (seq-find
                             (lambda (obj)
                               (and (equal (plist-get obj :type) "message_start")
                                    (equal (plist-get (plist-get obj :message) :role)
                                           "assistant")))
                             events))
           (message-updates
            (pilish-fake-pi-test--events-of-type events "message_update"))
           (text-deltas
            (pilish-fake-pi-test--message-updates events "text_delta")))
      (should (equal (plist-get response :type) "response"))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt"))
      (should (equal (car (pilish-fake-pi-test--event-types events)) "agent_start"))
      (should assistant-start)
      (should (> (length text-deltas) 0))
      (dolist (update message-updates)
        (should (plist-member update :usage))
        (should-not (plist-member update :message))
        (should-not (plist-member (plist-get update :assistantMessageEvent)
                                  :partial)))
      (should (equal (last (pilish-fake-pi-test--event-types events) 2)
                     '("agent_end" "agent_settled"))))))

(ert-deftest pilish-fake-pi-test-tool-stream-emits-tool-events ()
  "tool_stream emits an ordered, correlated, delta-only RPC lifecycle."
  (pilish-fake-pi-test-with-process (proc "tool-read")
    (should (eq (plist-get (pilish-fake-pi-test--rpc
                           proc '(:type "set_thinking_level" :level "low"))
                          :success)
                t))
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "use the tool"))
    (should (equal (plist-get (pilish-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let* ((events (pilish-fake-pi-test--collect-until
                    proc
                    (lambda (obj) (equal (plist-get obj :type) "agent_settled"))))
           (assistant-starts
            (pilish-fake-pi-test--message-events
             events "message_start" "assistant"))
           (assistant-ends
            (pilish-fake-pi-test--message-events
             events "message_end" "assistant"))
           (tool-result-start
            (car (pilish-fake-pi-test--message-events
                  events "message_start" "toolResult")))
           (tool-result-end
            (car (pilish-fake-pi-test--message-events
                  events "message_end" "toolResult")))
           (message-updates
            (pilish-fake-pi-test--events-of-type events "message_update"))
           (toolcall-start-update
            (car (pilish-fake-pi-test--message-updates
                  events "toolcall_start")))
           (toolcall-delta-updates
            (pilish-fake-pi-test--message-updates events "toolcall_delta"))
           (toolcall-end-update
            (car (pilish-fake-pi-test--message-updates
                  events "toolcall_end")))
           (text-delta-updates
            (pilish-fake-pi-test--message-updates events "text_delta"))
           (tool-execution-start
            (car (pilish-fake-pi-test--events-of-type
                  events "tool_execution_start")))
           (tool-execution-update
            (car (pilish-fake-pi-test--events-of-type
                  events "tool_execution_update")))
           (tool-execution-end
            (car (pilish-fake-pi-test--events-of-type
                  events "tool_execution_end")))
           (agent-end (car (pilish-fake-pi-test--events-of-type events "agent_end")))
           (lifecycle
            (mapcar
             (lambda (obj)
               (pcase (plist-get obj :type)
                 ((or "message_start" "message_end")
                  (format "%s:%s"
                          (plist-get obj :type)
                          (plist-get (plist-get obj :message) :role)))
                 ("message_update"
                  (plist-get (plist-get obj :assistantMessageEvent) :type))
                 (type type)))
             events)))
      (should
       (equal lifecycle
              '("agent_start"
                "message_start:user" "message_end:user"
                "message_start:assistant"
                "toolcall_start"
                "toolcall_delta" "toolcall_delta" "toolcall_delta"
                "toolcall_end"
                "message_end:assistant"
                "tool_execution_start" "tool_execution_update"
                "tool_execution_end"
                "message_start:toolResult" "message_end:toolResult"
                "message_start:assistant"
                "text_delta" "text_delta"
                "message_end:assistant"
                "agent_end" "agent_settled")))
      (should (> (length toolcall-delta-updates) 1))
      (should (> (length text-delta-updates) 0))
      (dolist (update message-updates)
        (should (plist-member update :usage))
        (should-not (plist-member update :message))
        (should-not (plist-member (plist-get update :assistantMessageEvent)
                                  :partial)))
      (let* ((first-assistant-end (car assistant-ends))
             (second-assistant-end (cadr assistant-ends))
             (toolcall-start-event
              (plist-get toolcall-start-update :assistantMessageEvent))
             (toolcall-end-event
              (plist-get toolcall-end-update :assistantMessageEvent))
             (call-id (plist-get toolcall-start-event :id))
             (streamed-arguments
              (mapconcat
               (lambda (update)
                 (plist-get (plist-get update :assistantMessageEvent) :delta))
               toolcall-delta-updates
               ""))
             (tool-call (plist-get toolcall-end-event :toolCall))
             (tool-assistant-message (plist-get first-assistant-end :message))
             (tool-call-from-message
              (aref (plist-get tool-assistant-message :content) 0))
             (tool-result-message (plist-get tool-result-start :message))
             (final-message (plist-get second-assistant-end :message))
             (agent-messages (plist-get agent-end :messages)))
        (dolist (start assistant-starts)
          (should (= (length (plist-get (plist-get start :message) :content)) 0))
          (should (equal (plist-get (plist-get start :message) :stopReason)
                         "pending")))
        (should (and (stringp call-id) (> (length call-id) 0)))
        (should (= (plist-get toolcall-start-event :contentIndex) 0))
        (should (equal (plist-get toolcall-start-event :toolName) "read"))
        (should (= (plist-get toolcall-end-event :contentIndex) 0))
        (dolist (update toolcall-delta-updates)
          (should (= (plist-get (plist-get update :assistantMessageEvent)
                                :contentIndex)
                     0)))
        (should (equal (pilish--parse-json-line streamed-arguments)
                       '(:path "/tmp/fake-tool.txt")))
        (should (equal tool-call tool-call-from-message))
        (should (equal (plist-get tool-assistant-message :stopReason) "toolUse"))
        (should (equal (plist-get tool-assistant-message :thinkingLevel) "low"))
        (should (equal (plist-get final-message :thinkingLevel) "low"))
        (should (equal (plist-get tool-call :id) call-id))
        (should (equal (plist-get tool-call :name) "read"))
        (should (equal (plist-get (plist-get tool-call :arguments) :path)
                       "/tmp/fake-tool.txt"))
        (dolist (event (list tool-execution-start
                             tool-execution-update
                             tool-execution-end))
          (should (equal (plist-get event :toolCallId) call-id))
          (should (equal (plist-get event :toolName) "read")))
        (should (equal (plist-get (plist-get tool-execution-start :args) :path)
                       "/tmp/fake-tool.txt"))
        (should (equal (plist-get (aref (plist-get (plist-get tool-execution-update
                                                               :partialResult)
                                                   :content)
                                          0)
                                  :text)
                       "fake tool output\n"))
        (should (equal (plist-get tool-execution-end :isError) :false))
        (should (equal (plist-get tool-result-end :message) tool-result-message))
        (should (equal (plist-get tool-result-message :toolCallId) call-id))
        (should (equal (plist-get tool-result-message :toolName) "read"))
        (should (equal (plist-get tool-result-message :isError) :false))
        (should (equal (plist-get tool-result-message :content)
                       (plist-get (plist-get tool-execution-end :result) :content)))
        (should (equal (plist-get tool-result-message :details)
                       (plist-get (plist-get tool-execution-end :result) :details)))
        (should (equal (plist-get (aref (plist-get tool-result-message :content) 0)
                                  :text)
                       "fake tool output\nmore output\n"))
        (should (equal (mapconcat
                        (lambda (update)
                          (plist-get (plist-get update :assistantMessageEvent) :delta))
                        text-delta-updates
                        "")
                       "Tool finished"))
        (should (equal (plist-get final-message :stopReason) "stop"))
        (should (equal (plist-get (aref (plist-get final-message :content) 0) :text)
                       "Tool finished"))
        (should (equal (mapcar (lambda (message) (plist-get message :role))
                               agent-messages)
                       '("user" "assistant" "toolResult" "assistant")))
        (should (equal (aref agent-messages 1) tool-assistant-message))
        (should (equal (plist-get (aref agent-messages 2) :toolCallId) call-id))
        (should (equal (aref agent-messages 3) final-message))
        (should (equal (plist-get agent-end :willRetry) :false))
        (pilish-fake-pi-test--assert-assistant-roundtrip
         proc (list tool-assistant-message final-message))))))

(ert-deftest pilish-fake-pi-test-nested-tools-wire-and-disk ()
  "Nested wire records keep the parent snapshot unchanged after a late end."
  (pilish-fake-pi-test-with-process (proc "nested-tools")
    (let* ((state (pilish-fake-pi-test--rpc proc '(:type "get_state")))
           (session-file (plist-get (plist-get state :data) :sessionFile))
           (literal-records
            (plist-get (plist-get (pilish-test--read-json-fixture
                                   "fake-pi/nested-tools.json") :prompt) :records))
           disk-before-late)
      (should-not (file-exists-p session-file))
      (let ((response (pilish-fake-pi-test--rpc
                       proc '(:id "nested" :type "prompt" :message "nested wire"))))
        (should (eq (plist-get response :success) t))
        (should (equal (plist-get (plist-get response :data) :disposition)
                       "started")))
      ;; Settlement is deliberately not the collection boundary.  Capture disk
      ;; bytes there, then keep reading until the outstanding child really ends.
      (let* ((events
              (pilish-fake-pi-test--collect-until
               proc (lambda (event)
                      (when (equal (plist-get event :type) "agent_settled")
                        (setq disk-before-late
                              (pilish-fake-pi-test--file-bytes session-file)))
                      (and (equal (plist-get event :type) "tool_execution_end")
                           (equal (plist-get event :toolCallId) "parent/3")))))
             (starts (pilish-fake-pi-test--events-of-type events "tool_execution_start"))
             (updates (pilish-fake-pi-test--events-of-type events "tool_execution_update"))
             (ends (pilish-fake-pi-test--events-of-type events "tool_execution_end"))
             (parent-start (car starts))
             (child-starts (cdr starts))
             (child-update (car updates))
             (partial (plist-get (cadr updates) :partialResult))
             (details-rows (plist-get (plist-get partial :details) :calls))
             (parent-end (nth 2 ends))
             (late-end (nth 3 ends))
             (assistants (mapcar (lambda (event) (plist-get event :message))
                                (pilish-fake-pi-test--message-events
                                 events "message_end" "assistant")))
             (tool-call (aref (plist-get (car assistants) :content) 0))
             (tool-results (pilish-fake-pi-test--message-events
                            events "message_end" "toolResult"))
             (parent-message (plist-get (car tool-results) :message))
             (nested (plist-get parent-message :nestedCalls))
             (calls (plist-get nested :calls))
             (omitted-row (aref calls 1))
             (late-disk-row (aref calls 2))
             (settled (car (pilish-fake-pi-test--events-of-type events "agent_settled")))
             (records (pilish-fake-pi-test--read-jsonl-file session-file))
             (disk-messages (seq-map (lambda (entry) (plist-get entry :message))
                                    (seq-subseq records 1)))
             (disk-after-late (pilish-fake-pi-test--file-bytes session-file)))
        (should
         (equal
          (mapcar (lambda (event)
                    (pcase (plist-get event :type)
                      ((or "message_start" "message_end")
                       (format "%s:%s" (plist-get event :type)
                               (plist-get (plist-get event :message) :role)))
                      ("message_update"
                       (plist-get (plist-get event :assistantMessageEvent) :type))
                      ((or "tool_execution_start" "tool_execution_update" "tool_execution_end")
                       (format "%s:%s" (plist-get event :type)
                               (plist-get event :toolCallId)))
                      (type type)))
                  events)
          '("agent_start" "message_start:user" "message_end:user"
            "message_start:assistant" "toolcall_start" "toolcall_delta" "toolcall_end"
            "message_end:assistant" "tool_execution_start:parent"
            "tool_execution_start:parent/1" "tool_execution_start:parent/2"
            "tool_execution_start:parent/3" "tool_execution_update:parent/3"
            "tool_execution_update:parent" "tool_execution_end:parent/1"
            "tool_execution_end:parent/2" "tool_execution_end:parent"
            "message_start:toolResult" "message_end:toolResult"
            "message_start:assistant" "text_delta" "message_end:assistant"
            "agent_end" "agent_settled" "tool_execution_end:parent/3")))
        (should (equal (nthcdr 3 events) (append literal-records nil)))
        (should (equal (plist-get parent-start :toolName) "codemode"))
        (should (string-match-p "\n" (plist-get (plist-get parent-start :args) :code)))
        (should (equal (plist-get tool-call :id) "parent"))
        (should (equal (plist-get tool-call :arguments) (plist-get parent-start :args)))
        (dolist (update (pilish-fake-pi-test--events-of-type events "message_update"))
          (should (plist-member update :usage))
          (should-not (plist-member update :message))
          (should-not (plist-member (plist-get update :assistantMessageEvent) :partial)))
        (should (equal (mapcar (lambda (event) (plist-get event :toolCallId)) child-starts)
                       '("parent/1" "parent/2" "parent/3")))
        (dolist (child-start child-starts)
          (should (equal (plist-get child-start :parentToolCallId) "parent")))
        (should (equal (plist-get (car child-starts) :toolName) "read"))
        (should (equal (plist-get (plist-get (car child-starts) :args) :path)
                       "/tmp/CHILD-READ"))
        (should (equal (plist-get (cadr child-starts) :toolName) "bash"))
        (should (string-match-p "CHILD-ERROR"
                                (plist-get (plist-get (cadr child-starts) :args) :command)))
        (should (= (string-bytes
                    (json-serialize (plist-get (cadr child-starts) :args)))
                   9000))
        (should (string-match-p "CHILD-LATE"
                                (plist-get (plist-get (nth 2 child-starts) :args) :command)))
        (should (equal (plist-get child-update :parentToolCallId) "parent"))
        (should (equal (plist-get child-update :args)
                       (plist-get (nth 2 child-starts) :args)))
        (should (equal (plist-get partial :content) []))
        (should (= (length details-rows) 5))
        (dotimes (i 3)
          (let ((running-row (aref details-rows i)))
            (should (equal (plist-get running-row :id) "parent/?"))
            (should (equal (plist-get running-row :status) "running"))))
        (dolist (details-row (append details-rows nil))
          (should (stringp (plist-get details-row :args))))
        (should (equal (aref details-rows 3)
                       '(:id "parent/models.classify/1" :name "models.classify"
                         :args "fake/classifier" :status "ok" :durationMs 30 :cost 0.002)))
        (should (equal (aref details-rows 4)
                       '(:id "parent/models.generateImages/2" :name "models.generateImages"
                         :args "fake/image" :status "cancelled" :durationMs 35)))
        (should (eq (plist-get (car ends) :isError) :false))
        (should (eq (plist-get (cadr ends) :isError) t))
        (should (eq (plist-get late-end :isError) t))
        (dolist (child-end (list (car ends) (cadr ends) late-end))
          (should (equal (plist-get child-end :parentToolCallId) "parent")))
        (should-not (plist-member (plist-get parent-end :result) :nestedCalls))
        (should (equal (plist-get parent-message :usage)
                       '(:input 100 :output 4 :cacheRead 0 :cacheWrite 0 :totalTokens 104
                         :cost (:input 0.001 :output 0.001 :cacheRead 0 :cacheWrite 0
                                :total 0.002))))
        (should (equal (plist-get parent-message :usage)
                       (plist-get (plist-get parent-end :result) :usage)))
        (should (= (length tool-results) 1))
        (should (equal (plist-get parent-message :toolCallId) "parent"))
        (should (equal (plist-get
                        (plist-get (car (pilish-fake-pi-test--message-events
                                        events "message_start" "toolResult")) :message)
                        :nestedCalls)
                       nested))
        (should (eq (plist-get nested :complete) :false))
        (should-not (plist-member nested :totalCount))
        (should (= (length calls) 3))
        (should (equal (mapcar (lambda (call) (plist-get call :id)) (append calls nil))
                       '("parent/1" "parent/2" "parent/3")))
        (should (equal (mapcar (lambda (call) (plist-get call :status)) (append calls nil))
                       '("ok" "error" "unfinished")))
        (should (equal (plist-get (aref calls 0) :arguments) '(:path "/tmp/CHILD-READ")))
        (should (= (plist-get omitted-row :argumentsBytes) 9000))
        (should-not (plist-member omitted-row :arguments))
        (should (equal (plist-get late-disk-row :status) "unfinished"))
        (should (equal (plist-get late-disk-row :arguments)
                       (plist-get (nth 2 child-starts) :args)))
        (should (< (seq-position events settled #'eq) (seq-position events late-end #'eq)))
        (should (equal (plist-get (cadr assistants) :content)
                       [(:type "text" :text "FINAL-AFTER-PARENT")]))
        (should disk-before-late)
        (should (equal disk-before-late disk-after-late))
        (pilish-fake-pi-test--assert-valid-v3-records (aref records 0) (seq-subseq records 1))
        (should (equal (mapcar (lambda (message) (plist-get message :role)) disk-messages)
                       '("user" "assistant" "toolResult" "assistant")))
        (should (equal (nth 2 disk-messages) parent-message))
        (should (equal (plist-get
                        (plist-get (pilish-fake-pi-test--rpc proc '(:type "get_messages")) :data)
                        :messages)
                       (vconcat disk-messages)))
        (should-not (process-get proc 'fake-pi-invalid-lines))))))

(ert-deftest pilish-fake-pi-test-nested-tools-live-history-contract ()
  "Literal nested wire events and saved history share one visible summary grammar."
  ;; Losing nested routing, appending a late end at the tail, or replaying live
  ;; status on reload breaks this subprocess-to-production-display boundary.
  ;; W2/W3's recorded REDs establish the missing summaries/readable script;
  ;; this is a boundary regression over that already implemented renderer.
  (pilish-fake-pi-test-with-session (session "nested-tools")
    (let* ((chat-buf (plist-get session :chat-buffer))
           (input-buf (plist-get session :input-buffer))
           (proc (plist-get session :process))
           (expected-ids '("parent/1" "parent/2" "parent/3"
                           "parent/models.classify/1" "parent/models.generateImages/2"))
           (script-opening
            "const read = tools.read({ path: \"/tmp/CHILD-READ\" });\nconst error = tools.bash(")
           completed-live-lines live-visible history-visible)
      (pilish-fake-pi-test--wait-or-fail
       proc (lambda ()
              (with-current-buffer chat-buf
                (and (plist-get pilish--state :session-id)
                     (not (pilish--session-transition-active-p)))))
       "initial nested session state")
      (with-current-buffer input-buf
        (erase-buffer)
        (insert "nested frontend contract")
        (pilish-send))
      ;; The fixture's actual late end is an error, not cancellation metadata
      ;; or a success.  Its post-settlement result must repaint the parent.
      (pilish-fake-pi-test--wait-or-fail
       proc (lambda ()
              (with-current-buffer chat-buf
                (let ((late (cdr (assoc "parent/3"
                                        (pilish-test--nested-summary-lines "parent" t)))))
                  (and late (string-prefix-p "  ✗ bash " late)
                       (string-match-p "Command aborted" late)))))
       "late child result under its original parent")
      (with-current-buffer chat-buf
        (font-lock-ensure)
        (let* ((rows (pilish-test--nested-summary-lines "parent" t))
               (visible-rows (pilish-test--nested-summary-lines "parent"))
               (read-summary (cdr (assoc "parent/1" visible-rows)))
               (error-summary (cdr (assoc "parent/2" visible-rows)))
               (read-row (seq-find (lambda (row)
                                     (string-match-p "/tmp/CHILD-READ" (cdr row))) rows))
               (error-row (seq-find (lambda (row)
                                      (string-match-p "CHILD-ERROR" (cdr row))) rows))
               (late-summary (cdr (assoc "parent/3" visible-rows))))
          (should (equal (mapcar #'car rows) expected-ids))
          (should (equal (car read-row) "parent/1"))
          (should (equal (car error-row) "parent/2"))
          (setq completed-live-lines (list read-row error-row)
                live-visible (pilish--visible-text (point-min) (point-max)))
          (should (= 1 (pilish-test--count-matches (regexp-quote read-summary) live-visible)))
          (should (= 1 (pilish-test--count-matches (regexp-quote error-summary) live-visible)))
          (should (equal (cdr read-row) "  ✓ read {\"path\":\"/tmp/CHILD-READ\"}"))
          (should (string-prefix-p
                   "  ✗ bash {\"command\":\"printf 'CHILD-ERROR'; exit 7; # " error-summary))
          (should (string-suffix-p
                   " — CHILD-ERROR\\n\\nCommand exited with code 7" error-summary))
          (should (equal (cdr (assoc "parent/models.classify/1" rows))
                         "  ✓ models.classify fake/classifier $0.002"))
          (should (equal (cdr (assoc "parent/models.generateImages/2" rows))
                         "  ⊘ models.generateImages fake/image"))
          (let ((late-child-position (string-match (regexp-quote late-summary) live-visible))
                (final-assistant-position (string-match "FINAL-AFTER-PARENT" live-visible)))
            (should late-child-position)
            (should final-assistant-position)
            (should (< late-child-position final-assistant-position)))
          (should (= 1 (pilish-test--count-matches (regexp-quote late-summary) live-visible)))
          ;; Received child output can be opened live through the public TAB.
          (goto-char (point-min))
          (search-forward read-summary)
          (goto-char (match-beginning 0))
          (pilish-toggle-tool-section)
          (should (string-match-p "CHILD-READ: contents\n"
                                  (pilish--visible-text (point-min) (point-max))))))
      (let* ((response (pilish--rpc-sync proc '(:type "get_messages")
                                        pilish-fake-pi-test--timeout))
             (messages (plist-get (plist-get response :data) :messages))
             (parent (seq-find (lambda (message)
                                 (equal (plist-get message :toolCallId) "parent")) messages))
             (records (pilish-fake-pi-test--read-jsonl-file
                       (plist-get (buffer-local-value 'pilish--state chat-buf) :session-file)))
             (disk-parent (plist-get
                           (seq-find (lambda (record)
                                       (equal (plist-get (plist-get record :message) :toolCallId)
                                              "parent")) records)
                           :message))
             (persisted-late-status
              (plist-get (seq-find (lambda (call)
                                    (equal (plist-get call :id) "parent/3"))
                                  (plist-get (plist-get disk-parent :nestedCalls) :calls))
                         :status)))
        (should (eq (plist-get response :success) t))
        (should (equal persisted-late-status "unfinished"))
        (should (equal (plist-get parent :nestedCalls) (plist-get disk-parent :nestedCalls)))
        (with-temp-buffer
          (pilish-chat-mode)
          (pilish--display-history-messages messages)
          (font-lock-ensure)
          (let* ((rows (pilish-test--nested-summary-lines "parent" t))
                 (visible-rows (pilish-test--nested-summary-lines "parent"))
                 (read-summary (cdr (assoc "parent/1" visible-rows)))
                 (error-summary (cdr (assoc "parent/2" visible-rows)))
                 (completed-history-lines (seq-take rows 2))
                 (late-summary (cdr (assoc "parent/3" rows))))
            (should (equal (mapcar #'car rows) expected-ids))
            (should (equal completed-live-lines completed-history-lines))
            (should (equal late-summary
                           "  ? bash {\"command\":\"printf 'CHILD-LATE'; sleep 1\"} unfinished when saved"))
            (setq history-visible (pilish--visible-text (point-min) (point-max)))
            (should (= 1 (pilish-test--count-matches (regexp-quote read-summary) history-visible)))
            (should (= 1 (pilish-test--count-matches (regexp-quote error-summary) history-visible)))
            (should (= 1 (pilish-test--count-matches (regexp-quote late-summary) history-visible)))
            (should (< (string-match (regexp-quote late-summary) history-visible)
                       (string-match "FINAL-AFTER-PARENT" history-visible))))
          (should-not (string-match-p "CHILD-READ: contents\\|Command aborted" history-visible))
          (should (string-match-p (regexp-quote "Child outputs are not saved in sessions.")
                                  history-visible))
          (should (string-match-p (regexp-quote "saved arguments omitted (9000 bytes)")
                                  history-visible))))
      (dolist (visible (list live-visible history-visible))
        (should (string-match-p (regexp-quote script-opening) visible))
        (should-not (string-match-p (regexp-quote "\\nconst error") visible))
        (should (= 1 (pilish-test--count-matches "Incomplete saved call summary" visible)))
        (should-not (string-match-p "[0-9]+ more calls not recorded" visible))))))

(ert-deftest pilish-fake-pi-test-nested-tools-abort-after-agent-end-settles-once ()
  "Abort after the literal end owes one settlement, not another low-level end."
  (pilish-fake-pi-test-with-process (proc "nested-tools")
    (let* ((state (pilish-fake-pi-test--rpc proc '(:type "get_state")))
           (session-file (plist-get (plist-get state :data) :sessionFile))
           abort-sent)
      ;; React in the actual wire filter, not after a collector discards the
      ;; already received lifecycle prefix.  Keep every event for the counts.
      (set-process-filter
       proc (lambda (process output)
              (pilish-fake-pi-test--process-filter process output)
              (when (and (not abort-sent)
                         (pilish-fake-pi-test--events-of-type
                          (process-get process 'fake-pi-objects) "agent_end"))
                (setq abort-sent t)
                (pilish-fake-pi-test--send
                 process '(:id "abort-after-end" :type "abort")))))
      (should (eq (plist-get (pilish-fake-pi-test--rpc
                             proc '(:type "prompt" :message "abort after literal end"))
                            :success) t))
      (let* ((events (pilish-fake-pi-test--collect-until
                      proc (lambda (event)
                             (equal (plist-get event :id) "abort-after-end"))))
             (ack (car (last events)))
             (disk-at-ack (pilish-fake-pi-test--file-bytes session-file)))
        (should abort-sent)
        (should (equal (plist-get ack :type) "response"))
        (should (equal (plist-get ack :command) "abort"))
        (should (eq (plist-get ack :success) t))
        (should (equal (seq-filter
                        (lambda (type)
                          (member type '("agent_start" "agent_end" "agent_settled")))
                        (pilish-fake-pi-test--event-types events))
                       '("agent_start" "agent_end" "agent_settled")))
        (should-not (seq-find
                     (lambda (event) (equal (plist-get event :toolCallId) "parent/3"))
                     (pilish-fake-pi-test--events-of-type events "tool_execution_end")))
        (set-process-filter proc #'pilish-fake-pi-test--process-filter)
        (let ((idle (plist-get (pilish-fake-pi-test--rpc proc '(:type "get_state")) :data)))
          (should (eq (plist-get idle :isStreaming) :false))
          (should (eq (plist-get idle :isCompacting) :false))
          (should (= (plist-get idle :pendingMessageCount) 0))
          (should (= (plist-get idle :messageCount) 4)))
        ;; Reuse the reset contract's observation window past the fixture's
        ;; late-record pause.  Fake abort success is a worker-join boundary;
        ;; this says nothing about real Pi's abort acknowledgment ordering.
        (should-not (pilish-test-wait-until
                     (lambda () (process-get proc 'fake-pi-objects))
                     0.75 0.01 proc))
        (should (equal disk-at-ack (pilish-fake-pi-test--file-bytes session-file)))
        (let ((next (pilish-fake-pi-test--rpc
                     proc '(:id "next" :type "prompt" :message "next replay"))))
          (should (eq (plist-get next :success) t))
          (should (equal (plist-get (plist-get next :data) :disposition) "started")))
        (let* ((next-events
                (pilish-fake-pi-test--collect-until
                 proc (lambda (event)
                        (and (equal (plist-get event :type) "tool_execution_end")
                             (equal (plist-get event :toolCallId) "parent/3")))))
               (final (car (last (pilish-fake-pi-test--message-events
                                  next-events "message_end" "assistant")))))
          (should (equal (plist-get (plist-get final :message) :content)
                         [(:type "text" :text "FINAL-AFTER-PARENT")])))
        (should (eq (plist-get (pilish-fake-pi-test--rpc
                               proc '(:id "join-next" :type "abort")) :success) t))))))

(ert-deftest pilish-fake-pi-test-nested-tools-reset-stops-playback ()
  "Abort and session resets join playback even after wire settlement."
  (let ((target-dir (make-temp-file "pilish-fake-pi-nested-reset-" t)))
    (unwind-protect
        (dolist (boundary '("tool_execution_update" "agent_end" "agent_settled"))
          (dolist (command-type '("abort" "new_session" "switch_session"))
            (ert-info ((format "boundary=%s command=%s" boundary command-type))
              (pilish-fake-pi-test-with-process (proc "nested-tools")
                (let* ((state (pilish-fake-pi-test--rpc proc '(:type "get_state")))
                       (old-file (plist-get (plist-get state :data) :sessionFile))
                       (target (expand-file-name
                                (concat boundary "-" command-type ".jsonl") target-dir))
                       (prefix
                        (progn
                          (should (eq (plist-get (pilish-fake-pi-test--rpc
                                                 proc '(:type "prompt" :message "stop this replay"))
                                                :success) t))
                          (pilish-fake-pi-test--collect-until
                           proc (lambda (event)
                                  (and (equal (plist-get event :type) boundary)
                                       (or (member boundary '("agent_end" "agent_settled"))
                                           (equal (plist-get event :toolCallId) "parent/3"))))))))
                  (pilish-fake-pi-test--send
                   proc (append (list :id "reset" :type command-type)
                                (when (equal command-type "switch_session")
                                  (list :sessionPath target))))
                  (let* ((events (append prefix
                                         (pilish-fake-pi-test--collect-until
                                          proc (lambda (event)
                                                 (equal (plist-get event :id) "reset")))))
                         (ack (car (last events)))
                         (disk-at-ack (pilish-fake-pi-test--file-bytes old-file)))
                    (should (eq (plist-get ack :success) t))
                    (dolist (type '("agent_start" "agent_end" "agent_settled"))
                      (should (= (length (pilish-fake-pi-test--events-of-type events type)) 1)))
                    (unless (equal command-type "abort")
                      (should (eq (plist-get (plist-get ack :data) :cancelled) :false)))
                    ;; The late end is paused after settlement in this fixture.
                    ;; Observe past that pause: acknowledging a reset is a join,
                    ;; not merely a flag change or a promise of a later stop.
                    (should-not (pilish-test-wait-until
                                 (lambda () (process-get proc 'fake-pi-objects))
                                 0.75 0.01 proc))
                    (should (equal disk-at-ack
                                   (pilish-fake-pi-test--file-bytes old-file)))
                    (let* ((after (plist-get (pilish-fake-pi-test--rpc
                                             proc '(:type "get_state")) :data))
                           (after-file (plist-get after :sessionFile)))
                      (should (eq (plist-get after :isStreaming) :false))
                      (pcase command-type
                        ("abort" (should (equal after-file old-file)))
                        ("new_session"
                         (should-not (equal after-file old-file))
                         (should (= (plist-get after :messageCount) 0))
                         (should-not (file-exists-p after-file)))
                        ("switch_session"
                         (should (equal after-file target))
                         (should (= (plist-get after :messageCount) 0))
                         (let ((records (pilish-fake-pi-test--read-jsonl-file target)))
                           (should (= (length records) 1))
                           (pilish-fake-pi-test--assert-v3-header (aref records 0))))))))))))
      (delete-directory target-dir t))))

(ert-deftest pilish-fake-pi-test-nested-tools-settlement-keeps-replay-worker ()
  "Settlement exposes idle state without permitting overlapping literal replay."
  (pilish-fake-pi-test-with-process (proc "nested-tools")
    (should (eq (plist-get (pilish-fake-pi-test--rpc
                           proc '(:type "prompt" :message "first replay")) :success) t))
    (pilish-fake-pi-test--collect-until
     proc (lambda (event) (equal (plist-get event :type) "agent_settled")))
    (should (eq (plist-get (plist-get (pilish-fake-pi-test--rpc
                                     proc '(:type "get_state")) :data) :isStreaming)
                :false))
    (let ((response (pilish-fake-pi-test--rpc
                     proc '(:id "overlap" :type "prompt" :message "second replay"))))
      (should (eq (plist-get response :success) :false)))
    (pilish-fake-pi-test--collect-until
     proc (lambda (event)
            (and (equal (plist-get event :type) "tool_execution_end")
                 (equal (plist-get event :toolCallId) "parent/3"))))
    ;; A last wire record is not a thread join.  Use the stop acknowledgment
    ;; as a barrier before proving that the one worker can serve another prompt.
    (should (eq (plist-get (pilish-fake-pi-test--rpc proc '(:type "abort")) :success) t))
    (let ((response (pilish-fake-pi-test--rpc
                     proc '(:id "next" :type "prompt" :message "next replay"))))
      (should (eq (plist-get response :success) t)))
    (pilish-fake-pi-test--send proc '(:id "stop-next" :type "abort"))
    (pilish-fake-pi-test--collect-until
     proc (lambda (event) (equal (plist-get event :id) "stop-next")))))

(ert-deftest pilish-fake-pi-test-clear-queue-before-abort-stops-continuation ()
  "Only clear_queue discards steering; abort acknowledges after final settlement."
  (dolist (clear '(t nil))
    (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
      (pilish-fake-pi-test--send proc '(:type "prompt" :message "first"))
      (pilish-fake-pi-test--collect-until
       proc (lambda (obj)
              (equal (plist-get (plist-get obj :assistantMessageEvent) :type)
                     "text_delta")))
      (pilish-fake-pi-test--send proc '(:type "steer" :message "queued"))
      (when clear
        (pilish-fake-pi-test--send proc '(:id "clear" :type "clear_queue")))
      (pilish-fake-pi-test--send proc '(:type "abort"))
      (let* ((events (pilish-fake-pi-test--collect-until
                      proc (lambda (obj) (equal (plist-get obj :command) "abort"))))
             (types (pilish-fake-pi-test--event-types events))
             (clear-response (seq-find (lambda (obj) (equal (plist-get obj :id) "clear"))
                                       events)))
        (should (eq (plist-get (car (last events)) :success) t))
        (should (equal (last types 2) '("agent_settled" "response")))
        (should (= 1 (cl-count "agent_settled" types :test #'equal)))
        (should (= (if clear 1 2) (cl-count "agent_end" types :test #'equal)))
        (should (= (if clear 0 1) (cl-count "agent_start" types :test #'equal)))
        (when clear
          (should (equal (plist-get clear-response :data)
                         '(:steering ["queued"] :followUp [])))
          (should (equal (pilish-fake-pi-test--events-of-type events "queue_update")
                         '((:type "queue_update" :steering [] :followUp []))))))
      (let ((state (pilish-fake-pi-test--rpc proc '(:type "get_state"))))
        (should (eq (plist-get (plist-get state :data) :isStreaming) :false))))))

(ert-deftest pilish-fake-pi-test-abort-stops-streaming ()
  "abort stops an in-flight prompt and leaves the fake idle."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (should (eq (plist-get (pilish-fake-pi-test--rpc
                           proc '(:type "set_thinking_level" :level "high"))
                          :success)
                t))
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "abort me"))
    (should (equal (plist-get (pilish-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (agent-end nil)
          (aborted-message nil)
          (saw-stop-message-end nil)
          (saw-abort-response nil))
      (while (not seen-first-delta)
        (let* ((obj (pilish-fake-pi-test--pop-object proc))
               (event-type (plist-get obj :type))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal event-type "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (pilish-fake-pi-test--send proc '(:type "abort"))
      (while (not (and saw-abort-response agent-end))
        (let ((obj (pilish-fake-pi-test--pop-object proc)))
          (pcase (plist-get obj :type)
            ("response"
             (when (equal (plist-get obj :command) "abort")
               (setq saw-abort-response (eq (plist-get obj :success) t))))
            ("message_end"
             (pcase (plist-get (plist-get obj :message) :stopReason)
               ("aborted" (setq aborted-message (plist-get obj :message)))
               ("stop" (setq saw-stop-message-end t))))
            ("agent_end"
             (setq agent-end obj)))))
      (should saw-abort-response)
      (should aborted-message)
      (should (equal (plist-get aborted-message :errorMessage)
                     "Request was aborted"))
      (should (equal (plist-get aborted-message :thinkingLevel) "high"))
      (should-not saw-stop-message-end)
      (let ((messages (plist-get agent-end :messages)))
        (should (equal (aref messages (1- (length messages)))
                       aborted-message)))
      (pilish-fake-pi-test--assert-assistant-roundtrip proc (list aborted-message))
      (pilish-fake-pi-test--send proc '(:type "get_state"))
      (let* ((state (pilish-fake-pi-test--pop-object proc))
             (data (plist-get state :data)))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest pilish-fake-pi-test-tool-abort-ends-partial-assistant-message ()
  "Aborting tool generation emits its authoritative partial message first."
  (pilish-fake-pi-test-with-process (proc "tool-abort")
    (pilish-fake-pi-test--send proc
                                        '(:type "prompt" :message "abort tool"))
    (should (equal (plist-get (pilish-fake-pi-test--pop-object proc)
                              :command)
                   "prompt"))
    (let ((events nil)
          (delta-count 0)
          (abort-response nil)
          (agent-end nil))
      (while (< delta-count 3)
        (let ((event (pilish-fake-pi-test--pop-object proc)))
          (push event events)
          (when (and (equal (plist-get event :type) "message_update")
                     (equal (plist-get
                             (plist-get event :assistantMessageEvent) :type)
                            "toolcall_delta"))
            (setq delta-count (1+ delta-count)))))
      (pilish-fake-pi-test--send proc '(:type "abort"))
      (while (not (and abort-response agent-end))
        (let ((event (pilish-fake-pi-test--pop-object proc)))
          (push event events)
          (pcase (plist-get event :type)
            ("response"
             (when (equal (plist-get event :command) "abort")
               (setq abort-response event)))
            ("agent_end"
             (setq agent-end event)))))
      (setq events (nreverse events))
      (let* ((aborted-end
              (seq-find
               (lambda (event)
                 (and (equal (plist-get event :type) "message_end")
                      (equal (plist-get (plist-get event :message) :stopReason)
                             "aborted")))
               events))
             (aborted-message (and aborted-end
                                   (plist-get aborted-end :message)))
             (messages (plist-get agent-end :messages)))
        (should (eq (plist-get abort-response :success) t))
        (should aborted-message)
        (should (equal (plist-get aborted-message :errorMessage)
                       "Request was aborted"))
        (should (equal (plist-get aborted-message :thinkingLevel) "off"))
        (should (equal (plist-get (aref (plist-get aborted-message :content) 0)
                                  :name)
                       "read"))
        (should (< (seq-position events aborted-end #'eq)
                   (seq-position events agent-end #'eq)))
        (should-not
         (seq-find
          (lambda (event)
            (or (equal (plist-get event :type) "tool_execution_start")
                (and (equal (plist-get event :type) "message_update")
                     (equal (plist-get
                             (plist-get event :assistantMessageEvent) :type)
                            "toolcall_end"))))
          events))
        (should (equal (aref messages (1- (length messages)))
                       aborted-message))
        (pilish-fake-pi-test--assert-assistant-roundtrip proc (list aborted-message))))))

(ert-deftest pilish-fake-pi-test-steer-queues-another-turn ()
  "steer queues another user turn and delivers it before agent_end."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "first turn"))
    (should (equal (plist-get (pilish-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (saw-steer-response nil)
          (saw-agent-settled nil)
          (user-starts 0)
          (steered-reply nil))
      (while (not seen-first-delta)
        (let* ((obj (pilish-fake-pi-test--pop-object proc))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal (plist-get obj :type) "message_start")
                     (equal (plist-get (plist-get obj :message) :role) "user"))
            (setq user-starts (1+ user-starts)))
          (when (and (equal (plist-get obj :type) "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (pilish-fake-pi-test--send proc '(:type "steer" :message "second turn"))
      (while (not saw-agent-settled)
        (let ((obj (pilish-fake-pi-test--pop-object proc)))
          (pcase (plist-get obj :type)
            ("response"
             (when (equal (plist-get obj :command) "steer")
               (setq saw-steer-response (eq (plist-get obj :success) t))
               (should (equal (plist-get (plist-get obj :data) :disposition)
                              "queued"))))
            ("message_start"
             (when (equal (plist-get (plist-get obj :message) :role) "user")
               (setq user-starts (1+ user-starts))))
            ("message_end"
             (let ((message (plist-get obj :message)))
               (when (and (equal (plist-get message :role) "assistant")
                          (string-match-p "Steered fake reply for: second turn"
                                          (or (plist-get (aref (plist-get message :content) 0)
                                                         :text)
                                              "")))
                 (setq steered-reply t))))
            ("agent_settled"
             (setq saw-agent-settled t)))))
      (should saw-steer-response)
      (should saw-agent-settled)
      (should (= user-starts 2))
      (should steered-reply)
      (pilish-fake-pi-test--send proc '(:type "get_fork_messages"))
      (let* ((fork-response (pilish-fake-pi-test--pop-object proc))
             (messages (plist-get (plist-get fork-response :data) :messages)))
        (should (= (length messages) 2))
        (should (equal (plist-get (aref messages 1) :text) "second turn"))))))

(ert-deftest pilish-fake-pi-test-new-session-resets-count-and-path ()
  "new_session resets state and allocates a fresh path without writing bytes."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "before reset"))
    (should (equal (plist-get (pilish-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (pilish-fake-pi-test--collect-until
     proc (lambda (obj) (equal (plist-get obj :type) "agent_settled")))
    (pilish-fake-pi-test--send proc '(:type "get_state"))
    (let* ((before-state (pilish-fake-pi-test--pop-object proc))
           (before-data (plist-get before-state :data))
           (before-file (plist-get before-data :sessionFile)))
      (should (> (plist-get before-data :messageCount) 0))
      (should (file-exists-p before-file))
      (pilish-fake-pi-test--send proc '(:type "new_session"))
      (let ((response (pilish-fake-pi-test--pop-object proc)))
        (should (eq (plist-get response :success) t))
        (should (eq (plist-get (plist-get response :data) :cancelled) :false)))
      (pilish-fake-pi-test--send proc '(:type "get_state"))
      (let* ((after-state (pilish-fake-pi-test--pop-object proc))
             (after-data (plist-get after-state :data))
             (after-file (plist-get after-data :sessionFile)))
        (should (equal (plist-get after-data :messageCount) 0))
        (should (stringp after-file))
        (should (not (equal after-file before-file)))
        (should-not (file-exists-p after-file))
        (should (file-exists-p before-file))))))

(ert-deftest pilish-fake-pi-test-new-session-waits-for-old-run-to-stop ()
  "new_session should not leak stale streaming events after it succeeds."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "before reset"))
    (should (equal (plist-get (pilish-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (new-session-response nil))
      (while (not seen-first-delta)
        (let* ((obj (pilish-fake-pi-test--pop-object proc))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal (plist-get obj :type) "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (pilish-fake-pi-test--send proc '(:type "new_session"))
      (while (not new-session-response)
        (let ((obj (pilish-fake-pi-test--pop-object proc)))
          (when (and (equal (plist-get obj :type) "response")
                     (equal (plist-get obj :command) "new_session"))
            (setq new-session-response obj))))
      (sleep-for 0.2)
      (should-not (process-get proc 'fake-pi-objects))
      (pilish-fake-pi-test--send proc '(:type "get_state"))
      (let* ((state (pilish-fake-pi-test--pop-object proc))
             (data (plist-get state :data)))
        (should (equal (plist-get data :messageCount) 0))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest pilish-fake-pi-test-set-session-name-writes-session-info ()
  "set_session_name appends a real session_info entry that Emacs can parse."
  (pilish-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pilish-fake-pi-test--send proc '(:type "prompt" :message "session me"))
    (should (equal (plist-get (pilish-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (pilish-fake-pi-test--collect-until
     proc (lambda (obj) (equal (plist-get obj :type) "agent_settled")))
    (pilish-fake-pi-test--send proc '(:type "get_state"))
    (let* ((state (pilish-fake-pi-test--pop-object proc))
           (session-file (plist-get (plist-get state :data) :sessionFile)))
      (pilish-fake-pi-test--send
       proc '(:type "set_session_name" :name "  Fake\r\nHarness Session  "))
      (let ((response (pilish-fake-pi-test--pop-object proc)))
        (should (eq (plist-get response :success) t)))
      (with-temp-buffer
        (insert-file-contents session-file)
        (should (string-match-p "session_info" (buffer-string)))
        (should (string-match-p "Fake Harness Session" (buffer-string))))
      (let ((metadata
             (pilish-jsonl-read-session-info session-file)))
        (should metadata)
        (should (equal (plist-get metadata :name)
                       "Fake Harness Session"))))))

(ert-deftest pilish-fake-pi-test-extension-confirm-zero-timeout-disables-expiry ()
  "An override of 0 disables dialog expiry for manual debugging."
  (pilish-fake-pi-test-with-process
      (proc "extension-confirm" "--extension-timeout-ms" "0")
    (pilish-fake-pi-test--send
     proc '(:id "no-timeout" :type "prompt" :message "/test-confirm"))
    (let ((request (pilish-fake-pi-test--pop-object proc)))
      (should (equal (plist-get request :type) "extension_ui_request"))
      (should-not (plist-member request :timeout))
      (should-not (pilish-test-wait-until
                   (lambda () (process-get proc 'fake-pi-objects)) 0.2 0.01 proc))
      (pilish-fake-pi-test--send
       proc
       (list :type "extension_ui_response"
             :id (plist-get request :id)
             :confirmed t))
      (let* ((events (pilish-fake-pi-test--collect-until
                      proc (lambda (obj)
                             (and (equal (plist-get obj :type) "response")
                                  (equal (plist-get obj :id) "no-timeout")))))
             (custom-end (car (pilish-fake-pi-test--message-events
                               events "message_end" "custom"))))
        (should (equal (plist-get (plist-get custom-end :message) :content)
                       "CONFIRMED"))
        (should (equal (plist-get (plist-get (car (last events)) :data) :disposition)
                       "handled"))))))

(ert-deftest pilish-fake-pi-test-extension-confirm-honors-timeout-override ()
  "CLI timeout override allows a delayed extension UI response to succeed."
  (pilish-fake-pi-test-with-process
      (proc "extension-confirm" "--extension-timeout-ms" "500")
    (pilish-fake-pi-test--send
     proc '(:id "delayed" :type "prompt" :message "/test-confirm"))
    (let ((request (pilish-fake-pi-test--pop-object proc)))
      (should (equal (plist-get request :type) "extension_ui_request"))
      (should (equal (plist-get request :method) "confirm"))
      (should (= (plist-get request :timeout) 500))
      (sleep-for 0.15)
      (pilish-fake-pi-test--send
       proc
       (list :type "extension_ui_response"
             :id (plist-get request :id)
             :confirmed t))
      (let* ((events (pilish-fake-pi-test--collect-until
                      proc (lambda (obj)
                             (and (equal (plist-get obj :type) "response")
                                  (equal (plist-get obj :id) "delayed")))))
             (custom-end (car (pilish-fake-pi-test--message-events
                               events "message_end" "custom"))))
        (should (equal (plist-get (plist-get custom-end :message) :content)
                       "CONFIRMED"))
        (should (equal (plist-get (plist-get (car (last events)) :data) :disposition)
                       "handled"))))))

(provide 'pilish-fake-pi-test)
;;; pilish-fake-pi-test.el ends here
