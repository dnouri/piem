;;; pilish-menu-test.el --- Tests for pilish-menu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for session management, transient menu, model/thinking commands,
;; reconnect, and slash commands via RPC — the menu and session layer.

;;; Code:

(require 'ert)
(require 'pilish)
(require 'pilish-test-common)

;;; Version Checks

(ert-deftest pilish-test-normalize-version-ignores-prefix-and-suffix ()
  "Version parsing should keep only the numeric portion."
  (should (equal "0.12.0"
                 (pilish--normalize-version
                  "v0.12.0-15-gfe5214e6-builtin"))))

(ert-deftest pilish-test-version-at-least-p-rejects-old-built-in-version ()
  "Older transient versions should fail the minimum version check."
  (should-not (pilish--version-at-least-p "0.7.2.2" "0.9.0")))

(ert-deftest pilish-test-version-at-least-p-accepts-built-in-snapshot-format ()
  "Snapshot version strings with prefixes should still compare correctly."
  (should (pilish--version-at-least-p
           "v0.12.0-15-gfe5214e6-builtin"
           "0.9.0")))

;;; Session Management

(ert-deftest pilish-test-buffer-name-default-session ()
  "Buffer name without session name."
  (should (equal (pilish--buffer-name :chat "/tmp/proj/" nil)
                 "*pilish-chat:/tmp/proj/*")))

(ert-deftest pilish-test-buffer-name-named-session ()
  "Buffer name with session name."
  (should (equal (pilish--buffer-name :chat "/tmp/proj/" "feature")
                 "*pilish-chat:/tmp/proj/<feature>*")))

(ert-deftest pilish-test-clear-chat-buffer-resets-to-startup ()
  "Clearing chat buffer shows startup header and resets state."
  (with-temp-buffer
    (pilish-chat-mode)
    ;; Add some content
    (let ((inhibit-read-only t))
      (insert "Some existing content\nMore content"))
    ;; Set markers as if streaming happened
    (setq pilish--message-start-marker (point-marker))
    (setq pilish--streaming-marker (point-marker))
    ;; Clear the buffer
    (pilish--clear-chat-buffer)
    ;; Should have startup header
    (should (string-match-p "C-c C-c" (buffer-string)))
    ;; Markers should be reset
    (should (null pilish--message-start-marker))
    (should (null pilish--streaming-marker))))

(ert-deftest pilish-test-clear-chat-buffer-resets-session-state ()
  "Clearing chat buffer resets all session-specific state."
  (with-temp-buffer
    (pilish-chat-mode)
    ;; Set various session state as if we had an active session
    (setq pilish--session-name "My Named Session"
          pilish--cached-stats '(:messages 10 :cost 0.05)
          pilish--assistant-header-shown t
          pilish--followup-queue '("pending message")
          pilish--local-user-message "user text"
          pilish--aborted t
          pilish--extension-status '(("ext1" . "status"))
          pilish--working-message "Reading README..."
          pilish--unsupported-extension-ui-methods-warned '("setWidget")
          pilish--message-start-marker (point-marker)
          pilish--streaming-marker (point-marker)
          pilish--thinking-marker (point-marker)
          pilish--thinking-start-marker (point-marker)
          pilish--thinking-raw "pending"
          pilish--in-code-block t
          pilish--in-thinking-block t
          pilish--line-parse-state 'code-fence
          pilish--pending-tool-overlay (make-overlay 1 1)
          pilish--activity-phase "running")
    ;; Add entries to tool-args-cache and live tool registry
    (puthash "tool-1" '(:path "/test-a") pilish--tool-args-cache)
    (puthash "tool-2" '(:path "/test-b") pilish--tool-args-cache)
    (puthash "tool-1" '(:tool-call-id "tool-1") pilish--live-tool-blocks)
    (puthash "tool-2" '(:tool-call-id "tool-2") pilish--live-tool-blocks)
    ;; Clear the buffer
    (pilish--clear-chat-buffer)
    ;; All session state should be reset
    (should (null pilish--session-name))
    (should (null pilish--cached-stats))
    (should (null pilish--assistant-header-shown))
    (should (null pilish--followup-queue))
    (should (null pilish--local-user-message))
    (should (null pilish--aborted))
    (should (null pilish--extension-status))
    (should (null pilish--working-message))
    (should (null pilish--unsupported-extension-ui-methods-warned))
    (should (null pilish--message-start-marker))
    (should (null pilish--streaming-marker))
    (should (null pilish--thinking-marker))
    (should (null pilish--thinking-start-marker))
    (should (null pilish--thinking-raw))
    (should (null pilish--in-code-block))
    (should (null pilish--in-thinking-block))
    (should (eq pilish--line-parse-state 'line-start))
    (should (null pilish--pending-tool-overlay))
    (should (equal pilish--activity-phase "idle"))
    ;; Tool args cache and live tool registry should be empty
    (should (= 0 (hash-table-count pilish--tool-args-cache)))
    (should (= 0 (hash-table-count pilish--live-tool-blocks)))))

(ert-deftest pilish-test-clear-chat-buffer-removes-pi-owned-render-overlays ()
  "Clearing chat buffer removes stale pi-owned tool and diff overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "tool\n+ 1 added\n- 2 removed\n"))
    (let ((tool-ov (make-overlay 1 5 nil nil nil))
          (tool-count 0)
          (diff-count 0))
      (overlay-put tool-ov 'pilish-tool-block t)
      (setq pilish--pending-tool-overlay tool-ov)
      (pilish--apply-diff-overlays 6 (point-max))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'pilish-tool-block)
          (setq tool-count (1+ tool-count)))
        (when (overlay-get ov 'pilish-diff-overlay)
          (setq diff-count (1+ diff-count))))
      (should (= tool-count 1))
      (should (= diff-count 4)))
    (pilish--clear-chat-buffer)
    (let ((tool-count 0)
          (diff-count 0))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'pilish-tool-block)
          (setq tool-count (1+ tool-count)))
        (when (overlay-get ov 'pilish-diff-overlay)
          (setq diff-count (1+ diff-count))))
      (should (= tool-count 0))
      (should (= diff-count 0))
      (should-not pilish--pending-tool-overlay))))

(ert-deftest pilish-test-new-session-clears-buffer-from-different-context ()
  "New session clears buffer and updates state even when callback runs elsewhere.
This tests that the async callback properly captures the chat buffer reference,
not relying on current buffer context which may change before callback executes.
Also verifies that the new session-file is stored in state for reload to work."
  (let ((chat-buf (generate-new-buffer "*pilish-chat:/tmp/test-new-session/*"))
        (captured-callback nil)
        (proc (start-process "test-new-session-state" nil "cat")))
    (unwind-protect
        (progn
          ;; Set up chat buffer with content and old state
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc
                  pilish--state '(:session-file "/tmp/old-session.jsonl"))
            (let ((inhibit-read-only t))
              (insert "Existing conversation content\nMore content here")))
          ;; Mock the RPC to capture the new_session callback and handle get_state
          (cl-letf (((symbol-function 'pilish--get-process) (lambda () proc))
                    ((symbol-function 'pilish--get-chat-buffer) (lambda () chat-buf))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (cond
                        ((equal (plist-get cmd :type) "new_session")
                         (setq captured-callback cb))
                        ((equal (plist-get cmd :type) "get_state")
                         (funcall cb '(:success t :data (:sessionFile "/tmp/new-session.jsonl")))))))
                    ((symbol-function 'pilish--refresh-header) #'ignore))
            ;; Call new-session from the chat buffer
            (with-current-buffer chat-buf
              (pilish-new-session))
            ;; Simulate callback being called from a DIFFERENT buffer
            ;; (This is what happens in practice - callbacks run in arbitrary contexts)
            (with-temp-buffer
              (funcall captured-callback '(:success t :data (:cancelled :false)))))
          ;; Verify buffer was cleared
          (with-current-buffer chat-buf
            (should-not (string-match-p "Existing conversation" (buffer-string)))
            (should (string-match-p "C-c C-c" (buffer-string)))
            ;; Verify state was updated with new session file (the actual bug fix)
            (should (equal (plist-get pilish--state :session-file)
                           "/tmp/new-session.jsonl"))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-new-session-refuses-prompt-preflight ()
  "New session cannot discard a prompt whose acceptance is unresolved."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--process 'mock-proc pilish--status 'sending)
    (pilish--begin-prompt-start-wait)
    (let (rpc-called feedback)
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (pilish-new-session))
      (should-not rpc-called)
      (should (string-match-p "Cannot start a new session"
                              (or feedback ""))))))

(ert-deftest pilish-test-new-session-preserves-queued-followups ()
  "Reset refuses rather than silently discarding accepted local follow-ups."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--process 'mock-proc
          pilish--status 'streaming
          pilish--followup-queue '("keep me"))
    (let (rpc-called feedback)
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (pilish-new-session))
      (should-not rpc-called)
      (should (equal pilish--followup-queue '("keep me")))
      (should (string-match-p "queued follow-ups" (or feedback ""))))))

(ert-deftest pilish-test-new-session-can-reset-server-streaming ()
  "A deliberate reset still reaches Pi while its agent is streaming."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--process 'mock-proc
          pilish--status 'streaming)
    (let (callback)
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_process _command cb)
                   (setq callback cb))))
        (pilish-new-session)
        (should (functionp callback))
        (should (pilish--session-transition-active-p))))))

(ert-deftest pilish-test-new-session-blocks-work-until-response ()
  "A scheduled reset owns the session transition until its response."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--process 'mock-proc
          pilish--status 'idle)
    (let (callback)
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_process _command cb)
                   (setq callback cb))))
        (pilish-new-session)
        (should (functionp callback))
        (should (pilish--session-transition-active-p))))))

(ert-deftest pilish-test-find-session-returns-existing ()
  "pilish--find-session returns an existing chat buffer."
  (let* ((root (pilish-test--make-temp-directory
                "pilish-test-find-session-"))
         (buf (generate-new-buffer (pilish-test--chat-buffer-name root))))
    (unwind-protect
        (with-current-buffer buf
          (pilish-chat-mode)
          (setq default-directory root)
          (should (eq (pilish--find-session root nil) buf)))
      (kill-buffer buf)
      (ignore-errors (delete-directory root t)))))

(ert-deftest pilish-test-find-session-returns-nil-when-missing ()
  "pilish--find-session returns nil when no session exists."
  (should (null (pilish--find-session "/tmp/nonexistent-session-xyz/" nil))))

(ert-deftest pilish-test-pilish-reuses-existing-session ()
  "Calling pi twice returns same buffers."
  (pilish-test-with-mock-session "/tmp/pilish-test-reuse/"
    (let ((chat1 (get-buffer "*pilish-chat:/tmp/pilish-test-reuse/*"))
          (input1 (get-buffer "*pilish-input:/tmp/pilish-test-reuse/*")))
      (pilish)  ; call again
      (should (eq chat1 (get-buffer "*pilish-chat:/tmp/pilish-test-reuse/*")))
      (should (eq input1 (get-buffer "*pilish-input:/tmp/pilish-test-reuse/*"))))))

(ert-deftest pilish-test-named-session-separate-from-default ()
  "Named session creates separate buffers from default."
  (let ((default-directory "/tmp/pilish-test-named/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil))
              ((symbol-function 'pilish--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pilish)  ; default session
            (pilish "feature")  ; named session
            (should (get-buffer "*pilish-chat:/tmp/pilish-test-named/*"))
            (should (get-buffer "*pilish-chat:/tmp/pilish-test-named/<feature>*"))
            (should-not (eq (get-buffer "*pilish-chat:/tmp/pilish-test-named/*")
                            (get-buffer "*pilish-chat:/tmp/pilish-test-named/<feature>*"))))
        (ignore-errors (kill-buffer "*pilish-chat:/tmp/pilish-test-named/*"))
        (ignore-errors (kill-buffer "*pilish-input:/tmp/pilish-test-named/*"))
        (ignore-errors (kill-buffer "*pilish-chat:/tmp/pilish-test-named/<feature>*"))
        (ignore-errors (kill-buffer "*pilish-input:/tmp/pilish-test-named/<feature>*"))))))

(ert-deftest pilish-test-named-session-from-existing-pilish-buffer ()
  "Creating named session while in pi buffer creates new session, not reuse."
  (let ((default-directory "/tmp/pilish-test-from-pi/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil))
              ((symbol-function 'pilish--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pilish)  ; default session
            ;; Now switch INTO the pi input buffer and create a named session
            (with-current-buffer "*pilish-input:/tmp/pilish-test-from-pi/*"
              (pilish "feature"))  ; should create NEW session
            ;; Both sessions should exist
            (should (get-buffer "*pilish-chat:/tmp/pilish-test-from-pi/*"))
            (should (get-buffer "*pilish-chat:/tmp/pilish-test-from-pi/<feature>*"))
            ;; They should be different buffers
            (should-not (eq (get-buffer "*pilish-chat:/tmp/pilish-test-from-pi/*")
                            (get-buffer "*pilish-chat:/tmp/pilish-test-from-pi/<feature>*"))))
        (ignore-errors (kill-buffer "*pilish-chat:/tmp/pilish-test-from-pi/*"))
        (ignore-errors (kill-buffer "*pilish-input:/tmp/pilish-test-from-pi/*"))
        (ignore-errors (kill-buffer "*pilish-chat:/tmp/pilish-test-from-pi/<feature>*"))
        (ignore-errors (kill-buffer "*pilish-input:/tmp/pilish-test-from-pi/<feature>*"))))))

(ert-deftest pilish-test-quit-kills-both-buffers ()
  "pilish-quit kills both chat and input buffers."
  (pilish-test-with-mock-session "/tmp/pilish-test-quit/"
    (with-current-buffer "*pilish-input:/tmp/pilish-test-quit/*"
      (pilish-quit))
    (should-not (get-buffer "*pilish-chat:/tmp/pilish-test-quit/*"))
    (should-not (get-buffer "*pilish-input:/tmp/pilish-test-quit/*"))))

(defmacro pilish-test--with-quit-confirmable-session
    (binding-spec &rest body)
  "Run BODY with a pi session whose live process would prompt on quit.
BINDING-SPEC is (DIR CHAT-NAME INPUT-NAME PROC).  DIR is evaluated once."
  (declare (indent 1) (debug t))
  (let ((dir (nth 0 binding-spec))
        (chat-name (nth 1 binding-spec))
        (input-name (nth 2 binding-spec))
        (proc (nth 3 binding-spec))
        (dir-value (make-symbol "dir-value")))
    `(let* ((,dir-value ,dir)
            (,chat-name (pilish-test--chat-buffer-name ,dir-value))
            (,input-name (pilish-test--input-buffer-name ,dir-value))
            (,proc nil))
       (make-directory ,dir-value t)
       (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                 ((symbol-function 'pilish--start-process)
                  (lambda (_)
                    (setq ,proc (start-process "pi-test-quit" nil "cat"))
                    (set-process-query-on-exit-flag ,proc t)
                    ,proc))
                 ((symbol-function 'pilish--display-buffers) #'ignore))
         (unwind-protect
             (progn
               (let ((default-directory ,dir-value))
                 (pilish))
               (with-current-buffer ,chat-name
                 (set-process-buffer ,proc (current-buffer)))
               ,@body)
           (when (and ,proc (process-live-p ,proc))
             (delete-process ,proc))
           (pilish-test--kill-session-buffers ,dir-value))))))

(ert-deftest pilish-test-quit-cancelled-preserves-session ()
  "When user cancels quit confirmation, both buffers remain intact and linked."
  (pilish-test--with-quit-confirmable-session
      ("/tmp/pilish-test-quit-cancel/" chat-name input-name _proc)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (with-current-buffer input-name
        (should-error (pilish-quit) :type 'user-error)))
    (should (get-buffer chat-name))
    (should (get-buffer input-name))
    (with-current-buffer chat-name
      (should (eq (pilish--get-input-buffer)
                  (get-buffer input-name))))
    (with-current-buffer input-name
      (should (eq (pilish--get-chat-buffer)
                  (get-buffer chat-name))))))

(ert-deftest pilish-test-quit-confirmed-kills-both ()
  "When user confirms quit, both buffers are killed without double-prompting."
  (let ((prompt-count 0))
    (pilish-test--with-quit-confirmable-session
        ("/tmp/pilish-test-quit-confirm/" chat-name input-name _proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   t)))
        (with-current-buffer input-name
          (pilish-quit)))
      (should-not (get-buffer chat-name))
      (should-not (get-buffer input-name))
      (should (<= prompt-count 1)))))

(ert-deftest pilish-test-quit-without-confirmation-kills-both-without-prompt ()
  "When configured, quitting a live session kills both buffers without prompting."
  (let ((pilish-quit-without-confirmation t))
    (pilish-test--with-quit-confirmable-session
        ("/tmp/pilish-test-quit-no-confirm/" chat-name input-name _proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _)
                   (ert-fail "pilish-quit prompted unexpectedly"))))
        (with-current-buffer input-name
          (pilish-quit)))
      (should-not (get-buffer chat-name))
      (should-not (get-buffer input-name)))))

(ert-deftest pilish-test-kill-chat-cancelled-preserves-session ()
  "Killing chat buffer asks before terminating its live process."
  (let ((prompt-count 0))
    (pilish-test--with-quit-confirmable-session
        ("/tmp/pilish-test-kill-chat-cancel/" chat-name input-name proc)
      ;; GUI test helpers disable this globally; this test needs the default
      ;; Emacs process-buffer query to exercise the chat-buffer contract.
      (let ((kill-buffer-query-functions
             (if (memq #'process-kill-buffer-query-function
                       kill-buffer-query-functions)
                 kill-buffer-query-functions
               (cons #'process-kill-buffer-query-function
                     kill-buffer-query-functions))))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_)
                     (cl-incf prompt-count)
                     nil)))
          (should-not (kill-buffer chat-name))))
      (should (= prompt-count 1))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc))
      (should (process-query-on-exit-flag proc)))))

(ert-deftest pilish-test-kill-chat-prompts-even-when-process-noquery ()
  "Direct chat-buffer kills still use Pi's own prompt for noquery processes."
  (let ((prompt-count 0))
    (pilish-test--with-quit-confirmable-session
        ("/tmp/pilish-test-kill-chat-noquery/" chat-name input-name proc)
      (set-process-query-on-exit-flag proc nil)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   nil)))
        (should-not (kill-buffer chat-name)))
      (should (= prompt-count 1))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc))
      (should-not (process-query-on-exit-flag proc)))))

(ert-deftest pilish-test-kill-emacs-query-is-installed ()
  "Exiting Emacs consults pi sessions via `kill-emacs-query-functions'."
  (should (memq #'pilish--session-kill-emacs-query
                kill-emacs-query-functions)))

(ert-deftest pilish-test-kill-emacs-query-prompts-for-live-session ()
  "Exiting Emacs asks once when a session process is still running."
  (let ((prompt-count 0))
    (pilish-test--with-quit-confirmable-session
        ("/tmp/pilish-test-kill-emacs-query/" chat-name input-name proc)
      (cl-letf (((symbol-function 'process-list) (lambda () (list proc)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (cl-incf prompt-count)
                   (should (equal prompt
                                  "Pi session has a running process; exit anyway? "))
                   nil)))
        (should-not (pilish--session-kill-emacs-query))
        (should (= prompt-count 1)))
      (cl-letf (((symbol-function 'process-list) (lambda () (list proc)))
                ((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (should (pilish--session-kill-emacs-query)))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc)))))

(ert-deftest pilish-test-kill-emacs-query-asks-only-when-required ()
  "Exit stays silent for dead, skipped, or configured-away processes."
  (pilish-test--with-quit-confirmable-session
      ("/tmp/pilish-test-kill-emacs-silent/" _chat _input proc)
    (cl-letf (((symbol-function 'process-list) (lambda () (list proc)))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (ert-fail "kill-emacs query prompted unexpectedly"))))
      ;; Intentional teardown marks the process; exit must not ask again.
      (pilish--skip-process-kill-confirmation proc)
      (should (pilish--session-kill-emacs-query))
      (process-put proc 'pilish-skip-kill-confirmation nil)
      ;; Opt-out defcustom applies to Emacs exit as it does to quit.
      (let ((pilish-quit-without-confirmation t))
        (should (pilish--session-kill-emacs-query)))
      ;; A dead process is nothing to protect.
      (delete-process proc)
      (should (pilish--session-kill-emacs-query)))))

(ert-deftest pilish-test-kill-input-cancelled-preserves-session ()
  "Killing input buffer asks before terminating the linked live process."
  (let ((prompt-count 0))
    (pilish-test--with-quit-confirmable-session
        ("/tmp/pilish-test-kill-input-cancel/" chat-name input-name proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   nil)))
        (should-not (kill-buffer input-name)))
      (should (= prompt-count 1))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc))
      (should (process-query-on-exit-flag proc)))))

(ert-deftest pilish-test-kill-input-confirmed-kills-session ()
  "Confirming input buffer kill terminates the linked session once."
  (let ((prompt-count 0))
    (pilish-test--with-quit-confirmable-session
        ("/tmp/pilish-test-kill-input-confirm/" chat-name input-name proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   t)))
        (should (kill-buffer input-name)))
      (should (= prompt-count 1))
      (should-not (get-buffer chat-name))
      (should-not (get-buffer input-name))
      (should-not (process-live-p proc)))))

(ert-deftest pilish-test-kill-chat-kills-input ()
  "Killing chat buffer also kills input buffer."
  (pilish-test-with-mock-session "/tmp/pilish-test-linked/"
    (kill-buffer "*pilish-chat:/tmp/pilish-test-linked/*")
    (should-not (get-buffer "*pilish-input:/tmp/pilish-test-linked/*"))))

(ert-deftest pilish-test-kill-input-kills-chat ()
  "Killing input buffer also kills chat buffer."
  (pilish-test-with-mock-session "/tmp/pilish-test-linked2/"
    (kill-buffer "*pilish-input:/tmp/pilish-test-linked2/*")
    (should-not (get-buffer "*pilish-chat:/tmp/pilish-test-linked2/*"))))

;;; Transient Menu

(ert-deftest pilish-test-transient-bound-to-key ()
  "C-c C-p is bound to pilish-menu in input mode."
  (with-temp-buffer
    (pilish-input-mode)
    (should (eq (key-binding (kbd "C-c C-p")) 'pilish-menu))))

(ert-deftest pilish-test-attach-routes-through-menu-submenu ()
  "Menu `a' opens the attach submenu; its `i' attaches prompt images."
  (should (eq (pilish-test--menu-suffix-command 'pilish-menu "a")
              'pilish-attach-menu))
  (should (eq (pilish-test--menu-suffix-command 'pilish-attach-menu "i")
              'pilish-attach-image)))

;;; Chat Navigation

(ert-deftest pilish-test-chat-has-navigation-keys ()
  "Chat mode has n/p for navigation, TAB for at-point toggles, f for fork."
  (with-temp-buffer
    (pilish-chat-mode)
    (should (eq (key-binding "n") 'pilish-next-message))
    (should (eq (key-binding "p") 'pilish-previous-message))
    (should (eq (key-binding (kbd "TAB")) 'pilish-toggle-tool-section))
    (should (eq (key-binding "f") 'pilish-fork-at-point))))

;;; Reconnect Tests

(ert-deftest pilish-test-reload-restarts-process ()
  "Reload starts new process when old process is dead."
  (let* ((started-new-process nil)
         (switch-session-called nil)
         (session-path-used nil)
         (chat-buf (get-buffer-create "*pilish-test-reconnect-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            ;; Set up state with session file (simulating previous get_state)
            (setq pilish--state '(:session-file "/tmp/test-session.json"
                                           :model (:name "test-model")))
            ;; Set up dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pilish-test-wait-for-process-exit dead-proc))
              (setq pilish--process dead-proc))
            ;; Mock functions
            (cl-letf (((symbol-function 'pilish--start-process)
                       (lambda (_dir)
                         (setq started-new-process t)
                         (let ((proc (start-process "test-new" nil "cat")))
                           (set-process-query-on-exit-flag proc nil)
                           proc)))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc msg _cb)
                         (when (equal (plist-get msg :type) "switch_session")
                           (setq switch-session-called t
                                 session-path-used (plist-get msg :sessionPath))))))
              ;; Call reload
              (pilish-reload)
              ;; Verify
              (should started-new-process)
              (should switch-session-called)
              (should (equal session-path-used "/tmp/test-session.json")))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and pilish--process (process-live-p pilish--process))
            (delete-process pilish--process)))
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-sends-process-local-session-path-for-remote-session ()
  "Reload sends process-local sessionPath when the Emacs state is remote."
  (let* ((session-path-used nil)
         (new-proc nil)
         (chat-buf (get-buffer-create "*pilish-test-remote-reload-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-chat-session-identity
             "/ssh:pi-host:/home/pi/project/")
            (setq pilish--state
                  '(:session-file "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl"
                    :model (:name "test-model")))
            (let ((dead-proc (start-process "test-remote-reload-dead" nil "true")))
              (should (pilish-test-wait-for-process-exit dead-proc))
              (setq pilish--process dead-proc))
            (cl-letf (((symbol-function 'pilish--start-process)
                       (lambda (_dir)
                         (setq new-proc
                               (start-process "test-remote-reload-new" nil "cat"))))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc msg _cb)
                         (when (equal (plist-get msg :type) "switch_session")
                           (setq session-path-used
                                 (plist-get msg :sessionPath))))))
              (pilish-reload)
              (should (equal session-path-used
                             "/home/pi/.pi/sessions/current.jsonl")))))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and pilish--process
                     (process-live-p pilish--process))
            (delete-process pilish--process)))
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-validates-session-path-before-killing-process ()
  "Reload leaves the old process alive when path validation fails."
  (let* ((started-new-process nil)
         (alive-proc nil)
         (project-dir (pilish-test--make-temp-directory
                       "pilish-test-reload-validate-project-"))
         (chat-buf (get-buffer-create "*pilish-test-reload-validate-chat*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (pilish--set-chat-session-identity project-dir)
          (setq pilish--state
                '(:session-file "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl"))
          (setq alive-proc (start-process "test-reload-validate-alive" nil "cat")
                pilish--process alive-proc)
          (cl-letf (((symbol-function 'pilish--start-process)
                     (lambda (_dir)
                       (setq started-new-process t)
                       (ert-fail "Reload started a process before validation failed")))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (&rest _)
                       (ert-fail "Reload switched session before validation failed"))))
            (should-error (pilish-reload) :type 'user-error)
            (should-not started-new-process)
            (should (process-live-p alive-proc))))
      (when (and alive-proc (process-live-p alive-proc))
        (delete-process alive-proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (delete-directory project-dir t))))

(ert-deftest pilish-test-reload-keeps-old-process-until-switch-succeeds ()
  "Reload keeps the old process until the fresh process owns the session."
  (let* ((started-new-process nil)
         (old-process-killed nil)
         (chat-buf (get-buffer-create "*pilish-test-reload-alive-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            ;; Set up state with session file
            (setq pilish--state '(:session-file "/tmp/test-session.json"))
            ;; Set up alive process
            (let ((alive-proc (start-process "test-alive" nil "cat")))
              (set-process-query-on-exit-flag alive-proc nil)
              (setq pilish--process alive-proc)
              (cl-letf (((symbol-function 'pilish--start-process)
                         (lambda (_dir)
                           (setq started-new-process t)
                           (let ((proc (start-process "test-new" nil "cat")))
                             (set-process-query-on-exit-flag proc nil)
                             proc)))
                        ((symbol-function 'pilish--rpc-async)
                         (lambda (_proc _msg _cb) nil)))
                ;; Call reload
                (pilish-reload)
                ;; Verify - SHOULD start new process even when old was alive.
                (should started-new-process)
                ;; The old process remains current until switch_session succeeds.
                (should (process-live-p alive-proc))))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and pilish--process (process-live-p pilish--process))
            (delete-process pilish--process)))
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-switch-failure-keeps-old-process ()
  "A failed reload switch does not attach the fresh process to the UI."
  (let* ((old-proc nil)
         (new-proc nil)
         (shown-message nil)
         (chat-buf (get-buffer-create "*pilish-test-reload-failure-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--state '(:session-file "/tmp/test-session.json"))
            (setq old-proc (start-process "test-reload-failure-old" nil "cat")
                  pilish--process old-proc))
          (cl-letf (((symbol-function 'pilish--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-failure-new" nil "cat"))))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc msg cb)
                       (when (equal (plist-get msg :type) "switch_session")
                         (funcall cb '(:success :false :error "nope")))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (with-current-buffer chat-buf
              (pilish-reload)
              (should (eq pilish--process old-proc))
              (should (process-live-p old-proc))
              (should-not (process-live-p new-proc))
              (should-not (pilish--session-transition-active-p))
              (should (equal shown-message
                             "Pi: Failed to reload - nope")))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-cancelled-keeps-old-process ()
  "A cancelled reload switch keeps the old process current and live."
  (let* ((old-proc nil)
         (new-proc nil)
         (shown-message nil)
         (chat-buf (get-buffer-create
                    "*pilish-test-reload-cancelled-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--state '(:session-file "/tmp/test-session.json"))
            (setq old-proc (start-process "test-reload-cancelled-old" nil "cat")
                  pilish--process old-proc))
          (cl-letf (((symbol-function 'pilish--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-cancelled-new" nil "cat"))))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc msg cb)
                       (when (equal (plist-get msg :type) "switch_session")
                         (funcall cb '(:success t :data (:cancelled t))))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (with-current-buffer chat-buf
              (pilish-reload)
              (should (eq pilish--process old-proc))
              (should (process-live-p old-proc))
              (should-not (process-live-p new-proc))
              (should-not (pilish--session-transition-active-p))
              (should (equal shown-message "Pi: Reload cancelled")))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-shows-immediate-feedback ()
  "Reload reports progress before the async session switch finishes."
  (let* ((shown-message nil)
         (chat-buf (get-buffer-create "*pilish-test-reload-feedback-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--state '(:session-file "/tmp/test-session.json"))
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pilish-test-wait-for-process-exit dead-proc))
              (setq pilish--process dead-proc))
            (cl-letf (((symbol-function 'pilish--start-process)
                       (lambda (_dir)
                         (let ((proc (start-process "test-new" nil "cat")))
                           (set-process-query-on-exit-flag proc nil)
                           proc)))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _msg _cb) nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish-reload)
              (should (equal shown-message "Pi: Reloading...")))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and pilish--process (process-live-p pilish--process))
            (delete-process pilish--process)))
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-fails-without-session-file ()
  "Reload shows error when no session file in state."
  (let* ((error-shown nil)
         (chat-buf (get-buffer-create "*pilish-test-reconnect-no-session*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            ;; State without session file
            (setq pilish--state '(:model (:name "test-model")))
            ;; Dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pilish-test-wait-for-process-exit dead-proc))
              (setq pilish--process dead-proc))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest _args)
                         (when (string-match-p "No session" fmt)
                           (setq error-shown t)))))
              (pilish-reload)
              (should error-shown))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-rebuilds-session-history ()
  "Reload replays current session history, including thinking and tool output."
  (let* ((chat-buf (get-buffer-create "*pilish-test-reload-history-chat*"))
         (rpc-calls nil)
         (messages [(:role "user"
                     :content [(:type "text" :text "How should reload behave?")]
                     :timestamp 1704067200000)
                    (:role "assistant"
                     :content [(:type "text" :text "Answer first.")
                               (:type "thinking" :thinking "Need to double-check.")
                               (:type "toolCall" :id "tc1"
                                :name "read"
                                :arguments (:path "foo.el"))]
                     :timestamp 1704067201000)
                    (:role "toolResult" :toolCallId "tc1"
                     :toolName "read"
                     :content [(:type "text" :text "(defun foo ())")]
                     :isError :json-false
                     :timestamp 1704067202000)])
         (new-proc nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--thinking-display 'visible
                  pilish--state '(:session-file "test-session.jsonl"
                                           :model (:name "test-model")))
            (let ((inhibit-read-only t))
              (insert "STALE CONTENT\n"))
            (let ((dead-proc (start-process "test-reload-history-dead" nil "true")))
              (should (pilish-test-wait-for-process-exit dead-proc))
              (setq pilish--process dead-proc)))
          (cl-letf (((symbol-function 'pilish--start-process)
                     (lambda (_dir)
                       (setq new-proc (start-process "test-reload-history-new" nil "cat"))
                       new-proc))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (push (plist-get cmd :type) rpc-calls)
                       (pcase (plist-get cmd :type)
                         ("switch_session"
                          (funcall cb '(:success t :data (:cancelled :false))))
                         ("get_state"
                          (funcall cb '(:success t
                                        :data (:model (:name "reloaded-model")
                                               :thinkingLevel "medium"
                                               :isStreaming :json-false
                                               :isCompacting :json-false
                                               :sessionId "reload-session"
                                               :sessionFile "test-session.jsonl"
                                               :messageCount 3
                                               :pendingMessageCount 0))))
                         ("get_messages"
                          (funcall cb (list :success t :data (list :messages messages))))
                         ("get_commands"
                          (funcall cb '(:success t :data (:commands []))))
                         (_ (ert-fail (format "Unexpected RPC during reload test: %S" cmd))))))
                    ((symbol-function 'pilish--update-session-name-from-file) #'ignore)
                    ((symbol-function 'pilish--refresh-header) #'ignore)
                    ((symbol-function 'pilish--rebuild-commands-menu) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (pilish-reload)))
          (with-current-buffer chat-buf
            (let ((text (buffer-string)))
              (should-not (string-match-p "STALE CONTENT" text))
              (should (string-match-p "How should reload behave\\?" text))
              (should (string-match-p "Answer first\\." text))
              (should (string-match-p "> Need to double-check\\." text))
              (should (string-match-p "read foo\\.el" text))
              (should (string-match-p "(defun foo ())" text))))
          (should (member "get_messages" rpc-calls)))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and pilish--process (process-live-p pilish--process))
            (delete-process pilish--process)))
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reload-transition-waits-for-state-and-history ()
  "Reload keeps sends blocked until both state and history callbacks settle."
  (let* ((dir (pilish-test--make-temp-directory
               "pilish-test-reload-transition-"))
         (session-file (expand-file-name "current.jsonl" dir))
         (chat-buf (generate-new-buffer "*pilish-test-reload-transition-chat*"))
         (input-buf (generate-new-buffer "*pilish-test-reload-transition-input*"))
         (old-proc nil)
         (new-proc nil)
         (switch-cb nil)
         (state-cb nil)
         (history-cb nil)
         (sent-text nil))
    (unwind-protect
        (progn
          (with-temp-file session-file (insert ""))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-chat-session-identity dir)
            (pilish--set-input-buffer input-buf)
            (setq old-proc (start-process "test-reload-transition-old" nil "cat")
                  pilish--process old-proc
                  pilish--status 'idle
                  pilish--state (list :session-file session-file)))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (pilish--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'pilish--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-transition-new" nil "cat"))
                       new-proc))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("switch_session" (setq switch-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         ("get_commands" nil)
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'pilish--prepare-and-send)
                     (lambda (text &optional _queued)
                       (setq sent-text text)))
                    ((symbol-function 'pilish--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'pilish--refresh-header) #'ignore)
                    ((symbol-function 'pilish--rebuild-commands-menu) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (pilish-reload))
            (should switch-cb)
            (funcall switch-cb '(:success t :data (:cancelled :false)))
            (should state-cb)
            (should history-cb)
            (with-current-buffer chat-buf
              (should (pilish--session-transition-active-p)))
            (with-current-buffer input-buf
              (insert "prompt while reloading")
              (pilish-send)
              (should (equal (buffer-string) "prompt while reloading")))
            (should-not sent-text)
            (funcall state-cb
                     `(:success t
                       :data (:model (:name "model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "reloaded"
                              :sessionFile ,session-file
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (pilish--session-transition-active-p)))
            (with-current-buffer input-buf
              (pilish-send)
              (should (equal (buffer-string) "prompt while reloading")))
            (should-not sent-text)
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (pilish--session-transition-active-p)))
            (with-current-buffer input-buf
              (pilish-send))
            (should (equal sent-text "prompt while reloading"))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (pilish-test--kill-live-buffers input-buf chat-buf)
      (delete-directory dir t))))

(ert-deftest pilish-test-reload-command-fetch-error-waits-for-refresh ()
  "Command-fetch scheduling errors do not finish a reload transition early."
  (let* ((dir (pilish-test--make-temp-directory
               "pilish-test-reload-command-fetch-"))
         (session-file (expand-file-name "current.jsonl" dir))
         (chat-buf (generate-new-buffer
                    "*pilish-test-reload-command-fetch-chat*"))
         (old-proc nil)
         (new-proc nil)
         (switch-cb nil)
         (state-cb nil)
         (history-cb nil))
    (unwind-protect
        (progn
          (with-temp-file session-file (insert ""))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-chat-session-identity dir)
            (setq old-proc (start-process "test-reload-cmd-old" nil "cat")
                  pilish--process old-proc
                  pilish--status 'idle
                  pilish--state (list :session-file session-file)))
          (cl-letf (((symbol-function 'pilish--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-cmd-new" nil "cat"))))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("switch_session" (setq switch-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'pilish--fetch-commands)
                     (lambda (&rest _)
                       (error "commands unavailable")))
                    ((symbol-function 'pilish--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'pilish--display-session-history)
                     #'ignore)
                    ((symbol-function 'pilish--refresh-header) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (pilish-reload))
            (should switch-cb)
            (funcall switch-cb '(:success t :data (:cancelled :false)))
            (should state-cb)
            (should history-cb)
            (with-current-buffer chat-buf
              (should (pilish--session-transition-active-p)))
            (funcall state-cb
                     `(:success t
                       :data (:model (:name "model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "reloaded"
                              :sessionFile ,session-file
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (pilish--session-transition-active-p)))
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (pilish--session-transition-active-p)))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (pilish-test--kill-live-buffers chat-buf)
      (delete-directory dir t))))

(ert-deftest pilish-test-transition-refresh-failure-releases ()
  "A failed state/history refresh still unlocks the active transition."
  (let ((chat-buf (generate-new-buffer "*pilish-test-transition-failure*"))
        (proc 'mock-proc)
        (state-cb nil)
        (history-cb nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc)
            (let ((generation (pilish--begin-session-transition proc)))
              (cl-letf (((symbol-function 'pilish--rpc-async)
                         (lambda (_proc cmd cb)
                           (pcase (plist-get cmd :type)
                             ("get_state" (setq state-cb cb))
                             ("get_messages" (setq history-cb cb))
                             (_ (ert-fail (format "Unexpected RPC: %S" cmd)))))))
                (pilish--refresh-transition-state-and-history
                 proc chat-buf generation))))
          (should state-cb)
          (should history-cb)
          (funcall state-cb '(:success :false :error "state failed"))
          (with-current-buffer chat-buf
            (should (pilish--session-transition-active-p)))
          (funcall history-cb '(:success :false :error "history failed"))
          (with-current-buffer chat-buf
            (should-not (pilish--session-transition-active-p))))
      (pilish-test--kill-live-buffers chat-buf))))

(ert-deftest pilish-test-stale-transition-refresh-cannot-finish-newer ()
  "An old transition callback cannot unlock a newer transition generation."
  (let ((chat-buf (generate-new-buffer "*pilish-test-stale-transition*"))
        (proc 'mock-proc)
        (state-cb nil)
        (history-cb nil)
        (new-generation nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc)
            (let ((old-generation (pilish--begin-session-transition proc)))
              (cl-letf (((symbol-function 'pilish--rpc-async)
                         (lambda (_proc cmd cb)
                           (pcase (plist-get cmd :type)
                             ("get_state" (setq state-cb cb))
                             ("get_messages" (setq history-cb cb))
                             (_ (ert-fail (format "Unexpected RPC: %S" cmd)))))))
                (pilish--refresh-transition-state-and-history
                 proc chat-buf old-generation))))
          (funcall state-cb '(:success :false :error "state failed"))
          (with-current-buffer chat-buf
            (setq new-generation
                  (pilish--begin-session-transition proc)))
          (funcall history-cb '(:success :false :error "history failed"))
          (with-current-buffer chat-buf
            (should (pilish--session-transition-active-p))
            (should (= pilish--session-transition-generation
                       new-generation))
            (pilish--finish-session-transition new-generation)))
      (pilish-test--kill-live-buffers chat-buf))))

(ert-deftest pilish-test-load-session-history-ignores-stale-older-response ()
  "Only the newest in-flight history request may rebuild the chat buffer."
  (let* ((chat-buf (get-buffer-create "*pilish-test-history-load-generation*"))
         (callbacks nil)
         (proc (start-process "test-history-load-generation" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_messages"))
                       (push cb callbacks)))
                    ((symbol-function 'pilish--refresh-header) #'ignore))
            (pilish--load-session-history proc nil chat-buf)
            (pilish--load-session-history proc nil chat-buf))
          (should (= 2 (length callbacks)))
          (let ((newer (car callbacks))
                (older (cadr callbacks))
                (newer-messages [(:role "assistant"
                                  :content [(:type "text" :text "Newer history")]
                                  :timestamp 1704067200000)])
                (older-messages [(:role "assistant"
                                  :content [(:type "text" :text "Older history")]
                                  :timestamp 1704067201000)]))
            (funcall newer (list :success t :data (list :messages newer-messages)))
            (with-current-buffer chat-buf
              (should (string-match-p "Newer history" (buffer-string)))
              (should-not (string-match-p "Older history" (buffer-string))))
            (funcall older (list :success t :data (list :messages older-messages)))
            (with-current-buffer chat-buf
              (should (string-match-p "Newer history" (buffer-string)))
              (should-not (string-match-p "Older history" (buffer-string))))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-reset-session-state-keeps-history-load-generation-monotonic ()
  "Resetting session state must not let old history callbacks collide with new ones."
  (let* ((chat-buf (get-buffer-create "*pilish-test-history-reset-generation*"))
         (callbacks nil)
         (proc (start-process "test-history-reset-generation" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_messages"))
                       (push cb callbacks)))
                    ((symbol-function 'pilish--refresh-header) #'ignore))
            (pilish--load-session-history proc nil chat-buf)
            (with-current-buffer chat-buf
              (pilish--reset-session-state)
              (setq pilish--process proc))
            (pilish--load-session-history proc nil chat-buf))
          (should (= 2 (length callbacks)))
          (let ((newer (car callbacks))
                (older (cadr callbacks)))
            (funcall newer '(:success t :data (:messages [(:role "assistant"
                                                   :content [(:type "text" :text "New session history")]
                                                   :timestamp 1704067200000)])))
            (with-current-buffer chat-buf
              (should (string-match-p "New session history" (buffer-string))))
            (funcall older '(:success t :data (:messages [(:role "assistant"
                                                   :content [(:type "text" :text "Old session history")]
                                                   :timestamp 1704067201000)])))
            (with-current-buffer chat-buf
              (should (string-match-p "New session history" (buffer-string)))
              (should-not (string-match-p "Old session history" (buffer-string))))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-refresh-session-state-ignores-stale-older-response ()
  "Only the newest async get_state refresh may update the chat buffer state."
  (let* ((chat-buf (get-buffer-create "*pilish-test-refresh-session-state*"))
         (callbacks nil)
         (proc (start-process "test-refresh-session-state" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_state"))
                       (push cb callbacks)))
                    ((symbol-function 'pilish--update-session-name-from-file) #'ignore)
                    ((symbol-function 'force-mode-line-update) #'ignore))
            (pilish--refresh-session-state proc chat-buf)
            (pilish--refresh-session-state proc chat-buf))
          (should (= 2 (length callbacks)))
          (let ((newer (car callbacks))
                (older (cadr callbacks))
                (newer-response
                 '(:success t :data (:model (:name "new")
                                    :thinkingLevel "medium"
                                    :isStreaming :json-false
                                    :isCompacting :json-false
                                    :sessionId "new-session"
                                    :sessionFile "new-session.jsonl"
                                    :messageCount 3
                                    :pendingMessageCount 0)))
                (older-response
                 '(:success t :data (:model (:name "old")
                                    :thinkingLevel "low"
                                    :isStreaming :json-false
                                    :isCompacting :json-false
                                    :sessionId "old-session"
                                    :sessionFile "old-session.jsonl"
                                    :messageCount 1
                                    :pendingMessageCount 0))))
            (funcall older older-response)
            (with-current-buffer chat-buf
              (should-not pilish--state))
            (funcall newer newer-response)
            (with-current-buffer chat-buf
              (should (equal (plist-get pilish--state :session-file)
                             (expand-file-name
                              "new-session.jsonl"
                              (pilish--chat-session-directory)))))
            (funcall older older-response)
            (with-current-buffer chat-buf
              (should (equal (plist-get pilish--state :session-file)
                             (expand-file-name
                              "new-session.jsonl"
                              (pilish--chat-session-directory)))))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-refresh-session-state-skips-duplicate-name-scan ()
  "Refreshing state does not re-read the same session file for its name."
  (let* ((chat-buf (get-buffer-create "*pilish-test-refresh-name*"))
         (session-file "/tmp/pi-session.jsonl")
         (update-calls 0)
         (proc (start-process "test-refresh-session-name" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_state"))
                       (funcall cb `(:success t
                                     :data (:model (:name "model")
                                            :thinkingLevel "medium"
                                            :isStreaming :json-false
                                            :isCompacting :json-false
                                            :sessionId "session-id"
                                            :sessionFile ,session-file
                                            :messageCount 0
                                            :pendingMessageCount 0)))))
                    ((symbol-function 'pilish--update-session-name-from-file)
                     (lambda (_session-file)
                       (setq update-calls (1+ update-calls))
                       '(:name "Cached name")))
                    ((symbol-function 'force-mode-line-update) #'ignore))
            (pilish--refresh-session-state proc chat-buf session-file))
          (should (= update-calls 1)))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-send-resets-activity-when-process-dead ()
  "Sending when process is dead resets activity phase and status."
  (let ((chat-buf (get-buffer-create "*pilish-test-process-dead*"))
        (input-buf (get-buffer-create "*pilish-test-process-dead-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--input-buffer input-buf
                  pilish--activity-phase "running"
                  pilish--status 'idle)
            ;; Set up dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pilish-test-wait-for-process-exit dead-proc))
              (setq pilish--process dead-proc)))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "test message")
            (pilish-send))
          ;; Verify activity phase and status reset
          (with-current-buffer chat-buf
            (should (equal pilish--activity-phase "idle"))
            (should (eq pilish--status 'idle))))
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
      (when (buffer-live-p input-buf) (kill-buffer input-buf)))))

;;; Slash Commands via RPC (get_commands)

(ert-deftest pilish-test-fetch-commands-parses-response ()
  "fetch-commands extracts command list from RPC response."
  (let* ((callback-result nil)
         (mock-response '(:success t
                          :data (:commands
                                 [(:name "fix-tests" :description "Fix tests" :source "prompt")
                                  (:name "session-name" :description "Set name" :source "extension")])))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pilish--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (pilish--fetch-commands fake-proc
            (lambda (commands)
              (setq callback-result commands)))
          ;; Verify commands were extracted correctly
          (should (= (length callback-result) 2))
          (should (equal (plist-get (car callback-result) :name) "fix-tests"))
          (should (equal (plist-get (cadr callback-result) :source) "extension")))
      (delete-process fake-proc))))

(ert-deftest pilish-test-fetch-commands-handles-failure ()
  "fetch-commands does not call callback on RPC failure."
  (let* ((callback-called nil)
         (mock-response '(:success :false :error "Connection failed"))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pilish--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (pilish--fetch-commands fake-proc
            (lambda (_) (setq callback-called t)))
          (should-not callback-called))
      (delete-process fake-proc))))

(ert-deftest pilish-test-fetch-commands-ignores-unsafe-source-paths ()
  "Passive command fetch does not store invalid source paths as navigable."
  (let* ((bad (concat "/tmp/a" (string ?\0) "b.md"))
         (callback-result nil)
         (mock-response
          (list :success t
                :data (list
                       :commands
                       (vector
                        (list :name "nul" :source "prompt"
                              :sourceInfo (list :scope "project" :path bad))
                        '(:name "other-remote" :source "prompt"
                          :sourceInfo (:scope "project"
                                       :path "/ssh:other:/tmp/fix.md"))
                        '(:name "ok" :source "prompt"
                          :sourceInfo (:scope "project"
                                       :path "prompts/ok.md"))))))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pilish--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (pilish--fetch-commands
           fake-proc
           (lambda (commands)
             (setq callback-result commands))
           "/ssh:pi-host:/home/pi/project/")
          (should (= (length callback-result) 3))
          (should-not (plist-get (nth 0 callback-result) :path))
          (should-not (plist-get (nth 1 callback-result) :path))
          (should (equal (plist-get (nth 2 callback-result) :path)
                         "/ssh:pi-host:/home/pi/project/prompts/ok.md")))
      (delete-process fake-proc))))

(ert-deftest pilish-test-set-commands-propagates-to-input ()
  "set-commands propagates commands to input buffer."
  (with-temp-buffer
    (let* ((input-buf (generate-new-buffer "*test-input*"))
           (pilish--input-buffer input-buf)
           (commands '((:name "test" :description "Test cmd" :source "prompt"))))
      (unwind-protect
          (progn
            (pilish--set-commands commands)
            ;; Verify local variable set in current buffer
            (should (equal pilish--commands commands))
            ;; Verify propagated to input buffer
            (should (equal (buffer-local-value 'pilish--commands input-buf)
                           commands)))
        (kill-buffer input-buf)))))

(ert-deftest pilish-test-command-capf-uses-commands ()
  "command-capf completion uses pilish--commands."
  (with-temp-buffer
    (let ((pilish--commands
           '((:name "fix-tests" :description "Fix" :source "prompt")
             (:name "review" :description "Review" :source "prompt"))))
      (insert "/")
      (let ((completion (pilish--command-capf)))
        (should completion)
        ;; Third element is the completion candidates
        (should (member "fix-tests" (nth 2 completion)))
        (should (member "review" (nth 2 completion)))))))

(ert-deftest pilish-test-run-command-formats-command-text ()
  "run-command builds literal slash commands from NAME and optional args."
  (let ((sent-messages nil)
        (fake-proc (start-process "test" nil "cat")))
    (set-process-query-on-exit-flag fake-proc nil)
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (let ((pilish--process fake-proc))
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc msg _cb)
                         (push (plist-get msg :message) sent-messages))))
              (pilish-run-command "greet")
              (pilish-run-command "greet" "")
              (pilish-run-command "greet" "world")
              (should (equal (nreverse sent-messages)
                             '("/greet" "/greet" "/greet world"))))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-run-command-uses-linked-input-session ()
  "run-command sends through the chat buffer linked to current input."
  (let ((sent-message nil)
        (fake-proc (start-process "test" nil "cat"))
        (chat-buf (generate-new-buffer " *pi-command-chat*"))
        (input-buf (generate-new-buffer " *pi-command-input*")))
    (set-process-query-on-exit-flag fake-proc nil)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process fake-proc)
            (pilish--set-input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (pilish--set-chat-buffer chat-buf)
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message)))))
              (pilish-run-command "greet" "world")))
          (should (equal sent-message "/greet world")))
      (pilish-test--kill-live-buffers input-buf chat-buf)
      (delete-process fake-proc))))

(ert-deftest pilish-test-run-command-requires-current-session ()
  "run-command reports a missing current pi session."
  (with-temp-buffer
    (should-error (pilish-run-command "greet")
                  :type 'user-error)))

(ert-deftest pilish-test-run-command-interactive-requires-session-first ()
  "run-command reports a missing session before prompting interactively."
  (with-temp-buffer
    (let (prompted)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (setq prompted t)
                   "greet"))
                ((symbol-function 'read-string)
                 (lambda (&rest _args)
                   (setq prompted t)
                   "")))
        (should-error (call-interactively #'pilish-run-command)
                      :type 'user-error)
        (should-not prompted)))))

(ert-deftest pilish-test-run-custom-command-sends-literal ()
  "run-custom-command sends literal /command text, not expanded."
  (let* ((sent-message nil)
         (fake-proc (start-process "test" nil "cat"))
         (cmd '(:name "greet" :description "Greet" :source "prompt")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (let ((pilish--process fake-proc))
            (cl-letf (((symbol-function 'pilish--get-chat-buffer)
                       (lambda () (current-buffer)))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message))))
                      ((symbol-function 'read-string)
                       (lambda (&rest _args) "world")))
              (pilish--run-custom-command cmd)
              ;; Should send literal /greet world, NOT expanded prompt
              (should (equal sent-message "/greet world")))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-run-custom-command-empty-args ()
  "run-custom-command with empty args sends just /command."
  ;; Note: Use "mycommand" not "compact" to avoid collision with built-in /compact handling
  (let* ((sent-message nil)
         (fake-proc (start-process "test" nil "cat"))
         (cmd '(:name "mycommand" :description "My Command" :source "extension")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (let ((pilish--process fake-proc))
            (cl-letf (((symbol-function 'pilish--get-chat-buffer)
                       (lambda () (current-buffer)))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message))))
                      ((symbol-function 'read-string)
                       (lambda (&rest _args) "")))
              (pilish--run-custom-command cmd)
              ;; Should send just /mycommand without trailing space
              (should (equal sent-message "/mycommand")))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-rebuild-menu-shows-prompt-source-as-templates ()
  "rebuild-commands-menu creates Templates section for source \"prompt\".
Pi v0.51.3+ renamed SlashCommandSource from \"template\" to \"prompt\"."
  (let ((pilish--commands
         '((:name "fix-tests" :description "Fix tests" :source "prompt" :location "user")
           (:name "review" :description "Code review" :source "prompt" :location "project"))))
    (unwind-protect
        (progn
          (pilish--rebuild-commands-menu)
          (should (transient-get-suffix 'pilish-menu '(3))))
      (ignore-errors (transient-remove-suffix 'pilish-menu '(3))))))

(defun pilish-test--suffix-key-bound-p (key)
  "Return non-nil if KEY is bound in current transient suffixes."
  (cl-find-if (lambda (obj) (equal (oref obj key) key))
              transient--suffixes))

(ert-deftest pilish-test-transient-opens-session-and-tree-browsers ()
  "The main menu exposes the session and tree browser actions."
  (transient-setup 'pilish-menu)
  (let ((sessions-suffix
         (pilish-test--suffix-key-bound-p "r"))
        (tree-suffix
         (pilish-test--suffix-key-bound-p "w")))
    (should sessions-suffix)
    (should (equal (transient-format-description sessions-suffix)
                   "sessions"))
    (should (eq (oref sessions-suffix command)
                'pilish-session-browser))
    (should tree-suffix)
    (should (equal (transient-format-description tree-suffix)
                   "tree"))
    (should (eq (oref tree-suffix command)
                'pilish-tree-browser))))

(ert-deftest pilish-test-submenus-open-with-no-commands ()
  "All submenus open without error when no commands are loaded."
  (let ((pilish--commands nil))
    (dolist (menu '(pilish-templates-menu
                    pilish-extensions-menu
                    pilish-skills-menu))
      (transient-setup menu))))

(ert-deftest pilish-test-templates-menu-shows-run-keys ()
  "Templates submenu binds letter keys to commands."
  (let ((pilish--commands
         '((:name "test-tmpl" :description "A template" :source "prompt"))))
    (transient-setup 'pilish-templates-menu)
    (should (pilish-test--suffix-key-bound-p "a"))))

(ert-deftest pilish-test-templates-menu-shows-edit-keys ()
  "Templates submenu binds uppercase letter keys to edit file paths."
  (let ((pilish--commands
         '((:name "uncle-bob" :description "Uncle Bob review"
            :source "prompt" :path "/tmp/uncle-bob.md" :location "user")
           (:name "fix-tests" :description "Fix tests"
            :source "prompt" :path "/tmp/fix-tests.md" :location "project"))))
    (transient-setup 'pilish-templates-menu)
    (should (pilish-test--suffix-key-bound-p "a"))
    (should (pilish-test--suffix-key-bound-p "A"))))

(ert-deftest pilish-test-stats-uses-i-key-not-S ()
  "Stats is bound to `i' so it doesn't conflict with Skills `S' key."
  (transient-setup 'pilish-menu)
  (should (pilish-test--suffix-key-bound-p "i"))
  (should-not (pilish-test--suffix-key-bound-p "S")))

(ert-deftest pilish-test-submenu-handles-more-than-9-commands ()
  "Submenu with 13 skills uses letter keys without crashing."
  (let ((pilish--commands
         (cl-loop for i from 1 to 13
                  collect (list :name (format "skill-%d" i)
                                :description (format "Skill number %d" i)
                                :source "skill"
                                :location "user"))))
    ;; Should not signal an error
    (transient-setup 'pilish-skills-menu)
    ;; First and last should be bound
    (should (pilish-test--suffix-key-bound-p "a"))
    (should (pilish-test--suffix-key-bound-p "m"))))

(ert-deftest pilish-test-submenu-run-and-edit-keys-correspond ()
  "Run key `a' and edit key `A' refer to the same command."
  (let ((pilish--commands
         '((:name "alpha" :description "First" :source "skill"
            :location "user" :path "/tmp/alpha.md")
           (:name "beta" :description "Second" :source "skill"
            :location "user" :path "/tmp/beta.md"))))
    (transient-setup 'pilish-skills-menu)
    ;; Run keys a, b and edit keys A, B should all be bound
    (should (pilish-test--suffix-key-bound-p "a"))
    (should (pilish-test--suffix-key-bound-p "b"))
    (should (pilish-test--suffix-key-bound-p "A"))
    (should (pilish-test--suffix-key-bound-p "B"))))

(ert-deftest pilish-test-builtin-command-run-key-without-edit-key ()
  "Built-in extension commands get a run key but consume no edit suffix.
The pathless built-in keeps its key slot, so a later file-backed command
keeps its run/edit correspondence."
  (let* ((pilish--commands
          (list (pilish--normalize-command
                 (list :name "mcp" :description "Model context"
                       :source "extension"
                       :sourceInfo '(:scope "temporary" :path "builtin:mcp"))
                 "/tmp/project/")
                (pilish--normalize-command
                 (list :name "z-local" :description "Local extension"
                       :source "extension"
                       :sourceInfo '(:scope "project" :path "extension.ts"))
                 "/tmp/project/")))
         (opened nil))
    (should (equal (mapcar #'car (seq-filter #'listp
                                (pilish--make-submenu-children "extension")))
                   '("a" "b")))
    (should (equal (mapcar #'car
                           (pilish--make-submenu-edit-children "extension"))
                   '("B")))
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (path) (push path opened))))
      (unwind-protect
          (progn
            (transient-setup 'pilish-extensions-menu)
            (should (pilish-test--suffix-key-bound-p "a"))
            (should-not (pilish-test--suffix-key-bound-p "A"))
            (should (pilish-test--suffix-key-bound-p "B"))
            (should-not opened))
        ;; Leave no transient keymaps or hooks behind for later tests.
        (transient--emergency-exit)))))

;;; Manual Compaction

(ert-deftest pilish-test-manual-compact-event-and-response-render-once ()
  "Manual compact success is rendered from compaction_end, not the RPC response."
  (let ((chat-buf (get-buffer-create "*pilish-test-compact-render-once*"))
        (compact-callback nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--followup-queue nil))
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p)
                     (lambda (_proc) t))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (when (equal (plist-get cmd :type) "compact")
                         (setq compact-callback cb))))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (pilish-compact)
              (pilish--handle-display-event
               '(:type "compaction_start" :reason "manual"))
              (pilish--handle-display-event
               '(:type "compaction_end"
                 :reason "manual"
                 :aborted :false
                 :willRetry :false
                 :result (:tokensBefore 1234
                          :summary "Unique manual compaction summary"
                          :firstKeptEntryId "entry-1"
                          :details nil))))
            (should (functionp compact-callback))
            (funcall compact-callback
                     '(:success t
                       :data (:tokensBefore 1234
                              :summary "Unique manual compaction summary"
                              :firstKeptEntryId "entry-1"
                              :details nil)))
            (with-current-buffer chat-buf
              (should (= 1 (pilish-test--count-matches
                            "Unique manual compaction summary"
                            (buffer-string)))))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-review-fixes-manual-reservation-until-correlated-response ()
  "Compaction end does not free its local RPC reservation or unlock a second command."
  (pilish-test-with-rpc-session (chat _input proc commands)
    (with-current-buffer chat
      (let (shown-message)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (pilish-compact)
          (let ((request (car commands)))
            (pilish--handle-display-event '(:type "compaction_start" :reason "manual"))
            (pilish--handle-display-event
             '(:type "compaction_end" :reason "manual" :aborted :false
               :result :null :errorMessage "old failure"))
            (should (pilish--session-busy-p))
            (should-not (pilish--canonical-rerender-safe-p))
            (pilish-compact)
            (should (= 1 (length commands)))
            (should-not (pilish--new-session-ready-p chat))
            ;; An extension can still start independent B while A's RPC awaits
            ;; session_compact_failed handlers.  A's reply must not idle B.
            (pilish--handle-display-event '(:type "agent_start"))
            (pilish-abort)
            (pilish--dispatch-response
             proc (list :type "response" :id (plist-get request :id)
                        :command "compact" :success :false :error "old failure"))
            (should (eq pilish--status 'streaming))
            (should pilish--aborted)
            (should (string-match-p "old failure" shown-message))
            (pilish--handle-display-event '(:type "agent_end" :messages []))
            (pilish--handle-display-event '(:type "agent_settled"))
            (should-not (pilish--session-busy-p))
            (pilish-compact)
            (pilish-abort)
            ;; Duplicate old replies cannot release the new correlated request.
            (pilish--dispatch-response
             proc (list :type "response" :id (plist-get request :id)
                        :command "compact" :success :false :error "old failure"))
            (should (pilish--session-busy-p))
            (should pilish--aborted)))))))

(ert-deftest pilish-test-review-fixes-manual-success-releases-fifo-on-response ()
  "Manual compaction's correlated response, not its end event, releases local FIFO."
  (pilish-test-with-rpc-session (chat input proc commands)
    (with-current-buffer chat
      (cl-letf (((symbol-function 'message) #'ignore))
        (pilish-compact)
        (let ((request (car commands)))
          (pilish--handle-display-event '(:type "compaction_start" :reason "manual"))
          (with-current-buffer input (insert "next") (pilish-send))
          (pilish--handle-display-event
           '(:type "compaction_end" :reason "manual" :aborted :false
             :result (:tokensBefore 1000 :summary "Done")))
          (should (= 1 (length commands)))
          (should (equal pilish--followup-queue '("next")))
          (pilish--dispatch-response
           proc (list :type "response" :id (plist-get request :id)
                      :command "compact" :success t))
          (should (equal (plist-get (car commands) :message) "next"))
          (should (equal pilish--followup-queue '("next"))))))))

(ert-deftest pilish-test-compact-refuses-unsettled-work ()
  "Manual compact cannot overtake prompt preflight, a run or its settlement."
  (dolist (status '(sending streaming compacting))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--status status
            pilish--process 'mock-proc
            pilish--followup-queue '("keep queued"))
      (let (commands shown-message)
        (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_proc command _callback) (push command commands)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (pilish-compact)
          (should-not commands)
          (should (string-match-p "Cannot compact" shown-message))
          (should (eq pilish--status status))
          (should (equal pilish--followup-queue '("keep queued"))))))))

(ert-deftest pilish-test-aborted-manual-compaction-rejection-releases-stop ()
  "A rejected manual compact command cannot leave stale local stop intent."
  (pilish-test-with-rpc-session (chat _input proc commands)
    (with-current-buffer chat
      (let (shown-message)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (pilish-compact)
          (let ((request (car commands)))
            (pilish-abort)
            (should pilish--aborted)
            (pilish--dispatch-response
             proc (list :type "response" :id (plist-get request :id)
                        :command "compact" :success :false :error "not started"))))
        (should (equal shown-message "Pi: Compact failed: not started"))
        (should-not (pilish--session-busy-p))
        (should-not pilish--aborted)))))

(ert-deftest pilish-test-compact-response-failure-reports-without-event ()
  "A command-level failure releases its reservation and restores unsent FIFO."
  (pilish-test-with-rpc-session (chat input proc commands)
    (with-current-buffer chat
      (let (shown-message)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (pilish-compact)
          (should (pilish--session-busy-p))
          (setq pilish--followup-queue '("queued during failed compact"))
          (pilish--dispatch-response
           proc (list :type "response" :id (plist-get (car commands) :id)
                      :command "compact" :success :false :error "not started")))
        (should (equal shown-message "Pi: Compact failed: not started"))
        (should-not (pilish--session-busy-p))
        (should (equal pilish--activity-phase "idle"))
        (should-not pilish--followup-queue)
        (should-not (string-match-p "Compacted from" (buffer-string)))))
    (with-current-buffer input
      (should (equal (buffer-string) "queued during failed compact")))))

(ert-deftest pilish-test-compact-dead-process-keeps-idle ()
  "Manual compact should not transition state when process is dead."
  (let ((chat-buf (get-buffer-create "*pilish-test-compact-dead-proc*"))
        (rpc-called nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--followup-queue nil))
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () 'dead-proc))
                    ((symbol-function 'process-live-p)
                     (lambda (_proc) nil))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (&rest _args)
                       (setq rpc-called t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (with-current-buffer chat-buf
              (pilish-compact)
              (should (eq pilish--status 'idle))))
          (should-not rpc-called)
          (should (equal shown-message
                         "Pi: Process died - try M-x pilish-reload or C-c C-p R")))
      (kill-buffer chat-buf))))

(defun pilish-test--seed-stale-session-rebuild-state (chat-buf stale-text)
  "Seed CHAT-BUF with stale state so a session rebuild must replace it.
STALE-TEXT is inserted into the buffer and also mirrored into the canonical
message cache so tests can prove both rendered and cached session state were
replaced by the resumed or forked history."
  (with-current-buffer chat-buf
    (setq pilish--process 'mock-proc
          pilish--state '(:session-id "old-session-id"
                                   :session-file "/tmp/old-session.jsonl"))
    (pilish--set-canonical-messages
     [(:role "assistant"
       :content [(:type "text" :text "Old canonical history")]
       :timestamp 1704067200000)])
    (let ((inhibit-read-only t))
      (insert stale-text "\n"))
    (let ((tool-ov (make-overlay 1 6 nil nil nil)))
      (overlay-put tool-ov 'pilish-tool-block t)
      (setq pilish--pending-tool-overlay tool-ov))
    (puthash "old-tool" '(:path "/tmp/old.el") pilish--tool-args-cache)
    (puthash "old-tool" '(:tool-call-id "old-tool") pilish--live-tool-blocks)))

(defun pilish-test--assert-clean-session-rebuild
    (chat-buf expected-messages stale-text)
  "Assert CHAT-BUF was rebuilt from EXPECTED-MESSAGES and cleared STALE-TEXT."
  (with-current-buffer chat-buf
    (should (equal pilish--canonical-messages expected-messages))
    (should-not (string-match-p (regexp-quote stale-text) (buffer-string)))
    (should-not pilish--pending-tool-overlay)
    (should (= 0 (hash-table-count pilish--tool-args-cache)))
    (should (= 0 (hash-table-count pilish--live-tool-blocks)))
    (should-not (cl-some (lambda (ov)
                           (overlay-get ov 'pilish-tool-block))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest pilish-test-update-session-name-from-file-uses-jsonl-name ()
  "Session-name refresh uses canonical JSONL metadata and clears absent names."
  (let* ((session-file "/tmp/pilish-session-name.jsonl")
         (named-info (list :path session-file :name "Canonical name"))
         (unnamed-info (list :path session-file :cwd "/tmp"))
         (responses (list named-info unnamed-info))
         (scanner-paths nil))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--session-name "Stale name")
      (cl-letf (((symbol-function 'pilish-jsonl-read-session-info)
                 (lambda (path)
                   (push path scanner-paths)
                   (prog1 (car responses)
                     (setq responses (cdr responses))))))
        (should (equal (pilish--update-session-name-from-file
                        session-file)
                       named-info))
        (should (equal pilish--session-name "Canonical name"))
        (should (equal (pilish--update-session-name-from-file
                        session-file)
                       unnamed-info))
        (should-not pilish--session-name)
        (should-not responses)
        (should (equal (nreverse scanner-paths)
                       (list session-file session-file)))))))

(ert-deftest pilish-test-session-file-cwd-or-error-returns-expanded-directory ()
  "Session-file cwd validator returns an expanded directory name."
  (let* ((project-dir (pilish-test--make-temp-directory
                       "pilish-test-project-"))
         (session-dir (pilish-test--make-temp-directory
                       "pilish-test-sessions-"))
         (session-file (expand-file-name "session.jsonl" session-dir)))
    (unwind-protect
        (let ((cwd (directory-file-name project-dir)))
          (pilish-test--write-session-file session-file "hello" cwd)
          (should (equal (pilish--session-file-cwd-or-error
                          session-file)
                         project-dir)))
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pilish-test-session-file-cwd-or-error-anchors-remote-cwd ()
  "Remote session header cwd is returned as a TRAMP directory."
  (let ((session-file "/ssh:pi-host:/home/pi/.pi/sessions/session.jsonl")
        (checked-dir nil)
        (scanner-called nil))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (path)
                 (equal path session-file)))
              ((symbol-function 'pilish-jsonl-read-session-info)
               (lambda (path)
                 (setq scanner-called t)
                 (should (equal path session-file))
                 '(:cwd "/home/pi/project")))
              ((symbol-function 'file-attributes)
               (lambda (&rest _)
                 (error "Unexpected direct metadata scan")))
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (setq checked-dir path)
                 (equal path "/ssh:pi-host:/home/pi/project/"))))
      (should (equal (pilish--session-file-cwd-or-error session-file)
                     "/ssh:pi-host:/home/pi/project/"))
      (should scanner-called)
      (should (equal checked-dir "/ssh:pi-host:/home/pi/project/")))))

(ert-deftest pilish-test-session-file-cwd-or-error-preserves-multi-hop-cwd ()
  "Remote session cwd anchoring keeps the full multi-hop TRAMP route."
  (let ((session-file
         "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/sessions/session.jsonl")
        (expected-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
        (checked-dir nil)
        (scanner-called nil))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (path) (equal path session-file)))
              ((symbol-function 'pilish-jsonl-read-session-info)
               (lambda (path)
                 (setq scanner-called t)
                 (should (equal path session-file))
                 '(:cwd "/home/pi/project")))
              ((symbol-function 'file-attributes)
               (lambda (&rest _)
                 (error "Unexpected direct metadata scan")))
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (setq checked-dir path)
                 (equal path expected-dir))))
      (should (equal (pilish--session-file-cwd-or-error session-file)
                     expected-dir))
      (should scanner-called)
      (should (equal checked-dir expected-dir)))))

(ert-deftest pilish-test-session-file-cwd-or-error-rejects-remote-cwd ()
  "Session header cwd must be process-local before remote anchoring."
  (let ((session-file "/ssh:pi-host:/home/pi/.pi/sessions/session.jsonl")
        (scanner-called nil))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (path)
                 (equal path session-file)))
              ((symbol-function 'pilish-jsonl-read-session-info)
               (lambda (path)
                 (setq scanner-called t)
                 (should (equal path session-file))
                 '(:cwd "/ssh:pi-host:/home/pi/project")))
              ((symbol-function 'file-attributes)
               (lambda (&rest _)
                 (error "Unexpected direct metadata scan")))
              ((symbol-function 'file-directory-p)
               (lambda (_path)
                 (ert-fail "Remote cwd should be rejected before directory check"))))
      (should-error (pilish--session-file-cwd-or-error session-file)
                    :type 'user-error)
      (should scanner-called))))

(ert-deftest pilish-test-session-file-cwd-or-error-rejects-home-cwd ()
  "Session header cwd must not depend on home expansion."
  (let ((session-file "/ssh:pi-host:/home/pi/.pi/sessions/session.jsonl"))
    (dolist (cwd '("~" "~/project" "~root/project"))
      (let ((scanner-called nil))
        (cl-letf (((symbol-function 'file-readable-p)
                   (lambda (path)
                     (equal path session-file)))
                  ((symbol-function 'pilish-jsonl-read-session-info)
                   (lambda (path)
                     (setq scanner-called t)
                     (should (equal path session-file))
                     (list :cwd cwd)))
                  ((symbol-function 'file-attributes)
                   (lambda (&rest _)
                     (error "Unexpected direct metadata scan")))
                  ((symbol-function 'file-directory-p)
                   (lambda (_path)
                     (ert-fail "Home cwd should be rejected before directory check"))))
          (ert-info ((format "cwd: %s" cwd))
            (should-error (pilish--session-file-cwd-or-error session-file)
                          :type 'user-error)
            (should scanner-called)))))))

(ert-deftest pilish-test-session-file-cwd-or-error-rejects-unreadable-file ()
  "Session-file cwd validator rejects unreadable files."
  (let* ((session-dir (pilish-test--make-temp-directory
                       "pilish-test-sessions-"))
         (missing-file (expand-file-name "missing.jsonl" session-dir)))
    (unwind-protect
        (should-error (pilish--session-file-cwd-or-error missing-file)
                      :type 'user-error)
      (delete-directory session-dir t))))

(ert-deftest pilish-test-session-file-cwd-or-error-rejects-invalid-session-metadata ()
  "Session-file cwd validator rejects files without valid session metadata."
  (let* ((session-dir (pilish-test--make-temp-directory
                       "pilish-test-sessions-"))
         (session-file (expand-file-name "not-a-session.jsonl" session-dir)))
    (unwind-protect
        (progn
          (with-temp-file session-file
            (insert "{\"type\":\"message\"}\n"))
          (should-error (pilish--session-file-cwd-or-error session-file)
                        :type 'user-error))
      (delete-directory session-dir t))))

(ert-deftest pilish-test-session-file-cwd-or-error-rejects-unusable-cwd ()
  "Session-file cwd validator rejects missing, non-string, and empty cwd values."
  (let* ((session-dir (pilish-test--make-temp-directory
                       "pilish-test-sessions-"))
         (cases '(("missing" . "{\"type\":\"session\",\"id\":\"test\"}")
                  ("null" . "{\"type\":\"session\",\"id\":\"test\",\"cwd\":null}")
                  ("number" . "{\"type\":\"session\",\"id\":\"test\",\"cwd\":123}")
                  ("empty" . "{\"type\":\"session\",\"id\":\"test\",\"cwd\":\"\"}"))))
    (unwind-protect
        (dolist (case cases)
          (let ((session-file (expand-file-name
                               (format "%s.jsonl" (car case))
                               session-dir)))
            (with-temp-file session-file
              (insert (cdr case) "\n"))
            (ert-info ((format "cwd case: %s" (car case)))
              (should-error (pilish--session-file-cwd-or-error
                             session-file)
                            :type 'user-error))))
      (delete-directory session-dir t))))

(ert-deftest pilish-test-session-file-cwd-or-error-rejects-relative-cwd ()
  "Session-file cwd validator rejects cwd values that depend on default-directory."
  (let* ((session-dir (pilish-test--make-temp-directory
                       "pilish-test-sessions-"))
         (session-file (expand-file-name "relative.jsonl" session-dir))
         (relative-cwd "relative-project"))
    (unwind-protect
        (progn
          (make-directory (expand-file-name relative-cwd session-dir))
          (pilish-test--write-session-file
           session-file "hello" relative-cwd)
          (let* ((default-directory session-dir)
                 (error-data
                  (should-error (pilish--session-file-cwd-or-error
                                 session-file)
                                :type 'user-error))
                 (message (cadr error-data)))
            (should (string-match-p (regexp-quote relative-cwd) message))
            (should (string-match-p (regexp-quote session-file) message))))
      (delete-directory session-dir t))))

(ert-deftest pilish-test-session-file-cwd-or-error-rejects-stale-cwd ()
  "Session-file cwd validator rejects cwd values that do not name a directory."
  (let* ((session-dir (pilish-test--make-temp-directory
                       "pilish-test-sessions-"))
         (session-file (expand-file-name "stale.jsonl" session-dir))
         (stale-cwd (expand-file-name "deleted-project" session-dir)))
    (unwind-protect
        (progn
          (pilish-test--write-session-file session-file "hello" stale-cwd)
          (let* ((error-data
                  (should-error (pilish--session-file-cwd-or-error
                                 session-file)
                                :type 'user-error))
                 (message (cadr error-data)))
            (should (string-match-p (regexp-quote stale-cwd) message))
            (should (string-match-p (regexp-quote session-file) message))))
      (delete-directory session-dir t))))

(ert-deftest pilish-test-session-list-directory-uses-session-file-parent ()
  "Session listing uses the current JSONL session file parent directory."
  (let* ((project-dir (pilish-test--make-temp-directory
                       "pilish-project-"))
         (expected-dir (file-name-as-directory
                        (expand-file-name "sessions" project-dir))))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (pilish--set-chat-session-identity project-dir)
          (setq pilish--state '(:session-file "sessions/current.jsonl"))
          (should (equal (pilish--session-list-directory (current-buffer))
                         expected-dir))
          (setq pilish--state '(:session-file ""))
          (should-not (pilish--session-list-directory (current-buffer)))
          (setq pilish--state '(:session-file :json-false))
          (should-not (pilish--session-list-directory (current-buffer))))
      (delete-directory project-dir t))))

(ert-deftest pilish-test-session-list-directory-uses-remote-session-file-parent ()
  "Session listing works when state stores a normalized remote session file."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--set-chat-session-identity
     "/ssh:pi-host:/home/pi/project/")
    (setq pilish--state
          '(:session-file "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl"))
    (should (equal (pilish--session-list-directory (current-buffer))
                   "/ssh:pi-host:/home/pi/.pi/sessions/"))))

(ert-deftest pilish-test-session-list-directory-preserves-multi-hop-parent ()
  "Session listing keeps the full multi-hop parent directory route."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--set-chat-session-identity
     "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
    (setq pilish--state
          '(:session-file "sessions/current.jsonl"))
    (should (equal (pilish--session-list-directory (current-buffer))
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/sessions/"))))

(ert-deftest pilish-test-resume-selected-session-sends-process-local-remote-path ()
  "Resuming a remote Emacs session file sends process-local sessionPath."
  (let ((proc (start-process "test-remote-resume" nil "cat"))
        (chat-buf (get-buffer-create "*pilish-test-remote-resume-chat*"))
        (session-path-used nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-chat-session-identity
             "/ssh:pi-host:/home/pi/project/"))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (when (equal (plist-get cmd :type) "switch_session")
                         (setq session-path-used (plist-get cmd :sessionPath))
                         (funcall cb '(:success t :data (:cancelled :false))))))
                    ((symbol-function 'pilish--session-file-cwd-or-error)
                     (lambda (_path) "/ssh:pi-host:/home/pi/project/"))
                    ((symbol-function 'pilish--refresh-session-state)
                     #'ignore)
                    ((symbol-function 'pilish--load-session-history)
                     #'ignore)
                    ((symbol-function 'pilish--fetch-commands)
                     (lambda (_proc callback _anchor)
                       (funcall callback nil)))
                    ((symbol-function 'message) #'ignore))
            (pilish--resume-selected-session
             proc chat-buf
             "/ssh:pi-host:/home/pi/.pi/sessions/target.jsonl"))
          (should (equal session-path-used
                         "/home/pi/.pi/sessions/target.jsonl")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-resume-selected-session-retargets-session-directory ()
  "Resuming a cross-cwd session moves frontend path ownership too."
  (let* ((old-dir (pilish-test--make-temp-directory
                   "pilish-test-resume-old-cwd-"))
         (new-dir (pilish-test--make-temp-directory
                   "pilish-test-resume-new-cwd-"))
         (session-dir (pilish-test--make-temp-directory
                       "pilish-test-resume-cross-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (chat-buf (generate-new-buffer "*pilish-test-resume-cross-chat*"))
         (input-buf (generate-new-buffer "*pilish-test-resume-cross-input*"))
         (proc 'mock-proc)
         (refresh-dir nil)
         (commands-anchor nil))
    (unwind-protect
        (progn
          (pilish-test--write-session-file
           target-session "target" (directory-file-name new-dir))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-chat-session-identity old-dir)
            (pilish--set-input-buffer input-buf)
            (setq pilish--process proc))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq default-directory old-dir)
            (pilish--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "switch_session"))
                       (funcall cb '(:success t :data (:cancelled :false)))))
                    ((symbol-function 'pilish--refresh-session-state)
                     (lambda (_proc chat _selected-path
                              &optional _generation _completion)
                       (setq refresh-dir
                             (with-current-buffer chat
                               (pilish--chat-session-directory)))))
                    ((symbol-function 'pilish--load-session-history)
                     #'ignore)
                    ((symbol-function 'pilish--fetch-commands)
                     (lambda (_proc callback anchor)
                       (setq commands-anchor anchor)
                       (funcall callback nil)))
                    ((symbol-function 'message) #'ignore))
            (pilish--resume-selected-session
             proc chat-buf target-session))
          (with-current-buffer chat-buf
            (should (equal (pilish--chat-session-directory)
                           new-dir)))
          (with-current-buffer input-buf
            (should (equal default-directory new-dir)))
          (should (equal refresh-dir new-dir))
          (should (equal commands-anchor new-dir)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (setq pilish--process nil)))
      (pilish-test--kill-live-buffers input-buf chat-buf)
      (delete-directory old-dir t)
      (delete-directory new-dir t)
      (delete-directory session-dir t))))

(ert-deftest pilish-test-resume-selected-session-duplicate-target-keeps-source-ready ()
  "Duplicate resume preflight does not leave the source session busy."
  (let* ((source-dir (pilish-test--make-temp-directory
                      "pilish-test-resume-duplicate-source-"))
         (target-dir (pilish-test--make-temp-directory
                      "pilish-test-resume-duplicate-target-"))
         (session-dir (pilish-test--make-temp-directory
                       "pilish-test-resume-duplicate-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (source-chat (generate-new-buffer
                       "*pilish-test-resume-duplicate-source*"))
         (target-chat (generate-new-buffer
                       "*pilish-test-resume-duplicate-target*"))
         (proc 'mock-proc)
         (rpc-called nil)
         (initial-generation nil))
    (unwind-protect
        (progn
          (pilish-test--write-session-file
           target-session "target" (directory-file-name target-dir))
          (with-current-buffer source-chat
            (pilish-chat-mode)
            (pilish--set-chat-session-identity source-dir)
            (setq pilish--process proc
                  pilish--status 'idle
                  initial-generation pilish--session-transition-generation))
          (with-current-buffer target-chat
            (pilish-chat-mode)
            (pilish--set-chat-session-identity target-dir)
            (setq pilish--status 'idle))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (&rest _args)
                       (setq rpc-called t)
                       (ert-fail "Duplicate resume target must not send RPC")))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer source-chat
              (should-error
               (pilish--resume-selected-session
                proc source-chat target-session)
               :type 'user-error))
            (should-not rpc-called)
            (with-current-buffer source-chat
              (should (= pilish--session-transition-generation
                         initial-generation))
              (should-not (pilish--session-transition-active-p))
              (should-not (pilish--session-busy-p))
              (should (pilish--session-transition-ready-p
                       source-chat "resume")))))
      (pilish-test--kill-live-buffers target-chat source-chat)
      (delete-directory source-dir t)
      (delete-directory target-dir t)
      (delete-directory session-dir t))))

(ert-deftest pilish-test-retarget-session-buffers-preserves-named-session ()
  "Cross-cwd resume keeps the frontend named-session identity."
  (let* ((old-dir (pilish-test--make-temp-directory
                   "pilish-test-retarget-name-old-"))
         (new-dir (pilish-test--make-temp-directory
                   "pilish-test-retarget-name-new-"))
         (chat-buf (generate-new-buffer "*pilish-test-retarget-name-chat*"))
         (input-buf (generate-new-buffer "*pilish-test-retarget-name-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-chat-session-identity old-dir "side")
            (pilish--set-input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (pilish--set-chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (pilish--retarget-session-buffers new-dir)
            (should (equal (pilish--chat-session-directory) new-dir))
            (should (equal (pilish--chat-session-name) "side"))
            (should (equal (buffer-name)
                           (pilish--buffer-name :chat new-dir "side"))))
          (with-current-buffer input-buf
            (should (equal default-directory new-dir))
            (should (equal (buffer-name)
                           (pilish--buffer-name :input new-dir "side")))))
      (pilish-test--kill-live-buffers input-buf chat-buf)
      (delete-directory old-dir t)
      (delete-directory new-dir t))))

(ert-deftest pilish-test-resume-selected-session-switches-session-and-rebuilds-history ()
  "Resuming a selected session refreshes chat history and session state."
  (let* ((dir (pilish-test--make-temp-directory
               "pilish-test-resume-happy-"))
         (session-dir (pilish-test--make-temp-directory
                       "pilish-test-current-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (resumed-session (expand-file-name "resumed.jsonl" session-dir))
         (shown-message nil)
         (name-scan-path nil)
         (rpc-calls nil))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let* ((chat-buf (get-buffer (pilish-test--chat-buffer-name dir)))
                 (messages [(:role "assistant"
                             :content [(:type "text" :text "Resumed history")]
                             :timestamp 1704067200000)]))
            (pilish-test--write-session-file
             target-session "Resume target" (directory-file-name dir))
            (pilish-test--seed-stale-session-rebuild-state
             chat-buf "STALE RESUME CONTENT")
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc cmd cb)
                         (push (plist-get cmd :type) rpc-calls)
                         (pcase (plist-get cmd :type)
                           ("switch_session"
                            (with-current-buffer chat-buf
                              (should (pilish--session-transition-active-p)))
                            (should (equal (plist-get cmd :sessionPath)
                                           target-session))
                            (funcall cb '(:success t :data (:cancelled :false))))
                           ("get_state"
                            (with-current-buffer chat-buf
                              (should (pilish--session-transition-active-p)))
                            (funcall cb `(:success t
                                          :data (:model (:name "resumed-model")
                                                 :thinkingLevel "medium"
                                                 :isStreaming :json-false
                                                 :isCompacting :json-false
                                                 :sessionId "resumed-session-id"
                                                 :sessionFile ,resumed-session
                                                 :messageCount 1
                                                 :pendingMessageCount 0))))
                           ("get_messages"
                            (with-current-buffer chat-buf
                              (should (pilish--session-transition-active-p)))
                            (funcall cb (list :success t
                                              :data (list :messages messages))))
                           ("get_commands"
                            (funcall cb '(:success t :data (:commands []))))
                           (_
                            (ert-fail
                             (format "Unexpected RPC during resume test: %S"
                                     cmd))))))
                      ((symbol-function 'pilish--update-session-name-from-file)
                       (lambda (path)
                         (setq name-scan-path path
                               pilish--session-name "Resume target")
                         '(:name "Resume target")))
                      ((symbol-function 'pilish--refresh-header) #'ignore)
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish--resume-selected-session
               'mock-proc chat-buf target-session))
            (with-current-buffer chat-buf
              (should (equal (plist-get pilish--state :session-id)
                             "resumed-session-id"))
              (should (equal (plist-get pilish--state :session-file)
                             resumed-session))
              (should (equal pilish--session-name "Resume target"))
              (should (string-match-p "Resumed history" (buffer-string)))
              (should-not (pilish--session-transition-active-p)))
            (pilish-test--assert-clean-session-rebuild
             chat-buf messages "STALE RESUME CONTENT")
            (should (equal name-scan-path target-session))
            (should (equal (nreverse rpc-calls)
                           '("switch_session" "get_state" "get_messages"
                             "get_commands")))
            (should (equal shown-message "Pi: Resumed session (1 messages)"))))
      (delete-directory dir t)
      (delete-directory session-dir t))))

(ert-deftest pilish-test-fork-from-input-switches-session-rebuilds-history-and-prefills-input ()
  "Forking from the input buffer rebuilds chat history and prefills input."
  (let ((dir "/tmp/pilish-test-fork-happy/")
        (shown-message nil)
        (rpc-calls nil))
    (pilish-test-with-mock-session dir
      (let* ((chat-buf (get-buffer (pilish-test--chat-buffer-name dir)))
             (input-buf (get-buffer (pilish-test--input-buffer-name dir)))
             (fork-messages [(:entryId "u1" :text "First question")
                             (:entryId "u2" :text "Second question")])
             (selected-choice
              (pilish--format-fork-message
               '(:entryId "u2" :text "Second question") 1))
             (messages [(:role "user"
                         :content [(:type "text" :text "Second question")]
                         :timestamp 1704067200000)
                        (:role "assistant"
                         :content [(:type "text" :text "Forked answer")]
                         :timestamp 1704067201000)]))
        (pilish-test--seed-stale-session-rebuild-state
         chat-buf "STALE FORK CONTENT")
        (with-current-buffer chat-buf
          (setq pilish--state
                (plist-put pilish--state :model
                           '(:name "Vision" :input ["text" "image"]))))
        (with-current-buffer input-buf
          (let ((path (make-temp-file "pi-prompt-attachment-" nil ".png")))
            (unwind-protect
                (progn
                  (pilish-test--write-prompt-image path 'png)
                  (pilish-test--attach-image path))
              (delete-file path)))
          (insert "old input text"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) selected-choice))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_proc cmd cb)
                     (push (plist-get cmd :type) rpc-calls)
                     (pcase (plist-get cmd :type)
                       ("get_fork_messages"
                        (funcall cb (list :success t :data
                                          (list :messages fork-messages))))
                       ("fork"
                        (should (equal (plist-get cmd :entryId) "u2"))
                        (funcall cb '(:success t :data (:text "Second question"))))
                       ("get_state"
                        (funcall cb '(:success t
                                      :data (:model (:name "forked-model")
                                             :thinkingLevel "high"
                                             :isStreaming :json-false
                                             :isCompacting :json-false
                                             :sessionId "forked-session-id"
                                             :sessionFile "/tmp/forked.jsonl"
                                             :messageCount 2
                                             :pendingMessageCount 0))))
                       ("get_messages"
                        (funcall cb (list :success t :data (list :messages messages))))
                       (_
                        (ert-fail (format "Unexpected RPC during fork test: %S"
                                          cmd))))))
                  ((symbol-function 'pilish--update-session-name-from-file)
                   #'ignore)
                  ((symbol-function 'pilish--refresh-header) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (with-current-buffer input-buf
            (pilish-fork)))
        (with-current-buffer chat-buf
          (should (equal (plist-get pilish--state :session-id)
                         "forked-session-id"))
          (should (equal (plist-get pilish--state :session-file)
                         "/tmp/forked.jsonl"))
          (should (string-match-p "Second question" (buffer-string)))
          (should (string-match-p "Forked answer" (buffer-string))))
        (with-current-buffer input-buf
          (should (equal (buffer-string) "Second question"))
          (should-not (string-match-p
                       "pi-prompt-attachment-"
                       (pilish-test--input-header))))
        (pilish-test--assert-clean-session-rebuild
         chat-buf messages "STALE FORK CONTENT")
        (should (equal (nreverse rpc-calls)
                       '("get_fork_messages" "fork" "get_state" "get_messages")))
        (should (equal shown-message
                       "Pi: Branched to new session (2 messages)"))))))

(ert-deftest pilish-test-resume-transition-waits-after-retarget ()
  "Resume retargets buffers but keeps sends blocked until state/history settle."
  (let* ((old-dir (pilish-test--make-temp-directory
                   "pilish-test-resume-transition-old-"))
         (new-dir (pilish-test--make-temp-directory
                   "pilish-test-resume-transition-new-"))
         (session-dir (pilish-test--make-temp-directory
                       "pilish-test-resume-transition-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (chat-buf (generate-new-buffer "*pilish-test-resume-transition-chat*"))
         (input-buf (generate-new-buffer "*pilish-test-resume-transition-input*"))
         (proc 'mock-proc)
         (switch-cb nil)
         (state-cb nil)
         (history-cb nil)
         (sent-text nil))
    (unwind-protect
        (progn
          (pilish-test--write-session-file
           target-session "target" (directory-file-name new-dir))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-chat-session-identity old-dir)
            (pilish--set-input-buffer input-buf)
            (setq pilish--process proc
                  pilish--status 'idle))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (pilish--set-chat-buffer chat-buf)
            (setq default-directory old-dir))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("switch_session" (setq switch-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         ("get_commands" nil)
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'pilish--prepare-and-send)
                     (lambda (text &optional _queued)
                       (setq sent-text text)))
                    ((symbol-function 'pilish--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'pilish--refresh-header) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (pilish--resume-selected-session
             proc chat-buf target-session)
            (funcall switch-cb '(:success t :data (:cancelled :false)))
            (with-current-buffer chat-buf
              (should (equal (pilish--chat-session-directory)
                             new-dir))
              (should (pilish--session-transition-active-p)))
            (with-current-buffer input-buf
              (should (equal default-directory new-dir))
              (insert "prompt after resume")
              (pilish-send)
              (should (equal (buffer-string) "prompt after resume")))
            (should-not sent-text)
            (funcall state-cb
                     `(:success t
                       :data (:model (:name "resumed-model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "resumed"
                              :sessionFile ,target-session
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (pilish--session-transition-active-p)))
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (pilish--session-transition-active-p)))
            (with-current-buffer input-buf
              (pilish-send))
            (should (equal sent-text "prompt after resume"))))
      (pilish-test--kill-live-buffers input-buf chat-buf)
      (delete-directory old-dir t)
      (delete-directory new-dir t)
      (delete-directory session-dir t))))

(ert-deftest pilish-test-fork-prefill-blocked-until-history-loaded ()
  "Fork pre-fills input immediately, but send stays blocked until history loads."
  (let ((chat-buf (generate-new-buffer "*pilish-test-fork-transition-chat*"))
        (input-buf (generate-new-buffer "*pilish-test-fork-transition-input*"))
        (proc 'mock-proc)
        (fork-cb nil)
        (state-cb nil)
        (history-cb nil)
        (sent-text nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (pilish--set-input-buffer input-buf)
            (setq pilish--process proc
                  pilish--status 'idle))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (pilish--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("fork" (setq fork-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'pilish--prepare-and-send)
                     (lambda (text &optional _queued)
                       (setq sent-text text)))
                    ((symbol-function 'pilish--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'pilish--refresh-header) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer input-buf
              (pilish--execute-fork proc "u1"))
            (funcall fork-cb '(:success t :data (:text "Forked prompt")))
            (with-current-buffer input-buf
              (should (equal (buffer-string) "Forked prompt"))
              (pilish-send)
              (should (equal (buffer-string) "Forked prompt")))
            (should-not sent-text)
            (with-current-buffer chat-buf
              (should (pilish--session-transition-active-p)))
            (funcall state-cb
                     '(:success t
                       :data (:model (:name "forked-model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "forked"
                              :sessionFile "/tmp/forked.jsonl"
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (pilish--session-transition-active-p)))
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (pilish--session-transition-active-p)))
            (with-current-buffer input-buf
              (pilish-send))
            (should (equal sent-text "Forked prompt"))))
      (pilish-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest pilish-test-fork-waits-for-local-user-echo ()
  "Fork refuses to switch sessions while a local prompt is awaiting echo."
  (let ((shown-message nil)
        (rpc-called nil))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--status 'idle
            pilish--process 'mock-proc
            pilish--local-user-message "Hello")
      (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _args)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-fork)))
    (should-not rpc-called)
    (should (equal shown-message
                   "Pi: Wait for pi to echo your prompt before you fork"))))

;;; Fork at Point

(ert-deftest pilish-test-fork-at-point-correct-entry-id ()
  "Fork-at-point picks the right entry on second heading."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'idle)
          (pilish--process 'mock-proc)
          (forked-entry-id nil)
          (fork-messages (pilish-test--make-3turn-fork-messages)))
      (let ((inhibit-read-only t))
        (pilish-test--insert-chat-turns))
      (goto-char (point-min))
      (pilish-next-message)
      (pilish-next-message)
      (should (looking-at "You · 10:05"))
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq forked-entry-id (plist-get cmd :entryId))
                     (funcall cb '(:success t :data (:text "Second question"))))
                    ((equal (plist-get cmd :type) "get_state")
                     (funcall cb '(:success t :data (:sessionFile "/tmp/forked.jsonl"))))
                    ((equal (plist-get cmd :type) "get_messages")
                     (funcall cb '(:success t :data (:messages [])))))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'pilish--refresh-header) #'ignore))
        (pilish-fork-at-point))
      (should (equal forked-entry-id "u2")))))

(ert-deftest pilish-test-fork-at-point-confirmation-declined ()
  "Fork-at-point does nothing when confirmation is declined."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'idle)
          (pilish--process 'mock-proc)
          (fork-called nil)
          (fork-messages (pilish-test--make-3turn-fork-messages)))
      (let ((inhibit-read-only t))
        (pilish-test--insert-chat-turns))
      (goto-char (point-min))
      (pilish-next-message)
      (pilish-next-message)
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq fork-called t)))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
        (pilish-fork-at-point))
      (should-not fork-called))))

(ert-deftest pilish-test-fork-at-point-no-user-turn ()
  "Before first You heading, fork-at-point skips RPC."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'idle)
          (pilish--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (pilish-test--insert-chat-turns))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _) (setq rpc-called t))))
        (pilish-fork-at-point))
      (should-not rpc-called))))

(ert-deftest pilish-test-fork-at-point-streaming-guard ()
  "During streaming, fork-at-point skips RPC."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'streaming)
          (pilish--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (pilish-test--insert-chat-turns))
      (goto-char (point-min))
      (pilish-next-message)
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _) (setq rpc-called t))))
        (pilish-fork-at-point))
      (should-not rpc-called))))

(ert-deftest pilish-test-fork-at-point-settlement-guard ()
  "Fork-at-point skips RPC while the run is awaiting settlement."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'sending)
          (pilish--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (pilish-test--insert-chat-turns))
      (goto-char (point-min))
      (pilish-next-message)
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _) (setq rpc-called t)))
                ((symbol-function 'message) #'ignore))
        (pilish-fork-at-point))
      (should-not rpc-called))))

(ert-deftest pilish-test-fork-at-point-rpc-failure-shows-error ()
  "Fork-at-point shows an explicit RPC failure message."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'idle)
          (pilish--process 'mock-proc)
          (shown-message nil))
      (let ((inhibit-read-only t))
        (pilish-test--insert-chat-turns))
      (goto-char (point-min))
      (pilish-next-message)
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd cb)
                   (when (equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb '(:success nil :error "Unknown command: get_fork_messages")))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-fork-at-point))
      (should (equal shown-message
                     "Pi: Failed to get fork messages: Unknown command: get_fork_messages")))))

(defconst pilish-test--deep-tree-depth 1700
  "Depth used for deep-tree fork and flatten regression tests.")

(ert-deftest pilish-test-fork-at-point-deep-tree ()
  "Fork-at-point maps visible ordinals on deep histories."
  (with-temp-buffer
    (pilish-chat-mode)
    (let* ((depth pilish-test--deep-tree-depth)
           (pilish--status 'idle)
           (pilish--process 'mock-proc)
           (forked-entry-id nil)
           (fork-messages (pilish-test--make-deep-fork-messages depth))
           (expected-entry-id (format "n%d" (- depth 2))))
      (let ((inhibit-read-only t))
        (insert "Pi 1.0.0\n========\nWelcome\n\n"
                "You · 10:00\n===========\nOlder visible turn\n\n"
                "Assistant\n=========\nAnswer\n\n"
                "You · 10:01\n===========\nLatest visible turn\n\n"
                "Assistant\n=========\nAnswer\n"))
      (goto-char (point-min))
      (pilish-next-message)
      (should (looking-at "You · 10:00"))
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq forked-entry-id (plist-get cmd :entryId))
                     (funcall cb '(:success t :data (:text "Older visible turn"))))
                    ((equal (plist-get cmd :type) "get_state")
                     (funcall cb '(:success t :data (:sessionFile "/tmp/forked.jsonl"))))
                    ((equal (plist-get cmd :type) "get_messages")
                     (funcall cb '(:success t :data (:messages [])))))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'pilish--refresh-header) #'ignore))
        (pilish-fork-at-point))
      (should (equal forked-entry-id expected-entry-id)))))

(ert-deftest pilish-test-fork-at-point-compaction ()
  "Fork-at-point uses last-N mapping in compacted sessions."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'idle)
          (pilish--process 'mock-proc)
          (forked-entry-id nil)
          (fork-messages
           [(:entryId "u1" :text "Compacted away")
            (:entryId "u2" :text "After compaction")
            (:entryId "u3" :text "Latest")]))
      (let ((inhibit-read-only t))
        (insert "Pi 1.0.0\n========\nWelcome\n\n"
                "Compaction\n==========\nSummary of earlier conversation\n\n"
                "You · 10:05\n===========\nAfter compaction\n\n"
                "Assistant\n=========\nResponse\n\n"
                "You · 10:10\n===========\nLatest\n\n"
                "Assistant\n=========\nFinal\n"))
      (goto-char (point-min))
      (pilish-next-message)
      (should (looking-at "You · 10:05"))
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq forked-entry-id (plist-get cmd :entryId))
                     (funcall cb '(:success t :data (:text "After compaction"))))
                    ((equal (plist-get cmd :type) "get_state")
                     (funcall cb '(:success t :data (:sessionFile "/tmp/forked.jsonl"))))
                    ((equal (plist-get cmd :type) "get_messages")
                     (funcall cb '(:success t :data (:messages [])))))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'pilish--refresh-header) #'ignore))
        (pilish-fork-at-point))
      (should (equal forked-entry-id "u2")))))

;;; Fork Entry Resolution

(ert-deftest pilish-test-resolve-fork-entry-maps-ordinal ()
  "resolve-fork-entry maps ordinal to entry ID and preview."
  (let* ((fork-messages (pilish-test--make-3turn-fork-messages))
         (response (list :success t :data (list :messages fork-messages)))
         (result (pilish--resolve-fork-entry response 1 3)))
    (should (equal (car result) "u2"))
    (should (equal (cdr result) "Second question"))))

(ert-deftest pilish-test-resolve-fork-entry-compaction ()
  "resolve-fork-entry uses last-N mapping in compacted sessions."
  (let* ((fork-messages (pilish-test--make-3turn-fork-messages))
         (response (list :success t :data (list :messages fork-messages)))
         (result (pilish--resolve-fork-entry response 0 2)))
    (should (equal (car result) "u2"))))

(ert-deftest pilish-test-resolve-fork-entry-failure ()
  "resolve-fork-entry returns nil on failure."
  (let ((response '(:success nil :error "Network error")))
    (should-not (pilish--resolve-fork-entry response 0 3))))

(defun pilish-test--make-deep-linear-tree (depth)
  "Return a single-branch tree vector with DEPTH nested nodes.
The tree is built iteratively to avoid recursion in test setup."
  (let* ((leaf-id (1- depth))
         (node (list :id (format "n%d" leaf-id)
                     :type "message"
                     :role "user"
                     :preview (format "node %d" leaf-id)
                     :parentId (and (> leaf-id 0) (format "n%d" (1- leaf-id)))
                     :children [])))
    (dotimes (i (1- depth))
      (let ((id (- depth i 2)))
        (setq node (list :id (format "n%d" id)
                         :type "message"
                         :role "user"
                         :preview (format "node %d" id)
                         :parentId (and (> id 0) (format "n%d" (1- id)))
                         :children (vector node)))))
    (vector node)))

(defun pilish-test--make-deep-fork-messages (depth)
  "Return DEPTH chronological fork messages."
  (let ((messages (make-vector depth nil)))
    (dotimes (i depth)
      (aset messages i (list :entryId (format "n%d" i)
                             :text (format "node %d" i))))
    messages))

(ert-deftest pilish-test-flatten-tree-deep-linear-tree ()
  "flatten-tree handles deep linear trees without eval-depth overflow."
  (let* ((depth pilish-test--deep-tree-depth)
         (tree (pilish-test--make-deep-linear-tree depth))
         (index (pilish--flatten-tree tree)))
    (should (= (hash-table-count index) depth))))

;;; Active Branch Tree Walk

(ert-deftest pilish-test-active-branch-linear ()
  "Linear tree: u1 → a1 → u2 → a2 (leaf) returns both user IDs."
  (let* ((data (pilish-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("u2" nil "message" :role "user" :preview "More")
                '("a2" nil "message" :role "assistant" :preview "Sure")))
         (index (pilish--flatten-tree (plist-get data :tree)))
         (ids (pilish--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pilish-test-active-branch-branched ()
  "Branched tree: active branch u1 → a1 → u2 → a2, ignores u3 → a3."
  (let* ((data (pilish-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("u2" nil "message" :role "user" :preview "Path A")
                '("a2" nil "message" :role "assistant" :preview "Sure A")
                '("u3" "a1" "message" :role "user" :preview "Path B")
                '("a3" nil "message" :role "assistant" :preview "Sure B")))
         (index (pilish--flatten-tree (plist-get data :tree)))
         (ids (pilish--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pilish-test-active-branch-with-compaction ()
  "Tree with compaction node: u1 → a1 → compaction → u2 → a2."
  (let* ((data (pilish-test--build-tree
                '("u1" nil "message" :role "user" :preview "First")
                '("a1" nil "message" :role "assistant" :preview "Response")
                '("c1" nil "compaction" :tokensBefore 5000)
                '("u2" nil "message" :role "user" :preview "After compaction")
                '("a2" nil "message" :role "assistant" :preview "Still here")))
         (index (pilish--flatten-tree (plist-get data :tree)))
         (ids (pilish--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pilish-test-active-branch-with-metadata ()
  "Tree with model_change and thinking nodes: only user IDs returned."
  (let* ((data (pilish-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("m1" nil "model_change" :provider "anthropic" :modelId "claude-4")
                '("t1" nil "thinking_level_change" :thinkingLevel "high")
                '("u2" nil "message" :role "user" :preview "More")
                '("a2" nil "message" :role "assistant" :preview "Sure")))
         (index (pilish--flatten-tree (plist-get data :tree)))
         (ids (pilish--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pilish-test-active-branch-empty-tree ()
  "Empty tree returns empty list."
  (let* ((index (pilish--flatten-tree []))
         (ids (pilish--active-branch-user-ids index nil)))
    (should (equal ids nil))))

(ert-deftest pilish-test-active-branch-nil-leaf ()
  "Nil leafId returns empty list."
  (let* ((data (pilish-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")))
         (index (pilish--flatten-tree (plist-get data :tree)))
         (ids (pilish--active-branch-user-ids index nil)))
    (should (equal ids nil))))

;;;; State Reading from Input Buffer

(ert-deftest pilish-test-menu-model-description-from-input-buffer ()
  "Menu descriptions and display rows read state from the linked chat buffer."
  (let ((pilish-thinking-display 'visible))
    (pilish-test-with-mock-session "/tmp/pilish-test-state/"
      (let ((chat-buf (get-buffer (pilish-test--chat-buffer-name
                                   "/tmp/pilish-test-state/")))
            (input-buf (get-buffer (pilish-test--input-buffer-name
                                    "/tmp/pilish-test-state/"))))
        ;; Set state in chat buffer (where it lives)
        (with-current-buffer chat-buf
          (setq pilish--state
                '(:model (:name "Claude Opus 4.6" :id "claude-opus-4-6"
                          :provider "anthropic")
                  :thinking-level "high")
                pilish--thinking-display 'hidden))
        ;; Call from input buffer (where cursor normally is)
        (with-current-buffer input-buf
          (should (string-match-p "Opus 4.6"
                                  (pilish--menu-model-description)))
          (should (string-match-p "high"
                                  (pilish--menu-thinking-description)))
          (should (equal 'hidden
                         (pilish--menu-current-thinking-display-mode)))
          (should (equal 'visible
                         (pilish--menu-default-thinking-display-mode)))
          (should (equal "Model: Opus 4.6 • Thinking: high"
                         (pilish--menu-description))))))))

(ert-deftest pilish-test-toggle-default-thinking-display-affects-new-chats-only ()
  "Toggling the new-chat default leaves existing chat buffers alone."
  (let ((pilish-thinking-display 'hidden)
        shown-message)
    (with-temp-buffer
      (pilish-chat-mode)
      (should (eq pilish--thinking-display 'hidden))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-toggle-default-thinking-display))
      (should (eq pilish--thinking-display 'hidden))
      (should (eq pilish-thinking-display 'visible)))
    (with-temp-buffer
      (pilish-chat-mode)
      (should (eq pilish--thinking-display 'visible)))
    (should (equal shown-message
                   "Pi: New chat buffers will show completed thinking by default"))))

(defun pilish-test--menu-collapsed-thinking-stub (text)
  "Return the hidden stub shown for completed thinking TEXT."
  (pilish--thinking-hidden-stub
   (pilish--thinking-normalize-text text)))

(defun pilish-test--menu-history-with-two-thinking-blocks ()
  "Return history with two completed thinking blocks and plain assistant text."
  [(:role "assistant"
    :content [(:type "text" :text "Answer first.")
              (:type "thinking" :thinking "Need to double-check.")
              (:type "text" :text "Final answer.")]
    :timestamp 1704067200000)
   (:role "assistant"
    :content [(:type "text" :text "Another answer.")
              (:type "thinking" :thinking "Second thought.")
              (:type "text" :text "Done.")]
    :timestamp 1704067201000)])

(defun pilish-test--menu-history-with-thinking-and-tool ()
  "Return history with completed thinking and a long tool block."
  [(:role "assistant"
    :content [(:type "text" :text "Answer first.")
              (:type "thinking"
               :thinking "Need to double-check.\n\nSecond paragraph.")
              (:type "text" :text "Final answer.")
              (:type "toolCall" :id "call_1"
               :name "read"
               :arguments (:path "example.txt"))]
    :timestamp 1704067200000)
   (:role "toolResult" :toolCallId "call_1"
    :toolName "read"
    :content [(:type "text"
               :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10\nL11\nL12")]
    :isError :json-false
    :timestamp 1704067201000)])

(ert-deftest pilish-test-toggle-thinking-display-from-input-buffer-updates-linked-chat ()
  "Toggling from the input buffer updates the linked chat buffer."
  (let ((pilish-thinking-display 'visible)
        shown-message)
    (pilish-test-with-mock-session "/tmp/pilish-test-toggle-linked-chat/"
      (let ((chat-buf (get-buffer (pilish-test--chat-buffer-name
                                   "/tmp/pilish-test-toggle-linked-chat/")))
            (input-buf (get-buffer (pilish-test--input-buffer-name
                                    "/tmp/pilish-test-toggle-linked-chat/"))))
        (with-current-buffer chat-buf
          (setq pilish--thinking-display 'hidden)
          (pilish--display-session-history
           [(:role "assistant"
             :content [(:type "text" :text "Answer first.")
                       (:type "thinking" :thinking "Need to double-check.")]
             :timestamp 1704067200000)]
           (current-buffer)))
        (with-current-buffer input-buf
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (pilish-toggle-thinking-display)))
        (with-current-buffer chat-buf
          (let ((text (buffer-string)))
            (should (eq pilish--thinking-display 'visible))
            (should (string-match-p "^> Need to double-check\\.$" text))
            (should-not (string-match-p
                         (regexp-quote
                          (pilish-test--menu-collapsed-thinking-stub
                           "Need to double-check."))
                         text))))))
    (should (equal shown-message
                   "Pi: This chat now shows completed thinking"))))

(ert-deftest pilish-test-toggle-thinking-display-overrides-per-block-states ()
  "Whole-buffer toggles apply one display mode to every completed thinking block."
  (let ((pilish-thinking-display 'hidden)
        shown-message)
    (with-temp-buffer
      (pilish-chat-mode)
      (pilish--display-session-history
       (pilish-test--menu-history-with-two-thinking-blocks)
       (current-buffer))
      (goto-char (point-min))
      (search-forward (pilish-test--menu-collapsed-thinking-stub
                       "Need to double-check."))
      (beginning-of-line)
      (pilish-toggle-tool-section)
      (let ((text (buffer-string)))
        (should (string-match-p "^> Need to double-check\\.$" text))
        (should (string-match-p
                 (regexp-quote
                  (pilish-test--menu-collapsed-thinking-stub
                   "Second thought."))
                 text)))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-toggle-thinking-display))
      (let ((text (buffer-string)))
        (should (eq pilish--thinking-display 'visible))
        (should (string-match-p "^> Need to double-check\\.$" text))
        (should (string-match-p "^> Second thought\\.$" text))
        (should-not (string-match-p
                     (regexp-quote
                      (pilish-test--menu-collapsed-thinking-stub
                       "Need to double-check."))
                     text))
        (should-not (string-match-p
                     (regexp-quote
                      (pilish-test--menu-collapsed-thinking-stub
                       "Second thought."))
                     text)))
      (pilish-toggle-thinking-display)
      (let ((text (buffer-string)))
        (should (eq pilish--thinking-display 'hidden))
        (should (string-match-p
                 (regexp-quote
                  (pilish-test--menu-collapsed-thinking-stub
                   "Need to double-check."))
                 text))
        (should (string-match-p
                 (regexp-quote
                  (pilish-test--menu-collapsed-thinking-stub
                   "Second thought."))
                 text))
        (should-not (string-match-p "^> Need to double-check\\.$" text))
        (should-not (string-match-p "^> Second thought\\.$" text))))
    (should (equal shown-message
                   "Pi: This chat now shows completed thinking"))))

(ert-deftest pilish-test-toggle-thinking-display-without-canonical-messages-leaves-buffer-alone ()
  "Without canonical messages, toggling updates only future completed-thinking rendering."
  (let ((pilish-thinking-display 'visible)
        shown-message)
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--status 'idle)
      (let ((inhibit-read-only t))
        (insert "Keep existing buffer text\n"))
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (pilish-toggle-thinking-display))
        (should (eq pilish--thinking-display 'hidden))
        (should (equal before (buffer-string)))))
    (should (equal shown-message
                   "Pi: This chat now hides completed thinking"))))

(ert-deftest pilish-test-toggle-thinking-display-keeps-local-user-message-visible ()
  "Thinking-display toggles keep a pending local user echo visible."
  (let ((pilish-thinking-display 'visible)
        shown-message)
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--status 'idle
            pilish--local-user-message "Hello"
            pilish--canonical-messages
            [(:role "assistant"
              :content [(:type "thinking" :thinking "Need to double-check.")]
              :timestamp 1704067200000)])
      (pilish--display-user-message "Hello")
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (pilish-toggle-thinking-display))
        (should (eq pilish--thinking-display 'hidden))
        (should (equal before (buffer-string)))
        (pilish--handle-display-event
         '(:type "message_start"
           :message (:role "user"
                     :content [(:type "text" :text "Hello")]
                     :timestamp 1704067201000)))
        (should (equal before (buffer-string)))
        (should-not pilish--local-user-message)))
    (should (equal shown-message
                   "Pi: This chat now hides completed thinking"))))

(ert-deftest pilish-test-toggle-thinking-display-keeps-live-custom-message-visible ()
  "Whole-buffer thinking toggles must not delete live custom messages."
  (let ((pilish-thinking-display 'visible))
    (with-temp-buffer
      (pilish-chat-mode)
      (pilish--display-session-history
       [(:role "assistant"
         :content [(:type "text" :text "Answer first.")
                   (:type "thinking" :thinking "Need to double-check.")]
         :timestamp 1704067200000)]
       (current-buffer))
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "custom" :display t :content "Extension note: keep me")))
      (pilish--handle-display-event
       '(:type "message_end"
         :message (:role "custom" :display t :content "Extension note: keep me")))
      (pilish-toggle-thinking-display)
      (let ((text (buffer-string)))
        (should (eq pilish--thinking-display 'hidden))
        (should (string-match-p "Answer first\\." text))
        (should (string-match-p "Extension note: keep me" text))
        (should (string-match-p
                 (regexp-quote
                  (pilish-test--menu-collapsed-thinking-stub
                   "Need to double-check."))
                 text))))))

(ert-deftest pilish-test-toggle-thinking-display-preserves-expanded-tool-block ()
  "Whole-buffer thinking toggles must not reset expanded tool output."
  (let ((pilish-thinking-display 'hidden))
    (with-temp-buffer
      (pilish-chat-mode)
      (pilish--display-session-history
       (pilish-test--menu-history-with-thinking-and-tool)
       (current-buffer))
      (goto-char (point-min))
      (let ((button (next-button (point-min))))
        (should button)
        (pilish--toggle-tool-output button))
      (should (string-match-p "L12" (buffer-string)))
      (should (string-match-p "\\[-\\]" (buffer-string)))
      (pilish-toggle-thinking-display)
      (let ((text (buffer-string)))
        (should (string-match-p "L12" text))
        (should (string-match-p "\\[-\\]" text))
        (should-not (string-match-p "\\.\\.\\. ([0-9]+ more lines)" text))))))

(ert-deftest pilish-test-menu-model-description-uses-short-name ()
  "Menu model description shows shortened name, not full \"Claude Opus 4.6\"."
  (pilish-test-with-mock-session "/tmp/pilish-test-short/"
    (let ((chat-buf (get-buffer (pilish-test--chat-buffer-name
                                 "/tmp/pilish-test-short/"))))
      (with-current-buffer chat-buf
        (setq pilish--state
              '(:model (:name "Claude Opus 4.6")))
        (should (string-match-p "Opus 4.6"
                                (pilish--menu-model-description)))
        (should-not (string-match-p "Claude"
                                    (pilish--menu-model-description)))))))

;;;; Model Selector Completion Styles

(ert-deftest pilish-test-select-model-case-insensitive ()
  "Model selector matches case-insensitively: \"opus\" finds \"Opus 4.6\"."
  (let ((models '((:name "Claude Opus 4.6" :id "opus-4-6" :provider "anthropic")
                  (:name "Claude Sonnet 4.5" :id "sonnet-4-5" :provider "anthropic")))
        captured-case captured-styles)
    (let ((buf (generate-new-buffer "*pilish-chat:flex-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'pilish--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _cmd _cb)))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq captured-case completion-ignore-case
                             captured-styles completion-styles)
                       "Opus 4.6")))
            (with-current-buffer buf
              (pilish-chat-mode)
              (setq pilish--process :fake-proc
                    pilish--state '(:model (:name "Claude Sonnet 4.5")))
              (pilish-select-model)))
        (with-current-buffer buf (setq pilish--process nil))
        (kill-buffer buf)))
    (should captured-case)
    (should (memq 'flex captured-styles))))

(ert-deftest pilish-test-select-model-flex-matches-substring ()
  "Flex completion: \"code\" matches \"GPT-5.1 Codex Max\"."
  (let* ((names '("Opus 4.6" "GPT-5.1 Codex Max"))
         (completion-ignore-case t)
         (completion-styles '(basic flex))
         (result (completion-all-completions "code" names nil (length "code"))))
    (when (consp result) (setcdr (last result) nil))
    (should (= 1 (length result)))
    (should (string-match-p "Codex" (car result)))))

(ert-deftest pilish-test-select-model-flex-matches-noncontiguous ()
  "Flex completion: \"o46\" matches \"Opus 4.6\" (non-contiguous)."
  (let* ((names '("Opus 4.6" "Sonnet 4.5" "GPT-5.1 Codex Max"))
         (completion-ignore-case t)
         (completion-styles '(basic flex))
         (result (completion-all-completions "o46" names nil (length "o46"))))
    (when (consp result) (setcdr (last result) nil))
    (should (= 1 (length result)))
    (should (string-match-p "Opus 4.6" (car result)))))

(ert-deftest pilish-test-select-model-unique-match-auto-selects ()
  "When initial-input uniquely matches one model, skip completing-read."
  (let ((models '((:name "Claude Opus 4.6" :id "opus-4-6" :provider "anthropic")
                  (:name "Claude Sonnet 4.5" :id "sonnet-4-5" :provider "anthropic")))
        completing-read-called set-model-id)
    (let ((buf (generate-new-buffer "*pilish-chat:auto-select*")))
      (unwind-protect
          (cl-letf (((symbol-function 'pilish--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd _cb)
                       (setq set-model-id (plist-get cmd :modelId))))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq completing-read-called t)
                       "Opus 4.6")))
            (with-current-buffer buf
              (pilish-chat-mode)
              (setq pilish--process :fake-proc
                    pilish--state '(:model (:name "Claude Sonnet 4.5")))
              (pilish-select-model "op46")))
        (with-current-buffer buf (setq pilish--process nil))
        (kill-buffer buf)))
    (should-not completing-read-called)
    (should (equal set-model-id "opus-4-6"))))

(ert-deftest pilish-test-select-model-no-match-shows-message ()
  "When initial-input matches nothing, show message and don't set model."
  (let ((models '((:name "Claude Opus 4.6" :id "opus-4-6" :provider "anthropic")))
        set-model-called last-message)
    (let ((buf (generate-new-buffer "*pilish-chat:no-match*")))
      (unwind-protect
          (cl-letf (((symbol-function 'pilish--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (&rest _) (setq set-model-called t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq last-message (apply #'format fmt args)))))
            (with-current-buffer buf
              (pilish-chat-mode)
              (setq pilish--process :fake-proc
                    pilish--state '(:model (:name "Claude Opus 4.6")))
              (pilish-select-model "zzzzz")))
        (with-current-buffer buf (setq pilish--process nil))
        (kill-buffer buf)))
    (should-not set-model-called)
    (should (string-match-p "No model matching" last-message))))

(ert-deftest pilish-test-select-model-multiple-matches-opens-selector ()
  "When initial-input matches multiple models, fall through to completing-read."
  (let ((models '((:name "Claude Opus 4" :id "opus-4" :provider "anthropic")
                  (:name "Claude Opus 4.5" :id "opus-4-5" :provider "anthropic")
                  (:name "Claude Sonnet 4.5" :id "sonnet-4-5" :provider "anthropic")))
        completing-read-called captured-initial)
    (let ((buf (generate-new-buffer "*pilish-chat:multi-match*")))
      (unwind-protect
          (cl-letf (((symbol-function 'pilish--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _cmd _cb)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt _coll _pred _req initial &rest _)
                       (setq completing-read-called t
                             captured-initial initial)
                       "Opus 4")))
            (with-current-buffer buf
              (pilish-chat-mode)
              (setq pilish--process :fake-proc
                    pilish--state '(:model (:name "Claude Sonnet 4.5")))
              (pilish-select-model "opus")))
        (with-current-buffer buf (setq pilish--process nil))
        (kill-buffer buf)))
    (should completing-read-called)
    (should (equal captured-initial "opus"))))

(ert-deftest pilish-test-filter-thinking-levels-removes-model-aliases ()
  "Thinking selector only offers distinct provider reasoning levels."
  (should
   (equal
    (pilish--filter-thinking-level-aliases
     '("off" "minimal" "low" "medium" "high" "xhigh" "max")
     '(:thinkingLevelMap
       (:minimal "low" :low "low" :medium "medium"
        :high "high" :xhigh "max" :max "max")))
    '("off" "low" "medium" "high" "max"))))

(ert-deftest pilish-test-filter-thinking-levels-removes-unsupported-levels ()
  "Explicitly unsupported model thinking levels are omitted."
  (should
   (equal
    (pilish--filter-thinking-level-aliases
     '("off" "minimal" "low" "medium" "high" "xhigh" "max")
     '(:thinkingLevelMap
       (:minimal :null :low :null :medium :null :high "high"
        :xhigh "xhigh" :max :null)))
    '("off" "high" "xhigh"))))

(ert-deftest pilish-test-get-available-thinking-levels-errors-on-rpc-failure ()
  "Thinking-level selection does not offer unsupported fallback values."
  (cl-letf (((symbol-function 'pilish--rpc-sync)
             (lambda (&rest _)
               '(:success :false :error "Unknown command"))))
    (should-error (pilish--get-available-thinking-levels :fake-proc)
                  :type 'user-error)))

(ert-deftest pilish-test-select-thinking-refreshes-state-from-server ()
  "Thinking selector refreshes state so server clamping is visible in the UI."
  (let (captured-prompt captured-collection rpc-commands last-message)
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--process :fake-proc
            pilish--state '(:thinking-level "low"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (setq captured-prompt prompt
                         captured-collection collection)
                   "high"))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ["off" "minimal" "low" "medium" "high" "xhigh"])))))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd callback)
                   (push cmd rpc-commands)
                   (pcase (plist-get cmd :type)
                     ("set_thinking_level"
                      (funcall callback '(:success t :command "set_thinking_level")))
                     ("get_state"
                      (funcall callback
                               '(:success t
                                 :data (:thinkingLevel "medium"
                                        :isStreaming nil
                                        :isCompacting nil)))))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq last-message (apply #'format fmt args)))))
        (pilish-select-thinking)
        (should (equal (plist-get pilish--state :thinking-level) "medium"))))
    (should (equal captured-prompt "Thinking level (current: low): "))
    (should (equal captured-collection
                   '("off" "minimal" "low" "medium" "high" "xhigh")))
    (let ((commands (nreverse rpc-commands)))
      (should (equal (mapcar (lambda (cmd) (plist-get cmd :type)) commands)
                     '("set_thinking_level" "get_state")))
      (should (equal (car commands)
                     '(:type "set_thinking_level" :level "high")))
      (should (equal (cadr commands) '(:type "get_state"))))
    (should (equal last-message "Pi: Thinking level: medium"))))

(ert-deftest pilish-test-select-thinking-noop-when-unchanged ()
  "Thinking selector does not send RPC when the user picks the current level."
  (let (rpc-called)
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--process :fake-proc
            pilish--state '(:thinking-level "medium"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "medium"))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ("off" "minimal" "low" "medium" "high" "xhigh"))))))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t))))
        (pilish-select-thinking)))
    (should-not rpc-called)))

(ert-deftest pilish-test-select-thinking-errors-without-process ()
  "Thinking selector should fail loudly when no pi process is running."
  (with-temp-buffer
    (pilish-chat-mode)
    (should-error (pilish-select-thinking) :type 'user-error)))

(ert-deftest pilish-test-select-thinking-shows-rpc-error ()
  "Thinking selector reports set_thinking_level RPC failures."
  (let (rpc-commands shown-message)
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--process :fake-proc
            pilish--state '(:thinking-level "low"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "high"))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ("off" "minimal" "low" "medium" "high" "xhigh"))))))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd callback)
                   (push cmd rpc-commands)
                   (funcall callback '(:success :false :error "unsupported"))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-select-thinking)
        (should (equal (plist-get pilish--state :thinking-level) "low"))))
    (should (equal rpc-commands
                   '((:type "set_thinking_level" :level "high"))))
    (should (equal shown-message
                   "Pi: Failed to set thinking level: unsupported"))))

(ert-deftest pilish-test-select-thinking-warns-when-state-refresh-fails ()
  "Thinking selector warns instead of guessing when state refresh fails."
  (let (shown-message)
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--process :fake-proc
            pilish--state '(:thinking-level "low"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "high"))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ("off" "minimal" "low" "medium" "high" "xhigh"))))))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd callback)
                   (pcase (plist-get cmd :type)
                     ("set_thinking_level"
                      (funcall callback '(:success t :command "set_thinking_level")))
                     ("get_state"
                      (funcall callback '(:success nil :error "state unavailable"))))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-select-thinking)
        (should (equal (plist-get pilish--state :thinking-level) "low"))))
    (should (equal shown-message
                   "Pi: Thinking level updated, but failed to refresh state: state unavailable"))))

(ert-deftest pilish-test-thinking-selector-uses-t-key-leaving-T-for-templates ()
  "Main menu keeps `t', `h', and `H' free without taking Templates `T'."
  (let ((pilish--commands
         '((:name "review" :description "Code review" :source "prompt"))))
    (unwind-protect
        (progn
          (pilish--rebuild-commands-menu)
          (transient-setup 'pilish-menu)
          (let ((thinking-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "t"))
                             transient--suffixes))
                (chat-display-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "h"))
                             transient--suffixes))
                (default-display-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "H"))
                             transient--suffixes))
                (templates-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "T"))
                             transient--suffixes)))
            (should thinking-suffix)
            (should (eq (oref thinking-suffix command)
                        'pilish-select-thinking))
            (should chat-display-suffix)
            (should (equal "This chat"
                           (transient-format-description chat-display-suffix)))
            (should default-display-suffix)
            (should (equal "New chat default"
                           (transient-format-description default-display-suffix)))
            (should templates-suffix)))
      (ignore-errors (transient-remove-suffix 'pilish-menu '(3))))))

;;; sourceInfo normalization

(ert-deftest pilish-test-normalize-command-extracts-source-info ()
  "Normalizer lifts sourceInfo.scope and sourceInfo.path to top level."
  (let* ((raw (list :name "fix" :source "prompt"
                    :sourceInfo '(:scope "user" :path "/home/me/.pi/fix.md")))
         (norm (pilish--normalize-command raw)))
    (should (equal (plist-get norm :location) "user"))
    (should (equal (plist-get norm :path) "/home/me/.pi/fix.md"))
    (should (equal (plist-get norm :name) "fix"))
    (should-not (plist-get norm :sourceInfo))))

(ert-deftest pilish-test-normalize-command-anchors-remote-source-path ()
  "Command source paths from Pi are normalized to Emacs/TRAMP paths."
  (let* ((anchor "/ssh:pi-host:/home/pi/project/")
         (raw (list :name "fix" :source "prompt"
                    :sourceInfo '(:scope "project" :path "prompts/fix.md")))
         (norm (pilish--normalize-command raw anchor)))
    (should (equal (plist-get norm :path)
                   "/ssh:pi-host:/home/pi/project/prompts/fix.md"))))

(ert-deftest pilish-test-normalize-command-maps-temporary-scope-to-path ()
  "Pi's temporary command scope belongs in the menu's path bucket."
  (let* ((raw '(:name "one-off" :source "prompt"
                :sourceInfo (:scope "temporary" :path "/tmp/one-off.md")))
         (norm (pilish--normalize-command raw "/tmp/project/")))
    (should (equal (plist-get norm :location) "path"))))

(ert-deftest pilish-test-normalize-command-ignores-unsafe-source-path ()
  "Command normalization ignores unsafe passive source path metadata."
  (let* ((bad (concat "/tmp/a" (string ?\0) "b.md"))
         (raw (list :name "fix" :source "prompt"
                    :sourceInfo (list :scope "project" :path bad)))
         (norm (pilish--normalize-command raw)))
    (should (equal (plist-get norm :location) "project"))
    (should-not (plist-get norm :path))
    (should-not (plist-get norm :sourceInfo))))

(ert-deftest pilish-test-normalize-command-ignores-mismatched-remote-source-path ()
  "Command normalization ignores source paths from another TRAMP remote."
  (let* ((anchor "/ssh:pi-host:/home/pi/project/")
         (raw (list :name "fix" :source "prompt"
                    :sourceInfo '(:scope "project"
                                  :path "/ssh:other:/tmp/fix.md")))
         (norm (pilish--normalize-command raw anchor)))
    (should (equal (plist-get norm :location) "project"))
    (should-not (plist-get norm :path))
    (should-not (plist-get norm :sourceInfo))))

(ert-deftest pilish-test-normalize-command-builtin-paths ()
  "Normalizer drops `builtin:' command identifiers instead of storing paths.
The check precedes path expansion for both wire and lifted inputs, under
local and remote anchors.  A real file whose name merely contains
`builtin:' stays a usable path."
  (let ((real (symbol-function 'pilish--passive-emacs-path)))
    (dolist (anchor '("/tmp/project/" "/ssh:pi-host:/home/pi/project/"))
      (cl-letf (((symbol-function 'pilish--passive-emacs-path)
                 (lambda (path &optional path-anchor)
                   (when (and (stringp path)
                              (string-prefix-p "builtin:" path))
                     (error "builtin: identifier reached path conversion: %S"
                            path))
                   (funcall real path path-anchor))))
        (dolist (value '("builtin:mcp" "builtin:"))
          ;; Wire input: raw sourceInfo with a temporary scope.
          (let ((norm (pilish--normalize-command
                       (list :name "mcp" :source "extension"
                             :sourceInfo (list :scope "temporary"
                                               :path value))
                       anchor)))
            (should (equal (plist-get norm :location) "path"))
            (should (equal (plist-get norm :source) "extension"))
            (should-not (plist-member norm :path))
            (should-not (plist-member norm :sourceInfo)))
          ;; Lifted input: path already at top level.
          (let ((norm (pilish--normalize-command
                       (list :name "mcp" :source "extension" :path value)
                       anchor)))
            (should-not (plist-member norm :location))
            (should-not (plist-member norm :path))))))
    ;; Non-string top-level paths are still not stored.
    (let ((norm (pilish--normalize-command
                 (list :name "odd" :source "prompt" :path 42)
                 "/tmp/project/")))
      (should-not (plist-member norm :path)))
    ;; A real file whose name merely contains `builtin:' stays a usable
    ;; path; only the prefix is reserved.
    (let ((norm (pilish--normalize-command
                 (list :name "notes" :source "prompt"
                       :sourceInfo '(:scope "project"
                                            :path "prompts/builtin:notes.md"))
                 "/tmp/project/")))
      (should (equal (plist-get norm :location) "project"))
      (should (equal (plist-get norm :path)
                     "/tmp/project/prompts/builtin:notes.md")))))

(ert-deftest pilish-test-edit-command-source-opens-remote-emacs-path ()
  "Editing command sources opens the Emacs/TRAMP path for remote sessions."
  (let ((chat-buf (generate-new-buffer "*test-edit-command-source*"))
        (opened-path nil))
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (pilish--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq opened-path path))))
            (pilish--edit-command-source "/home/pi/.pi/prompts/fix.md"))
          (should (equal opened-path
                         "/ssh:pi-host:/home/pi/.pi/prompts/fix.md")))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-export-html-uses-process-local-remote-paths ()
  "Remote export sends process-local outputPath and reports an Emacs path."
  (let ((chat-buf (generate-new-buffer "*test-export-html-remote*"))
        (proc (start-process "test-export-html-remote" nil "cat"))
        (sent-command nil)
        (message-text nil))
    (set-process-query-on-exit-flag proc nil)
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (pilish--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (setq pilish--process proc)
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc command callback)
                       (setq sent-command command)
                       (funcall callback
                                '(:success t
                                  :data (:path "/home/pi/project/reports/out.html")))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (pilish-export-html "reports/out.html"))
          (should (equal (plist-get sent-command :outputPath)
                         "/home/pi/project/reports/out.html"))
          (should (equal message-text
                         "Pi: Exported to /ssh:pi-host:/home/pi/project/reports/out.html")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-export-html-ignores-unsafe-response-path ()
  "Malformed backend export paths do not escape the async callback."
  (let ((chat-buf (generate-new-buffer "*test-export-html-unsafe*"))
        (proc (start-process "test-export-html-unsafe" nil "cat"))
        (message-text nil)
        (bad-path (concat "/tmp/out" (string ?\0) ".html")))
    (set-process-query-on-exit-flag proc nil)
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (pilish--set-chat-session-identity "/tmp/project/")
          (setq pilish--process proc)
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _command callback)
                       (funcall callback
                                (list :success t
                                      :data (list :path bad-path)))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (pilish-export-html))
          (should (equal message-text
                         "Pi: Exported, but Pi did not return a usable path")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-export-html-preserves-remote-home-output-path ()
  "Remote export sends home-relative outputPath without local expansion."
  (let ((chat-buf (generate-new-buffer "*test-export-html-remote-home*"))
        (proc (start-process "test-export-html-remote-home" nil "cat"))
        (sent-command nil))
    (set-process-query-on-exit-flag proc nil)
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (pilish--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (setq pilish--process proc)
          (let ((file-name-handler-alist nil))
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc command _callback)
                         (setq sent-command command))))
              (pilish-export-html "~/out.html")))
          (should (equal (plist-get sent-command :outputPath)
                         "~/out.html")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-normalize-command-without-source-info ()
  "Normalizer leaves commands unchanged when sourceInfo is absent."
  (let* ((raw '(:name "ext" :source "extension"))
         (norm (pilish--normalize-command raw)))
    (should (equal (plist-get norm :name) "ext"))
    (should-not (plist-get norm :location))
    (should-not (plist-get norm :path))))

(ert-deftest pilish-test-normalize-command-partial-source-info ()
  "Normalizer handles sourceInfo with scope but no path."
  (let* ((raw '(:name "s" :source "skill"
                :sourceInfo (:scope "project")))
         (norm (pilish--normalize-command raw)))
    (should (equal (plist-get norm :location) "project"))
    (should-not (plist-get norm :path))))

;;; Silence observation during session ownership changes

(ert-deftest pilish-test-inactivity-manual-compact-reservation-is-not-monitored ()
  "An idle compact RPC reservation is busy for submission, not for silence."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (let (notices)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) notices))))
        (with-current-buffer input (pilish-compact))
        (should (equal notices '("Pi: Compacting...")))
        (should (equal "compact" (plist-get (car commands) :type)))
        (should (with-current-buffer chat (pilish--session-busy-p)))
        (should (eq 'idle (buffer-local-value 'pilish--status chat)))
        (setq now 2000.0)
        (pilish-test--assert-inactivity input nil)
        (should-not (buffer-local-value 'pilish--inactivity-timer chat))
        (should (= 1 (hash-table-count (pilish--get-pending-requests proc))))
        (pilish-test--stdout proc '(:type "compaction_start" :reason "manual")
                             '(:type "compaction_end" :reason "manual" :aborted t)))
      (should (equal notices '("Pi: Compaction cancelled"
                               "Pi: Compacting..." "Pi: Compacting..."))))
    (setq now 3000.0)
    (should (eq 'idle (buffer-local-value 'pilish--status chat)))
    (should (with-current-buffer chat (pilish--session-busy-p)))
    (should (= 1 (hash-table-count (pilish--get-pending-requests proc))))
    (pilish-test--assert-inactivity input nil)
    (should-not (buffer-local-value 'pilish--inactivity-timer chat))))

(ert-deftest pilish-test-inactivity-transition-cancellation-rearms-same-process ()
  "Transition guards cancel monitoring, and finishing preserves existing age."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (pilish-test--stdout proc '(:type "agent_start"))
    (with-current-buffer chat (setq pilish--followup-queue '("keep")))
    (let* ((old (buffer-local-value 'pilish--inactivity-timer chat))
           (generation (with-current-buffer chat
                         (pilish--begin-session-transition proc))))
      (setq now 1400.0)
      (pilish-test--assert-inactivity input nil)
      (should-not (memq old timer-list))
      (with-current-buffer chat (pilish--finish-session-transition generation))
      (let ((current (buffer-local-value 'pilish--inactivity-timer chat)) refreshed)
        (should-not (eq current old))
        (pilish-test--assert-inactivity input "thinking (no output 6m)")
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&rest _) (push (current-buffer) refreshed))))
          (setq now 900.0 refreshed nil)
          (pilish-test--fire-timer old)
          (should-not refreshed)
          (setq now 1400.0))
        (should (memq current timer-list))
        (should (eq current (buffer-local-value 'pilish--inactivity-timer chat)))
        (should (equal 1000.0 (process-get proc 'pilish-last-output-time)))
        (should (equal '("keep") (buffer-local-value 'pilish--followup-queue chat)))
        (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
        (pilish-test--fire-timer current)
        (pilish-test--assert-inactivity input "thinking (no output 6m)")))))

(ert-deftest pilish-test-inactivity-session-reset-rebases-same-process ()
  "Explicit session reset invalidates the old observer even without replacement."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (pilish-test--stdout proc '(:type "agent_start"))
    (let ((old (buffer-local-value 'pilish--inactivity-timer chat)))
      (should (memq old timer-list))
      (setq now 1400.0)
      (with-current-buffer chat (pilish--reset-session-state))
      (pilish-test--assert-inactivity input nil)
      (should-not (memq old timer-list))
      (should (eq proc (buffer-local-value 'pilish--process chat)))
      ;; Reset's existing status semantics must not be changed by the UI.
      (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
      (setq now 1700.0)
      (pilish-test--assert-inactivity input "idle (no output 5m)"))))

(ert-deftest pilish-test-inactivity-resume-adopts-new-session-on-same-process ()
  "Successful same-process resume resets observation, not just history text."
  (let* ((dir (pilish-test--make-temp-directory "pilish-inactivity-resume-"))
         (path (expand-file-name "target.jsonl" dir)))
    (unwind-protect
        (pilish-test-with-inactivity-session (chat input proc commands now)
          (let (notices)
            (pilish-test--write-session-file path "target" (directory-file-name dir))
            (pilish-test--stdout proc '(:type "agent_start"))
            (let ((old (buffer-local-value 'pilish--inactivity-timer chat)))
              (should (memq old timer-list))
              (pilish-test--stdout proc '(:type "agent_end" :messages [])
                                   '(:type "agent_settled"))
              (should (eq 'idle (buffer-local-value 'pilish--status chat)))
              (setq now 1400.0)
              ;; Deliver synchronous callbacks so stdout cannot mask adoption's
              ;; separate responsibility to rebase the observation clock.
              (cl-letf (((symbol-function 'pilish--rpc-async)
                         (lambda (_proc cmd cb)
                           (pcase (plist-get cmd :type)
                             ("switch_session"
                              (funcall cb '(:success t :data (:cancelled :false))))
                             ("get_state"
                              (funcall cb `(:success t :data
                                            (:isStreaming t :isCompacting :false
                                             :sessionId "resumed" :sessionFile ,path))))
                             ("get_messages"
                              (funcall cb '(:success t :data (:messages []))))
                             ("get_commands"
                              (funcall cb '(:success t :data (:commands []))))
                             (_ (ert-fail (format "Unexpected resume RPC: %S" cmd))))))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) notices))))
                (pilish--resume-selected-session proc chat path))
              ;; A resumed streaming owner intentionally prevents history
              ;; rerender (and its success notice); preserve that behavior.
              (should-not notices)
              (should (equal "resumed"
                             (plist-get (buffer-local-value 'pilish--state chat) :session-id)))
              (should (eq proc (buffer-local-value 'pilish--process chat)))
              (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
              (pilish-test--assert-inactivity input nil)
              (should-not (memq old timer-list))
              (setq now 1700.0)
              (pilish-test--assert-inactivity input "idle (no output 5m)"))))
      (delete-directory dir t))))

(provide 'pilish-menu-test)
;;; pilish-menu-test.el ends here
