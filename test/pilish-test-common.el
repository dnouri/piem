;;; pilish-test-common.el --- Shared test utilities and configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Common definitions shared across Pilish test files.
;; Centralizes timeout values, fake-pi launch helpers, the mock-session
;; macro, and toolcall streaming helpers for easy adjustment (e.g., slow CI).

;;; Code:

(require 'cl-lib) ; for cl-letf in mock-session macro
(require 'ert)
(require 'json)

;;; Timeout Configuration

(defvar pilish-test-short-wait 0.5
  "Short wait in seconds for async operations to complete.")

(defvar pilish-test-poll-interval 0.1
  "Polling interval in seconds for waiting loops.")

(defvar pilish-test-rpc-timeout 10
  "Timeout in seconds for RPC calls in tests.")

(defvar pilish-test-integration-timeout 600
  "Timeout in seconds for real-backend integration tests.
The shared contract still includes multi-turn model interactions, so the real
lane keeps a generous timeout for slower CI runners.")

(defvar pilish-test-gui-timeout 180
  "Timeout in seconds for GUI tests.
The GUI suite is fake-backed, but it still waits for real Emacs window updates,
redisplay, and subprocess event delivery.")

;;;; Formatting Helpers

(defun pilish-test-format-elapsed (seconds)
  "Format SECONDS as a human-readable duration with millisecond precision."
  (format "%.3fs" (float seconds)))

;;;; JSON Fixtures

(defvar pilish-test--fixture-dir
  (expand-file-name "test/fixtures/"
                    (or (and load-file-name
                             (file-name-directory
                              (directory-file-name
                               (file-name-directory load-file-name))))
                        (locate-dominating-file default-directory "Makefile")
                        default-directory))
  "Directory containing JSON test fixtures.")

(defun pilish-test--read-json-fixture (filename)
  "Read JSON fixture FILENAME from test/fixtures/ and return as plist."
  (let ((path (expand-file-name filename pilish-test--fixture-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (json-parse-string (buffer-string) :object-type 'plist))))

;;;; Fake-pi Helpers

(defconst pilish-test-fake-pi-script
  (expand-file-name "support/fake_pi.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the fake-pi harness script.")

(defun pilish-test-python-executable ()
  "Return a Python executable path, or skip if none is available."
  (or (executable-find "python3")
      (executable-find "python")
      (ert-skip "python3 or python not found")))

(defun pilish-test-fake-pi-executable ()
  "Return the command list for launching the fake-pi harness.
Uses python3 in the test suite so local and CI runs do not need a
separate uv dependency, while the script itself still keeps uv metadata
and an executable shebang for manual runs."
  (list (pilish-test-python-executable)
        pilish-test-fake-pi-script))

(defun pilish-test-fake-pi-extra-args (scenario &optional extra-args)
  "Return fake-pi CLI args for SCENARIO plus optional EXTRA-ARGS."
  (append (list "--scenario" scenario) extra-args))

(defun pilish-test-backend-spec
    (backend default-fake-scenario &optional fake-scenario fake-extra-args)
  "Return shared backend launch data for BACKEND.
DEFAULT-FAKE-SCENARIO is used when BACKEND is `fake' and FAKE-SCENARIO is
nil.  FAKE-EXTRA-ARGS are appended after the generated fake-pi scenario args."
  (pcase backend
    ('fake
     (let* ((scenario (or fake-scenario default-fake-scenario))
            (extra-args (pilish-test-fake-pi-extra-args
                         scenario fake-extra-args)))
       (list :name 'fake
             :label (format "fake:%s" scenario)
             :executable (pilish-test-fake-pi-executable)
             :extra-args extra-args
             :scenario scenario)))
    ('real
     (list :name 'real
           :label "real"
           :executable pilish-executable
           :extra-args pilish-extra-args))
    (_
     (error "Unknown test backend: %S" backend))))

;;;; Batch Emacs Helpers

(defconst pilish-test--batch-result-marker
  "\n\036PI-CODING-AGENT-BATCH-RESULT-8F3D6A\037\n"
  "Marker separating child Emacs diagnostics from its Lisp result.")

(defun pilish-test--read-batch-emacs-result (expression)
  "Evaluate EXPRESSION in a fresh batch Emacs and return its Lisp result.
Initializes packages, then re-prepends the current project root to
`load-path' so the checkout under test wins over any installed copy.
Diagnostic output before the framed result is ignored."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (repo-root (file-name-directory (locate-library "pilish")))
         (output-buffer (generate-new-buffer " *pilish-batch-emacs*"))
         (exit-code (call-process emacs nil output-buffer nil
                                  "--batch" "-Q" "-L" repo-root
                                  "--eval" "(require 'package)"
                                  "--eval"
                                  "(let ((dir (getenv \"PACKAGE_USER_DIR\")))\n  (when dir\n    (setq package-user-dir\n          (directory-file-name (expand-file-name dir)))))"
                                  "--eval" "(package-initialize)"
                                  "--eval" "(setq load-prefer-newer t)"
                                  "--eval"
                                  (format "(setq load-path (cons %S load-path))"
                                          repo-root)
                                  "--eval"
                                  (format "(prin1 (prog1 %s (princ %S)))"
                                          expression
                                          pilish-test--batch-result-marker))))
    (unwind-protect
        (progn
          (unless (eq 0 exit-code)
            (error "Batch Emacs exited with %s" exit-code))
          (with-current-buffer output-buffer
            (goto-char (point-min))
            (unless (search-forward pilish-test--batch-result-marker
                                    nil t)
              (error "Batch Emacs result marker missing: %s"
                     (buffer-string)))
            (read (current-buffer))))
      (kill-buffer output-buffer))))

(defun pilish-test--markdown-load-state (library)
  "Return Markdown association state after requiring LIBRARY in batch Emacs."
  (pilish-test--read-batch-emacs-result
   (format "(progn
  (defvar major-mode-remap-alist nil)
  (defvar treesit-major-mode-remap-alist nil)
  (let ((before-auto (copy-tree auto-mode-alist))
        (before-major-remap (copy-tree major-mode-remap-alist))
        (before-treesit-remap (copy-tree treesit-major-mode-remap-alist)))
    (require '%s)
    (prin1 (list
            :auto-unchanged (equal before-auto auto-mode-alist)
            :major-remap-unchanged (equal before-major-remap major-mode-remap-alist)
            :treesit-remap-unchanged (equal before-treesit-remap treesit-major-mode-remap-alist)
            :md-mode-defined (fboundp 'md-ts-mode)
            :md-mode-maybe-defined (fboundp 'md-ts-mode-maybe)
            :before-md-association (assoc \"\\.md\\'\" before-auto)
            :after-md-association (assoc \"\\.md\\'\" auto-mode-alist)
            :before-major-markdown-remap (alist-get 'markdown-mode before-major-remap)
            :after-major-markdown-remap (alist-get 'markdown-mode major-mode-remap-alist)
            :before-treesit-markdown-remap (alist-get 'markdown-mode before-treesit-remap)
            :after-treesit-markdown-remap (alist-get 'markdown-mode treesit-major-mode-remap-alist)))))"
           library)))

;;;; Waiting Helpers

(defun pilish-test-wait-until (predicate &optional timeout poll-interval process)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse.
POLL-INTERVAL controls how often to check (default
`pilish-test-poll-interval'). If PROCESS is non-nil, it is
passed to `accept-process-output' to allow process I/O.

Returns the predicate value, or nil on timeout."
  (let* ((timeout (or timeout pilish-test-short-wait))
         (poll-interval (or poll-interval pilish-test-poll-interval))
         (start (float-time))
         (result (funcall predicate)))
    (while (and (not result)
                (< (- (float-time) start) timeout))
      (accept-process-output process poll-interval)
      (setq result (funcall predicate)))
    result))

(defun pilish-test-wait-for-process-exit (process &optional timeout)
  "Wait until PROCESS is no longer live, up to TIMEOUT seconds.
Returns non-nil if the process exits before the timeout."
  (pilish-test-wait-until
   (lambda () (not (process-live-p process)))
   (or timeout pilish-test-short-wait)
   pilish-test-poll-interval
   process))

;;;; Toolcall Streaming Helpers

(defun pilish-test--count-matches (regexp string)
  "Count non-overlapping occurrences of REGEXP in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defmacro pilish-test--with-streaming-assistant (&rest body)
  "Run BODY in a temp chat buffer with an active assistant stream."
  (declare (indent 0) (debug body))
  `(with-temp-buffer
     (pilish-chat-mode)
     (pilish--handle-display-event '(:type "agent_start"))
     (pilish--handle-display-event
      '(:type "message_start" :message (:role "assistant")))
     ,@body))

(defun pilish-test--toolcall (id tool-name args)
  "Return a toolCall content block for ID, TOOL-NAME, and ARGS."
  `(:type "toolCall" :id ,id :name ,tool-name :arguments ,args))

(defun pilish-test--send-assistant-message-update (message-event)
  "Send delta-only MESSAGE-EVENT using Pi's current RPC wire shape."
  (pilish--handle-display-event
   `(:type "message_update" :assistantMessageEvent ,message-event)))

(defun pilish-test--send-toolcall-message-update
    (event-type content-index toolcalls &optional _delta)
  "Render one cumulative toolcall snapshot for focused display tests.
EVENT-TYPE controls streaming versus completed presentation.  CONTENT-INDEX
selects the toolCall in TOOLCALLS.  Protocol-facing tests should instead use
`pilish-test--send-assistant-message-update' with raw JSON deltas."
  (let ((tool-call (nth content-index toolcalls)))
    (unless tool-call
      (error "No test tool call at content index %s" content-index))
    (pilish--reconcile-toolcall-preview-block
     content-index tool-call event-type)))

(defmacro pilish-test--with-toolcall (tool-name args &rest body)
  "Set up a chat buffer with a streaming tool call, then run BODY.
Creates a temp buffer in chat mode, fires agent_start, message_start,
and toolcall_start for TOOL-NAME with ARGS (a plist).  The tool call
ID is \"call_1\" and contentIndex is 0."
  (declare (indent 2) (debug (sexp sexp body)))
  `(pilish-test--with-streaming-assistant
     (pilish-test--send-toolcall-message-update
      "toolcall_start" 0
      (list (pilish-test--toolcall "call_1" ,tool-name ,args)))
     ,@body))

(defun pilish-test--send-delta (tool-name args)
  "Send a toolcall_delta event for TOOL-NAME with ARGS.
Uses tool call ID \"call_1\" and contentIndex 0."
  (pilish-test--send-toolcall-message-update
   "toolcall_delta" 0
   (list (pilish-test--toolcall "call_1" tool-name args))
   "x"))

(defconst pilish-test--prompt-image-fixtures
  '((png . "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGP4z8AAAAMBAQDJ/pLvAAAAAElFTkSuQmCC")
    (jpeg . "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAARCAABAAEDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDi6KKK+ZP3E//Z")
    (gif . "R0lGODdhAQABAIEAAP8AAAAAAAAAAAAAACwAAAAAAQABAAAIBAABBAQAOw==")
    (webp . "UklGRjwAAABXRUJQVlA4IDAAAADQAQCdASoBAAEAAUAmJaACdLoB+AADsAD+8ut//NgVzXPv9//S4P0uD9Lg/9KQAAA="))
  "Valid one-pixel raster images used by prompt attachment tests.")

(defun pilish-test--prompt-image-base64 (type)
  "Return the base64 fixture for image TYPE."
  (or (alist-get type pilish-test--prompt-image-fixtures)
      (error "No prompt image fixture for %S" type)))

(defun pilish-test--write-prompt-image (path type)
  "Write the binary prompt image fixture TYPE to PATH and return PATH."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert (base64-decode-string (pilish-test--prompt-image-base64 type)))))
  path)

(defun pilish-test--input-header (&optional input)
  "Return INPUT's header without properties, defaulting to the current buffer."
  (with-current-buffer (or input (current-buffer))
    (substring-no-properties (pilish--header-line-string))))

(defun pilish-test--attach-image (path)
  "Attach prompt image PATH through the public interactive command."
  (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) path)))
    (call-interactively #'pilish-attach-image)))

(defun pilish-test--menu-suffix-command (prefix key)
  "Return the command bound to KEY in transient PREFIX's layout."
  (plist-get (cdr (transient-get-suffix prefix key)) :command))

(defun pilish-test--attach-image-via-menu (path &optional clear)
  "Invoke the attach-menu image entry for PATH, with prefix arg when CLEAR."
  (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) path)))
    (let* ((command (pilish-test--menu-suffix-command 'pilish-attach-menu "i"))
           (current-prefix-arg (and clear '(4))))
      (call-interactively command))))

(cl-defmacro pilish-test-with-prompt-image-session
    ((dir chat-buf input-buf) &rest body)
  "Run BODY in a fresh vision-capable mock session."
  (declare (indent 1) (debug ((symbolp symbolp symbolp) body)))
  `(let ((,dir (pilish-test--make-temp-directory "pi-prompt-image-")))
     (unwind-protect
         (pilish-test-with-mock-session ,dir
           (let ((,chat-buf (get-buffer (pilish-test--chat-buffer-name ,dir)))
                 (,input-buf (get-buffer (pilish-test--input-buffer-name ,dir))))
             (with-current-buffer ,chat-buf
               (setq pilish--status 'idle
                     pilish--state '(:model (:name "Vision" :input ["text" "image"]))))
             ,@body))
       (delete-directory ,dir t))))

;;;; Mock Session

(cl-defmacro pilish-test-with-rpc-session ((chat input proc commands) &rest body)
  "Run BODY with linked CHAT/INPUT buffers and real PROC request correlation.
Capture outbound JSON as COMMANDS (newest first), without sending it to Pi.
Tests can deliver events and correlated responses through the production
handlers; no mock bypasses pending request registration or removal."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let ((,chat (generate-new-buffer " *pilish-rpc-chat*"))
         (,input (generate-new-buffer " *pilish-rpc-input*"))
         (,proc (start-process "pilish-test-rpc" nil "cat"))
         ,commands)
     (unwind-protect
         (progn
           (set-process-query-on-exit-flag ,proc nil)
           (with-current-buffer ,chat
             (pilish-chat-mode)
             (setq pilish--process ,proc pilish--input-buffer ,input))
           (with-current-buffer ,input
             (pilish-input-mode)
             (setq pilish--chat-buffer ,chat))
           (process-put ,proc 'pilish-chat-buffer ,chat)
           (pilish--register-display-handler ,proc)
           (cl-letf (((symbol-function 'pilish--send-string)
                      (lambda (_process line)
                        (push (pilish--parse-json-line line) ,commands)))
                     ((symbol-function 'pilish--refresh-header) #'ignore))
             ,@body))
       (pilish-test--kill-live-buffers ,input ,chat)
       (when (process-live-p ,proc) (delete-process ,proc)))))

(defmacro pilish-test-with-mock-session (dir &rest body)
  "Execute BODY with a mocked pi session in DIR, cleaning up after.
DIR should be a unique directory path, typically created with
`pilish-test--make-temp-directory'.
Mocks `project-current', dependency checks, process startup, and display.
Automatically cleans up chat and input buffers."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,dir))
     (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
               ((symbol-function 'pilish--check-dependencies) #'ignore)
               ((symbol-function 'pilish--start-process) (lambda (_) nil))
               ((symbol-function 'pilish--display-buffers) #'ignore))
       (unwind-protect
           (progn (pilish) ,@body)
         (pilish-test--kill-session-buffers ,dir)))))

(defun pilish-test--nested-summary-lines (root-id &optional omit-duration)
  "Return ordered (ID . SUMMARY) rows displayed for ROOT-ID.
When OMIT-DURATION is non-nil, exclude only duration spans.  Keep duplicate
nil IDs; buttons and notices are outside summary spans."
  (let ((position (point-min)) rows)
    (while (< position (point-max))
      (let ((end (next-single-property-change
                  position 'pilish-nested-summary nil (point-max))))
        (when (and (get-text-property position 'pilish-nested-summary)
                   (equal root-id (get-text-property position 'pilish-nested-root-id)))
          (push (cons (get-text-property position 'pilish-nested-call-id)
                      (if omit-duration
                          (mapconcat
                           (lambda (range) (pilish--visible-text (car range) (cdr range)))
                           (pilish--ranges-excluding-property
                            position end 'pilish-nested-duration) "")
                        (pilish--visible-text position end)))
                rows))
        (setq position end)))
    (nreverse rows)))

(defun pilish-test--chat-buffer-name (dir &optional session)
  "Return the chat buffer name for DIR and optional SESSION."
  (pilish--buffer-name :chat dir session))

(defun pilish-test--input-buffer-name (dir &optional session)
  "Return the input buffer name for DIR and optional SESSION."
  (pilish--buffer-name :input dir session))

(defun pilish-test--kill-session-buffers (dir &optional session)
  "Kill chat and input buffers for DIR and optional SESSION."
  (pilish-test--kill-live-buffers
   (get-buffer (pilish-test--input-buffer-name dir session))
   (get-buffer (pilish-test--chat-buffer-name dir session))))

(defun pilish-test--kill-live-buffers (&rest buffers)
  "Kill each live buffer in BUFFERS without interactive session prompts."
  (let ((pilish-quit-without-confirmation t))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun pilish-test--make-temp-directory (prefix)
  "Create and return a fresh temporary directory for tests.
PREFIX is forwarded to `make-temp-file'.  The returned path always has a
trailing slash so it behaves like `default-directory'."
  (file-name-as-directory (make-temp-file prefix t)))

(defun pilish-test--write-session-file (path &optional text cwd)
  "Write a minimal pi session file to PATH.
When TEXT is non-nil, include it as the first user message.  When CWD is
non-nil, include it in the session header."
  (with-temp-file path
    (insert (json-encode `(:type "session" :id "test"
                           ,@(when cwd (list :cwd cwd))))
            "\n")
    (when text
      (insert (json-encode `(:type "message"
                             :message (:role "user"
                                       :content [(:type "text" :text ,text)])))
              "\n"))))

(defun pilish-test--write-chat-buffer (chat prefix &optional appended-text)
  "Save CHAT to a temp markdown file and return the file name.
PREFIX is forwarded to `make-temp-file'.  When APPENDED-TEXT is non-nil,
append it to CHAT before saving.  The temp file is created without initial
contents so tests can verify the full write result explicitly."
  (let ((file (make-temp-file prefix nil ".md")))
    (delete-file file)
    (with-current-buffer chat
      (when appended-text
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert appended-text)))
      (write-file file))
    file))

;;;; Tree Fixtures

(defun pilish-test--build-tree (&rest specs)
  "Build a conversation tree from flat node SPECS.
Each SPEC is (ID PARENT-OVERRIDE TYPE &rest PROPS) where:
- ID is the node identifier string
- PARENT-OVERRIDE is nil (auto-chain to previous node) or a parent ID
- TYPE is \"message\", \"compaction\", \"model_change\", etc.
- PROPS are keyword plist properties (:role, :preview, etc.)
First node with nil PARENT-OVERRIDE becomes the root.
Returns (:tree VECTOR :leafId LAST-ID)."
  (let ((nodes (make-hash-table :test 'equal))
        (child-ids (make-hash-table :test 'equal))
        (roots nil)
        (prev-id nil)
        (last-id nil))
    ;; Pass 1: create nodes, track parent-child relationships
    (dolist (spec specs)
      (let* ((id (nth 0 spec))
             (parent-override (nth 1 spec))
             (type (nth 2 spec))
             (props (nthcdr 3 spec))
             (parent-id (or parent-override prev-id))
             (node (append (list :id id :type type)
                           (when parent-id (list :parentId parent-id))
                           props)))
        (puthash id node nodes)
        (if parent-id
            (puthash parent-id
                     (append (gethash parent-id child-ids) (list id))
                     child-ids)
          (push id roots))
        (setq prev-id id
              last-id id)))
    ;; Pass 2: build nested structure with :children vectors
    (cl-labels ((build (id)
                  (let* ((node (gethash id nodes))
                         (kids (gethash id child-ids))
                         (child-vec (if kids
                                        (apply #'vector (mapcar #'build kids))
                                      [])))
                    (append node (list :children child-vec)))))
      (list :tree (apply #'vector (mapcar #'build (nreverse roots)))
            :leafId last-id))))

(defun pilish-test--make-3turn-fork-messages ()
  "Return get_fork_messages payload for three user turns."
  [(:entryId "u1" :text "First question")
   (:entryId "u2" :text "Second question")
   (:entryId "u3" :text "Third question")])

;;;; Chat Buffer Fixtures

(defun pilish-test--insert-chat-turns ()
  "Insert a 3-turn chat with setext headings into current buffer.
Returns the buffer with content ready for navigation tests."
  (insert "Pi 1.0.0\n========\nWelcome\n\n"
          "You · 10:00\n===========\nFirst question\n\n"
          "Assistant\n=========\nFirst answer\n\n"
          "You · 10:05\n===========\nSecond question\n\n"
          "Assistant\n=========\nSecond answer\n\n"
          "You · 10:10\n===========\nThird question\n\n"
          "Assistant\n=========\nThird answer\n"))

;;;; Inactivity observation fixtures

(defvar pilish-session-inactivity-timeout)

(defmacro pilish-test-with-clock (now &rest body)
  "Run BODY with NOW initially 1000.0; explicit time conversions stay real.
Timers use ordinary Emacs scheduling.  Do not wait with this frozen clock."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,now 1000.0) (real-float-time (symbol-function 'float-time)))
     (cl-letf (((symbol-function 'float-time)
                (lambda (&optional time)
                  (if time (funcall real-float-time time) ,now))))
       ,@body)))

(defmacro pilish-test-with-repeating-timer-allocations (timers &rest body)
  "Observe real repeating timer allocations in TIMERS while running BODY.
Keep cancelled allocations too; count each timer once even when
`run-with-timer' delegates to `run-at-time'.  On exit cancel only these
new repeating timers.  BODY should contain synchronous test actions."
  (declare (indent 1) (debug (symbolp body)))
  `(let (,timers)
     (cl-flet ((observe (timer)
                 (when (timer--repeat-delay timer)
                   (cl-pushnew timer ,timers :test #'eq))
                 timer))
       (unwind-protect
           (progn
             (advice-add 'run-at-time :filter-return #'observe)
             (advice-add 'run-with-timer :filter-return #'observe)
             ,@body)
         (advice-remove 'run-at-time #'observe)
         (advice-remove 'run-with-timer #'observe)
         (mapc #'cancel-timer ,timers)))))

(defun pilish-test--fire-timer (timer)
  "Deliver TIMER's production callback, even after cancellation."
  (should (timerp timer))
  (apply (timer--function timer) (timer--args timer)))

(defun pilish-test--adopt-rpc-process (chat process)
  "Exercise genuine adoption of fixture PROCESS in CHAT."
  (with-current-buffer chat
    ;; The base RPC fixture deliberately assigns its process directly.
    (setq pilish--process nil)
    (pilish--set-process process)))

(cl-defmacro pilish-test-with-inactivity-session
    ((chat input proc commands now) &rest body)
  "Run BODY in an adopted RPC session with clock NOW and real timers.
The RPC fixture kills its buffers and cleans up their timers on exit."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp symbolp) body)))
  `(pilish-test-with-clock ,now
     (pilish-test-with-rpc-session (,chat ,input ,proc ,commands)
       (let ((pilish-session-inactivity-timeout 300))
         (pilish-test--adopt-rpc-process ,chat ,proc)
         ,@body))))

(defun pilish-test--assert-inactivity (input expected)
  "Assert INPUT has EXPECTED warning text and face, or no warning when nil."
  (let ((header (with-current-buffer input (pilish--header-line-string))))
    (ert-info ((format "input=%s expected=%S header=%S"
                       (buffer-name input) expected header))
      (if expected
          (let ((start (string-match (regexp-quote expected) header)))
            (should start)
            (should (eq 'warning (get-text-property start 'face header))))
        (should-not (string-match-p "no output" header))))
    header))

(defun pilish-test--stdout (process &rest events)
  "Deliver EVENTS together in one real stdout filter call for PROCESS.
Use parser-style JSON values: t, :false and :null.  The legacy :json-false
sentinel is not accepted."
  (pilish--process-filter
   process (mapconcat (lambda (event)
                        (concat (json-serialize event :false-object :false
                                                :null-object :null)
                                "\n"))
                      events "")))

(provide 'pilish-test-common)
;;; pilish-test-common.el ends here
