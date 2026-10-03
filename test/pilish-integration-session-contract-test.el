;;; pilish-integration-session-contract-test.el --- Shared session contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Session persistence behaviors that remain valuable at the subprocess
;; boundary, even though session metadata formatting is already unit-tested.

;;; Code:

(require 'ert)
(require 'seq)
(require 'pilish-integration-test-common)

(pilish-integration-deftest
    (session-contract-first-user-persists)
  "The first user persists a named session even if its assistant is interrupted."
  (let* ((initial (pilish--rpc-sync proc '(:type "get_state")
                                  pilish-test-rpc-timeout))
         (session-file (plist-get (plist-get initial :data) :sessionFile))
         (user-end nil)
         (got-agent-settled nil))
    (should (eq (plist-get initial :success) t))
    (should (stringp session-file))
    (should-not (file-exists-p session-file))
    (let ((response (pilish--rpc-sync
                     proc '(:type "set_session_name" :name "Named before first user")
                     pilish-test-rpc-timeout)))
      (should (eq (plist-get response :success) t)))
    (let* ((response (pilish--rpc-sync proc '(:type "get_state")
                                    pilish-test-rpc-timeout))
           (named (plist-get response :data)))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get named :sessionFile) session-file))
      (should (equal (plist-get named :sessionName) "Named before first user"))
      (should-not (file-exists-p session-file)))
    (push (lambda (event)
            (pcase (plist-get event :type)
              ("message_end"
               (when (equal (plist-get (plist-get event :message) :role) "user")
                 (setq user-end event)))
              ("agent_settled" (setq got-agent-settled t))))
          pilish--event-handlers)
    (let ((response (pilish--rpc-sync
                     proc `(:type "prompt"
                            :message ,pilish-integration--prompt-abort-message)
                     pilish-test-rpc-timeout)))
      (should (eq (plist-get response :success) t)))
    (should (pilish-test-wait-until
             (lambda () user-end)
             pilish-test-rpc-timeout pilish-test-poll-interval proc))
    ;; Pi emits the user event before appending it.  Poll complete user bytes
    ;; outside the event callback; an existing header alone is not enough.
    (should (pilish-test-wait-until
             (lambda ()
               (and (file-exists-p session-file)
                    (with-temp-buffer
                      (insert-file-contents session-file)
                      (re-search-forward
                       "\"role\"[[:space:]]*:[[:space:]]*\"user\"[^\n]*\n" nil t))))
             pilish-test-rpc-timeout 0.01 proc))
    (let ((response (pilish--rpc-sync proc '(:type "abort")
                                    pilish-test-rpc-timeout)))
      (should (eq (plist-get response :success) t)))
    (should (pilish-test-wait-until
             (lambda () got-agent-settled)
             pilish-test-rpc-timeout pilish-test-poll-interval proc))
    (let* ((records (with-temp-buffer
                      (insert-file-contents session-file)
                      (mapcar (lambda (line)
                                (json-parse-string line :object-type 'plist
                                                   :array-type 'array))
                              (split-string (buffer-string) "\n" t))))
           (entries (cdr records))
           (names (seq-filter
                   (lambda (entry) (equal (plist-get entry :type) "session_info"))
                   entries))
           (users (seq-filter
                   (lambda (entry)
                     (and (equal (plist-get entry :type) "message")
                          (equal (plist-get (plist-get entry :message) :role) "user")))
                   entries)))
      (should (equal (plist-get (car records) :type) "session"))
      (should (= (plist-get (car records) :version) 3))
      (should (= (seq-count (lambda (entry)
                             (equal (plist-get entry :type) "session"))
                           records)
                 1))
      (should (= (length names) 1))
      (should (equal (plist-get (car names) :name) "Named before first user"))
      (should (= (length users) 1))
      (should (equal (pilish-integration--message-text
                      (plist-get (car users) :message))
                     pilish-integration--prompt-abort-message)))))

(pilish-integration-deftest
    (session-contract-name-persists-across-session-file)
  "Setting a session name persists backend-visible session metadata."
  (let ((got-agent-settled nil))
    (push (lambda (event)
            (when (equal (plist-get event :type) "agent_settled")
              (setq got-agent-settled t)))
          pilish--event-handlers)
    (let ((prompt-response (pilish--rpc-sync
                            proc
                            `(:type "prompt"
                              :message
                              ,pilish-integration--prompt-session-materialize-message)
                            pilish-test-rpc-timeout)))
      (should prompt-response)
      (should (eq (plist-get prompt-response :success) t)))
    (let* ((state-before (pilish-integration--rpc-until
                          proc
                          '(:type "get_state")
                          #'pilish-integration--response-has-existing-session-file-p
                          pilish-test-integration-timeout))
           (data-before (plist-get state-before :data))
           (session-file (plist-get data-before :sessionFile)))
      (should state-before)
      (should session-file)
      (should (file-exists-p session-file))
      (let ((name-response (pilish--rpc-sync
                            proc
                            '(:type "set_session_name" :name "Integration Test Session")
                            pilish-test-rpc-timeout)))
        (should name-response)
        (should (eq (plist-get name-response :success) t))
        (should (equal (plist-get name-response :command) "set_session_name")))
      (let* ((state-after (pilish-integration--rpc-until
                           proc
                           '(:type "get_state")
                           (lambda (response)
                             (let* ((data (plist-get response :data))
                                    (response-session-file (plist-get data :sessionFile))
                                    (response-session-name (plist-get data :sessionName)))
                               (and (equal response-session-file session-file)
                                    (equal response-session-name
                                           "Integration Test Session"))))
                           pilish-test-rpc-timeout))
             (data-after (plist-get state-after :data)))
        (should state-after)
        (should (equal (plist-get data-after :sessionFile) session-file))
        (should (equal (plist-get data-after :sessionName)
                       "Integration Test Session")))
      (unless got-agent-settled
        (let ((abort-response (pilish--rpc-sync proc '(:type "abort")
                                                         pilish-test-rpc-timeout)))
          (should abort-response)
          (should (eq (plist-get abort-response :success) t))
          (should (equal (plist-get abort-response :command) "abort")))
        (with-timeout (pilish-test-rpc-timeout
                       (ert-fail "Timeout waiting for agent_settled after session abort"))
          (while (not got-agent-settled)
            (accept-process-output proc pilish-test-poll-interval))))
      (let* ((final-state (pilish--rpc-sync proc '(:type "get_state")
                                                     pilish-test-rpc-timeout))
             (final-data (plist-get final-state :data)))
        (should (equal (plist-get final-data :sessionFile) session-file))
        (should (equal (plist-get final-data :sessionName)
                       "Integration Test Session"))
        (should (eq (plist-get final-data :isStreaming) :false)))
      (with-temp-buffer
        (insert-file-contents session-file)
        (should (string-match-p "session_info" (buffer-string)))
        (should (string-match-p "Integration Test Session" (buffer-string)))))))

(provide 'pilish-integration-session-contract-test)
;;; pilish-integration-session-contract-test.el ends here
