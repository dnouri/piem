;;; pilish-integration-prompt-contract-test.el --- Shared prompt contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared prompt-turn behaviors that the fake must model and the real backend
;; must continue to honor.

;;; Code:

(require 'ert)
(require 'seq)
(require 'pilish-integration-test-common)

(pilish-integration-deftest
    (prompt-contract-lifecycle)
  "One prompt proves the shared lifecycle contract end to end."
  (let* ((initial-state (pilish--rpc-sync proc '(:type "get_state")
                                                   pilish-test-rpc-timeout))
         (initial-count (plist-get (plist-get initial-state :data) :messageCount))
         (events nil)
         (got-agent-settled nil)
         (assistant-start nil)
         (assistant-end nil)
         (prompt-response nil))
    (push (lambda (event)
            (push event events)
            (pcase (plist-get event :type)
              ("message_start"
               (when (equal (plist-get (plist-get event :message) :role) "assistant")
                 (setq assistant-start event)))
              ("message_end"
               (when (equal (plist-get (plist-get event :message) :role) "assistant")
                 (setq assistant-end event)))
              ("agent_settled"
               (setq got-agent-settled t))))
          pilish--event-handlers)
    (setq prompt-response
          (pilish--rpc-sync
           proc
           `(:type "prompt"
             :message ,pilish-integration--prompt-lifecycle-message)
           pilish-test-rpc-timeout))
    (should prompt-response)
    (should (eq (plist-get prompt-response :success) t))
    (should (equal (plist-get prompt-response :command) "prompt"))
    (should (equal (plist-get (plist-get prompt-response :data) :disposition)
                   "started"))
    (with-timeout (pilish-test-integration-timeout
                   (ert-fail "Timeout waiting for prompt lifecycle to finish"))
      (while (not got-agent-settled)
        (accept-process-output proc pilish-test-poll-interval)))
    (setq events (nreverse events))
    (ert-info ("agent_start should be emitted")
      (should (seq-find (lambda (event)
                          (equal (plist-get event :type) "agent_start"))
                        events)))
    (ert-info ("assistant message_start should be emitted")
      (should assistant-start))
    (ert-info ("assistant text should stream via message_update text_delta")
      (should (seq-find (lambda (event)
                          (and (equal (plist-get event :type) "message_update")
                               (equal (plist-get (plist-get event :assistantMessageEvent) :type)
                                      "text_delta")))
                        events)))
    (ert-info ("assistant message_end should be emitted")
      (should assistant-end))
    (ert-info ("agent_end is followed by final agent_settled")
      (should (seq-find (lambda (event) (equal (plist-get event :type) "agent_end"))
                        events))
      (should (equal (plist-get (car (last events)) :type) "agent_settled")))
    (let* ((final-state (pilish--rpc-sync proc '(:type "get_state")
                                                   pilish-test-rpc-timeout))
           (final-data (plist-get final-state :data))
           (final-count (plist-get final-data :messageCount)))
      (ert-info ("backend should return to idle after completion")
        (should (eq (plist-get final-data :isStreaming) :false)))
      (ert-info ("message count should increase after one completed turn")
        (should (> final-count initial-count))))))

(pilish-integration-deftest
    (prompt-contract-abort-stops-streaming)
  "Aborting a running prompt leaves the backend idle again."
  (let ((got-agent-start nil)
        (got-agent-settled nil)
        (prompt-response nil))
    (push (lambda (event)
            (pcase (plist-get event :type)
              ("agent_start"
               (setq got-agent-start t))
              ("agent_settled"
               (setq got-agent-settled t))))
          pilish--event-handlers)
    (setq prompt-response
          (pilish--rpc-sync
           proc
           `(:type "prompt"
             :message ,pilish-integration--prompt-abort-message)
           pilish-test-rpc-timeout))
    (should (eq (plist-get prompt-response :success) t))
    (with-timeout (pilish-test-rpc-timeout
                   (ert-fail "Timeout waiting for abortable prompt to start streaming"))
      (while (not got-agent-start)
        (accept-process-output proc pilish-test-poll-interval)))
    (let ((abort-response (pilish--rpc-sync proc '(:type "abort")
                                                     pilish-test-rpc-timeout)))
      (should abort-response)
      (should (eq (plist-get abort-response :success) t))
      (should (equal (plist-get abort-response :command) "abort")))
    (with-timeout (pilish-test-rpc-timeout
                   (ert-fail "Timeout waiting for agent_settled after abort"))
      (while (not got-agent-settled)
        (accept-process-output proc pilish-test-poll-interval)))
    (let* ((state (pilish--rpc-sync proc '(:type "get_state")
                                             pilish-test-rpc-timeout))
           (data (plist-get state :data)))
      (should (eq (plist-get data :isStreaming) :false)))))

(provide 'pilish-integration-prompt-contract-test)
;;; pilish-integration-prompt-contract-test.el ends here
