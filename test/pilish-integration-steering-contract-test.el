;;; pilish-integration-steering-contract-test.el --- Shared steering contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Steering remains a distinct subprocess-boundary behavior, so it keeps its
;; own contract instead of being folded into the happy-path prompt test.

;;; Code:

(require 'ert)
(require 'seq)
(require 'pilish-integration-test-common)

(pilish-integration-deftest
    (steering-contract-queues-and-delivers)
  "A steer command queued during streaming is delivered visibly later on."
  (let ((got-agent-start nil)
        (got-agent-settled nil)
        (queued-delivered nil)
        (user-message-events nil))
    (push (lambda (event)
            (when (equal (plist-get event :type) "agent_start")
              (setq got-agent-start t))
            (when (and (equal (plist-get event :type) "message_start")
                       (equal (plist-get (plist-get event :message) :role) "user"))
              (push event user-message-events)
              (when (string-match-p "queued-steer-test"
                                    (pilish-integration--message-text
                                     (plist-get event :message)))
                (setq queued-delivered t)))
            (when (equal (plist-get event :type) "agent_settled")
              (setq got-agent-settled t)))
          pilish--event-handlers)
    (let ((prompt-response (pilish--rpc-sync
                            proc
                            `(:type "prompt"
                              :message
                              ,pilish-integration--prompt-steering-initial-message)
                            pilish-test-rpc-timeout)))
      (should prompt-response)
      (should (eq (plist-get prompt-response :success) t)))
    (with-timeout (pilish-test-rpc-timeout
                   (ert-fail "Timeout waiting for agent_start before steer"))
      (while (not got-agent-start)
        (accept-process-output proc pilish-test-poll-interval)))
    (let ((steer-response (pilish--rpc-sync
                           proc
                           `(:type "steer"
                             :message
                             ,pilish-integration--prompt-steering-queued-message)
                           pilish-test-rpc-timeout)))
      (should steer-response)
      (should (eq (plist-get steer-response :success) t))
      (should (equal (plist-get steer-response :command) "steer"))
      (should (equal (plist-get (plist-get steer-response :data) :disposition)
                     "queued")))
    (with-timeout (pilish-test-integration-timeout
                   (ert-fail "Timeout waiting for queued steer delivery"))
      (while (not queued-delivered)
        (accept-process-output proc pilish-test-poll-interval)))
    (let ((abort-response (pilish--rpc-sync proc '(:type "abort")
                                                     pilish-test-rpc-timeout)))
      (should abort-response)
      (should (eq (plist-get abort-response :success) t))
      (should (equal (plist-get abort-response :command) "abort")))
    (with-timeout (pilish-test-rpc-timeout
                   (ert-fail "Timeout waiting for agent_settled after steering abort"))
      (while (not got-agent-settled)
        (accept-process-output proc pilish-test-poll-interval)))
    (setq user-message-events (nreverse user-message-events))
    (should (= (length user-message-events) 2))
    (let ((queued-msg (seq-find
                       (lambda (event)
                         (string-match-p "queued-steer-test"
                                         (pilish-integration--message-text
                                          (plist-get event :message))))
                       user-message-events)))
      (should queued-msg))
    (let* ((state (pilish--rpc-sync proc '(:type "get_state")
                                             pilish-test-rpc-timeout))
           (data (plist-get state :data)))
      (should (eq (plist-get data :isStreaming) :false)))))

(provide 'pilish-integration-steering-contract-test)
;;; pilish-integration-steering-contract-test.el ends here
