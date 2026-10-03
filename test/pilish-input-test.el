;;; pilish-input-test.el --- Tests for pilish-input -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for input history, history isearch, send/abort commands,
;; message queuing, file reference completion, path completion,
;; and slash command completion — the input buffer layer.

;;; Code:

(require 'ert)
(require 'pilish)
(require 'pilish-test-common)

;;; Sending Prompts

(ert-deftest pilish-test-send-extracts-text ()
  "pilish-send extracts text from input buffer and clears it."
  (let ((sent-text nil))
    (pilish-test-with-mock-session "/tmp/pilish-test-send1/"
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success)))))
        (with-current-buffer "*pilish-input:/tmp/pilish-test-send1/*"
          (insert "Hello, pi!")
          (pilish-send)
          (should (equal sent-text "Hello, pi!"))
          (should (string-empty-p (buffer-string))))))))

(ert-deftest pilish-test-send-empty-is-noop ()
  "pilish-send with empty buffer does nothing."
  (let ((send-called nil))
    (pilish-test-with-mock-session "/tmp/pilish-test-send2/"
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pilish-input:/tmp/pilish-test-send2/*"
          (pilish-send)
          (should-not send-called))))))

(ert-deftest pilish-test-send-whitespace-only-is-noop ()
  "pilish-send with only whitespace does nothing."
  (let ((send-called nil))
    (pilish-test-with-mock-session "/tmp/pilish-test-send3/"
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pilish-input:/tmp/pilish-test-send3/*"
          (insert "   \n\t  ")
          (pilish-send)
          (should-not send-called))))))

(ert-deftest pilish-test-send-queues-locally-while-streaming ()
  "pilish-send adds to local queue while streaming, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-stream*"))
        (input-buf (get-buffer-create "*pilish-test-queue-stream-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "My message")
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (pilish-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal pilish--followup-queue '("My message"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-queues-locally-while-sending ()
  "pilish-send adds to local queue while waiting for agent_start."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-sending*"))
        (input-buf (get-buffer-create "*pilish-test-queue-sending-input*"))
        (rpc-called nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'sending)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Queued while retry is starting")
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message) #'ignore))
              (pilish-send))
            (should-not rpc-called)
            (with-current-buffer chat-buf
              (should (equal pilish--followup-queue
                             '("Queued while retry is starting"))))
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-refuses-during-session-transition ()
  "A prompt typed while switching sessions is not queued into either session."
  (let ((chat-buf (get-buffer-create "*pilish-test-send-transition*"))
        (input-buf (get-buffer-create "*pilish-test-send-transition-input*"))
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--input-buffer input-buf
                  pilish--followup-queue nil)
            (pilish--begin-session-transition 'mock-proc))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "not yet")
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish-send))
            (should (equal (buffer-string) "not yet")))
          (with-current-buffer chat-buf
            (should (null pilish--followup-queue)))
          (should (equal shown-message
                         "Pi: Cannot send while session is switching")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-refuses-builtin-command-while-busy ()
  "Client-side slash commands are not hidden in the follow-up queue."
  (let ((chat-buf (get-buffer-create "*pilish-test-builtin-busy*"))
        (input-buf (get-buffer-create "*pilish-test-builtin-busy-input*"))
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming
                  pilish--input-buffer input-buf
                  pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "/new")
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish-send))
            (should (equal (buffer-string) "/new")))
          (with-current-buffer chat-buf
            (should (null pilish--followup-queue)))
          (should (equal shown-message "Pi: Cannot queue /new while Pi is busy")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-queues-locally-while-compacting ()
  "pilish-send adds to local queue while compacting, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-compact*"))
        (input-buf (get-buffer-create "*pilish-test-queue-compact-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'compacting)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "My message during compaction")
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (pilish-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal pilish--followup-queue '("My message during compaction"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-slash-compact-handled-locally-not-sent-as-prompt ()
  "/compact in input buffer invokes pilish-compact locally, not sent to pi."
  (let ((chat-buf (get-buffer-create "*pilish-test-slash-compact*"))
        (input-buf (get-buffer-create "*pilish-test-slash-compact-input*"))
        (compact-called nil)
        (compact-args nil)
        (prompt-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "/compact")
            (cl-letf (((symbol-function 'pilish-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args)))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (_) (setq prompt-sent t))))
              (pilish-send))
            ;; Should have called compact function with no args
            (should compact-called)
            (should (null compact-args))
            ;; Should NOT have sent as prompt
            (should-not prompt-sent)
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-slash-compact-with-args-passes-instructions ()
  "/compact with args passes custom instructions to compact function."
  (let ((chat-buf (get-buffer-create "*pilish-test-slash-compact-args*"))
        (input-buf (get-buffer-create "*pilish-test-slash-compact-args-input*"))
        (compact-called nil)
        (compact-args nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "/compact focus on the API design decisions")
            (cl-letf (((symbol-function 'pilish-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args))))
              (pilish-send))
            ;; Should have called compact function with custom instructions
            (should compact-called)
            (should (equal compact-args "focus on the API design decisions"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-manual-compaction-success-preserves-fifo ()
  "Standalone manual compaction releases exactly the oldest follow-up."
  (with-temp-buffer
    (pilish-chat-mode)
    (let (sent-prompts shown-message)
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (setq pilish--status 'sending)
                   (funcall on-success)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish--handle-display-event '(:type "compaction_start" :reason "manual"))
        (dolist (text '("First" "Second" "Third"))
          (pilish--push-followup text))
        (pilish--handle-display-event
         '(:type "compaction_end" :reason "manual" :aborted :false :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (equal shown-message "Pi: Compacted from 1,000 tokens"))
        (should (equal sent-prompts '("First")))
        (should (equal pilish--followup-queue '("Third" "Second")))))))

(ert-deftest pilish-test-overflow-compaction-retry-drains-queue-after-settled ()
  "Overflow recovery and its retry stay ahead of local FIFO until settlement."
  (pilish-test--with-streaming-assistant
    (let (sent-prompts)
      (setq pilish--followup-queue '("queued behind retry"))
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (setq pilish--status 'sending)
                   (funcall on-success)))
                ((symbol-function 'pilish--refresh-header) #'ignore)
                ((symbol-function 'message) #'ignore))
        (pilish--handle-display-event '(:type "agent_end" :messages []))
        (pilish--handle-display-event '(:type "compaction_start" :reason "overflow"))
        (pilish--handle-display-event
         '(:type "compaction_end" :reason "overflow" :aborted :false :willRetry t
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (eq pilish--status 'sending))
        (should-not sent-prompts)
        (should (equal pilish--followup-queue '("queued behind retry")))
        (pilish--handle-display-event '(:type "agent_start"))
        (pilish--handle-display-event '(:type "agent_end" :messages []))
        (should-not sent-prompts)
        (pilish--handle-display-event '(:type "agent_settled"))
        (should (equal sent-prompts '("queued behind retry")))
        (should-not pilish--followup-queue)))))

(ert-deftest pilish-test-preflight-compaction-keeps-followups-behind-prompt ()
  "Compaction during prompt preflight must not drain local follow-ups."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((sent-prompts nil))
      (setq pilish--status 'sending
            pilish--pre-compaction-status nil
            pilish--followup-queue '("queued behind original prompt"))
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                ((symbol-function 'message) #'ignore))
        (pilish--handle-display-event
         '(:type "compaction_start" :reason "threshold"))
        (pilish--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted :false
           :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (eq pilish--status 'sending))
        (should (null sent-prompts))
        (should (equal pilish--followup-queue
                       '("queued behind original prompt")))))))

(ert-deftest pilish-test-failed-preflight-compaction-keeps-followups-until-prompt-result ()
  "Failed compaction leaves prompt and follow-up ownership with preflight."
  (let ((chat-buf (get-buffer-create "*pilish-test-preflight-compaction-failure-chat*"))
        (input-buf (get-buffer-create "*pilish-test-preflight-compaction-failure-input*"))
        (sent-text nil))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (let ((generation (pilish--begin-prompt-start-wait)))
              (setq pilish--status 'sending
                    pilish--input-buffer input-buf
                    pilish--pre-compaction-status nil
                    pilish--followup-queue '("follow-up after prompt"))
              (cl-letf (((symbol-function 'pilish--send-prompt)
                         (lambda (text &optional on-success &rest _)
                           (setq sent-text text)
                           (when on-success (funcall on-success))))
                        ((symbol-function 'message) #'ignore))
                (pilish--handle-display-event
                 '(:type "compaction_start" :reason "threshold"))
                (pilish--handle-display-event
                 '(:type "compaction_end"
                   :reason "threshold"
                   :aborted :false
                   :willRetry :false
                   :result :null
                   :errorMessage "quota exceeded")))
              (should (eq pilish--status 'sending))
              (should (equal pilish--activity-phase "thinking"))
              (should (pilish--prompt-start-current-p generation))
              (should (equal pilish--followup-queue '("follow-up after prompt")))
              (should (null sent-text))))
          (with-current-buffer input-buf
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-extension-cancelled-preflight-compaction-keeps-followups ()
  "Extension cancellation is not a local stop and may continue preflight."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((sent-text nil))
      (pilish--begin-prompt-start-wait)
      (setq pilish--status 'sending
            pilish--pre-compaction-status nil
            pilish--followup-queue '("discarded follow-up"))
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success))))
                ((symbol-function 'message) #'ignore))
        (pilish--handle-display-event
         '(:type "compaction_start" :reason "threshold"))
        (pilish--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted t
           :willRetry :false
           :result nil)))
      (should (eq pilish--status 'sending))
      (should (equal pilish--activity-phase "thinking"))
      (should (equal pilish--followup-queue '("discarded follow-up")))
      (should (null sent-text)))))

(ert-deftest pilish-test-failed-preflight-compaction-prompt-failure-restores-original ()
  "Prompt RPC failure clears preflight wait after compaction failure."
  (let ((chat-buf (get-buffer-create "*pilish-test-preflight-prompt-failure-chat*"))
        (input-buf (get-buffer-create "*pilish-test-preflight-prompt-failure-input*"))
        (rpc-callback nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--activity-phase "idle"
                  pilish--input-buffer input-buf
                  pilish--followup-queue '("queued follow-up")))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "original prompt"))
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _cmd callback)
                       (setq rpc-callback callback)))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer input-buf
              (pilish-send))
            (with-current-buffer chat-buf
              (pilish--handle-display-event
               '(:type "compaction_start" :reason "threshold"))
              (pilish--handle-display-event
               '(:type "compaction_end"
                 :reason "threshold"
                 :aborted :false
                 :willRetry :false
                 :result :null
                 :errorMessage "quota exceeded"))
              (should (pilish--session-busy-p chat-buf))
              (funcall rpc-callback '(:success :false :error "quota exceeded"))
              (should (eq pilish--status 'idle))
              (should (equal pilish--activity-phase "idle"))
              (should-not (pilish--session-busy-p chat-buf))))
          (with-current-buffer input-buf
            (should (equal (buffer-string)
                           "original prompt\n\nqueued follow-up"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-agent-end-will-retry-preserves-followup-queue ()
  "Transient retry does not release FIFO until the retried run settles."
  (pilish-test--with-streaming-assistant
    (let (sent-prompts)
      (setq pilish--followup-queue '("queued behind transient retry"))
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (funcall on-success)))
                ((symbol-function 'pilish--refresh-header) #'ignore))
        (pilish--handle-display-event '(:type "agent_end" :messages [] :willRetry t))
        (pilish--handle-display-event
         '(:type "auto_retry_start" :attempt 1 :maxAttempts 3 :delayMs 1000))
        (should (eq pilish--status 'sending))
        (should-not sent-prompts)
        (should (equal pilish--followup-queue '("queued behind transient retry")))
        (pilish--handle-display-event '(:type "auto_retry_end" :success t))
        (pilish--handle-display-event '(:type "agent_start"))
        (pilish--handle-display-event '(:type "agent_end" :messages []))
        (should-not sent-prompts)
        (pilish--handle-display-event '(:type "agent_settled"))
        (should (equal sent-prompts '("queued behind transient retry")))
        (should-not pilish--followup-queue)))))

(ert-deftest pilish-test-compaction-failure-restores-queued-followups ()
  "Failed compaction surfaces queued follow-ups for user recovery."
  (let ((chat-buf (get-buffer-create "*pilish-test-compaction-failure-chat*"))
        (input-buf (get-buffer-create "*pilish-test-compaction-failure-input*"))
        (sent-text nil))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'compacting
                  pilish--input-buffer input-buf
                  pilish--followup-queue '("recover me later"))
            (cl-letf (((symbol-function 'pilish--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-text text)
                         (when on-success (funcall on-success))))
                      ((symbol-function 'message) #'ignore))
              (pilish--handle-display-event
               '(:type "compaction_end"
                 :reason "threshold"
                 :aborted :false
                 :willRetry :false
                 :result :null
                 :errorMessage "quota exceeded")))
            (should (eq pilish--status 'idle))
            (should (null pilish--followup-queue))
            (should (null sent-text)))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "recover me later"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-auto-retry-failure-restores-queued-followups ()
  "Final auto-retry failure surfaces queued follow-ups for user recovery."
  (let ((chat-buf (get-buffer-create "*pilish-test-retry-failure-chat*"))
        (input-buf (get-buffer-create "*pilish-test-retry-failure-input*")))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'sending
                  pilish--activity-phase "thinking"
                  pilish--input-buffer input-buf
                  pilish--followup-queue '("queued behind failed retry"))
            (pilish--handle-display-event
             '(:type "auto_retry_end"
               :success :false
               :attempt 3
               :finalError "overloaded"))
            (should (eq pilish--status 'sending))
            (should (equal pilish--activity-phase "thinking"))
            (pilish--handle-display-event '(:type "agent_settled"))
            (should (eq pilish--status 'idle))
            (should (equal pilish--activity-phase "idle"))
            (should (null pilish--followup-queue))
            (should (string-match-p "Retry failed" (buffer-string))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "queued behind failed retry"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-cancelled-manual-compaction-keeps-queue-without-input ()
  "Extension cancellation cannot discard text when its editor is unavailable."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((sent-text nil))
      (setq pilish--status 'compacting)
      (setq pilish--followup-queue '("queued message"))
      (cl-letf (((symbol-function 'pilish--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success)))))
        ;; Simulate compaction_end event (aborted)
        (pilish--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted t)))
      ;; No explicit stop occurred, and there is nowhere to restore the text.
      (should (equal pilish--followup-queue '("queued message")))
      ;; No message should have been sent
      (should (null sent-text)))))

;;; Run Settlement

(ert-deftest pilish-test-review-fixes-settled-reentry-keeps-new-run ()
  "A's delayed settlement cannot release observably newer streaming work B.
If B has also ended, A/B settlements have no wire identity; this guard does
not claim to distinguish those otherwise identical sending-state traces."
  (dolist (stop '(nil t))
    (pilish-test-with-rpc-session (chat _input _proc commands)
      (with-current-buffer chat
        (cl-letf (((symbol-function 'message) #'ignore))
          (dolist (event '((:type "agent_start")
                           (:type "agent_end" :messages [])
                           (:type "agent_start")
                           (:type "message_start" :message (:role "assistant"))
                           (:type "message_update"
                            :assistantMessageEvent (:type "text_delta" :delta "B"))))
            (pilish--handle-display-event event))
          (setq pilish--followup-queue '("next"))
          (when stop (pilish-abort))
          (setq commands nil)
          (pilish--handle-display-event '(:type "agent_settled")) ; A
          (should-not commands)
          (should (eq pilish--status 'streaming))
          (should (eq pilish--aborted stop))
          (should (equal pilish--activity-phase "replying"))
          (pilish--handle-display-event '(:type "agent_end" :messages [])) ; B
          (pilish--handle-display-event '(:type "agent_settled"))
          (should-not pilish--aborted)
          (should (= (if stop 0 1) (length commands))))))))

(ert-deftest pilish-test-review-fixes-settled-manual-reentry-keeps-compaction ()
  "A's settlement neither ends extension-started manual B nor resurrects A."
  (pilish-test-with-rpc-session (chat input _proc commands)
    (with-current-buffer chat
      (cl-letf (((symbol-function 'message) #'ignore))
        (pilish--handle-display-event '(:type "agent_start"))
        (pilish--handle-display-event '(:type "agent_end" :messages []))
        (setq pilish--followup-queue '("next"))
        (pilish--handle-display-event '(:type "compaction_start" :reason "manual"))
        (pilish--handle-display-event '(:type "agent_settled"))
        (should (eq pilish--status 'compacting))
        (should (equal pilish--activity-phase "compact"))
        (should-not commands)
        (pilish--handle-display-event
         '(:type "compaction_end" :reason "manual" :aborted :false :willRetry :false
           :result :null :errorMessage "stub auth failure"))
        (should (eq pilish--status 'idle))
        (should-not (pilish--session-busy-p))
        (should (string-match-p "stub auth failure" (buffer-string)))))
    (with-current-buffer input (should (equal (buffer-string) "next")))))

(ert-deftest pilish-test-review-fixes-stop-before-delayed-compaction-start ()
  "Reapply Stop when manual or automatic compaction finally creates a controller."
  (dolist (reason '("manual" "threshold"))
    (pilish-test-with-rpc-session (chat _input _proc commands)
      (with-current-buffer chat
        (cl-letf (((symbol-function 'message) #'ignore))
          (if (equal reason "manual")
              (pilish-compact)
            (pilish--prepare-and-send "prompt preflight"))
          (pilish-abort)
          (should pilish--aborted)
          (setq commands nil)
          (pilish--handle-display-event (list :type "compaction_start" :reason reason))
          (should (equal (mapcar (lambda (cmd) (plist-get cmd :type))
                                (reverse commands))
                         '("clear_queue" "abort")))
          (should pilish--aborted))))))

(ert-deftest pilish-test-adversarial-steering-after-agent-end-stays-local ()
  "Steering after agent_end waits locally, including before a late prompt ack."
  (dolist (late-ack '(nil t))
    (pilish-test-with-rpc-session (chat input proc commands)
      (let (notice)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq notice (apply #'format fmt args)))))
          (with-current-buffer chat
            (pilish--prepare-and-send "/extension-run")
            (let ((request (car commands)))
              (unless late-ack
                (pilish--dispatch-response
                 proc (list :type "response" :id (plist-get request :id)
                            :command "prompt" :success t)))
              (pilish--handle-display-event '(:type "agent_start"))
              (pilish--handle-display-event '(:type "agent_end" :messages []))
              ;; Pi may now be awaiting agent_settled hooks with its backend
              ;; queue drain already finished.  No steer RPC is safe here.
              (with-current-buffer input
                (insert "after the run")
                (pilish-queue-steering)
                (should (string-empty-p (buffer-string))))
              (should (= 1 (length commands)))
              (should (equal notice "Pi: Steering queued (will send when Pi is ready)"))
              (should (equal pilish--followup-queue '("after the run")))
              (pilish--handle-display-event '(:type "agent_settled"))
              (when late-ack
                (should (= 1 (length commands)))
                (pilish--dispatch-response
                 proc (list :type "response" :id (plist-get request :id)
                            :command "prompt" :success t)))
              (should (equal (mapcar (lambda (cmd) (plist-get cmd :type))
                                    (reverse commands))
                             '("prompt" "prompt")))
              (let ((followup (car commands)))
                (should (equal (plist-get followup :message) "after the run"))
                (pilish--dispatch-response
                 proc (list :type "response" :id (plist-get followup :id)
                            :command "prompt" :success t)))
              (should-not pilish--followup-queue))))))))

(defun pilish-test--retry-failure-with-late-ack (success &optional disposition)
  "Check retry exhaustion before a FIFO owner's late SUCCESS and DISPOSITION."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let (notice)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq notice (apply #'format fmt args)))))
        (with-current-buffer input (insert "new draft"))
        (with-current-buffer chat
          (setq pilish--followup-queue '("next" "/extension-run"))
          (pilish--process-followup-queue)
          (let ((request (car commands)))
            (dolist (event '((:type "agent_start")
                             (:type "agent_end" :messages [] :willRetry t)
                             (:type "auto_retry_start" :attempt 1 :maxAttempts 1)
                             (:type "agent_start")
                             (:type "agent_end" :messages [])
                             (:type "auto_retry_end" :success :false :attempt 1
                              :finalError "overloaded")))
              (pilish-test--stdout proc event))
            (should (string-match-p "Retry failed" (buffer-string)))
            (with-current-buffer input
              (should (equal (buffer-string) "new draft")))
            (should (equal pilish--followup-queue '("next" "/extension-run")))
            (pilish-test--stdout proc '(:type "agent_settled"))
            (should (pilish--session-busy-p))
            (should (= 1 (length commands)))
            (let ((response
                   (append (list :type "response" :id (plist-get request :id)
                                 :command "prompt" :success success)
                           (unless (eq success t) (list :error "command rejected"))
                           (when disposition (list :data (list :disposition disposition))))))
              (pilish-test--stdout proc response response))
            (should-not (pilish--session-busy-p))
            (should-not pilish--followup-queue)
            ;; Exhaustion recovers only unsent work; accepting the owner must
            ;; neither restore that command nor automatically run its successor.
            (should (= 1 (length commands)))
            (with-current-buffer input
              (should (equal (buffer-string)
                             (if (eq success t)
                                 "next\n\nnew draft"
                               "/extension-run\n\nnext\n\nnew draft"))))
            (unless (eq success t)
              (should (equal notice "Pi: Send failed: command rejected")))))))))

(ert-deftest pilish-test-adversarial-retry-failure-before-late-success ()
  "Late acceptance removes a submitted FIFO owner before restoring its successor."
  (dolist (disposition '(nil "handled"))
    (pilish-test--retry-failure-with-late-ack t disposition)))

(ert-deftest pilish-test-adversarial-retry-failure-before-late-rejection ()
  "Late rejection restores the unresolved FIFO owner and successor exactly once."
  (pilish-test--retry-failure-with-late-ack :false))

(ert-deftest pilish-test-review-fixes-queued-extension-ack-after-run ()
  "An extension awaiting waitForIdle accepts late, without resending its FIFO item."
  (dolist (disposition '(nil "handled"))
    (pilish-test-with-rpc-session (chat _input proc commands)
      (with-current-buffer chat
        (setq pilish--followup-queue '("next" "/extension-run"))
        (pilish--process-followup-queue)
        (let ((request (car commands)))
          (dolist (event '((:type "agent_start")
                           (:type "agent_end" :messages [])
                           (:type "agent_settled")))
            (pilish-test--stdout proc event))
          (should (= 1 (length commands)))
          (should (pilish--session-busy-p)) ; request still unacknowledged
          (should (equal pilish--followup-queue '("next" "/extension-run")))
          (let ((response
                 (append (list :type "response" :id (plist-get request :id)
                               :command "prompt" :success t)
                         (when disposition (list :data (list :disposition disposition))))))
            (pilish-test--stdout proc response response))
          (should (equal (mapcar (lambda (cmd) (plist-get cmd :message))
                                (reverse commands))
                         '("/extension-run" "next")))
          (should (equal pilish--followup-queue '("next"))))))))

(ert-deftest pilish-test-review-fixes-stop-while-awaiting-late-acceptance ()
  "Stop drops the FIFO once; late acceptance cannot resend it or poison input."
  (dolist (disposition '(nil "handled"))
    (pilish-test-with-rpc-session (chat input proc commands)
      (with-current-buffer chat
        (cl-letf (((symbol-function 'message) #'ignore))
          (setq pilish--followup-queue '("next" "/extension-run"))
          (pilish--process-followup-queue)
          (let ((request (car commands)))
            (dolist (event '((:type "agent_start") (:type "agent_end" :messages [])
                             (:type "agent_settled")))
              (pilish-test--stdout proc event))
            (with-current-buffer input (insert "newer draft"))
            (pilish-abort)
            (should pilish--aborted)
            (let ((response
                   (append (list :type "response" :id (plist-get request :id)
                                 :command "prompt" :success t)
                           (when disposition (list :data (list :disposition disposition))))))
              (pilish-test--stdout proc response response))
            (should-not pilish--aborted)
            (should-not pilish--followup-queue)
            (should-not (pilish--session-busy-p))
            (should (equal (mapcar (lambda (cmd) (plist-get cmd :type))
                                  (reverse commands))
                           '("prompt" "clear_queue" "abort")))
            (with-current-buffer input
              (should (equal (buffer-string) "newer draft")))))))))

(ert-deftest pilish-test-review-fixes-manual-reentry-retains-unacknowledged-fifo ()
  "Extension manual failure cannot restore a FIFO request still awaiting acceptance."
  (pilish-test-with-rpc-session (chat input proc commands)
    (with-current-buffer chat
      (cl-letf (((symbol-function 'message) #'ignore))
        (setq pilish--followup-queue '("next" "/extension-run"))
        (pilish--process-followup-queue)
        (let ((request (car commands)))
          (dolist (event '((:type "agent_start") (:type "agent_end" :messages [])
                           (:type "compaction_start" :reason "manual")
                           (:type "agent_settled")
                           (:type "compaction_end" :reason "manual" :aborted :false
                            :result :null :errorMessage "manual failed")))
            (pilish--handle-display-event event))
          (should (equal pilish--followup-queue '("next" "/extension-run")))
          (with-current-buffer input (should (string-empty-p (buffer-string))))
          (pilish--dispatch-response
           proc (list :type "response" :id (plist-get request :id)
                      :command "prompt" :success t))
          (should (= 2 (length commands)))
          (should (equal (plist-get (car commands) :message) "next")))))))

(ert-deftest pilish-test-review-fixes-late-prompt-ack-does-not-append-user-echo ()
  "Acceptance after an authoritative echo or run never appends a late user turn."
  (dolist (timing '(echo-only streaming settled))
    (pilish-test-with-rpc-session (chat _input proc commands)
      (with-current-buffer chat
        (pilish--prepare-and-send "original")
        (let ((request (car commands)))
          (unless (eq timing 'echo-only)
            (pilish--handle-display-event '(:type "agent_start")))
          (pilish--handle-display-event
           '(:type "message_start"
             :message (:role "user" :content [(:type "text" :text "original")])))
          (when (eq timing 'settled)
            (pilish--handle-display-event '(:type "agent_end" :messages []))
            (pilish--handle-display-event '(:type "agent_settled")))
          (pilish--dispatch-response
           proc (list :type "response" :id (plist-get request :id)
                      :command "prompt" :success t))
          (should (= 1 (pilish-test--count-matches "original" (buffer-string))))
          (should-not pilish--local-user-message)
          (when (eq timing 'echo-only)
            (pilish--handle-display-event '(:type "agent_start"))))))))

(ert-deftest pilish-test-review-fixes-late-rejection-preserves-active-work ()
  "A rejected command restores its text without idling independently started work."
  (pilish-test-with-rpc-session (chat input proc commands)
    (with-current-buffer chat
      (cl-letf (((symbol-function 'message) #'ignore))
        (pilish--prepare-and-send "rejected request")
        (let ((request (car commands)))
          (pilish--handle-display-event '(:type "agent_start"))
          (pilish-abort)
          (pilish--dispatch-response
           proc (list :type "response" :id (plist-get request :id)
                      :command "prompt" :success :false :error "extension failed"))
          (with-current-buffer input
            (should (equal (buffer-string) "rejected request")))
          (should (eq pilish--status 'streaming))
          (should pilish--aborted))))))

(ert-deftest pilish-test-review-fixes-prompt-response-after-process-replacement ()
  "Old acceptance or rejection cannot mutate replacement-process state or drafts."
  (dolist (success '(t :false))
    (pilish-test-with-rpc-session (chat input proc commands)
      (with-current-buffer chat
        (cl-letf (((symbol-function 'message) #'ignore))
          (pilish--prepare-and-send "old request")
          (let ((request (car commands)))
            (pilish--set-process (start-process "replacement" nil "cat"))
            (setq pilish--status 'streaming pilish--aborted t
                  pilish--followup-queue '("replacement work"))
            (with-current-buffer input (insert "new draft"))
            (pilish--dispatch-response
             proc (list :type "response" :id (plist-get request :id)
                        :command "prompt" :success success :error "old failure"))
            (should-not (string-match-p "old request" (buffer-string)))
            (should (eq pilish--status 'streaming))
            (should pilish--aborted)
            (should (equal pilish--followup-queue '("replacement work")))
            (with-current-buffer input
              (should (equal (buffer-string) "new draft")))))))))

(ert-deftest pilish-test-lifecycle-interturn-compaction-keeps-guard-and-abort ()
  "A resumed turn remains guarded and stoppable without another agent_start."
  (pilish-test--with-streaming-assistant
    (let (commands shown-message)
      (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'pilish--refresh-header) #'ignore)
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc command callback)
                   (push (plist-get command :type) commands)
                   (when (equal (plist-get command :type) "clear_queue")
                     (funcall callback '(:success t)))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (dolist (event '((:type "message_end" :message (:role "assistant"))
                         (:type "turn_end")
                         (:type "compaction_start" :reason "threshold")
                         (:type "compaction_end" :aborted :false :willRetry :false
                          :result (:tokensBefore 1000 :summary "Summary"))
                         (:type "turn_start")
                         (:type "message_start" :message (:role "assistant"))))
          (pilish--handle-display-event event))
        ;; No queue or prompt-start wait may accidentally mask an idle status.
        (should-not (pilish--session-transition-ready-p (current-buffer) "reload"))
        (should (string-match-p "Cannot reload" shown-message))
        (should (pilish--session-busy-p))
        (pilish-abort)
        (should (equal (reverse commands) '("clear_queue" "abort")))
        (should pilish--aborted)))))

(ert-deftest pilish-test-lifecycle-interturn-unsuccessful-compaction-keeps-followups ()
  "Compaction failure or extension cancellation neither restores nor drops FIFO."
  (dolist (outcome '((:aborted :false :result :null :errorMessage "quota exceeded")
                     (:aborted t :result :null)))
    (ert-info ((format "Compaction outcome: %S" outcome))
      (pilish-test-with-mock-session "/tmp/pilish-test-lifecycle-compaction-queue/"
        (let ((chat-buf (get-buffer "*pilish-chat:/tmp/pilish-test-lifecycle-compaction-queue/*"))
              (input-buf (get-buffer "*pilish-input:/tmp/pilish-test-lifecycle-compaction-queue/*"))
              sent-prompts shown-message)
          (with-current-buffer input-buf
            (insert "new draft"))
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pilish--send-prompt)
                       (lambda (text &rest _) (push text sent-prompts)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish--handle-display-event '(:type "agent_start"))
              (setq pilish--followup-queue '("second" "first"))
              (pilish--handle-display-event '(:type "turn_end"))
              (pilish--handle-display-event
               '(:type "compaction_start" :reason "threshold"))
              (pilish--handle-display-event
               (append '(:type "compaction_end" :willRetry :false) outcome))
              (should (equal pilish--followup-queue '("second" "first")))
              (should-not sent-prompts)
              (should-not pilish--aborted)
              (should (eq pilish--status 'streaming))
              (should-not (equal pilish--activity-phase "idle"))
              (should (equal shown-message
                             (if (eq (plist-get outcome :aborted) t)
                                 "Pi: Compaction cancelled"
                               "Pi: Compaction failed: quota exceeded")))
              (pilish--handle-display-event '(:type "turn_start"))
              (should (pilish--session-busy-p))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "new draft"))))))))

(ert-deftest pilish-test-lifecycle-delayed-post-run-work-waits-for-settled ()
  "Elapsed time, post-run compaction and extra work cannot release FIFO early."
  (pilish-test--with-streaming-assistant
    (let (sent-prompts prompt-callback notices)
      (setq pilish--followup-queue '("second" "first"))
      (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'pilish--refresh-header) #'ignore)
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc command callback)
                   (when (equal (plist-get command :type) "prompt")
                     (push (plist-get command :message) sent-prompts)
                     (setq prompt-callback callback))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) notices)))))
        (pilish--handle-display-event
         '(:type "agent_end" :messages [] :willRetry :false))
        ;; Deliberately deliver post-run events later than the old 50ms drain.
        (sleep-for 0.1)
        (should-not sent-prompts)
        (should (eq pilish--status 'sending))
        (pilish--handle-display-event
         '(:type "compaction_start" :reason "threshold"))
        (pilish--handle-display-event
         '(:type "compaction_end" :aborted :false :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary")))
        (sleep-for 0.1)
        (should-not sent-prompts)
        (should (eq pilish--status 'sending))
        (should (member "Pi: Compacted from 1,000 tokens" notices))
        ;; Extension-owned work may start before the session finally settles.
        (pilish--handle-display-event '(:type "agent_start"))
        (pilish--handle-display-event '(:type "agent_end" :messages []))
        (sleep-for 0.1)
        (should-not sent-prompts)
        (pilish--handle-display-event '(:type "agent_settled"))
        (should (equal sent-prompts '("first")))
        ;; Sending is not acceptance: the oldest item still owns its text.
        (should (equal pilish--followup-queue '("second" "first")))
        (funcall prompt-callback '(:success t))
        (should (equal pilish--followup-queue '("second")))
        (pilish--handle-display-event '(:type "agent_start"))
        (pilish--handle-display-event '(:type "agent_end" :messages []))
        (should (equal sent-prompts '("first")))
        (pilish--handle-display-event '(:type "agent_settled"))
        (should (equal (reverse sent-prompts) '("first" "second")))
        (funcall prompt-callback '(:success t))
        (should-not pilish--followup-queue)
        (pilish--handle-display-event '(:type "agent_start"))
        (pilish--handle-display-event '(:type "agent_end" :messages []))
        (pilish--handle-display-event '(:type "agent_settled"))
        (should-not (pilish--session-busy-p))
        (should (pilish--session-transition-ready-p (current-buffer) "reload"))
        (should (equal (reverse sent-prompts) '("first" "second")))))))

(ert-deftest pilish-test-lifecycle-idle-header-response-cannot-unlock-active-work ()
  "Header refresh cannot make active work idle when no prompt wait exists."
  (dolist (status '(sending streaming compacting))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--status status)
      (pilish--apply-state-response
       (current-buffer)
       '(:success t :data (:isStreaming :false :isCompacting :false
                          :thinkingLevel "high")))
      (should (equal (plist-get pilish--state :thinking-level) "high"))
      (should (eq pilish--status status))
      (should (pilish--session-busy-p)))))

;;; Abort Command

(ert-deftest pilish-test-inactivity-abort-acknowledgment-is-output-not-settlement ()
  "Only actual stdout clears silence; Stop and its replies never force idle."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (pilish-test--stdout proc '(:type "agent_start"))
    (with-current-buffer chat (setq pilish--followup-queue '("discard me")))
    (setq now 1300.0)
    (pilish-test--assert-inactivity input "thinking (no output 5m)")
    (should-not commands)
    (let (notices)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) notices))))
        (with-current-buffer input (pilish-abort)))
      (should (equal notices '("Pi: Aborting..."))))
    (should (equal (mapcar (lambda (cmd) (plist-get cmd :type)) (reverse commands))
                   '("clear_queue" "abort")))
    (should-not (buffer-local-value 'pilish--followup-queue chat))
    (should (buffer-local-value 'pilish--aborted chat))
    (pilish-test--assert-inactivity input "thinking (no output 5m)")
    (dolist (command (reverse commands))
      (pilish-test--stdout
       proc `(:type "response" :id ,(plist-get command :id) :success t)))
    (pilish-test--assert-inactivity input nil)
    (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
    (should (buffer-local-value 'pilish--aborted chat))
    (setq now 1600.0)
    (pilish-test--assert-inactivity input "thinking (no output 5m)")))

(ert-deftest pilish-test-lifecycle-abort-clears-backend-before-stopping ()
  "Stop clears backend steering and local FIFO in every busy phase."
  (dolist (status '(streaming sending compacting))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--status status
            pilish--followup-queue '("must not continue"))
      (let (commands shown-message)
        (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_proc command callback)
                     (push (plist-get command :type) commands)
                     (when (equal (plist-get command :type) "clear_queue")
                       (funcall callback '(:success t)))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (pilish-abort)
          (should (equal shown-message "Pi: Aborting..."))
          (should (equal (reverse commands) '("clear_queue" "abort")))
          (should-not pilish--followup-queue)
          (should pilish--aborted))))))

(ert-deftest pilish-test-lifecycle-abort-preflight-stops-eventual-agent-start ()
  "Stop during preflight compaction survives a later accepted prompt and run."
  (pilish-test-with-mock-session "/tmp/pilish-test-lifecycle-abort-preflight/"
    (let ((chat-buf (get-buffer "*pilish-chat:/tmp/pilish-test-lifecycle-abort-preflight/*"))
          (input-buf (get-buffer "*pilish-input:/tmp/pilish-test-lifecycle-abort-preflight/*"))
          commands prompts prompt-callback shown-message)
      (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'pilish--refresh-header) #'ignore)
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc command callback)
                   (push (plist-get command :type) commands)
                   (pcase (plist-get command :type)
                     ("prompt"
                      (push (plist-get command :message) prompts)
                      (setq prompt-callback callback))
                     ("clear_queue" (funcall callback '(:success t))))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (setq shown-message (apply #'format fmt args))))))
        (with-current-buffer input-buf
          (insert "original prompt")
          (pilish-send)
          (insert "unwanted follow-up")
          (pilish-send))
        (with-current-buffer chat-buf
          (pilish--handle-display-event
           '(:type "compaction_start" :reason "threshold"))
          (pilish-abort)
          (pilish--handle-display-event
           '(:type "compaction_end" :aborted t :willRetry :false :result :null))
          (should (equal shown-message "Pi: Compaction cancelled"))
          (funcall prompt-callback '(:success t))
          (setq commands nil)
          (pilish--handle-display-event '(:type "agent_start"))
          (should (member "abort" commands))
          (should pilish--aborted)
          (should-not pilish--followup-queue)
          (pilish--handle-display-event '(:type "agent_end" :messages []))
          (pilish--handle-display-event '(:type "agent_settled"))
          (should-not (pilish--session-busy-p))
          (should (equal prompts '("original prompt")))
          (should-not (string-match-p "unwanted follow-up" (buffer-string))))))))

(ert-deftest pilish-test-lifecycle-late-abort-acknowledgments-do-not-touch-new-work ()
  "Old clear_queue and abort acknowledgments cannot stop or release a new run."
  (pilish-test--with-streaming-assistant
    (let (callbacks)
      (setq pilish--process 'mock-proc)
      (cl-letf (((symbol-function 'pilish--rpc-async)
                 (lambda (_proc _command callback) (push callback callbacks)))
                ((symbol-function 'pilish--refresh-header) #'ignore)
                ((symbol-function 'message) #'ignore))
        (pilish-abort)
        (let ((old-callbacks callbacks))
          (pilish--handle-display-event '(:type "agent_end" :messages []))
          (should pilish--aborted)
          (pilish--handle-display-event '(:type "agent_settled"))
          (pilish--handle-display-event '(:type "agent_start"))
          (setq pilish--followup-queue '("new work"))
          (dolist (callback old-callbacks)
            (funcall callback '(:success t)))
          (should (eq pilish--status 'streaming))
          (should-not pilish--aborted)
          (should (equal pilish--followup-queue '("new work")))
          (should (equal pilish--activity-phase "thinking")))))))

(ert-deftest pilish-test-lifecycle-abort-preflight-rejection-or-no-turn-releases-stop ()
  "A stopped prompt that never starts releases intent at its owned completion."
  (dolist (accepted '(nil t))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--process 'mock-proc)
      (let (prompt-callback restored commands)
        (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_proc command callback)
                     (push (plist-get command :type) commands)
                     (pcase (plist-get command :type)
                       ("prompt" (setq prompt-callback callback))
                       ("get_state"
                        (funcall callback
                                 '(:success t :data (:isStreaming :false
                                                    :isCompacting :false)))))))
                  ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
                  ((symbol-function 'pilish--refresh-header) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (pilish--send-prompt "original" nil (lambda () (setq restored t)))
          (pilish-abort)
          (should pilish--aborted)
          (funcall prompt-callback (list :success (if accepted t :false)))
          (when accepted
            (pilish--clear-sending-if-no-agent-start
             (current-buffer) pilish--prompt-wait))
          (should (eq restored (not accepted)))
          (should-not (pilish--session-busy-p))
          (should-not pilish--aborted)
          (pilish--send-prompt "new prompt")
          (pilish--handle-display-event '(:type "agent_start"))
          (should (= 1 (cl-count "abort" commands :test #'equal)))
          (should (eq pilish--status 'streaming)))))))

(ert-deftest pilish-test-abort-noop-when-idle ()
  "pilish-abort does nothing when idle."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((sent-command nil)
          (pilish--status 'idle))
      (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pilish--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (pilish-abort)
        (should (null sent-command))))))

(ert-deftest pilish-test-abort-clears-followup-queue ()
  "Aborting clears the follow-up queue so queued messages are not sent.
When user aborts, they want to stop everything - including queued messages."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t)
          (message-was-sent nil))
      (insert "Some streaming content")
      ;; Set up state as if we're streaming with a queued message
      (setq pilish--aborted t
            pilish--followup-queue '("queued message that should be discarded"))
      ;; Mock send functions to detect if queue processing sends the message
      (cl-letf (((symbol-function 'pilish--prepare-and-send)
                 (lambda (_text) (setq message-was-sent t)))
                ((symbol-function 'pilish--refresh-header) #'ignore))
        ;; Simulate agent_end arriving after abort
        (pilish--display-agent-end)
        ;; Queue should be empty (either cleared or not processed)
        (should (null pilish--followup-queue))
        ;; Key assertion: queued message should NOT have been sent
        (should-not message-was-sent)))))

;;; Kill Buffer Protection

(ert-deftest pilish-test-handler-removed-on-kill ()
  "Event handler is removed when chat buffer is killed."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((fake-proc (start-process "test" nil "true")))
      (unwind-protect
          (progn
            (setq pilish--process fake-proc)
            (pilish--register-display-handler fake-proc)
            (should (process-get fake-proc 'pilish-display-handler))
            (should (process-get fake-proc 'pilish-exit-handler))
            (pilish--cleanup-on-kill)
            (should-not (process-get fake-proc 'pilish-display-handler))
            (should-not (process-get fake-proc 'pilish-exit-handler)))
        (when (process-live-p fake-proc)
          (delete-process fake-proc))))))

;;; Message Queuing

(ert-deftest pilish-test-disposition-steer-feedback ()
  "Steering feedback follows its correlated response, not local scheduling."
  (dolist (case '((queued t (:disposition "queued") "Pi: Steering acknowledged as queued")
                  (handled t (:disposition "handled") "Pi: Steering handled by extension")
                  (missing-data t nil "Pi: Steering message sent")
                  (failure :false nil "Pi: Steering failed: rejected")))
    (ert-info ((format "steering response: %s" (car case)))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let (notices)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices))))
            (pilish-test--stdout proc '(:type "agent_start"))
            (with-current-buffer input
              (insert "steer this run")
              (pilish-queue-steering)
              (should (string-empty-p (buffer-string)))
              (should (= 1 (ring-length pilish--input-ring)))
              (should (equal (ring-ref pilish--input-ring 0) "steer this run"))
              (insert "newer draft"))
            (should-not notices)
            (should (= 1 (length commands)))
            (let* ((request (car commands))
                   (response (append
                              (list :type "response" :command "steer"
                                    :id (plist-get request :id) :success (nth 1 case))
                              (when (nth 2 case) (list :data (nth 2 case)))
                              (when (eq (car case) 'failure) '(:error "rejected")))))
              (should (equal (plist-get request :type) "steer"))
              (should (equal (plist-get request :message) "steer this run"))
              (pilish-test--stdout proc response response))
            (should (equal notices (list (nth 3 case))))
            (with-current-buffer chat
              (should (eq pilish--status 'streaming))
              (should (equal pilish--activity-phase "thinking"))
              (should-not pilish--followup-queue)
              (should-not pilish--local-user-message)
              (should-not (string-match-p "steer this run" (buffer-string))))
            (with-current-buffer input
              (should (equal (buffer-string) "newer draft"))
              (should (= 1 (ring-length pilish--input-ring))))
            (should-not (pilish--process-queue-snapshot proc))
            (pilish-test--queued-header input 0)))))))

(ert-deftest pilish-test-disposition-steer-replacement-suppresses-old-feedback ()
  "An old process's correlated success or failure cannot report on its replacement."
  (dolist (success '(t :false))
    (pilish-test-with-rpc-session (chat input proc commands)
      (let ((replacement (start-process "pilish-new-steer-owner" nil "cat")) notices)
        (unwind-protect
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) notices))))
              (set-process-query-on-exit-flag replacement nil)
              (pilish-test--stdout proc '(:type "agent_start"))
              (with-current-buffer input (insert "old steering") (pilish-queue-steering))
              (let ((request (car commands)))
                (setq notices nil)
                (with-current-buffer chat
                  (pilish--set-process replacement)
                  (pilish--push-followup "new successor"))
                (with-current-buffer input (insert "newer draft"))
                (pilish-test--stdout
                 proc (list :type "response" :command "steer" :success success
                            :id (plist-get request :id) :error "rejected"
                            :data '(:disposition "queued")))
                (should-not notices)
                (with-current-buffer chat
                  (should (eq (pilish--get-process) replacement))
                  (should (eq pilish--status 'streaming))
                  (should (equal pilish--followup-queue '("new successor"))))
                (with-current-buffer input (should (equal (buffer-string) "newer draft")))
                (pilish-test--queued-header input 1)))
          (when (process-live-p replacement) (delete-process replacement)))))))

(ert-deftest pilish-test-queue-steering-when-streaming-sends-steer ()
  "Queue steering sends steer RPC command when agent is streaming."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-steer*"))
        (input-buf (get-buffer-create "*pilish-test-queue-steer-input*"))
        (sent-command nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Please stop and focus on X")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc cmd _cb) (setq sent-command cmd))))
              (pilish-queue-steering))
            ;; Should send steer command
            (should sent-command)
            (should (equal (plist-get sent-command :type) "steer"))
            (should (equal (plist-get sent-command :message) "Please stop and focus on X"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-steering-when-sending-sends-steer ()
  "Steer an unstarted prompt or an announced retry, not a bare sending status."
  (dolist (phase '(preflight accepted retry))
    (pilish-test-with-rpc-session (chat input proc commands)
      (let (notice)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq notice (apply #'format fmt args)))))
          (with-current-buffer chat
            (setq pilish--state (list :status 'idle))
            (pilish--prepare-and-send "original")
            (unless (eq phase 'preflight)
              (pilish--dispatch-response
               proc (list :type "response" :id (plist-get (car commands) :id)
                          :command "prompt" :success t)))
            (when (eq phase 'retry)
              (pilish--handle-display-event '(:type "agent_start"))
              (pilish--handle-display-event '(:type "agent_end" :messages []))
              (pilish--handle-display-event
               '(:type "auto_retry_start" :attempt 1 :maxAttempts 3))))
          (with-current-buffer input
            (insert "Steer the upcoming run")
            (pilish-queue-steering)
            (should (string-empty-p (buffer-string))))
          (should (= 2 (length commands)))
          (should (equal (plist-get (car commands) :type) "steer"))
          (should (equal (plist-get (car commands) :message) "Steer the upcoming run"))
          (should-not notice)
          (pilish-test--stdout
           proc (list :type "response" :command "steer" :success t
                      :id (plist-get (car commands) :id)))
          (should (equal notice "Pi: Steering message sent")))))))

(defun pilish-test--retry-steering-after-state-refresh (path)
  "Check retry steering across correlated state refreshes through PATH."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let (notice)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq notice (apply #'format fmt args)))))
        (with-current-buffer chat
          (setq pilish--state (list :thinking-level "low"))
          (dolist (event '((:type "agent_start")
                           (:type "agent_end" :messages [] :willRetry t)
                           (:type "auto_retry_start" :attempt 1 :maxAttempts 3)))
            (pilish--handle-display-event event))
          (should (pilish--session-steerable-p))
          (dolist (retrying '(t nil))
            ;; Reuse the actual core response callback or the thinking-level
            ;; refresh's UI callback, with production request correlation.
            (if (eq path 'core)
                (pilish--rpc-async proc '(:type "get_state")
                                   #'pilish--update-state-from-response)
              (pilish--refresh-thinking-level-state proc chat))
            (let ((request (car commands)))
              (unless retrying
                ;; Events may finish the retry after a refresh is requested.
                ;; Its delayed snapshot must not resurrect that retry either.
                (pilish--handle-display-event '(:type "agent_start"))
                (pilish--handle-display-event '(:type "agent_end" :messages [])))
              (pilish--dispatch-response
               proc (list :type "response" :id (plist-get request :id)
                          :command "get_state" :success t
                          :data '(:isStreaming t :isCompacting :false
                                  :thinkingLevel "high"))))
            (should (equal (plist-get pilish--state :thinking-level) "high"))
            (should (eq pilish--status 'sending))
            (setq notice nil)
            (with-current-buffer input
              (insert (if retrying "steer the retry" "after the run"))
              (pilish-queue-steering)
              (should (string-empty-p (buffer-string))))
            (if retrying
                (progn
                  (should (equal (plist-get (car commands) :type) "steer"))
                  (should (equal (plist-get (car commands) :message) "steer the retry"))
                  (should-not notice)
                  (pilish-test--stdout
                   proc (list :type "response" :command "steer" :success t
                              :id (plist-get (car commands) :id)))
                  (should (equal notice "Pi: Steering message sent"))
                  (should (= 2 (length commands)))
                  (should-not pilish--followup-queue))
              (should (= 3 (length commands)))
              (should (equal pilish--followup-queue '("after the run")))
              (should (equal notice "Pi: Steering queued (will send when Pi is ready)")))
            (should (eq (plist-get pilish--state :is-retrying) retrying))))))))

(ert-deftest pilish-test-retry-steering-survives-core-state-refresh ()
  "A core get_state response cannot erase event-owned retry steering."
  (pilish-test--retry-steering-after-state-refresh 'core))

(ert-deftest pilish-test-retry-steering-survives-thinking-state-refresh ()
  "A thinking-level get_state callback cannot erase event-owned retry steering."
  (pilish-test--retry-steering-after-state-refresh 'ui))

(ert-deftest pilish-test-queue-steering-refuses-during-session-transition ()
  "Steering must not bypass the session-transition send guard."
  (let ((chat-buf (get-buffer-create "*pilish-test-steer-transition*"))
        (input-buf (get-buffer-create "*pilish-test-steer-transition-input*"))
        (steer-sent nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming
                  pilish--input-buffer input-buf
                  pilish--followup-queue nil)
            (pilish--begin-session-transition 'mock-proc))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (pilish--set-chat-buffer chat-buf)
            (insert "do not steer during switch")
            (cl-letf (((symbol-function 'pilish--send-steer-message)
                       (lambda (_text)
                         (setq steer-sent t)
                         t))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish-queue-steering))
            (should-not steer-sent)
            (should (equal (buffer-string) "do not steer during switch")))
          (with-current-buffer chat-buf
            (should (null pilish--followup-queue)))
          (should (equal shown-message
                         "Pi: Cannot send steering while session is switching")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-steering-send-failure-preserves-input ()
  "Steering send failures keep input text for retry and avoid success feedback."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-steer-fail*"))
        (input-buf (get-buffer-create "*pilish-test-queue-steer-fail-input*"))
        (success-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Retry this steer")
            (cl-letf (((symbol-function 'pilish--send-steer-message)
                       (lambda (_text) nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "steering message sent" (downcase fmt)))
                           (setq success-message t)))))
              (pilish-queue-steering))
            ;; Failed send should keep input so user can retry.
            (should (equal (buffer-string) "Retry this steer"))
            ;; Should not claim success.
            (should-not success-message)
            ;; Failed send should not enqueue a normal follow-up.
            (with-current-buffer chat-buf
              (should (null pilish--followup-queue)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-steering-while-compacting-queues-locally ()
  "Queue steering during compaction should queue locally instead of sending steer now."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-steer-compacting*"))
        (input-buf (get-buffer-create "*pilish-test-queue-steer-compacting-input*"))
        (steer-sent nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'compacting)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Steer during compaction")
            (cl-letf (((symbol-function 'pilish--send-steer-message)
                       (lambda (_text)
                         (setq steer-sent t)
                         t))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish-queue-steering))
            ;; Should NOT send steer immediately
            (should-not steer-sent)
            ;; Should queue in local follow-up queue
            (with-current-buffer chat-buf
              (should (equal pilish--followup-queue '("Steer during compaction"))))
            ;; Should tell user it was queued without over-promising timing.
            (should (equal shown-message "Pi: Steering queued (will send when Pi is ready)"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-steering-and-send-during-compaction-preserve-fifo ()
  "Steering and normal sends queued during compaction keep FIFO order."
  (let ((chat-buf (get-buffer-create "*pilish-test-compaction-fifo*"))
        (input-buf (get-buffer-create "*pilish-test-compaction-fifo-input*"))
        (sent-prompts nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'compacting)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (cl-letf (((symbol-function 'message) #'ignore))
              (insert "Steering first")
              (pilish-queue-steering)
              (insert "Normal send second")
              (pilish-send)))
          (with-current-buffer chat-buf
            (should (equal pilish--followup-queue
                           '("Normal send second" "Steering first")))
            (cl-letf (((symbol-function 'pilish--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (push text sent-prompts)
                         (setq pilish--status 'sending)
                         (funcall on-success)))
                      ((symbol-function 'pilish--refresh-header) #'ignore))
              (pilish--handle-display-event
               '(:type "compaction_end" :reason "manual" :aborted :false
                 :willRetry :false :result (:tokensBefore 1000 :summary "Summary")))
              (should (equal sent-prompts '("Steering first")))
              (pilish--handle-display-event '(:type "agent_start"))
              (pilish--handle-display-event '(:type "agent_end" :messages []))
              (should (equal sent-prompts '("Steering first")))
              (pilish--handle-display-event '(:type "agent_settled")))
            (should (equal (reverse sent-prompts)
                           '("Steering first" "Normal send second")))
            (should (null pilish--followup-queue))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queued-builtin-command-restores-input-without-running ()
  "Stale queued built-in commands are surfaced instead of run automatically."
  (let ((chat-buf (get-buffer-create "*pilish-test-queued-builtin-chat*"))
        (input-buf (get-buffer-create "*pilish-test-queued-builtin-input*"))
        (new-session-called nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--input-buffer input-buf
                  pilish--followup-queue '("/new")))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pilish-new-session)
                       (lambda () (setq new-session-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (pilish--process-followup-queue))
            (should-not new-session-called)
            (should (null pilish--followup-queue)))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "/new")))
          (should (equal shown-message
                         "Pi: Cannot run queued /new command automatically")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-followup-uses-local-queue ()
  "Queue follow-up adds to local queue, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-followup*"))
        (input-buf (get-buffer-create "*pilish-test-queue-followup-input*"))
        (rpc-called nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "After you're done, also do Y")
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t))))
              (pilish-queue-followup))
            ;; Should NOT call RPC
            (should-not rpc-called)
            ;; Should add to local queue
            (with-current-buffer chat-buf
              (should (member "After you're done, also do Y" pilish--followup-queue)))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-steering-when-idle-refuses ()
  "Queue steering refuses when agent is idle (nothing to interrupt)."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-steer-idle*"))
        (input-buf (get-buffer-create "*pilish-test-queue-steer-idle-input*"))
        (sent-anything nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Do something")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (_) (setq sent-anything t)))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd _cb) (setq sent-anything t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "nothing\\|idle\\|C-c C-c" (downcase fmt)))
                           (setq message-shown t)))))
              (pilish-queue-steering))
            ;; Should NOT send anything
            (should-not sent-anything)
            ;; Should show message about using C-c C-c instead
            (should message-shown)
            ;; Input should be preserved (not accepted)
            (should (equal (buffer-string) "Do something"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-followup-when-idle-sends-prompt ()
  "Queue follow-up sends as normal prompt when agent is idle."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-followup-idle*"))
        (input-buf (get-buffer-create "*pilish-test-queue-followup-idle-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Do something else")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success)))))
              (pilish-queue-followup))
            ;; Should send as normal prompt
            (should (equal sent-prompt "Do something else"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-steering-adds-to-history ()
  "Queue steering adds input to history."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-hist*"))
        (input-buf (get-buffer-create "*pilish-test-queue-hist-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (setq pilish--input-ring (make-ring 10))
            (insert "History test message")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--rpc-async) #'ignore))
              (pilish-queue-steering))
            ;; Should be in history
            (should (not (ring-empty-p pilish--input-ring)))
            (should (equal (ring-ref pilish--input-ring 0) "History test message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-empty-input-does-nothing ()
  "Queue with empty input does nothing."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-empty*"))
        (input-buf (get-buffer-create "*pilish-test-queue-empty-input*"))
        (command-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            ;; Empty input (just whitespace)
            (insert "   \n  ")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd _cb) (setq command-sent t))))
              (pilish-queue-steering))
            ;; Should not send anything
            (should-not command-sent)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-steering-shows-minibuffer-message ()
  "A missing-data steering acknowledgment keeps feedback without local display."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let (notice)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq notice (apply #'format fmt args)))))
        (pilish-test--stdout proc '(:type "agent_start"))
        (with-current-buffer input (insert "My steering message") (pilish-queue-steering))
        (should-not notice)
        (pilish-test--stdout
         proc (list :type "response" :command "steer" :success t
                    :id (plist-get (car commands) :id)))
        (should (equal notice "Pi: Steering message sent"))
        (with-current-buffer chat
          (should-not (string-match-p "My steering message" (buffer-string))))))))

(ert-deftest pilish-test-input-mode-has-queue-keybindings ()
  "Input mode has C-c C-s for steering (C-c C-c handles follow-up)."
  (with-temp-buffer
    (pilish-input-mode)
    (should (eq (key-binding (kbd "C-c C-s")) 'pilish-queue-steering))
    ;; C-c C-c handles follow-up when streaming (no separate C-c C-q)
    (should (eq (key-binding (kbd "C-c C-c")) 'pilish-send))))

(ert-deftest pilish-test-queue-handles-rpc-error ()
  "Queue handles RPC error response by showing message to user."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-error*"))
        (input-buf (get-buffer-create "*pilish-test-queue-error-input*"))
        (captured-callback nil)
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd cb) (setq captured-callback cb)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "error\\|fail" (downcase fmt)))
                           (setq error-shown t)))))
              (pilish-queue-steering)
              ;; Simulate error response from RPC
              (when captured-callback
                (funcall captured-callback '(:success :false :error "Queue limit reached")))))
          ;; Should have shown an error message
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queue-with-dead-process-shows-error ()
  "Queue with dead process shows error message."
  (let ((chat-buf (get-buffer-create "*pilish-test-queue-dead*"))
        (input-buf (get-buffer-create "*pilish-test-queue-dead-input*"))
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "process\\|unavailable\\|error" (downcase fmt)))
                           (setq error-shown t)))))
              (pilish-queue-steering)))
          ;; Should have shown an error about unavailable process
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-when-idle-sends-literal-commands ()
  "C-c C-c when idle sends commands literally (pi expands)."
  (let ((chat-buf (get-buffer-create "*pilish-test-send-slash*"))
        (input-buf (get-buffer-create "*pilish-test-send-slash-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "/greet world")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success)))))
              (pilish-send))
            ;; Should send literal command (pi handles expansion)
            (should (equal sent-prompt "/greet world"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

;; Note: pilish-test-send-queues-locally-while-streaming covers this case

(ert-deftest pilish-test-steering-when-idle-refuses ()
  "C-c C-s when idle shows message and does nothing."
  (let ((chat-buf (get-buffer-create "*pilish-test-steer-idle*"))
        (input-buf (get-buffer-create "*pilish-test-steer-idle-input*"))
        (send-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Steer message")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (_) (setq send-called t)))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (_proc _cmd _cb) (setq send-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "idle\\|nothing\\|use" (downcase fmt)))
                           (setq message-shown t)))))
              (pilish-queue-steering))
            ;; Should NOT have sent anything
            (should-not send-called)
            ;; Should have shown a message
            (should message-shown)
            ;; Input should be preserved
            (should (equal (buffer-string) "Steer message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-message-start-user-echo-ignored-when-displayed-locally ()
  "message_start role=user is ignored when we already displayed the same message locally.
Uses local-user-message to track what we displayed for comparison."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'streaming)
          (pilish--state nil)
          ;; Simulate that we displayed this message locally (normal send)
          (pilish--local-user-message "Same message")
          (initial-content (buffer-string)))
      ;; Simulate receiving message_start for a user message (pi echoing back same text)
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Same message")]
                   :timestamp 1704067200000)))
      ;; Buffer should be unchanged - pi's echo matches local display, so skip
      (should (equal (buffer-string) initial-content))
      (should-not (string-match-p "Same message" (buffer-string)))
      ;; Variable should be cleared
      (should-not pilish--local-user-message))))

(ert-deftest pilish-test-message-start-user-displayed-when-different ()
  "message_start role=user IS displayed when pi's text differs from local.
+This handles slash command expansion: user types '/greet', pi sends 'Hello!'."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'streaming)
          (pilish--state nil)
          (pilish--local-user-message "/greet world")
          (initial-content (buffer-string)))
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Hello world!")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since text differs (expanded template)
      (should (string-match-p "Hello world!" (buffer-string)))
      (should-not pilish--local-user-message))))

(ert-deftest pilish-test-message-start-user-skipped-when-template-equals-command ()
  "Edge case: if template expands to exactly the command text, we skip display.
This is rare but possible - the local display is already correct."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'streaming)
          (pilish--state nil)
          (pilish--local-user-message "/echo hello")
          (initial-content (buffer-string)))
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "/echo hello")]
                   :timestamp 1704067200000)))
      ;; Should NOT be displayed - text matches what we displayed locally
      (should (equal (buffer-string) initial-content))
      (should-not pilish--local-user-message))))

(ert-deftest pilish-test-message-start-user-displayed-when-not-local ()
  "message_start role=user IS displayed when local-user-message is nil (steering case).
Steering messages are not displayed locally - they're displayed from the echo."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'streaming)
          (pilish--state nil)
          ;; Variable is nil - no locally displayed message pending
          (pilish--local-user-message nil))
      ;; Simulate receiving message_start for a steering message
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Steering message here")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since local-user-message was nil
      (should (string-match-p "Steering message here" (buffer-string)))
      ;; Variable should still be nil
      (should-not pilish--local-user-message))))

(ert-deftest pilish-test-steering-display-not-interleaved ()
  "Steering message during streaming appears cleanly, not interleaved.
When user sends steering while assistant is streaming, the sequence is:
1. Current assistant output ends cleanly
2. User steering message with header appears
3. New assistant turn begins with its own header

This tests for a bug where user message header and assistant text got
mixed together like:
  > ...count from 1 to
  You · 01:32
  ===========
  STOP NOW
  10 slowly...  <- WRONG: '10 slowly' is assistant text after user msg!"
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'streaming)
          (pilish--state nil)
          (pilish--local-user-message nil)
          (pilish--assistant-header-shown nil))
      ;; Simulate initial prompt response - assistant starts streaming
      (pilish--handle-display-event '(:type "agent_start"))
      (pilish--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      ;; Stream some content
      (pilish--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "Counting: 1, 2, 3, ")))
      (pilish--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "4, 5, 6, ")))

      ;; Now user sends steering - this comes as message_start with role=user
      ;; (steering messages are displayed from pi's echo, not locally)
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "STOP-MARKER")]
                   :timestamp 1704067200000)))

      ;; Assistant continues with new turn after steering
      (setq pilish--assistant-header-shown nil)  ; Reset for new turn
      (pilish--handle-display-event '(:type "agent_start"))
      (pilish--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      (pilish--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "OK, stopping.")))
      (pilish--handle-display-event '(:type "agent_end"))

      ;; Now verify the buffer structure
      (let ((content (buffer-string)))
        ;; All expected content should be present
        (should (string-match-p "Counting: 1, 2, 3, 4, 5, 6," content))
        (should (string-match-p "STOP-MARKER" content))
        (should (string-match-p "OK, stopping" content))

        ;; Find positions to verify order
        (let ((first-assistant-pos (string-match "Counting:" content))
              (steering-pos (string-match "STOP-MARKER" content))
              (second-response-pos (string-match "OK, stopping" content)))
          ;; Order must be: first-assistant < steering < second-response
          (should (< first-assistant-pos steering-pos))
          (should (< steering-pos second-response-pos))

          ;; "You" header must appear before the steering message
          (let ((you-header-pos (string-match "You" content)))
            (should you-header-pos)
            (should (< you-header-pos steering-pos)))

          ;; After STOP-MARKER, we should see "Assistant" header before second response
          (let* ((after-steering (substring content steering-pos))
                 (assistant-after-steering (string-match "Assistant" after-steering)))
            (should assistant-after-steering)))

        ;; Verify NO interleaving: counting text should NOT appear after STOP-MARKER
        (let* ((steering-pos (string-match "STOP-MARKER" content))
               (after-steering (substring content (+ steering-pos (length "STOP-MARKER")))))
          ;; Should NOT see counting continuation after the steering message
          (should-not (string-match-p "^[0-9]" (string-trim-left after-steering)))
          (should-not (string-match-p "^, [0-9]" (string-trim-left after-steering))))))))

(ert-deftest pilish-test-local-user-message-tracks-display ()
  "The local-user-message variable tracks locally displayed messages.
- Normal send stores the text
- message_start role=user clears it to nil
- Steering doesn't set it (displayed from echo)
- agent_end clears it to nil"
  (let ((chat-buf (get-buffer-create "*pilish-test-echo-flag*"))
        (input-buf (get-buffer-create "*pilish-test-echo-flag-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf)
            ;; Variable starts as nil
            (should-not pilish--local-user-message))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "First message")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (_text &optional on-success &rest _)
                         (when on-success
                           (funcall on-success)))))
              (pilish-send)))
          ;; After accepted normal send, variable should store the message text
          (with-current-buffer chat-buf
            (should (equal pilish--local-user-message "First message"))
            ;; Simulate pi echo - variable clears to nil
            (pilish--handle-display-event
             '(:type "message_start"
               :message (:role "user" :content [(:type "text" :text "First message")])))
            (should-not pilish--local-user-message)
            ;; Now simulate steering (doesn't set it)
            (setq pilish--status 'streaming))
          (with-current-buffer input-buf
            (erase-buffer)
            (insert "Steer this")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--rpc-async) #'ignore))
              (pilish-queue-steering)))
          ;; Variable still nil (steering doesn't set it)
          (with-current-buffer chat-buf
            (should-not pilish--local-user-message)
            ;; agent_end clears to nil (in case of edge cases)
            (setq pilish--local-user-message "test")  ; Simulate weird state
            (pilish--display-agent-end)
            (should-not pilish--local-user-message)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-normal-send-not-duplicated-by-message-start ()
  "Accepted normal sends should not be duplicated when message_start arrives.
When prompt preflight succeeds, we display the user text locally.  When pi
echoes it back via message_start, we should NOT display it again."
  (let ((chat-buf (get-buffer-create "*pilish-test-no-dup*"))
        (input-buf (get-buffer-create "*pilish-test-no-dup-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle)
            (setq pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Hello pi")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (_text &optional on-success &rest _)
                         (when on-success
                           (funcall on-success)))))
              (pilish-send)))
          ;; Now simulate pi echoing the message back via message_start
          (with-current-buffer chat-buf
            (pilish--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Hello pi")]
                         :timestamp 1704067200000)))
            ;; Count occurrences of "Hello pi" - should be exactly 1
            (let ((count 0)
                  (start 0))
              (while (string-match "Hello pi" (buffer-string) start)
                (setq count (1+ count))
                (setq start (match-end 0)))
              (should (= count 1)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-agent-settled-sends-queued-followup ()
  "A locally queued prompt is sent and displayed only after agent_settled."
  (let ((chat-buf (get-buffer-create "*pilish-test-agent-end-queue*"))
        (input-buf (get-buffer-create "*pilish-test-agent-end-queue-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil)
            ;; Simulate some prior content
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nSome response...\n")))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "My follow-up question")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t)))
              (pilish-send)))  ; Adds to local queue when streaming
          ;; Message should be in queue, not in chat yet
          (with-current-buffer chat-buf
            (should (equal pilish--followup-queue '("My follow-up question")))
            (should-not (string-match-p "My follow-up question" (buffer-string))))
          ;; Low-level completion does not release the queued prompt.
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success))))
                      ((symbol-function 'pilish--refresh-header) #'ignore))
              (pilish--handle-display-event '(:type "agent_end"))
              (should (equal pilish--followup-queue '("My follow-up question")))
              (should (null sent-prompt))
              (pilish--handle-display-event '(:type "agent_settled")))
            ;; Queue should be empty now
            (should (null pilish--followup-queue))
            ;; Should have sent the queued message
            (should (equal sent-prompt "My follow-up question"))
            ;; Message should now be displayed in chat
            (should (string-match-p "My follow-up question" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-queues-while-awaiting-settlement ()
  "New input cannot overtake an older follow-up before run settlement."
  (let ((chat-buf (get-buffer-create "*pilish-test-drain-pending-send*"))
        (input-buf (get-buffer-create "*pilish-test-drain-pending-send-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--input-buffer input-buf
                  pilish--followup-queue '("older queued follow-up"))
            (pilish--handle-display-event '(:type "agent_start"))
            (pilish--handle-display-event '(:type "agent_end")))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "new prompt during drain window")
            (cl-letf (((symbol-function 'pilish--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success))))
                      ((symbol-function 'message) #'ignore))
              (pilish-send))
            (should (string-empty-p (buffer-string))))
          (with-current-buffer chat-buf
            (should (null sent-prompt))
            (should (equal pilish--followup-queue
                           '("new prompt during drain window"
                             "older queued follow-up")))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-queues-when-stale-state-arrives-before-agent-start ()
  "A stale idle get_state response must not open a second-send gap."
  (let ((chat-buf (get-buffer-create "*pilish-test-stale-state-send-chat*"))
        (input-buf (get-buffer-create "*pilish-test-stale-state-send-input*"))
        (sent-prompts nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--input-buffer input-buf
                  pilish--state '(:session-id "same-session")))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd _callback)
                       (push (plist-get cmd :message) sent-prompts))))
            (with-current-buffer input-buf
              (insert "first prompt")
              (pilish-send))
            (with-current-buffer chat-buf
              (pilish--apply-state-response
               chat-buf
               '(:success t :data (:isStreaming :false
                                   :isCompacting :false
                                   :sessionId "same-session"
                                   :sessionFile "/tmp/same.jsonl"))))
            (with-current-buffer input-buf
              (insert "second prompt")
              (pilish-send)))
          (with-current-buffer chat-buf
            (should (equal (reverse sent-prompts) '("first prompt")))
            (should (equal pilish--followup-queue
                           '("second prompt")))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-process-followup-queue-waits-until-ready ()
  "Direct queue draining keeps its own safe-to-send guard."
  (dolist (case '((sending nil nil)
                  (streaming nil nil)
                  (compacting nil nil)
                  (idle "pending local echo" nil)
                  (idle nil prompt-wait)
                  (idle nil transition)))
    (with-temp-buffer
      (pilish-chat-mode)
      (let ((sent-prompts nil))
        (setq pilish--status (nth 0 case)
              pilish--local-user-message (nth 1 case)
              pilish--followup-queue '("queued follow-up"))
        (pcase (nth 2 case)
          ('prompt-wait
           (pilish--begin-prompt-start-wait)
           (setq pilish--status 'idle))
          ('transition
           (pilish--begin-session-transition 'mock-proc)))
        (cl-letf (((symbol-function 'pilish--send-prompt)
                   (lambda (text &optional on-success &rest _)
                     (push text sent-prompts)
                     (when on-success (funcall on-success)))))
          (pilish--process-followup-queue))
        (should (null sent-prompts))
        (should (equal pilish--followup-queue
                       '("queued follow-up")))))))

(ert-deftest pilish-test-followup-queue-fifo-order ()
  "Multiple follow-ups are processed in FIFO order."
  (let ((chat-buf (get-buffer-create "*pilish-test-fifo*"))
        (input-buf (get-buffer-create "*pilish-test-fifo-input*"))
        (sent-prompts nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf)
            (setq pilish--followup-queue nil))
          ;; Queue three messages while busy
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (dolist (msg '("First message" "Second message" "Third message"))
              (erase-buffer)
              (insert msg)
              (pilish-send)))
          ;; All three should be in queue
          (with-current-buffer chat-buf
            (should (= 3 (length pilish--followup-queue))))
          ;; Each completed queued run releases one prompt at settlement.
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--send-prompt)
                       (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                      ((symbol-function 'pilish--refresh-header) #'ignore))
              (dotimes (i 3)
                (pilish--handle-display-event '(:type "agent_start"))
                (pilish--handle-display-event '(:type "agent_end"))
                (should (= i (length sent-prompts)))
                (pilish--handle-display-event '(:type "agent_settled")))))
          ;; Should have sent all three in FIFO order (sent-prompts is reversed)
          (should (equal (reverse sent-prompts)
                         '("First message" "Second message" "Third message")))
          ;; Queue should be empty
          (with-current-buffer chat-buf
            (should (null pilish--followup-queue))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-steering-displayed-from-echo ()
  "Steering is NOT displayed locally - it's displayed when pi echoes it back.
This ensures steering appears at the correct position in the conversation
(after the current assistant output completes)."
  (let ((chat-buf (get-buffer-create "*pilish-test-steer-echo*"))
        (input-buf (get-buffer-create "*pilish-test-steer-echo-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming)
            (setq pilish--input-buffer input-buf)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nWorking on something...\n")))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Stop and do something else")
            (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--rpc-async) #'ignore))
              (pilish-queue-steering)))
          ;; Steering is NOT displayed when sent (unlike normal sends)
          (with-current-buffer chat-buf
            (should-not (string-match-p "Stop and do something else" (buffer-string)))
            ;; local-user-message should still be nil (steering doesn't set it)
            (should-not pilish--local-user-message))
          ;; Simulate pi echoing the steering message back via message_start
          (with-current-buffer chat-buf
            (pilish--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Stop and do something else")]
                         :timestamp 1704067200000)))
            ;; NOW it should be displayed (from the echo)
            (should (string-match-p "Stop and do something else" (buffer-string)))
            ;; Should be displayed exactly once
            (let ((count 0)
                  (start 0))
              (while (string-match "Stop and do something else" (buffer-string) start)
                (setq count (1+ count))
                (setq start (match-end 0)))
              (should (= count 1)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-steering-echo-followed-by-assistant-shows-header ()
  "After steering message, the next assistant message shows its header.
This tests the full flow: steering echo resets the flag, then the next
message_start role=assistant displays the 'Assistant' header."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish--status 'streaming)
          (pilish--local-user-message nil)
          ;; Simulate that first assistant header was already shown
          (pilish--assistant-header-shown t))
      ;; First, some assistant content is already in the buffer
      (let ((inhibit-read-only t))
        (insert "Assistant\n=========\nPrevious response...\n"))
      ;; Simulate steering message echo from pi
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Stop it")]
                   :timestamp 1704067200000)))
      ;; Steering message should be displayed
      (should (string-match-p "Stop it" (buffer-string)))
      ;; Flag should be reset
      (should-not pilish--assistant-header-shown)
      ;; Now simulate the assistant's response to steering
      (pilish--handle-display-event
       '(:type "message_start"
         :message (:role "assistant")))
      ;; Now we should see TWO "Assistant" headers in the buffer
      (let ((count 0)
            (start 0)
            (content (buffer-string)))
        (while (string-match "Assistant\n=+" content start)
          (setq count (1+ count))
          (setq start (match-end 0)))
        (should (= count 2))))))


(ert-deftest pilish-test-tool-toggle-expands-content ()
  "Toggle button expands collapsed tool output."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    (pilish--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Initially collapsed - should have "... (N more lines)"
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))
    ;; Find and click the button
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pilish-toggle-tool-section)
    ;; Now should show all lines
    (should (string-match-p "L10" (buffer-string)))
    (should (string-match-p "\\[-\\]" (buffer-string)))))

(ert-deftest pilish-test-tool-toggle-collapses-content ()
  "Toggle button collapses expanded tool output."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    (pilish--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Expand first
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pilish-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))
    ;; Now collapse
    (goto-char (point-min))
    (search-forward "[-]" nil t)
    (backward-char 1)
    (pilish-toggle-tool-section)
    ;; Should be collapsed again
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))))

(ert-deftest pilish-test-tool-toggle-re-expand-after-collapse-from-button ()
  "TAB re-expands after collapsing from the [-] button position.
Regression: collapsing from the [-] button placed cursor at the overlay
boundary where overlays-at returns nil, making the next TAB fall through
to outline-cycle instead of toggling.  Uses enough lines so the [-]
button position in the expanded state exceeds the collapsed overlay end."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    (pilish--display-tool-end "bash" nil
                          '((:type "text" :text "L01\nL02\nL03\nL04\nL05\nL06\nL07\nL08\nL09\nL10\nL11\nL12\nL13\nL14\nL15"))
                          nil nil)
    ;; Expand
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pilish-toggle-tool-section)
    (should (string-match-p "L15" (buffer-string)))
    ;; Navigate to the [-] button (near end of expanded block)
    (goto-char (point-min))
    (search-forward "[-]" nil t)
    (beginning-of-line)
    ;; Collapse from the button position
    (pilish-toggle-tool-section)
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    ;; Verify cursor landed inside the tool block overlay, not at its boundary
    (should (seq-find (lambda (o) (overlay-get o 'pilish-tool-block))
                      (overlays-at (point))))
    ;; The critical assertion: TAB must still work to re-expand
    (pilish-toggle-tool-section)
    (should (string-match-p "L15" (buffer-string)))))

(ert-deftest pilish-test-tool-toggle-expands-with-highlighting ()
  "Expanded tool output has syntax highlighting applied.
With tree-sitter, code blocks get `font-lock-string-face' from
the markdown grammar.  Tool output face also applies."
  (with-temp-buffer
    (pilish-chat-mode)
    ;; Create a read tool with Python content (>10 lines to trigger collapse)
    ;; The 'def' keyword is on line 11, hidden initially
    (pilish--display-tool-start "read" '(:path "test.py"))
    (pilish--display-tool-end "read" '(:path "test.py")
                          '((:type "text" :text "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\ndef hello():\n    return 42"))
                          nil nil)
    ;; Initially collapsed - 'def' is hidden
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "def hello" (buffer-string)))
    ;; Expand
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pilish-toggle-tool-section)
    ;; Now 'def' should be visible
    (should (string-match-p "def hello" (buffer-string)))
    ;; Re-fontify after expansion (in GUI, jit-lock handles this)
    (font-lock-ensure)
    ;; Find 'def' keyword and check for some face being applied
    (goto-char (point-min))
    (search-forward "def" nil t)
    (let ((face (get-text-property (match-beginning 0) 'face)))
      ;; With embedded language support, 'def' gets font-lock-keyword-face
      ;; from the Python grammar.  Without it, font-lock-string-face.
      (should face))))

(ert-deftest pilish-test-tab-works-from-anywhere-in-block ()
  "TAB toggles tool output from any position within the block."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    (pilish--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to the header line (not the button)
    (goto-char (point-min))
    (search-forward "$ ls" nil t)
    (beginning-of-line)
    ;; TAB should still expand
    (pilish-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))))

(ert-deftest pilish-test-tab-preserves-cursor-position ()
  "TAB toggle doesn't jump cursor unnecessarily."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    (pilish--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to L3 line
    (goto-char (point-min))
    (search-forward "L3" nil t)
    (beginning-of-line)
    (let ((line-content (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
      ;; Expand
      (pilish-toggle-tool-section)
      ;; Should still be on a line starting with L
      (should (string-match-p "^L[0-9]" 
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))))

(ert-deftest pilish-test-toggle-preserves-window-scroll ()
  "Toggle collapse/expand should preserve window scroll when viewing content before tool.
When window shows content BEFORE the tool block, toggle should not jump away."
  (let ((buf (generate-new-buffer "*test-toggle-scroll*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (pilish-chat-mode)
            ;; Add some content before the tool block
            (let ((inhibit-read-only t))
              (insert "Header line 1\nHeader line 2\nHeader line 3\n\n"))
            ;; Create tool output with many lines
            (pilish--display-tool-start "read" '(:path "test.el"))
            (pilish--display-tool-end
             "read" nil
             `((:type "text"
                :text ,(mapconcat (lambda (n) (format "Line %03d content" n))
                                  (number-sequence 1 50) "\n")))
             nil nil))
          ;; Display buffer in a window so we can test scroll
          (let ((win (display-buffer buf)))
            (when win
              (with-selected-window win
                ;; Position window at the header (before tool block)
                (goto-char (point-min))
                (recenter 0)
                (let ((start-before (window-start win)))
                  ;; Expand the tool content
                  (search-forward "..." nil t)
                  (pilish-toggle-tool-section)
                  ;; Window should not have jumped
                  (should (= (window-start win) start-before))
                  ;; Now collapse
                  (search-forward "[-]" nil t)
                  (pilish-toggle-tool-section)
                  ;; Window should still be at same position
                  (should (= (window-start win) start-before)))))))
      (kill-buffer buf))))

(ert-deftest pilish-test-format-fork-message ()
  "Fork message formatted with index and preview."
  (let ((msg '(:entryId "abc-123" :text "Hello world, this is a test")))
    ;; With index
    (let ((result (pilish--format-fork-message msg 2)))
      (should (string-match-p "2:" result))
      (should (string-match-p "Hello world" result)))
    ;; Without index
    (let ((result (pilish--format-fork-message msg)))
      (should (string-match-p "Hello world" result))
      (should-not (string-match-p ":" result)))))

(ert-deftest pilish-test-fork-detects-empty-messages-vector ()
  "Fork correctly detects empty messages vector from RPC.
JSON arrays are parsed as vectors, and (null []) is nil, not t.
The fork code must use seq-empty-p or length check."
  (let ((rpc-called nil)
        (message-shown nil))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--status 'idle)
      (cl-letf (((symbol-function 'pilish--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_proc cmd cb)
                   (setq rpc-called t)
                   ;; Simulate response with empty vector (no messages to fork from)
                   (funcall cb '(:success t :data (:messages [])))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "No messages" fmt)
                     (setq message-shown t)))))
        (pilish-fork)
        (should rpc-called)
        ;; Should show "No messages to fork from", not call completing-read
        (should message-shown)))))

(ert-deftest pilish-test-format-fork-message-handles-nil-text ()
  "Format fork message handles nil text gracefully."
  (let ((msg '(:entryId "abc-123" :text nil)))
    ;; Should not error, should return something displayable
    (let ((result (pilish--format-fork-message msg 1)))
      (should (stringp result)))))

(ert-deftest pilish-test-load-session-history-uses-provided-buffer ()
  "load-session-history uses provided chat buffer, not current buffer context.
This ensures history loads correctly when callback runs in arbitrary context."
  (let* ((chat-buf (generate-new-buffer "*pilish-chat:test-history/*"))
         (rpc-callback nil)
         (proc (start-process "test-history-load-provided-buffer" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc))
          ;; Mock RPC to capture callback
          (cl-letf (((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            ;; Call with explicit buffer
            (pilish--load-session-history proc nil chat-buf))
          ;; Simulate callback from different buffer context
          (with-temp-buffer
            (funcall rpc-callback
                     '(:success t :data (:messages [(:role "user" :content "test")]))))
          ;; Chat buffer should have been updated (has startup header)
          (with-current-buffer chat-buf
            (should (string-match-p "C-c C-c" (buffer-string)))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-header-line-includes-session-name ()
  "pilish--header-line-string includes session name when set."
  (let ((chat-buf (get-buffer-create "*pi-test-header-session-name*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (setq pilish--state '(:model (:name "test-model") :thinking-level "high"))
          ;; Without session name
          (setq pilish--session-name nil)
          (let ((header (pilish--header-line-string)))
            (should-not (string-match-p "My Session" header)))
          ;; With session name
          (setq pilish--session-name "My Session")
          (let ((header (pilish--header-line-string)))
            (should (string-match-p "My Session" header))
            ;; Should have separator before session name
            (should (string-match-p "│" header))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-header-line-truncates-long-session-name ()
  "pilish--header-line-string truncates long session names."
  (let ((chat-buf (get-buffer-create "*pi-test-header-truncate*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (setq pilish--state '(:model (:name "test-model")))
          ;; Set a very long session name (longer than 30 chars)
          (setq pilish--session-name "This is a very long session name that should be truncated")
          (let ((header (pilish--header-line-string)))
            ;; Should contain truncated version with ellipsis
            (should (string-match-p "This is a very long session" header))
            (should (string-match-p "…" header))
            ;; Should NOT contain the full name
            (should-not (string-match-p "truncated$" header))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-session-name-empty-shows-current ()
  "pilish-set-session-name with empty string shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-show-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--session-name "My Session"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pilish-set-session-name "")))
          ;; Should show current name, not change it
          (should (equal (buffer-local-value 'pilish--session-name chat-buf)
                         "My Session"))
          (should (member "Pi: Session name: My Session" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-session-name-empty-no-name-shows-message ()
  "pilish-set-session-name with empty string and no name shows message."
  (let ((chat-buf (get-buffer-create "*pi-test-no-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--session-name nil))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pilish-set-session-name "")))
          (should (member "Pi: No session name set" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-session-name-no-process-errors ()
  "pilish-set-session-name errors when no process is running."
  (let ((chat-buf (get-buffer-create "*pi-test-no-proc*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode))
          ;; Mock pilish--get-process to return nil
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () nil)))
            (should-error
             (with-current-buffer chat-buf
               (pilish-set-session-name "New Name"))
             :type 'user-error)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-session-name-sends-rpc ()
  "pilish-set-session-name sends correct RPC command."
  (let ((chat-buf (get-buffer-create "*pi-test-rpc*"))
        (pilish--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (pilish-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'pilish--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'pilish--get-chat-buffer)
                           (lambda () chat-buf)))
                  (pilish-set-session-name "Test Session"))
                ;; Wait for output
                (pilish-test-wait-until
                 (lambda ()
                   (with-current-buffer output-buffer
                     (> (buffer-size) 0)))
                 1.0 0.05 fake-proc)
                ;; Verify JSON sent
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :type) "set_session_name"))
                    (should (equal (plist-get json :name) "Test Session")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer)
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-session-name-trims-whitespace ()
  "pilish-set-session-name trims whitespace from name."
  (let ((chat-buf (get-buffer-create "*pi-test-trim*"))
        (pilish--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (pilish-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'pilish--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'pilish--get-chat-buffer)
                           (lambda () chat-buf)))
                  (pilish-set-session-name "  Trimmed Name  "))
                ;; Wait for output
                (pilish-test-wait-until
                 (lambda ()
                   (with-current-buffer output-buffer
                     (> (buffer-size) 0)))
                 1.0 0.05 fake-proc)
                ;; Verify JSON sent has trimmed name
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :name) "Trimmed Name")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer)
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-session-name-whitespace-only-shows-current ()
  "pilish-set-session-name with whitespace-only shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-ws*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--session-name "Existing Name"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pilish-set-session-name "   ")))  ; whitespace only
          ;; Should show current name, not try to set
          (should (equal (buffer-local-value 'pilish--session-name chat-buf)
                         "Existing Name"))
          (should (member "Pi: Session name: Existing Name" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-session-name-rpc-failure-shows-error ()
  "pilish-set-session-name shows error on RPC failure."
  (let ((chat-buf (get-buffer-create "*pi-test-fail*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--session-name "Old Name"))
          ;; Mock RPC to call callback with failure
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () 'fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _cmd callback)
                       (funcall callback '(:success nil :error "test error"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pilish-set-session-name "New Name")))
          ;; Name should NOT be updated
          (should (equal (buffer-local-value 'pilish--session-name chat-buf)
                         "Old Name"))
          ;; Error message should be shown
          (should (member "Pi: Failed to set session name: test error" messages)))
      (kill-buffer chat-buf))))

;;; Input History

(ert-deftest pilish-test-history-add-to-ring ()
  "pilish--history-add adds input to ring."
  (let ((pilish--input-ring nil))
    (pilish--history-add "first")
    (pilish--history-add "second")
    (should (equal (ring-ref (pilish--input-ring) 0) "second"))
    (should (equal (ring-ref (pilish--input-ring) 1) "first"))))

(ert-deftest pilish-test-history-no-duplicate ()
  "pilish--history-add skips duplicates of last entry."
  (let ((pilish--input-ring nil))
    (pilish--history-add "first")
    (pilish--history-add "first")
    (should (= (ring-length (pilish--input-ring)) 1))))

(ert-deftest pilish-test-history-skip-empty ()
  "pilish--history-add skips empty input."
  (let ((pilish--input-ring nil))
    (pilish--history-add "")
    (pilish--history-add "   ")
    (should (ring-empty-p (pilish--input-ring)))))

(ert-deftest pilish-test-history-previous-input ()
  "pilish-previous-input navigates backward through history."
  (let ((pilish--input-ring nil))
    (pilish--history-add "first")
    (pilish--history-add "second")
    (with-temp-buffer
      (pilish-input-mode)
      (insert "current")
      (pilish-previous-input)
      (should (equal (buffer-string) "second"))
      (should (equal pilish--input-saved "current"))
      (pilish-previous-input)
      (should (equal (buffer-string) "first")))))

(ert-deftest pilish-test-history-next-input ()
  "pilish-next-input navigates forward and restores saved input."
  (let ((pilish--input-ring nil))
    (pilish--history-add "first")
    (pilish--history-add "second")
    (with-temp-buffer
      (pilish-input-mode)
      (insert "current")
      (pilish-previous-input)
      (pilish-previous-input)
      (should (equal (buffer-string) "first"))
      (pilish-next-input)
      (should (equal (buffer-string) "second"))
      (pilish-next-input)
      (should (equal (buffer-string) "current")))))

(ert-deftest pilish-test-history-keys-bound ()
  "History keys are bound in pilish-input-mode."
  (with-temp-buffer
    (pilish-input-mode)
    (should (eq (key-binding (kbd "M-p")) 'pilish-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'pilish-next-input))
    (should (eq (key-binding (kbd "C-r")) 'pilish-history-isearch-backward))))

(ert-deftest pilish-test-history-isolated-per-buffer ()
  "Input history is isolated per buffer, not shared globally.
Regression test for #27: history was shared across all sessions."
  (let ((buf1 (generate-new-buffer "*pilish-input:project-a*"))
        (buf2 (generate-new-buffer "*pilish-input:project-b*")))
    (unwind-protect
        (progn
          ;; Add history in buffer 1
          (with-current-buffer buf1
            (pilish-input-mode)
            (pilish--history-add "project-a-query"))
          ;; Add different history in buffer 2
          (with-current-buffer buf2
            (pilish-input-mode)
            (pilish--history-add "project-b-query"))
          ;; Buffer 1 should only see its own history
          (with-current-buffer buf1
            (should (= (ring-length (pilish--input-ring)) 1))
            (should (equal (ring-ref (pilish--input-ring) 0) "project-a-query")))
          ;; Buffer 2 should only see its own history
          (with-current-buffer buf2
            (should (= (ring-length (pilish--input-ring)) 1))
            (should (equal (ring-ref (pilish--input-ring) 0) "project-b-query"))))
      ;; Cleanup
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; History Isearch (C-r incremental search)

(ert-deftest pilish-test-history-isearch-empty-history-errors ()
  "pilish-history-isearch-backward errors with empty history."
  (with-temp-buffer
    (pilish-input-mode)
    (should-error (pilish-history-isearch-backward) :type 'user-error)))

(ert-deftest pilish-test-history-isearch-saves-current-input ()
  "pilish-history-isearch-backward saves current buffer content."
  (with-temp-buffer
    (pilish-input-mode)
    (pilish--history-add "old command")
    (insert "my current input")
    ;; Mock isearch-backward to avoid actually starting isearch
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (pilish-history-isearch-backward))
    (should (equal pilish--history-isearch-saved-input "my current input"))))

(ert-deftest pilish-test-history-isearch-sets-active-flag ()
  "pilish-history-isearch-backward sets the active flag."
  (with-temp-buffer
    (pilish-input-mode)
    (pilish--history-add "old command")
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (pilish-history-isearch-backward))
    (should pilish--history-isearch-active)))

(ert-deftest pilish-test-history-isearch-end-restores-on-quit ()
  "pilish--history-isearch-end restores input when isearch is quit."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--history-isearch-active t)
    (setq pilish--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "some history item")
    ;; Simulate isearch quit
    (let ((isearch-mode-end-hook-quit t))
      (pilish--history-isearch-end))
    (should (equal (buffer-string) "original input"))
    (should-not pilish--history-isearch-active)))

(ert-deftest pilish-test-history-isearch-end-keeps-on-accept ()
  "pilish--history-isearch-end keeps history item when accepted."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--history-isearch-active t)
    (setq pilish--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "chosen history item")
    ;; Simulate isearch accept (quit is nil)
    (let ((isearch-mode-end-hook-quit nil))
      (pilish--history-isearch-end))
    (should (equal (buffer-string) "chosen history item"))
    (should-not pilish--history-isearch-active)))

(ert-deftest pilish-test-history-isearch-goto-index ()
  "pilish--history-isearch-goto loads history item into buffer."
  (with-temp-buffer
    (pilish-input-mode)
    (pilish--history-add "first")
    (pilish--history-add "second")
    (pilish--history-add "third")
    (insert "current")
    (pilish--history-isearch-goto 1)  ; "second" (0=third, 1=second)
    (should (equal (buffer-string) "second"))
    (should (= pilish--history-isearch-index 1))))

(ert-deftest pilish-test-history-isearch-hook-added ()
  "isearch-mode-hook is set up in pilish-input-mode."
  (with-temp-buffer
    (pilish-input-mode)
    (should (memq 'pilish--history-isearch-setup isearch-mode-hook))))

(ert-deftest pilish-test-history-isearch-goto-nil-restores-saved ()
  "pilish--history-isearch-goto with nil index restores saved input."
  (with-temp-buffer
    (pilish-input-mode)
    (pilish--history-add "history item")
    (setq pilish--history-isearch-saved-input "my original input")
    (insert "something else")
    (pilish--history-isearch-goto nil)
    (should (equal (buffer-string) "my original input"))
    (should (null pilish--history-isearch-index))))

(ert-deftest pilish-test-history-isearch-goto-empty-saved-input ()
  "pilish--history-isearch-goto with nil index and empty saved input."
  (with-temp-buffer
    (pilish-input-mode)
    (pilish--history-add "history item")
    (setq pilish--history-isearch-saved-input "")
    (insert "something else")
    (pilish--history-isearch-goto nil)
    (should (equal (buffer-string) ""))
    (should (null pilish--history-isearch-index))))

;;; Input Buffer Completion

(ert-deftest pilish-test-input-mode-has-only-own-capfs ()
  "Input mode should only include our own completion functions.
`text-mode' adds `ispell-completion-at-point' by default, which pollutes
the completion candidates with dictionary words.  Our input buffer should
only offer our own capfs (slash commands, file references, paths)."
  (with-temp-buffer
    (pilish-input-mode)
    (should (equal completion-at-point-functions
                   '(pilish--path-capf
                     pilish--file-reference-capf
                     pilish--command-capf)))))

(ert-deftest pilish-test-input-mode-has-only-own-capfs-with-markdown ()
  "Only our capfs present even with markdown highlighting enabled."
  (let ((pilish-input-markdown-highlighting t))
    (with-temp-buffer
      (pilish-input-mode)
      (should (equal completion-at-point-functions
                     '(pilish--path-capf
                       pilish--file-reference-capf
                       pilish--command-capf))))))

;;; Input Buffer Slash Completion

(ert-deftest pilish-test-command-capf-returns-nil-without-slash ()
  "Completion returns nil when not after slash."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "hello")
    (should-not (pilish--command-capf))))

(ert-deftest pilish-test-command-capf-returns-nil-at-line-start ()
  "Completion returns nil when point is at beginning of line."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "/test")
    (goto-char (line-beginning-position))
    (should-not (pilish--command-capf))))

(ert-deftest pilish-test-command-capf-returns-completion-data ()
  "Completion returns data when after slash at start of buffer."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--commands '((:name "test-cmd" :description "Test")))
    (insert "/te")
    (let ((result (pilish--command-capf)))
      (should result)
      (should (= (nth 0 result) 2))  ; Start after /
      (should (= (nth 1 result) 4))  ; End at point
      (should (member "test-cmd" (nth 2 result))))))

(ert-deftest pilish-test-command-capf-ignores-slash-on-later-lines ()
  "Completion ignores / on lines after the first (pi only expands at buffer start)."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--commands '((:name "test-cmd" :description "Test")))
    (insert "Some context:\n/te")
    (should-not (pilish--command-capf))))

(ert-deftest pilish-test-command-capf-includes-builtins ()
  "Completion includes built-in commands even when RPC returns nothing."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--commands nil)
    (insert "/co")
    (let ((result (pilish--command-capf)))
      (should result)
      (should (member "compact" (nth 2 result)))
      (should (member "new" (nth 2 result)))
      (should (member "model" (nth 2 result))))))

(ert-deftest pilish-test-command-capf-merges-builtins-and-rpc ()
  "Completion merges built-in and RPC commands without duplicates."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--commands '((:name "my-ext" :description "Extension")))
    (insert "/")
    (let* ((result (pilish--command-capf))
           (names (nth 2 result)))
      ;; Has built-in
      (should (member "compact" names))
      ;; Has RPC command
      (should (member "my-ext" names))
      ;; No duplicates
      (should (= (length (seq-filter (lambda (n) (equal n "compact")) names)) 1)))))

(ert-deftest pilish-test-send-prompt-sends-literal ()
  "pilish--send-prompt sends text literally (no expansion).
Pi handles command expansion on the server side."
  (let* ((rpc-message nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pilish--get-process)
                   (lambda () fake-proc))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_proc msg _cb) (setq rpc-message msg))))
          (pilish--send-prompt "/greet world")
          ;; Should send literal /greet world, NOT expanded
          (should (equal (plist-get rpc-message :message) "/greet world")))
      (delete-process fake-proc))))

(ert-deftest pilish-test-image-preview-does-not-add-prompt-images ()
  "Display previews do not add an images field to outgoing prompts."
  (let* ((rpc-message nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pilish--get-process)
                   (lambda () fake-proc))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_proc message _callback)
                     (setq rpc-message message))))
          (pilish--send-prompt "/tmp/screenshot.png")
          (should (equal "/tmp/screenshot.png"
                         (plist-get rpc-message :message)))
          (should-not (plist-member rpc-message :images)))
      (delete-process fake-proc))))

(ert-deftest pilish-test-prompt-image-png-end-to-end ()
  "Menu attach-image entry content-sniffs a misleadingly named PNG for exact RPC content."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (pilish-test--write-prompt-image
                  (expand-file-name "pixel.txt" dir) 'png))
           (data (pilish-test--prompt-image-base64 'png))
           rpc-message)
      (with-current-buffer input-buf
        (pilish-test--attach-image-via-menu path)
        (should (string-match-p "pixel.txt" (pilish-test--input-header)))
        (pilish-test--attach-image-via-menu path 'clear)
        (should-not (string-match-p "pixel.txt" (pilish-test--input-header)))
        (pilish-test--attach-image-via-menu path)
        (delete-file path)
        (insert "Describe the pixel")
        (cl-letf (((symbol-function 'pilish--get-process)
                   (lambda () 'image-process))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_process command _callback)
                     (setq rpc-message command))))
          (pilish-send))
        (should (equal rpc-message
                       (list :type "prompt" :message "Describe the pixel" :images
                             (vector (list :type "image" :data data :mimeType "image/png")))))
        (should (equal (ring-ref pilish--input-ring 0) "Describe the pixel"))))))

(ert-deftest pilish-test-prompt-image-sync-rpc-error-restores-draft ()
  "A synchronous RPC error restores the exact pending image draft."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (pilish-test--write-prompt-image
                  (expand-file-name "sync-error.png" dir) 'png))
           (text "Keep this image prompt")
           attached-image
           chat-before)
      (with-current-buffer input-buf
        (pilish-test--attach-image path)
        (setq attached-image (pilish--get-prompt-image))
        (insert text))
      (setq chat-before (with-current-buffer chat-buf (buffer-string)))
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _) (error "synchronous RPC failure")))
                ((symbol-function 'message) #'ignore))
        (with-current-buffer input-buf
          (condition-case nil
              (pilish-send)
            (error nil))))
      (with-current-buffer input-buf
        (should (equal (buffer-string) text))
        (should (eq (pilish--get-prompt-image) attached-image)))
      (with-current-buffer chat-buf
        (should-not (pilish--prompt-start-wait-active-p))
        (should (eq pilish--status 'idle))
        (should-not pilish--local-user-message)
        (should (equal (buffer-string) chat-before))))))

(ert-deftest pilish-test-prompt-image-signatures-and-rejections ()
  "Other raster signatures attach; non-images and over-cap sources do not."
  (pilish-test-with-prompt-image-session (dir _chat-buf input-buf)
    (with-current-buffer input-buf
      (let (previous)
        (dolist (spec '((jpeg "photo.jpg") (gif "pixel.gif")
                        (webp "pixel.webp")))
          (when previous
            (pilish-test--attach-image-via-menu previous 'clear))
          (setq previous
                (pilish-test--write-prompt-image
                 (expand-file-name (cadr spec) dir) (car spec)))
          (pilish-test--attach-image previous)
          (should (string-match-p
                   (regexp-quote (file-name-nondirectory previous))
                   (pilish-test--input-header))))
        (pilish-test--attach-image-via-menu previous 'clear))
      (let* ((not-image (expand-file-name "not-image.txt" dir))
             (too-large (pilish-test--write-prompt-image
                         (expand-file-name "too-large.png" dir) 'png)))
        (with-temp-file not-image (insert "not an image"))
        (dolist (case `((,not-image nil "image\\|format")
                        (,too-large 1 "large\\|limit\\|byte\\|size")))
          (let (feedback)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (when format-string
                           (setq feedback (apply #'format format-string args))))))
              (condition-case error-data
                  (let ((pilish-prompt-image-max-bytes
                         (or (cadr case) most-positive-fixnum)))
                    (pilish-test--attach-image (car case)))
                (user-error (setq feedback (error-message-string error-data)))))
            (should (string-match-p (caddr case) (downcase (or feedback ""))))
            (should-not (string-match-p
                         (regexp-quote (file-name-nondirectory (car case)))
                         (pilish-test--input-header)))))))))

(ert-deftest pilish-test-prompt-image-refusal-matrix-preserves-draft ()
  "Capability, busy, slash, steering, and empty refusals retain the draft."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let ((path (pilish-test--write-prompt-image
                 (expand-file-name "guard.png" dir) 'png)))
      (dolist (case '((text-model "Describe" idle send "model\\|support")
                      (missing-input "Missing metadata" idle send "model\\|support\\|load")
                      (unknown-model "Unknown model" idle send "model\\|support\\|load")
                      (busy "Wait" streaming send "busy\\|stream")
                      (slash "/new" idle send "slash\\|command")
                      (steering "Change" streaming steer "steer")
                      (empty "" idle send "empty\\|text\\|prompt")))
        (pcase-let ((`(,kind ,text ,status ,action ,reason) case))
          (with-current-buffer input-buf
            (erase-buffer)
            (pilish-test--attach-image path)
            (insert text))
          (with-current-buffer chat-buf
            (setq pilish--status status
                  pilish--state
                  (pcase kind
                    ('text-model '(:model (:name "Text" :input ["text"])))
                    ('missing-input '(:model (:name "Loading")))
                    ('unknown-model nil)
                    (_ '(:model (:name "Vision" :input ["text" "image"]))))))
          (let (feedback rpc-called builtin-called)
            (cl-letf (((symbol-function 'pilish--get-process)
                       (lambda () 'image-process))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pilish--rpc-async)
                       (lambda (&rest _) (setq rpc-called t)))
                      ((symbol-function 'pilish-new-session)
                       (lambda () (setq builtin-called t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (when format-string
                           (setq feedback (apply #'format format-string args))))))
              (with-current-buffer input-buf
                (pcase action
                  ('send (pilish-send))
                  ('steer (pilish-queue-steering)))
                (should (equal (buffer-string) text))
                (should (string-match-p "guard.png"
                                        (pilish-test--input-header))))
              (should-not rpc-called)
              (should-not builtin-called)
              (should (string-match-p reason (downcase (or feedback "")))))
          (with-current-buffer input-buf
            (pilish-test--attach-image-via-menu path 'clear))))))))

(ert-deftest pilish-test-prompt-image-waits-for-model-change ()
  "Image send waits for model selection, then uses the accepted model state."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (pilish-test--write-prompt-image
                  (expand-file-name "model-change.png" dir) 'png))
           (old-model '(:id "vision-old" :name "Vision Old"
                        :provider "fake" :input ["text" "image"]))
           (new-model '(:id "vision-next" :name "Vision Next"
                        :provider "fake" :input ["text" "image"]))
           (text "Wait for the selected model")
           attached-image
           model-callback
           prompt-command)
      (with-current-buffer chat-buf
        (setq pilish--process 'image-process
              pilish--state (list :model old-model)))
      (with-current-buffer input-buf
        (pilish-test--attach-image path)
        (setq attached-image (pilish--get-prompt-image))
        (insert text))
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model new-model)))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (or (seq-find
                        (lambda (candidate)
                          (string-match-p "Vision Next" candidate))
                        collection)
                       (ert-fail "Missing Vision Next model choice"))))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("set_model" (setq model-callback callback))
                     ("prompt" (setq prompt-command command)))))
                ((symbol-function 'message) #'ignore))
        (with-current-buffer input-buf
          (pilish-select-model)
          (should (functionp model-callback))
          (pilish-send)
          (should (equal (buffer-string) text))
          (should (eq (pilish--get-prompt-image) attached-image)))
        (should-not prompt-command)
        (funcall model-callback
                 (list :success t :command "set_model" :data new-model))
        (with-current-buffer input-buf
          (pilish-send)
          (should (string-empty-p (buffer-string)))
          (should-not (pilish--get-prompt-image)))
        (should (equal (plist-get prompt-command :message) text))
        (should (plist-member prompt-command :images))))))

(ert-deftest pilish-test-model-change-refuses-image-preflight ()
  "Model selection cannot overlap an image prompt awaiting acceptance."
  (pilish-test-with-prompt-image-session (_dir chat-buf _input-buf)
    (let (rpc-called feedback)
      (with-current-buffer chat-buf
        (setq pilish--process 'image-process pilish--status 'sending)
        (pilish--begin-prompt-start-wait))
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () chat-buf))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (with-current-buffer chat-buf
          (pilish-select-model)))
      (should-not rpc-called)
      (should (string-match-p "Cannot change models"
                              (or feedback ""))))))

(ert-deftest pilish-test-model-change-aborts-if-process-changes-during-selection ()
  "A selector cannot acquire a model gate for a process that was replaced."
  (pilish-test-with-prompt-image-session (_dir chat-buf _input-buf)
    (let* ((old-model '(:id "old" :name "Old" :provider "fake"))
           (new-model '(:id "new" :name "New" :provider "fake"))
           rpc-called
           feedback)
      (with-current-buffer chat-buf
        (setq pilish--process 'old-process
              pilish--state (list :model old-model)))
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'old-process))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () chat-buf))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model new-model)))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (with-current-buffer chat-buf
                     (setq pilish--process 'new-process))
                   "New [fake]"))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (with-current-buffer chat-buf
          (pilish-select-model)))
      (should-not rpc-called)
      (with-current-buffer chat-buf
        (should-not (pilish--model-change-pending-p)))
      (should (equal feedback
                     "Pi: Process changed while selecting a model; try again")))))

(ert-deftest pilish-test-model-cancellation-restores-gated-queue ()
  "Cancelling a model change makes text queued behind it visible."
  (pilish-test-with-prompt-image-session (_dir chat-buf input-buf)
    (with-current-buffer chat-buf
      (setq pilish--process 'old-process)
      (should (pilish--begin-model-change
               'old-process chat-buf))
      (pilish--push-followup "do not strand me")
      (pilish--cancel-model-change-and-restore-followups chat-buf)
      (pilish--set-process 'new-process)
      (should-not (pilish--model-change-pending-p))
      (should-not pilish--followup-queue))
    (with-current-buffer input-buf
      (should (equal (buffer-string) "do not strand me")))))

(ert-deftest pilish-test-failed-model-change-restores-queued-text ()
  "A failed model change must not send queued text under the old model."
  (pilish-test-with-prompt-image-session (_dir chat-buf input-buf)
    (let* ((old-model '(:id "old" :name "Old" :provider "fake"))
           (new-model '(:id "new" :name "New" :provider "fake"))
           model-callback
           prompt-called
           feedback)
      (with-current-buffer chat-buf
        (setq pilish--process 'image-process
              pilish--state (list :model old-model)))
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'pilish--get-chat-buffer)
                 (lambda () chat-buf))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model new-model)))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "New [fake]"))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("set_model" (setq model-callback callback))
                     ("prompt" (setq prompt-called t)))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (with-current-buffer chat-buf
          (pilish-select-model))
        (with-current-buffer input-buf
          (insert "keep this queued")
          (pilish-send)
          (should (string-empty-p (buffer-string))))
        (should (functionp model-callback))
        (funcall model-callback '(:success :false :error "model unavailable")))
      (should-not prompt-called)
      (with-current-buffer chat-buf
        (should-not (pilish--model-change-pending-p))
        (should-not pilish--followup-queue)
        (should (equal (plist-get pilish--state :model) old-model)))
      (with-current-buffer input-buf
        (should (equal (buffer-string) "keep this queued")))
      (should (equal feedback
                     "Pi: Failed to set model: model unavailable")))))

(ert-deftest pilish-test-prompt-image-stale-model-callback-stays-gated ()
  "A replaced process's model callback cannot release the current gate."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (pilish-test--write-prompt-image
                  (expand-file-name "stale-model.png" dir) 'png))
           (old-model '(:id "vision-old" :name "Vision Old"
                        :provider "fake" :input ["text" "image"]))
           (model-a '(:id "vision-a" :name "Vision A"
                      :provider "fake" :input ["text" "image"]))
           (model-b '(:id "vision-b" :name "Vision B"
                      :provider "fake" :input ["text" "image"]))
           (text "Keep gating this image")
           (current-process 'image-process)
           choice-name attached-image model-callbacks prompt-command)
      (with-current-buffer chat-buf
        (setq pilish--process current-process
              pilish--state (list :model old-model)))
      (with-current-buffer input-buf
        (pilish-test--attach-image path)
        (setq attached-image (pilish--get-prompt-image))
        (insert text))
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () current-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'pilish--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model model-a model-b)))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (or (seq-find
                        (lambda (candidate)
                          (string-match-p choice-name candidate))
                        collection)
                       (ert-fail "Missing requested model choice"))))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("set_model"
                      (push (cons (plist-get command :modelId) callback)
                            model-callbacks))
                     ("prompt" (setq prompt-command command)))))
                ((symbol-function 'message) #'ignore))
        (setq choice-name "Vision A")
        (with-current-buffer input-buf
          (pilish-select-model))
        (setq current-process 'replacement-process)
        (with-current-buffer chat-buf
          (pilish--set-process current-process))
        (setq choice-name "Vision B")
        (with-current-buffer input-buf
          (pilish-select-model))
        (let ((callback-a (alist-get "vision-a" model-callbacks
                                     nil nil #'equal))
              (callback-b (alist-get "vision-b" model-callbacks
                                     nil nil #'equal)))
          (should (functionp callback-a))
          (should (functionp callback-b))
          (funcall callback-a
                   (list :success t :command "set_model" :data model-a))
          (with-current-buffer input-buf
            (pilish-send)
            (should (equal (buffer-string) text))
            (should (eq (pilish--get-prompt-image) attached-image)))
          (should-not prompt-command)
          (funcall callback-b
                   (list :success t :command "set_model" :data model-b))
          (with-current-buffer chat-buf
            (should (equal (plist-get (plist-get pilish--state :model)
                                      :id)
                           "vision-b")))
          (with-current-buffer input-buf
            (pilish-send)
            (should (string-empty-p (buffer-string)))
            (should-not (pilish--get-prompt-image)))
          (should (equal (plist-get prompt-command :message) text))
          (should (plist-member prompt-command :images)))))))

(ert-deftest pilish-test-prompt-image-preflight-ownership ()
  "No-process and rejected sends restore image bytes; acceptance consumes them."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (pilish-test--write-prompt-image
                  (expand-file-name "restored.png" dir) 'png))
           (data (pilish-test--prompt-image-base64 'png))
           rpc-message rpc-callback attached-image)
      (with-current-buffer chat-buf (setq pilish--process 'image-process))
      (with-current-buffer input-buf
        (pilish-test--attach-image path)
        (setq attached-image (pilish--get-prompt-image))
        (delete-file path)
        (insert "Recover this turn")
        (cl-letf (((symbol-function 'pilish--get-process) (lambda () nil))
                  ((symbol-function 'message) #'ignore))
          (pilish-send))
        (should (equal (buffer-string) "Recover this turn"))
        (should (string-match-p "restored.png"
                                (pilish-test--input-header)))
        (cl-labels ((send ()
                      (cl-letf (((symbol-function 'pilish--get-process)
                                 (lambda () 'image-process))
                                ((symbol-function 'process-live-p) (lambda (_) t))
                                ((symbol-function 'pilish--rpc-async)
                                 (lambda (_process command callback)
                                   (setq rpc-message command
                                         rpc-callback callback)))
                                ((symbol-function 'message) #'ignore))
                        (pilish-send))))
          (send)
          (let ((pending-block (aref (plist-get rpc-message :images) 0)))
            (should (equal (plist-get pending-block :data) data))
            (should-error (pilish-attach-image 'clear)
                          :type 'user-error)
            (funcall rpc-callback '(:success nil :error "rejected"))
            (should (eq (pilish--get-prompt-image) attached-image))
            (should (equal
                     (pilish--prompt-image-content-block
                      (pilish--get-prompt-image))
                     pending-block)))
          (should (equal (buffer-string) "Recover this turn"))
          (should (string-match-p "restored.png"
                                  (pilish-test--input-header)))
          (setq rpc-message nil rpc-callback nil)
          (send)
          (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
            (funcall rpc-callback '(:success t))))
        (should (string-empty-p (buffer-string)))
        (should-not (string-match-p "restored.png"
                                    (pilish-test--input-header))))
      (with-current-buffer chat-buf
        (should (string-match-p "Recover this turn" (buffer-string)))
        (should (string-match-p "Image: image/png" (buffer-string)))))))

(ert-deftest pilish-test-prompt-image-authoritative-echo-compares-content ()
  "A same-text echo with a different image renders authoritative content."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (pilish-test--write-prompt-image
                  (expand-file-name "original.png" dir) 'png))
           (text "Inspect this image")
           (jpeg-data (pilish-test--prompt-image-base64 'jpeg))
           rpc-callback)
      (with-current-buffer chat-buf (setq pilish--process 'image-process))
      (with-current-buffer input-buf
        (pilish-test--attach-image path)
        (insert text)
        (cl-letf (((symbol-function 'pilish--get-process)
                   (lambda () 'image-process))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_process _command callback)
                     (setq rpc-callback callback))))
          (pilish-send)))
      (should (functionp rpc-callback))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
        (funcall rpc-callback '(:success t))
        (with-current-buffer chat-buf
          (pilish--handle-display-event '(:type "agent_start"))
          (should (string-match-p "Image: image/png" (buffer-string)))
          (pilish--handle-display-event
           (list :type "message_start"
                 :message
                 (list :role "user" :timestamp 1704067200000
                       :content
                       (vector (list :type "text" :text text)
                               (list :type "image" :data jpeg-data
                                     :mimeType "image/jpeg")))))
          (should (string-match-p "Image: image/jpeg" (buffer-string))))))))

(ert-deftest pilish-test-disposition-handled-releases-unstarted-prompt ()
  "Handled input releases its wait immediately, without echo or state probe."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let ((schedule (symbol-function 'run-at-time)) fallbacks)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (seconds repeat function &rest args)
                   (if (eq function 'pilish--clear-sending-if-no-agent-start)
                       (progn (push (cons function args) fallbacks)
                              'fake-prompt-start-timer)
                     (apply schedule seconds repeat function args)))))
        (with-current-buffer input (insert "consumed input") (pilish-send))
        (let ((request (car commands)))
          (with-current-buffer input (insert "newer draft"))
          (pilish-test--stdout
           proc (list :type "response" :command "prompt" :success t
                      :id (plist-get request :id) :data '(:disposition "handled")))
          (with-current-buffer chat
            (should (eq pilish--status 'idle))
            (should (equal pilish--activity-phase "idle"))
            (should-not pilish--prompt-wait)
            (should-not pilish--prompt-start-timer)
            (should-not pilish--local-user-message)
            (should-not pilish--local-user-message-region)
            (should-not (string-match-p "consumed input" (buffer-string))))
          (with-current-buffer input
            (should (equal (buffer-string) "newer draft")))
          (should-not fallbacks)
          (should-not (seq-find (lambda (c) (equal (plist-get c :type) "get_state"))
                                commands)))))))

(ert-deftest pilish-test-disposition-handled-consumes-image-without-local-echo ()
  "An accepted handled image stays consumed, without a phantom image/text turn."
  (pilish-test-with-prompt-image-session (dir chat input)
    (let ((proc (start-process "pilish-handled-image" nil "cat"))
          (path (pilish-test--write-prompt-image
                 (expand-file-name "handled.png" dir) 'png))
          (schedule (symbol-function 'run-at-time)) commands fallbacks)
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag proc nil)
            (with-current-buffer chat (setq pilish--process proc))
            (process-put proc 'pilish-chat-buffer chat)
            (pilish--register-display-handler proc)
            (cl-letf (((symbol-function 'pilish--send-string)
                       (lambda (_process line)
                         (push (pilish--parse-json-line line) commands)))
                      ((symbol-function 'pilish--refresh-header) #'ignore)
                      ((symbol-function 'run-at-time)
                       (lambda (seconds repeat function &rest args)
                         (if (eq function 'pilish--clear-sending-if-no-agent-start)
                             (progn (push (cons function args) fallbacks)
                                    'fake-prompt-start-timer)
                           (apply schedule seconds repeat function args))))
                      ((symbol-function 'message) #'ignore))
              (with-current-buffer input
                (pilish-test--attach-image path)
                (insert "consumed image input")
                (pilish-send)
                (should (string-empty-p (buffer-string)))
                (should-not (pilish--get-prompt-image))
                (insert "newer draft"))
              (let ((request (car commands)))
                (let ((images (plist-get request :images)))
                  (should (= 1 (length images)))
                  (should (equal (plist-get (aref images 0) :type) "image"))
                  (should (equal (plist-get (aref images 0) :mimeType) "image/png"))
                  (should (equal (plist-get (aref images 0) :data)
                                 (pilish-test--prompt-image-base64 'png))))
                (pilish-test--stdout
                 proc (list :type "response" :command "prompt" :success t
                            :id (plist-get request :id) :data '(:disposition "handled"))))
              (with-current-buffer chat
                (should (eq pilish--status 'idle))
                (should-not pilish--prompt-wait)
                (should-not pilish--prompt-start-timer)
                (should-not pilish--local-user-message)
                (should-not pilish--local-user-message-region)
                (should-not (string-match-p "consumed image input\\|Image:" (buffer-string))))
              (with-current-buffer input
                (should (equal (buffer-string) "newer draft"))
                (should-not (pilish--get-prompt-image)))
              (should-not fallbacks)
              (should (= 1 (length commands)))))
        (when (process-live-p proc) (delete-process proc))))))

(ert-deftest pilish-test-disposition-handled-preserves-observed-work ()
  "Start/echo observations outrank handled, retaining active phase, Stop and FIFO."
  (dolist (case '((streaming ((:type "agent_start")) streaming "thinking")
                  (ended ((:type "agent_start") (:type "agent_end" :messages []))
                         sending "thinking")
                  (manual ((:type "agent_start") (:type "agent_end" :messages [])
                           (:type "compaction_start" :reason "manual"))
                          compacting "compact")
                  (retry ((:type "agent_start") (:type "agent_end" :messages [])
                          (:type "auto_retry_start" :attempt 1 :maxAttempts 3
                           :delayMs 1000 :errorMessage "overloaded"))
                         sending "thinking")
                  (settled ((:type "agent_start") (:type "agent_end" :messages [])
                            (:type "agent_settled")) idle "idle")
                  (echo-only nil sending "thinking")))
    (ert-info ((format "before handled acknowledgment: %s" (car case)))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let ((schedule (symbol-function 'run-at-time)) fallbacks)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (seconds repeat function &rest args)
                       (if (eq function 'pilish--clear-sending-if-no-agent-start)
                           (progn (push (cons function args) fallbacks)
                                  'fake-prompt-start-timer)
                         (apply schedule seconds repeat function args))))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer input (insert "consumed input") (pilish-send))
            (let* ((request (car commands))
                   (echoed (memq (car case) '(settled echo-only))))
              (dolist (event (nth 1 case)) (pilish-test--stdout proc event))
              (when echoed
                (pilish-test--stdout
                 proc '(:type "message_start" :message
                         (:role "user" :timestamp 1704067200000
                          :content [(:type "text" :text "consumed input")]))))
              (with-current-buffer input (insert "newer draft"))
              (with-current-buffer chat
                (should (eq pilish--status (nth 2 case)))
                (should (equal pilish--activity-phase (nth 3 case)))
                (unless echoed (setq pilish--followup-queue '("waiting successor")))
                (let ((status-before pilish--status)
                      (phase-before pilish--activity-phase)
                      (queue-before (copy-sequence pilish--followup-queue))
                      (transcript-before (buffer-string)))
                  (setq pilish--aborted t)
                  (pilish-test--stdout
                   proc (list :type "response" :command "prompt" :success t
                              :id (plist-get request :id) :data '(:disposition "handled")))
                  (if echoed
                      (progn
                        (should (eq pilish--status 'idle))
                        (should-not pilish--aborted)
                        (should (= 1 (pilish-test--count-matches
                                      "consumed input" (buffer-string)))))
                    (should (eq pilish--status status-before))
                    (should (equal pilish--activity-phase phase-before))
                    (should pilish--aborted)
                    (should (equal pilish--followup-queue queue-before))
                    (should-not (string-match-p "consumed input" (buffer-string))))
                  (should (equal (buffer-string) transcript-before))
                  (should-not pilish--prompt-wait)
                  (should-not pilish--prompt-start-timer)
                  (should-not pilish--local-user-message)
                  (should-not pilish--local-user-message-region)))
              (with-current-buffer input (should (equal (buffer-string) "newer draft")))
              (should-not fallbacks)
              (should (= 1 (length commands))))))))))

(ert-deftest pilish-test-disposition-handled-during-unstarted-compaction ()
  "Handled cannot finish over compaction; its guarded probe or newer start wins."
  (dolist (stop '(nil t))
    (dolist (next '(idle agent-start))
      (ert-info ((format "stop=%S after compaction=%s" stop next))
        (pilish-test-with-rpc-session (chat input proc commands)
          (let ((schedule (symbol-function 'run-at-time)) fallbacks)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (seconds repeat function &rest args)
                         (if (eq function 'pilish--clear-sending-if-no-agent-start)
                             (progn (push (cons function args) fallbacks)
                                    'fake-prompt-start-timer)
                           (apply schedule seconds repeat function args))))
                      ((symbol-function 'message) #'ignore))
              (with-current-buffer input (insert "consumed input") (pilish-send))
              (let ((request (car commands))
                    (wait (buffer-local-value 'pilish--prompt-wait chat)))
                (pilish-test--stdout proc '(:type "compaction_start" :reason "manual"))
                (with-current-buffer input (insert "newer draft"))
                (with-current-buffer chat
                  (setq pilish--aborted stop pilish--followup-queue '("next")))
                (pilish-test--stdout
                 proc (list :type "response" :command "prompt" :success t
                            :id (plist-get request :id) :data '(:disposition "handled")))
                (with-current-buffer chat
                  (should (eq pilish--status 'compacting))
                  (should (equal pilish--activity-phase "compact"))
                  (should (eq pilish--aborted stop))
                  (should (equal pilish--followup-queue '("next")))
                  (should (eq pilish--prompt-wait wait))
                  (should (pilish--prompt-wait-accepted wait))
                  (should-not (pilish--prompt-wait-started wait))
                  (should-not pilish--local-user-message)
                  (should-not pilish--local-user-message-region)
                  (should-not (string-match-p "consumed input" (buffer-string))))
                (should (= 1 (length fallbacks)))
                (should (= 1 (length commands)))
                (let ((fallback (car fallbacks)))
                  ;; Compaction ends, but this same accepted wait still reserves
                  ;; submission until its existing fallback confirms no turn.
                  (pilish-test--stdout
                   proc '(:type "compaction_end" :reason "manual" :aborted :false
                           :result (:tokensBefore 1000 :summary "Summary")))
                  (with-current-buffer chat
                    (should (eq pilish--prompt-wait wait))
                    (should (eq pilish--status 'idle)))
                  (if (eq next 'agent-start)
                      (progn
                        (pilish-test--stdout proc '(:type "agent_start"))
                        (let ((before (length commands)))
                          ;; Even a cancelled callback already in flight must
                          ;; not probe, idle, or erase the newer activity.
                          (apply (car fallback) (cdr fallback))
                          (should (= before (length commands))))
                        (with-current-buffer chat
                          (should (eq pilish--status 'streaming))
                          (should (equal pilish--activity-phase "thinking"))
                          (should (eq pilish--aborted stop))
                          (should (equal pilish--followup-queue '("next")))
                          (should-not pilish--prompt-wait)
                          (should-not pilish--prompt-start-timer)))
                    (apply (car fallback) (cdr fallback))
                    (let ((probe (car commands)))
                      (should (equal (plist-get probe :type) "get_state"))
                      (pilish-test--stdout
                       proc (list :type "response" :command "get_state" :success t
                                  :id (plist-get probe :id)
                                  :data '(:isStreaming :false :isCompacting :false))))
                    (with-current-buffer chat
                      (should-not (eq pilish--prompt-wait wait))
                      (should-not pilish--aborted)
                      (should (eq pilish--status (if stop 'idle 'sending)))
                      (should (equal pilish--followup-queue (unless stop '("next")))))
                    (should (equal (mapcar (lambda (c) (plist-get c :message))
                                           (seq-filter (lambda (c) (equal (plist-get c :type) "prompt"))
                                                       (reverse commands)))
                                   (if stop '("consumed input") '("consumed input" "next"))))))
                (with-current-buffer input
                  (should (equal (buffer-string) "newer draft")))))))))))

(ert-deftest pilish-test-disposition-handled-stale-and-reentrant ()
  "Stale or reentrant handled acceptance leaves the newer wait, Stop and draft."
  (dolist (case '(process-replacement invalidated reentrant))
    (ert-info ((format "handled acknowledgment: %s" case))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let ((schedule (symbol-function 'run-at-time))
              replacement newer-wait fallbacks (accepted 0) (finished 0))
          (unwind-protect
              (cl-letf (((symbol-function 'run-at-time)
                         (lambda (seconds repeat function &rest args)
                           (if (eq function 'pilish--clear-sending-if-no-agent-start)
                               (progn (push (cons function args) fallbacks)
                                      'fake-prompt-start-timer)
                             (apply schedule seconds repeat function args)))))
                (with-current-buffer chat
                  (pilish--send-prompt
                   "old request"
                   (lambda ()
                     (cl-incf accepted)
                     ;; Disposition must already suppress speculative display
                     ;; when this acceptance callback runs.
                     (pilish--display-accepted-prompt "old request")
                     (when (eq case 'reentrant)
                       (pilish--send-prompt "new request")
                       (setq newer-wait pilish--prompt-wait
                             pilish--aborted t pilish--followup-queue '("new successor"))
                       (with-current-buffer input
                         (erase-buffer) (insert "reentrant draft"))))
                   nil (lambda () (cl-incf finished)))
                  ;; Restoration belonging to the old request must not clear
                  ;; the newer queue after a reentrant acceptance callback.
                  (setf (pilish--prompt-wait-restore-followups pilish--prompt-wait) t))
                (let ((request (car commands)))
                  (unless (eq case 'reentrant)
                    (with-current-buffer chat
                      (if (eq case 'process-replacement)
                          (progn
                            (setq replacement (start-process "pilish-new-owner" nil "cat"))
                            (set-process-query-on-exit-flag replacement nil)
                            (pilish--set-process replacement)
                            (process-put replacement 'pilish-chat-buffer chat)
                            (pilish--register-display-handler replacement))
                        (pilish--invalidate-prompt-start-wait))
                      (pilish--send-prompt "new request")
                      (setq newer-wait pilish--prompt-wait))
                    (pilish-test--stdout (or replacement proc) '(:type "agent_start"))
                    (with-current-buffer chat
                      (setq pilish--aborted t pilish--followup-queue '("new successor"))))
                  (with-current-buffer input (insert "newer draft"))
                  (let ((response (list :type "response" :command "prompt" :success t
                                        :id (plist-get request :id) :data '(:disposition "handled"))))
                    (pilish-test--stdout proc response response))
                  (with-current-buffer chat
                    (should (eq pilish--prompt-wait newer-wait))
                    (should (eq pilish--status (if (eq case 'reentrant) 'sending 'streaming)))
                    (should (equal pilish--activity-phase "thinking"))
                    (should pilish--aborted)
                    (should (equal pilish--followup-queue '("new successor")))
                    (should-not pilish--prompt-start-timer)
                    (should-not pilish--local-user-message)
                    (should-not pilish--local-user-message-region)
                    (should-not (string-match-p "old request" (buffer-string))))
                  (with-current-buffer input
                    (should (equal (buffer-string)
                                   (if (eq case 'reentrant) "reentrant draft" "newer draft"))))
                  (should (= accepted (if (eq case 'reentrant) 1 0)))
                  (should (= finished 0))
                  (should-not fallbacks)))
            (when (and replacement (process-live-p replacement))
              (delete-process replacement))))))))

(ert-deftest pilish-test-disposition-queued-reentrant-success-keeps-newer-wait ()
  "Queued acceptance cannot echo old text or change work started by its callback."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let (newer-wait notices)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) notices))))
        (with-current-buffer chat
          (pilish--send-prompt
           "old request"
           (lambda ()
             (pilish--display-accepted-prompt "old request")
             (pilish--send-prompt "new request")
             (setq newer-wait pilish--prompt-wait
                   pilish--aborted t pilish--followup-queue '("new successor"))
             (with-current-buffer input (insert "newer draft"))))
          (setf (pilish--prompt-wait-restore-followups pilish--prompt-wait) t))
        (let* ((request (car commands))
               (response (list :type "response" :command "prompt" :success t
                               :id (plist-get request :id) :data '(:disposition "queued"))))
          (pilish-test--stdout proc response response))
        (with-current-buffer chat
          (should (eq pilish--prompt-wait newer-wait))
          (should (eq pilish--status 'sending))
          (should (equal pilish--activity-phase "thinking"))
          (should pilish--aborted)
          (should (equal pilish--followup-queue '("new successor")))
          (should-not pilish--prompt-start-timer)
          (should-not pilish--local-user-message)
          (should-not (string-match-p "old request" (buffer-string))))
        (with-current-buffer input (should (equal (buffer-string) "newer draft")))
        (should-not notices)
        (should (equal (mapcar (lambda (c) (plist-get c :message)) (reverse commands))
                       '("old request" "new request")))))))

(ert-deftest pilish-test-disposition-queued-prompt-acknowledges-without-echo ()
  "Queued acceptance drops the FIFO head, without guessing a turn or snapshot."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let ((schedule (symbol-function 'run-at-time)) fallbacks notices)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (seconds repeat function &rest args)
                   (if (eq function 'pilish--clear-sending-if-no-agent-start)
                       (progn (push (cons function args) fallbacks)
                              'fake-prompt-start-timer)
                     (apply schedule seconds repeat function args))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) notices))))
        (with-current-buffer chat
          (pilish--push-followup "first")
          (pilish--push-followup "second")
          (pilish--process-followup-queue))
        (let ((request (car commands))
              (wait (buffer-local-value 'pilish--prompt-wait chat)))
          (should (equal (plist-get request :message) "first"))
          (with-current-buffer input (insert "newer draft"))
          (pilish-test--stdout
           proc (list :type "response" :command "prompt" :success t
                      :id (plist-get request :id) :data '(:disposition "queued")))
          (with-current-buffer chat
            (should (equal pilish--followup-queue '("second")))
            (should (equal (pilish--prompt-wait-disposition pilish--prompt-wait) "queued"))
            (should (pilish--prompt-wait-accepted pilish--prompt-wait))
            (should (equal pilish--activity-phase "queued"))
            (should (eq pilish--status 'sending))
            (should-not pilish--local-user-message)
            (should-not pilish--local-user-message-region)
            (should-not (string-match-p "first" (buffer-string))))
          (should (equal notices '("Pi: Prompt acknowledged as queued")))
          (should-not (pilish--process-queue-snapshot proc))
          (pilish-test--queued-header input 1)
          (should (= 1 (length fallbacks)))
          (should (= 1 (length commands)))
          (apply (caar fallbacks) (cdar fallbacks))
          (let ((probe (car commands)))
            (should (equal (plist-get probe :type) "get_state"))
            (pilish-test--stdout
             proc (list :type "response" :command "get_state" :success t
                        :id (plist-get probe :id)
                        :data '(:isStreaming :false :isCompacting :false
                                :pendingMessageCount 1))))
          (with-current-buffer chat
            (should (eq pilish--prompt-wait wait))
            (should (equal pilish--activity-phase "queued"))
            (should (equal pilish--followup-queue '("second")))
            (should-not (string-match-p "first" (buffer-string))))
          (should (= 2 (length fallbacks)))
          (pilish-test--stdout proc '(:type "agent_start"))
          (pilish-test--stdout
           proc '(:type "message_start" :message
                   (:role "user" :timestamp 1704067200000
                    :content [(:type "text" :text "first")])))
          (with-current-buffer chat
            (should-not pilish--prompt-wait)
            (should-not pilish--prompt-start-timer)
            (should (eq pilish--status 'streaming))
            (should (equal pilish--activity-phase "thinking"))
            (should (equal pilish--followup-queue '("second")))
            (should (= 1 (pilish-test--count-matches "first" (buffer-string)))))
          (with-current-buffer input (should (equal (buffer-string) "newer draft")))
          (should (= 2 (length commands))))))))

(ert-deftest pilish-test-disposition-queued-prompt-probe-preserves-pending-fifo ()
  "Pending backend work blocks no-turn completion; gone work releases the FIFO."
  (dolist (gone-state '((:isStreaming :false :isCompacting :false :pendingMessageCount 0)
                        (:isStreaming :false :isCompacting :false)))
    (ert-info ((format "queue removed, state=%S" gone-state))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let ((schedule (symbol-function 'run-at-time)) fallbacks notices)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (seconds repeat function &rest args)
                       (if (eq function 'pilish--clear-sending-if-no-agent-start)
                           (progn (push (cons function args) fallbacks)
                                  'fake-prompt-start-timer)
                         (apply schedule seconds repeat function args))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices))))
            (with-current-buffer chat
              (pilish--push-followup "first")
              (pilish--push-followup "second")
              (pilish--process-followup-queue))
            (let ((request (car commands))
                  (wait (buffer-local-value 'pilish--prompt-wait chat)))
              (pilish-test--stdout
               proc (list :type "response" :command "prompt" :success t
                          :id (plist-get request :id) :data '(:disposition "queued")))
              (with-current-buffer input (insert "newer draft"))
              (apply (caar fallbacks) (cdar fallbacks))
              (let ((probe (car commands)))
                (should (equal (plist-get probe :type) "get_state"))
                (pilish-test--stdout
                 proc (list :type "response" :command "get_state" :success t
                            :id (plist-get probe :id)
                            :data '(:isStreaming :false :isCompacting :false
                                    :pendingMessageCount 1))))
              (with-current-buffer chat
                (should (eq pilish--prompt-wait wait))
                (should (eq pilish--status 'sending))
                (should (equal pilish--activity-phase "queued"))
                (should (equal pilish--followup-queue '("second")))
                (should-not pilish--local-user-message)
                (should-not (string-match-p "first" (buffer-string))))
              (should (= 2 (length fallbacks)))
              (should (= 2 (length commands)))
              (pilish-test--queued-header input 1)
              (should-not (pilish--process-queue-snapshot proc))
              (apply (caar fallbacks) (cdar fallbacks))
              (let ((probe (car commands)))
                (should (equal (plist-get probe :type) "get_state"))
                (pilish-test--stdout
                 proc (list :type "response" :command "get_state" :success t
                            :id (plist-get probe :id) :data gone-state)))
              (with-current-buffer chat
                (should-not (eq pilish--prompt-wait wait))
                (should (eq pilish--status 'sending))
                (should (equal pilish--followup-queue '("second")))
                (should-not pilish--local-user-message)
                (should-not (string-match-p "first\\|second" (buffer-string))))
              (should (equal (mapcar (lambda (c) (plist-get c :message))
                                     (seq-filter (lambda (c) (equal (plist-get c :type) "prompt"))
                                                 (reverse commands)))
                             '("first" "second")))
              (should (equal notices '("Pi: Prompt acknowledged as queued")))
              (with-current-buffer input
                (should (equal (buffer-string) "newer draft"))))))))))

(ert-deftest pilish-test-disposition-queued-prompt-preserves-observed-work ()
  "A historical queued acknowledgment cannot regress observed run/echo activity."
  (dolist (case '((streaming ((:type "agent_start")) streaming "thinking")
                  (ended ((:type "agent_start") (:type "agent_end" :messages []))
                         sending "thinking")
                  (manual ((:type "agent_start") (:type "agent_end" :messages [])
                           (:type "compaction_start" :reason "manual"))
                          compacting "compact")
                  (unstarted-manual ((:type "compaction_start" :reason "manual"))
                                    compacting "compact")
                  (retry ((:type "agent_start") (:type "agent_end" :messages [])
                          (:type "auto_retry_start" :attempt 1 :maxAttempts 3
                           :delayMs 1000 :errorMessage "overloaded"))
                         sending "thinking")
                  (settled ((:type "agent_start") (:type "agent_end" :messages [])
                            (:type "agent_settled")) idle "idle")
                  (echo-only nil sending "thinking")))
    (ert-info ((format "before queued acknowledgment: %s" (car case)))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let ((schedule (symbol-function 'run-at-time)) fallbacks notices)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (seconds repeat function &rest args)
                       (if (eq function 'pilish--clear-sending-if-no-agent-start)
                           (progn (push (cons function args) fallbacks)
                                  'fake-prompt-start-timer)
                         (apply schedule seconds repeat function args))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices))))
            (with-current-buffer input (insert "first") (pilish-send))
            (let ((request (car commands)))
              (dolist (event (nth 1 case)) (pilish-test--stdout proc event))
              (when (memq (car case) '(settled echo-only))
                (pilish-test--stdout
                 proc '(:type "message_start" :message
                         (:role "user" :timestamp 1704067200000
                          :content [(:type "text" :text "first")]))))
              (with-current-buffer input (insert "newer draft"))
              (with-current-buffer chat
                (unless (eq (car case) 'settled)
                  (pilish--push-followup "second")
                  (setq pilish--aborted t))
                (setq notices nil)
                (let ((before (buffer-string)))
                  (pilish-test--stdout
                   proc (list :type "response" :command "prompt" :success t
                              :id (plist-get request :id) :data '(:disposition "queued")))
                  (should (equal (buffer-string) before)))
                (should (eq pilish--status (nth 2 case)))
                (should (equal pilish--activity-phase (nth 3 case)))
                (should (eq pilish--aborted (not (eq (car case) 'settled))))
                (should (equal pilish--followup-queue
                               (unless (eq (car case) 'settled) '("second"))))
                (should-not pilish--local-user-message)
                (if (memq (car case) '(echo-only unstarted-manual))
                    (progn (should pilish--prompt-wait)
                           (should (= 1 (length fallbacks))))
                  (should-not pilish--prompt-wait)
                  (should-not fallbacks)))
              (should (equal notices '("Pi: Prompt acknowledged as queued")))
              (should (= 1 (length commands)))
              (should-not (pilish--process-queue-snapshot proc))
              (with-current-buffer input (should (equal (buffer-string) "newer draft"))))))))))

(ert-deftest pilish-test-disposition-queued-stop-keeps-empty-snapshot-and-local-queue ()
  "Stop followed by queued prompt/steer acceptance cannot resurrect discarded work."
  (dolist (command '(prompt steer))
    (pilish-test-with-rpc-session (chat input proc commands)
      (let ((schedule (symbol-function 'run-at-time)) fallbacks notices)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (seconds repeat function &rest args)
                     (if (eq function 'pilish--clear-sending-if-no-agent-start)
                         (progn (push (cons function args) fallbacks)
                                'fake-prompt-start-timer)
                       (apply schedule seconds repeat function args))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) notices))))
          (if (eq command 'prompt)
              (with-current-buffer chat
                (pilish--push-followup "first")
                (pilish--push-followup "second")
                (pilish--process-followup-queue))
            (pilish-test--stdout proc '(:type "agent_start"))
            (with-current-buffer input (insert "first") (pilish-queue-steering))
            (with-current-buffer chat (pilish--push-followup "second")))
          (let ((request (car commands)))
            (pilish-test--stdout
             proc '(:type "queue_update" :steering ["first"] :followUp []))
            (pilish-test--queued-header input (if (eq command 'prompt) 3 2))
            (with-current-buffer input (insert "newer draft") (pilish-abort))
            (pilish-test--stdout proc '(:type "queue_update" :steering [] :followUp []))
            (pilish-test--queued-header input 0)
            (setq notices nil)
            (pilish-test--stdout
             proc (list :type "response" :command (symbol-name command) :success t
                        :id (plist-get request :id) :data '(:disposition "queued")))
            (should (equal notices
                           (list (if (eq command 'prompt)
                                     "Pi: Prompt acknowledged as queued"
                                   "Pi: Steering acknowledged as queued"))))
            (pilish-test--queued-header input 0)
            (should (equal (pilish--process-queue-snapshot proc)
                           '(:type "queue_update" :steering [] :followUp [])))
            (with-current-buffer chat
              (should-not pilish--followup-queue)
              (should-not pilish--local-user-message)
              (should-not (string-match-p "first" (buffer-string)))
              (should pilish--aborted))
            (with-current-buffer input (should (equal (buffer-string) "newer draft")))
            (should (equal (mapcar (lambda (c) (plist-get c :type)) (reverse commands))
                           (list (symbol-name command) "clear_queue" "abort")))
            (when (eq command 'prompt)
              (apply (caar fallbacks) (cdar fallbacks))
              (let ((probe (car commands)))
                (should (equal (plist-get probe :type) "get_state"))
                (pilish-test--stdout
                 proc (list :type "response" :command "get_state" :success t
                            :id (plist-get probe :id)
                            :data '(:isStreaming :false :isCompacting :false
                                    :pendingMessageCount 0))))
              (with-current-buffer chat
                (should (eq pilish--status 'idle))
                (should-not pilish--aborted)
                (should-not pilish--prompt-wait)
                (should-not pilish--followup-queue))
              (should (= 4 (length commands))))))))))

(ert-deftest pilish-test-prompt-image-no-turn-success-retracts-local-echo ()
  "An extension-handled image prompt leaves no phantom user turn."
  (pilish-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (pilish-test--write-prompt-image
                  (expand-file-name "handled.png" dir) 'png))
           rpc-callback state-callback fallback-callback fallback-args)
      (with-current-buffer chat-buf
        (setq pilish--process 'image-process))
      (with-current-buffer input-buf
        (pilish-test--attach-image path)
        (insert "Handle this without a turn"))
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'pilish--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("prompt" (setq rpc-callback callback))
                     ("get_state" (setq state-callback callback)))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat function &rest args)
                   (if (eq function
                           'pilish--clear-sending-if-no-agent-start)
                       (setq fallback-callback function
                             fallback-args args)
                     'fake-timer)
                   'fake-prompt-start-timer))
                ((symbol-function 'display-images-p) (lambda (&rest _) nil))
                ((symbol-function 'message) #'ignore))
        (with-current-buffer input-buf
          (pilish-send))
        (funcall rpc-callback '(:success t))
        (with-current-buffer chat-buf
          (should pilish--local-user-message)
          (should (string-match-p "Handle this without a turn"
                                  (buffer-string)))
          (narrow-to-region (1+ (point-min)) (point-max)))
        (apply fallback-callback fallback-args)
        (should (functionp state-callback))
        (funcall state-callback
                 '(:success t
                   :data (:isStreaming :false :isCompacting :false))))
      (with-current-buffer chat-buf
        (widen)
        (should (eq pilish--status 'idle))
        (should-not pilish--local-user-message)
        (should-not pilish--local-user-message-region)
        (should-not (string-match-p "Handle this without a turn"
                                    (buffer-string)))))))

(ert-deftest pilish-test-no-turn-fallback-keeps-server-active-prompt ()
  "A delayed agent_start must not be mistaken for an extension-handled prompt."
  (let ((fake-proc (start-process "test-active-prompt" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (setq pilish--process fake-proc
                pilish--status 'sending
                pilish--local-user-message "slow prompt"
                pilish--followup-queue '("wait behind it"))
          (setq pilish--local-user-message-region
                (pilish--display-user-message
                 "slow prompt" (current-time) nil t))
          (let ((generation (pilish--begin-prompt-start-wait)))
            (setf (pilish--prompt-wait-accepted generation) t)
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (_process command callback)
                         (should (equal (plist-get command :type) "get_state"))
                         (funcall callback
                                  '(:success t
                                    :data (:isStreaming t
                                           :isCompacting :false)))))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _) 'fake-prompt-start-timer)))
              (pilish--clear-sending-if-no-agent-start
               (current-buffer) generation
               #'pilish--handle-no-turn-local-prompt))
            (should (pilish--prompt-start-current-p generation))
            (should (equal pilish--local-user-message "slow prompt"))
            (should pilish--local-user-message-region)
            (should (equal pilish--followup-queue '("wait behind it")))
            (should (string-match-p "slow prompt" (buffer-string)))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest pilish-test-no-turn-prompt-retracts-local-echo ()
  "An extension-handled prompt leaves no speculative user turn behind."
  (let ((chat-buf (generate-new-buffer "*pi-no-turn-retract-chat*"))
        (input-buf (generate-new-buffer "*pi-no-turn-retract-input*"))
        (fake-proc (start-process "test-no-turn-retract" nil "cat"))
        prompt-callback state-callback fallback-callback fallback-args)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process fake-proc
                  pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "Handle this without a turn"))
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_process command callback)
                       (pcase (plist-get command :type)
                         ("prompt" (setq prompt-callback callback))
                         ("get_state" (setq state-callback callback)))))
                    ((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (if (eq function
                               'pilish--clear-sending-if-no-agent-start)
                           (setq fallback-callback function
                                 fallback-args args)
                         'fake-timer)
                       'fake-prompt-start-timer))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer input-buf
              (pilish-send))
            (funcall prompt-callback '(:success t))
            (with-current-buffer chat-buf
              (should (equal pilish--local-user-message
                             "Handle this without a turn"))
              (narrow-to-region (1+ (point-min)) (point-max)))
            (apply fallback-callback fallback-args)
            (funcall state-callback
                     '(:success t
                       :data (:isStreaming :false :isCompacting :false))))
          (with-current-buffer chat-buf
            (widen)
            (should (eq pilish--status 'idle))
            (should-not pilish--local-user-message)
            (should-not pilish--local-user-message-region)
            (should-not (string-match-p "Handle this without a turn"
                                        (buffer-string)))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-prompt-marks-sending-until-preflight-fails ()
  "pilish--send-prompt closes the local pre-agent_start idle gap."
  (let* ((rpc-callback nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (setq pilish--status 'idle
                pilish--activity-phase "idle")
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _msg cb) (setq rpc-callback cb)))
                    ((symbol-function 'message) #'ignore))
            (pilish--send-prompt "hello")
            (should (eq pilish--status 'sending))
            (should (equal pilish--activity-phase "thinking"))
            (should (functionp rpc-callback))
            (funcall rpc-callback '(:success :false :error "preflight failed"))
            (should (eq pilish--status 'idle))
            (should (equal pilish--activity-phase "idle"))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-rejected-prompts-restore-input ()
  "Rejected regular and slash requests restore drafts without ghost transcript turns."
  (dolist (text '("this send will fail" "/unknown command"))
    (pilish-test-with-rpc-session (chat input proc commands)
      (with-current-buffer input (insert text) (pilish-send))
      (with-current-buffer chat
        (let (shown-message)
          (should-not (string-match-p (regexp-quote text) (buffer-string)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (pilish--dispatch-response
             proc (list :type "response" :id (plist-get (car commands) :id)
                        :command "prompt" :success :false :error "preflight failed")))
          (should (equal shown-message "Pi: Send failed: preflight failed"))
          (should (eq pilish--status 'idle))
          (should (equal pilish--activity-phase "idle"))
          (should-not pilish--prompt-wait)
          (should-not pilish--local-user-message)
          (should-not (string-match-p (regexp-quote text) (buffer-string)))))
      (with-current-buffer input (should (equal (buffer-string) text))))))

(ert-deftest pilish-test-normal-send-without-process-restores-input ()
  "Direct sends without a process do not create ghost chat text."
  (let ((chat-buf (get-buffer-create "*pilish-test-send-no-process-chat*"))
        (input-buf (get-buffer-create "*pilish-test-send-no-process-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--activity-phase "idle"
                  pilish--process nil
                  pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "no process prompt")
            (cl-letf (((symbol-function 'message) #'ignore))
              (pilish-send))
            (should (equal (buffer-string) "no process prompt")))
          (with-current-buffer chat-buf
            (should (eq pilish--status 'idle))
            (should (null pilish--local-user-message))
            (should-not (string-match-p "no process prompt" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-restore-followups-prepends-existing-draft ()
  "Recovered queued text stays before any newer input draft."
  (let ((chat-buf (get-buffer-create "*pilish-test-restore-before-draft-chat*"))
        (input-buf (get-buffer-create "*pilish-test-restore-before-draft-input*")))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "newer draft"))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--input-buffer input-buf
                  pilish--followup-queue '("second" "first"))
            (pilish--restore-followup-queue-to-input)
            (should (null pilish--followup-queue)))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "first\n\nsecond\n\nnewer draft"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-queued-followup-send-failure-restores-queue-to-input ()
  "Rejected queued follow-ups become visible input instead of hidden work."
  (let ((chat-buf (get-buffer-create "*pilish-test-queued-fail-chat*"))
        (input-buf (get-buffer-create "*pilish-test-queued-fail-input*"))
        (rpc-callback nil)
        (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--activity-phase "idle"
                  pilish--input-buffer input-buf
                  pilish--followup-queue '("newer queued item" "queued send will fail")))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _msg cb) (setq rpc-callback cb)))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (pilish--process-followup-queue)
              (should (eq pilish--status 'sending))
              (should (equal pilish--followup-queue
                             '("newer queued item" "queued send will fail")))
              (should-not (string-match-p "queued send will fail" (buffer-string)))
              (funcall rpc-callback '(:success :false :error "preflight failed"))
              (should (eq pilish--status 'idle))
              (should (null pilish--followup-queue))
              (should-not (string-match-p "queued send will fail" (buffer-string)))))
          (with-current-buffer input-buf
            (should (equal (buffer-string)
                           "queued send will fail\n\nnewer queued item"))))
      (delete-process fake-proc)
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-send-prompt-success-without-agent-start-returns-idle ()
  "Successful no-turn slash commands do not leave the frontend sending."
  (let* ((rpc-callback nil)
         (fallback-callback nil)
         (fallback-args nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (setq pilish--status 'idle
                pilish--activity-phase "idle"
                pilish--process fake-proc)
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc command cb)
                       (pcase (plist-get command :type)
                         ("prompt" (setq rpc-callback cb))
                         ("get_state"
                          (funcall cb
                                   '(:success t
                                     :data (:isStreaming :false
                                            :isCompacting :false)))))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (setq fallback-callback fn
                             fallback-args args)
                       'fake-prompt-start-timer)))
            (pilish--send-prompt "/test-noop")
            (should (eq pilish--status 'sending))
            (funcall rpc-callback '(:success t))
            (should (functionp fallback-callback))
            (apply fallback-callback fallback-args)
            (should (eq pilish--status 'idle))
            (should (equal pilish--activity-phase "idle"))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-no-turn-prompt-fallback-drains-queued-followup ()
  "Successful no-turn commands release follow-ups queued while sending."
  (let* ((rpc-callbacks nil)
         (prompt-fallback-callback nil)
         (prompt-fallback-args nil)
         (sent-messages nil)
         (chat-buf (get-buffer-create "*pilish-test-no-turn-drain*"))
         (input-buf (get-buffer-create "*pilish-test-no-turn-drain-input*"))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--activity-phase "idle"
                  pilish--process fake-proc
                  pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("prompt"
                          (push (plist-get cmd :message) sent-messages)
                          (push cb rpc-callbacks))
                         ("get_state"
                          (funcall cb
                                   '(:success t
                                     :data (:isStreaming :false
                                            :isCompacting :false)))))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (cond
                        ((eq fn 'pilish--clear-sending-if-no-agent-start)
                         (setq prompt-fallback-callback fn
                               prompt-fallback-args args)
                         'fake-prompt-start-timer)
                        (t 'fake-timer))))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (pilish--prepare-and-send "/test-noop"))
            (with-current-buffer input-buf
              (insert "follow-up after noop")
              (pilish-send))
            (with-current-buffer chat-buf
              (should (eq pilish--status 'sending))
              (should (equal pilish--followup-queue
                             '("follow-up after noop"))))
            (funcall (car rpc-callbacks) '(:success t))
            (should (functionp prompt-fallback-callback))
            (apply prompt-fallback-callback prompt-fallback-args)
            (should (equal (reverse sent-messages)
                           '("/test-noop" "follow-up after noop")))))
      (delete-process fake-proc)
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-stale-prompt-start-fallback-does-not-clear-newer-send ()
  "A stale no-turn fallback timer cannot clear a newer sending prompt."
  (let* ((rpc-callbacks nil)
         (fallbacks nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (setq pilish--status 'idle
                pilish--activity-phase "idle")
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _msg cb) (push cb rpc-callbacks)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (push (cons fn args) fallbacks)
                       'fake-prompt-start-timer)))
            (pilish--send-prompt "/first-noop")
            (funcall (car rpc-callbacks) '(:success t))
            (let ((first-fallback (car fallbacks)))
              (pilish--send-prompt "/second-command")
              (should (eq pilish--status 'sending))
              (apply (car first-fallback) (cdr first-fallback))
              (should (eq pilish--status 'sending))
              (should (equal pilish--activity-phase "thinking")))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-agent-start-makes-late-success-skip-fallback ()
  "Acceptance after an observed start cannot schedule a no-turn idle reset."
  (let ((rpc-callback nil)
        (fallback-scheduled nil)
        (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (setq pilish--status 'idle
                pilish--activity-phase "idle")
          (cl-letf (((symbol-function 'pilish--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pilish--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'pilish--rpc-async)
                     (lambda (_proc _msg cb) (setq rpc-callback cb)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _)
                       (setq fallback-scheduled t)
                       'fake-prompt-start-timer)))
            (pilish--send-prompt "/starts-fast")
            (pilish--handle-display-event '(:type "agent_start"))
            (funcall rpc-callback '(:success t))
            (should-not fallback-scheduled)
            (should (eq pilish--status 'streaming))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-agent-start-cancels-prompt-start-fallback ()
  "A real agent_start cancels the no-turn prompt fallback timer."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((cancelled nil))
      (setq pilish--status 'sending
            pilish--prompt-start-timer 'fake-timer)
      (cl-letf (((symbol-function 'timerp) (lambda (timer) (eq timer 'fake-timer)))
                ((symbol-function 'cancel-timer) (lambda (_timer) (setq cancelled t))))
        (pilish--handle-display-event '(:type "agent_start")))
      (should cancelled)
      (should (null pilish--prompt-start-timer))
      (should (eq pilish--status 'streaming)))))

(ert-deftest pilish-test-format-session-stats ()
  "Format session stats returns readable string with cache details."
  (let ((stats '(:tokens (:input 50000 :output 10000 :total 60000
                         :cacheRead 123000 :cacheWrite 4567)
                 :cost 0.45
                 :userMessages 5
                 :toolCalls 12)))
    (let ((result (pilish--format-session-stats stats)))
      (should (string-match-p "50,000" result))
      (should (string-match-p "10,000" result))
      (should (string-match-p "60,000" result))
      (should (string-match-p "123,000" result))
      (should (string-match-p "4,567" result))
      (should (string-match-p "\\$0.45" result))
      (should (string-match-p "Messages: 5" result))
      (should (string-match-p "Tools: 12" result)))))

(ert-deftest pilish-test-header-line-shows-model ()
  "Header line displays current model."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4"))
    (let ((header (pilish--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pilish-test-header-line-shows-thinking ()
  "Header line displays thinking level."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4" :thinking-level "high"))
    (let ((header (pilish--header-line-string)))
      (should (string-match-p "high" header)))))

(ert-deftest pilish-test-header-line-shows-activity-phase ()
  "Header line shows the current activity phase label."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4" :thinking-level "high")
          pilish--activity-phase "thinking")
    (let ((header (pilish--header-line-string)))
      (should (string-match-p "thinking" header)))))

(ert-deftest pilish-test-header-line-shows-idle ()
  "Header line shows idle activity phase with fixed-width padding."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4" :thinking-level "high")
          pilish--activity-phase "idle")
    (let ((header (substring-no-properties (pilish--header-line-string))))
      (should (string-match-p "idle    " header)))))

(ert-deftest pilish-test-header-line-phase-is-padded ()
  "Header line activity phase slot is always 8 characters wide."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4" :thinking-level "high")
          pilish--activity-phase "running")
    (let* ((header (substring-no-properties (pilish--header-line-string)))
           (pos (string-match "running" header)))
      (should pos)
      (should (equal (substring header pos (+ pos 8)) "running ")))))

(ert-deftest pilish-test-header-line-shows-thinking-activity-phase ()
  "Header line shows semantic activity label during streaming."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4")
          pilish--activity-phase "thinking")
    (let ((header (pilish--header-line-string)))
      (should (string-match-p "thinking" header)))))

(ert-deftest pilish-test-quit-prompts-even-when-process-noquery ()
  "Explicit quit still asks before killing a live noquery process."
  (let ((chat-buf (generate-new-buffer "*pilish-test-quit-chat*"))
        (input-buf (generate-new-buffer "*pilish-test-quit-input*"))
        (proc (start-process "test-quit-noquery" nil "cat"))
        (asked nil))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag proc nil)
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc)
            (pilish--set-input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (pilish--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt)
                       (setq asked t)
                       nil)))
            (with-current-buffer chat-buf
              (should-error (pilish-quit) :type 'user-error)))
          (should asked)
          (should (buffer-live-p chat-buf))
          (should (buffer-live-p input-buf))
          (should (process-live-p proc)))
      (when (process-live-p proc)
        (delete-process proc))
      (pilish-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest pilish-test-process-exit-resets-busy-chat-and-restores-queue ()
  "Process exit leaves the frontend idle, visible, and restores queued work."
  (let ((chat-buf (get-buffer-create "*pilish-test-process-exit-chat*"))
        (input-buf (get-buffer-create "*pilish-test-process-exit-input*"))
        (stderr-buf (generate-new-buffer
                     " *pilish-test-process-exit-stderr*"))
        (proc (start-process "test-process-exit" nil "sh" "-c" "exit 1")))
    (unwind-protect
        (progn
          (set-process-sentinel proc nil)
          (set-process-query-on-exit-flag proc nil)
          (should (pilish-test-wait-for-process-exit proc))
          (with-current-buffer stderr-buf
            (insert "ECOMPROMISED: lock was compromised\n"
                    (make-string 5000 ?x)
                    "\nEND provider stack\n"))
          (process-put proc 'pilish-stderr-buf stderr-buf)
          (process-put proc 'pilish-chat-buffer chat-buf)
          (pilish--register-display-handler proc)
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming
                  pilish--activity-phase "replying"
                  pilish--process proc
                  pilish--input-buffer input-buf
                  pilish--local-user-message "pending echo"
                  pilish--followup-queue '("queued after crash")
                  pilish--aborted t
                  pilish--prompt-wait (pilish--make-prompt-wait :process proc))
            (pilish--handle-process-exit proc
                                                  "exited abnormally with code 1\n")
            (should (eq pilish--status 'idle))
            (should (equal pilish--activity-phase "idle"))
            (should (null pilish--process))
            (should (null pilish--local-user-message))
            (should (null pilish--followup-queue))
            (should-not pilish--prompt-wait)
            (should-not pilish--aborted)
            (should (equal (plist-get pilish--state :last-error)
                           "Process exited: exited abnormally with code 1"))
            (let ((chat-text (buffer-string)))
              (should (string-match-p "pi process exited" chat-text))
              (should (string-match-p
                       "Process exited: exited abnormally with code 1"
                       chat-text))
              (should (string-match-p "Exit code: 1" chat-text))
              (should (string-match-p "ECOMPROMISED" chat-text))
              (should (string-match-p "stderr truncated" chat-text))
              (should (string-match-p "END provider stack" chat-text))
              (should (< (length chat-text) 4500))))
          (should-not (buffer-live-p stderr-buf))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "queued after crash"))))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-process-exit-without-stderr-is-visible ()
  "A current process exit remains visible when no stderr was captured."
  (let ((chat-buf (get-buffer-create
                   "*pilish-test-process-exit-no-stderr-chat*"))
        (proc (start-process "test-process-exit-no-stderr" nil
                             "sh" "-c" "exit 2")))
    (unwind-protect
        (progn
          (set-process-sentinel proc nil)
          (set-process-query-on-exit-flag proc nil)
          (should (pilish-test-wait-for-process-exit proc))
          (process-put proc 'pilish-chat-buffer chat-buf)
          (pilish--register-display-handler proc)
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--process proc)
            (pilish--handle-process-exit proc
                                                  "exited abnormally with code 2\n")
            (let ((chat-text (buffer-string)))
              (should (string-match-p "pi process exited" chat-text))
              (should (string-match-p "Process exited" chat-text))
              (should (string-match-p "Exit code: 2" chat-text))
              (should-not (string-match-p "stderr:" chat-text)))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-process-exit-cleans-up-when-display-errors ()
  "Current process cleanup and queue restoration survive a display error."
  (let ((chat-buf (generate-new-buffer
                   "*pilish-test-process-exit-display-error-chat*"))
        (input-buf (generate-new-buffer
                    "*pilish-test-process-exit-display-error-input*"))
        (proc (start-process "test-process-exit-display-error" nil "cat"))
        (mode-line-updates 0))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag proc nil)
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'streaming
                  pilish--activity-phase "replying"
                  pilish--process proc
                  pilish--input-buffer input-buf
                  pilish--local-user-message "pending echo"
                  pilish--pre-compaction-status 'streaming
                  pilish--followup-queue '("restore after error")
                  pilish--prompt-wait (pilish--make-prompt-wait :process proc))
            (cl-letf (((symbol-function
                        'pilish--display-process-exit-error)
                       (lambda (&rest _)
                         (error "display failed")))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all)
                         (setq mode-line-updates (1+ mode-line-updates)))))
              (should-error
               (pilish--mark-process-exited
                proc '(:success :false
                       :error "Process exited: display failure"
                       :exitCode 1))
               :type 'error)
              (should (eq pilish--status 'idle))
              (should (equal pilish--activity-phase "idle"))
              (should (null pilish--process))
              (should (null pilish--local-user-message))
              (should (null pilish--pre-compaction-status))
              (should (null pilish--followup-queue))
              (should-not pilish--prompt-wait)
              (should (>= mode-line-updates 2))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "restore after error"))))
      (when (process-live-p proc)
        (delete-process proc))
      (pilish-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest pilish-test-stale-process-exit-is-not-rendered ()
  "A registered process that is not current must not show a crash block."
  (let ((current-proc (start-process "test-current-process" nil "cat"))
        (stale-proc (start-process "test-stale-process" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (setq pilish--process current-proc
                pilish--status 'streaming
                pilish--activity-phase "replying"
                pilish--state '(:last-error "keep me")
                pilish--local-user-message "in flight"
                pilish--followup-queue '("still queued"))
          (process-put stale-proc 'pilish-chat-buffer
                       (current-buffer))
          (pilish--register-display-handler stale-proc)
          (funcall (process-get stale-proc 'pilish-exit-handler)
                   '(:success :false
                     :error "Process exited: stale transition failed"
                     :exitCode 1
                     :stderr "ECOMPROMISED: stale process"))
          (should (eq pilish--process current-proc))
          (should (eq pilish--status 'streaming))
          (should (equal pilish--activity-phase "replying"))
          (should (equal (plist-get pilish--state :last-error)
                         "keep me"))
          (should (equal pilish--local-user-message "in flight"))
          (should (equal pilish--followup-queue '("still queued")))
          (should-not (string-match-p "pi process exited" (buffer-string))))
      (pilish--unregister-display-handler stale-proc)
      (when (process-live-p current-proc)
        (delete-process current-proc))
      (when (process-live-p stale-proc)
        (delete-process stale-proc)))))

(ert-deftest pilish-test-process-exit-restores-direct-pending-prompt ()
  "Process exit restores a direct prompt whose RPC preflight never completed."
  (let ((chat-buf (get-buffer-create "*pilish-test-process-exit-direct-chat*"))
        (input-buf (get-buffer-create "*pilish-test-process-exit-direct-input*"))
        (proc (start-process "test-process-exit-direct" nil "cat")))
    (unwind-protect
        (progn
          (set-process-sentinel proc nil)
          (set-process-query-on-exit-flag proc nil)
          (process-put proc 'pilish-chat-buffer chat-buf)
          (pilish--register-display-handler proc)
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--status 'idle
                  pilish--process proc
                  pilish--input-buffer input-buf))
          (with-current-buffer input-buf
            (pilish-input-mode)
            (setq pilish--chat-buffer chat-buf)
            (insert "please survive the crash")
            (pilish-send)
            (should (string-empty-p (buffer-string))))
          (with-current-buffer chat-buf
            (should (eq pilish--status 'sending))
            (should-not (string-match-p "please survive the crash" (buffer-string)))
            (pilish--handle-process-exit proc "exited abnormally with code 1\n")
            (should (eq pilish--status 'idle))
            (should (null pilish--process))
            (should (string-match-p "Process exited"
                                    (plist-get pilish--state :last-error))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "please survive the crash"))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pilish-test-abort-send-resets-unstarted-activity-phase ()
  "Failed unstarted submission resets activity and status in its chat buffer."
  (let ((chat-buf (generate-new-buffer "*pilish-chat:test-abort-send/*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--activity-phase "thinking"
                  pilish--status 'sending)
            (pilish--begin-prompt-start-wait))
          ;; Simulate callback/sentinel context by calling from other buffer
          (with-temp-buffer
            (pilish--abort-send chat-buf))
          (with-current-buffer chat-buf
            (should (equal pilish--activity-phase "idle"))
            (should (eq pilish--status 'idle))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pilish-test-working-message-in-header ()
  "Header line includes transient working message when set."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4")
          pilish--working-message "📖 Skimming…")
    (let ((header (substring-no-properties (pilish--header-line-string))))
      (should (string-match-p "Skimming" header)))))

(ert-deftest pilish-test-header-no-pipes-when-minimal ()
  "Header has no pipe separators when only identity group is present."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4")
          pilish--cached-stats nil
          pilish--session-name nil
          pilish--extension-status nil
          pilish--working-message nil)
    (let ((header (substring-no-properties (pilish--header-line-string))))
      (should-not (string-match-p "│" header)))))

(ert-deftest pilish-test-header-pipes-collapse-correctly ()
  "Header renders only needed pipes when stats and context groups are set."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model (:name "claude-sonnet-4" :contextWindow 200000))
          pilish--cached-stats '(:cost 0.05
                                   :contextUsage (:tokens 150 :contextWindow 200000 :percent 0.075))
          pilish--session-name "My Session"
          pilish--extension-status nil
          pilish--working-message nil)
    (let ((header (substring-no-properties (pilish--header-line-string)))
          (count 0)
          (start 0))
      (while (string-match "│" header start)
        (setq count (1+ count)
              start (match-end 0)))
      (should (= count 2)))))

(ert-deftest pilish-test-header-all-groups-present ()
  "Header shows three group separators when all groups have content."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model (:name "claude-sonnet-4" :contextWindow 200000))
          pilish--cached-stats '(:cost 0.05
                                   :contextUsage (:tokens 150 :contextWindow 200000 :percent 0.075))
          pilish--session-name "My Session"
          pilish--extension-status '(("ext" . "Git: synced"))
          pilish--working-message "📖 Skimming…")
    (let ((header (substring-no-properties (pilish--header-line-string)))
          (count 0)
          (start 0))
      (while (string-match "│" header start)
        (setq count (1+ count)
              start (match-end 0)))
      (should (= count 3))
      (should (string-match-p "My Session" header))
      (should (string-match-p "Git: synced · 📖 Skimming…" header)))))

(ert-deftest pilish-test-header-extension-group-escapes-percent-signs ()
  "Extension header text escapes percent signs for header-line display."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model (:name "gpt-5.4" :contextWindow 200000))
          pilish--extension-status '(("sub-status:usage" . "5h 4% · Week 3% · degraded"))
          pilish--working-message "refresh 50%")
    (let ((header (substring-no-properties (pilish--header-line-string))))
      (should (string-match-p "5h 4%% · Week 3%% · degraded" header))
      (should (string-match-p "refresh 50%%" header)))))

(ert-deftest pilish-test-header-session-name-in-context-group ()
  "Context group shows session name when set, collapses when nil."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model "claude-sonnet-4")
          pilish--session-name "Refactor auth")
    (let ((header (substring-no-properties (pilish--header-line-string))))
      (should (string-match-p "Refactor auth" header))
      (should (string-match-p "│" header)))
    (setq pilish--session-name nil)
    (let ((header (substring-no-properties (pilish--header-line-string))))
      (should-not (string-match-p "│" header)))))

(ert-deftest pilish-test-format-tokens-compact ()
  "Tokens formatted compactly."
  (should (equal "500" (pilish--format-tokens-compact 500)))
  (should (equal "5k" (pilish--format-tokens-compact 5000)))
  (should (equal "50k" (pilish--format-tokens-compact 50000)))
  (should (equal "1.2M" (pilish--format-tokens-compact 1200000))))

(ert-deftest pilish-test-input-mode-has-header-line ()
  "Input mode sets up header-line-format."
  (with-temp-buffer
    (pilish-input-mode)
    (should header-line-format)))

(ert-deftest pilish-test-header-line-handles-model-plist ()
  "Header line handles model as plist with :name."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model (:name "claude-sonnet-4" :id "model-123")))
    (let ((header (pilish--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pilish-test-menu-model-description-buffer-local ()
  "Menu model description uses buffer-local model."
  (let ((buf-a (generate-new-buffer "*pilish-chat:model-a*"))
        (buf-b (generate-new-buffer "*pilish-chat:model-b*")))
    (unwind-protect
        (let (desc-a desc-b)
          (with-current-buffer buf-a
            (pilish-chat-mode)
            (setq pilish--state '(:model (:name "Alpha")))
            (setq desc-a (pilish--menu-model-description)))
          (with-current-buffer buf-b
            (pilish-chat-mode)
            (setq pilish--state '(:model (:name "Beta")))
            (setq desc-b (pilish--menu-model-description)))
          (should (equal (list desc-a desc-b)
                         '("Model: Alpha" "Model: Beta"))))
      (mapc #'kill-buffer (list buf-a buf-b)))))

(ert-deftest pilish-test-select-model-updates-current-session-only ()
  "Selecting a model updates only the current session."
  (let* ((buf-a (generate-new-buffer "*pilish-chat:model-select-a*"))
         (buf-b (generate-new-buffer "*pilish-chat:model-select-b*"))
         (available-models (list (list :id "model-a" :name "Model A" :provider "test")
                                 (list :id "model-b" :name "Model B" :provider "test")))
         (selected-model (list :id "model-b" :name "Model B" :provider "test")))
    (unwind-protect
        (cl-letf (((symbol-function 'pilish--rpc-sync)
                   (lambda (&rest _) (list :success t :data (list :models available-models))))
                  ((symbol-function 'pilish--rpc-async)
                   (lambda (_proc _cmd callback)
                     (funcall callback (list :success t :command "set_model" :data selected-model))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Model B")))
          (with-current-buffer buf-a
            (pilish-chat-mode)
            (setq pilish--process :proc-a)
            (setq pilish--state '(:model (:name "Model A" :id "model-a"))))
          (with-current-buffer buf-b
            (pilish-chat-mode)
            (setq pilish--process :proc-b)
            (setq pilish--state '(:model (:name "Model B-old" :id "model-b-old"))))
          (with-current-buffer buf-a
            (pilish-select-model))
          (let ((model-a (with-current-buffer buf-a
                           (plist-get (plist-get pilish--state :model) :name)))
                (model-b (with-current-buffer buf-b
                           (plist-get (plist-get pilish--state :model) :name))))
            (should (equal (list model-a model-b)
                           '("Model B" "Model B-old")))))
      (mapc (lambda (buf)
              (with-current-buffer buf
                (setq pilish--process nil))
              (kill-buffer buf))
            (list buf-a buf-b)))))

(ert-deftest pilish-test-update-state-refreshes-header ()
  "Updating state should trigger header-line refresh."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model (:name "old-model") :thinking-level "low"))
    (let ((header-before (pilish--header-line-string)))
      ;; Simulate state update
      (setq pilish--state '(:model (:name "new-model") :thinking-level "high"))
      (let ((header-after (pilish--header-line-string)))
        ;; Header string should reflect new state
        (should (string-match-p "new-model" header-after))
        (should (string-match-p "high" header-after))))))

(ert-deftest pilish-test-apply-state-response-updates-buffer ()
  "Apply state response updates buffer-local variables in correct buffer."
  (let ((chat-buf (generate-new-buffer "*test-apply-state*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--state nil
                  pilish--status nil))
          ;; Call from a different buffer to verify buffer context handling
          (with-temp-buffer
            (pilish--apply-state-response
             chat-buf
             '(:success t :data (:isStreaming :false
                                 :sessionFile "/tmp/test.jsonl"
                                 :model "test-model"))))
          ;; Verify state was updated in chat-buf, not temp buffer
          (with-current-buffer chat-buf
            (should (eq pilish--status 'idle))
            (should (equal (plist-get pilish--state :session-file)
                           "/tmp/test.jsonl"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-apply-state-response-handles-dead-buffer ()
  "Apply state response handles dead buffer gracefully."
  (let ((chat-buf (generate-new-buffer "*test-dead-buf*")))
    (kill-buffer chat-buf)
    ;; Should not error when buffer is dead
    (pilish--apply-state-response
     chat-buf
     '(:success t :data (:sessionFile "/tmp/test.jsonl")))))

(ert-deftest pilish-test-header-line-model-is-clickable ()
  "Model name in header-line has click properties."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model (:name "claude-sonnet-4")))
    (let ((header (pilish--header-line-string)))
      ;; Should have local-map property
      (should (get-text-property 0 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property 0 'mouse-face header)))))

(ert-deftest pilish-test-header-line-thinking-is-clickable ()
  "Thinking level in header-line cycles on mouse click."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--state '(:model (:name "test") :thinking-level "high"))
    (let* ((header (pilish--header-line-string))
           ;; Find position of "high" in header
           (pos (string-match "high" header))
           (map (and pos (get-text-property pos 'local-map header))))
      (should pos)
      ;; Should have local-map at that position
      (should map)
      ;; Should have mouse-face for highlight
      (should (get-text-property pos 'mouse-face header))
      (should (eq (lookup-key map [header-line mouse-1])
                  #'pilish-cycle-thinking))
      (should (eq (lookup-key map [header-line mouse-2])
                  #'pilish-cycle-thinking)))))

(ert-deftest pilish-test-header-format-context-returns-nil-when-no-window ()
  "Context format returns nil when context window is 0."
  (should (null (pilish--header-format-context 25.0 0))))

(ert-deftest pilish-test-header-format-context-shows-percentage ()
  "Context format shows percentage and window size."
  (let ((result (pilish--header-format-context 25.0 200000)))
    (should (string-match-p "25.0%%" result))
    (should (string-match-p "200k" result))))

(ert-deftest pilish-test-header-format-context-shows-unknown-when-percent-nil ()
  "Context format shows unknown usage when percentage is unavailable."
  (let ((result (pilish--header-format-context nil 200000)))
    (should (string-match-p "\\?/200k" result))))

(ert-deftest pilish-test-header-format-context-warning-over-70 ()
  "Context format uses warning face over 70%."
  (let ((result (pilish--header-format-context 75.0 200000)))
    (should (eq (get-text-property 0 'face result) 'warning))))

(ert-deftest pilish-test-header-format-context-error-over-90 ()
  "Context format uses error face over 90%."
  (let ((result (pilish--header-format-context 95.0 200000)))
    (should (eq (get-text-property 0 'face result) 'error))))

(ert-deftest pilish-test-message-end-refreshes-header-for-assistant ()
  "Assistant message_end refreshes header stats for fresher cost updates."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((refresh-count 0))
      (cl-letf (((symbol-function 'pilish--refresh-header)
                 (lambda () (setq refresh-count (1+ refresh-count)))))
        (pilish--handle-display-event
         '(:type "message_end"
           :message (:role "assistant"
                     :stopReason "stop"
                     :usage (:input 100 :output 50 :cacheRead 10 :cacheWrite 5)))))
      (should (= refresh-count 1)))))

(ert-deftest pilish-test-message-end-does-not-refresh-header-for-user ()
  "User message_end does not trigger header stats refresh."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((refresh-count 0))
      (cl-letf (((symbol-function 'pilish--refresh-header)
                 (lambda () (setq refresh-count (1+ refresh-count)))))
        (pilish--handle-display-event
         '(:type "message_end"
           :message (:role "user" :content "hello"))))
      (should (= refresh-count 0)))))

(ert-deftest pilish-test-header-format-stats-returns-nil-when-no-stats ()
  "Stats format returns nil when stats is nil."
  (should (null (pilish--header-format-stats nil))))

(ert-deftest pilish-test-header-format-stats-shows-cost-and-context ()
  "Header stats shows cost and context percentage from contextUsage."
  (let* ((stats '(:cost 0.05
                  :contextUsage (:tokens 3500 :contextWindow 200000 :percent 1.75)))
         (result (pilish--header-format-stats stats)))
    (should (string-match-p "\\$0.05" result))
    (should (string-match-p "1.8%%/200k" result))))

(ert-deftest pilish-test-header-format-stats-no-context-without-context-usage ()
  "Header stats omit context display when contextUsage is absent.
Without contextUsage there is no context window to display against."
  (let* ((stats '(:cost 0.05))
         (result (pilish--header-format-stats stats)))
    (should (string-match-p "\\$0.05" result))
    (should-not (string-match-p "\\?" result))))

(ert-deftest pilish-test-header-format-stats-shows-unknown-when-tokens-null ()
  "Header stats show ? for context when contextUsage.tokens is :null.
This occurs after compaction before the next assistant message."
  (let* ((stats '(:cost 0.12
                  :contextUsage (:tokens :null :contextWindow 200000 :percent 0)))
         (result (pilish--header-format-stats stats)))
    (should (string-match-p "\\$0.12" result))
    (should (string-match-p "\\?/200k" result))))

;;; File Reference Completion (@)

(ert-deftest pilish-test-at-trigger-context ()
  "@ completion should only trigger at word boundaries, not in emails."
  (with-temp-buffer
    (pilish-input-mode)
    ;; @ at start of buffer - should trigger
    (erase-buffer)
    (insert "@")
    (should (pilish--at-trigger-p))
    ;; @ after space - should trigger
    (erase-buffer)
    (insert "hello @")
    (should (pilish--at-trigger-p))
    ;; @ after newline - should trigger
    (erase-buffer)
    (insert "hello\n@")
    (should (pilish--at-trigger-p))
    ;; @ after punctuation - should trigger
    (erase-buffer)
    (insert "see:@")
    (should (pilish--at-trigger-p))
    ;; @ after alphanumeric (email) - should NOT trigger
    (erase-buffer)
    (insert "user@")
    (should-not (pilish--at-trigger-p))
    ;; @ in middle of email - should NOT trigger
    (erase-buffer)
    (insert "test123@")
    (should-not (pilish--at-trigger-p))))

(ert-deftest pilish-test-file-reference-capf-returns-nil-without-at ()
  "File reference completion returns nil when not after @."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "hello world")
    (should-not (pilish--file-reference-capf))))

(ert-deftest pilish-test-file-reference-capf-returns-data-after-at ()
  "File reference completion returns data when point is after @."
  (with-temp-buffer
    (pilish-input-mode)
    ;; Mock project files
    (setq pilish--project-files-cache '("file1.el" "file2.py" "dir/file3.ts"))
    (setq pilish--project-files-cache-time (float-time))
    (insert "Check @fi")
    (let ((result (pilish--file-reference-capf)))
      (should result)
      ;; Start should be after @
      (should (= (nth 0 result) (- (point) 2)))  ; Position after @
      ;; End should be at point
      (should (= (nth 1 result) (point)))
      ;; Candidates should include matching files
      (should (member "file1.el" (nth 2 result)))
      (should (member "file2.py" (nth 2 result))))))

(ert-deftest pilish-test-file-reference-capf-empty-prefix ()
  "File reference completion returns all files when no prefix after @."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--project-files-cache '("a.el" "b.py" "c.ts"))
    (setq pilish--project-files-cache-time (float-time))
    (insert "See @")
    (let ((result (pilish--file-reference-capf)))
      (should result)
      ;; Should return all files when prefix is empty
      (should (= (length (nth 2 result)) 3)))))

(ert-deftest pilish-test-file-reference-capf-mid-line ()
  "File reference completion works in the middle of a line."
  (with-temp-buffer
    (pilish-input-mode)
    (setq pilish--project-files-cache '("test.el"))
    (setq pilish--project-files-cache-time (float-time))
    (insert "Look at @te and also")
    (goto-char 11)  ; Position right after "@te"
    (let ((result (pilish--file-reference-capf)))
      (should result)
      (should (member "test.el" (nth 2 result))))))

;;; Path Completion (Tab)

(ert-deftest pilish-test-path-capf-returns-nil-for-non-path ()
  "Path completion returns nil for text that doesn't look like a path."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "hello world")
    (should-not (pilish--path-capf))))

(ert-deftest pilish-test-path-capf-returns-nil-for-non-prefixed-path ()
  "Path completion returns nil for paths without ./ ../ ~/ or / prefix."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "src/file.el")
    (should-not (pilish--path-capf))))

(ert-deftest pilish-test-path-capf-triggers-for-dot-slash ()
  "Path completion triggers for paths starting with ./"
  (let* ((temp-dir (make-temp-file "pilish-path-test-" t))
         (test-file (expand-file-name "test.txt" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file test-file (insert "test"))
          (let ((default-directory temp-dir))
            (with-temp-buffer
              (pilish-input-mode)
              (setq pilish--chat-buffer (current-buffer))
              ;; Mock session directory
              (cl-letf (((symbol-function 'pilish--session-directory)
                         (lambda () temp-dir)))
                (insert "./te")
                (let ((result (pilish--path-capf)))
                  (should result)
                  ;; Should have candidates
                  (should (> (length (nth 2 result)) 0)))))))
      (delete-directory temp-dir t))))

(ert-deftest pilish-test-path-capf-triggers-for-tilde ()
  "Path completion triggers for paths starting with ~/"
  (with-temp-buffer
    (pilish-input-mode)
    (insert "~/")
    ;; Just verify it doesn't error and returns something
    ;; (actual completions depend on user's home directory)
    (let ((result (pilish--path-capf)))
      ;; May return nil if ~ directory doesn't exist or has no completions
      ;; but should not error
      (should (or (null result) (listp result))))))

(ert-deftest pilish-test-path-capf-triggers-for-absolute ()
  "Path completion triggers for absolute paths not at buffer start."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "see /tmp/")
    (let ((result (pilish--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest pilish-test-path-capf-remote-absolute-uses-remote-root ()
  "Remote absolute path completion checks remote / but inserts /-style paths."
  (let ((default-directory "/ssh:pi-host:/home/pi/project/")
        (completion-base nil)
        (completion-dir nil)
        (directory-checks nil))
    (with-temp-buffer
      (pilish-input-mode)
      (setq default-directory "/ssh:pi-host:/home/pi/project/")
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (path)
                   (push path directory-checks)
                   (string-prefix-p "/ssh:pi-host:" path)))
                ((symbol-function 'file-name-all-completions)
                 (lambda (base dir)
                   (setq completion-base base
                         completion-dir dir)
                   '("log/" "local"))))
        (insert "see /var/lo")
        (let* ((result (pilish--path-capf))
               (candidates (nth 2 result))
               (annotation (plist-get (nthcdr 3 result) :annotation-function)))
          (should result)
          (should (equal completion-base "lo"))
          (should (equal completion-dir "/ssh:pi-host:/var/"))
          (should (equal candidates '("/var/log/" "/var/local")))
          (should-not (cl-some #'file-remote-p candidates))
          (should (equal (funcall annotation "/var/log/") " (dir)"))
          (should (equal (funcall annotation "/var/local") " (file)"))
          (should (member "/ssh:pi-host:/var/" directory-checks))
          (should-not (member "/ssh:pi-host:/var/log/" directory-checks))
          (should-not (member "/var/" directory-checks)))))))

(ert-deftest pilish-test-path-capf-multi-hop-absolute-keeps-full-route ()
  "Remote absolute path completion checks the full multi-hop TRAMP route."
  (let ((completion-base nil)
        (completion-dir nil)
        (directory-checks nil)
        (session-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (with-temp-buffer
      (pilish-input-mode)
      (cl-letf (((symbol-function 'pilish--session-directory)
                 (lambda () session-dir))
                ((symbol-function 'file-directory-p)
                 (lambda (path)
                   (push path directory-checks)
                   (equal path "/ssh:bastion|sudo:root@pi-host:/var/")))
                ((symbol-function 'file-name-all-completions)
                 (lambda (base dir)
                   (setq completion-base base
                         completion-dir dir)
                   '("log/" "local"))))
        (insert "see /var/lo")
        (let* ((result (pilish--path-capf))
               (candidates (nth 2 result)))
          (should result)
          (should (equal completion-base "lo"))
          (should (equal completion-dir
                         "/ssh:bastion|sudo:root@pi-host:/var/"))
          (should (equal candidates '("/var/log/" "/var/local")))
          (should (member "/ssh:bastion|sudo:root@pi-host:/var/"
                          directory-checks)))))))

(ert-deftest pilish-test-path-completions-returns-nil-for-unsafe-input ()
  "Path completion treats malformed path text as no completions."
  (let ((bad-path (concat "/tmp/bad" (string ?\0) "name")))
    (should-not (pilish--path-completions bad-path))))

(ert-deftest pilish-test-path-completions-excludes-dot-entries ()
  "Path completions should not include ./ or ../ entries."
  (let* ((temp-dir (make-temp-file "pilish-path-test-" t))
         (subdir (expand-file-name "subdir" temp-dir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (cl-letf (((symbol-function 'pilish--session-directory)
                     (lambda () temp-dir)))
            (let ((completions (pilish--path-completions "./")))
              ;; Should have the subdir
              (should (member "./subdir/" completions))
              ;; Should NOT have ./ or ../
              (should-not (member "./" completions))
              (should-not (member "./../" completions))
              (should-not (member "././" completions)))))
      (delete-directory temp-dir t))))

(ert-deftest pilish-test-complete-command-exists ()
  "pilish-complete should be an interactive command."
  (should (commandp 'pilish-complete)))

(ert-deftest pilish-test-path-capf-skips-slash-at-buffer-start ()
  "Path completion skips / at buffer start to allow slash commands."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "/tmp")
    (should-not (pilish--path-capf))))

(ert-deftest pilish-test-path-capf-allows-slash-on-later-lines ()
  "Path completion works for / on lines after the first."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "Check this file:\n/tmp/")
    (let ((result (pilish--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest pilish-test-tool-start-creates-overlay ()
  "tool_execution_start creates an overlay with pending face."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    ;; Should have an overlay with pilish-tool-block property
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'pilish-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'pilish-tool-block))
      (should (equal (overlay-get tool-ov 'pilish-tool-name) "bash")))))

(ert-deftest pilish-test-tool-start-header-format ()
  "tool_execution_start uses simple header format, not drawer syntax."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls -la"))
    ;; Should have "$ ls -la" header
    (should (string-match-p "\\$ ls -la" (buffer-string)))
    ;; Should NOT have drawer syntax
    (should-not (string-match-p ":BASH:" (buffer-string)))))

(ert-deftest pilish-test-tool-end-keeps-overlay-face ()
  "tool_execution_end keeps base face on success."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    ;; Initially base face
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pilish-tool-block)))
    ;; After success — face stays the same
    (pilish--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pilish-tool-block)))))

(ert-deftest pilish-test-tool-end-error-face ()
  "tool_execution_end sets error face on failure."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "bad"))
    (pilish--display-tool-end "bash" '(:command "bad")
                          '((:type "text" :text "error"))
                          nil t)  ; is-error = t
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pilish-tool-block-error)))))

(ert-deftest pilish-test-tool-end-no-drawer-syntax ()
  "tool_execution_end does not insert :END: marker."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-tool-start "bash" '(:command "ls"))
    (pilish--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "output"))
                          nil nil)
    (should-not (string-match-p ":END:" (buffer-string)))))

(ert-deftest pilish-test-tool-overlay-does-not-extend-to-subsequent-content ()
  "Tool overlay should not extend when content is inserted after tool block.
Regression test: overlay with rear-advance was extending to subsequent content."
  (with-temp-buffer
    (pilish-chat-mode)
    ;; Create a complete tool block
    (pilish--display-tool-start "write" '(:path "/tmp/test.txt" :content "hello"))
    (pilish--display-tool-end "write" '(:path "/tmp/test.txt" :content "hello")
                          '((:type "text" :text "Written to /tmp/test.txt"))
                          nil nil)
    ;; Simulate inserting more content after the tool (like next message)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "AFTER_TOOL_CONTENT\n"))
    ;; The new content should NOT be inside any tool overlay
    (let* ((new-content-pos (- (point-max) 10))  ; somewhere in AFTER_TOOL_CONTENT
           (overlays (overlays-at new-content-pos))
           (tool-overlay (seq-find
                          (lambda (ov) (overlay-get ov 'pilish-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest pilish-test-abort-mid-tool-cleans-up-overlay ()
  "Aborting mid-tool should clean up the pending overlay.
When abort happens during tool execution, tool_execution_end never arrives.
display-agent-end must finalize the pending overlay with error face."
  (with-temp-buffer
    (pilish-chat-mode)
    ;; Start a tool (creates pending overlay)
    (pilish--display-tool-start "bash" '(:command "sleep 100"))
    ;; Verify overlay is pending
    (should pilish--pending-tool-overlay)
    (should (eq (overlay-get pilish--pending-tool-overlay 'face)
                'pilish-tool-block))
    ;; Simulate abort - display-agent-end is called WITHOUT tool-end
    (setq pilish--aborted t)
    (pilish--display-agent-end)
    ;; Pending overlay variable should be nil
    (should-not pilish--pending-tool-overlay)
    ;; But there should still be a finalized overlay with error face
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'pilish-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'pilish-tool-block-error)))
    ;; Content inserted after should NOT be inside the overlay
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "AFTER_ABORT_CONTENT\n"))
    (let* ((new-content-pos (- (point-max) 10))
           (overlays (overlays-at new-content-pos))
           (tool-overlay (seq-find
                          (lambda (ov) (overlay-get ov 'pilish-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest pilish-test-delta-no-transform-inside-code-block ()
  "Hash inside fenced code block should NOT be transformed."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta "```python\n# This is a comment\n```")
    ;; The # inside code block should stay as single #
    (should (string-match-p "^# This is a comment$" (buffer-string)))))

(ert-deftest pilish-test-delta-transform-resumes-after-code-block ()
  "Headings after code block closes should be transformed."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta "```\n# comment\n```\n# Real Heading")
    ;; Inside block: stays #
    (should (string-match-p "^# comment$" (buffer-string)))
    ;; After block: becomes ##
    (should (string-match-p "^## Real Heading" (buffer-string)))))

(ert-deftest pilish-test-delta-code-fence-split-across-deltas ()
  "Code fence split across deltas still detected."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta "``")
    (pilish--display-message-delta "`python\n# comment\n```")
    ;; Should recognize the split ``` and not transform inside
    (should (string-match-p "^# comment$" (buffer-string)))))

(ert-deftest pilish-test-delta-backticks-mid-line-not-fence ()
  "Backticks mid-line don't trigger code block state."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta "Use ```code``` inline\n# Heading")
    ;; Inline backticks shouldn't affect heading transform
    (should (string-match-p "^## Heading" (buffer-string)))))

;;; Input Mode — Markdown Highlighting

(ert-deftest pilish-test-input-mode-md-ts-by-default ()
  "By default, input mode has tree-sitter markdown font-lock."
  (with-temp-buffer
    (pilish-input-mode)
    (should (derived-mode-p 'pilish-input-mode))
    (insert "some **bold** text")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "bold")
    (should (memq 'bold
                  (let ((f (get-text-property (1- (point)) 'face)))
                    (if (listp f) f (list f)))))))

(ert-deftest pilish-test-input-mode-md-ts-preserves-mode-identity ()
  "Markdown highlighting preserves the derived mode's identity.
`md-ts-mode' is a full major mode, so `pilish-input-mode' must restore
the identity `define-derived-mode' installed: mode name, `major-mode',
the local keymap, and the tree-sitter font-lock."
  (with-temp-buffer
    (let ((pilish-input-markdown-highlighting t))
      (pilish-input-mode)
      (should (eq major-mode 'pilish-input-mode))
      (should (string= mode-name "Pi-Input"))
      (should (eq (current-local-map) pilish-input-mode-map))
      ;; md-ts font-lock is engaged, not merely absent.
      (insert "some **bold** text")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "bold")
      (should (memq 'bold
                    (let ((f (get-text-property (1- (point)) 'face)))
                      (if (listp f) f (list f))))))))

(ert-deftest pilish-test-input-mode-no-metadata-face ()
  "With markdown highlighting, lines ending with colon have no metadata face.
Tree-sitter markdown doesn't have metadata face, so this verifies
no spurious faces are applied to plain colon-ending lines."
  (with-temp-buffer
    (pilish-input-mode)
    (insert "Fix the bug:\n- item\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((f (get-text-property (point) 'face)))
      ;; No heading, bold, or other markdown face on plain text
      (should-not (and f (not (eq f 'default)))))))

(ert-deftest pilish-test-input-mode-no-hidden-markup ()
  "Input mode does NOT hide markup, even when user customizes it globally."
  (with-temp-buffer
    (let ((old-default (default-value 'md-ts-hide-markup)))
      (unwind-protect
          (progn
            (setq-default md-ts-hide-markup t)
            (pilish-input-mode)
            (should-not md-ts-hide-markup)
            (insert "some **bold** text")
            (font-lock-ensure)
            (goto-char (point-min))
            (search-forward "**")
            (should-not (get-text-property (1- (point)) 'invisible)))
        (setq-default md-ts-hide-markup old-default)))))

(ert-deftest pilish-test-input-mode-no-fontification-without-markdown ()
  "Without markdown highlighting, bold text gets no bold face."
  (with-temp-buffer
    (let ((pilish-input-markdown-highlighting nil))
      (pilish-input-mode)
      (insert "some **bold** text")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "bold")
      (should-not (memq 'bold
                        (let ((f (get-text-property (1- (point)) 'face)))
                          (if (listp f) f (list f))))))))

(ert-deftest pilish-test-input-mode-keybindings ()
  "Pi input keybindings are active in input mode."
  (with-temp-buffer
    (pilish-input-mode)
    (should (eq (key-binding (kbd "C-c C-c")) 'pilish-send))
    (should (eq (key-binding (kbd "C-c C-k")) 'pilish-abort))
    (should (eq (key-binding (kbd "C-c C-p")) 'pilish-menu))
    (should (eq (key-binding (kbd "C-c C-r"))
                'pilish-session-browser))
    (should (eq (key-binding (kbd "M-p")) 'pilish-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'pilish-next-input))
    (should (eq (key-binding (kbd "TAB")) 'pilish-complete))
    (should (eq (key-binding (kbd "C-c C-s")) 'pilish-queue-steering))))

;;; Input-Buffer Chat Navigation

(ert-deftest pilish-test-input-next-message-moves-chat ()
  "Input-side next-message moves the linked chat and keeps focus."
  (let ((chat-buf (generate-new-buffer "*test-chat*"))
        (input-buf (generate-new-buffer "*test-input*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer chat-buf)
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (let ((inhibit-read-only t))
              (pilish-test--insert-chat-turns))
            (pilish--set-input-buffer input-buf)
            (goto-char (point-min)))
          (let ((input-win (split-window nil -10 'below)))
            (set-window-buffer input-win input-buf)
            (with-current-buffer input-buf
              (pilish-input-mode)
              (pilish--set-chat-buffer chat-buf))
            (select-window input-win)
            (pilish-input-next-message)
            (with-current-buffer chat-buf
              (should (looking-at "You · 10:00")))
            (should (eq (window-buffer (selected-window)) input-buf))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf)
      (delete-other-windows))))

(ert-deftest pilish-test-input-previous-message-moves-linked-chat ()
  "Input-side previous-message uses the linked chat, not scroll state."
  (let ((chat-a (generate-new-buffer "*test-chat-a*"))
        (chat-b (generate-new-buffer "*test-chat-b*"))
        (input-buf (generate-new-buffer "*test-input*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer chat-a)
          (let* ((chat-win-a (selected-window))
                 (input-win (split-window chat-win-a -10 'below))
                 (chat-win-b (split-window chat-win-a nil 'right)))
            (set-window-buffer input-win input-buf)
            (set-window-buffer chat-win-b chat-b)
            (with-current-buffer chat-a
              (pilish-chat-mode)
              (let ((inhibit-read-only t))
                (pilish-test--insert-chat-turns))
              (goto-char (point-max))
              (re-search-backward "^You · 10:10$" nil t))
            (with-current-buffer chat-b
              (pilish-chat-mode)
              (let ((inhibit-read-only t))
                (pilish-test--insert-chat-turns))
              (goto-char (point-max))
              (re-search-backward "^You · 10:10$" nil t))
            (with-current-buffer input-buf
              (pilish-input-mode)
              (pilish--set-chat-buffer chat-a)
              (setq-local other-window-scroll-buffer chat-b))
            (select-window input-win)
            (pilish-input-previous-message)
            (with-current-buffer chat-a
              (should (looking-at "You · 10:05")))
            (with-current-buffer chat-b
              (should (looking-at "You · 10:10")))
            (should (eq (window-buffer (selected-window)) input-buf))))
      (kill-buffer chat-a)
      (kill-buffer chat-b)
      (kill-buffer input-buf)
      (delete-other-windows))))

(ert-deftest pilish-test-input-previous-message-no-chat-window-errors ()
  "Navigating from input without a visible linked chat signals error."
  (let ((chat-buf (generate-new-buffer "*test-chat-hidden*")))
    (unwind-protect
        (with-temp-buffer
          (pilish-input-mode)
          (pilish--set-chat-buffer chat-buf)
          (should-error (pilish-input-previous-message)
                        :type 'user-error))
      (kill-buffer chat-buf))))

;;; Queued-count Input Header

(defun pilish-test--queued-header-wire (proc event)
  "Deliver EVENT as a framed JSON line through PROC's production filter."
  (pilish--process-filter proc (concat (json-encode event) "\n")))

(defun pilish-test--queued-header (input count)
  "Check INPUT's queued COUNT and return its tooltip, or nil for zero."
  (let* ((header (with-current-buffer input (pilish--header-line-string)))
         (position (string-match " queued [0-9]+" header)))
    (if (zerop count)
        (progn
          (should-not position)
          nil)
      (should position)
      (should (equal (match-string 0 header) (format " queued %d" count)))
      (let* ((start (1+ position))
             (help (get-text-property start 'help-echo header)))
        (should (stringp help))
        (should (eq (get-text-property start 'mouse-face header) 'highlight))
        (should-not (get-text-property start 'local-map header))
        (should-not (get-text-property start 'keymap header))
        help))))

(ert-deftest pilish-test-queued-header-local-busy-sends-in-fifo-order ()
  "Busy sends show the known local FIFO even without backend queue information."
  (dolist (status '(sending streaming compacting))
    (pilish-test-with-rpc-session (chat input _proc commands)
      (with-current-buffer chat (setq pilish--status status))
      (pilish-test--queued-header input 0)
      (let (notices)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) notices))))
          (dolist (text '("first local" "second local"))
            (with-current-buffer input (insert text) (pilish-send))))
        (should (= 2 (length notices))))
      (should-not commands)
      (with-current-buffer input (should (string-empty-p (buffer-string))))
      (with-current-buffer chat
        (should (equal (pilish--followups-in-fifo-order)
                       '("first local" "second local"))))
      (let ((help (pilish-test--queued-header input 2)))
        (should (string-match-p "Follow-ups.*2" help))
        (should (string-match-p "[-•] first local\n[-•] second local" help))
        (should-not (string-match-p "Steering" help))))))

(ert-deftest pilish-test-queued-header-steering-snapshots-not-acks ()
  "Queue snapshots replace one another; steer send/ACK never invent entries."
  (pilish-test-with-rpc-session (chat input proc commands)
    (pilish-test--queued-header input 0)
    (pilish-test--queued-header-wire
     proc '(:type "queue_update" :steering [] :followUp []))
    (pilish-test--queued-header input 0)
    (with-current-buffer chat (setq pilish--status 'streaming))
    (let (notice)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq notice (apply #'format fmt args)))))
        (with-current-buffer input (insert "duplicate") (pilish-queue-steering)))
      (should-not notice))
    (should (equal (plist-get (car commands) :type) "steer"))
    (let ((request (car commands)))
      (pilish-test--queued-header input 0)
      ;; A snapshot can precede its correlated steer response.  Split framing
      ;; must not expose a partial snapshot or lose duplicate messages.
      (pilish--process-filter
       proc "{\"type\":\"queue_update\",\"steering\":[\"duplicate\",")
      (pilish-test--queued-header input 0)
      (pilish--process-filter proc "\"duplicate\"],\"followUp\":[]}\n")
      (let ((help (pilish-test--queued-header input 2)))
        (should (string-match-p "Steering.*2" help))
        (should (string-match-p "[-•] duplicate\n[-•] duplicate" help))
        (should-not (string-match-p "Follow-ups" help)))
      (let (notice)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq notice (apply #'format fmt args)))))
          (pilish-test--queued-header-wire
           proc (list :type "response" :id (plist-get request :id)
                      :command "steer" :success t)))
        (should (equal notice "Pi: Steering message sent")))
      (pilish-test--queued-header input 2))
    (pilish-test--queued-header-wire
     proc '(:type "queue_update" :steering ["replacement"] :followUp []))
    (let ((help (pilish-test--queued-header input 1)))
      (should (string-match-p "replacement" help))
      (should-not (string-match-p "duplicate" help)))
    (pilish-test--queued-header-wire
     proc '(:type "queue_update" :steering [] :followUp []))
    (pilish-test--queued-header input 0)
    (should (= 1 (length commands)))))

(ert-deftest pilish-test-queued-header-stats-order-and-exact-counts ()
  "Queued text sits after cost/context and before session, without a count cap."
  (dolist (count '(1 10 99 123))
    (pilish-test-with-rpc-session (chat input proc _commands)
      (with-current-buffer chat
        (setq pilish--cached-stats
              '(:cost 1.25 :contextUsage (:tokens 500 :percent 25.0
                                        :contextWindow 2000))
              pilish--session-name "my session"))
      (pilish-test--queued-header-wire
       proc (list :type "queue_update" :steering (make-vector count "next")
                  :followUp []))
      (pilish-test--queued-header input count)
      (with-current-buffer input
        (let ((header (pilish--header-line-string)))
          (should (string-match-p
                   (regexp-quote (format " │ $1.25 25.0%%%%/2k queued %d │ my session"
                                         count))
                   header)))))))

(ert-deftest pilish-test-queued-header-tooltip-groups-and-source-order ()
  "Follow-ups show backend order then local FIFO; groups do not deduplicate."
  (pilish-test-with-rpc-session (chat input proc _commands)
    (with-current-buffer chat
      (pilish--push-followup "same")
      (pilish--push-followup "local second"))
    (pilish-test--queued-header-wire
     proc '(:type "queue_update" :followUp ["backend first"]
            :steering ["steer first" "steer second"]))
    (let ((help (pilish-test--queued-header input 5)))
      (should (string-match-p "Follow-ups.*3" help))
      (should (string-match-p "Steering.*2" help))
      (should (string-match-p
               "[-•] backend first\n[-•] same\n[-•] local second\n\nSteering"
               help))
      (should (string-match-p "[-•] steer first\n[-•] steer second" help)))
    (pilish-test--queued-header-wire
     proc '(:type "queue_update" :followUp ["same"] :steering []))
    (let ((help (pilish-test--queued-header input 3)))
      (should (string-match-p "[-•] same\n[-•] same\n[-•] local second" help))
      (should-not (string-match-p "Steering" help)))))

(ert-deftest pilish-test-queued-header-tooltip-bounded-literal-and-read-only ()
  "Hover previews are bounded plain literal text and never poll or mutate queues."
  (pilish-test-with-rpc-session (chat input proc commands)
    (with-current-buffer chat
      (pilish--push-followup
       (propertize "100% \\[save-buffer]\n\t literal"
                   'display "WRONG" 'invisible t 'face 'error
                   'help-echo "WRONG" 'keymap (make-sparse-keymap)))
      (pilish--push-followup (concat "long " (make-string 500 ?x)))
      ;; Bound source processing before whitespace flattening, not just the
      ;; final preview length: this suffix must never reach the tooltip.
      (pilish--push-followup (concat (make-string 513 ?\s) "AFTER-SOURCE-BOUND"))
      (pilish--push-followup "hidden fourth")
      (pilish--push-followup "hidden fifth"))
    (pilish-test--queued-header-wire
     proc '(:type "queue_update" :followUp []
            :steering ["one" "two" "three" "hidden four" "hidden five" "hidden six" "hidden seven"]))
    (let ((before (with-current-buffer chat
                    (mapcar #'copy-sequence pilish--followup-queue)))
          (pending (hash-table-count (pilish--get-pending-requests proc))))
      (cl-letf (((symbol-function 'pilish--refresh-header)
                 (lambda () (ert-fail "Formatting must not fetch stats"))))
        (dotimes (_ 5)
          (let ((help (pilish-test--queued-header input 12)))
            (should (string-match-p "Follow-ups.*5" help))
            (should (string-match-p "Steering.*7" help))
            (let ((literal-position
                   (string-match (regexp-quote "100% \\[save-buffer] literal") help)))
              (should literal-position)
              (should-not (get-text-property literal-position 'face help)))
            (should (get-text-property 0 'help-echo-inhibit-substitution help))
            (should-not (string-match-p "\t\\|hidden\\|WRONG" help))
            (should-not (string-match-p "AFTER-SOURCE-BOUND" help))
            (should (string-match-p "and 2 more" help))
            (should (string-match-p "and 4 more" help))
            (let ((previews (seq-filter
                             (lambda (line) (string-match-p "^[-•] " line))
                             (split-string help "\n"))))
              (should (= 6 (length previews)))
              (should (or (string-suffix-p "…" (nth 2 previews))
                          (string-suffix-p "..." (nth 2 previews))))
              ;; Allow the bullet and a short truncation marker around the
              ;; approximately 120-character preview budget.
              (dolist (line previews) (should (<= (length line) 125))))
            (dotimes (i (length help))
              (dolist (prop '(display invisible keymap local-map help-echo))
                (should-not (get-text-property i prop help)))))))
      (with-current-buffer chat
        (should (equal-including-properties pilish--followup-queue before)))
      (should (= pending (hash-table-count (pilish--get-pending-requests proc))))
      (should-not commands))))

(ert-deftest pilish-test-queued-header-process-owned-snapshots ()
  "Snapshots precede display and survive state replacement and process adoption."
  (pilish-test-with-rpc-session (chat input old commands)
    (let ((candidate (start-process "pilish-test-queue-candidate" nil "cat")))
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag candidate nil)
            ;; Candidates receive early events without a display handler.
            (pilish-test--queued-header-wire
             candidate '(:type "queue_update" :followUp ["candidate"] :steering []))
            (pilish-test--queued-header input 0)
            (let ((handler (process-get old 'pilish-display-handler)) observed)
              (process-put old 'pilish-display-handler
                           (lambda (event)
                             (setq observed
                                   (with-current-buffer input
                                     (pilish--header-line-string)))
                             (funcall handler event)))
              (pilish-test--queued-header-wire
               old '(:type "queue_update" :followUp ["old one" "old two"] :steering []))
              (should (string-match-p " queued 2" observed))
              (process-put old 'pilish-display-handler handler))
            ;; A full get_state replacement must not erase the event snapshot.
            (pilish--rpc-async old '(:type "get_state")
                              (lambda (response)
                                (pilish--apply-state-response chat response)))
            (pilish-test--queued-header-wire
             old (list :type "response" :command "get_state" :success t
                       :id (plist-get (car commands) :id)
                       :data '(:model (:name "replacement state") :isStreaming :false)))
            (pilish-test--queued-header input 2)
            (process-put candidate 'pilish-chat-buffer chat)
            (pilish--register-display-handler candidate)
            (with-current-buffer chat (pilish--set-process candidate))
            (should (string-match-p "candidate" (pilish-test--queued-header input 1)))
            ;; An old handler can still be installed while its late event arrives.
            (pilish-test--queued-header-wire
             old '(:type "queue_update" :followUp ["stale"] :steering ["stale too"]))
            (let ((help (pilish-test--queued-header input 1)))
              (should (string-match-p "candidate" help))
              (should-not (string-match-p "stale" help)))
            (delete-process old)
            (pilish-test--queued-header input 1)
            (delete-process candidate)
            (pilish-test--queued-header input 0)
            (with-current-buffer chat (pilish--push-followup "still local"))
            (should (string-match-p "still local" (pilish-test--queued-header input 1))))
        (when (process-live-p candidate) (delete-process candidate))))))

(ert-deftest pilish-test-queued-header-submitted-head-accept-reject-abort ()
  "A submitted FIFO head stays counted until acceptance, restoration, or Stop."
  (dolist (outcome '(accept reject abort))
    (pilish-test-with-rpc-session (chat input proc commands)
      (with-current-buffer chat
        (pilish--push-followup "first")
        (pilish--push-followup "second")
        (pilish--process-followup-queue))
      (should (equal (plist-get (car commands) :message) "first"))
      (let ((request (car commands)) notices)
        ;; An extension may finish its run before accepting the prompt.  The
        ;; existing FIFO owner remains present across start/end/settlement.
        (dolist (event '((:type "agent_start") (:type "agent_end" :messages [])
                         (:type "agent_settled")))
          (pilish-test--queued-header-wire proc event))
        (should (= 1 (length commands)))
        (pilish-test--queued-header input 2)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) notices))))
          (if (eq outcome 'abort)
              (with-current-buffer input (pilish-abort))
            (pilish-test--queued-header-wire
             proc (list :type "response" :command "prompt"
                        :id (plist-get request :id)
                        :success (if (eq outcome 'accept) t :false)
                        :error "preflight rejected"))))
        (pcase outcome
          ('accept
           (should-not notices)
           (let ((help (pilish-test--queued-header input 1)))
             (should (string-match-p "second" help))
             (should-not (string-match-p "first" help))))
          ('reject
           (should (member "Pi: Send failed: preflight rejected" notices))
           (pilish-test--queued-header input 0)
           (with-current-buffer input
             (should (equal (buffer-string) "first\n\nsecond"))))
          ('abort
           (should (member "Pi: Aborting..." notices))
           (should (equal (mapcar (lambda (cmd) (plist-get cmd :type))
                                 (reverse commands))
                          '("prompt" "clear_queue" "abort")))
           (pilish-test--queued-header input 0)
           ;; A late acceptance cannot resurrect cleared local work.
           (pilish-test--queued-header-wire
            proc (list :type "response" :command "prompt" :success t
                       :id (plist-get request :id)))
           (pilish-test--queued-header input 0)))))))

(ert-deftest pilish-test-queued-header-local-and-current-events-request-redisplay ()
  "Actual FIFO changes, current snapshots, and adoption redraw locally, not by RPC."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let ((candidate (start-process "pilish-test-queue-redraw" nil "cat")) updates)
      (unwind-protect
          (save-window-excursion
            (set-window-buffer (selected-window) input)
            (cl-letf (((symbol-function 'force-mode-line-update)
                       (lambda (&optional all)
                         (push (cons (current-buffer) all) updates)))
                      ((symbol-function 'pilish--refresh-header)
                       (lambda () (ert-fail "Queue changes must not fetch stats"))))
              (dolist (operation '(push pop clear snapshot adopt))
                (when (eq operation 'clear)
                  (with-current-buffer chat (pilish--push-followup "clear me")))
                (setq updates nil)
                (with-current-buffer chat
                  (pcase operation
                    ('push (pilish--push-followup "first"))
                    ('pop (should (equal (pilish--dequeue-followup) "first")))
                    ('clear (pilish--clear-followup-queue))
                    ('snapshot (pilish-test--queued-header-wire
                                proc '(:type "queue_update" :followUp [] :steering ["next"])))
                    ('adopt
                     (process-put candidate 'pilish-chat-buffer chat)
                     (pilish--register-display-handler candidate)
                     (pilish--set-process candidate))))
                (ert-info ((format "Missing input redisplay for %s" operation))
                  (should (seq-some (lambda (update)
                                      (or (cdr update) (eq (car update) input)))
                                    updates))))
              (setq updates nil)
              (pilish-test--queued-header-wire
               proc '(:type "queue_update" :followUp [] :steering []))
              (should-not updates)
              (should-not commands)))
        (when (process-live-p candidate) (delete-process candidate))))))

(provide 'pilish-input-test)
;;; pilish-input-test.el ends here
