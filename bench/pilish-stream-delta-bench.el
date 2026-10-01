;;; pilish-stream-delta-bench.el --- Stream-delta benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Deterministic coalesced text/thinking/toolcall benchmark.  A fake pi replays
;; synthetic history, then sends timed bursts and one deliberately collected
;; backlog through the real `pilish--process-filter'.  Correctness assertions
;; fail the run; all timing values are diagnostic only.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst pilish-sd-bench-repo-root
  (file-name-as-directory
   (expand-file-name ".."
                     (file-name-directory
                      (or load-file-name buffer-file-name default-directory))))
  "Repository root containing the stream-delta benchmark files.")

(add-to-list 'load-path pilish-sd-bench-repo-root)
(require 'pilish)

(defun pilish-sd-bench--env (name default)
  "Return environment variable NAME, or DEFAULT when unset or empty."
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value))) value default)))

(defun pilish-sd-bench--env-int (name default)
  "Return environment variable NAME as an integer, or DEFAULT."
  (string-to-number
   (pilish-sd-bench--env name (number-to-string default))))

(defun pilish-sd-bench--truthy-env-p (name default)
  "Return whether environment variable NAME is truthy, using DEFAULT."
  (member (downcase (pilish-sd-bench--env name default))
          '("1" "true" "yes" "on")))

(defun pilish-sd-bench--json-bool (value)
  "Return VALUE encoded as a JSON boolean sentinel."
  (if value t :json-false))

(defvar pilish-sd-bench-scenario
  (pilish-sd-bench--env "PI_SD_BENCH_SCENARIO" "full")
  "Scenario label written into stream-delta artifacts.")

(defvar pilish-sd-bench-iteration
  (pilish-sd-bench--env-int "PI_SD_BENCH_ITERATION" 1)
  "Iteration number written into stream-delta artifacts.")

(defvar pilish-sd-bench-history-turns
  (pilish-sd-bench--env-int "PI_SD_BENCH_HISTORY_TURNS" 180)
  "Number of user/assistant turns in synthetic history.")

(defvar pilish-sd-bench-history-text-bytes
  (pilish-sd-bench--env-int "PI_SD_BENCH_HISTORY_TEXT_BYTES" 1200)
  "Characters in each synthetic history message body.")

(defvar pilish-sd-bench-timer-text-deltas
  (pilish-sd-bench--env-int "PI_SD_BENCH_TIMER_TEXT_DELTAS" 700)
  "Number of text deltas sent in timer-separated bursts.")

(defvar pilish-sd-bench-text-burst
  (pilish-sd-bench--env-int "PI_SD_BENCH_TEXT_BURST" 20)
  "Number of text deltas in each timer-separated burst.")

(defvar pilish-sd-bench-thinking-deltas
  (pilish-sd-bench--env-int "PI_SD_BENCH_THINKING_DELTAS" 80)
  "Number of thinking deltas in timer-separated bursts.")

(defvar pilish-sd-bench-thinking-burst
  (pilish-sd-bench--env-int "PI_SD_BENCH_THINKING_BURST" 20)
  "Number of thinking deltas in each timer-separated burst.")

(defvar pilish-sd-bench-backlog-deltas
  (pilish-sd-bench--env-int "PI_SD_BENCH_BACKLOG_DELTAS" 300)
  "Number of text deltas delivered in one collected `process-filter' call.")

(defvar pilish-sd-bench-toolcall-repeats
  (pilish-sd-bench--env-int "PI_SD_BENCH_TOOLCALL_REPEATS" 0)
  "Number of echo fragments in a bash command streamed in 32-character deltas.
Zero keeps the original single-delta tool boundary check.")

(defvar pilish-sd-bench-burst-pause-ms
  (pilish-sd-bench--env-int "PI_SD_BENCH_BURST_PAUSE_MS" 80)
  "Milliseconds between ordinary stream bursts.")

(defvar pilish-sd-bench-seed
  (pilish-sd-bench--env-int "PI_SD_BENCH_SEED" 20240817)
  "Seed used by deterministic stream payload formulas.")

(defvar pilish-sd-bench-timeout-seconds
  (pilish-sd-bench--env-int "PI_SD_BENCH_TIMEOUT_SECONDS" 120)
  "Maximum seconds to wait for history or stream settlement.")

(defvar pilish-sd-bench-display-buffers
  (and (pilish-sd-bench--truthy-env-p "PI_SD_BENCH_DISPLAY" "0") t)
  "Whether the benchmark displays chat/input buffers in a GUI frame.")

(defvar pilish-sd-bench-out-dir
  (file-name-as-directory
   (expand-file-name
    (pilish-sd-bench--env
     "PI_SD_BENCH_OUT_DIR" "tmp/stream-delta-bench/standalone")
    pilish-sd-bench-repo-root))
  "Output directory for one stream-delta benchmark iteration.")

(defvar pilish-sd-bench-runner-out-dir
  (file-name-as-directory
   (expand-file-name
    (pilish-sd-bench--env
     "PI_SD_BENCH_RUNNER_OUT_DIR" pilish-sd-bench-out-dir)
    pilish-sd-bench-repo-root))
  "Top-level runner output directory used in reproduction commands.")

(defvar pilish-sd-bench-fake-pi
  (expand-file-name "bench/fake-pi-stream-delta.py"
                    pilish-sd-bench-repo-root)
  "Fake pi executable used by stream-delta runs.")

(defvar pilish-sd-bench-result-file
  (expand-file-name "result.json" pilish-sd-bench-out-dir)
  "JSON result path for one stream-delta run.")

(defvar pilish-sd-bench-report-file
  (expand-file-name "report.md" pilish-sd-bench-out-dir)
  "Markdown report path for one stream-delta run.")

(defvar pilish-sd-bench-times-file
  (expand-file-name "times.tsv" pilish-sd-bench-out-dir)
  "Detailed timing TSV path for one stream-delta run.")

(defvar pilish-sd-bench-fake-log
  (expand-file-name "fake-pi.jsonl" pilish-sd-bench-out-dir)
  "Content-free fake backend log path for one stream-delta run.")

(defconst pilish-sd-bench--backlog-marker
  "SD-BACKLOG-COMPLETE-CONTROL"
  "Unique wire marker terminating the deliberately collected backlog.")

(defvar pilish-sd-bench--phase "setup"
  "Current benchmark phase recorded with `process-filter' rows.")
(defvar pilish-sd-bench--current-filter-id nil
  "Dynamically bound `process-filter' invocation ID.")
(defvar pilish-sd-bench--current-event-type nil
  "Dynamically bound effective display event type.")
(defvar pilish-sd-bench--current-event-phase nil
  "Dynamically bound synthetic event phase.")
(defvar pilish-sd-bench--current-flush-kind nil
  "Dynamically bound stream flush classification.")
(defvar pilish-sd-bench--current-flush-phase nil
  "Dynamically bound stream flush phase.")
(defvar pilish-sd-bench--filter-sequence 0
  "Monotonic real `process-filter' invocation counter.")
(defvar pilish-sd-bench--filter-rows nil
  "Recorded real `process-filter' timing rows, newest first.")
(defvar pilish-sd-bench--flush-rows nil
  "Recorded nonempty stream flush rows, newest first.")
(defvar pilish-sd-bench--display-rows nil
  "Recorded text/thinking display-call rows, newest first.")
(defvar pilish-sd-bench--toolcall-paints 0
  "Number of coalesced toolcall delta paints, excluding immediate starts.")
(defvar pilish-sd-bench--event-counts nil
  "Hash table counting effective display event types.")
(defvar pilish-sd-bench--agent-end-time nil
  "Time when the benchmark agent_end handler returned.")
(defvar pilish-sd-bench--settled-time nil
  "Time when the benchmark agent_settled handler returned.")
(defvar pilish-sd-bench--backlog-filter-ids nil
  "Filter IDs observed for backlog text deltas, newest first.")
(defvar pilish-sd-bench--backlog-boundary-filter-id nil
  "Filter ID that handled the backlog text_end boundary.")
(defvar pilish-sd-bench--collector-active nil
  "Whether raw process output is being collected for one backlog call.")
(defvar pilish-sd-bench--collector-chunks nil
  "Raw backlog transport chunks, newest first.")
(defvar pilish-sd-bench--collector-tail ""
  "Bounded suffix used to detect a split backlog marker.")
(defvar pilish-sd-bench--collector-original-filter nil
  "Real process filter restored after backlog collection.")
(defvar pilish-sd-bench--collector-transport-chunks 0
  "Number of raw transport callbacks collected for the backlog.")
(defvar pilish-sd-bench--collector-transport-bytes 0
  "Bytes collected before the one real backlog filter call.")
(defvar pilish-sd-bench--collector-complete nil
  "Whether the backlog marker was collected and dispatched.")
(defvar pilish-sd-bench--probe-interval 0.05
  "Probe timer interval in seconds.")
(defvar pilish-sd-bench--probe-expected nil
  "Expected wall time of the next probe tick.")
(defvar pilish-sd-bench--probe-lateness nil
  "Probe lateness samples in seconds, newest first.")
(defvar pilish-sd-bench--probe-timer nil
  "Active benchmark probe timer, or nil.")

(defun pilish-sd-bench--percentile (samples fraction)
  "Return FRACTION percentile of numeric SAMPLES."
  (let* ((sorted (sort (copy-sequence samples) #'<))
         (count (length sorted)))
    (if (zerop count)
        0.0
      (nth (min (1- count) (floor (* fraction count))) sorted))))

(defun pilish-sd-bench--probe-tick ()
  "Record lateness relative to the expected probe time."
  (let ((now (float-time)))
    (when pilish-sd-bench--probe-expected
      (push (max 0.0 (- now pilish-sd-bench--probe-expected))
            pilish-sd-bench--probe-lateness))
    (setq pilish-sd-bench--probe-expected
          (+ now pilish-sd-bench--probe-interval))))

(defun pilish-sd-bench--start-probe ()
  "Reset and start the repeating lateness probe."
  (setq pilish-sd-bench--probe-expected nil
        pilish-sd-bench--probe-lateness nil
        pilish-sd-bench--probe-timer
        (run-with-timer 0 pilish-sd-bench--probe-interval
                        #'pilish-sd-bench--probe-tick)))

(defun pilish-sd-bench--stop-probe ()
  "Stop the repeating lateness probe."
  (when (timerp pilish-sd-bench--probe-timer)
    (cancel-timer pilish-sd-bench--probe-timer))
  (setq pilish-sd-bench--probe-timer nil))

(defun pilish-sd-bench--probe-stats ()
  "Return probe lateness summary in milliseconds."
  (let ((samples pilish-sd-bench--probe-lateness))
    (list :intervalMs (* 1000.0 pilish-sd-bench--probe-interval)
          :fires (length samples)
          :p50Ms (* 1000.0
                    (pilish-sd-bench--percentile samples 0.50))
          :p95Ms (* 1000.0
                    (pilish-sd-bench--percentile samples 0.95))
          :maxMs (if samples
                     (* 1000.0 (apply #'max samples))
                   0.0)
          :latenessMs
          (vconcat (mapcar (lambda (sample) (* 1000.0 sample))
                           (nreverse (copy-sequence samples)))))))

(defun pilish-sd-bench--event-type (event)
  "Return the effective type of display EVENT."
  (if (equal (plist-get event :type) "message_update")
      (or (plist-get (plist-get event :assistantMessageEvent) :type)
          "message_update")
    (or (plist-get event :type) "unknown")))

(defun pilish-sd-bench--increment-event (type)
  "Increment the handled event count for TYPE."
  (puthash type (1+ (gethash type pilish-sd-bench--event-counts 0))
           pilish-sd-bench--event-counts))

(defun pilish-sd-bench--event-count (type)
  "Return the handled display event count for TYPE."
  (gethash type pilish-sd-bench--event-counts 0))

(defun pilish-sd-bench--around-process-filter (orig proc output)
  "Call real filter ORIG for PROC and OUTPUT and record its cost."
  (let* ((id (cl-incf pilish-sd-bench--filter-sequence))
         (start (float-time))
         (gc-before gcs-done)
         (gc-time-before gc-elapsed)
         (backlog (string-match-p
                   (regexp-quote pilish-sd-bench--backlog-marker)
                   output))
         (pilish-sd-bench--current-filter-id id)
         value)
    (unwind-protect
        (setq value (funcall orig proc output))
      (push (list :id id
                  :phase pilish-sd-bench--phase
                  :bytes (string-bytes output)
                  :lines (cl-count ?\n output)
                  :wallMs (* 1000.0 (- (float-time) start))
                  :gcs (- gcs-done gc-before)
                  :gcMs (* 1000.0 (- gc-elapsed gc-time-before))
                  :backlog (pilish-sd-bench--json-bool backlog))
            pilish-sd-bench--filter-rows))
    value))

(defun pilish-sd-bench--restore-process-filter (proc)
  "Restore PROC's real process filter after backlog collection."
  (when (and (processp proc) pilish-sd-bench--collector-original-filter)
    (set-process-filter proc pilish-sd-bench--collector-original-filter))
  (setq pilish-sd-bench--collector-active nil))

(defun pilish-sd-bench--collecting-process-filter (proc output)
  "Collect raw PROC OUTPUT and dispatch the complete backlog exactly once."
  (push output pilish-sd-bench--collector-chunks)
  (cl-incf pilish-sd-bench--collector-transport-chunks)
  (cl-incf pilish-sd-bench--collector-transport-bytes
           (string-bytes output))
  (let* ((combined (concat pilish-sd-bench--collector-tail output))
         (marker-found
          (string-match-p
           (regexp-quote pilish-sd-bench--backlog-marker) combined))
         (keep (max 0 (1- (length pilish-sd-bench--backlog-marker)))))
    (setq pilish-sd-bench--collector-tail
          (if (> (length combined) keep)
              (substring combined (- (length combined) keep))
            combined))
    (when marker-found
      (let ((aggregate
             (mapconcat #'identity
                        (nreverse pilish-sd-bench--collector-chunks) ""))
            (real-filter pilish-sd-bench--collector-original-filter))
        (setq pilish-sd-bench--collector-chunks nil
              pilish-sd-bench--collector-tail ""
              pilish-sd-bench--collector-complete t)
        (pilish-sd-bench--restore-process-filter proc)
        (funcall real-filter proc aggregate)))))

(defun pilish-sd-bench--start-backlog-collector (proc)
  "Install the one-shot raw backlog collector on PROC."
  (unless pilish-sd-bench--collector-active
    (setq pilish-sd-bench--collector-original-filter (process-filter proc)
          pilish-sd-bench--collector-chunks nil
          pilish-sd-bench--collector-tail ""
          pilish-sd-bench--collector-transport-chunks 0
          pilish-sd-bench--collector-transport-bytes 0
          pilish-sd-bench--collector-complete nil
          pilish-sd-bench--collector-active t)
    (set-process-filter proc #'pilish-sd-bench--collecting-process-filter)))

(defun pilish-sd-bench--around-handle-event (orig event)
  "Call display-event handler ORIG for EVENT while recording structure."
  (let* ((type (pilish-sd-bench--event-type event))
         (phase (or (plist-get event :benchmarkPhase) "unmarked"))
         (pilish-sd-bench--current-event-type type)
         (pilish-sd-bench--current-event-phase phase)
         value)
    (pilish-sd-bench--increment-event type)
    (when (and (equal phase "backlog")
               (equal type "text_delta"))
      (push pilish-sd-bench--current-filter-id
            pilish-sd-bench--backlog-filter-ids))
    (when (and (equal phase "backlog-boundary")
               (equal type "text_end"))
      (setq pilish-sd-bench--backlog-boundary-filter-id
            pilish-sd-bench--current-filter-id))
    (setq value (funcall orig event))
    (pcase type
      ("benchmark_backlog_ready"
       (let ((proc (or (and (boundp 'pilish--process) pilish--process)
                       (error "Backlog control event has no process"))))
         (pilish-sd-bench--start-backlog-collector proc)
         (pilish--send-string
          proc
          "{\"type\":\"benchmark_backlog_begin\",\"id\":\"sd-bench-backlog-begin\"}\n")))
      ("agent_end"
       (setq pilish-sd-bench--agent-end-time (float-time)))
      ("agent_settled"
       (setq pilish-sd-bench--settled-time (float-time))))
    value))

(defun pilish-sd-bench--around-flush (orig &optional buffer)
  "Call stream flush ORIG for BUFFER and record nonempty flush work."
  (let* ((target (or buffer (current-buffer)))
         (pending (and (buffer-live-p target)
                       (buffer-local-value
                        'pilish--pending-stream-deltas target)))
         (timer (and (buffer-live-p target)
                     (buffer-local-value
                      'pilish--stream-delta-flush-timer target))))
    (if (not (or pending timer))
        (funcall orig buffer)
      (let* ((kind (if pilish-sd-bench--current-filter-id
                       "synchronous" "timer"))
             (phase (or pilish-sd-bench--current-event-phase "timer"))
             (items (length pending))
             (start (float-time))
             (gc-before gcs-done)
             (gc-time-before gc-elapsed)
             (pilish-sd-bench--current-flush-kind kind)
             (pilish-sd-bench--current-flush-phase phase)
             value)
        (unwind-protect
            (setq value (funcall orig buffer))
          (push (list :index (1+ (length pilish-sd-bench--flush-rows))
                      :kind kind
                      :phase phase
                      :boundary pilish-sd-bench--current-event-type
                      :filterId pilish-sd-bench--current-filter-id
                      :items items
                      :wallMs (* 1000.0 (- (float-time) start))
                      :gcs (- gcs-done gc-before)
                      :gcMs (* 1000.0 (- gc-elapsed gc-time-before)))
                pilish-sd-bench--flush-rows))
        value))))

(defun pilish-sd-bench--record-display (kind text elapsed-ms)
  "Record a KIND display call for TEXT taking ELAPSED-MS."
  (push (list :index (1+ (length pilish-sd-bench--display-rows))
              :kind kind
              :flushKind (or pilish-sd-bench--current-flush-kind "unknown")
              :phase (or pilish-sd-bench--current-flush-phase "unknown")
              :chars (length text)
              :wallMs elapsed-ms
              :filterId pilish-sd-bench--current-filter-id)
        pilish-sd-bench--display-rows))

(defun pilish-sd-bench--around-display-text (orig text)
  "Call text-delta renderer ORIG with TEXT and record the display call."
  (let ((start (float-time)))
    (prog1 (funcall orig text)
      (pilish-sd-bench--record-display
       "text" text (* 1000.0 (- (float-time) start))))))

(defun pilish-sd-bench--around-display-thinking (orig text)
  "Call thinking-delta renderer ORIG with TEXT and record the display call."
  (let ((start (float-time)))
    (prog1 (funcall orig text)
      (pilish-sd-bench--record-display
       "thinking" text (* 1000.0 (- (float-time) start))))))

(defun pilish-sd-bench--around-toolcall-paint (orig stream event-type)
  "Call toolcall renderer ORIG for STREAM and EVENT-TYPE, counting delta paints."
  (when (equal event-type "toolcall_delta")
    (cl-incf pilish-sd-bench--toolcall-paints))
  (funcall orig stream event-type))

(defun pilish-sd-bench--install-advice ()
  "Install narrow stream benchmark measurement advice."
  (advice-add 'pilish--process-filter
              :around #'pilish-sd-bench--around-process-filter)
  (advice-add 'pilish--handle-display-event
              :around #'pilish-sd-bench--around-handle-event)
  (advice-add 'pilish--flush-stream-deltas
              :around #'pilish-sd-bench--around-flush)
  (advice-add 'pilish--display-message-delta
              :around #'pilish-sd-bench--around-display-text)
  (advice-add 'pilish--display-thinking-delta
              :around #'pilish-sd-bench--around-display-thinking)
  (advice-add 'pilish--render-toolcall-stream
              :around #'pilish-sd-bench--around-toolcall-paint))

(defun pilish-sd-bench--remove-advice ()
  "Remove all stream benchmark measurement advice."
  (advice-remove 'pilish--process-filter
                 #'pilish-sd-bench--around-process-filter)
  (advice-remove 'pilish--handle-display-event
                 #'pilish-sd-bench--around-handle-event)
  (advice-remove 'pilish--flush-stream-deltas
                 #'pilish-sd-bench--around-flush)
  (advice-remove 'pilish--display-message-delta
                 #'pilish-sd-bench--around-display-text)
  (advice-remove 'pilish--display-thinking-delta
                 #'pilish-sd-bench--around-display-thinking)
  (advice-remove 'pilish--render-toolcall-stream
                 #'pilish-sd-bench--around-toolcall-paint))

(defun pilish-sd-bench--wait-until (predicate timeout)
  "Wait up to TIMEOUT seconds for PREDICATE to return non-nil."
  (let ((deadline (+ (float-time) timeout))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01)
      (when (and pilish-sd-bench-display-buffers (not noninteractive))
        (redisplay t)))
    value))

(defun pilish-sd-bench--pending-requests-count (proc)
  "Return pending RPC request count for PROC."
  (let ((pending (and (processp proc)
                      (process-get proc 'pilish-pending-requests))))
    (if (hash-table-p pending) (hash-table-count pending) 0)))

(defun pilish-sd-bench--text-line (index)
  "Return deterministic text delta line INDEX."
  (format "SD-TEXT-%04d value-%05d"
          index (mod (+ pilish-sd-bench-seed (* index 7919)) 100000)))

(defun pilish-sd-bench--thinking-line (index)
  "Return deterministic thinking delta line INDEX."
  (format "SD-THINK-%04d thought-%05d"
          index (mod (+ pilish-sd-bench-seed (* index 3571)) 100000)))

(defun pilish-sd-bench--backlog-line (index)
  "Return deterministic backlog delta line INDEX."
  (format "SD-BACKLOG-%04d value-%05d"
          index (mod (+ pilish-sd-bench-seed (* index 6151)) 100000)))

(defun pilish-sd-bench--tool-command ()
  "Return the deterministic bash command for the configured toolcall stream."
  (concat "echo SD-BOUNDARY-TOOL"
          (when (> pilish-sd-bench-toolcall-repeats 0)
            (concat "; "
                    (mapconcat (lambda (index) (format "echo SD-CMD-%04d; " index))
                               (number-sequence 0 (1- pilish-sd-bench-toolcall-repeats))
                               "")))))

(defun pilish-sd-bench--toolcall-delta-count ()
  "Return the expected number of raw toolcall argument deltas."
  (if (> pilish-sd-bench-toolcall-repeats 0)
      (ceiling (/ (float (length (json-serialize
                                 (list :command (pilish-sd-bench--tool-command)))))
                  32))
    1))

(defun pilish-sd-bench--expected-projection ()
  "Return exact deterministic visible marker projection."
  (string-join
   (cl-loop for (kind . line) in (pilish-sd-bench--projection-line-specs)
            unless (eq kind 'omit) collect line)
   "\n"))

(defun pilish-sd-bench--projection-line-specs ()
  "Return ordered line specifications for the complete visible stream span."
  (append
   '((omit . "Assistant")
     ;; `pilish--visible-text' has already omitted the display-empty underline.
     (omit . ""))
   (cl-loop for index below pilish-sd-bench-timer-text-deltas
            collect (cons 'payload (pilish-sd-bench--text-line index)))
   '((omit . ""))
   (cl-loop for index below pilish-sd-bench-thinking-deltas
            collect (cons 'thinking (pilish-sd-bench--thinking-line index)))
   '((omit . ""))
   (cl-loop for index below pilish-sd-bench-backlog-deltas
            collect (cons 'payload (pilish-sd-bench--backlog-line index)))
   (list '(omit . "")
         (cons 'tool (substring (pilish-sd-bench--tool-command) 5))
         '(omit . ""))))

(defun pilish-sd-bench--actual-projection (chat-buf)
  "Return CHAT-BUF's exact normalized visible stream-span projection.
The span starts at the post-history assistant header and extends to buffer end.
Only exact renderer chrome at its expected line is omitted or stripped; every
other visible line is retained so unexpected text fails projection equality."
  (with-current-buffer chat-buf
    (let* ((visible
            (substring-no-properties
             (pilish--visible-text (point-min) (point-max))))
           (anchor "run the deterministic stream-delta benchmark\n\n")
           (anchor-start (string-match (regexp-quote anchor) visible)))
      (unless anchor-start
        (error "Stream-span anchor not found in rendered benchmark buffer"))
      (when (string-match-p "SD-" (substring visible 0 anchor-start))
        (error "Synthetic replay prefix contains reserved SD- marker"))
      (let ((lines (split-string
                    (substring visible (+ anchor-start (length anchor)))
                    "\n" nil))
            (specs (pilish-sd-bench--projection-line-specs))
            normalized)
        (dolist (line lines)
          (let ((spec (pop specs)))
            (pcase (car-safe spec)
              ('omit
               (unless (equal line (cdr spec))
                 (push line normalized)))
              ('thinking
               (push (if (equal line (concat "> " (cdr spec)))
                         (cdr spec)
                       line)
                     normalized))
              ('tool
               (push (if (equal line (concat "$ echo " (cdr spec)))
                         (cdr spec)
                       line)
                     normalized))
              (_ (push line normalized)))))
        (string-join (nreverse normalized) "\n")))))

(defun pilish-sd-bench--dirty-ranges ()
  "Return guarded md-ts dirty-range diagnostics in the current buffer."
  (if (not (fboundp 'md-ts--font-lock-dirty-side-effect-bounds))
      (list :supported :json-false :count nil)
    (condition-case err
        (let ((ranges (md-ts--font-lock-dirty-side-effect-bounds)))
          (list :supported t :count (length ranges)))
      (error
       (list :supported :json-false
             :count nil
             :error (error-message-string err))))))

(defun pilish-sd-bench--rows-in-order (rows)
  "Return newest-first metric ROWS in chronological order."
  (nreverse (copy-sequence rows)))

(defun pilish-sd-bench--rows-of-kind (rows key value)
  "Return ROWS whose KEY equals VALUE."
  (seq-filter (lambda (row) (equal (plist-get row key) value)) rows))

(defun pilish-sd-bench--check (name ok detail)
  "Return correctness check NAME with OK and DETAIL."
  (list :name name :ok (pilish-sd-bench--json-bool ok) :detail detail))

(defun pilish-sd-bench--collect-checks
    (chat-buf proc history-count history-bytes projection md-ts)
  "Return correctness results for CHAT-BUF and PROC.
HISTORY-COUNT and HISTORY-BYTES describe replay; PROJECTION and MD-TS describe
the settled rendered buffer."
  (let* ((full (equal pilish-sd-bench-scenario "full"))
         (large-history (member pilish-sd-bench-scenario '("full" "toolcall")))
         (toolcall-deltas (pilish-sd-bench--toolcall-delta-count))
         (expected-history (* 2 pilish-sd-bench-history-turns))
         (text-events (+ pilish-sd-bench-timer-text-deltas
                         pilish-sd-bench-backlog-deltas))
         (delta-events (+ text-events pilish-sd-bench-thinking-deltas))
         (filter-rows (pilish-sd-bench--rows-in-order
                       pilish-sd-bench--filter-rows))
         (backlog-filters
          (seq-filter (lambda (row) (eq (plist-get row :backlog) t))
                      filter-rows))
         (flushes (pilish-sd-bench--rows-in-order
                   pilish-sd-bench--flush-rows))
         (timer-flushes (pilish-sd-bench--rows-of-kind
                         flushes :kind "timer"))
         (sync-flushes (pilish-sd-bench--rows-of-kind
                        flushes :kind "synchronous"))
         (displays (pilish-sd-bench--rows-in-order
                    pilish-sd-bench--display-rows))
         (text-displays (pilish-sd-bench--rows-of-kind
                         displays :kind "text"))
         (thinking-displays (pilish-sd-bench--rows-of-kind
                             displays :kind "thinking"))
         (backlog-displays
          (seq-filter (lambda (row)
                        (equal (plist-get row :phase) "backlog-boundary"))
                      text-displays))
         (unique-backlog-ids
          (delete-dups (copy-sequence
                        pilish-sd-bench--backlog-filter-ids)))
         (expected-projection (pilish-sd-bench--expected-projection))
         checks)
    (push (pilish-sd-bench--check
           "history-replayed"
           (= history-count expected-history)
           (format "expected %d messages, loaded %s"
                   expected-history history-count))
          checks)
    (push (pilish-sd-bench--check
           "representative-history-size"
           (if large-history
               (<= (* 400 1024) history-bytes (* 500 1024))
             (> history-bytes 0))
           (format "%d rendered bytes%s" history-bytes
                   (if large-history " (required 409600..512000)" "")))
          checks)
    (push (pilish-sd-bench--check
           "exact-stream-payload-visible-projection"
           (equal projection expected-projection)
           (format "expected %d lines/%s, got %d lines/%s"
                   (+ delta-events 1)
                   (secure-hash 'sha256 expected-projection)
                   (if (string-empty-p projection)
                       0 (length (split-string projection "\n")))
                   (secure-hash 'sha256 projection)))
          checks)
    (push (pilish-sd-bench--check
           "representative-text-delta-count"
           (if full (<= 900 text-events 1100) (> text-events 0))
           (format "%d text deltas%s" text-events
                   (if full " (required 900..1100)" "")))
          checks)
    (push (pilish-sd-bench--check
           "exact-delta-event-counts"
           (and (= (pilish-sd-bench--event-count "text_delta")
                   text-events)
                (= (pilish-sd-bench--event-count "thinking_delta")
                   pilish-sd-bench-thinking-deltas))
           (format "text=%d/%d thinking=%d/%d"
                   (pilish-sd-bench--event-count "text_delta") text-events
                   (pilish-sd-bench--event-count "thinking_delta")
                   pilish-sd-bench-thinking-deltas))
          checks)
    (push (pilish-sd-bench--check
           "exact-stream-boundary-counts"
           (and (= (pilish-sd-bench--event-count "text_start") 2)
                (= (pilish-sd-bench--event-count "text_end") 2)
                (= (pilish-sd-bench--event-count "thinking_start") 1)
                (= (pilish-sd-bench--event-count "thinking_end") 1)
                (= (pilish-sd-bench--event-count "toolcall_start") 1)
                (= (pilish-sd-bench--event-count "toolcall_delta") toolcall-deltas)
                (= (pilish-sd-bench--event-count "toolcall_end") 1))
           (format "text=%d/%d thinking=%d/%d tool=%d/%d/%d"
                   (pilish-sd-bench--event-count "text_start")
                   (pilish-sd-bench--event-count "text_end")
                   (pilish-sd-bench--event-count "thinking_start")
                   (pilish-sd-bench--event-count "thinking_end")
                   (pilish-sd-bench--event-count "toolcall_start")
                   (pilish-sd-bench--event-count "toolcall_delta")
                   (pilish-sd-bench--event-count "toolcall_end")))
          checks)
    (push (pilish-sd-bench--check
           "complete-lifecycle"
           (and (= (pilish-sd-bench--event-count "agent_start") 1)
                (= (pilish-sd-bench--event-count "message_start") 1)
                (= (pilish-sd-bench--event-count "message_end") 1)
                (= (pilish-sd-bench--event-count "agent_end") 1)
                (= (pilish-sd-bench--event-count "agent_settled") 1))
           (format "agent_start=%d message=%d/%d agent_end=%d settled=%d"
                   (pilish-sd-bench--event-count "agent_start")
                   (pilish-sd-bench--event-count "message_start")
                   (pilish-sd-bench--event-count "message_end")
                   (pilish-sd-bench--event-count "agent_end")
                   (pilish-sd-bench--event-count "agent_settled")))
          checks)
    (let ((status (and (buffer-live-p chat-buf)
                       (buffer-local-value 'pilish--status chat-buf))))
      (push (pilish-sd-bench--check
             "final-state-idle" (eq status 'idle)
             (format "status=%S" status))
            checks))
    (let ((pending (and (buffer-live-p chat-buf)
                        (buffer-local-value
                         'pilish--pending-stream-deltas chat-buf)))
          (timer (and (buffer-live-p chat-buf)
                      (buffer-local-value
                       'pilish--stream-delta-flush-timer chat-buf))))
      (push (pilish-sd-bench--check
             "stream-flush-state-clean"
             (and (null pending) (null timer))
             (format "pending=%d timer=%S" (length pending) timer))
            checks))
    (push (pilish-sd-bench--check
           "rpc-requests-drained"
           (zerop (pilish-sd-bench--pending-requests-count proc))
           (format "pending=%d"
                   (pilish-sd-bench--pending-requests-count proc)))
          checks)
    (push (pilish-sd-bench--check
           "timer-and-synchronous-flushes-observed"
           (and (> pilish-sd-bench-burst-pause-ms
                   (* 1000 pilish--stream-delta-render-interval))
                (>= (length timer-flushes) (if full 5 1))
                (>= (length sync-flushes) 1))
           (format "pause=%d ms; timer=%d (minimum %d) synchronous=%d"
                   pilish-sd-bench-burst-pause-ms
                   (length timer-flushes) (if full 5 1)
                   (length sync-flushes)))
          checks)
    (push (pilish-sd-bench--check
           "display-calls-coalesced"
           (and text-displays thinking-displays
                (< (* 10 (length displays)) delta-events))
           (format "%d display calls for %d deltas (%.4f)"
                   (length displays) delta-events
                   (/ (float (length displays)) delta-events)))
          checks)
    (push (pilish-sd-bench--check
           "toolcall-paints-coalesced"
           (and (> pilish-sd-bench--toolcall-paints 0)
                (if (> toolcall-deltas 1)
                    (< pilish-sd-bench--toolcall-paints toolcall-deltas)
                  (= pilish-sd-bench--toolcall-paints 1)))
           (format "%d paints for %d deltas; command length %d"
                   pilish-sd-bench--toolcall-paints toolcall-deltas
                   (length (pilish-sd-bench--tool-command))))
          checks)
    (push (pilish-sd-bench--check
           "backlog-used-one-real-filter"
           (and pilish-sd-bench--collector-complete
                (>= pilish-sd-bench-backlog-deltas (if full 100 2))
                (= (length backlog-filters) 1)
                (= (length pilish-sd-bench--backlog-filter-ids)
                   pilish-sd-bench-backlog-deltas)
                (= (length unique-backlog-ids) 1)
                (equal (car unique-backlog-ids)
                       pilish-sd-bench--backlog-boundary-filter-id)
                (equal (car unique-backlog-ids)
                       (plist-get (car backlog-filters) :id)))
           (format "transport=%d chunks/%d bytes; events=%d filters=%S boundary=%S"
                   pilish-sd-bench--collector-transport-chunks
                   pilish-sd-bench--collector-transport-bytes
                   (length pilish-sd-bench--backlog-filter-ids)
                   unique-backlog-ids
                   pilish-sd-bench--backlog-boundary-filter-id))
          checks)
    (push (pilish-sd-bench--check
           "backlog-grouped-into-one-display-call"
           (and (= (length backlog-displays) 1)
                (equal (plist-get (car backlog-displays) :flushKind)
                       "synchronous"))
           (format "%d backlog text display calls; kind=%S"
                   (length backlog-displays)
                   (and backlog-displays
                        (plist-get (car backlog-displays) :flushKind))))
          checks)
    (push (pilish-sd-bench--check
           "md-ts-dirty-range-probe-guarded"
           (memq (plist-get md-ts :supported) '(t :json-false))
           (format "supported=%S before=%S after=%S"
                   (plist-get md-ts :supported)
                   (plist-get md-ts :beforeCount)
                   (plist-get md-ts :afterCount)))
          checks)
    (nreverse checks)))

(defun pilish-sd-bench--cleanup-session (chat-buf)
  "Kill CHAT-BUF, its input buffer, and its fake process."
  (when (buffer-live-p chat-buf)
    (let ((input (buffer-local-value 'pilish--input-buffer chat-buf))
          (proc (buffer-local-value 'pilish--process chat-buf)))
      (when (processp proc)
        (pilish-sd-bench--restore-process-filter proc)
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (delete-process proc)))
      (kill-buffer chat-buf)
      (when (buffer-live-p input)
        (kill-buffer input)))))

(defun pilish-sd-bench--run-session ()
  "Run one fake-backed history and stream session and return metrics."
  (let* ((project-dir (expand-file-name "synthetic-project"
                                        pilish-sd-bench-out-dir))
         (chat-buf nil)
         (proc nil)
         (history-count -1)
         (history-complete nil)
         (history-bytes 0)
         (settled nil)
         (error-text nil)
         (start nil)
         (end nil)
         (gc-before nil)
         (gc-time-before nil)
         (measured-gcs nil)
         (measured-gc-seconds nil)
         (projection "")
         (dirty-before nil)
         (dirty-after nil)
         (md-ts nil)
         checks)
    (make-directory project-dir t)
    (setq pilish-executable
          (list (or (executable-find "python3")
                    (error "Python3 not found"))
                pilish-sd-bench-fake-pi)
          pilish-extra-args (list "--log-file" pilish-sd-bench-fake-log)
          pilish-essential-grammar-action 'warn
          pilish-thinking-display 'visible)
    (unwind-protect
        (condition-case err
            (progn
              (let ((pilish--version-probe-delay 3600))
                (setq chat-buf (pilish--setup-session project-dir)))
              (setq proc (buffer-local-value 'pilish--process chat-buf))
              (unless (pilish-sd-bench--wait-until
                       (lambda ()
                         (and (process-live-p proc)
                              (zerop
                               (pilish-sd-bench--pending-requests-count proc))
                              (buffer-local-value 'pilish--state chat-buf)))
                       pilish-sd-bench-timeout-seconds)
                (error "Timed out waiting for startup RPCs"))
              (setq pilish-sd-bench--phase "history")
              (pilish--load-session-history
               proc
               (lambda (count) (setq history-count count))
               chat-buf
               (lambda (_response) (setq history-complete t)))
              (unless (pilish-sd-bench--wait-until
                       (lambda ()
                         (and history-complete
                              (>= history-count 0)
                              (zerop
                               (pilish-sd-bench--pending-requests-count proc))))
                       pilish-sd-bench-timeout-seconds)
                (error "Timed out replaying synthetic history"))
              (with-current-buffer chat-buf
                (setq history-bytes (string-bytes (buffer-string)))
                (font-lock-ensure (point-min) (point-max)))
              (when (and pilish-sd-bench-display-buffers
                         (not noninteractive))
                (pilish--show-session-buffers
                 chat-buf
                 (buffer-local-value 'pilish--input-buffer chat-buf))
                (redisplay t))
              (garbage-collect)
              (setq gc-before gcs-done
                    gc-time-before gc-elapsed
                    start (float-time)
                    pilish-sd-bench--phase "stream")
              (pilish-sd-bench--start-probe)
              (with-current-buffer chat-buf
                (pilish--prepare-and-send
                 "run the deterministic stream-delta benchmark"))
              (setq settled
                    (pilish-sd-bench--wait-until
                     (lambda ()
                       (unless (process-live-p proc)
                         (error "Fake pi exited before stream settlement"))
                       (and pilish-sd-bench--settled-time
                            (eq (buffer-local-value
                                 'pilish--status chat-buf) 'idle)
                            (not pilish-sd-bench--collector-active)
                            (zerop
                             (pilish-sd-bench--pending-requests-count proc))))
                     pilish-sd-bench-timeout-seconds))
              (setq end (float-time)
                    measured-gcs (- gcs-done gc-before)
                    measured-gc-seconds (- gc-elapsed gc-time-before))
              (pilish-sd-bench--stop-probe)
              (unless settled
                (error "Timed out waiting for agent_settled"))
              (with-current-buffer chat-buf
                (setq dirty-before (pilish-sd-bench--dirty-ranges))
                (font-lock-ensure (point-min) (point-max))
                (setq dirty-after (pilish-sd-bench--dirty-ranges)
                      projection
                      (pilish-sd-bench--actual-projection chat-buf)))
              (setq md-ts
                    (list :supported (plist-get dirty-before :supported)
                          :beforeCount (plist-get dirty-before :count)
                          :afterCount (plist-get dirty-after :count))))
          (error
           (setq error-text (error-message-string err)
                 end (float-time))
           (when gc-before
             (setq measured-gcs (- gcs-done gc-before)
                   measured-gc-seconds (- gc-elapsed gc-time-before)))))
      (pilish-sd-bench--stop-probe))
    (unless md-ts
      (setq md-ts (list :supported :json-false
                        :beforeCount nil
                        :afterCount nil)))
    (setq checks
          (pilish-sd-bench--collect-checks
           chat-buf proc history-count history-bytes projection md-ts))
    (unwind-protect
        (list :settled (pilish-sd-bench--json-bool settled)
              :error error-text
              :historyCount history-count
              :historyBytes history-bytes
              :wallMs (and start end (* 1000.0 (- end start)))
              :settlementMs
              (and start pilish-sd-bench--settled-time
                   (* 1000.0 (- pilish-sd-bench--settled-time start)))
              :agentEndMs
              (and start pilish-sd-bench--agent-end-time
                   (* 1000.0 (- pilish-sd-bench--agent-end-time start)))
              :gcs measured-gcs
              :gcSeconds measured-gc-seconds
              :bufferBytes
              (and (buffer-live-p chat-buf)
                   (with-current-buffer chat-buf
                     (string-bytes (buffer-string))))
              :bufferLines
              (and (buffer-live-p chat-buf)
                   (with-current-buffer chat-buf
                     (count-lines (point-min) (point-max))))
              :projection projection
              :mdTs md-ts
              :checks checks)
      (pilish-sd-bench--cleanup-session chat-buf))))

(defun pilish-sd-bench--git-string (&rest args)
  "Run git with ARGS at the repository root and return trimmed output."
  (string-trim
   (with-temp-buffer
     (let ((default-directory pilish-sd-bench-repo-root))
       (if (zerop (apply #'process-file "git" nil t nil args))
           (buffer-string)
         "")))))

(defun pilish-sd-bench--sum-row-field (rows field)
  "Sum numeric FIELD across ROWS."
  (cl-loop for row in rows sum (or (plist-get row field) 0)))

(defun pilish-sd-bench--max-row-field (rows field)
  "Return maximum numeric FIELD across ROWS, or zero."
  (if rows
      (apply #'max (mapcar (lambda (row) (or (plist-get row field) 0)) rows))
    0.0))

(defun pilish-sd-bench--metrics-ok-p (metrics)
  "Return whether METRICS settled and every correctness check passed."
  (and (eq (plist-get metrics :settled) t)
       (seq-every-p (lambda (check) (eq (plist-get check :ok) t))
                    (plist-get metrics :checks))))

(defun pilish-sd-bench--workload-json ()
  "Return configured workload as a JSON-encodable plist."
  (list :historyTurns pilish-sd-bench-history-turns
        :historyTextBytes pilish-sd-bench-history-text-bytes
        :timerTextDeltas pilish-sd-bench-timer-text-deltas
        :textBurst pilish-sd-bench-text-burst
        :thinkingDeltas pilish-sd-bench-thinking-deltas
        :thinkingBurst pilish-sd-bench-thinking-burst
        :backlogDeltas pilish-sd-bench-backlog-deltas
        :toolcallRepeats pilish-sd-bench-toolcall-repeats
        :burstPauseMs pilish-sd-bench-burst-pause-ms
        :seed pilish-sd-bench-seed))

(defun pilish-sd-bench--write-times-tsv ()
  "Write `process-filter', flush, display, and probe rows to timing TSV."
  (with-temp-file pilish-sd-bench-times-file
    (insert (concat "series\tindex\tphase\tkind\titems\tbytes\tchars\t"
                    "wall_ms\tgcs\tgc_ms\tfilter_id\tboundary\tbacklog\n"))
    (dolist (row (pilish-sd-bench--rows-in-order
                  pilish-sd-bench--filter-rows))
      (insert (format "process-filter\t%d\t%s\tfilter\t%d\t%d\t\t%.3f\t%d\t%.3f\t%d\t\t%s\n"
                      (plist-get row :id) (plist-get row :phase)
                      (plist-get row :lines) (plist-get row :bytes)
                      (plist-get row :wallMs) (plist-get row :gcs)
                      (plist-get row :gcMs) (plist-get row :id)
                      (if (eq (plist-get row :backlog) t) 1 0))))
    (dolist (row (pilish-sd-bench--rows-in-order
                  pilish-sd-bench--flush-rows))
      (insert (format "flush\t%d\t%s\t%s\t%d\t\t\t%.3f\t%d\t%.3f\t%s\t%s\t\n"
                      (plist-get row :index) (plist-get row :phase)
                      (plist-get row :kind) (plist-get row :items)
                      (plist-get row :wallMs) (plist-get row :gcs)
                      (plist-get row :gcMs) (plist-get row :filterId)
                      (plist-get row :boundary))))
    (dolist (row (pilish-sd-bench--rows-in-order
                  pilish-sd-bench--display-rows))
      (insert (format "display\t%d\t%s\t%s\t1\t\t%d\t%.3f\t\t\t%s\t\t\n"
                      (plist-get row :index) (plist-get row :phase)
                      (plist-get row :kind) (plist-get row :chars)
                      (plist-get row :wallMs) (plist-get row :filterId))))
    (cl-loop for sample in (plist-get (pilish-sd-bench--probe-stats)
                                      :latenessMs)
             for index from 1 do
             (insert (format "probe\t%d\tstream\tlateness\t1\t\t\t%.3f\t\t\t\t\t\n"
                             index sample)))))

(defun pilish-sd-bench--write-result-json (metrics run-ok)
  "Write METRICS and RUN-OK verdict to the JSON artifact."
  (let* ((filters (pilish-sd-bench--rows-in-order
                   pilish-sd-bench--filter-rows))
         (stream-filters
          (pilish-sd-bench--rows-of-kind filters :phase "stream"))
         (backlog-filter
          (car (seq-filter (lambda (row)
                             (eq (plist-get row :backlog) t))
                           stream-filters)))
         (flushes (pilish-sd-bench--rows-in-order
                   pilish-sd-bench--flush-rows))
         (timer-flushes (pilish-sd-bench--rows-of-kind
                         flushes :kind "timer"))
         (sync-flushes (pilish-sd-bench--rows-of-kind
                        flushes :kind "synchronous"))
         (displays (pilish-sd-bench--rows-in-order
                    pilish-sd-bench--display-rows))
         (text-displays (pilish-sd-bench--rows-of-kind
                         displays :kind "text"))
         (thinking-displays (pilish-sd-bench--rows-of-kind
                             displays :kind "thinking"))
         (probe (pilish-sd-bench--probe-stats))
         (projection (plist-get metrics :projection))
         (expected (pilish-sd-bench--expected-projection))
         (dirty (not (string-empty-p
                      (pilish-sd-bench--git-string
                       "status" "--porcelain" "--untracked-files=no"))))
         (object
          (list
           :scenario pilish-sd-bench-scenario
           :iteration pilish-sd-bench-iteration
           :commit (pilish-sd-bench--git-string "rev-parse" "--short" "HEAD")
           :dirty (pilish-sd-bench--json-bool dirty)
           :display (pilish-sd-bench--json-bool
                     pilish-sd-bench-display-buffers)
           :emacsVersion emacs-version
           :markdownGrammar
           (pilish-sd-bench--json-bool
            (treesit-language-available-p 'markdown))
           :timingPolicy "diagnostic-only"
           :ok (pilish-sd-bench--json-bool run-ok)
           :settled (plist-get metrics :settled)
           :error (plist-get metrics :error)
           :workload (pilish-sd-bench--workload-json)
           :history (list :messages (plist-get metrics :historyCount)
                          :renderedBytes (plist-get metrics :historyBytes))
           :wallMs (plist-get metrics :wallMs)
           :settlementMs (plist-get metrics :settlementMs)
           :agentEndMs (plist-get metrics :agentEndMs)
           :gc (list :collections (plist-get metrics :gcs)
                     :seconds (plist-get metrics :gcSeconds))
           :buffer (list :bytes (plist-get metrics :bufferBytes)
                         :lines (plist-get metrics :bufferLines))
           :processFilters
           (list :count (length stream-filters)
                 :totalMs (pilish-sd-bench--sum-row-field
                           stream-filters :wallMs)
                 :maxMs (pilish-sd-bench--max-row-field
                         stream-filters :wallMs)
                 :backlog backlog-filter
                 :backlogTransport
                 (list :chunks pilish-sd-bench--collector-transport-chunks
                       :bytes pilish-sd-bench--collector-transport-bytes)
                 :rows (vconcat stream-filters))
           :flushes
           (list :total (length flushes)
                 :timerDriven (length timer-flushes)
                 :synchronous (length sync-flushes)
                 :totalMs (pilish-sd-bench--sum-row-field flushes :wallMs)
                 :maxMs (pilish-sd-bench--max-row-field flushes :wallMs)
                 :rows (vconcat flushes))
           :displayCalls
           (list :text (length text-displays)
                 :thinking (length thinking-displays)
                 :total (length displays)
                 :deltaEvents (+ pilish-sd-bench-timer-text-deltas
                                 pilish-sd-bench-backlog-deltas
                                 pilish-sd-bench-thinking-deltas)
                 :ratio (/ (float (length displays))
                           (+ pilish-sd-bench-timer-text-deltas
                              pilish-sd-bench-backlog-deltas
                              pilish-sd-bench-thinking-deltas))
                 :rows (vconcat displays))
           :toolcallPaints
           (list :count pilish-sd-bench--toolcall-paints
                 :deltaEvents (pilish-sd-bench--toolcall-delta-count)
                 :ratio (/ (float pilish-sd-bench--toolcall-paints)
                           (pilish-sd-bench--toolcall-delta-count)))
           :probe probe
           :mdTs (plist-get metrics :mdTs)
           :projection
           (list :exact (pilish-sd-bench--json-bool
                         (equal projection expected))
                 :expectedLines (+ pilish-sd-bench-timer-text-deltas
                                   pilish-sd-bench-thinking-deltas
                                   pilish-sd-bench-backlog-deltas 1)
                 :actualLines (if (string-empty-p projection)
                                  0 (length (split-string projection "\n")))
                 :expectedSha256 (secure-hash 'sha256 expected)
                 :actualSha256 (secure-hash 'sha256 projection))
           :checks (vconcat (plist-get metrics :checks)))))
    (with-temp-file pilish-sd-bench-result-file
      (insert (json-encode object) "\n"))))

(defun pilish-sd-bench--write-report (metrics run-ok)
  "Write Markdown report for METRICS and RUN-OK verdict."
  (let* ((filters (pilish-sd-bench--rows-of-kind
                   (pilish-sd-bench--rows-in-order
                    pilish-sd-bench--filter-rows)
                   :phase "stream"))
         (backlog (car (seq-filter (lambda (row)
                                    (eq (plist-get row :backlog) t))
                                  filters)))
         (flushes (pilish-sd-bench--rows-in-order
                   pilish-sd-bench--flush-rows))
         (timer-count (length (pilish-sd-bench--rows-of-kind
                               flushes :kind "timer")))
         (sync-count (length (pilish-sd-bench--rows-of-kind
                              flushes :kind "synchronous")))
         (displays (pilish-sd-bench--rows-in-order
                    pilish-sd-bench--display-rows))
         (text-count (length (pilish-sd-bench--rows-of-kind
                              displays :kind "text")))
         (thinking-count (length (pilish-sd-bench--rows-of-kind
                                  displays :kind "thinking")))
         (delta-count (+ pilish-sd-bench-timer-text-deltas
                         pilish-sd-bench-backlog-deltas
                         pilish-sd-bench-thinking-deltas))
         (probe (pilish-sd-bench--probe-stats))
         (md-ts (plist-get metrics :mdTs)))
    (with-temp-file pilish-sd-bench-report-file
      (insert "# Pilish stream-delta benchmark\n\n")
      (insert "Synthetic deterministic workload only; timing values are diagnostic and never pass/fail thresholds.\n\n")
      (insert (format "- Verdict: `%s`\n" (if run-ok "pass" "FAIL")))
      (insert (format "- Scenario: `%s`; iteration: `%d`\n"
                      pilish-sd-bench-scenario pilish-sd-bench-iteration))
      (insert (format "- Commit: `%s`; Emacs: `%s`; GUI: `%s`\n"
                      (pilish-sd-bench--git-string
                       "rev-parse" "--short" "HEAD")
                      emacs-version
                      (if pilish-sd-bench-display-buffers "yes" "no")))
      (insert "\n## Reproduction\n\n```sh\n")
      (insert (format "./bench/run-stream-delta-bench.sh %s --scenario %s -c 1 --out-dir %s\n"
                      (if pilish-sd-bench-display-buffers "" "--batch")
                      pilish-sd-bench-scenario
                      pilish-sd-bench-runner-out-dir))
      (insert "```\n\n## Workload and result\n\n")
      (insert (format "- Existing transcript: `%d` messages, `%d` rendered bytes\n"
                      (plist-get metrics :historyCount)
                      (plist-get metrics :historyBytes)))
      (insert (format "- Deltas: `%d` text (`%d` backlog), `%d` thinking; pause `%d` ms\n"
                      (+ pilish-sd-bench-timer-text-deltas
                         pilish-sd-bench-backlog-deltas)
                      pilish-sd-bench-backlog-deltas
                      pilish-sd-bench-thinking-deltas
                      pilish-sd-bench-burst-pause-ms))
      (insert (format "- Wall: `%.1f` ms; agent_end: `%.1f` ms; settlement: `%.1f` ms\n"
                      (or (plist-get metrics :wallMs) 0.0)
                      (or (plist-get metrics :agentEndMs) 0.0)
                      (or (plist-get metrics :settlementMs) 0.0)))
      (insert (format "- Timed-interval GC: `%s` collections / `%s` s\n"
                      (or (plist-get metrics :gcs) "n/a")
                      (or (plist-get metrics :gcSeconds) "n/a")))
      (insert (format "- Process filters: `%d`, total `%.3f` ms, max `%.3f` ms\n"
                      (length filters)
                      (pilish-sd-bench--sum-row-field filters :wallMs)
                      (pilish-sd-bench--max-row-field filters :wallMs)))
      (insert (format "- Backlog filter: `%s` deltas in filter `%s`, `%s` bytes / `%.3f` ms; transport chunks `%d`\n"
                      pilish-sd-bench-backlog-deltas
                      (or (plist-get backlog :id) "n/a")
                      (or (plist-get backlog :bytes) "n/a")
                      (or (plist-get backlog :wallMs) 0.0)
                      pilish-sd-bench--collector-transport-chunks))
      (insert (format "- Flushes: `%d` timer-driven, `%d` synchronous\n"
                      timer-count sync-count))
      (insert (format "- Display calls: `%d` text + `%d` thinking = `%d` for `%d` deltas (ratio `%.4f`)\n"
                      text-count thinking-count (length displays) delta-count
                      (/ (float (length displays)) delta-count)))
      (insert (format "- Toolcall paints: `%d` for `%d` raw deltas; command length `%d`\n"
                      pilish-sd-bench--toolcall-paints
                      (pilish-sd-bench--toolcall-delta-count)
                      (length (pilish-sd-bench--tool-command))))
      (insert (format "- Probe lateness p95/max: `%.3f`/`%.3f` ms\n"
                      (plist-get probe :p95Ms) (plist-get probe :maxMs)))
      (insert (format "- md-ts dirty ranges before/after final fontification: `%s`/`%s` (supported `%s`)\n"
                      (plist-get md-ts :beforeCount)
                      (plist-get md-ts :afterCount)
                      (plist-get md-ts :supported)))
      (insert "\n## Correctness checks\n\n")
      (insert "| check | ok | detail |\n|---|---|---|\n")
      (dolist (check (plist-get metrics :checks))
        (insert (format "| `%s` | %s | %s |\n"
                        (plist-get check :name)
                        (if (eq (plist-get check :ok) t) "yes" "NO")
                        (plist-get check :detail))))
      (insert "\n## Artifacts\n\n")
      (insert (format "- JSON: `%s`\n- Timing TSV: `%s`\n- Fake log: `%s`\n"
                      pilish-sd-bench-result-file
                      pilish-sd-bench-times-file
                      pilish-sd-bench-fake-log)))))

;;;###autoload
(defun pilish-sd-bench-run ()
  "Run one stream-delta benchmark and write artifacts.
Return non-nil only when settlement and every correctness assertion pass."
  (when (and pilish-sd-bench-display-buffers
             (not noninteractive)
             (not (display-graphic-p)))
    (error "GUI stream benchmark requires a graphic display"))
  (make-directory pilish-sd-bench-out-dir t)
  (ignore-errors (delete-file pilish-sd-bench-fake-log))
  (setq pilish-sd-bench--phase "setup"
        pilish-sd-bench--current-filter-id nil
        pilish-sd-bench--filter-sequence 0
        pilish-sd-bench--filter-rows nil
        pilish-sd-bench--flush-rows nil
        pilish-sd-bench--display-rows nil
        pilish-sd-bench--toolcall-paints 0
        pilish-sd-bench--event-counts (make-hash-table :test 'equal)
        pilish-sd-bench--agent-end-time nil
        pilish-sd-bench--settled-time nil
        pilish-sd-bench--backlog-filter-ids nil
        pilish-sd-bench--backlog-boundary-filter-id nil
        pilish-sd-bench--collector-active nil
        pilish-sd-bench--collector-chunks nil
        pilish-sd-bench--collector-tail ""
        pilish-sd-bench--collector-original-filter nil
        pilish-sd-bench--collector-transport-chunks 0
        pilish-sd-bench--collector-transport-bytes 0
        pilish-sd-bench--collector-complete nil
        pilish-sd-bench--probe-expected nil
        pilish-sd-bench--probe-lateness nil
        pilish-sd-bench--probe-timer nil)
  (pilish-sd-bench--install-advice)
  (unwind-protect
      (let* ((metrics (pilish-sd-bench--run-session))
             (run-ok (pilish-sd-bench--metrics-ok-p metrics)))
        (pilish-sd-bench--write-times-tsv)
        (pilish-sd-bench--write-result-json metrics run-ok)
        (pilish-sd-bench--write-report metrics run-ok)
        (princ (format "Wrote %s\n" pilish-sd-bench-result-file))
        (princ (format "Wrote %s\n" pilish-sd-bench-times-file))
        (princ (format "Wrote %s\n" pilish-sd-bench-report-file))
        run-ok)
    (pilish-sd-bench--stop-probe)
    (pilish-sd-bench--remove-advice)))

(defun pilish-sd-bench-run-batch ()
  "Run one stream-delta benchmark in batch mode and exit."
  (let ((standard-output #'external-debugging-output))
    (kill-emacs (if (pilish-sd-bench-run) 0 1))))

(provide 'pilish-stream-delta-bench)
;;; pilish-stream-delta-bench.el ends here
