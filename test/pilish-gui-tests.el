;;; pilish-gui-tests.el --- GUI integration tests for Pilish -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests that require a real Emacs GUI (windows, frames, scrolling).
;; Run with: make test-gui [SELECTOR=pattern]
;;
;; These tests focus on behavior that CANNOT be tested with unit tests:
;; - Real window scrolling during streamed updates in a displayed buffer
;; - Auto-scroll vs scroll-preservation with deterministic fake scenarios
;; - Tool-block overlays and extension UI behavior through the subprocess seam
;;
;; Many behaviors (history, spacing, and linked-buffer teardown) are covered
;; more directly by unit tests.
;;
;; For quick fake-backed debugging with a visible window:
;;   ./test/run-gui-tests.sh pilish-gui-test-scroll-auto-when-at-end

;;; Code:

(require 'ert)
(require 'pilish-gui-test-utils)
(require 'pilish-test-common)

(defun pilish-gui-test--table-display-strings (beg end)
  "Return ordered table overlay display strings between BEG and END."
  (when-let ((buf (plist-get pilish-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (mapcar (lambda (ov) (overlay-get ov 'display))
              (sort (seq-filter
                     (lambda (ov) (overlay-get ov 'pilish-table-display))
                     (overlays-in beg end))
                    (lambda (left right)
                      (< (overlay-start left) (overlay-start right))))))))

(defun pilish-gui-test--thinking-scroll-lines (prefix count)
  "Return COUNT newline-separated lines starting with PREFIX."
  (mapconcat (lambda (n) (format "%s %02d" prefix n))
             (number-sequence 1 count)
             "\n"))

(defun pilish-gui-test--thinking-scroll-messages ()
  "Return canonical history that exercises thinking-display scroll behavior."
  (vector
   (list :role "user"
         :content (vector (list :type "text" :text "Question one"))
         :timestamp 1704067200000)
   (list :role "assistant"
         :content (vector (list :type "text"
                                :text (concat
                                       (pilish-gui-test--thinking-scroll-lines
                                        "Earlier assistant line" 18)
                                       "\n\nShort bridge.")))
         :timestamp 1704067200500)
   (list :role "user"
         :content (vector (list :type "text" :text "Question two"))
         :timestamp 1704067200800)
   (list :role "assistant"
         :content (vector
                   (list :type "text"
                         :text "Prelude line A\nPrelude line B\nPrelude line C")
                   (list :type "thinking"
                         :thinking (concat
                                    (pilish-gui-test--thinking-scroll-lines
                                     "Thinking detail" 30)
                                    "\n\nConclusion thought"))
                   (list :type "text"
                         :text (concat "\n"
                                       (pilish-gui-test--thinking-scroll-lines
                                        "Final answer line" 12))))
         :timestamp 1704067201000)))

(defun pilish-gui-test--thinking-scroll-hidden-stub ()
  "Return the collapsed thinking stub used by the scroll-history fixture."
  (pilish--thinking-hidden-stub
   (pilish--thinking-normalize-text
    (concat
     (pilish-gui-test--thinking-scroll-lines
      "Thinking detail" 30)
     "\n\nConclusion thought"))))

(defun pilish-gui-test--render-thinking-scroll-history (buffer display-mode)
  "Render scroll-regression history into BUFFER using DISPLAY-MODE."
  (with-current-buffer buffer
    (pilish-chat-mode)
    (setq pilish--status 'idle
          pilish--thinking-display display-mode
          pilish--canonical-messages
          (pilish-gui-test--thinking-scroll-messages))
    (pilish--display-session-history pilish--canonical-messages
                                              buffer)
    (font-lock-ensure)
    (redisplay)))

(defun pilish-gui-test--window-visible-screen-lines (window)
  "Return how many screen lines WINDOW currently shows from buffer text."
  (count-screen-lines (window-start window) (window-end window t) nil window))

(defun pilish-gui-test--window-point-row (window)
  "Return WINDOW point's screen-line row within the current viewport."
  (count-screen-lines (window-start window) (window-point window) nil window))

(defun pilish-gui-test--window-current-line (window)
  "Return WINDOW point's current line without text properties."
  (with-selected-window window
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun pilish-gui-test--window-thinking-block-order (window)
  "Return the completed-thinking block order at WINDOW point, or nil."
  (with-selected-window window
    (pilish--thinking-block-at-pos (point))))

(defun pilish-gui-test--window-substantially-filled-p (window)
  "Return non-nil when WINDOW shows nearly a full body of buffer text."
  (>= (pilish-gui-test--window-visible-screen-lines window)
      (- (window-body-height window) 2)))

;;;; Session Tests

(ert-deftest pilish-gui-test-session-starts ()
  "Test that a fake-backed pi session starts with proper layout."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle")
    (should (pilish-gui-test-session-active-p))
    (should (pilish-gui-test-chat-window))
    (should (pilish-gui-test-input-window))
    (should (pilish-gui-test-verify-layout))))

(ert-deftest pilish-gui-test-send-waits-for-turn-start ()
  "Regression for issue #303: send waits until the submitted turn starts.
With a pre-start delay between the prompt ack and `agent_start', the
helpers previously returned while the status was still `sending', so
the next `pilish-send' queued its prompt as a follow-up and assertions
raced a turn that was never submitted."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle"
              :fake-extra-args ("--pre-start-delay-ms=100"))
    (pilish-gui-test-send "first turn")
    (pilish-gui-test-send "second turn")
    (should (pilish-gui-test-chat-contains "Fake reply for: second turn"))
    (should (pilish-gui-test-idle-p))))

;;;; Scroll Preservation Tests

(ert-deftest pilish-gui-test-scroll-preserved-streaming ()
  "Test scroll position is preserved while a fake stream updates below."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "scrolling-text")
    (pilish-gui-test-send "first turn")
    ;; The fake stream is usually tall enough already, but large frames can make
    ;; the buffer barely non-scrollable.  Top off with dummy lines so the test
    ;; exercises scroll preservation rather than frame geometry.
    (pilish-gui-test-ensure-scrollable)
    (pilish-gui-test-scroll-up 20)
    (should-not (pilish-gui-test-at-end-p))
    (let ((line-before (pilish-gui-test-top-line-number)))
      (should (> line-before 1))
      (pilish-gui-test-send "second turn")
      (should (= line-before (pilish-gui-test-top-line-number)))
      (should (pilish-gui-test-chat-contains "Scroll line 24 for second turn")))))

(ert-deftest pilish-gui-test-scroll-preserved-tool-use ()
  "Test scroll position is preserved while fake tool output arrives."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pilish-gui-test-ensure-scrollable)
    (pilish-gui-test-scroll-up 20)
    (should-not (pilish-gui-test-at-end-p))
    (let ((line-before (pilish-gui-test-top-line-number)))
      (should (> line-before 1))
      (pilish-gui-test-send "Use the fake read tool")
      (should (= line-before (pilish-gui-test-top-line-number)))
      (should (pilish-gui-test-chat-contains "fake tool output")))))

(ert-deftest pilish-gui-test-scroll-auto-when-at-end ()
  "Test auto-scroll when user is at end across deterministic fake turns.
Regression: `pilish--display-agent-end' must leave window-point at
buffer end so the next streamed turn still follows automatically."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "scrolling-text")
    (pilish-gui-test-send "first turn")
    (should (pilish-gui-test-window-point-at-end-p))
    (should (pilish-gui-test-at-end-p))
    (pilish-gui-test-send "second turn")
    (should (pilish-gui-test-window-point-at-end-p))
    (should (pilish-gui-test-at-end-p))
    (should (pilish-gui-test-chat-contains "Scroll line 24 for second turn"))))

(ert-deftest pilish-gui-test-thinking-display-toggle-keeps-tail-filled ()
  "Toggling completed thinking keeps a tail-following chat window filled.
After the toggle, a new streamed turn should still auto-scroll from the tail."
  (let ((buf (get-buffer-create "*pi-gui-thinking-tail*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pilish-gui-test--render-thinking-scroll-history buf 'visible)
          (let ((win (selected-window)))
            (goto-char (point-max))
            (recenter -1)
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (pilish-gui-test--window-substantially-filled-p win))
            (pilish-toggle-thinking-display)
            (redisplay)
            (should (string-match-p
                     (regexp-quote
                      (pilish-gui-test--thinking-scroll-hidden-stub))
                     (buffer-string)))
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (should (pilish-gui-test--window-substantially-filled-p win))
            (pilish--display-agent-start)
            (pilish--display-message-delta "Streaming tail line 01\n")
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (pilish--display-message-delta "Streaming tail line 02\n")
            (pilish--display-agent-end)
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (should (string-match-p "Streaming tail line 02" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest pilish-gui-test-thinking-display-toggle-keeps-context-window-usable ()
  "Toggling completed thinking keeps a non-tail chat window usable.
Point should stay on the same logical thinking block and the window should
remain substantially filled even when the collapsed tail is shorter."
  (let ((buf (get-buffer-create "*pi-gui-thinking-context*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pilish-gui-test--render-thinking-scroll-history buf 'visible)
          (let ((win (selected-window))
                block-before)
            (goto-char (point-min))
            (should (search-forward "Thinking detail 25" nil t))
            (beginning-of-line)
            (recenter 10)
            (redisplay)
            (setq block-before
                  (pilish-gui-test--window-thinking-block-order win))
            (should block-before)
            (should (pilish-gui-test--window-substantially-filled-p win))
            (should (equal "> Thinking detail 25"
                           (pilish-gui-test--window-current-line win)))
            (pilish-toggle-thinking-display)
            (redisplay)
            (should (string-match-p
                     (regexp-quote
                      (pilish-gui-test--thinking-scroll-hidden-stub))
                     (buffer-string)))
            (should (equal (pilish-gui-test--thinking-scroll-hidden-stub)
                           (pilish-gui-test--window-current-line win)))
            (should (equal block-before
                           (pilish-gui-test--window-thinking-block-order win)))
            (should (pilish-gui-test--window-substantially-filled-p win))))
      (kill-buffer buf))))

(ert-deftest pilish-gui-test-thinking-display-toggle-expands-same-block-from-hidden-stub ()
  "Expanding a hidden thinking stub restores the same logical block.
Point should land back on the same completed-thinking block rather than an
unrelated raw offset in the conversation."
  (let ((buf (get-buffer-create "*pi-gui-thinking-expand*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pilish-gui-test--render-thinking-scroll-history buf 'hidden)
          (let ((win (selected-window))
                block-before)
            (goto-char (point-min))
            (should (search-forward
                     (pilish-gui-test--thinking-scroll-hidden-stub)
                     nil t))
            (beginning-of-line)
            (recenter 10)
            (redisplay)
            (setq block-before
                  (pilish-gui-test--window-thinking-block-order win))
            (should block-before)
            (should (equal (pilish-gui-test--thinking-scroll-hidden-stub)
                           (pilish-gui-test--window-current-line win)))
            (pilish-toggle-thinking-display)
            (redisplay)
            (should (string-match-p "Thinking detail 25" (buffer-string)))
            (should (equal block-before
                           (pilish-gui-test--window-thinking-block-order win)))
            (should (string-match-p
                     "^> Thinking detail [0-9][0-9]$"
                     (pilish-gui-test--window-current-line win)))
            (should (pilish-gui-test--window-substantially-filled-p win))))
      (kill-buffer buf))))

(ert-deftest pilish-gui-test-thinking-display-toggle-restores-each-visible-window ()
  "Toggling completed thinking preserves each visible window's own context.
The tail window should stay at the end, while the context window keeps its
logical block and should not start following later streamed output."
  (let ((buf (get-buffer-create "*pi-gui-thinking-multi-window*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buf)
          (pilish-gui-test--render-thinking-scroll-history buf 'visible)
          (let* ((tail-win (selected-window))
                 (context-win (split-window-below)))
            (set-window-buffer context-win buf)
            (with-selected-window tail-win
              (goto-char (point-max))
              (recenter -1)
              (redisplay))
            (with-selected-window context-win
              (goto-char (point-min))
              (should (search-forward "Thinking detail 25" nil t))
              (beginning-of-line)
              (recenter 8)
              (redisplay))
            (let ((context-line-before
                   (pilish-gui-test--window-current-line context-win))
                  context-start-after-toggle)
              (with-selected-window tail-win
                (pilish-toggle-thinking-display))
              (redisplay)
              (should (eq (selected-window) tail-win))
              (should (>= (window-point tail-win) (1- (with-current-buffer buf (point-max)))))
              (should (>= (window-end tail-win t)
                          (1- (with-current-buffer buf (point-max)))))
              (should (pilish-gui-test--window-substantially-filled-p tail-win))
              (should (equal "> Thinking detail 25" context-line-before))
              (should (equal (pilish-gui-test--thinking-scroll-hidden-stub)
                             (pilish-gui-test--window-current-line context-win)))
              (should (pilish-gui-test--window-substantially-filled-p context-win))
              (setq context-start-after-toggle (window-start context-win))
              (with-current-buffer buf
                (pilish--display-agent-start)
                (pilish--display-message-delta "Dual window tail line\n"))
              (redisplay)
              (should (>= (window-point tail-win) (1- (with-current-buffer buf (point-max)))))
              (should (>= (window-end tail-win t)
                          (1- (with-current-buffer buf (point-max)))))
              (should (= context-start-after-toggle (window-start context-win)))
              (should (equal (pilish-gui-test--thinking-scroll-hidden-stub)
                             (pilish-gui-test--window-current-line context-win))))))
      (kill-buffer buf))))

(ert-deftest pilish-gui-test-table-resize-refreshes-hot-tail-only ()
  "Resizing rewraps hot tables only and preserves context below the table."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle")
    (let* ((chat-buf (plist-get pilish-gui-test--session :chat-buffer))
           (frame (selected-frame))
           (orig-width (frame-width))
           chat-win
           initial-chat-width
           (cold-table
            "| Feature | Notes |\n|---------|-------|\n| Cold history | This older table was wrapped at the original wide width and should stay frozen after resize |\n")
           (hot-table
            "| Feature | Notes |\n|---------|-------|\n| Hot tail | This recent table should rewrap when the window narrows so the columns remain readable |\n")
           cold-before
           hot-before
           cold-start
           hot-start
           filler-start)
      (unwind-protect
          (progn
            (with-current-buffer chat-buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "You · 10:00\n===========\n")
                (setq cold-start (point))
                (insert cold-table "\nAssistant\n=========\nRecent reply\n\nYou · 10:05\n===========\n")
                (setq hot-start (point))
                ;; Terminate the Markdown table before adding scrollable context.
                ;; Without the blank line, tree-sitter treats every filler line
                ;; as a lazy continuation row inside one giant table.
                (insert hot-table "\n")
                (setq filler-start (point))
                (dotimes (i 80)
                  (insert (format "filler line %d\n" i))))
              (font-lock-ensure)
              (setq chat-win (pilish-gui-test-chat-window)
                    initial-chat-width (window-width chat-win))
              (pilish--decorate-tables-in-region
               (point-min) (point-max) initial-chat-width)
              (move-marker pilish--hot-tail-start hot-start)
              (setq cold-before
                    (pilish-gui-test--table-display-strings
                     cold-start hot-start)
                    hot-before
                    (pilish-gui-test--table-display-strings
                     hot-start filler-start))
              (should cold-before)
              (should hot-before)
              (should-not
               (pilish-gui-test--table-display-strings
                filler-start (point-max))))
            (redisplay)
            (pilish-gui-test-scroll-up 20)
            (let ((line-before (pilish-gui-test-top-line-number)))
              (set-frame-size frame (- orig-width 30) (frame-height))
              (redisplay)
              (unless (pilish-test-wait-until
                       (lambda ()
                         (< (window-width chat-win) initial-chat-width))
                       2 0.05)
                (ert-skip
                 (format
                  (concat "Window manager did not honor resize request from "
                          "frame width %d to %d: chat window width %d is not "
                          "narrower than its initial width %d")
                  orig-width (- orig-width 30)
                  (window-width chat-win) initial-chat-width)))
              ;; X11 can report the new width while a synchronous ERT body has
              ;; not re-entered the command-loop redisplay that delivers this
              ;; buffer-local hook.  Exercise our registered callback in the
              ;; same selected-window context Emacs documents for the hook.
              (with-selected-window chat-win
                (should
                 (memq #'pilish--maybe-refresh-hot-tail-tables
                       window-configuration-change-hook))
                (run-hooks 'window-configuration-change-hook)
                (should (= pilish--last-table-display-width
                           (pilish--chat-window-width))))
              (should-not
               (equal hot-before
                      (pilish-gui-test--table-display-strings
                       hot-start filler-start)))
              (should (equal cold-before
                             (pilish-gui-test--table-display-strings
                              cold-start hot-start)))
              (should (= line-before (pilish-gui-test-top-line-number)))))
        (set-frame-size frame orig-width (frame-height))
        (redisplay)))))

;;;; Content Tests

(ert-deftest pilish-gui-test-tool-result-image-has-real-display-property ()
  "A real PNG result inserts an image display property in graphical Emacs."
  (unless (and (display-images-p) (image-type-available-p 'png))
    (ert-skip "This Emacs display cannot render PNG images"))
  (let ((buffer (get-buffer-create "*pi-gui-image-preview*")))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (pilish-chat-mode)
          (let ((block (pilish--display-tool-start
                        "generate" nil "gui-image")))
            (pilish--display-tool-end
             "generate" nil
             '((:type "image"
                :mimeType "image/png"
                :data "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAYAAAD0In+KAAAADklEQVR4nGP4z8DwHwQBEPgD/U6VwW8AAAAASUVORK5CYII="))
             nil nil block))
          (redisplay)
          (let* ((position
                  (text-property-any (point-min) (point-max)
                                     'pilish-image-preview t))
                 (display (and position
                               (get-text-property position 'display)))
                 (size (and display (image-size display t))))
            (should position)
            (should (eq 'image (car-safe display)))
            (should (consp size))
            (should (> (car size) 0))
            (should (> (cdr size) 0))))
      (kill-buffer buffer))))

(ert-deftest pilish-gui-test-startup-logo-renders-real-image ()
  "The startup heading renders the real SVG logo on image-capable displays.
The 1.5em image glyph makes the heading row taller than a text row and
pushes the heading text right of the logo box."
  (unless (and (display-images-p) (image-type-available-p 'svg))
    (ert-skip "This Emacs display cannot render SVG images"))
  (let ((buffer (get-buffer-create "*pi-gui-startup-logo*")))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (pilish-chat-mode)
          (let ((inhibit-read-only t))
            (insert (pilish--format-startup-header))
            (font-lock-ensure)
            (goto-char (point-min))
            (set-window-start (selected-window) (point-min))
            (redisplay)
            (should (get-text-property 1 'line-prefix))
            ;; A rendered 1.5em image makes this row taller than a text row.
            (should (> (line-pixel-height) (frame-char-height)))
            ;; Heading text starts right of the logo box plus gap.
            (should (> (car (posn-x-y (posn-at-point (point-min))))
                       (frame-char-width)))))
      (kill-buffer buffer))))

(ert-deftest pilish-gui-test-read-svg-has-real-display-property ()
  "A complete standalone SVG returned by read renders graphically."
  (unless (and (display-images-p) (image-type-available-p 'svg))
    (ert-skip "This Emacs display cannot render SVG images"))
  (let ((buffer (get-buffer-create "*pi-gui-svg-preview*"))
        (source
         "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"2\" height=\"1\"><rect width=\"2\" height=\"1\" fill=\"#369\"/></svg>"))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (pilish-chat-mode)
          (pilish--display-tool-end
           "read" '(:path "/missing/returned.svg")
           (list (list :type "text" :text source)) nil nil)
          (redisplay)
          (let* ((position
                  (text-property-any (point-min) (point-max)
                                     'pilish-image-preview t))
                 (display (and position
                               (get-text-property position 'display)))
                 (size (and display (image-size display t))))
            (should position)
            (should (eq 'image (car-safe display)))
            (should (consp size))
            (should (> (car size) 0))
            (should (> (cdr size) 0))))
      (kill-buffer buffer))))

(ert-deftest pilish-gui-test-content-tool-output-shown ()
  "Test that fake-backed tool output appears in chat and in the tool block."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pilish-gui-test-send "Use the fake read tool")
    (should (pilish-gui-test-chat-contains "read /tmp/fake-tool.txt"))
    (should (pilish-gui-test-chat-text-in-tool-block-p "fake tool output"))
    (should (pilish-gui-test-chat-contains "Tool finished"))))

(ert-deftest pilish-gui-test-tool-overlay-bounded ()
  "Test that the tool overlay stops before later assistant text.
Regression: `pilish--tool-overlay-finalize' must replace the
rear-advance overlay before assistant text continues after the tool block."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pilish-gui-test-send "Use the fake read tool")
    (with-current-buffer (plist-get pilish-gui-test--session :chat-buffer)
      (goto-char (point-min))
      (search-forward "fake tool output")
      (let* ((tool-pos (match-beginning 0))
             (tool-overlay (seq-find
                            (lambda (ov) (overlay-get ov 'pilish-tool-block))
                            (overlays-at tool-pos))))
        (should tool-overlay))
      (goto-char (point-min))
      (search-forward "Tool finished")
      (let* ((assistant-pos (match-beginning 0))
             (tool-overlay (seq-find
                            (lambda (ov) (overlay-get ov 'pilish-tool-block))
                            (overlays-at assistant-pos))))
        (should-not tool-overlay)))))

;;;; Extension Command Tests

(ert-deftest pilish-gui-test-extension-command-returns-to-idle ()
  "Fake extension command without a visible turn returns to idle."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-noop")
    (pilish-gui-test-send "/test-noop" t)
    (should (pilish-gui-test-wait-for-idle 2))))

(ert-deftest pilish-gui-test-extension-custom-message-displayed ()
  "Fake extension command displays a custom message in chat."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-message")
    (pilish-gui-test-send "/test-message")
    (should (pilish-gui-test-chat-contains "Test message from extension"))))

(ert-deftest pilish-gui-test-extension-confirm-response-displayed ()
  "Fake extension confirm response triggers the displayed follow-up message."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-confirm")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (pilish-gui-test-send "/test-confirm")
      (should (pilish-gui-test-chat-contains "CONFIRMED")))))

;;;; Tool Toggle Tests

(ert-deftest pilish-gui-test-tool-toggle-expand-collapse-cycle ()
  "TAB expands, collapses, and re-expands in a displayed GUI buffer.
Regression for issue #166: collapsing from the [-] button placed cursor
at the overlay boundary (half-open interval), making the next TAB fall
through to `outline-cycle' instead of toggling.  Uses a standalone
chat-mode buffer to isolate the toggle logic from RPC timing."
  (let ((buf (get-buffer-create "*pi-gui-toggle-test*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pilish-chat-mode)
          (pilish--display-tool-start "read" '(:path "/tmp/test.txt"))
          (pilish--display-tool-end "read" '(:path "/tmp/test.txt")
            `((:type "text"
               :text ,(mapconcat (lambda (n) (format "Line %02d of file" n))
                                 (number-sequence 1 20) "\n")))
            nil nil)
          (font-lock-ensure)
          (redisplay)
          ;; Initially collapsed
          (should (string-match-p "more lines)" (buffer-string)))
          (should-not (string-match-p "Line 20" (buffer-string)))
          ;; Expand from the collapsed indicator
          (goto-char (point-min))
          (search-forward "more lines)" nil t)
          (beginning-of-line)
          (pilish-toggle-tool-section)
          (redisplay)
          (should (string-match-p "Line 20" (buffer-string)))
          ;; Navigate to [-] button and collapse
          (goto-char (point-min))
          (search-forward "[-]" nil t)
          (beginning-of-line)
          (pilish-toggle-tool-section)
          (redisplay)
          (should (string-match-p "more lines)" (buffer-string)))
          (should-not (string-match-p "Line 20" (buffer-string)))
          ;; Re-expand must still work from current cursor position
          (pilish-toggle-tool-section)
          (redisplay)
          (should (string-match-p "Line 20" (buffer-string))))
      (kill-buffer buf))))

;;;; Streaming Fontification Tests

(ert-deftest pilish-gui-test-streamed-reference-definition-refreshes-distant-link ()
  "A streamed definition refontifies an offscreen reference link."
  (pilish-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle")
    (let* ((chat (plist-get pilish-gui-test--session :chat-buffer))
           (window (pilish-gui-test-chat-window))
           doc-position)
      (with-current-buffer chat
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "See [Doc][id].\n\n")
          (dotimes (i (+ 20 (* 2 (window-body-height window))))
            (insert (format "Filler line %03d.\n" i)))
          (insert "\n"))
        (goto-char (point-min))
        (search-forward "Doc")
        (setq doc-position (match-beginning 0))
        (setq pilish--assistant-header-shown nil)
        (pilish--handle-display-event '(:type "agent_start"))
        (pilish--handle-display-event
         '(:type "message_start" :message (:role "assistant"))))
      (with-selected-window window
        (goto-char (point-min))
        (set-window-start window (point-min))
        (redisplay t)
        (should-not (pos-visible-in-window-p (point-max) window))
        (should (eq t (get-text-property doc-position 'fontified)))
        (should-not (button-at doc-position)))
      (with-selected-window window
        (goto-char (point-max))
        (recenter -1)
        (redisplay t)
        (should-not (pos-visible-in-window-p doc-position window)))
      (with-current-buffer chat
        (pilish--handle-display-event
         '(:type "message_update"
           :assistantMessageEvent
           (:type "text_delta" :delta "[id]: https://added.example\n")))
        (pilish--flush-stream-deltas))
      (with-selected-window window
        (goto-char doc-position)
        (set-window-start window (point-min))
        (redisplay t)
        (let ((button (button-at doc-position)))
          (should button)
          (should (equal "https://added.example"
                         (button-get button 'help-echo))))))))

(ert-deftest pilish-gui-test-streaming-no-fences ()
  "Streaming write content shows no fence markers to the user.
Fences exist in the buffer for tree-sitter parsing, but
`md-ts-hide-markup' makes them invisible.  Uses a displayed
buffer (jit-lock active) to verify under real GUI conditions."
  (let ((buf (get-buffer-create "*pi-gui-fontify-test*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pilish-chat-mode)
          (pilish--handle-display-event '(:type "agent_start"))
          (pilish--handle-display-event
           '(:type "message_start" :message (:role "assistant" :content [])))
          (pilish-test--send-assistant-message-update
           '(:type "toolcall_start" :contentIndex 0
             :id "call_1" :toolName "write"))
          (redisplay)
          (pilish-test--send-assistant-message-update
           '(:type "toolcall_delta" :contentIndex 0
             :delta "{\"path\":\"/tmp/test.py\",\"content\":\"def hello():\\n    return 42\\n\"}"))
          (pilish--flush-stream-deltas)
          (font-lock-ensure)
          ;; Fences are in the buffer (for tree-sitter) but invisible
          (let ((visible (pilish--visible-text
                          (point-min) (point-max))))
            (should-not (string-match-p "```" visible)))
          ;; Content is present with syntax faces
          (goto-char (point-min))
          (should (search-forward "def" nil t))
          (let ((face (get-text-property (match-beginning 0) 'face)))
            (should (or (eq face 'font-lock-keyword-face)
                        (and (listp face)
                             (memq 'font-lock-keyword-face face))))))
      (kill-buffer buf))))

(defun pilish-gui-test--display-prefix (line width)
  "Return LINE's prefix occupying the first WIDTH display columns."
  (let ((chars nil) (w 0))
    (catch 'done
      (dotimes (i (length line))
        (let ((cw (string-width (substring line i (1+ i)))))
          (when (> (+ w cw) width) (throw 'done nil))
          (push (aref line i) chars)
          (setq w (+ w cw)))))
    (concat (nreverse chars))))

(ert-deftest pilish-gui-test-session-browser-narrow-visibility ()
  "All-projects identity and live status stay visible on a real narrow
window.
Renders the session browser in a dedicated buffer and frame whose
window body is exactly 32 columns wide — the text area of a
52-column terminal next to the mode's 20-column right margin
\\(`window-body-width' already excludes margins) — and checks
visibility with display widths (`string-width'), not character
offsets.  Adversarial fixtures: two projects sharing a long common
prefix (ordinal-front tokens), a wide-character project name, a
depth-18 threaded connector with the live marker behind it, and long
identical titles.  On every row the bounded token field and the
two-column live field must end inside the visible columns, with
`truncate-lines' on and no horizontal scroll; empty-state hint lines
keep their recovery keys on screen too."
  (let* ((long-title (make-string 60 ?x))
         (deep-dir "/home/u/long-common-prefix-abcdef")
         (token-width 16)
         (items
          (append
           (list (list :path "/a/one.jsonl" :cwd (concat deep-dir "/app")
                       :name (concat "One " long-title)
                       :messageCount 2 :modified "2026-03-11T10:00:00Z")
                 (list :path "/a/two.jsonl"
                       :cwd "/home/u/long-common-prefix-abcdeg/app"
                       :name (concat "Two " long-title)
                       :messageCount 4 :modified "2026-03-11T11:00:00Z")
                 (list :path "/a/wide.jsonl" :cwd "/home/u/\u4e2d\u6587\u9879\u76ee"
                       :name (concat "Wide " long-title)
                       :messageCount 1 :modified "2026-03-11T12:00:00Z")
                 ;; A malformed row: recorded cwd is rejected, but the
                 ;; bounded field is still reserved for it.
                 (list :path "/a/broken.jsonl" :cwd "/a\0b"
                       :name (concat "Broken " long-title)
                       :messageCount 1 :modified "2026-03-11T13:00:00Z"))
           ;; A depth-18 chain under the first project whose last
           ;; child is the live session: its connector alone would
           ;; far exceed the body.
           (let ((chain nil) (parent "/a/one.jsonl"))
             (dotimes (i 18)
               (let ((path (format "/a/deep-%02d.jsonl" i)))
                 (push (list :path path :cwd (concat deep-dir "/app")
                             :name (format "Deep %02d" i)
                             :parentSessionPath parent
                             :messageCount 1
                             :modified "2026-03-11T10:00:00Z")
                       chain)
                 (setq parent path)))
             (nreverse chain))))
         (live-line-title "Deep 17")
         frame win buffer chat-buf proc)
    ;; Enter cleanup protection before acquiring any GUI/process
    ;; resource, so a failure halfway through setup cannot leak those
    ;; already created.
    (unwind-protect
        (progn
          (setq frame (make-frame '((width . 52) (height . 22))))
          (setq win (frame-selected-window frame))
          (setq buffer (generate-new-buffer " *pilish-gui-narrow*"))
          (setq chat-buf
                (generate-new-buffer " *pilish-gui-narrow-chat*"))
          (setq proc (start-process "pilish-gui-narrow" nil "sleep" "30"))
          (set-process-query-on-exit-flag proc nil)
          (process-put proc 'pilish-chat-buffer chat-buf)
          (with-current-buffer chat-buf
            (setq pilish--process proc
                  pilish--state
                  (list :session-file "/a/deep-17.jsonl")))
          (set-window-buffer win buffer)
          (with-current-buffer buffer
            (pilish-session-browser-mode)
            ;; Bounded resize: let the mode's window-configuration
            ;; hook apply the right margin, then adjust the frame
            ;; until the body is exactly 32 columns; the assertion
            ;; catches an oscillating or stuck body.
            (redisplay)
            (let ((attempts 0))
              (while (not (= 32 (window-body-width win)))
                (cl-assert (< (cl-incf attempts) 60) t
                           "body width stuck at %d"
                           (window-body-width win))
                (set-frame-width
                 frame (+ (frame-width frame)
                          (- 32 (window-body-width win))))
                (redisplay)))
            (setq pilish--session-browser-scope 'all
                  pilish--session-browser-items items
                  pilish--session-browser-view 'threaded)
            (pilish--session-browser-rerender)
            (redisplay)
            (let* ((usable (window-body-width win))
                   (rows
                    (seq-remove
                     (lambda (line)
                       (or (string-empty-p line)
                           (not (string-match-p
                                 "One x\\|Two x\\|Wide x\\|Broken x\\|Deep "
                                 line))))
                     (split-string
                      (buffer-substring-no-properties
                       (point-min) (point-max))
                      "\n"))))
              (should (= usable 32))
              (should truncate-lines)
              (should (zerop (window-hscroll win)))
              (should (= (length rows) (length items)))
              (dolist (line rows)
                ;; Token field is exactly the fixed display width on
                ;; every row, and the two-column live field follows at
                ;; one constant column before any unbounded content.
                (let* ((tok (pilish-gui-test--display-prefix
                             line token-width))
                       (marker
                        (substring-no-properties
                         line (length tok) (+ (length tok) 2))))
                  (should (= (string-width tok) token-width))
                  (should (member marker '("\u25cf " "  ")))
                  ;; The visible invariant: identity plus live field
                  ;; end inside the narrow body.
                  (should (<= (+ token-width 2) usable))))
              ;; The malformed row keeps the placeholder field —
              ;; bounded and truthful even behind a long title.
              (let ((broken (cl-find "Broken " rows :test #'string-match-p)))
                (should broken)
                (should (string-prefix-p
                         "?"
                         (string-trim-right
                          (pilish-gui-test--display-prefix
                           broken token-width)))))
              ;; The live marker really is on the depth-18 child.
              (let ((live-line (cl-find live-line-title rows
                                        :test #'string-match-p)))
                (should live-line)
                (should (string-prefix-p
                         "\u25cf "
                         (substring-no-properties
                          live-line
                          (length (pilish-gui-test--display-prefix
                                   live-line token-width))
                          (+ (length (pilish-gui-test--display-prefix
                                      live-line token-width))
                             2))))))
            ;; Empty state: every hint line fits the same narrow body.
            (setq pilish--session-browser-search-query "zzz-no-match"
                  pilish--session-browser-search-tokens '("zzz-no-match"))
            (pilish--session-browser-rerender)
            (redisplay)
            (goto-char (point-min))
            (while (< (point) (point-max))
              (should (<= (string-width
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                          (window-body-width win)))
              (forward-line 1))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and frame (frame-live-p frame))
        (delete-frame frame)))))

(defun pilish-gui-test--folded-browser-motions (down up &optional evil)
  "Check DOWN and UP skip a folded session family on a real display.
When EVIL is non-nil, enable Evil locally before exercising its motions."
  (let (frame buffer)
    (unwind-protect
        (progn
          (setq frame (make-frame '((width . 80) (height . 22)))
                buffer (generate-new-buffer " *pilish-gui-fold-motion*"))
          (with-selected-frame frame
            (switch-to-buffer buffer)
            (pilish-session-browser-mode)
            (when evil
              (evil-local-mode 1)
              (evil-motion-state))
            (setq pilish--session-browser-scope 'all
                  pilish--session-browser-view 'threaded
                  pilish--session-browser-items
                  '((:path "/tmp/pilish-gui-fold-parent.jsonl" :name "Parent"
                     :modified "2026-01-02T00:00:00Z")
                    (:path "/tmp/pilish-gui-fold-child.jsonl" :name "Child"
                     :parentSessionPath "/tmp/pilish-gui-fold-parent.jsonl"
                     :modified "2026-01-02T00:00:00Z")
                    (:path "/tmp/pilish-gui-fold-other.jsonl" :name "Other"
                     :modified "2026-01-01T00:00:00Z")))
            (pilish--session-browser-rerender)
            (goto-char (point-min))
            (search-forward "Parent")
            (beginning-of-line)
            (pilish-browse-toggle-fold)
            (redisplay t)
            (let ((this-command down)) (call-interactively down))
            (redisplay t)
            (should-not (invisible-p (point)))
            (should (equal (oref (magit-current-section) value)
                           "/tmp/pilish-gui-fold-other.jsonl"))
            (let ((this-command up)) (call-interactively up))
            (redisplay t)
            (should-not (invisible-p (point)))
            (should (equal (oref (magit-current-section) value)
                           "/tmp/pilish-gui-fold-parent.jsonl"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (and frame (frame-live-p frame)) (delete-frame frame)))))

(ert-deftest pilish-gui-test-browser-fold-plain-motions ()
  "Ordinary line motions do not strand the cursor on folded session rows."
  (pilish-gui-test--folded-browser-motions #'next-line #'previous-line))

(ert-deftest pilish-gui-test-browser-fold-evil-motions ()
  "Evil j/k leave point on a visible row after redisplay."
  (unless (require 'evil nil t) (ert-skip "Evil not installed"))
  (pilish-gui-test--folded-browser-motions #'evil-next-line #'evil-previous-line t))

(provide 'pilish-gui-tests)
;;; pilish-gui-tests.el ends here
