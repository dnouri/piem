;;; pilish-table-test.el --- Tests for pilish-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for display-only pipe table decoration: overlay creation,
;; line mapping, streaming decoration, hot-tail resize, prefix handling,
;; inline markup, and interaction correctness.

;;; Code:

(require 'ert)
(require 'pilish)
(require 'pilish-test-common)


(defconst pilish-test--wide-table
  "| Feature | Status | Notes |\n|---------|--------|-------------------------------|\n| Auth | Done | OAuth2 with refresh tokens |\n| DB | WIP | PostgreSQL connection pool |\n"
  "A wide pipe table for decoration tests.")

(ert-deftest pilish-test-decorate-tables-creates-display-overlay ()
  "decorate-tables-in-region creates overlays with display property."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1))
      (should (cl-every (lambda (ov) (overlay-get ov 'display)) ovs)))))

(ert-deftest pilish-test-decorate-tables-preserves-raw-buffer-text ()
  "Table decoration does not alter the raw buffer text."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (let ((before (buffer-string)))
      (font-lock-ensure)
      (pilish--decorate-tables-in-region (point-min) (point-max) 40)
      (should (equal before (buffer-string))))))

(ert-deftest pilish-test-table-query-failure-keeps-raw-text-and-is-cached ()
  "Either compilation failure leaves raw tables intact without retrying queries."
  (dolist (failed-call '(1 2))
    (with-temp-buffer
      (pilish-chat-mode)
      (let ((inhibit-read-only t)) (insert pilish-test--wide-table))
      (let ((pilish--treesit-table-query nil)
            (pilish--treesit-data-row-query nil)
            (original-compile (symbol-function 'treesit-query-compile))
            (compilations 0)
            warnings)
        (cl-letf (((symbol-function 'treesit-query-compile)
                   (lambda (language query &rest args)
                     (cl-incf compilations)
                     (if (= compilations failed-call)
                         (signal 'treesit-query-error '("unsupported table node"))
                       (apply original-compile language query args))))
                  ((symbol-function 'display-warning)
                   (lambda (_type text &rest _) (push text warnings))))
          (dotimes (_ 2)
            (pilish--decorate-tables-in-region (point-min) (point-max) 40)
            (should-not (pilish--table-has-data-row-p (point-min) (point-max)))))
        (should (= compilations failed-call))
        (should (= 1 (length warnings)))
        (should (string-match-p "Incompatible Markdown" (car warnings)))
        (should (equal pilish-test--wide-table
                       (buffer-substring-no-properties (point-min) (point-max))))
        (should-not pilish--treesit-data-row-query)
        (should-not (cl-some (lambda (ov) (overlay-get ov 'pilish-table-display))
                            (overlays-in (point-min) (point-max))))))))

(ert-deftest pilish-test-decorate-tables-is-idempotent ()
  "Running decoration twice does not accumulate extra overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((count-after-first
           (length (seq-filter
                    (lambda (ov) (overlay-get ov 'pilish-table-display))
                    (overlays-in (point-min) (point-max))))))
      (pilish--decorate-tables-in-region (point-min) (point-max) 40)
      (let ((count-after-second
             (length (seq-filter
                      (lambda (ov) (overlay-get ov 'pilish-table-display))
                      (overlays-in (point-min) (point-max))))))
        (should (= count-after-first count-after-second))))))

(ert-deftest pilish-test-decorate-tables-skips-fenced-table ()
  "Tables inside fenced code blocks are not decorated."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "```\n| A | B |\n|---|---|\n| 1 | 2 |\n```\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (= (length ovs) 0)))))

(ert-deftest pilish-test-decorate-tables-only-outside-fence ()
  "Only tables outside fences are decorated; fenced ones are skipped."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| Real | Table |\n|------|-------|\n| yes  | data  |\n\n")
      (insert "```\n| Fake | Table |\n|------|-------|\n| no   | data  |\n```\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1)))))

(ert-deftest pilish-test-table-overlays-have-no-font-faces ()
  "Table display overlays carry no font-changing face attributes.
Inline markdown faces like `md-ts-code' inherit from `fixed-pitch', which
changes the font family.  Display strings must use anonymous face plists
with font-identity attributes stripped, so columns align under any GUI
font configuration."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| Name | Description |\n|------|-------------|\n| `react` | A **library** |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 60)
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1))
      (dolist (ov ovs)
        (let* ((display-str (overlay-get ov 'display))
               (pos 0)
               (len (length display-str)))
          (while (< pos len)
            (let ((face-val (get-text-property pos 'face display-str)))
              (when face-val
                ;; Must be an anonymous face plist, not a symbolic face
                ;; that could resolve to include font-changing attributes.
                (should (consp face-val))
                (should (keywordp (car face-val)))
                ;; And that plist must not contain font-identity keys
                (should-not (plist-get face-val :family))
                (should-not (plist-get face-val :foundry))
                (should-not (plist-get face-val :height))))
            (setq pos (next-single-property-change pos 'face display-str len))))))))

(ert-deftest pilish-test-neutralize-fonts-strips-family ()
  "Font-identity attributes are removed, visual attributes preserved."
  (let* ((str (propertize "hello" 'face '(:family "Courier" :foreground "red")))
         (result (pilish--neutralize-fonts str)))
    (should (equal (get-text-property 0 'face result) '(:foreground "red")))))

(ert-deftest pilish-test-neutralize-fonts-resolves-symbolic-face ()
  "Symbolic faces are resolved; font attributes from inheritance stripped."
  (let* ((str (propertize "code" 'face 'fixed-pitch))
         (result (pilish--neutralize-fonts str))
         (face (get-text-property 0 'face result)))
    (should-not (plist-get face :family))))

(ert-deftest pilish-test-neutralize-fonts-preserves-plain-text ()
  "Text without face properties passes through unchanged."
  (let ((result (pilish--neutralize-fonts "plain")))
    (should (equal result "plain"))
    (should-not (get-text-property 0 'face result))))

(ert-deftest pilish-test-neutralize-fonts-multi-span ()
  "Each face span is neutralized independently."
  (let* ((str (concat (propertize "a" 'face '(:family "Mono" :foreground "red"))
                      "b"
                      (propertize "c" 'face '(:weight bold :height 140))))
         (result (pilish--neutralize-fonts str)))
    (should (equal (get-text-property 0 'face result) '(:foreground "red")))
    (should-not (get-text-property 1 'face result))
    (should (equal (get-text-property 2 'face result) '(:weight bold)))))

(ert-deftest pilish-test-markdown-visible-string-falls-back-on-font-lock-error ()
  "Visible-string extraction should fall back to raw markdown on font-lock errors."
  (unwind-protect
      (let ((debug-on-error nil))
        (cl-letf (((symbol-function 'font-lock-ensure)
                   (lambda (&rest _args)
                     (error "Broken font-lock"))))
          (should (equal (pilish--markdown-visible-string "`0xAF`")
                         "`0xAF`"))))
    (pilish--cleanup-visible-string-buffer)))

(ert-deftest pilish-test-display-user-message-decorates-table ()
  "User messages with tables get display-only decoration."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-user-message
     "| Feature | Status |\n|---------|--------|\n| Auth | Done |")
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1)))))

(ert-deftest pilish-test-render-complete-message-decorates-table ()
  "Completed assistant messages get display-only table decoration."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n| Auth | Done |\n")
    (pilish--render-complete-message)
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1)))))

(ert-deftest pilish-test-render-history-text-decorates-table ()
  "History text with tables gets display-only decoration."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--render-history-text
     "| Feature | Status |\n|---------|--------|\n| Auth | Done |\n")
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1)))))

(ert-deftest pilish-test-render-history-text-survives-font-lock-error ()
  "History rendering should keep going when markdown font-lock fails."
  (unwind-protect
      (with-temp-buffer
        (pilish-chat-mode)
        (let ((debug-on-error nil))
          (cl-letf (((symbol-function 'font-lock-ensure)
                     (lambda (&rest _args)
                       (error "Broken font-lock"))))
            (pilish--render-history-text
             "| Code | Notes |\n|------|-------|\n| `0xAF` | **bold** text |\n")))
        (let ((ovs (seq-filter
                    (lambda (ov) (overlay-get ov 'pilish-table-display))
                    (overlays-in (point-min) (point-max)))))
          (should (>= (length ovs) 1))
          (should (string-match-p "`0xAF`" (buffer-string)))
          (should (string-suffix-p "\n\n" (buffer-string)))))
    (pilish--cleanup-visible-string-buffer)))

(ert-deftest pilish-test-display-compaction-decorates-table ()
  "Compaction summary with tables gets display-only decoration."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-compaction-result
     50000 "| Key | Value |\n|-----|-------|\n| ctx | 50k |")
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1)))))

(ert-deftest pilish-test-custom-message-decorates-table ()
  "Custom messages with tables get display-only decoration."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--handle-display-event
     '(:type "message_start"
       :message (:role "custom" :display t
                 :content "| Key | Val |\n|-----|-----|\n| a | b |")))
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      (should (>= (length ovs) 1)))))

(ert-deftest pilish-test-decorate-table-preserves-trailing-newline ()
  "Display string preserves trailing newline from the raw table."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "before\n\n" pilish-test--wide-table "\nafter\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let* ((ovs (seq-filter
                 (lambda (ov) (overlay-get ov 'pilish-table-display))
                 (overlays-in (point-min) (point-max))))
           (disp (overlay-get (car ovs) 'display)))
      ;; Display string should end with newline (from tree-sitter node)
      (should (string-suffix-p "\n" disp)))))

(ert-deftest pilish-test-table-decoration-copy-returns-raw ()
  "Copying a decorated table returns the raw canonical markdown."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((copied (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "OAuth2 with refresh tokens" copied)))))

(ert-deftest pilish-test-table-line-mapping-1to1 ()
  "Non-wrapping table maps each raw line to one wrapped line."
  (let* ((raw-lines '("| A | B |" "|---|---|" "| 1 | 2 |"))
         (wrap-lines '("| A | B |" "| - | - |" "| 1 | 2 |"))
         (mapping (pilish--table-line-mapping raw-lines wrap-lines)))
    (should (equal (mapcar #'length mapping) '(1 1 1)))
    (should (equal (nth 0 mapping) '("| A | B |")))
    (should (equal (nth 1 mapping) '("| - | - |")))
    (should (equal (nth 2 mapping) '("| 1 | 2 |")))))

(ert-deftest pilish-test-table-line-mapping-data-wraps ()
  "Data rows that wrap produce multi-line groups."
  (require 'markdown-table-wrap)
  (let* ((raw "| Name | Desc |\n|------|------|\n| Auth | OAuth2 with refresh tokens and renewal |")
         (wrapped (markdown-table-wrap raw 30 nil t))
         (mapping (pilish--table-line-mapping
                   (split-string raw "\n")
                   (split-string wrapped "\n"))))
    ;; Header and separator: 1 line each; data row: multiple lines
    (should (= (length (nth 0 mapping)) 1))
    (should (= (length (nth 1 mapping)) 1))
    (should (> (length (nth 2 mapping)) 1))))

(ert-deftest pilish-test-table-line-mapping-header-wraps ()
  "Header that wraps produces a multi-line header group."
  (require 'markdown-table-wrap)
  (let* ((raw "| Feature Name | Current Status |\n|---|---|\n| A | B |")
         (wrapped (markdown-table-wrap raw 20 nil t))
         (mapping (pilish--table-line-mapping
                   (split-string raw "\n")
                   (split-string wrapped "\n"))))
    (should (> (length (nth 0 mapping)) 1))
    (should (= (length (nth 1 mapping)) 1))))

(ert-deftest pilish-test-table-line-mapping-multiple-data-rows ()
  "Multiple data rows split by spacer rows map correctly."
  (require 'markdown-table-wrap)
  (let* ((raw "| A | B |\n|---|---|\n| x | long value |\n| y | another long value |")
         (wrapped (markdown-table-wrap raw 20 nil t))
         (raw-lines (split-string raw "\n"))
         (wrap-lines (split-string wrapped "\n"))
         (mapping (pilish--table-line-mapping raw-lines wrap-lines)))
    ;; 4 raw lines → 4 mapping groups
    (should (= (length mapping) 4))
    ;; Each group has at least one line
    (should (cl-every (lambda (g) (>= (length g) 1)) mapping))))

(ert-deftest pilish-test-table-line-mapping-nil-without-separator ()
  "Mapping returns nil when no separator is found."
  (let ((mapping (pilish--table-line-mapping
                  '("| A |" "| B |")
                  '("| A |" "| B |"))))
    (should (null mapping))))

(ert-deftest pilish-test-decorate-table-creates-per-line-overlays ()
  "Each raw table line gets its own display overlay."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((ovs (seq-filter
                (lambda (ov) (overlay-get ov 'pilish-table-display))
                (overlays-in (point-min) (point-max)))))
      ;; 4 raw lines (header + separator + 2 data rows) → 4 overlays
      (should (= (length ovs) 4)))))

(ert-deftest pilish-test-decorate-table-point-visits-each-line ()
  "Point can stop on every raw table line, not just before/after."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "before\n" pilish-test--wide-table "after\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    ;; Collect line-beginning positions for each raw table line
    (let ((raw-lines (split-string
                      (string-trim-right pilish-test--wide-table "\n+")
                      "\n"))
          (table-line-positions nil))
      (save-excursion
        (goto-char (point-min))
        (forward-line 1) ; skip "before"
        (dotimes (_ (length raw-lines))
          (push (line-beginning-position) table-line-positions)
          (forward-line 1)))
      (setq table-line-positions (nreverse table-line-positions))
      ;; Each position should have its own overlay (point can stop there)
      (dolist (pos table-line-positions)
        (let ((ovs-at (seq-filter
                       (lambda (ov)
                         (and (overlay-get ov 'pilish-table-display)
                              (<= (overlay-start ov) pos)
                              (> (overlay-end ov) pos)))
                       (overlays-in (1- pos) (1+ pos)))))
          (should (= (length ovs-at) 1)))))))

(ert-deftest pilish-test-decorate-table-single-line-copy-returns-raw ()
  "Copying a single raw table line returns just that raw line."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    ;; Copy just the third raw line (first data row: "| Auth ...")
    (goto-char (point-min))
    (forward-line 2)
    (let* ((line-beg (line-beginning-position))
           (line-end (1+ (line-end-position)))
           (copied (buffer-substring-no-properties line-beg line-end)))
      ;; Should be the raw pipe-table row, not the wrapped version
      (should (string-match-p "| Auth" copied))
      (should (string-match-p "OAuth2 with refresh tokens" copied)))))

(ert-deftest pilish-test-decorate-table-backtick-cells-aligned ()
  "Tables with inline markup keep consistent visible line widths.
Wrapped table overlays hide markdown delimiters just like the chat buffer,
so visible text still needs consistent alignment across all display lines."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| code | value |\n|------|-------|\n| `0xAF` | test |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    ;; Collect all display lines from all overlays
    (let* ((ovs (seq-filter
                 (lambda (ov) (overlay-get ov 'pilish-table-display))
                 (overlays-in (point-min) (point-max))))
           (all-lines nil))
      (dolist (ov ovs)
        (dolist (line (split-string
                       (string-trim-right (overlay-get ov 'display) "\n+")
                       "\n"))
          (push line all-lines)))
      ;; All display lines in a well-formed table have the same width
      (let ((widths (mapcar #'string-width (nreverse all-lines))))
        (should (= (length (delete-dups (copy-sequence widths))) 1))))))

(ert-deftest pilish-test-decorate-table-prettifies-visible-separators ()
  "Rendered table display uses box-drawing separators instead of raw pipes."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| Name | Value |\n|------|-------|\n| Alpha | Beta |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((all-display (mapconcat #'identity
                                  (pilish-test--table-overlay-displays-in-region
                                   (point-min) (point-max))
                                  "\n")))
      ;; Rows use box-drawing verticals, not raw markdown pipes
      (should (string-match-p "^│ " all-display))
      (should-not (string-match-p "^| " all-display))
      ;; Separator uses box-drawing horizontals
      (should (string-match-p "├.*┼.*┤" all-display))
      ;; Table content preserved
      (should (string-match-p "Name" all-display))
      (should (string-match-p "Alpha" all-display)))))

(ert-deftest pilish-test-decorate-table-preserves-pipes-when-prettify-off ()
  "With prettify disabled, rendered tables use standard markdown pipes."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((pilish-prettify-tables nil)
          (inhibit-read-only t))
      (insert "| Name | Value |\n|------|-------|\n| Alpha | Beta |\n")
      (font-lock-ensure)
      (pilish--decorate-tables-in-region (point-min) (point-max) 40)
      (let ((all-display (mapconcat #'identity
                                    (pilish-test--table-overlay-displays-in-region
                                     (point-min) (point-max))
                                    "\n")))
        ;; Standard markdown pipe delimiters
        (should (string-match-p "^| " all-display))
        ;; No box-drawing characters
        (should-not (string-match-p "│" all-display))
        (should-not (string-match-p "├" all-display))
        ;; Separator uses dashes
        (should (string-match-p "---" all-display))
        ;; Table content preserved
        (should (string-match-p "Name" all-display))
        (should (string-match-p "Alpha" all-display))))))

(ert-deftest pilish-test-table-overlay-suppresses-buffer-face ()
  "Table overlays suppress tree-sitter buffer faces without losing inline formatting.
Tree-sitter applies `md-ts-delimiter' (shadow) to separator rows and `bold'
to headers.  The overlay must suppress these while preserving inline markdown
formatting (bold, italic) in the display string's text properties."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| Name | Note |\n|------|------|\n| **bold** | text |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((ovs (sort (seq-filter
                      (lambda (ov) (overlay-get ov 'pilish-table-display))
                      (overlays-in (point-min) (point-max)))
                     (lambda (a b) (< (overlay-start a) (overlay-start b))))))
      ;; Every overlay has an explicit face to block buffer face bleed-through
      (should (cl-every (lambda (ov) (overlay-get ov 'face)) ovs))
      ;; Separator overlay does not inherit shadow
      (let ((sep-face (overlay-get (nth 1 ovs) 'face)))
        (should-not (eq sep-face 'md-ts-delimiter))
        (should-not (eq sep-face 'shadow)))
      ;; Inline bold from **bold** survives in the data row display string
      (should (pilish-test--string-has-face-attr-p
               (overlay-get (nth 2 ovs) 'display) :weight 'bold)))))

(ert-deftest pilish-test-decorate-table-hides-inline-markup-in-display ()
  "Wrapped table display hides markdown delimiters like the chat buffer does."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| code | emphasis |\n|------|----------|\n| `0xAF` | **bold** text that wraps |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 30)
    (let ((display (mapconcat #'identity
                              (pilish-test--table-overlay-displays-in-region
                               (point-min) (point-max))
                              "\n")))
      (should-not (string-match-p "`0xAF`" display))
      (should-not (string-match-p "\\*\\*bold\\*\\*" display))
      (should (string-match-p "0xAF" display))
      (should (string-match-p "bold" display)))))

(ert-deftest pilish-test-table-cell-render-function-is-used ()
  "A non-nil `pilish-table-cell-render-function' renders cells.
display-cell calls it INSTEAD of the built-in markdown fontification,
so an external table styler can shape cell display without pi depending
on it.  Here the styler upcases cells — observable behavior that
fontification would never produce."
  (let ((pilish-table-cell-render-function
         (lambda (cell) (upcase cell))))
    (with-temp-buffer
      (pilish-chat-mode)
      (let ((inhibit-read-only t))
        (insert "| name |\n|------|\n| alice |\n"))
      (font-lock-ensure)
      (pilish--decorate-tables-in-region (point-min) (point-max) 40)
      (let ((display (mapconcat #'identity
                                (pilish-test--table-overlay-displays-in-region
                                 (point-min) (point-max))
                                "\n")))
        ;; the custom styler upcased the cell (fontification never would).
        ;; Bind case-fold-search nil: string-match-p otherwise matches
        ;; "alice" against "ALICE" case-insensitively.
        (let ((case-fold-search nil))
          (should (string-match-p "ALICE" display))
          (should-not (string-match-p "alice" display)))
        ;; canonical buffer text is untouched
        (should (string-match-p "| alice |" (buffer-string)))))))

(ert-deftest pilish-test-table-cell-render-function-faces-survive ()
  "Faces applied by a custom render function reach the display string.
`pilish--neutralize-fonts' resolves them to attribute plists,
so an external styler's faces (e.g. bold) render just like built-in
markdown faces do."
  (let ((pilish-table-cell-render-function
         (lambda (cell) (propertize cell 'face 'bold))))
    (with-temp-buffer
      (pilish-chat-mode)
      (let ((inhibit-read-only t))
        (insert "| name |\n|------|\n| alice |\n"))
      (font-lock-ensure)
      (pilish--decorate-tables-in-region (point-min) (point-max) 40)
      (let ((display (mapconcat #'identity
                                (pilish-test--table-overlay-displays-in-region
                                 (point-min) (point-max))
                                "\n")))
        (should (string-match-p "alice" display))
        (should (pilish-test--string-has-face-attr-p display :weight 'bold))))))

(defun pilish-test--table-row-tokens (lines)
  "Recover whitespace-free cell tokens from wrapped display LINES."
  (let ((rows
         (mapcar
          (lambda (line)
            (let ((bare (string-remove-prefix "> " line)))
              (mapcar #'string-trim
                      (if pilish-prettify-tables
                          (butlast (cdr (split-string bare "│")))
                        (markdown-table-wrap--split-table-row bare)))))
          lines)))
    (apply #'cl-mapcar (lambda (&rest chunks) (apply #'concat chunks)) rows)))

(ert-deftest pilish-test-decorate-ragged-table-retains-all-cells ()
  "Ragged rows keep every cell and full-width separators when wrapped."
  (dolist (pretty '(t nil))
    (dolist (prefix '("" "> "))
      (dolist (width '(38 80 140))
        (with-temp-buffer
          (pilish-chat-mode)
          (let* ((pilish-prettify-tables pretty)
                 (inhibit-read-only t)
                 (raw-lines
                  '("| H1 | H2 |" "|:---|---:|"
                    "| Cell01 | Cell02 | Cell03 | Cell04 | Cell05 | Cell06 | Cell07 | Cell08 |"))
                 (raw (concat (mapconcat (lambda (line) (concat prefix line))
                                        raw-lines "\n")
                              "\n")))
            (insert raw)
            (font-lock-ensure)
            (pilish--decorate-tables-in-region (point-min) (point-max) width)
            (let* ((displays (pilish-test--table-overlay-displays-in-region
                              (point-min) (point-max)))
                   (groups (mapcar (lambda (display)
                                     (split-string
                                      (string-trim-right display "\n+") "\n"))
                                   displays)))
              (should (= (length groups) 3))
              ;; Reassemble wrapped columns: substring checks would miss
              ;; cells split across lines, or silently dropped columns.
              (should (equal (pilish-test--table-row-tokens (nth 2 groups))
                             '("Cell01" "Cell02" "Cell03" "Cell04"
                               "Cell05" "Cell06" "Cell07" "Cell08")))
              (should (equal (pilish-test--table-row-tokens (car groups))
                             '("H1" "H2" "" "" "" "" "" "")))
              (should (apply #'= (mapcar #'string-width
                                        (apply #'append groups))))
              (should (equal raw (buffer-substring-no-properties
                                  (point-min) (point-max)))))))))))

(ert-deftest pilish-test-ragged-table-keeps-alignment-and-empty-cells ()
  "Extra columns default to left alignment without changing existing cells."
  (dolist (pretty '(t nil))
    (let* ((pilish-prettify-tables pretty)
           (groups (pilish--table-display-groups
                    '("| LongLeft | LongRight | Center |" "|:---|---:|:---:|"
                      "| a | b | c | D4 |" "| short | | | LongerD4 |"
                      "| left\\|right | right | center | E4 |" "| one |")
                    140)))
      (should (apply #'= (mapcar #'string-width (apply #'append groups))))
      (should (equal (car (nth 2 groups))
                     (if pretty
                         "│ a           │         b │   c    │ D4       │"
                       "| a           |         b |   c    | D4       |")))
      (should (equal (pilish-test--table-row-tokens (nth 3 groups))
                     '("short" "" "" "LongerD4")))
      (should (equal (pilish-test--table-row-tokens (nth 4 groups))
                     '("left\\|right" "right" "center" "E4")))
      (should (equal (pilish-test--table-row-tokens (nth 5 groups))
                     '("one" "" "" ""))))))

(ert-deftest pilish-test-decorate-table-keeps-blockquote-prefix ()
  "Display-only wrapping preserves blockquote prefixes on every visual line."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "> | Feature | Notes |\n"
              "> |---------|-------|\n"
              "> | Auth | OAuth2 with refresh tokens and renewal plus extra prose for wrapping |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (dolist (display (pilish-test--table-overlay-displays-in-region
                      (point-min) (point-max)))
      (dolist (line (split-string (string-trim-right display "\n+") "\n"))
        (should (string-prefix-p "> " line))))))

(ert-deftest pilish-test-decorate-table-keeps-indentation-prefix ()
  "Display-only wrapping preserves indentation for nested tables."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "  | Feature | Notes |\n"
              "  |---------|-------|\n"
              "  | Auth | OAuth2 with refresh tokens and renewal plus extra prose for wrapping |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (dolist (display (pilish-test--table-overlay-displays-in-region
                      (point-min) (point-max)))
      (dolist (line (split-string (string-trim-right display "\n+") "\n"))
        (should (string-prefix-p "  " line))))))

(ert-deftest pilish-test-decorate-table-empty-row-follows-treesit-truth ()
  "All-empty data rows stay undecorated when tree-sitter stops the table early.
This is a deliberate limitation of the tree-sitter-only detector: we keep the
raw markdown canonical rather than extending the region heuristically beyond
what the parser recognizes as a `pipe_table'."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| A | B |\n|---|---|\n| x | long value that wraps a lot |\n|   |   |\n| y | another long value that also wraps |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 20)
    (should (= (pilish-test--table-overlay-count) 0))))

(ert-deftest pilish-test-decorate-table-keeps-dash-only-data-row-visible ()
  "A data row containing dashes is not mistaken for the separator row."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "| A | B |\n|---|---|\n| ---- | ---- |\n| x | y |\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 20)
    (let ((dash-row-display (nth 2 (pilish-test--table-overlay-displays-in-region
                                    (point-min) (point-max)))))
      (should (string-match-p "----" dash-row-display)))))

;;; Per-line overlay interaction verification (Phase 4)

(ert-deftest pilish-test-table-copy-mixed-selection-coherent ()
  "Selection crossing table/prose boundary returns coherent raw text."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "before\n" pilish-test--wide-table "after\n"))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (let ((copied (buffer-substring-no-properties (point-min) (point-max))))
      ;; Prose and table text both present in raw copy
      (should (string-match-p "before" copied))
      (should (string-match-p "after" copied))
      (should (string-match-p "| Feature |" copied)))))

(ert-deftest pilish-test-table-search-finds-cell-content ()
  "Search finds text inside decorated table cells."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    (goto-char (point-min))
    (should (search-forward "OAuth2" nil t))
    ;; Point should be inside a per-line overlay
    (let ((ovs (seq-filter
                (lambda (ov)
                  (and (overlay-get ov 'pilish-table-display)
                       (<= (overlay-start ov) (point))
                       (> (overlay-end ov) (point))))
                (overlays-in (max 1 (1- (point))) (1+ (point))))))
      (should (= (length ovs) 1)))))

(ert-deftest pilish-test-table-overlay-independent-of-tool-overlay ()
  "Removing table overlays does not affect tool overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      ;; Table
      (insert pilish-test--wide-table)
      ;; Simulate tool block after table
      (let ((tool-start (point)))
        (insert "tool output\n")
        (let ((ov (make-overlay tool-start (point))))
          (overlay-put ov 'pilish-tool-overlay t))))
    (font-lock-ensure)
    (pilish--decorate-tables-in-region (point-min) (point-max) 40)
    ;; Both overlay types exist
    (should (>= (pilish-test--table-overlay-count) 1))
    (should (>= (length (seq-filter
                         (lambda (ov) (overlay-get ov 'pilish-tool-overlay))
                         (overlays-in (point-min) (point-max))))
                1))
    ;; Remove table overlays
    (pilish--remove-table-overlays (point-min) (point-max))
    ;; Tool overlays survive
    (should (>= (length (seq-filter
                         (lambda (ov) (overlay-get ov 'pilish-tool-overlay))
                         (overlays-in (point-min) (point-max))))
                1))
    (should (= (pilish-test--table-overlay-count) 0))))

(defun pilish-test--string-has-face-attr-p (str attr value)
  "Return non-nil if STR has face ATTR equal to VALUE at any position."
  (let ((pos 0)
        (len (length str)))
    (cl-loop while (< pos len)
             for face = (get-text-property pos 'face str)
             thereis (and (consp face) (eq (plist-get face attr) value))
             do (setq pos (next-single-property-change pos 'face str len)))))

(defun pilish-test--table-overlay-count ()
  "Count table display overlays in the current buffer."
  (length (seq-filter
           (lambda (ov) (overlay-get ov 'pilish-table-display))
           (overlays-in (point-min) (point-max)))))

(defun pilish-test--table-overlay-displays-in-region (beg end)
  "Return table overlay display strings between BEG and END in order."
  (mapcar (lambda (ov) (overlay-get ov 'display))
          (sort (seq-filter
                 (lambda (ov) (overlay-get ov 'pilish-table-display))
                 (overlays-in beg end))
                (lambda (left right)
                  (< (overlay-start left) (overlay-start right))))))

(ert-deftest pilish-test-streaming-table-no-decoration-without-newline ()
  "Streaming a partial table row without newline creates no table overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta "| Feature | Status |")
    (should (= (pilish-test--table-overlay-count) 0))))

(ert-deftest pilish-test-streaming-table-no-decoration-header-sep-only ()
  "Header + separator without data row creates no table overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta "| Feature | Status |\n")
    (pilish--display-message-delta "|---------|--------|\n")
    (should (= (pilish-test--table-overlay-count) 0))))

(ert-deftest pilish-test-streaming-table-decorates-on-first-data-row ()
  "First complete data row triggers table decoration."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n")
    (should (= (pilish-test--table-overlay-count) 0))
    (pilish--display-message-delta "| Auth | Done |\n")
    (should (>= (pilish-test--table-overlay-count) 1))))

(ert-deftest pilish-test-streaming-table-updates-on-later-rows ()
  "Later data rows update the active table's overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n| Auth | Done |\n")
    (let ((count-after-first (pilish-test--table-overlay-count)))
      (should (>= count-after-first 1))
      (pilish--display-message-delta "| DB | WIP |\n")
      ;; More overlays now (4 lines instead of 3)
      (should (> (pilish-test--table-overlay-count)
                 count-after-first)))))

(ert-deftest pilish-test-streaming-table-raw-text-unchanged ()
  "Raw buffer text is canonical markdown throughout streaming."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n| Auth | Done |\n")
    (should (string-match-p
             "| Auth | Done |"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest pilish-test-streaming-table-fenced-ignored ()
  "Table-like text inside a fenced code block is not decorated."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta "```\n")
    (pilish--display-message-delta
     "| A | B |\n|---|---|\n| 1 | 2 |\n")
    (pilish--display-message-delta "```\n")
    (should (= (pilish-test--table-overlay-count) 0))))

(ert-deftest pilish-test-streaming-table-text-end-finalizes ()
  "text_end decorates a trailing table row without newline."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    ;; Header + separator arrive with newlines
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n")
    (should (= (pilish-test--table-overlay-count) 0))
    ;; Data row arrives WITHOUT newline — no streaming decoration
    (pilish--display-message-delta "| Auth | Done |")
    (should (= (pilish-test--table-overlay-count) 0))
    ;; text_end backstop triggers decoration
    (pilish--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_end"
                               :content "ignored")))
    (should (>= (pilish-test--table-overlay-count) 1))))

(ert-deftest pilish-test-streaming-table-prose-after-table-preserves ()
  "Prose after a finished table does not corrupt table overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n| Auth | Done |\n")
    (let ((count-before-prose (pilish-test--table-overlay-count)))
      (should (>= count-before-prose 1))
      (pilish--display-message-delta "\nSome prose after the table.\n")
      ;; Table overlays should still exist
      (should (= (pilish-test--table-overlay-count) count-before-prose)))))

(ert-deftest pilish-test-streaming-table-second-table-preserves-first ()
  "Streaming a second table does not corrupt the first table's overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    ;; Stream first table
    (pilish--display-message-delta
     "| A | B |\n|---|---|\n| 1 | 2 |\n")
    (let ((first-count (pilish-test--table-overlay-count)))
      (should (>= first-count 1))
      ;; Stream prose between tables
      (pilish--display-message-delta "\nSome prose.\n\n")
      ;; Stream second table
      (pilish--display-message-delta
       "| C | D |\n|---|---|\n| 3 | 4 |\n")
      ;; Both tables should now have overlays
      (should (> (pilish-test--table-overlay-count) first-count)))))

(ert-deftest pilish-test-streaming-table-message-end-safety-net ()
  "render-complete-message re-decorates as safety net after streaming."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n| Auth | Done |\n")
    (pilish--render-complete-message)
    (should (>= (pilish-test--table-overlay-count) 1))
    ;; Raw text preserved
    (should (string-match-p
             "| Auth | Done |"
             (buffer-substring-no-properties (point-min) (point-max))))))

;;; Hot-tail resize refresh

(ert-deftest pilish-test-hot-tail-refresh-updates-hot-table-only ()
  "Refreshing the hot tail rewrites recent tables without touching cold ones."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((hot-start nil)
          (inhibit-read-only t))
      (insert "You · 10:00\n===========\n"
              pilish-test--wide-table
              "\nAssistant\n=========\nRecent reply\n\n"
              "You · 10:05\n===========\n")
      (setq hot-start (point))
      (insert pilish-test--wide-table)
      (font-lock-ensure)
      (pilish--decorate-tables-in-region (point-min) (point-max) 80)
      (move-marker pilish--hot-tail-start hot-start)
      (let ((cold-before (pilish-test--table-overlay-displays-in-region
                          (point-min) hot-start))
            (hot-before (pilish-test--table-overlay-displays-in-region
                         hot-start (point-max))))
        (pilish--refresh-hot-tail-tables 40)
        (should (equal cold-before
                       (pilish-test--table-overlay-displays-in-region
                        (point-min) hot-start)))
        (should-not (equal hot-before
                           (pilish-test--table-overlay-displays-in-region
                            hot-start (point-max))))))))

(ert-deftest pilish-test-hot-tail-refresh-skips-height-only-change ()
  "Window configuration changes without a width change do not refresh tables."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--last-table-display-width 80)
    (let ((called nil))
      (cl-letf (((symbol-function 'pilish--chat-window-width)
                 (lambda () 80))
                ((symbol-function 'pilish--refresh-hot-tail-tables)
                 (lambda (width)
                   (setq called width))))
        (pilish--maybe-refresh-hot-tail-tables)
        (should-not called)))))

(ert-deftest pilish-test-hot-tail-refresh-runs-on-width-change ()
  "A changed chat width refreshes hot-tail tables and updates the cache."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--last-table-display-width 80)
    (let ((called nil))
      (cl-letf (((symbol-function 'pilish--chat-window-width)
                 (lambda () 40))
                ((symbol-function 'pilish--refresh-hot-tail-tables)
                 (lambda (width)
                   (setq called width))))
        (pilish--maybe-refresh-hot-tail-tables)
        (should (= called 40))
        (should (= pilish--last-table-display-width 40))))))

(ert-deftest pilish-test-hot-tail-refresh-skips-incomplete-streaming-table ()
  "Resizing during a header-only stream does not decorate the table early."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--display-agent-start)
    (pilish--display-message-delta
     "| Feature | Status |\n|---------|--------|\n")
    (move-marker pilish--hot-tail-start (point-min))
    (pilish--refresh-hot-tail-tables 40)
    (should (= (pilish-test--table-overlay-count) 0))
    (pilish--display-message-delta "| Auth | Done |\n")
    (should (>= (pilish-test--table-overlay-count) 1))))

(ert-deftest pilish-test-chat-buffer-hidden-p-sees-visible-window-on-other-frame ()
  "A chat buffer visible on another frame is not hidden."
  (with-temp-buffer
    (let ((noninteractive nil)
          (calls nil))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (buffer &optional all-frames)
                   (should (eq buffer (current-buffer)))
                   (push all-frames calls)
                   (cond
                    ((null all-frames) nil)
                    ((eq all-frames 'visible) 'other-frame-window)
                    (t (error "Unexpected all-frames value: %S" all-frames))))))
        (should-not (pilish--chat-buffer-hidden-p))
        (should (equal (nreverse calls) '(nil visible)))))))

(ert-deftest pilish-test-chat-buffer-hidden-p-returns-nil-in-batch-without-window ()
  "Batch tests using windowless temp buffers are not treated as hidden."
  (with-temp-buffer
    (let ((noninteractive t))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) nil)))
        (should-not (pilish--chat-buffer-hidden-p))))))

(ert-deftest pilish-test-chat-window-width-excludes-fringe-columns ()
  "Chat window width reports usable character columns, not raw window width.
When fringes like `display-line-numbers-mode' consume columns,
`--chat-window-width' must return only the columns available for text."
  (with-temp-buffer
    (let ((fake-window (selected-window)))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) fake-window))
                ((symbol-function 'window-max-chars-per-line)
                 (lambda (&optional _window _face) 76)))
        (should (= (pilish--chat-window-width) 76))))))

(ert-deftest pilish-test-chat-window-width-falls-back-to-visible-window-on-other-frame ()
  "Chat window width uses another visible frame when selected frame lacks chat."
  (with-temp-buffer
    (let ((calls nil))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (buffer &optional all-frames)
                   (should (eq buffer (current-buffer)))
                   (push all-frames calls)
                   (cond
                    ((null all-frames) nil)
                    ((eq all-frames 'visible) 'other-frame-window)
                    (t (error "Unexpected all-frames value: %S" all-frames)))))
                ((symbol-function 'window-max-chars-per-line)
                 (lambda (window &optional _face)
                   (should (eq window 'other-frame-window))
                   64)))
        (should (= (pilish--chat-window-width) 64))
        (should (equal (nreverse calls) '(nil visible)))))))

(ert-deftest pilish-test-chat-window-width-prefers-selected-frame-window ()
  "Chat window width keeps selected-frame preference when chat is visible there."
  (with-temp-buffer
    (let ((calls nil))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (buffer &optional all-frames)
                   (should (eq buffer (current-buffer)))
                   (push all-frames calls)
                   (cond
                    ((null all-frames) 'selected-frame-window)
                    ((eq all-frames 'visible) 'other-frame-window)
                    (t (error "Unexpected all-frames value: %S" all-frames)))))
                ((symbol-function 'window-max-chars-per-line)
                 (lambda (window &optional _face)
                   (pcase window
                     ('selected-frame-window 90)
                     ('other-frame-window 50)
                     (_ (error "Unexpected window: %S" window))))))
        (should (= (pilish--chat-window-width) 90))
        (should (equal (nreverse calls) '(nil)))))))


;;; Toggle (reveal raw table text)

(defconst pilish-test--two-tables
  (concat "| a | b |\n|---|---|\n| 1 | 2 |\n\n"
          "| c | d |\n|---|---|\n| 3 | 4 |\n")
  "Two pipe tables separated by a blank line, for toggle-all tests.")

(defun pilish-test--display-overlays ()
  "Return pi table display overlays in the current buffer."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'pilish-table-display))
   (overlays-in (point-min) (point-max))))

(defun pilish-test--raw-overlays ()
  "Return pi table raw-marker overlays in the current buffer."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'pilish-table-raw))
   (overlays-in (point-min) (point-max))))

(defun pilish-test--decorate-all-tables (&optional width)
  "Decorate every table in the current buffer at WIDTH (default 80)."
  (pilish--decorate-tables-in-region
   (point-min) (point-max) (or width 80)))

(ert-deftest pilish-test-toggle-on-table-reveals-raw ()
  "Toggling a table at point removes its display overlays and marks it raw."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish-test--decorate-all-tables)
    (should (>= (length (pilish-test--display-overlays)) 1))
    (should (null (pilish-test--raw-overlays)))
    (goto-char (point-min))
    (pilish-toggle-table-pretty)
    (should (null (pilish-test--display-overlays)))
    (should (= 1 (length (pilish-test--raw-overlays))))))

(ert-deftest pilish-test-toggle-on-table-restores-pretty ()
  "Toggling a raw table at point re-renders it and clears the raw marker."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--wide-table))
    (font-lock-ensure)
    (pilish-test--decorate-all-tables)
    (goto-char (point-min))
    (pilish-toggle-table-pretty) ; pretty -> raw
    (should (null (pilish-test--display-overlays)))
    (pilish-toggle-table-pretty) ; raw -> pretty
    (should (>= (length (pilish-test--display-overlays)) 1))
    (should (null (pilish-test--raw-overlays)))))

(ert-deftest pilish-test-toggle-off-table-toggles-all ()
  "Point off-table toggles every table in the buffer."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--two-tables))
    (font-lock-ensure)
    (pilish-test--decorate-all-tables)
    (should (= 2 (length (pilish--treesit-table-regions
                          (point-min) (point-max)))))
    ;; Point after the last table (off-table).
    (goto-char (point-max))
    (pilish-toggle-table-pretty) ; all pretty -> all raw
    (should (null (pilish-test--display-overlays)))
    (should (= 2 (length (pilish-test--raw-overlays))))
    (pilish-toggle-table-pretty) ; all raw -> all pretty
    (should (>= (length (pilish-test--display-overlays)) 2))
    (should (null (pilish-test--raw-overlays)))))

(ert-deftest pilish-test-toggle-c-u-forces-pretty-on-all ()
  "`C-u' forces pretty on every table, clearing any raw markers."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--two-tables))
    (font-lock-ensure)
    (pilish-test--decorate-all-tables)
    ;; Toggle the first table to raw; the second stays pretty.
    (goto-char (point-min))
    (pilish-toggle-table-pretty)
    (should (= 1 (length (pilish-test--raw-overlays))))
    (should (>= (length (pilish-test--display-overlays)) 1))
    ;; C-u forces pretty on all: both tables re-decorated, raw cleared.
    (pilish-toggle-table-pretty '(4))
    (should (>= (length (pilish-test--display-overlays)) 2))
    (should (null (pilish-test--raw-overlays)))))

(ert-deftest pilish-test-toggle-c-u-c-u-forces-raw-on-all ()
  "`C-u C-u' forces raw on every table, removing all display overlays."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--two-tables))
    (font-lock-ensure)
    (pilish-test--decorate-all-tables)
    (should (>= (length (pilish-test--display-overlays)) 2))
    (pilish-toggle-table-pretty '(16))
    (should (null (pilish-test--display-overlays)))
    (should (= 2 (length (pilish-test--raw-overlays))))))

(ert-deftest pilish-test-toggle-region-toggles-tables-in-region ()
  "An active region toggles only tables whose start falls within it."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--two-tables))
    (font-lock-ensure)
    (pilish-test--decorate-all-tables)
    (let ((transient-mark-mode t))
      ;; Select a region covering only the second table.
      (goto-char (point-min))
      (search-forward "| c")
      (set-mark (line-beginning-position))
      (search-forward "| 4")
      (end-of-line)
      (setq mark-active t)
      (pilish-toggle-table-pretty)
      ;; Second table raw, first table still pretty.
      (should (= 1 (length (pilish-test--raw-overlays))))
      (let ((raw-start (overlay-start
                        (car (pilish-test--raw-overlays)))))
        (should (> raw-start (point-min))))
      (should (>= (length (pilish-test--display-overlays)) 1)))))

(ert-deftest pilish-test-toggle-raw-survives-redecoration ()
  "A table toggled to raw is skipped by the re-decoration path (resize/resume)."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--two-tables))
    (font-lock-ensure)
    (pilish-test--decorate-all-tables)
    ;; Toggle the first table to raw; the second stays pretty.
    (goto-char (point-min))
    (pilish-toggle-table-pretty)
    (should (= 1 (length (pilish-test--raw-overlays))))
    (should (>= (length (pilish-test--display-overlays)) 1))
    ;; Simulate the resize / resume / hot-tail refresh path.
    (pilish-test--decorate-all-tables)
    ;; The raw table stays raw; the second table (not marked) gets re-decorated.
    (should (= 1 (length (pilish-test--raw-overlays))))
    (should (>= (length (pilish-test--display-overlays)) 1))
    ;; The raw marker is still on the first table.
    (let ((raw-start (overlay-start
                      (car (pilish-test--raw-overlays)))))
      (should (= raw-start (point-min))))))

(ert-deftest pilish-test-toggle-preserves-buffer-text ()
  "Toggling never alters the canonical buffer text."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert pilish-test--two-tables))
    (font-lock-ensure)
    (let ((before (buffer-string)))
      (pilish-test--decorate-all-tables)
      (pilish-toggle-table-pretty)        ; all pretty -> all raw
      (pilish-toggle-table-pretty '(4))   ; force pretty
      (pilish-toggle-table-pretty '(16))  ; force raw
      (pilish-test--decorate-all-tables)  ; resize path
      (should (equal before (buffer-string))))))

(provide 'pilish-table-test)
;;; pilish-table-test.el ends here
