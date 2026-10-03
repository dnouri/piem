;;; pilish-ui-test.el --- Tests for pilish-ui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for buffer naming, creation, major modes, session directory,
;; buffer linkage, and startup header — the UI foundation layer.

;;; Code:

(require 'ert)
(require 'warnings)  ; ensure display-warning is loaded (not autoloaded)
(require 'pilish)
(require 'pilish-test-common)

;;; Buffer Naming

(ert-deftest pilish-test-buffer-name-chat ()
  "Buffer name for chat includes abbreviated directory."
  (let ((name (pilish--buffer-name :chat "/home/user/project/")))
    (should (string-match-p "\\*pilish-chat:" name))
    (should (string-match-p "project" name))))

(ert-deftest pilish-test-buffer-name-input ()
  "Buffer name for input includes abbreviated directory."
  (let ((name (pilish--buffer-name :input "/home/user/project/")))
    (should (string-match-p "\\*pilish-input:" name))
    (should (string-match-p "project" name))))

(ert-deftest pilish-test-buffer-name-abbreviates-home ()
  "Buffer name abbreviates home directory to ~."
  (let ((name (pilish--buffer-name :chat (expand-file-name "~/myproject/"))))
    (should (string-match-p "~" name))))

(ert-deftest pilish-test-buffer-name-preserves-multi-hop-route ()
  "Buffer names keep the full TRAMP route for remote sessions."
  (let* ((dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
         (name (pilish--buffer-name :chat dir)))
    (should (string-match-p (regexp-quote dir) name))
    (should-not (string-match-p (regexp-quote "/sudo:root@pi-host:")
                                name))))

(ert-deftest pilish-test-path-to-language-known-extension ()
  "path-to-language returns correct language for known extensions."
  (should (equal "python" (pilish--path-to-language "/tmp/foo.py")))
  (should (equal "javascript" (pilish--path-to-language "/tmp/bar.js")))
  (should (equal "emacs-lisp" (pilish--path-to-language "/tmp/baz.el"))))

(ert-deftest pilish-test-path-to-language-unknown-extension ()
  "path-to-language returns 'text' for unknown extensions.
This ensures all files get code fences for consistent display."
  (should (equal "text" (pilish--path-to-language "/tmp/foo.txt")))
  (should (equal "text" (pilish--path-to-language "/tmp/bar.xyz")))
  (should (equal "text" (pilish--path-to-language "/tmp/noext"))))

(ert-deftest pilish-test-path-to-language-ignores-non-string ()
  "path-to-language returns nil for malformed path metadata."
  (should-not (pilish--path-to-language '(:not "a path")))
  (should-not (pilish--path-to-language ["not" "a" "path"])))

;;; Buffer Creation

(ert-deftest pilish-test-get-or-create-buffer-creates-new ()
  "get-or-create-buffer creates a new buffer if none exists."
  (let* ((dir "/tmp/pilish-test-unique-12345/")
         (buf (pilish--get-or-create-buffer :chat dir)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest pilish-test-get-or-create-buffer-returns-existing ()
  "get-or-create-buffer returns existing buffer."
  (let* ((dir "/tmp/pilish-test-unique-67890/")
         (buf1 (pilish--get-or-create-buffer :chat dir))
         (buf2 (pilish--get-or-create-buffer :chat dir)))
    (unwind-protect
        (should (eq buf1 buf2))
      (when (buffer-live-p buf1)
        (kill-buffer buf1)))))

;;; Major Modes

(ert-deftest pilish-test-chat-mode-is-read-only ()
  "pilish-chat-mode sets buffer to read-only."
  (with-temp-buffer
    (pilish-chat-mode)
    (should buffer-read-only)))

(ert-deftest pilish-test-chat-mode-disables-undo-history ()
  "Generated chat updates do not accumulate undo history."
  (with-temp-buffer
    (pilish-chat-mode)
    (should (eq buffer-undo-list t))
    (let ((inhibit-read-only t))
      (insert "streamed response")
      (delete-region (point-min) (point-max)))
    (should (eq buffer-undo-list t))))

(ert-deftest pilish-test-chat-mode-has-word-wrap ()
  "pilish-chat-mode enables word wrap."
  (with-temp-buffer
    (pilish-chat-mode)
    (should word-wrap)
    (should-not truncate-lines)))

(ert-deftest pilish-test-chat-mode-disables-hl-line ()
  "pilish-chat-mode disables hl-line to prevent scroll oscillation."
  (with-temp-buffer
    (pilish-chat-mode)
    (should-not hl-line-mode)
    (should-not (buffer-local-value 'global-hl-line-mode (current-buffer)))))

(ert-deftest pilish-test-chat-mode-is-special-buffer-mode ()
  "Chat mode advertises the standard special-buffer contract."
  (should (eq (get 'pilish-chat-mode 'mode-class) 'special)))

(ert-deftest pilish-test-chat-mode-adds-window-change-hook ()
  "pilish-chat-mode installs the buffer-local width refresh hook."
  (with-temp-buffer
    (pilish-chat-mode)
    (should (local-variable-p 'window-configuration-change-hook))
    (should (memq #'pilish--maybe-refresh-hot-tail-tables
                  window-configuration-change-hook))))

(ert-deftest pilish-test-chat-mode-initializes-with-theme-derived-diff-faces ()
  "Chat mode startup should not depend on diff-mode being loaded elsewhere."
  (with-temp-buffer
    (let ((debug-on-error t))
      (pilish-chat-mode)
      (should (derived-mode-p 'pilish-chat-mode)))))

(ert-deftest pilish-test-thinking-display-default-is-visible ()
  "Package default keeps completed thinking expanded in new chat buffers."
  (should (eq (default-value 'pilish-thinking-display) 'visible)))

(ert-deftest pilish-test-chat-mode-initializes-thinking-display-from-default ()
  "New chat buffers inherit the configured completed-thinking display default."
  (let ((pilish-thinking-display 'hidden))
    (with-temp-buffer
      (pilish-chat-mode)
      (should (eq pilish--thinking-display 'hidden)))))

(ert-deftest pilish-test-thinking-display-override-is-buffer-local ()
  "Changing one chat buffer's thinking display leaves others and the default alone."
  (let ((pilish-thinking-display 'visible)
        (buf-a (generate-new-buffer " *pi-thinking-display-a*"))
        (buf-b (generate-new-buffer " *pi-thinking-display-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (pilish-chat-mode)
            (pilish--set-thinking-display 'hidden))
          (with-current-buffer buf-b
            (pilish-chat-mode))
          (should (eq pilish-thinking-display 'visible))
          (should (eq (buffer-local-value 'pilish--thinking-display buf-a) 'hidden))
          (should (eq (buffer-local-value 'pilish--thinking-display buf-b) 'visible)))
      (when (buffer-live-p buf-a)
        (kill-buffer buf-a))
      (when (buffer-live-p buf-b)
        (kill-buffer buf-b)))))

(ert-deftest pilish-test-theme-diff-background-prefers-diff-face-background ()
  "Theme-derived diff lines should reuse an existing diff background first."
  (cl-letf (((symbol-function 'face-background)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added "#224422")
                 ('default "#111111")
                 (_ nil))))
            ((symbol-function 'color-defined-p)
             (lambda (color) (stringp color))))
    (should (equal (pilish--theme-diff-background
                    'diff-added 'diff-indicator-added)
                   "#224422"))))

(ert-deftest pilish-test-theme-diff-background-prefers-diff-face-foreground ()
  "Theme-derived diff lines should prefer the diff face foreground before the indicator."
  (cl-letf (((symbol-function 'face-background)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added nil)
                 ('default "#111111")
                 (_ nil))))
            ((symbol-function 'face-foreground)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added "#bb3333")
                 ('diff-indicator-added "#22aa22")
                 (_ nil))))
            ((symbol-function 'color-defined-p)
             (lambda (color) (stringp color))))
    (should (equal (pilish--theme-diff-background
                    'diff-added 'diff-indicator-added)
                   (pilish--blend-color "#111111" "#bb3333" 0.20)))))

(ert-deftest pilish-test-theme-diff-background-falls-back-to-indicator-foreground ()
  "Theme-derived diff lines should fall back to the indicator color when needed."
  (cl-letf (((symbol-function 'face-background)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added nil)
                 ('default "#fefefe")
                 (_ nil))))
            ((symbol-function 'face-foreground)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added nil)
                 ('diff-indicator-added "#22aa22")
                 (_ nil))))
            ((symbol-function 'color-defined-p)
             (lambda (color) (stringp color))))
    (should (equal (pilish--theme-diff-background
                    'diff-added 'diff-indicator-added)
                   (pilish--blend-color "#fefefe" "#22aa22" 0.10)))))

(ert-deftest pilish-test-update-theme-derived-faces-uses-background-only-overlays ()
  "Theme-derived overlay faces should only contribute background tint."
  (let (calls)
    (cl-letf (((symbol-function 'face-background)
               (lambda (face &optional _frame _inherit)
                 (pcase face
                   ('default "#111111")
                   ('diff-added "#224422")
                   ('diff-removed nil)
                   (_ nil))))
              ((symbol-function 'face-foreground)
               (lambda (face &optional _frame _inherit)
                 (pcase face
                   ('diff-removed "#bb3333")
                   ('diff-indicator-removed "#aa2222")
                   (_ nil))))
              ((symbol-function 'color-defined-p)
               (lambda (color) (stringp color)))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest args)
                 (push (cons face args) calls))))
      (pilish--update-theme-derived-faces)
      (dolist (face '(pilish-diff-line-added
                      pilish-diff-line-removed
                      pilish-tool-block-error))
        (let ((args (cdr (assq face calls))))
          (should args)
          (should (eq (plist-get args :inherit) nil))
          (should (eq (plist-get args :foreground) 'unspecified))
          (should (stringp (plist-get args :background)))
          (should (eq (plist-get args :extend) t)))))))

(ert-deftest pilish-test-chat-mode-write-file-preserves-chat-state ()
  "`write-file' keeps chat buffers in chat mode with file backing attached."
  (let ((file nil)
        (root (pilish-test--make-temp-directory
               "pilish-test-write-file-"))
        (make-backup-files nil))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (setq default-directory root)
          (setq pilish--process 'mock-process)
          (let ((inhibit-read-only t))
            (insert "Assistant\n=========\n\nHello\n"))
          (setq file (pilish-test--write-chat-buffer
                      (current-buffer) "pilish-chat-write-"))
          (should (derived-mode-p 'pilish-chat-mode))
          (should (eq pilish--process 'mock-process))
          (should (equal buffer-file-name file))
          (should buffer-read-only))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest pilish-test-chat-mode-save-buffer-keeps-writing-to-bound-file ()
  "Later `save-buffer' keeps writing to the same file-backed chat buffer."
  (let ((file nil)
        (make-backup-files nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (pilish-chat-mode)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\n\nHello\n"))
            (setq file (pilish-test--write-chat-buffer
                        (current-buffer) "pilish-chat-save-"))
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "More\n"))
            (save-buffer)
            (should (derived-mode-p 'pilish-chat-mode))
            (should (equal buffer-file-name file))
            (should buffer-read-only))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string)
                           "Assistant\n=========\n\nHello\nMore\n"))))
      (ignore-errors (delete-file file)))))

(ert-deftest pilish-test-session-chat-write-file-preserves-canonical-name-and-directory ()
  "Session chat buffers keep their canonical identity after `write-file'."
  (let ((root (pilish-test--make-temp-directory
               "pilish-test-write-file-session-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pilish--start-process) (lambda (_) nil)))
          (setq chat (pilish--setup-session root nil)
                input (buffer-local-value 'pilish--input-buffer chat))
          (setq file (pilish-test--write-chat-buffer
                      chat "pilish-chat-session-" "Saved copy\n"))
          (with-current-buffer chat
            (should (equal (pilish--chat-session-buffer-name)
                           (pilish-test--chat-buffer-name root)))
            (should (equal (pilish--session-directory) root))
            (should (equal buffer-file-name file))
            (should buffer-read-only)))
      (pilish-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest pilish-test-input-mode-keeps-own-mode-with-markdown-default ()
  "pilish-input-mode keeps its identity with markdown highlighting."
  (with-temp-buffer
    (pilish-input-mode)
    (should (derived-mode-p 'pilish-input-mode))
    (should (derived-mode-p 'text-mode))
    (should-not md-ts-hide-markup)))

(ert-deftest pilish-test-input-mode-not-read-only ()
  "pilish-input-mode allows editing."
  (with-temp-buffer
    (pilish-input-mode)
    (should-not buffer-read-only)))

;;; Session Directory Detection

(ert-deftest pilish-test-session-directory-uses-project-root ()
  "Session directory is project root when in a project."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc . "/home/user/myproject/")))
              ((symbol-function 'project-root)
               (lambda (_) "/home/user/myproject/")))
      (should (equal (pilish--session-directory) "/home/user/myproject/")))))

(ert-deftest pilish-test-session-directory-falls-back-to-default ()
  "Session directory is default-directory when not in a project."
  (let ((default-directory "/tmp/somedir/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) nil)))
      (should (equal (pilish--session-directory) "/tmp/somedir/")))))

(ert-deftest pilish-test-session-directory-preserves-multi-hop-root ()
  "Session directory detection keeps multi-hop TRAMP project roots intact."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _)
                 '(vc . "/ssh:bastion|sudo:root@pi-host:/srv/project/")))
              ((symbol-function 'project-root)
               (lambda (_)
                 "/ssh:bastion|sudo:root@pi-host:/srv/project/")))
      (should (equal (pilish--session-directory)
                     "/ssh:bastion|sudo:root@pi-host:/srv/project/")))))

(ert-deftest pilish-test-session-directory-recovers-projectile-root ()
  "Recovers root when a backend returns a cons cell with no `project-root'.
Older projectile returns (projectile . DIR) but defines no method, raising
`cl-no-applicable-method' (issue #234)."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) (cons 'projectile "/home/user/proj/")))
              ((symbol-function 'project-root)
               (lambda (_proj)
                 (signal 'cl-no-applicable-method
                         (list 'project-root
                               (cons 'projectile "/home/user/proj/"))))))
      (should (equal (pilish--session-directory) "/home/user/proj/")))))

(ert-deftest pilish-test-session-directory-survives-malformed-backend ()
  "Degrades to `default-directory' when a backend's root can't be found.
Closes the category from issue #234: any instance without a usable
`(SYMBOL . DIR)' shape must not crash session startup."
  (let ((default-directory "/tmp/somedir/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) (vector 'weird-backend)))
              ((symbol-function 'project-root)
               (lambda (_proj)
                 (signal 'cl-no-applicable-method
                         (list 'project-root
                               (vector 'weird-backend))))))
      (should (equal (pilish--session-directory) "/tmp/somedir/")))))

;;; Buffer Linkage

(defvar-local pilish-test--activity-marker nil
  "Buffer-local marker used by activity-phase hook tests.")

(ert-deftest pilish-test-input-buffer-finds-chat ()
  "Input buffer can find associated chat buffer."
  (pilish-test-with-mock-session "/tmp/pilish-test-link1/"
    (with-current-buffer "*pilish-input:/tmp/pilish-test-link1/*"
      (should (eq (pilish--get-chat-buffer)
                  (get-buffer "*pilish-chat:/tmp/pilish-test-link1/*"))))))

(ert-deftest pilish-test-chat-buffer-finds-input ()
  "Chat buffer can find associated input buffer."
  (pilish-test-with-mock-session "/tmp/pilish-test-link2/"
    (with-current-buffer "*pilish-chat:/tmp/pilish-test-link2/*"
      (should (eq (pilish--get-input-buffer)
                  (get-buffer "*pilish-input:/tmp/pilish-test-link2/*"))))))

(ert-deftest pilish-test-activity-phase-functions-receive-session-buffers ()
  "Activity phase functions receive buffers, phases, and reason."
  (let ((calls nil)
        (dir "/tmp/pilish-test-activity-hook/"))
    (pilish-test-with-mock-session dir
      (let ((chat (get-buffer (pilish--buffer-name :chat dir)))
            (input (get-buffer (pilish--buffer-name :input dir)))
            (pilish-activity-phase-functions
             (list (lambda (chat-buf input-buf old-phase new-phase reason)
                     (push (list chat-buf input-buf old-phase new-phase reason)
                           calls)))))
        (with-current-buffer chat
          (pilish--set-activity-phase "thinking")
          (pilish--set-activity-phase "thinking"))
        (should (= (length calls) 1))
        (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                     (car calls)))
          (should (eq seen-chat chat))
          (should (eq seen-input input))
          (should (equal old-phase "idle"))
          (should (equal new-phase "thinking"))
          (should (eq reason 'phase-change)))))))

(ert-deftest pilish-test-reset-session-state-forces-idle-activity-phase ()
  "Session reset applies idle even when user display state needs resync."
  (let ((calls nil)
        (dir "/tmp/pilish-test-activity-reset/"))
    (pilish-test-with-mock-session dir
      (let ((chat (get-buffer (pilish--buffer-name :chat dir)))
            (input (get-buffer (pilish--buffer-name :input dir)))
            (pilish-activity-phase-functions
             (list (lambda (chat-buf input-buf old-phase new-phase reason)
                     (push (list chat-buf input-buf old-phase new-phase reason)
                           calls)))))
        (with-current-buffer chat
          (pilish--set-activity-phase "running")
          (setq calls nil)
          (pilish--reset-session-state))
        (should (= (length calls) 1))
        (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                     (car calls)))
          (should (eq seen-chat chat))
          (should (eq seen-input input))
          (should (equal old-phase "running"))
          (should (equal new-phase "idle"))
          (should (eq reason 'reset)))))))

(ert-deftest pilish-test-set-input-buffer-resyncs-activity-phase ()
  "Relinking an input buffer reapplies the current activity phase."
  (let ((calls nil)
        (dir "/tmp/pilish-test-activity-relink/")
        (new-input (generate-new-buffer " *pi-activity-relink-input*")))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let ((chat (get-buffer (pilish--buffer-name :chat dir)))
                (pilish-activity-phase-functions
                 (list (lambda (chat-buf input-buf old-phase new-phase reason)
                         (push (list chat-buf input-buf old-phase new-phase reason)
                               calls)))))
            (with-current-buffer chat
              (pilish--set-activity-phase "running")
              (setq calls nil)
              (pilish--set-input-buffer new-input))
            (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                         (cl-find-if (lambda (call)
                                       (eq (cadr call) new-input))
                                     calls)))
              (should (eq seen-chat chat))
              (should (eq seen-input new-input))
              (should (equal old-phase "running"))
              (should (equal new-phase "running"))
              (should (eq reason 'input-link)))))
      (when (buffer-live-p new-input)
        (kill-buffer new-input)))))

(ert-deftest pilish-test-set-input-buffer-clears-old-input-activity ()
  "Relinking input buffers lets hooks clean state from the old input."
  (let ((dir "/tmp/pilish-test-activity-relink-cleanup/")
        (new-input (generate-new-buffer " *pi-activity-relink-cleanup-input*")))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let ((chat (get-buffer (pilish--buffer-name :chat dir)))
                (old-input (get-buffer (pilish--buffer-name :input dir)))
                (pilish-activity-phase-functions
                 (list (lambda (_chat-buf input-buf _old-phase new-phase reason)
                         (when (buffer-live-p input-buf)
                           (with-current-buffer input-buf
                             (cond
                              ((eq reason 'input-unlink)
                               (setq pilish-test--activity-marker nil))
                              ((not (equal new-phase "idle"))
                               (setq pilish-test--activity-marker t)))))))))
            (with-current-buffer chat
              (pilish--set-activity-phase "running"))
            (with-current-buffer old-input
              (should pilish-test--activity-marker))
            (with-current-buffer chat
              (pilish--set-input-buffer new-input))
            (with-current-buffer old-input
              (should-not pilish-test--activity-marker))
            (with-current-buffer new-input
              (should pilish-test--activity-marker))))
      (when (buffer-live-p new-input)
        (kill-buffer new-input)))))

(ert-deftest pilish-test-activity-phase-reason-distinguishes-relink-from-idle ()
  "Relinking input buffers does not look like a real idle transition."
  (let ((finished-notifications 0)
        (dir "/tmp/pilish-test-activity-relink-reason/")
        (new-input (generate-new-buffer " *pi-activity-relink-reason-input*")))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let ((chat (get-buffer (pilish--buffer-name :chat dir)))
                (pilish-activity-phase-functions
                 (list (lambda (_chat-buf _input-buf old-phase new-phase reason)
                         (when (and (eq reason 'phase-change)
                                    (not (equal old-phase "idle"))
                                    (equal new-phase "idle"))
                           (setq finished-notifications
                                 (1+ finished-notifications)))))))
            (with-current-buffer chat
              (pilish--set-activity-phase "running")
              (pilish--set-input-buffer new-input))
            (should (= finished-notifications 0))
            (with-current-buffer chat
              (pilish--set-activity-phase "idle"))
            (should (= finished-notifications 1))))
      (when (buffer-live-p new-input)
        (kill-buffer new-input)))))

(ert-deftest pilish-test-chat-buffer-kill-forces-teardown-activity-phase ()
  "Killing a chat buffer applies idle with teardown as the reason."
  (let ((calls nil)
        (dir "/tmp/pilish-test-activity-teardown/"))
    (pilish-test-with-mock-session dir
      (let ((chat (get-buffer (pilish--buffer-name :chat dir)))
            (input (get-buffer (pilish--buffer-name :input dir)))
            (pilish-activity-phase-functions
             (list (lambda (chat-buf input-buf old-phase new-phase reason)
                     (push (list chat-buf input-buf old-phase new-phase reason)
                           calls)))))
        (with-current-buffer chat
          (pilish--set-activity-phase "running"))
        (setq calls nil)
        (kill-buffer chat)
        (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                     (cl-find-if (lambda (call)
                                   (and (eq (nth 4 call) 'teardown)
                                        (equal (nth 2 call) "running")
                                        (equal (nth 3 call) "idle")))
                                 calls)))
          (should (eq seen-chat chat))
          (should (or (null seen-input)
                      (eq seen-input input)))
          (should (equal old-phase "running"))
          (should (equal new-phase "idle"))
          (should (eq reason 'teardown)))))))

(ert-deftest pilish-test-get-process-from-chat ()
  "Can get process from chat buffer."
  (let ((default-directory "/tmp/pilish-test-proc1/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) fake-proc))
              ((symbol-function 'pilish--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pilish)
            (with-current-buffer "*pilish-chat:/tmp/pilish-test-proc1/*"
              (should (eq (pilish--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*pilish-chat:/tmp/pilish-test-proc1/*"))
        (ignore-errors (kill-buffer "*pilish-input:/tmp/pilish-test-proc1/*"))))))

(ert-deftest pilish-test-get-process-from-input ()
  "Can get process from input buffer via chat buffer."
  (let ((default-directory "/tmp/pilish-test-proc2/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) fake-proc))
              ((symbol-function 'pilish--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pilish)
            (with-current-buffer "*pilish-input:/tmp/pilish-test-proc2/*"
              (should (eq (pilish--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*pilish-chat:/tmp/pilish-test-proc2/*"))
        (ignore-errors (kill-buffer "*pilish-input:/tmp/pilish-test-proc2/*"))))))

(ert-deftest pilish-test-display-buffers-uses-current-frame-window-list ()
  "`pilish--display-buffers' should query windows in current frame only."
  (let ((root "/tmp/pilish-test-display-frame-local/")
        (all-frames-args nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat))
                 (orig-get-buffer-window-list (symbol-function 'get-buffer-window-list)))
            (delete-other-windows)
            (cl-letf (((symbol-function 'get-buffer-window-list)
                       (lambda (buffer minibuf &optional all-frames)
                         (push all-frames all-frames-args)
                         (funcall orig-get-buffer-window-list buffer minibuf all-frames))))
              (pilish--display-buffers chat input))
            (should-not (memq t all-frames-args)))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-chat-mode-map-binds-commands ()
  "Chat mode map binds abort, session, context, model, and info chords."
  (dolist (expected '(("C-c C-k" . pilish-abort)
                      ("C-c C-n" . pilish-new-session)
                      ("C-c C-r" . pilish-session-browser)
                      ("C-c C-e" . pilish-export-html)
                      ("C-c C-c" . pilish-compact)
                      ("C-c C-m" . pilish-select-model)
                      ("C-c C-t" . pilish-cycle-thinking)
                      ("C-c C-y" . pilish-copy-last-message)))
    (should (eq (lookup-key pilish-chat-mode-map
                            (kbd (car expected)))
                (cdr expected)))))

(ert-deftest pilish-test-display-buffers-soft-dedicates-input-window ()
  "Input window should be soft-dedicated so `display-buffer' skips it."
  (let ((root "/tmp/pilish-test-dedicated/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--display-buffers chat input)
            (should (eq 'side (window-dedicated-p
                               (get-buffer-window input)))))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-display-buffers-chat-only-when-show-input-nil ()
  "SHOW-INPUT nil displays only the chat window."
  (let ((root "/tmp/pilish-test-display-chat-only/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--display-buffers chat input t)
            (should (get-buffer-window chat))
            (should-not (get-buffer-window input)))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-open-input-splits-below-chat ()
  "`pilish-open-input' opens a soft-dedicated input window below chat."
  (let ((root "/tmp/pilish-test-open-input-split/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--display-buffers chat input t)
            (select-window (get-buffer-window chat))
            (pilish-open-input)
            (let ((input-win (get-buffer-window input)))
              (should input-win)
              (should (eq (selected-window) input-win))
              (should (eq 'side (window-dedicated-p input-win)))
              (should (eq (window-in-direction 'above input-win)
                          (get-buffer-window chat)))))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-open-input-focuses-visible-input ()
  "`pilish-open-input' selects an already-visible input window."
  (let ((root "/tmp/pilish-test-open-input-focus/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--display-buffers chat input)
            (select-window (get-buffer-window chat))
            (pilish-open-input)
            (should (eq (selected-window) (get-buffer-window input)))
            (should (= 2 (length (window-list nil 'no-mini)))))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-send-hides-input-window-on-demand ()
  "Sending hides the input window when display is `on-demand'."
  (let ((root "/tmp/pilish-test-send-hide/")
        (pilish-input-window-display 'on-demand))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil))
              ((symbol-function 'pilish--prepare-and-send) #'ignore))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--display-buffers chat input)
            (with-current-buffer input
              (insert "Hello, pi!")
              (pilish-send))
            (should-not (get-buffer-window input))
            (should (eq (selected-window) (get-buffer-window chat))))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-send-keeps-input-window-when-always ()
  "Sending keeps the input window when display is `always'."
  (let ((root "/tmp/pilish-test-send-keep/")
        (pilish-input-window-display 'always))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil))
              ((symbol-function 'pilish--prepare-and-send) #'ignore))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--display-buffers chat input)
            (with-current-buffer input
              (insert "Hello, pi!")
              (pilish-send))
            (should (get-buffer-window input)))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-show-session-buffers-hidden-launches-chat-only ()
  "A fresh session launches chat-only when display is `hidden'.
`pilish--show-session-buffers' honors
`pilish-input-window-display', so a `hidden' session starts
without an input window."
  (let ((root "/tmp/pilish-test-show-hidden/")
        (pilish-input-window-display 'hidden))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--show-session-buffers chat input)
            (should (get-buffer-window chat))
            (should-not (get-buffer-window input)))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-send-hides-input-window-when-hidden ()
  "Sending hides the input window when display is `hidden'."
  (let ((root "/tmp/pilish-test-send-hide-hidden/")
        (pilish-input-window-display 'hidden))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil))
              ((symbol-function 'pilish--prepare-and-send) #'ignore))
      (unwind-protect
          (let* ((chat (pilish--setup-session root nil))
                 (input (buffer-local-value 'pilish--input-buffer chat)))
            (delete-other-windows)
            (pilish--display-buffers chat input)
            (with-current-buffer input
              (insert "Hello, pi!")
              (pilish-send))
            (should-not (get-buffer-window input))
            (should (eq (selected-window) (get-buffer-window chat))))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pilish-test-hide-session-windows-uses-current-frame-window-list ()
  "`pilish--hide-session-windows' should query current frame windows only."
  (let ((root "/tmp/pilish-test-hide-frame-local/")
        (all-frames-args nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pilish)
            (let ((chat (get-buffer (pilish-test--chat-buffer-name root)))
                  (orig-get-buffer-window-list (symbol-function 'get-buffer-window-list)))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'get-buffer-window-list)
                           (lambda (buffer minibuf &optional all-frames)
                             (push all-frames all-frames-args)
                             (funcall orig-get-buffer-window-list buffer minibuf all-frames))))
                  (pilish--hide-session-windows)))
              (should-not (memq t all-frames-args))))
        (pilish-test--kill-session-buffers root)
        (delete-other-windows)))))

;;; Chat Keymap

(ert-deftest pilish-test-chat-mode-map-shell-command-at-point ()
  "The chat `!' key runs one file command without changing other actions."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--set-chat-session-identity "/tmp/project/")
    (let ((inhibit-read-only t))
      (insert "src/report.el"))
    (goto-char (+ (point-min) 2))
    (let (prompt command command-directory)
      (cl-letf (((symbol-function 'read-shell-command)
                 (lambda (value)
                   (setq prompt value)
                   "file *"))
                ((symbol-function 'shell-command)
                 (lambda (value)
                   (setq command value
                         command-directory default-directory))))
        (let ((binding (key-binding (kbd "!"))))
          (should (eq binding #'pilish-shell-command-at-point))
          (call-interactively binding)))
      (should (equal "! on src/report.el: " prompt))
      (should (equal "file /tmp/project/src/report.el" command))
      (should (equal "/tmp/project/" command-directory)))
    (should (eq (lookup-key pilish-chat-mode-map (kbd "RET"))
                #'pilish-visit-file))
    (should (eq (lookup-key pilish-chat-mode-map
                            [remap push-button])
                #'pilish--dispatch-button))
    (dolist (key '("&" "E" "o"))
      (should-not (lookup-key pilish-chat-mode-map (kbd key))))))

(ert-deftest pilish-test-chat-mode-map-copy-file-path ()
  "The chat `w' key copies the same shell-local path used by `!'."
  (with-temp-buffer
    (pilish-chat-mode)
    (pilish--set-chat-session-identity "/tmp/project/")
    (let ((inhibit-read-only t))
      (insert "src/report.el:7"))
    (goto-char (+ (point-min) 2))
    (let ((kill-ring nil)
          (kill-ring-yank-pointer nil)
          (messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (let ((binding (key-binding (kbd "w"))))
          (should (eq binding #'pilish-copy-file-path))
          (call-interactively binding)))
      (should (equal (car kill-ring) "/tmp/project/src/report.el"))
      (should (member "Pi: Copied /tmp/project/src/report.el" messages)))))

;;; Startup Header

(ert-deftest pilish-test-startup-header-shows-keybindings ()
  "Startup header includes key keybindings."
  (let ((header (pilish--format-startup-header)))
    (should (string-match-p "C-c C-c" header))
    (should (string-match-p "send" header))
    (should (string-match-p "C-c C-r   sessions" header))
    (should (string-match-p "C-c C-p   menu\n\npilish" header))))

(ert-deftest pilish-test-startup-header-shows-label ()
  "Startup header labels the buffer with the project title."
  (let ((header (pilish--format-startup-header)))
    (should (string-equal "Pilish" (car (split-string header "\n"))))))

;;;; Startup Logo

(ert-deftest pilish-test-startup-header-decorates-heading-with-logo ()
  "Startup heading carries the logo as a display-only line-prefix.
The raw separator text stays byte-identical; the property covers the
label line through its newline and never the setext underline."
  (let ((header (pilish--format-startup-header)))
    (should (string-equal "Pilish\n======\n"
                          (substring-no-properties header 0 14)))
    (should (get-text-property 0 'line-prefix header))
    (should (get-text-property 6 'line-prefix header))
    (should-not (get-text-property 7 'line-prefix header))))

(ert-deftest pilish-test-logo-file-form-evaluates-without-load-file-name ()
  "The `pilish--logo-file' form evaluates when `load-file-name' is nil.
Covers the eval-buffer/eval-region dev flow over already-loaded
Pilish: the fallback chain must answer via `symbol-file' without
signaling, or evaluation of pilish-ui.el aborts at the defconst."
  (let* ((lib (or (symbol-file 'pilish--make-separator 'defun)
                  (locate-library "pilish-ui")))
         (source (and lib
                      (if (string-suffix-p ".elc" lib)
                          (concat (substring lib 0 -1))
                        lib))))
    (if (not (and source (file-exists-p source)))
        (ert-skip "pilish-ui.el source not readable")
      (let ((form (catch 'found
                    (with-temp-buffer
                      (insert-file-contents source)
                      (goto-char (point-min))
                      (condition-case nil
                          (while t
                            (let ((f (read (current-buffer))))
                              (when (eq (nth 1 f) 'pilish--logo-file)
                                (throw 'found f))))
                        (end-of-file)))))
            (load-file-name nil))
        (should (eq 'defconst (car-safe form)))
        (let ((value (eval (nth 2 form))))
          (should (stringp value))
          (should (file-name-absolute-p value))
          (should (string-suffix-p "assets/pilish-logo.svg" value)))))))

(defun pilish-test--startup-logo-glyph-specs (prefix)
  "Return the two display specs of logo PREFIX for structural checks."
  (mapcar (lambda (i) (get-text-property i 'display prefix)) '(0 1)))

(ert-deftest pilish-test-startup-logo-prefix-structure ()
  "Logo prefix glyphs are conditional and collapse on plain displays.
Each glyph's display value pairs a `(when ...)' spec gated on
`pilish--startup-logo-displayable-p' with a zero-width space fallback,
so text terminals and no-SVG displays show neither logo nor gap.  The
image glyph renders the shipped asset adapted for theme matching: no
backplate, currentColor body, violet horns kept, 1.5em height."
  (let* ((header (pilish--format-startup-header))
         (prefix (get-text-property 0 'line-prefix header))
         (specs (pilish-test--startup-logo-glyph-specs prefix)))
    (should (stringp prefix))
    (should (= 2 (length prefix)))
    (dolist (i '(0 1))
      (should (eq 'md-ts-heading-1 (get-text-property i 'face prefix))))
    ;; Inert decoration: no interactive properties on either glyph.
    (dolist (i '(0 1))
      (should-not (get-text-property i 'keymap prefix))
      (should-not (get-text-property i 'mouse-face prefix))
      (should-not (get-text-property i 'help-echo prefix)))
    (pcase-let ((`(,logo-display ,gap-display) specs))
      (dolist (display (list logo-display gap-display))
        (let ((conditional (nth 0 display))
              (fallback (nth 1 display)))
          (should (eq 'when (car conditional)))
          (should (equal '(pilish--startup-logo-displayable-p)
                         (nth 1 conditional)))
          (should (equal '(space . (:width 0)) fallback))))
      (let* ((logo-conditional (nth 0 logo-display))
             (payload (nthcdr 2 logo-conditional)))
        (should (eq 'image (car payload)))
        (should (eq 'svg (plist-get (cdr payload) :type)))
        (should (equal '(1.5 . em) (plist-get (cdr payload) :height)))
        (should (eq 1 (plist-get (cdr payload) :scale)))
        (should (eq 'center (plist-get (cdr payload) :ascent)))
        (let ((data (plist-get (cdr payload) :data)))
          (should (string-match-p "currentColor" data))
          (should (string-match-p "#A970FF" data))
          (should-not (string-match-p "#EEF2E8" data))
          (should-not (string-match-p "#07100F" data))
          (should-not (string-match-p "<rect" data))))
      (should (equal '(space . (:width 0.75))
                     (nthcdr 2 (nth 0 gap-display)))))))

(ert-deftest pilish-test-startup-logo-displayable-p-gates-both-display-and-svg ()
  "The predicate needs image display AND SVG support.
Batch Emacs reports SVG available with no display, so either alone
must not enable the logo."
  (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _f) t))
            ((symbol-function 'image-type-available-p) (lambda (&optional _t) t)))
    (should (pilish--startup-logo-displayable-p)))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _f) nil))
            ((symbol-function 'image-type-available-p) (lambda (&optional _t) t)))
    (should-not (pilish--startup-logo-displayable-p)))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _f) t))
            ((symbol-function 'image-type-available-p) (lambda (&optional _t) nil)))
    (should-not (pilish--startup-logo-displayable-p))))

(ert-deftest pilish-test-startup-header-omits-logo-when-asset-missing ()
  "Missing asset means a plain heading: no prefix, no error, no warning."
  (let ((pilish--logo-file "/nonexistent/pilish-logo.svg")
        (pilish--logo-svg-cache nil)
        (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (let ((header (pilish--format-startup-header)))
        (should (string-equal "Pilish\n======\n"
                              (substring-no-properties header 0 14)))
        (should-not (get-text-property 0 'line-prefix header))
        (should (null warnings))))))

(ert-deftest pilish-test-startup-header-logo-preserves-copy-and-single-decoration ()
  "Displayed startup header copies as plain text with one decoration.
The filtered copy still returns the bare heading; exactly one heading
carries the prefix; font-lock-ensure keeps the decoration and the
heading face in place."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert (pilish--format-startup-header)))
    (font-lock-ensure)
    (should (equal "Pilish\n" (pilish--filter-buffer-substring 1 8)))
    (should (equal "Pilish\n======"
                   (buffer-substring-no-properties 1 14)))
    ;; Exactly the seven heading-line characters carry the prefix.
    (let ((decorated 0) (pos (point-min)))
      (while (< pos (point-max))
        (when (get-text-property pos 'line-prefix)
          (cl-incf decorated))
        (setq pos (1+ pos)))
      (should (= 7 decorated))
      (should (eq 'md-ts-heading-1 (get-text-property 1 'face))))))

(defun pilish-test--banner-commands ()
  "Return a command fixture with two prompts and two skills."
  (list '(:name "create-todo" :description "New todo" :source "prompt")
        '(:name "fix-tests" :description "Fix tests" :source "prompt")
        '(:name "skill:aws-sso" :description "AWS SSO" :source "skill")
        '(:name "skill:uv" :description "uv runner" :source "skill")))

(ert-deftest pilish-test-startup-header-shows-summary-line ()
  "Startup header ends with a compact summary line after the keybindings.
Counts come from `pilish--commands' and the pi version from
`pilish--process-version'."
  (let ((dir (pilish-test--make-temp-directory "pilish-test-banner-summary-")))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let ((chat (get-buffer (pilish-test--chat-buffer-name dir))))
            (with-current-buffer chat
              (setq pilish--process-version "0.84.2"
                    pilish--commands (pilish-test--banner-commands))
              (pilish--display-startup-header)
              (should (string-match-p
                       (regexp-quote
                        (format "pi v0.84.2 · pilish %s · 2 skills · 2 prompts · TAB details"
                                pilish-version))
                       (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest pilish-test-startup-header-omits-missing-summary-data ()
  "Summary line drops the pi version and count segments when data is missing.
Never renders misleading zero counts."
  (let ((dir (pilish-test--make-temp-directory "pilish-test-banner-omitted-")))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let* ((chat (get-buffer (pilish-test--chat-buffer-name dir)))
                 (text (with-current-buffer chat (buffer-string)))
                 (lines (split-string text "\n")))
            (should (member (format "pilish %s · TAB details" pilish-version)
                            lines))
            (should-not (string-match-p "pi v" text))
            (should-not (string-match-p "[0-9]+ skills" text))
            (should-not (string-match-p "[0-9]+ prompts" text))))
      (delete-directory dir t))))

(ert-deftest pilish-test-refresh-startup-banner-fills-summary-line ()
  "Refreshing the banner replaces the summary line in place, idempotently."
  (let ((dir (pilish-test--make-temp-directory "pilish-test-banner-refresh-")))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let ((chat (get-buffer (pilish-test--chat-buffer-name dir))))
            (with-current-buffer chat
              ;; The mock session displayed the banner with no data yet.
              (setq pilish--process-version "0.84.2")
              (pilish--set-commands (pilish-test--banner-commands))
              (pilish--refresh-startup-banner)
              (should (string-match-p
                       (regexp-quote
                        (format "pi v0.84.2 · pilish %s · 2 skills · 2 prompts · TAB details"
                                pilish-version))
                       (buffer-string)))
              (should-not (member (format "pilish %s · TAB details" pilish-version)
                                  (split-string (buffer-string) "\n")))
              (let ((after-first (buffer-string)))
                (pilish--refresh-startup-banner)
                (should (equal (buffer-string) after-first))))))
      (delete-directory dir t))))

(ert-deftest pilish-test-refresh-startup-banner-noop-without-banner ()
  "Refreshing the banner is a no-op when the buffer shows no banner."
  (let ((dir (pilish-test--make-temp-directory "pilish-test-banner-noop-")))
    (unwind-protect
        (pilish-test-with-mock-session dir
          (let ((chat (get-buffer (pilish-test--chat-buffer-name dir))))
            (with-current-buffer chat
              (let ((inhibit-read-only t))
                (erase-buffer))
              ;; Must not signal and must not insert anything.
              (pilish--refresh-startup-banner)
              (should-not (string-match-p "TAB details" (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest pilish-test-startup-context-files-scan ()
  "Context scan walks up from DIRECTORY collecting AGENTS-family files.
The user-level file comes first, then nearest directory first walking
upward; in a given directory AGENTS.override.md wins and shadows the
other family names there."
  (let* ((root (pilish-test--make-temp-directory "pilish-test-banner-context-"))
         (user-dir (expand-file-name "home" root))
         (walk-dir (expand-file-name "walk" root))
         (sub-dir (expand-file-name "sub" walk-dir))
         (proj-dir (expand-file-name "proj" sub-dir))
         (user-file (expand-file-name "AGENTS.md" user-dir))
         (proj-file (expand-file-name "CLAUDE.md" proj-dir))
         (override-file (expand-file-name "AGENTS.override.md" sub-dir))
         (walk-file (expand-file-name "AGENTS.md" walk-dir)))
    (make-directory user-dir t)
    (make-directory proj-dir t)
    ;; sub-dir carries both the override and a plain AGENTS.md; only the
    ;; override may be reported.
    (dolist (file (list user-file proj-file override-file
                        (expand-file-name "AGENTS.md" sub-dir) walk-file))
      (with-temp-file file (insert "agents\n")))
    (unwind-protect
        (should (equal (pilish--startup-context-files proj-dir user-dir)
                       (list user-file proj-file override-file walk-file)))
      (delete-directory root t))))

(ert-deftest pilish-test-extract-pi-version-from-clean-output ()
  "Extract the plain semantic version returned by pi."
  (should (equal (pilish--extract-pi-version "0.79.1\n")
                 "0.79.1")))

(ert-deftest pilish-test-extract-pi-version-from-stderr-style-output ()
  "Ignore npm warnings and extract the standalone pi version line."
  (should (equal (pilish--extract-pi-version
                  "npm warn deprecated package@1.0.0: old\n0.79.1\n")
                 "0.79.1")))

(ert-deftest pilish-test-extract-pi-version-returns-nil-for-unparseable-output ()
  "Unparseable version output should be harmless."
  (should-not (pilish--extract-pi-version "npm warn only\n")))

(ert-deftest pilish-test-pi-version-outdated-compares-segments-numerically ()
  "Compare pi versions numerically against the 1.0.0 floor."
  (should (equal pilish--minimum-pi-version "1.0.0"))
  (should (pilish--pi-version-outdated-p "0.85.0"))
  (should (pilish--pi-version-outdated-p "0.87.1"))
  (should (pilish--pi-version-outdated-p "0.99.99"))
  (should-not (pilish--pi-version-outdated-p "1.0.0"))
  (should-not (pilish--pi-version-outdated-p "1.0.1"))
  (should-not (pilish--pi-version-outdated-p "1.10.0")))

(ert-deftest pilish-test-finish-pi-version-process-parses-stderr ()
  "Version probing should accept pi versions printed to stderr."
  (let ((proc (start-process "pilish-test-version" nil "cat"))
        (stdout-buf (generate-new-buffer " *pi-test-version-stdout*"))
        (stderr-buf (generate-new-buffer " *pi-test-version-stderr*"))
        (resolved-version nil))
    (unwind-protect
        (progn
          (with-current-buffer stderr-buf
            (insert "npm warn deprecated package@1.0.0: old\n0.79.1\n"))
          (process-put proc 'pilish-version-callback
                       (lambda (version)
                         (setq resolved-version version)))
          (process-put proc 'pilish-version-stdout-buf stdout-buf)
          (process-put proc 'pilish-version-stderr-buf stderr-buf)
          (pilish--finish-pi-version-process proc)
          (should (equal resolved-version "0.79.1"))
          (should-not (buffer-live-p stdout-buf))
          (should-not (buffer-live-p stderr-buf)))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf)))))

(ert-deftest pilish-test-request-pi-version-async-waits-before-probe ()
  "Version lookup waits briefly before starting the probe process."
  (let ((scheduled-delay nil)
        (scheduled-directory nil)
        (resolved-version nil))
    (cl-letf (((symbol-function 'pilish--run-pi-version-once-async)
               (lambda (callback &optional directory)
                 (setq scheduled-directory directory)
                 (funcall callback "0.79.1")))
              ((symbol-function 'run-at-time)
               (lambda (secs _repeat fn &rest args)
                 (setq scheduled-delay secs)
                 (apply fn args)
                 'mock-timer)))
      (let ((default-directory "/ssh:pi-host:/home/pi/project/"))
        (pilish--request-pi-version-async
         (lambda (version)
           (setq resolved-version version)))))
    (should (= scheduled-delay pilish--version-probe-delay))
    (should (equal scheduled-directory "/ssh:pi-host:/home/pi/project/"))
    (should (equal resolved-version "0.79.1"))))

(ert-deftest pilish-test-run-pi-version-uses-default-directory-file-handler ()
  "Version probes let `default-directory' file handlers create the process."
  (let ((pilish-executable '("pi"))
        (captured nil)
        (captured-default-directory nil)
        (dummy-proc (start-process "pilish-test-version-capture" nil "cat")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured args
                             captured-default-directory default-directory)
                       dummy-proc)))
            (pilish--run-pi-version-once-async
             #'ignore "/ssh:pi-host:/home/pi/project/"))
          (should (eq (plist-get captured :file-handler) t))
          (should (equal captured-default-directory
                         "/ssh:pi-host:/home/pi/project/"))
          (should (bufferp (plist-get captured :buffer)))
          (should (bufferp (plist-get captured :stderr))))
      (when-let* ((stdout-buf (plist-get captured :buffer)))
        (when (buffer-live-p stdout-buf)
          (kill-buffer stdout-buf)))
      (when-let* ((stderr-buf (plist-get captured :stderr)))
        (when (buffer-live-p stderr-buf)
          (kill-buffer stderr-buf)))
      (when (process-live-p dummy-proc)
        (delete-process dummy-proc)))))

(ert-deftest pilish-test-probe-process-version-uses-chat-session-directory ()
  "Version probing uses the stable chat session directory."
  (let ((captured-default-directory nil))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq default-directory "/tmp/transcript/"
            pilish--canonical-session-directory
            "/ssh:pi-host:/home/pi/project/")
      (cl-letf (((symbol-function 'pilish--request-pi-version-async)
                 (lambda (_callback)
                   (setq captured-default-directory default-directory))))
        (pilish--probe-process-version-async (current-buffer)))
      (should (equal captured-default-directory
                     "/ssh:pi-host:/home/pi/project/")))))

(ert-deftest pilish-test-process-replacement-invalidates-model-change ()
  "A model callback cannot mutate state after its target process is replaced."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--process 'old-process)
    (let ((token (pilish--begin-model-change
                  'old-process (current-buffer))))
      (should (pilish--model-change-current-p token))
      (pilish--set-process 'new-process)
      (should-not (pilish--model-change-current-p token))
      (should-not (pilish--model-change-pending-p)))))

(ert-deftest pilish-test-set-process-probes-version-for-current-process ()
  "Setting process starts version probe and stores result for current process."
  (let ((callback nil)
        (messages nil)
        (noninteractive nil)
        (proc (start-process "pilish-test-proc" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (cl-letf (((symbol-function 'pilish--request-pi-version-async)
                     (lambda (cb)
                       (setq callback cb)
                       nil))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (pilish--set-process proc)
            (should callback)
            (funcall callback "0.79.1")
            (should (equal pilish--process-version "0.79.1"))
            (should (equal (car messages) "Pi: version 0.79.1"))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest pilish-test-set-process-version-callback-uses-chat-buffer-context ()
  "Version callback updates chat buffer even when current buffer changed."
  (let ((callback nil)
        (messages nil)
        (noninteractive nil)
        (proc (start-process "pilish-test-proc-a" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pilish-chat-mode)
          (let ((chat-buf (current-buffer)))
            (cl-letf (((symbol-function 'pilish--request-pi-version-async)
                       (lambda (cb)
                         (setq callback cb)
                         nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (pilish--set-process proc)
              (with-temp-buffer
                (funcall callback "0.79.1"))
              (with-current-buffer chat-buf
                (should (equal pilish--process-version "0.79.1")))
              (should (equal (car messages) "Pi: version 0.79.1")))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest pilish-test-probe-process-version-warns-when-pi-too-old ()
  "Version probe warns clearly for every tested below-minimum pi version."
  (dolist (version '("0.85.0" "0.87.1" "0.99.99"))
    (ert-info ((format "Unsupported Pi %s" version))
      (let ((callback nil)
            (warnings nil)
            (noninteractive nil)
            (proc (start-process "pilish-test-proc-old" nil "cat")))
        (unwind-protect
            (with-temp-buffer
              (pilish-chat-mode)
              (cl-letf (((symbol-function 'pilish--request-pi-version-async)
                         (lambda (cb)
                           (setq callback cb)
                           nil))
                        ((symbol-function 'message) #'ignore)
                        ((symbol-function 'display-warning)
                         (lambda (&rest args)
                           (push args warnings))))
                (pilish--set-process proc)
                (should callback)
                (funcall callback version)
                (should (equal pilish--process-version version))
                (should (equal (length warnings) 1))
                (should (equal (car (car warnings)) 'pi))
                (let ((warning-text (nth 1 (car warnings))))
                  (should (string-match-p
                           "requires Pi 1.0.0 or later" warning-text))
                  (should (string-match-p
                           (format "found Pi %s" version) warning-text))
                  (should (string-match-p
                           "npm install -g @earendil-works/pi-coding-agent"
                           warning-text)))))
          (when (process-live-p proc)
            (delete-process proc)))))))

(ert-deftest pilish-test-probe-process-version-does-not-warn-when-supported ()
  "Version probe accepts Pi 1.0.0 exactly and newer versions without warning."
  (dolist (version '("1.0.0" "1.0.1" "1.10.0"))
    (ert-info ((format "Supported Pi %s" version))
      (let ((callback nil)
            (warning-called nil)
            (noninteractive nil)
            (proc (start-process "pilish-test-proc-supported" nil "cat")))
        (unwind-protect
            (with-temp-buffer
              (pilish-chat-mode)
              (cl-letf (((symbol-function 'pilish--request-pi-version-async)
                         (lambda (cb)
                           (setq callback cb)
                           nil))
                        ((symbol-function 'message) #'ignore)
                        ((symbol-function 'display-warning)
                         (lambda (&rest _)
                           (setq warning-called t))))
                (pilish--set-process proc)
                (should callback)
                (funcall callback version)
                (should (equal pilish--process-version version))
                (should-not warning-called)))
          (when (process-live-p proc)
            (delete-process proc)))))))

;;; Copy Visible Text

(defmacro pilish-test--with-chat-markup (markdown &rest body)
  "Insert MARKDOWN into a chat-mode buffer, fontify, then run BODY.
Buffer is read-only with `inhibit-read-only' used for insertion.
`font-lock-ensure' runs before BODY to apply invisible/display properties."
  (declare (indent 1) (debug (stringp body)))
  `(with-temp-buffer
     (pilish-chat-mode)
     (let ((inhibit-read-only t))
       (insert ,markdown))
     (font-lock-ensure)
     ,@body))

(ert-deftest pilish-test-visible-text-strips-bold-markers ()
  "visible-text strips invisible bold markers (**)."
  (pilish-test--with-chat-markup "Hello **bold** world"
    (should (equal (pilish--visible-text (point-min) (point-max))
                   "Hello bold world"))))

(ert-deftest pilish-test-visible-text-strips-inline-code-backticks ()
  "visible-text strips invisible backticks around inline code."
  (pilish-test--with-chat-markup "Use `foo` here"
    (should (equal (pilish--visible-text (point-min) (point-max))
                   "Use foo here"))))

(ert-deftest pilish-test-visible-text-strips-code-fences ()
  "visible-text strips invisible code fences and language label."
  (pilish-test--with-chat-markup "```python\ndef foo():\n    pass\n```\n"
    (let ((result (pilish--visible-text (point-min) (point-max))))
      (should (string-match-p "def foo" result))
      (should-not (string-match-p "```" result))
      (should-not (string-match-p "python" result)))))

(ert-deftest pilish-test-visible-text-strips-setext-underline ()
  "visible-text strips setext underlines (hidden by md-ts-hide-markup)."
  (pilish-test--with-chat-markup "Assistant\n=========\n\nHello\n"
    (let ((result (pilish--visible-text (point-min) (point-max))))
      (should (string-match-p "Assistant" result))
      (should-not (string-match-p "=====" result))
      (should (string-match-p "Hello" result)))))

(ert-deftest pilish-test-visible-text-strips-atx-heading-prefix ()
  "visible-text strips invisible ATX heading prefix characters."
  (pilish-test--with-chat-markup "## Code Example\n\nSome text\n"
    (let ((result (pilish--visible-text (point-min) (point-max))))
      (should (string-match-p "Code Example" result))
      (should (string-match-p "Some text" result))
      (should-not (string-match-p "^##" result)))))

(ert-deftest pilish-test-visible-text-preserves-plain-text ()
  "visible-text preserves text that has no hidden markup."
  (pilish-test--with-chat-markup "Just plain text with no markup"
    (should (equal (pilish--visible-text (point-min) (point-max))
                   "Just plain text with no markup"))))

(ert-deftest pilish-test-visible-text-position-map-preserves-source-envelope ()
  "Visible character indices map exactly across omitted property spans."
  (with-temp-buffer
    (insert "aXXbcYYd")
    (put-text-property 2 4 'invisible 'md-ts--markup)
    (put-text-property 6 8 'invisible 'md-ts--markup)
    (let ((at-boundary
           (pilish--visible-text-with-position-map 1 9 6))
          (inside-hidden
           (pilish--visible-text-with-position-map 1 9 7)))
      (should (equal "abcd" (plist-get at-boundary :text)))
      (should (equal [1 4 5 8] (plist-get at-boundary :positions)))
      (should (= 3 (plist-get at-boundary :index)))
      (should (= 3 (plist-get inside-hidden :index)))
      ;; Visible [1,3) is "bc" and maps to the real half-open envelope 4..6.
      (let ((positions (plist-get at-boundary :positions)))
        (should (equal (cons (aref positions 1)
                             (1+ (aref positions 2)))
                       '(4 . 6)))))))

(ert-deftest pilish-test-copy-raw-markdown-defcustom-default ()
  "pilish-copy-raw-markdown defcustom defaults to nil."
  (should (eq pilish-copy-raw-markdown nil)))

(ert-deftest pilish-test-project-trust-policy-default ()
  "Project trust policy defaults to approving project-local Pi inputs."
  (should (eq pilish-project-trust-policy 'approve)))

(ert-deftest pilish-test-hot-tail-turn-count-defcustom-defaults ()
  "Hot-tail turn count defaults to 3 headed turns."
  (should (= 3 pilish-hot-tail-turn-count)))

(ert-deftest pilish-test-extension-status-properties-apply-in-header-line ()
  "Extension status properties are applied by status key in the header line."
  (let ((pilish-extension-status-faces
         '(("sub-status:usage" . (:foreground "#c6a0f6")))))
    (with-temp-buffer
      (pilish-chat-mode)
      (setq pilish--state '(:model "claude-sonnet-4")
            pilish--extension-status
            '(("solveit-mode" . "⚡ concise")
              ("sub-status:usage" . "4h51m 1% · 9h9m 41%")))
      (let ((header (pilish--header-line-string)))
        (should (string-match-p "⚡ concise · 4h51m 1%% · 9h9m 41%%"
                                (substring-no-properties header)))
        (should-not (get-text-property (string-match-p "⚡" header) 'face header))
        (should (equal (get-text-property (string-match-p "⚡" header)
                                          'help-echo header)
                       "solveit-mode"))
        (should (eq (get-text-property (string-match-p "⚡" header)
                                       'mouse-face header)
                    'highlight))
        (should (equal (get-text-property (string-match-p "4h" header) 'face header)
                       '(:foreground "#c6a0f6")))
        (should (equal (get-text-property (string-match-p "4h" header)
                                          'help-echo header)
                       "sub-status:usage"))
        (should (eq (get-text-property (string-match-p "4h" header)
                                       'mouse-face header)
                    'highlight))))))

;;; Pending prompt acceptance presentation

(ert-deftest pilish-test-header-pending-acceptance-copy ()
  "Each unacknowledged prompt shows honest wait copy without reading config."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let (phases)
      (let ((pilish-activity-phase-functions
             (list (lambda (_chat _input _old new _reason) (push new phases)))))
        (dolist (text '("first ordinary prompt" "second ordinary prompt"))
          (with-current-buffer input (insert text) (pilish-send))
          (let ((request (car commands))
                (phases-before (copy-sequence phases))
                (commands-before (copy-tree commands)))
            (cl-letf (((symbol-function 'file-exists-p)
                       (lambda (&rest _) (ert-fail "Header checked disk config")))
                      ((symbol-function 'insert-file-contents)
                       (lambda (&rest _) (ert-fail "Header read disk config")))
                      ((symbol-function 'directory-files)
                       (lambda (&rest _) (ert-fail "Header scanned disk config"))))
              (should (string-match-p "waiting for Pi"
                                      (pilish-test--input-header input)))
              (dolist (buffer (list input chat))
                (let* ((header (with-current-buffer buffer (pilish--header-line-string)))
                       (start (string-match (regexp-quote "waiting for Pi") header)))
                  (should start)
                  (should (equal (get-text-property start 'help-echo header)
                                 "MCP servers may be connecting during startup."))
                  (should (eq (get-text-property start 'face header)
                              'pilish-activity-phase)))))
            (with-current-buffer chat
              (should (eq pilish--status 'sending))
              (should (equal pilish--activity-phase "thinking"))
              (should-not (pilish--prompt-wait-accepted pilish--prompt-wait)))
            (should (equal phases phases-before))
            (should (equal commands commands-before))
            ;; A handled acknowledgment permits the next ordinary submission.
            (pilish-test--stdout
             proc (list :type "response" :command "prompt" :success t
                        :id (plist-get request :id) :data '(:disposition "handled"))))))
      (should (equal (mapcar (lambda (command) (plist-get command :message))
                            (reverse commands))
                     '("first ordinary prompt" "second ordinary prompt"))))))

(ert-deftest pilish-test-header-pending-acceptance-acknowledgments ()
  "Acceptance redisplays normal thinking, handled idle, or queued feedback."
  (dolist (case '(((:disposition "started") sending "thinking" nil)
                  (nil sending "thinking" nil)
                  ((:disposition "handled") idle "idle" nil)
                  ((:disposition "queued") sending "queued"
                   "Pi: Prompt acknowledged as queued")))
    (ert-info ((format "prompt acknowledgment data=%S" (car case)))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let ((schedule (symbol-function 'run-at-time)) refreshed notices)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (seconds repeat function &rest args)
                       (if (eq function 'pilish--clear-sending-if-no-agent-start)
                           'fake-prompt-start-timer
                         (apply schedule seconds repeat function args))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) notices))))
            (with-current-buffer input (insert "ordinary prompt") (pilish-send))
            (let ((request (car commands)))
              (cl-letf (((symbol-function 'force-mode-line-update)
                         (lambda (&optional all)
                           (when (or all (eq (current-buffer) input))
                             (push (pilish-test--input-header input) refreshed)))))
                (pilish-test--stdout
                 proc (append (list :type "response" :command "prompt" :success t
                                    :id (plist-get request :id))
                              (when (car case) (list :data (car case))))))
              (should refreshed)
              (dolist (header refreshed)
                (should-not (string-match-p "waiting for Pi" header)))
              (let ((header (pilish-test--input-header input)))
                (should (string-match-p (nth 2 case) header))
                (should-not (string-match-p "waiting for Pi" header)))
              (with-current-buffer chat
                (should (eq pilish--status (nth 1 case)))
                (should (equal pilish--activity-phase (nth 2 case))))
              (should (equal notices (when (nth 3 case) (list (nth 3 case)))))
              (should (equal (mapcar (lambda (command) (plist-get command :type)) commands)
                             '("prompt"))))))))))

(ert-deftest pilish-test-header-pending-acceptance-observed-work ()
  "Start, echo, compaction, and ended independent runs outrank wait feedback."
  (dolist (case '((start ((:type "agent_start")) streaming "thinking")
                  (ended ((:type "agent_start") (:type "agent_end" :messages []))
                         sending "thinking")
                  (echo-only ((:type "message_start" :message
                                     (:role "user" :timestamp 1704067200000
                                      :content [(:type "text" :text "ordinary prompt")])))
                             sending "thinking")
                  (compaction ((:type "compaction_start" :reason "manual"))
                              compacting "compact")
                  (settled ((:type "agent_start") (:type "agent_end" :messages [])
                            (:type "agent_settled")) idle "idle")
                  (newer-ended ((:type "agent_start") (:type "agent_end" :messages [])
                                (:type "agent_settled") (:type "agent_start")
                                (:type "agent_end" :messages [])) sending "thinking")))
    (ert-info ((format "observed before acknowledgment=%s" (car case)))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let ((schedule (symbol-function 'run-at-time)) notices)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (seconds repeat function &rest args)
                       (if (eq function 'pilish--clear-sending-if-no-agent-start)
                           'fake-prompt-start-timer
                         (apply schedule seconds repeat function args))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) notices))))
            (with-current-buffer input (insert "ordinary prompt") (pilish-send))
            (let ((request (car commands)))
              (dolist (event (nth 1 case)) (pilish-test--stdout proc event))
              ;; Both the observed event and a later acknowledgment retain phase.
              (dotimes (after-acceptance 2)
                (when (= after-acceptance 1)
                  (pilish-test--stdout
                   proc (list :type "response" :command "prompt" :success t
                              :id (plist-get request :id) :data '(:disposition "started"))))
                (let ((header (pilish-test--input-header input)))
                  (should (string-match-p (nth 3 case) header))
                  (should-not (string-match-p "waiting for Pi" header)))
                (with-current-buffer chat
                  (should (eq pilish--status (nth 2 case)))
                  (should (equal pilish--activity-phase (nth 3 case)))))
              (should (equal notices (when (eq (car case) 'compaction)
                                       '("Pi: Compacting..."))))
              (should (equal (mapcar (lambda (command) (plist-get command :type)) commands)
                             '("prompt"))))))))))

(ert-deftest pilish-test-header-pending-acceptance-does-not-expire ()
  "Twelve silent seconds never time out an unacknowledged prompt or draft."
  (pilish-test-with-clock now
    (pilish-test-with-rpc-session (chat input proc commands)
      (let ((schedule (symbol-function 'run-at-time))
            (pilish-session-inactivity-timeout 10) fallbacks)
        (pilish-test--adopt-rpc-process chat proc)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (seconds repeat function &rest args)
                     (when (eq function 'pilish--clear-sending-if-no-agent-start)
                       (push (cons function args) fallbacks))
                     (apply schedule seconds repeat function args))))
          (with-current-buffer input (insert "slow prompt") (pilish-send))
          (let ((wait (buffer-local-value 'pilish--prompt-wait chat)))
            (with-current-buffer input (insert "unsent newer draft"))
            (setq now 1012.0)
            (let ((header (pilish-test--input-header input)))
              (should (string-match-p "waiting for Pi" header))
              (should-not (string-match-p "no output" header)))
            (with-current-buffer chat
              (should (eq pilish--prompt-wait wait))
              (should (eq pilish--status 'sending))
              (should (equal pilish--activity-phase "thinking"))
              (should-not (pilish--prompt-wait-accepted wait))
              (should-not pilish--prompt-start-timer))
            (should-not fallbacks)
            (should (= 1 (hash-table-count (pilish--get-pending-requests proc))))
            (should (equal (mapcar (lambda (command) (plist-get command :type)) commands)
                           '("prompt")))
            (with-current-buffer input
              (should (equal (buffer-string) "unsent newer draft")))))))))

(ert-deftest pilish-test-header-pending-acceptance-inactivity-precedence ()
  "A silent observed run keeps its existing warning despite an unacked prompt."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (with-current-buffer input (insert "ordinary prompt") (pilish-send))
    (pilish-test--stdout proc '(:type "agent_start"))
    (setq now 1300.0)
    (let* ((header (with-current-buffer input (pilish--header-line-string)))
           (start (string-match (regexp-quote "thinking (no output 5m)") header)))
      (should start)
      (should (eq (get-text-property start 'face header) 'warning))
      (should (string-match-p "Pi may still be working"
                              (get-text-property start 'help-echo header)))
      (should-not (string-match-p "waiting for Pi" header)))
    (with-current-buffer chat
      (should (eq pilish--status 'streaming))
      (should (equal pilish--activity-phase "thinking"))
      (should-not (pilish--prompt-wait-accepted pilish--prompt-wait)))
    (should (equal (mapcar (lambda (command) (plist-get command :type)) commands)
                   '("prompt")))))

(ert-deftest pilish-test-kill-ring-save-strips-by-default ()
  "kill-ring-save strips hidden markup by default."
  (pilish-test--with-chat-markup "Hello **bold** world"
    (kill-ring-save (point-min) (point-max))
    (should (equal (car kill-ring) "Hello bold world"))))

(ert-deftest pilish-test-kill-ring-save-keeps-raw-when-enabled ()
  "When copy-raw-markdown is t, kill-ring-save keeps raw markdown."
  (pilish-test--with-chat-markup "Hello **bold** world"
    (let ((pilish-copy-raw-markdown t))
      (kill-ring-save (point-min) (point-max))
      (should (equal (car kill-ring) "Hello **bold** world")))))

;;; Chat Navigation Behavior

(ert-deftest pilish-test-next-message-from-top ()
  "n from point-min reaches first You heading."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (should (looking-at "You · 10:00"))))

(ert-deftest pilish-test-next-message-successive ()
  "Successive n reaches each You heading in order."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (should (looking-at "You · 10:00"))
    (pilish-next-message)
    (should (looking-at "You · 10:05"))
    (pilish-next-message)
    (should (looking-at "You · 10:10"))))

(ert-deftest pilish-test-next-message-recognizes-full-date-heading ()
  "Message navigation recognizes full-date You headings."
  (with-temp-buffer
    (insert "Intro\n\nYou · 2026-06-13 10:05\n========================\nQuestion\n")
    (goto-char (point-min))
    (pilish-next-message)
    (should (looking-at "You · 2026-06-13 10:05"))))

(ert-deftest pilish-test-next-message-at-last ()
  "n at last You heading keeps point and shows message."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (pilish-next-message)
    (pilish-next-message)
    (should (looking-at "You · 10:10"))
    (let ((pos (point))
          (shown-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-next-message))
      ;; Point stays on the last heading
      (should (= (point) pos))
      (should (equal shown-message "No more messages")))))

(ert-deftest pilish-test-previous-message-from-last ()
  "p from last You heading reaches previous."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    ;; Navigate to last heading first
    (pilish-next-message)
    (pilish-next-message)
    (pilish-next-message)
    (should (looking-at "You · 10:10"))
    (pilish-previous-message)
    (should (looking-at "You · 10:05"))))

(ert-deftest pilish-test-previous-message-at-first ()
  "p at first You heading keeps point and shows message."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (should (looking-at "You · 10:00"))
    (let ((pos (point))
          (shown-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pilish-previous-message))
      ;; Point stays on the first heading
      (should (= (point) pos))
      (should (equal shown-message "No previous message")))))

(ert-deftest pilish-test-other-window-scroll-buffer-set-locally ()
  "Session setup stores `other-window-scroll-buffer' as input-local state."
  (let ((root "/tmp/pilish-test-scroll-other/")
        (original-default (default-value 'other-window-scroll-buffer)))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil))
              ((symbol-function 'pilish--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (let ((default-directory root))
              (pilish))
            (let ((chat (get-buffer (pilish-test--chat-buffer-name root)))
                  (input (get-buffer (pilish-test--input-buffer-name root))))
              (with-current-buffer input
                (should (local-variable-p 'other-window-scroll-buffer))
                (should (eq other-window-scroll-buffer chat)))
              (should (eq (default-value 'other-window-scroll-buffer)
                          original-default))))
        (set-default 'other-window-scroll-buffer original-default)
        (pilish-test--kill-session-buffers root)))))

(ert-deftest pilish-test-other-window-for-scrolling-tracks-each-input-session ()
  "Each input buffer scrolls its own chat buffer."
  (let ((root-a "/tmp/pilish-test-scroll-a/")
        (root-b "/tmp/pilish-test-scroll-b/")
        (original-default (default-value 'other-window-scroll-buffer)))
    (make-directory root-a t)
    (make-directory root-b t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pilish--start-process) (lambda (_) nil))
              ((symbol-function 'pilish--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (let ((default-directory root-a))
              (pilish))
            (let ((default-directory root-b))
              (pilish))
            (let* ((chat-a (get-buffer (pilish-test--chat-buffer-name root-a)))
                   (input-a (get-buffer (pilish-test--input-buffer-name root-a)))
                   (chat-b (get-buffer (pilish-test--chat-buffer-name root-b)))
                   (input-b (get-buffer (pilish-test--input-buffer-name root-b))))
              (delete-other-windows)
              (switch-to-buffer chat-a)
              (let* ((chat-win-a (selected-window))
                     (input-win-a (split-window chat-win-a -10 'below))
                     (chat-win-b (split-window chat-win-a nil 'right))
                     input-win-b)
                (set-window-buffer input-win-a input-a)
                (set-window-buffer chat-win-b chat-b)
                (setq input-win-b (split-window chat-win-b -10 'below))
                (set-window-buffer input-win-b input-b)
                (select-window input-win-a)
                (should (eq (window-buffer (other-window-for-scrolling)) chat-a))
                (select-window input-win-b)
                (should (eq (window-buffer (other-window-for-scrolling)) chat-b)))))
        (set-default 'other-window-scroll-buffer original-default)
        (pilish-test--kill-session-buffers root-a)
        (pilish-test--kill-session-buffers root-b)))))

;;; Turn Detection

(ert-deftest pilish-test-turn-index-on-first-heading ()
  "Turn index is 0 when point is on first You heading."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (should (= (pilish--user-turn-index-at-point) 0))))

(ert-deftest pilish-test-turn-index-in-first-body ()
  "Turn index is 0 when point is in first user message body."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (forward-line 2) ; skip heading + underline into body
    (should (= (pilish--user-turn-index-at-point) 0))))

(ert-deftest pilish-test-turn-index-on-underline ()
  "Turn index is 0 when point is on === underline of first You."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (forward-line 1) ; on ===
    (should (= (pilish--user-turn-index-at-point) 0))))

(ert-deftest pilish-test-turn-index-on-second-heading ()
  "Turn index is 1 on second You heading."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (pilish-next-message)
    (should (= (pilish--user-turn-index-at-point) 1))))

(ert-deftest pilish-test-turn-index-on-assistant-heading ()
  "Turn index is index of preceding You when point is on Assistant heading."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    ;; Navigate to first You, then move into assistant section
    (pilish-next-message)
    (forward-line 4) ; past heading + underline + body + blank → "Assistant"
    (should (looking-at "Assistant"))
    (should (= (pilish--user-turn-index-at-point) 0))))

(ert-deftest pilish-test-turn-index-in-assistant-body ()
  "Turn index is index of preceding You when point is in assistant response."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (pilish-next-message)
    (forward-line 6) ; heading + underline + body + blank + Assistant + underline → response
    (should (looking-at "First answer"))
    (should (= (pilish--user-turn-index-at-point) 0))))

(ert-deftest pilish-test-turn-index-before-first-you ()
  "Turn index is nil before first You heading."
  (with-temp-buffer
    (pilish-test--insert-chat-turns)
    (goto-char (point-min))
    (should-not (pilish--user-turn-index-at-point))))

(ert-deftest pilish-test-turn-index-empty-buffer ()
  "Turn index is nil in empty buffer."
  (with-temp-buffer
    (should-not (pilish--user-turn-index-at-point))))

(ert-deftest pilish-test-turn-index-no-false-match ()
  "Turn index ignores text starting with You without setext underline."
  (with-temp-buffer
    (insert "You mentioned something\nRegular text\n\n"
            "You · 10:00\n===========\nFirst question\n")
    (goto-char (point-min))
    ;; Point is on "You mentioned" which has no === underline
    (should-not (pilish--user-turn-index-at-point))
    ;; Move to the real heading
    (goto-char (point-max))
    (should (= (pilish--user-turn-index-at-point) 0))))

;;; You Heading Detection

(ert-deftest pilish-test-heading-re-matches-plain-you ()
  "Heading regex matches bare `You' at start of line."
  (should (string-match-p pilish--you-heading-re "You")))

(ert-deftest pilish-test-heading-re-matches-you-with-timestamp ()
  "Heading regex matches `You · 22:10' at start of line."
  (should (string-match-p pilish--you-heading-re "You · 22:10")))

(ert-deftest pilish-test-heading-re-rejects-you-colon ()
  "Heading regex does not match `You:' (old broken pattern)."
  (should-not (string-match-p pilish--you-heading-re "You: hello")))

(ert-deftest pilish-test-heading-re-rejects-mid-line ()
  "Heading regex does not match `You' mid-line."
  (should-not (string-match-p pilish--you-heading-re "  You · 22:10")))

(ert-deftest pilish-test-heading-re-rejects-you-prefix ()
  "Heading regex does not match words starting with You like `Your'."
  (should-not (string-match-p pilish--you-heading-re "Your code is fine")))

(ert-deftest pilish-test-at-you-heading-p-true ()
  "Predicate returns t when on a valid You setext heading."
  (with-temp-buffer
    (insert "You · 22:10\n===========\n")
    (goto-char (point-min))
    (should (pilish--at-you-heading-p))))

(ert-deftest pilish-test-at-you-heading-p-no-underline ()
  "Predicate returns nil when You line lacks setext underline."
  (with-temp-buffer
    (insert "You · 22:10\nSome text\n")
    (goto-char (point-min))
    (should-not (pilish--at-you-heading-p))))

(ert-deftest pilish-test-at-you-heading-p-short-underline ()
  "Predicate returns t with minimum 3-char underline."
  (with-temp-buffer
    (insert "You\n===\n")
    (goto-char (point-min))
    (should (pilish--at-you-heading-p))))

(ert-deftest pilish-test-at-you-heading-p-wrong-line ()
  "Predicate returns nil when not on the heading line."
  (with-temp-buffer
    (insert "You · 22:10\n===========\nBody text\n")
    (goto-char (point-max))
    (forward-line -1)  ; on "Body text"
    (should-not (pilish--at-you-heading-p))))

;;; Hot Tail

(ert-deftest pilish-test-hot-tail-boundary-keeps-buffer-hot-when-few-turns ()
  "Buffers with at most N headed turns stay entirely hot."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "You · 10:00\n===========\nFirst question\n\n"
              "Assistant\n=========\nFirst answer\n\n"
              "You · 10:05\n===========\nSecond question\n"))
    (let ((pilish-hot-tail-turn-count 3))
      (pilish--update-hot-tail-boundary)
      (should (= (marker-position pilish--hot-tail-start)
                 (point-min))))))

(ert-deftest pilish-test-hot-tail-boundary-moves-to-nth-newest-heading ()
  "Hot tail starts at the Nth newest headed turn boundary."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "You · 10:00\n===========\nFirst question\n\n"
              "Assistant\n=========\nFirst answer\n\n"
              "You · 10:05\n===========\nSecond question\n\n"
              "Assistant\n=========\nSecond answer\n\n"
              "You · 10:10\n===========\nThird question\n"))
    (let ((pilish-hot-tail-turn-count 3))
      (pilish--update-hot-tail-boundary)
      (goto-char (marker-position pilish--hot-tail-start))
      (should (looking-at "You · 10:05")))))

(ert-deftest pilish-test-in-hot-tail-p-respects-boundary ()
  "Positions before the hot-tail marker are cold; marker and later are hot."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((inhibit-read-only t))
      (insert "You · 10:00\n===========\nFirst question\n\n"
              "Assistant\n=========\nFirst answer\n\n"
              "You · 10:05\n===========\nSecond question\n\n"
              "Assistant\n=========\nSecond answer\n\n"
              "You · 10:10\n===========\nThird question\n"))
    (let ((pilish-hot-tail-turn-count 3))
      (pilish--update-hot-tail-boundary)
      (should-not (pilish--in-hot-tail-p (point-min)))
      (should (pilish--in-hot-tail-p
               (marker-position pilish--hot-tail-start))))))

;;; Executable Customization

(ert-deftest pilish-test-check-pi-uses-executable ()
  "check-pi uses car of `pilish-executable' for lookup."
  (let ((pilish-executable '("npx" "pi"))
        looked-up-command
        remote-flag)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd &optional remote)
                 (setq looked-up-command cmd
                       remote-flag remote)
                 (when (equal cmd "npx")
                   "/usr/bin/npx"))))
      (should (pilish--check-pi))
      (should (equal looked-up-command "npx"))
      (should (eq remote-flag t)))))

(ert-deftest pilish-test-check-pi-returns-nil-when-missing ()
  "check-pi returns nil when executable is not found."
  (let ((pilish-executable '("nonexistent-binary")))
    (cl-letf (((symbol-function 'executable-find) (lambda (_cmd &optional _remote) nil)))
      (should-not (pilish--check-pi)))))

(ert-deftest pilish-test-check-pi-uses-remote-executable-find ()
  "Remote sessions should look for pi on the remote host."
  (let ((pilish-executable '("pi"))
        (default-directory "/ssh:pi-host:/home/pi/project/")
        (calls nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd &optional remote)
                 (push (list cmd remote default-directory) calls)
                 (and remote "/ssh:pi-host:/usr/bin/pi"))))
      (should (pilish--check-pi))
      (should (member '("pi" t "/ssh:pi-host:/home/pi/project/") calls))
      (should-not (cl-find-if (lambda (call)
                                (and (equal (car call) "pi")
                                     (null (cadr call))))
                              calls)))))

(ert-deftest pilish-test-check-pi-multi-hop-uses-full-prefix-candidates ()
  "Multi-hop remote dependency lookup builds candidates with the full route."
  (let ((pilish-executable '("pi"))
        (exec-path '("/usr/local/bin" "/usr/bin" nil))
        (exec-suffixes '(""))
        (default-directory "/ssh:bastion|sudo:root@pi-host:/srv/project/")
        (checked nil))
    (cl-letf (((symbol-function 'exec-path)
               (lambda () exec-path))
              ((symbol-function 'executable-find)
               (lambda (&rest _)
                 (ert-fail "multi-hop lookup should not call executable-find")))
              ((symbol-function 'file-executable-p)
               (lambda (path)
                 (push path checked)
                 (equal path
                        "/ssh:bastion|sudo:root@pi-host:/usr/bin/pi"))))
      (should (pilish--check-pi))
      (should (member "/ssh:bastion|sudo:root@pi-host:/usr/local/bin/pi"
                      checked))
      (should (member "/ssh:bastion|sudo:root@pi-host:/usr/bin/pi"
                      checked))
      (should-not (cl-some (lambda (path)
                             (string-prefix-p "/sudo:root@pi-host:" path))
                           checked)))))

(ert-deftest pilish-test-check-pi-multi-hop-uses-remote-exec-path ()
  "Multi-hop dependency lookup asks TRAMP for the remote PATH entries."
  (let ((pilish-executable '("pi"))
        (exec-path '("/local-only/bin"))
        (exec-suffixes '(""))
        (remote-dir "/ssh:bastion|sudo:root@pi-host:/srv/project/")
        (checked nil))
    (cl-letf (((symbol-function 'exec-path)
               (lambda ()
                 (should (equal default-directory remote-dir))
                 '("/opt/remote/bin")))
              ((symbol-function 'executable-find)
               (lambda (&rest _)
                 (ert-fail "multi-hop lookup should not call executable-find")))
              ((symbol-function 'file-executable-p)
               (lambda (path)
                 (push path checked)
                 (equal path
                        "/ssh:bastion|sudo:root@pi-host:/opt/remote/bin/pi"))))
      (let ((default-directory remote-dir))
        (should (pilish--check-pi)))
      (should (equal checked
                     '("/ssh:bastion|sudo:root@pi-host:/opt/remote/bin/pi"))))))

(ert-deftest pilish-test-check-dependencies-no-local-warning-for-remote-pi ()
  "Remote dependency checks should not warn just because local PATH lacks pi."
  (let ((pilish-executable '("pi"))
        (default-directory "/ssh:pi-host:/home/pi/project/")
        (warning-text nil)
        (remote-lookup nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd &optional remote)
                 (setq remote-lookup remote)
                 (and remote "/ssh:pi-host:/usr/bin/pi")))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _)
                 (setq warning-text msg)))
              ((symbol-function 'pilish--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'pilish--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'pilish--maybe-install-optional-grammars)
               #'ignore))
      (pilish--check-dependencies)
      (should remote-lookup)
      (should-not warning-text))))

(ert-deftest pilish-test-check-dependencies-warning-names-remote-path ()
  "Remote missing-pi warnings explain that the remote PATH was checked."
  (let ((pilish-executable '("pi"))
        (warning-text nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd &optional _remote) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'pilish--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'pilish--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'pilish--maybe-install-optional-grammars)
               #'ignore))
      (pilish--check-dependencies "/ssh:pi-host:/home/pi/project/")
      (should (string-match-p "remote PATH (/ssh:pi-host:)" warning-text))
      (should (string-match-p "npm install -g @earendil-works/pi-coding-agent" warning-text))
      (should-not (string-match-p "@earendil-works/pi-coding-agent@" warning-text)))))

(ert-deftest pilish-test-check-dependencies-warning-names-multi-hop-remote-path ()
  "Remote missing-pi warnings preserve the full TRAMP route."
  (let ((pilish-executable '("pi"))
        (exec-path '("/usr/bin"))
        (warning-text nil))
    (cl-letf (((symbol-function 'exec-path)
               (lambda () exec-path))
              ((symbol-function 'pilish--remote-executable-file-p)
               (lambda (_path) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'pilish--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'pilish--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'pilish--maybe-install-optional-grammars)
               #'ignore))
      (pilish--check-dependencies
       "/ssh:bastion|sudo:root@pi-host:/srv/project/")
      (should (string-match-p
               (regexp-quote "remote PATH (/ssh:bastion|sudo:root@pi-host:)")
               warning-text)))))

(ert-deftest pilish-test-check-dependencies-uses-explicit-directory ()
  "An explicit dependency directory overrides the caller buffer context."
  (let ((checked-directory nil))
    (cl-letf (((symbol-function 'pilish--check-pi)
               (lambda (&optional directory)
                 (setq checked-directory directory)
                 t))
              ((symbol-function 'pilish--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'pilish--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'pilish--maybe-install-optional-grammars)
               #'ignore))
      (pilish--check-dependencies "/ssh:pi-host:/home/pi/project/")
      (should (equal checked-directory "/ssh:pi-host:/home/pi/project/")))))

(ert-deftest pilish-test-executable-default-value ()
  "Default value of pilish-executable is (\"pi\")."
  (should (equal (default-value 'pilish-executable) '("pi"))))

(ert-deftest pilish-test-check-dependencies-names-executable ()
  "Warning message includes the actual executable name."
  (let ((pilish-executable '("my-custom-pi"))
        (warning-text nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_cmd &optional _remote) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg))))
      (pilish--check-dependencies)
      (should (string-match-p "my-custom-pi" warning-text))
      (should (string-match-p
               "npm install -g @earendil-works/pi-coding-agent"
               warning-text))
      (should-not (string-match-p
                   "npm install -g @earendil-works/pi-coding-agent@"
                   warning-text)))))

;;; Essential Grammar Install Prompt (markdown + markdown-inline)

(ert-deftest pilish-test-essential-grammars-ignore-optional-gaps ()
  "Only Markdown grammars should count as essential for chat rendering."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(markdown markdown-inline)))))
    (should-not (pilish--missing-essential-grammars))))

(ert-deftest pilish-test-missing-essential-grammars-detected ()
  "Detect when markdown or markdown-inline grammars are missing."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (not (memq lang '(markdown markdown-inline))))))
    (should (equal '(markdown markdown-inline)
                   (pilish--missing-essential-grammars)))))

(ert-deftest pilish-test-no-missing-essential-grammars ()
  "Return nil when both essential grammars are installed."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) t)))
    (should-not (pilish--missing-essential-grammars))))

(ert-deftest pilish-test-essential-grammars-auto-install ()
  "Auto-install essential grammars without prompting when action is `auto'."
  (let ((installed-langs nil)
        (noninteractive nil)
        (pilish-essential-grammar-action 'auto))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (push lang installed-langs)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-essential-grammars)
      (should (memq 'markdown installed-langs))
      (should (memq 'markdown-inline installed-langs)))))

(ert-deftest pilish-test-essential-grammars-prompt-accept ()
  "Install essential grammars when action is `prompt' and user accepts."
  (let ((installed-langs nil)
        (noninteractive nil)
        (pilish-essential-grammar-action 'prompt))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (push lang installed-langs)))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-essential-grammars)
      (should (memq 'markdown installed-langs))
      (should (memq 'markdown-inline installed-langs)))))

(ert-deftest pilish-test-essential-grammars-prompt-decline ()
  "Warn without installing when action is `prompt' and user declines."
  (let ((installed nil)
        (warning-message nil)
        (noninteractive nil)
        (pilish-essential-grammar-action 'prompt))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (setq installed t)))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-message msg)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-essential-grammars)
      (should-not installed)
      (should (stringp warning-message))
      (should (string-match-p "not installed" warning-message)))))

(ert-deftest pilish-test-essential-grammars-warn-only ()
  "Only warn when action is `warn' — never attempt installation."
  (let ((installed nil)
        (warning-message nil)
        (noninteractive nil)
        (pilish-essential-grammar-action 'warn))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (setq installed t)))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-message msg)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-essential-grammars)
      (should-not installed)
      (should (stringp warning-message))
      (should (string-match-p "not installed" warning-message)))))

(ert-deftest pilish-test-essential-grammars-error-without-cc ()
  "Show clear error when C compiler is not available."
  (let ((noninteractive nil)
        (error-message nil)
        (pilish-essential-grammar-action 'auto))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (error "Cannot find suitable compiler")))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq error-message msg)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-essential-grammars)
      (should (stringp error-message))
      (should (string-match-p "C compiler" error-message)))))

(ert-deftest pilish-test-essential-grammars-no-install-in-batch ()
  "Never install essential grammars in batch mode."
  (let ((noninteractive t)
        (installed nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (setq installed t))))
      (pilish--maybe-install-essential-grammars)
      (should-not installed))))

;;; Markdown Grammar Compatibility

(ert-deftest pilish-test-incompatible-markdown-grammar-detected ()
  "Detect an installed Markdown grammar that lacks required table nodes."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) t))
            ((symbol-function 'pilish--markdown-grammar-compatible-p)
             (lambda () nil)))
    (should (pilish--markdown-grammar-incompatible-p))))

(ert-deftest pilish-test-missing-markdown-grammar-not-incompatible ()
  "Missing Markdown grammar is handled by the missing-essential path."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) nil))
            ((symbol-function 'pilish--markdown-grammar-compatible-p)
             (lambda () nil)))
    (should-not (pilish--markdown-grammar-incompatible-p))))

(ert-deftest pilish-test-incompatible-markdown-grammar-warns-once ()
  "Warn once when the loaded Markdown grammar is incompatible."
  (let ((noninteractive nil)
        (pilish--markdown-grammar-warning-done nil)
        (warnings nil))
    (cl-letf (((symbol-function 'pilish--markdown-grammar-incompatible-p)
               (lambda () t))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (push msg warnings))))
      (pilish--maybe-warn-incompatible-markdown-grammar)
      (pilish--maybe-warn-incompatible-markdown-grammar)
      (should (= 1 (length warnings)))
      (should (string-match-p "Incompatible Markdown" (car warnings)))
      (should (string-match-p "treesit-extra-load-path" (car warnings))))))

;;; Grammar Recipe Validation

(ert-deftest pilish-test-grammar-recipes-all-registered ()
  "All grammar recipes are registered in `treesit-language-source-alist'.
Catches accidentally dropped or malformed entries."
  (dolist (recipe pilish-grammar-recipes)
    (let ((lang (car recipe)))
      (should (assq lang treesit-language-source-alist)))))

(ert-deftest pilish-test-grammar-recipes-have-required-fields ()
  "Every recipe has LANG, URL, and REVISION.  SOURCE-DIR is optional."
  (dolist (recipe pilish-grammar-recipes)
    (should (symbolp (nth 0 recipe)))      ; LANG
    (should (stringp (nth 1 recipe)))      ; URL
    (should (string-prefix-p "https://" (nth 1 recipe)))
    (should (stringp (nth 2 recipe)))))    ; REVISION

(ert-deftest pilish-test-grammar-recipes-source-dir-entries ()
  "Recipes needing SOURCE-DIR have it set (monorepos with subdirectories)."
  (let ((ts-recipe (assq 'typescript treesit-language-source-alist))
        (tsx-recipe (assq 'tsx treesit-language-source-alist))
        (php-recipe (assq 'php treesit-language-source-alist)))
    ;; These share repos with other parsers — SOURCE-DIR is required
    (should (equal (nth 3 ts-recipe) "typescript/src"))
    (should (equal (nth 3 tsx-recipe) "tsx/src"))
    (should (equal (nth 3 php-recipe) "php/src"))))

;;; Optional Grammar Install Prompt (embedded languages)

(ert-deftest pilish-test-missing-optional-grammars-detected ()
  "Detect missing optional grammars from recipe list."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(python bash)))))
    (let ((missing (pilish--missing-optional-grammars)))
      ;; python and bash are installed, rest should be missing
      (should-not (memq 'python missing))
      (should-not (memq 'bash missing))
      (should-not (memq 'markdown missing))
      (should-not (memq 'markdown-inline missing))
      (should (memq 'javascript missing))
      (should (memq 'rust missing)))))

(ert-deftest pilish-test-optional-grammars-offer-install ()
  "Offer to install optional grammars when missing."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (installed-langs nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline python))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (push lang installed-langs)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-optional-grammars)
      ;; Should have installed some grammars (not python, already present)
      (should installed-langs)
      (should-not (memq 'python installed-langs))
      (should (memq 'javascript installed-langs)))))

(ert-deftest pilish-test-optional-grammars-decline-persists ()
  "Declining optional grammars saves the missing set via customize."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (saved-var nil)
        (saved-val nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (var val)
                 (setq saved-var var saved-val val)
                 (set var val)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-optional-grammars)
      (should (eq saved-var 'pilish-grammar-declined-set))
      ;; Saved the full set of missing grammars
      (should (memq 'javascript saved-val))
      (should (memq 'rust saved-val)))))

(ert-deftest pilish-test-optional-grammars-no-repeat-in-session ()
  "No re-prompt after already prompted this session."
  (let ((pilish--grammar-prompt-done t)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (pilish--maybe-install-optional-grammars)
      (should-not prompted))))

(ert-deftest pilish-test-optional-grammars-no-prompt-when-all-installed ()
  "No prompt when all optional grammars are already installed."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (_lang &rest _) t))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (pilish--maybe-install-optional-grammars)
      (should-not prompted))))

(ert-deftest pilish-test-optional-grammars-no-prompt-in-batch ()
  "Never prompt for optional grammars in batch mode."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive t)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (pilish--maybe-install-optional-grammars)
      (should-not prompted))))

(ert-deftest pilish-test-optional-grammars-cc-failure-reports ()
  "Report failure with actionable error when compiler is missing."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (install-attempts 0)
        (warning-text nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (cl-incf install-attempts)
                 (error "Cannot find suitable compiler")))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-optional-grammars)
      (should (= install-attempts 1))
      (should (stringp warning-text))
      (should (string-match-p "C compiler" warning-text)))))

(ert-deftest pilish-test-optional-grammars-prompt-mentions-command ()
  "The prompt mentions M-x pilish-install-grammars."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (prompt-text nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline python))))
              ((symbol-function 'y-or-n-p)
               (lambda (prompt) (setq prompt-text prompt) nil))
              ((symbol-function 'customize-save-variable) #'ignore)
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-optional-grammars)
      (should (stringp prompt-text))
      (should (string-match-p "pilish-install-grammars" prompt-text)))))

;;; Stickiness: Decline persists, new grammars re-prompt

(ert-deftest pilish-test-optional-grammars-decline-suppresses-permanently ()
  "After declining, same missing set on next startup does NOT re-prompt."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (prompt-count 0))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt)
                 (cl-incf prompt-count)
                 nil))
              ((symbol-function 'customize-save-variable)
               (lambda (var val) (set var val)))
              ((symbol-function 'message) #'ignore))
      ;; First session: user declines
      (pilish--maybe-install-optional-grammars)
      (should (= prompt-count 1))
      (should pilish-grammar-declined-set)
      ;; Simulate Emacs restart: reset session flag, keep persisted set
      (setq pilish--grammar-prompt-done nil)
      ;; Second session: same missing grammars — no prompt
      (pilish--maybe-install-optional-grammars)
      (should (= prompt-count 1)))))

(ert-deftest pilish-test-optional-grammars-new-grammar-reprompts ()
  "Adding a new grammar to recipes re-prompts even after a prior decline.
Simulates: user declined when javascript/rust were missing, then
a new grammar (e.g., `zig') appears in the missing set."
  (let ((pilish--grammar-prompt-done nil)
        ;; Prior decline covered javascript and rust only
        (pilish-grammar-declined-set '(javascript rust))
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 ;; javascript, rust, AND go are all missing
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (var val) (set var val)))
              ((symbol-function 'message) #'ignore))
      ;; `go' is missing but not in declined-set → re-prompt
      (pilish--maybe-install-optional-grammars)
      (should prompted))))

(ert-deftest pilish-test-optional-grammars-accept-does-not-persist ()
  "Accepting the install offer does not persist a declined set."
  (let ((pilish--grammar-prompt-done nil)
        (pilish-grammar-declined-set nil)
        (noninteractive nil)
        (customize-called nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) (setq customize-called t)))
              ((symbol-function 'message) #'ignore))
      (pilish--maybe-install-optional-grammars)
      (should-not customize-called)
      (should-not pilish-grammar-declined-set))))

;;; Install Helper: pilish--install-grammars

(ert-deftest pilish-test-install-grammars-returns-count ()
  "install-grammars returns number of successfully installed grammars."
  (cl-letf (((symbol-function 'treesit-install-language-grammar)
             (lambda (_lang &optional _out-dir) nil))
            ((symbol-function 'message) #'ignore))
    (should (= (pilish--install-grammars '(python rust go)) 3))))

(ert-deftest pilish-test-install-grammars-empty-list ()
  "install-grammars with empty list returns 0."
  (should (= (pilish--install-grammars '()) 0)))

(ert-deftest pilish-test-install-grammars-failure-returns-partial-count ()
  "install-grammars returns count of grammars installed before failure."
  (let ((warning-text nil))
    (cl-letf (((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (when (eq lang 'rust)
                   (error "cc: not found"))))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message) #'ignore))
      ;; python succeeds (idx=1), rust fails (idx=2, returned as 1)
      (should (= (pilish--install-grammars '(python rust go)) 1))
      (should (string-match-p "rust" warning-text))
      (should (string-match-p "1/3" warning-text)))))

(ert-deftest pilish-test-install-grammars-names-failing-grammar ()
  "install-grammars warning identifies which grammar failed."
  (let ((warning-text nil))
    (cl-letf (((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (when (eq lang 'go)
                   (error "compilation failed"))))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message) #'ignore))
      (pilish--install-grammars '(python rust go))
      (should (string-match-p "`go'" warning-text)))))

;;; Installed Optional Grammars

(ert-deftest pilish-test-installed-optional-grammars ()
  "installed-optional-grammars returns only grammars that are available."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(python rust)))))
    (let ((installed (pilish--installed-optional-grammars)))
      (should (memq 'python installed))
      (should (memq 'rust installed))
      (should-not (memq 'javascript installed)))))

;;; Interactive Command: M-x pilish-install-grammars

(ert-deftest pilish-test-install-grammars-command-all-installed ()
  "Interactive command shows message when all grammars are installed."
  (let ((msg nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (_lang &rest _) t))
              ((symbol-function 'pilish--markdown-grammar-compatible-p)
               (lambda () t))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (pilish-install-grammars)
      (should (string-match-p "installed" msg))
      (should (string-match-p "✓" msg)))))

(ert-deftest pilish-test-install-grammars-command-shows-status-buffer ()
  "Interactive command creates status buffer listing missing grammars."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(markdown markdown-inline python))))
            ((symbol-function 'pilish--markdown-grammar-compatible-p)
             (lambda () t))
            ((symbol-function 'pop-to-buffer)
             #'ignore))
    (unwind-protect
        (progn
          (pilish-install-grammars)
          (let ((buf (get-buffer "*pilish-grammars*")))
            (should buf)
            (with-current-buffer buf
              ;; Has missing grammars listed
              (should (string-match-p "Missing" (buffer-string)))
              (should (string-match-p "javascript" (buffer-string)))
              ;; Has installed grammars listed
              (should (string-match-p "Installed" (buffer-string)))
              (should (string-match-p "python" (buffer-string)))
              ;; Has keybinding hint
              (should (string-match-p "Press.*i.*to install" (buffer-string)))
              ;; Is in special-mode (read-only)
              (should (derived-mode-p 'special-mode)))))
      (when-let* ((buf (get-buffer "*pilish-grammars*")))
        (kill-buffer buf)))))

(ert-deftest pilish-test-install-grammars-command-shows-essential-missing ()
  "Interactive command highlights missing essential grammars prominently."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) nil))
            ((symbol-function 'pilish--markdown-grammar-compatible-p)
             (lambda () t))
            ((symbol-function 'pop-to-buffer)
             #'ignore))
    (unwind-protect
        (progn
          (pilish-install-grammars)
          (let ((buf (get-buffer "*pilish-grammars*")))
            (should buf)
            (with-current-buffer buf
              (should (string-match-p "ESSENTIAL" (buffer-string)))
              (should (string-match-p "markdown" (buffer-string))))))
      (when-let* ((buf (get-buffer "*pilish-grammars*")))
        (kill-buffer buf)))))

(ert-deftest pilish-test-install-grammars-command-warns-incompatible-markdown ()
  "Interactive command warns when an installed Markdown grammar is incompatible."
  (let ((warning-text nil)
        (msg nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (_lang &rest _) t))
              ((symbol-function 'pilish--markdown-grammar-compatible-p)
               (lambda () nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (pilish-install-grammars)
      (should (string-match-p "Incompatible Markdown" warning-text))
      (should-not msg))))

;;; CI Install Script Smoke Test

(ert-deftest pilish-test-ci-install-script-loads ()
  "The CI grammar install script loads without error.
Catches wiring bugs like requiring deleted modules."
  ;; Just load it — if the requires are broken, this errors.
  ;; We mock the install loop to avoid actually compiling grammars.
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) t))
            ((symbol-function 'message) #'ignore))
    ;; Tests run from the project root (Makefile sets load-path to ".")
    (load (expand-file-name "scripts/install-ts-grammars.el") nil t t)))

;;; check-dependencies

(ert-deftest pilish-test-check-dependencies-calls-grammar-checks ()
  "check-dependencies invokes grammar install and compatibility checks."
  (let ((essential-called nil)
        (compatibility-called nil)
        (optional-called nil))
    (cl-letf (((symbol-function 'pilish--check-pi) (lambda (&optional _directory) t))
              ((symbol-function 'pilish--maybe-install-essential-grammars)
               (lambda () (setq essential-called t)))
              ((symbol-function 'pilish--maybe-warn-incompatible-markdown-grammar)
               (lambda () (setq compatibility-called t)))
              ((symbol-function 'pilish--maybe-install-optional-grammars)
               (lambda () (setq optional-called t))))
      (pilish--check-dependencies)
      (should essential-called)
      (should compatibility-called)
      (should optional-called))))

;;; State response

(ert-deftest pilish-test-session-busy-includes-prompt-start-wait ()
  "A locally pending prompt keeps the session busy before Pi echoes events."
  (with-temp-buffer
    (pilish-chat-mode)
    (let ((generation (pilish--begin-prompt-start-wait)))
      (setq pilish--status 'idle)
      (should (pilish--prompt-start-current-p generation))
      (should (pilish--session-busy-p (current-buffer))))))

(ert-deftest pilish-test-session-busy-includes-session-transition ()
  "An in-flight session switch keeps the session busy."
  (with-temp-buffer
    (pilish-chat-mode)
    (setq pilish--status 'idle)
    (let ((generation (pilish--begin-session-transition 'mock-proc)))
      (should (pilish--session-transition-active-p (current-buffer)))
      (should (pilish--session-busy-p (current-buffer)))
      (pilish--finish-session-transition generation)
      (should-not (pilish--session-transition-active-p
                   (current-buffer))))))

(ert-deftest pilish-test-apply-state-response-normalizes-remote-session-file ()
  "Applying state anchors inbound sessionFile paths in the chat session dir."
  (let ((chat-buf (generate-new-buffer "*test-state-remote-session-file*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (pilish--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (pilish--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :isCompacting :false
                               :sessionId "remote-session"
                               :sessionFile "/home/pi/.pi/sessions/current.jsonl")))
          (should (equal (plist-get pilish--state :session-file)
                         "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl")))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-apply-state-response-ignores-nul-session-file ()
  "Applying state does not store unsafe sessionFile as a navigable path."
  (let ((chat-buf (generate-new-buffer "*test-state-nul-session-file*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (let ((bad (concat "/tmp/a" (string ?\0) "b.jsonl")))
            (pilish--apply-state-response
             chat-buf
             (list :success t
                   :data (list :isStreaming :false
                               :isCompacting :false
                               :sessionId "nul-session"
                               :sessionFile bad)))
            (should (equal (plist-get pilish--state :session-id)
                           "nul-session"))
            (should-not (plist-get pilish--state :session-file))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-rereview-snapshot-initializes-idle-ui ()
  "Without an event-owned busy phase, UI initialization adopts remote status."
  (dolist (snapshot '((:false :false idle) (t :false streaming)
                      (:false t compacting) (t t streaming)))
    (with-temp-buffer
      (pilish-chat-mode)
      (should (eq pilish--status 'idle))
      (pilish--apply-state-response
       (current-buffer)
       (list :success t :data (list :isStreaming (car snapshot)
                                   :isCompacting (cadr snapshot)
                                   :thinkingLevel "high")))
      (should (equal (plist-get pilish--state :thinking-level) "high"))
      (should (eq pilish--status (nth 2 snapshot)))
      (should (eq (plist-get pilish--state :status) (nth 2 snapshot))))))

(ert-deftest pilish-test-rereview-snapshot-preserves-post-run-ownership ()
  "Core/UI refreshes cannot invent agent_start during post-run work or compaction.
Pi's isStreaming includes post-run processing, not just the low-level loop."
  (dolist (path '(core ui))
    (dolist (compaction '(nil t))
      (ert-info ((format "%s refresh; post-run compaction %s" path compaction))
        (pilish-test-with-rpc-session (chat _input proc commands)
          (with-current-buffer chat
            (cl-letf (((symbol-function 'message) #'ignore))
              (pilish--handle-display-event '(:type "agent_start"))
              (pilish--handle-display-event '(:type "agent_end" :messages []))
              (when compaction
                (pilish--handle-display-event
                 '(:type "compaction_start" :reason "threshold")))
              (setq pilish--followup-queue '("next"))
              (if (eq path 'ui)
                  (pilish--refresh-thinking-level-state proc chat)
                (pilish--rpc-async
                 proc '(:type "get_state")
                 (lambda (response)
                   (with-current-buffer chat
                     (pilish--update-state-from-response response)))))
              (pilish--dispatch-response
               proc (list :type "response" :id (plist-get (car commands) :id)
                          :command "get_state" :success t
                          :data (list :isStreaming t
                                      :isCompacting (if compaction t :false)
                                      :thinkingLevel "high")))
              (should (equal (plist-get pilish--state :thinking-level) "high"))
              (should (eq pilish--status (if compaction 'compacting 'sending)))
              (should (eq (plist-get pilish--state :status) pilish--status))
              (when compaction
                (should (eq pilish--pre-compaction-status 'sending))
                (pilish--handle-display-event
                 '(:type "compaction_end" :reason "threshold"
                   :aborted :false :willRetry :false
                   :result (:summary "Done" :tokensBefore 1000))))
              (should (eq pilish--status 'sending))
              (should (= 1 (length commands)))
              (pilish--handle-display-event '(:type "agent_settled"))
              (should (= 2 (length commands)))
              (should (equal (plist-get (car commands) :type) "prompt"))
              (should (equal (plist-get (car commands) :message) "next")))))))))

(ert-deftest pilish-test-apply-state-response-keeps-local-prompt-start-busy ()
  "Stale idle get_state must not erase local prompt preflight state."
  (let ((chat-buf (generate-new-buffer "*test-state-local-prompt-start*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pilish-chat-mode)
          (let ((generation (pilish--begin-prompt-start-wait)))
            (setq pilish--status 'sending
                  pilish--state '(:session-id "same-session"))
            (pilish--apply-state-response
             chat-buf
             '(:success t :data (:isStreaming :false
                                 :isCompacting :false
                                 :sessionId "same-session"
                                 :sessionFile "/tmp/same.jsonl")))
            (should (pilish--prompt-start-current-p generation))
            (should (eq pilish--status 'sending))
            (should (eq (plist-get pilish--state :status) 'sending))
            (should (pilish--session-busy-p chat-buf))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-apply-state-response-preserves-extension-ui-warnings-without-session-change ()
  "Applying state keeps unsupported UI warnings within the same pi session."
  (let ((chat-buf (generate-new-buffer "*test-state-same-session*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--state nil
                  pilish--unsupported-extension-ui-methods-warned
                  '("setWidget")))
          (pilish--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :sessionId "new-session"
                               :sessionFile "/tmp/new.jsonl")))
          (with-current-buffer chat-buf
            (should (equal pilish--unsupported-extension-ui-methods-warned
                           '("setWidget"))))
          (with-current-buffer chat-buf
            (setq pilish--unsupported-extension-ui-methods-warned
                  '("setWidget")))
          (pilish--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :sessionId "new-session"
                               :sessionFile "/tmp/newer.jsonl")))
          (with-current-buffer chat-buf
            (should (equal pilish--unsupported-extension-ui-methods-warned
                           '("setWidget")))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-apply-state-response-resets-extension-ui-warnings-on-session-change ()
  "Applying state clears unsupported UI warnings when the pi session changes."
  (let ((chat-buf (generate-new-buffer "*test-state-session-change*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pilish-chat-mode)
            (setq pilish--state '(:session-id "old-session")
                  pilish--unsupported-extension-ui-methods-warned
                  '("setWidget")))
          (pilish--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :sessionId "new-session"
                               :sessionFile "/tmp/new.jsonl")))
          (with-current-buffer chat-buf
            (should (equal (plist-get pilish--state :session-id)
                           "new-session"))
            (should (null pilish--unsupported-extension-ui-methods-warned))))
      (kill-buffer chat-buf))))

;;; Input Window Height (integer and float ratio)

(ert-deftest pilish-test-input-height-integer-returns-configured-value ()
  "Integer setting returns that many lines when window is large enough."
  (let ((pilish-input-window-height 10)
        (window-min-height 4))
    (should (= (pilish--input-height-for-window-height 40) 10))))

(ert-deftest pilish-test-input-height-integer-clamps-to-max ()
  "Integer setting clamps when window is too small."
  (let ((pilish-input-window-height 10)
        (window-min-height 4))
    (should (= (pilish--input-height-for-window-height 12) 8))))

(ert-deftest pilish-test-input-height-float-computes-ratio ()
  "Float setting computes height as fraction of total."
  (let ((pilish-input-window-height 0.3)
        (window-min-height 4))
    (should (= (pilish--input-height-for-window-height 40) 12))))

(ert-deftest pilish-test-input-height-float-clamps-to-min ()
  "Float setting clamps up to window-min-height for tiny ratios."
  (let ((pilish-input-window-height 0.05)
        (window-min-height 4))
    (should (= (pilish--input-height-for-window-height 40) 4))))

(ert-deftest pilish-test-input-height-float-clamps-to-max ()
  "Float setting clamps down to preserve chat min-height."
  (let ((pilish-input-window-height 0.9)
        (window-min-height 4))
    (should (= (pilish--input-height-for-window-height 40) 36))))

(ert-deftest pilish-test-input-height-float-small-window ()
  "Float ratio on a small total still respects min heights."
  (let ((pilish-input-window-height 0.3)
        (window-min-height 4))
    (should (= (pilish--input-height-for-window-height 10) 4))))

;;; Dynamic ratio rebalancing

(defmacro pilish-test-with-split-layout (&rest body)
  "Execute BODY with a chat/input window pair.
Binds `chat-win' and `input-win' for use in BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (pilish-chat-mode)
     (let ((input-buf (generate-new-buffer " *test-input*")))
       (unwind-protect
           (progn
             (setq-local pilish--input-buffer input-buf)
             (delete-other-windows)
             (switch-to-buffer (current-buffer))
             (let* ((input-win (split-window nil -10 'below))
                    (chat-win (selected-window)))
               (set-window-buffer input-win input-buf)
               ,@body))
         (when (buffer-live-p input-buf)
           (kill-buffer input-buf))))))

(ert-deftest pilish-test-rebalance-adjusts-float-ratio ()
  "Rebalance resizes input window to match float ratio."
  (let ((pilish-input-window-height 0.3))
    (pilish-test-with-split-layout
      (pilish--rebalance-input-window chat-win input-win)
      (let* ((total (+ (window-total-height chat-win)
                       (window-total-height input-win)))
             (expected (pilish--input-height-for-window-height total)))
        (should (= (window-total-height input-win) expected))))))

(ert-deftest pilish-test-rebalance-skips-integer-height ()
  "Rebalance is a no-op when height is an integer."
  (let ((pilish-input-window-height 10))
    (pilish-test-with-split-layout
      (let ((before (window-total-height input-win)))
        (pilish--rebalance-input-window chat-win input-win)
        (should (= (window-total-height input-win) before))))))

(ert-deftest pilish-test-chat-mode-adds-size-change-hook ()
  "Chat mode installs the window-size-change rebalance hook."
  (with-temp-buffer
    (pilish-chat-mode)
    (should (memq #'pilish--maybe-rebalance-windows
                  window-size-change-functions))))

(ert-deftest pilish-test-repeating-timer-observation-uses-real-scheduling-and-cleanup ()
  "Observe both scheduling APIs once, retain cancelled allocations, and unwind."
  (let ((unrelated (run-at-time 3600 3600 #'ignore))
        first second once captured scheduled)
    (unwind-protect
        (progn
          (should-error
           (pilish-test-with-repeating-timer-allocations timers
             (setq first (run-at-time 3600 3600 #'ignore)
                   second (run-with-timer 3600 3600 #'ignore)
                   once (run-with-timer 3600 nil #'ignore)
                   scheduled (cl-every (lambda (timer) (memq timer timer-list))
                                       (list first second once)))
             (cancel-timer first)
             (setq captured timers)
             (ert-fail "Deliberate fixture failure"))
           :type 'ert-test-failed)
          (should scheduled)
          (should (equal captured (list second first)))
          (should-not (memq first timer-list))
          (should-not (memq second timer-list))
          (should (memq once timer-list))
          (should (memq unrelated timer-list)))
      (dolist (timer (list first second once unrelated))
        (when (timerp timer) (cancel-timer timer))))))

;;; Active-session stdout silence, in the existing input status only

(ert-deftest pilish-test-inactivity-header-threshold-phase-face-and-help ()
  "At the exact threshold only the existing status slot changes."
  (dolist (case '(((:type "agent_start") "thinking")
                  ((:type "tool_execution_start" :toolName "bash"
                          :toolCallId "quiet-tool" :args (:command "sleep 600"))
                   "running")
                  ((:type "compaction_start" :reason "manual") "compact")))
    (pilish-test-with-clock now
      (pilish-test-with-rpc-session (chat input proc commands)
        ;; Do not mask the real default with the adopted-session fixture.
        (should (= pilish-session-inactivity-timeout 300))
        (let (notices)
          (pilish-test--adopt-rpc-process chat proc)
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) notices))))
            (pilish-test--stdout proc '(:type "agent_start") (car case)))
          (with-current-buffer chat
            (setq pilish--session-name "Kept session"
                  pilish--followup-queue '("do not submit")
                  pilish--extension-status '(("extension" . "kept status"))))
          (let* ((phase (cadr case))
                 (normal (with-current-buffer input (pilish--header-line-string)))
                 (chat-text (with-current-buffer chat (buffer-string)))
                 (input-text (with-current-buffer input (buffer-string)))
                 (status (buffer-local-value 'pilish--status chat))
                 (state (copy-tree (buffer-local-value 'pilish--state chat)))
                 (window (selected-window)))
            (should (eq 'pilish-activity-phase
                        (get-text-property (string-match phase normal) 'face normal)))
            (setq notices nil commands nil now 1299.999)
            (should (equal-including-properties
                     normal (with-current-buffer input (pilish--header-line-string))))
            (setq now 1300.0)
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest args) (push args notices)))
                      ((symbol-function 'display-warning)
                       (lambda (&rest args) (push args notices))))
              (let* ((header (with-current-buffer input (pilish--header-line-string)))
                     (warning-text (concat phase " (no output 5m)"))
                     (start (string-match (regexp-quote warning-text) header)))
                (should start)
                (should (equal (substring-no-properties header)
                               (string-replace (format "%-8s" phase) warning-text
                                               (substring-no-properties normal))))
                (should (eq (get-text-property start 'face header) 'warning))
                (let ((help (get-text-property start 'help-echo header)))
                  (should (string-match-p "Pi may still be working" help))
                  (should (string-match-p "M-x pilish-abort" help))
                  (should (string-match-p "normally C-c C-k" help))
                  (should (string-match-p "discard queued continuations" help)))
                ;; Redisplay must be pure, not start another observer.
                (let ((scheduled (copy-sequence timer-list)))
                  (dotimes (_ 3)
                    (with-current-buffer input (pilish--header-line-string)))
                  (should (equal scheduled timer-list)))))
            (should-not notices)
            (should-not commands)
            (should (eq status (buffer-local-value 'pilish--status chat)))
            (should (equal state (buffer-local-value 'pilish--state chat)))
            (should (equal phase (buffer-local-value 'pilish--activity-phase chat)))
            (should (equal '("do not submit")
                           (buffer-local-value 'pilish--followup-queue chat)))
            (should-not (buffer-local-value 'pilish--aborted chat))
            (should-not (buffer-local-value 'header-line-format chat))
            (should (equal chat-text (with-current-buffer chat (buffer-string))))
            (should (equal input-text (with-current-buffer input (buffer-string))))
            (should (eq window (selected-window)))))))))

(ert-deftest pilish-test-inactivity-live-timeout-policy-retains-output-age ()
  "Disable, reenable and threshold edits use the same factual output age."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (setq pilish-session-inactivity-timeout 20)
    (pilish-test--stdout proc '(:type "agent_start"))
    (let ((normal (pilish-test--input-header input)))
      (setq now 1020.0)
      (pilish-test--assert-inactivity input "thinking (no output 20s)")
      (setq pilish-session-inactivity-timeout nil)
      (should (equal normal (pilish-test--input-header input)))
      (let ((timer (buffer-local-value 'pilish--inactivity-timer chat)))
        (pilish-test--fire-timer timer)
        (should (memq timer timer-list)))
      (setq now 1070.0 pilish-session-inactivity-timeout 20)
      (pilish-test--assert-inactivity input "thinking (no output 1m)")
      (setq pilish-session-inactivity-timeout 100)
      (should (equal normal (pilish-test--input-header input)))
      (setq pilish-session-inactivity-timeout 60)
      (pilish-test--assert-inactivity input "thinking (no output 1m)"))))

(ert-deftest pilish-test-inactivity-refresh-is-active-only-and-not-a-stats-poll ()
  "Initial and repeating real ticks refresh only input, without stats or work."
  (pilish-test-with-rpc-session (chat input proc commands)
    (let ((pilish-session-inactivity-timeout 0.02) observer refreshed notices)
      (unwind-protect
          (progn
            (pilish-test--adopt-rpc-process chat proc)
            (should-not (buffer-local-value 'pilish--inactivity-timer chat))
            (pilish-test--stdout proc '(:type "agent_start"))
            (setq observer (buffer-local-value 'pilish--inactivity-timer chat))
            (should (memq observer timer-list))
            (with-current-buffer chat (setq pilish--followup-queue '("wait")))
            (let ((last (process-get proc 'pilish-last-output-time))
                  (state (copy-tree (buffer-local-value 'pilish--state chat))))
              (cl-letf (((symbol-function 'force-mode-line-update)
                         (lambda (&optional all)
                           (push (list (current-buffer) all
                                       (when (eq (current-buffer) input)
                                         (pilish--header-line-string)))
                                 refreshed)))
                        ((symbol-function 'pilish--refresh-header)
                         (lambda () (ert-fail "Watchdog attempted stats refresh")))
                        ((symbol-function 'message)
                         (lambda (&rest args) (push args notices))))
                ;; No manual delivery or formatter reads in the wait predicate.
                (with-temp-buffer
                  (should (pilish-test-wait-until
                           (lambda () (>= (length refreshed) 2)) 10 0.05))))
              (dolist (sample refreshed)
                (ert-info ((format "autonomous refresh=%S" sample))
                  (should (eq input (car sample)))
                  (should-not (cadr sample))
                  (let* ((header (nth 2 sample))
                         (start (string-match "thinking (no output" header)))
                    (should start)
                    (should (eq 'warning (get-text-property start 'face header))))))
              (should (equal last (process-get proc 'pilish-last-output-time)))
              (should (equal state (buffer-local-value 'pilish--state chat))))
            (should-not notices)
            (should-not commands)
            (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
            (should (equal '("wait") (buffer-local-value 'pilish--followup-queue chat)))
            (pilish-test--stdout proc '(:type "agent_end" :messages []))
            (should-not (memq observer timer-list))
            (should-not (buffer-local-value 'pilish--inactivity-timer chat))
            (should (eq 'sending (buffer-local-value 'pilish--status chat)))
            (pilish-test--assert-inactivity input nil)
            ;; Do not drain the deliberately held followup in fixture teardown.
            (with-current-buffer chat (setq pilish--followup-queue nil))
            (pilish-test--stdout proc '(:type "agent_settled"))
            (should (eq 'idle (buffer-local-value 'pilish--status chat)))
            (should-not (buffer-local-value 'pilish--inactivity-timer chat)))
        (when observer (cancel-timer observer))))))

(ert-deftest pilish-test-inactivity-startup-state-response-arms-without-agent-event ()
  "An accepted get_state can initialize busy observation, not settle it."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (pilish-test-with-repeating-timer-allocations allocated
      (pilish--rpc-async proc '(:type "get_state")
                         (lambda (response)
                           (pilish--apply-state-response chat response)))
      (setq now 2000.0)
      (pilish-test--stdout
       proc `(:type "response" :id ,(plist-get (car commands) :id)
                    :success t :data (:isStreaming t :isCompacting :false)))
      (should (equal allocated
                     (list (buffer-local-value 'pilish--inactivity-timer chat))))
      (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
      (setq now 2300.0)
      (pilish-test--fire-timer (buffer-local-value 'pilish--inactivity-timer chat))
      (pilish-test--assert-inactivity input "no output 5m")
      (pilish--apply-state-response
       chat '(:success t :data (:isStreaming :false :isCompacting :false)))
      (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
      (should (memq (buffer-local-value 'pilish--inactivity-timer chat) timer-list))
      (should (equal allocated
                     (list (buffer-local-value 'pilish--inactivity-timer chat)))))))

(ert-deftest pilish-test-inactivity-compacting-state-response-preserves-json-literals ()
  "Framed false/null literals initialize compaction, not a phantom stream."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (pilish-test-with-repeating-timer-allocations allocated
      (let ((phase (buffer-local-value 'pilish--activity-phase chat)))
        (pilish--rpc-async proc '(:type "get_state")
                           (lambda (response)
                             (pilish--apply-state-response chat response)))
        (setq now 2000.0)
        (pilish-test--stdout
         proc `(:type "response" :id ,(plist-get (car commands) :id)
                      :success t :data (:isStreaming :false :isCompacting t
                                                     :sessionFile :null)))
        (should (eq 'compacting (buffer-local-value 'pilish--status chat)))
        (should-not (plist-get (buffer-local-value 'pilish--state chat) :session-file))
        (should (= 0 (hash-table-count (pilish--get-pending-requests proc))))
        ;; State snapshots preserve the existing phase, even when it is idle.
        (should (equal phase (buffer-local-value 'pilish--activity-phase chat)))
        (pilish-test--assert-inactivity input nil)
        (setq now 2300.0)
        (pilish-test--fire-timer (buffer-local-value 'pilish--inactivity-timer chat))
        (pilish-test--assert-inactivity input (concat phase " (no output 5m)"))
        (should (eq 'compacting (buffer-local-value 'pilish--status chat)))
        (should (= 1 (length commands)))
        (should (memq (buffer-local-value 'pilish--inactivity-timer chat) timer-list))
        (should (equal allocated
                       (list (buffer-local-value 'pilish--inactivity-timer chat))))))))

(ert-deftest pilish-test-inactivity-two-sessions-and-candidate-output-isolation ()
  "Traffic and candidate display routing cannot borrow another owner's timer."
  (pilish-test-with-inactivity-session (a ai ap ac now)
    (pilish-test-with-rpc-session (b bi bp bc)
      (pilish-test--adopt-rpc-process b bp)
      (pilish-test--stdout ap '(:type "agent_start"))
      (pilish-test--stdout bp '(:type "agent_start"))
      (setq now 1300.0)
      (pilish--process-filter bp " \n")
      (pilish-test--assert-inactivity ai "thinking (no output 5m)")
      (pilish-test--assert-inactivity bi nil)
      ;; Match the baseline reload candidate route.  Ordinary events still
      ;; render there; the observer must independently guard its source.
      (pilish-test--stdout ap '(:type "agent_end" :messages []))
      (let ((observer (buffer-local-value 'pilish--inactivity-timer b)))
        (process-put bp 'pilish-chat-buffer a)
        (pilish-test--stdout bp '(:type "agent_start"))
        (should-not (buffer-local-value 'pilish--inactivity-timer a))
        (should (eq observer (buffer-local-value 'pilish--inactivity-timer b)))
        (should (memq observer timer-list)))
      (process-put bp 'pilish-chat-buffer b))))

(ert-deftest pilish-test-inactivity-replacement-and-stale-callback-ownership ()
  "Replacement seeds a fresh clock; stale work may cancel only itself."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (let ((next (start-process "pilish-replacement" nil "cat")))
      (unwind-protect
          (progn
            (pilish-test--stdout proc '(:type "agent_start"))
            (let ((old (buffer-local-value 'pilish--inactivity-timer chat)) refreshed)
              (should (memq old timer-list))
              (pilish--process-filter next "candidate output before adoption\n")
              (setq now 1400.0)
              (with-current-buffer chat (pilish--set-process next))
              (process-put next 'pilish-chat-buffer chat)
              (pilish--register-display-handler next)
              (should-not (memq old timer-list))
              (pilish-test--assert-inactivity input nil)
              (let ((current (buffer-local-value 'pilish--inactivity-timer chat))
                    (state (copy-tree (buffer-local-value 'pilish--state chat))))
                (should-not (eq current old))
                ;; Backdate only after subr-mock setup, which may compile/yield.
                (cl-letf (((symbol-function 'force-mode-line-update)
                           (lambda (&rest _) (push (current-buffer) refreshed))))
                  (setq now 1300.0 refreshed nil)
                  (pilish-test--fire-timer old)
                  (should-not refreshed)
                  (setq now 1700.0))
                (should (memq current timer-list))
                (should (eq current (buffer-local-value 'pilish--inactivity-timer chat)))
                (should (equal 1400.0 (process-get next 'pilish-last-output-time)))
                (should (equal state (buffer-local-value 'pilish--state chat)))
                (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
                (pilish--process-filter proc "late old stdout\n")
                (pilish-test--assert-inactivity input "thinking (no output 5m)")
                ;; Idempotent same-process assignment is not adoption.
                (with-current-buffer chat (pilish--set-process next))
                (pilish-test--fire-timer current)
                (pilish-test--assert-inactivity input "thinking (no output 5m)")
                (should (memq current timer-list)))))
        (when (process-live-p next) (delete-process next))))))

(ert-deftest pilish-test-inactivity-exit-and-buffer-kill-cancel-owned-timer ()
  "Exit, chat kill and input kill cancel rather than just orphan callbacks."
  (dolist (how '(exit chat input))
    (ert-info ((format "teardown=%S" how))
      (pilish-test-with-inactivity-session (chat input proc commands now)
        (pilish-test--stdout proc '(:type "agent_start"))
        (let ((timer (buffer-local-value 'pilish--inactivity-timer chat)) refreshed)
          (should (memq timer timer-list))
          (pcase how
            ('exit
             (delete-process proc)
             (pilish--process-sentinel proc "finished\n")
             (should-not (buffer-local-value 'pilish--process chat))
             (should (eq 'idle (buffer-local-value 'pilish--status chat))))
            ('chat (pilish-test--kill-live-buffers chat))
            ('input (pilish-test--kill-live-buffers input)))
          (should-not (memq timer timer-list))
          (cl-letf (((symbol-function 'force-mode-line-update)
                     (lambda (&rest _) (push (current-buffer) refreshed))))
            (pilish-test--fire-timer timer))
          (should-not refreshed))))))

(ert-deftest pilish-test-inactivity-cancelled-kill-keeps-monitoring ()
  "A rejected kill decision must not cancel live observation."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (pilish-test--stdout proc '(:type "agent_start"))
    (let ((timer (buffer-local-value 'pilish--inactivity-timer chat)))
      (with-current-buffer chat
        (let ((kill-buffer-query-functions (list (lambda () nil))))
          (should-not (kill-buffer chat))))
      (should (buffer-live-p chat))
      (should (memq timer timer-list))
      (setq now 1300.0)
      (pilish-test--fire-timer timer)
      (pilish-test--assert-inactivity input "thinking (no output 5m)"))))

(ert-deftest pilish-test-inactivity-backward-clock-is-pure-until-owner-tick ()
  "Backward time hides warning; the owner tick rebases without settlement."
  (pilish-test-with-inactivity-session (chat input proc commands now)
    (pilish-test--stdout proc '(:type "agent_start"))
    (let ((timer (buffer-local-value 'pilish--inactivity-timer chat))
          (normal (pilish-test--input-header input)))
      (setq now 900.0)
      (should (equal normal (pilish-test--input-header input)))
      ;; A pure header read must not itself rewrite the observation clock.
      (should (equal (process-get proc 'pilish-last-output-time) 1000.0))
      (pilish-test--fire-timer timer)
      (setq now 1200.0)
      (pilish-test--assert-inactivity input "thinking (no output 5m)")
      (should (eq 'streaming (buffer-local-value 'pilish--status chat)))
      (should (memq timer timer-list)))))

(ert-deftest pilish-test-inactivity-lost-owner-cancels-captured-timer ()
  "A mode reset or dead chat retires its real timer without side effects."
  (dolist (kill-before-tick '(nil t))
    (ert-info ((format "kill-before-tick=%S" kill-before-tick))
      (pilish-test-with-rpc-session (chat input proc commands)
        (let (observer refreshed)
          (unwind-protect
              (progn
                (pilish-test--adopt-rpc-process chat proc)
                (pilish-test--stdout proc '(:type "agent_start"))
                (setq observer (buffer-local-value 'pilish--inactivity-timer chat))
                (should (memq observer timer-list))
                (let ((last (process-get proc 'pilish-last-output-time)))
                  (with-current-buffer chat (fundamental-mode))
                  (should-not (buffer-local-value 'pilish--inactivity-timer chat))
                  (when kill-before-tick (kill-buffer chat))
                  (cl-letf (((symbol-function 'force-mode-line-update)
                             (lambda (&rest _) (push (current-buffer) refreshed))))
                    (with-temp-buffer (pilish-test--fire-timer observer)))
                  (should-not (memq observer timer-list))
                  (should-not refreshed)
                  (should (equal last (process-get proc 'pilish-last-output-time)))
                  (when (buffer-live-p chat)
                    (should-not (buffer-local-value 'pilish--process chat))
                    (should-not (buffer-local-value 'pilish--state chat))
                    (should (eq 'idle (buffer-local-value 'pilish--status chat))))
                  (should-not commands)))
            (when observer (cancel-timer observer))))))))

(provide 'pilish-ui-test)
;;; pilish-ui-test.el ends here
