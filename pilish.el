;;; pilish.el --- Emacs frontend for pi coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; Assisted-by: pi:kimi-k3
;; Assisted-by: pi:glm-5.3
;; Assisted-by: pi:gpt-5.6
;; Assisted-by: pi:claude-opus-4.6
;; URL: https://github.com/dnouri/pilish
;; Keywords: ai llm ai-pair-programming tools
;; Version: 3.2.1
;; Package-Requires: ((emacs "29.1") (transient "0.9.0") (magit-section "4.0.0") (md-ts-mode "0.4.0") (markdown-table-wrap "0.2.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs frontend for the pi coding agent (https://pi.dev).
;; Provides a two-window interface for AI-assisted coding: chat history
;; with rendered markdown, and a separate prompt composition buffer.
;;
;; Requirements:
;;   - Emacs 29.1 or later (tree-sitter support required)
;;   - pi coding agent @earendil-works/pi-coding-agent 1.0.0 or later,
;;     installed and in PATH on the host where Pi runs (tested with 1.0.0)
;;   - tree-sitter grammars for markdown and markdown-inline
;;
;; Pilish uses `md-ts-mode` for its own chat and input buffers;
;; loading it does not change global Markdown file associations.
;;
;; Usage:
;;   M-x pilish                    Start or focus session in current project
;;   C-u M-x pilish                Start a named session
;;   M-x pilish-open-session-file  Open a JSONL session file as live session
;;   M-x pilish-toggle             Hide/show session windows in current frame
;;   M-x pilish-session-browser    Browse sessions (filter, switch)
;;   M-x pilish-tree-browser       Browse conversation tree (navigate, label)
;;
;; Many users define an alias: (defalias 'pi 'pilish)
;;
;; Key Bindings:
;;   Input buffer:
;;     C-c C-c        Send prompt (queues text as follow-up if busy)
;;     C-c C-s        Queue steering (interrupts after current tool; busy only)
;;     C-c C-k        Abort current operation
;;     C-c C-p        Open menu
;;     C-c C-r        Browse sessions
;;     M-p / M-n      History navigation
;;     C-r            Incremental history search (like readline)
;;     TAB            Path/file completion
;;     @              File reference (search project files)
;;
;;   Chat buffer:
;;     n / p          Navigate messages
;;     TAB            Toggle completed thinking/tool section or fold turn
;;     !              Run a Dired-inspired shell command on a strict file target
;;                    (command + dash-options appends it; otherwise use *)
;;     w              Copy the shell-local file path to the kill ring
;;     RET            Visit strict file target at point (tool content,
;;                    plain path, or local Markdown label)
;;     C-c C-k        Abort current operation
;;     C-c C-n        New session
;;     C-c C-r        Browse sessions
;;     C-c C-e        Export HTML
;;     C-c C-c        Compact context
;;     C-c C-m        Select model
;;     C-c C-t        Cycle thinking level
;;     C-c C-y        Copy last message
;;     C-c C-p        Open menu
;;
;; Editor Features:
;;   - File reference (@): Type @ to search project files (respects .gitignore)
;;   - Path completion (Tab): Complete relative paths, ../, ~/, etc.
;;   - Prompt image: Attach one content-sniffed raster image to a direct,
;;     idle, non-slash prompt; the input header shows its name and size.
;;   - Message queuing: Submit text messages while agent is working:
;;       C-c C-c  queues follow-up (delivered after agent completes)
;;       C-c C-s  queues steering (interrupts after current tool)
;;     Image-bearing drafts refuse these busy paths and remain intact.
;;
;; Press C-c C-p for the full transient menu with model selection,
;; thinking level, completed-thinking controls, session management,
;; and custom commands.  Its Session r entry opens the disk-backed
;; session browser, and Context w opens the conversation-tree browser;
;; press ? in either browser to discover switching/navigation, search,
;; filters, renaming, and labels.
;;
;; See README.org for more documentation.

;;; Code:

(require 'pilish-menu)
(require 'pilish-input)
(require 'pilish-browse)

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

;;;; Main Entry Point

(defcustom pilish-evil-integration t
  "When non-nil, load Evil keybindings automatically when Evil is in use.
Loads `pilish-evil' when a session is set up while Evil is
present.  Set to nil before loading this package to opt out."
  :type 'boolean
  :group 'pilish)

(defun pilish--maybe-load-evil-integration ()
  "Load the optional Evil integration when Evil is in use.
Skips when `pilish-evil-integration' is nil or Evil has not
been loaded.  Called before session buffers are created so initial
Evil states apply to them."
  (when (and pilish-evil-integration (featurep 'evil))
    (require 'pilish-evil nil t)))

(defun pilish--setup-session (dir &optional session)
  "Set up a new or existing session for DIR with optional SESSION name.
Returns the chat buffer."
  (pilish--maybe-load-evil-integration)
  (let* ((chat-buf (pilish--get-or-create-buffer :chat dir session))
         (input-buf (pilish--get-or-create-buffer :input dir session))
         (new-session nil))
    ;; Link buffers to each other
    (with-current-buffer chat-buf
      (pilish--set-chat-session-identity dir session)
      (pilish--set-input-buffer input-buf)
      ;; Start process if not already running
      (unless (and pilish--process (process-live-p pilish--process))
        (pilish--check-dependencies dir)
        (pilish--set-process (pilish--start-process dir))
        (setq new-session t)
        ;; Associate process events and ownership with this chat buffer.
        (when (processp pilish--process)
          (set-process-buffer pilish--process chat-buf)
          (process-put pilish--process 'pilish-chat-buffer chat-buf)
          ;; Register event handler
          (pilish--register-display-handler pilish--process)
          ;; Initialize state from server
          (let ((buf chat-buf)
                (proc pilish--process))  ; Capture for closures
            (pilish--rpc-async proc '(:type "get_state")
              (lambda (response)
                (if (eq (plist-get response :success) t)
                    (progn
                      (pilish--apply-state-response buf response)
                      ;; Check if no model available and warn user
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (unless (plist-get pilish--state :model)
                            (pilish--display-no-model-warning)))))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (when (eq pilish--process proc)
                        (pilish--display-startup-error
                         (plist-get response :error)
                         (plist-get response :stderr)
                         (plist-get response :exitCode))
                        ;; Core invokes pending callbacks before the generic exit
                        ;; handler.  Remember this dead process was rendered so
                        ;; that handler does not append the same diagnostic again.
                        (when (and (plist-get response :processExit)
                                   (processp proc)
                                   (not (process-live-p proc)))
                          (process-put
                           proc 'pilish-exit-error-rendered t))))))))
            ;; Fetch commands via RPC (independent of get_state)
            (pilish--fetch-commands proc
              (lambda (commands)
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (pilish--set-commands commands)
                    (pilish--rebuild-commands-menu))))
              dir))))
      ;; Display startup header for new sessions
      (when new-session
        (pilish--display-startup-header)))
    (with-current-buffer input-buf
      (setq default-directory dir)
      (pilish--set-chat-buffer chat-buf))
    chat-buf))

(defun pilish--show-session-buffers (chat-buf input-buf)
  "Show CHAT-BUF and INPUT-BUF, focusing input when both are visible.
When `pilish-input-window-display' is `hidden', a freshly
displayed session starts with only the chat window visible."
  (if (and (get-buffer-window-list chat-buf nil)
           (get-buffer-window-list input-buf nil))
      (pilish--focus-input-window chat-buf input-buf)
    (pilish--display-buffers
     chat-buf input-buf
     (eq pilish-input-window-display 'hidden))))

(defun pilish--dired-regular-file-at-point ()
  "Return Dired's regular file at point, or nil."
  (when (derived-mode-p 'dired-mode)
    (when-let* ((file (dired-get-filename nil t)))
      (and (file-regular-p file)
           (pilish--route-preserving-expand-file-name file)))))

(defun pilish--regular-jsonl-file-p (file)
  "Return non-nil if FILE is a cheap local JSONL file candidate."
  (when (stringp file)
    (let ((path (expand-file-name file)))
      (and (string-suffix-p ".jsonl" path)
           (not (file-remote-p path))
           (ignore-errors
             (and (file-regular-p path)
                  (file-readable-p path)))))))

(defun pilish--visited-jsonl-file-prompt-default ()
  "Return the current buffer's visited JSONL file for the prompt, or nil."
  (when-let* ((file buffer-file-name)
              (path (expand-file-name file)))
    (and (pilish--regular-jsonl-file-p path)
         path)))

(defun pilish--session-file-prompt-default ()
  "Return an explicit default file for the session-file prompt, or nil."
  (if (derived-mode-p 'dired-mode)
      (pilish--dired-regular-file-at-point)
    (pilish--visited-jsonl-file-prompt-default)))

(defun pilish--read-session-file-name ()
  "Read an existing pi session file name from the minibuffer."
  (let* ((default-file (pilish--session-file-prompt-default))
         (default-dir (and default-file
                           (pilish--route-preserving-file-name-directory
                            default-file)))
         (initial (and default-file (file-name-nondirectory default-file)))
         ;; `read-file-name' otherwise uses the current buffer's visited file
         ;; as a hidden default when DEFAULT-FILENAME and INITIAL are nil.
         (buffer-file-name nil))
    (read-file-name "Pi session file: "
                    default-dir
                    default-file
                    t
                    initial)))

;;;###autoload
(defun pilish (&optional session)
  "Start or switch to pi coding agent session in current project.
With prefix arg, prompt for SESSION name to allow multiple sessions.
If already in a pi buffer and no SESSION specified, ensures this session
is visible. When both chat and input are already shown in the current
frame, keeps layout unchanged and focuses the input window."
  (interactive
   (list (when current-prefix-arg
           (read-string "Session name: "))))
  (let (chat-buf input-buf)
    (if (and (derived-mode-p 'pilish-chat-mode 'pilish-input-mode)
             (not session))
        ;; Already in pi buffer with no new session requested - use current session
        (setq chat-buf (pilish--get-chat-buffer)
              input-buf (pilish--get-input-buffer))
      ;; Find or create session for current directory
      (let ((dir (pilish--session-directory)))
        (setq chat-buf (pilish--setup-session dir session))
        (setq input-buf (buffer-local-value 'pilish--input-buffer chat-buf))))
    (pilish--show-session-buffers chat-buf input-buf)))

;;;###autoload
(defun pilish-open-session-file (session-file)
  "Open pi JSONL SESSION-FILE as a live session.
This uses the normal chat/input UI and switches pi to SESSION-FILE; it is not a
static viewer.  The session header must record a non-empty absolute cwd that
names an existing directory.  Interactively, prompt for an existing file.  In
Dired, default to the regular file at point; otherwise, default to the current
visited local regular readable .jsonl file when there is one."
  (interactive (list (pilish--read-session-file-name)))
  (let* ((session-file (pilish--route-preserving-expand-file-name
                        session-file))
         (dir (pilish--session-file-cwd-or-error session-file)))
    (pilish--check-dependencies dir)
    (let* ((chat-buf (pilish--setup-session dir))
           (input-buf (buffer-local-value 'pilish--input-buffer
                                          chat-buf))
           (proc (buffer-local-value 'pilish--process chat-buf)))
      (pilish--show-session-buffers chat-buf input-buf)
      (when (pilish--session-transition-ready-p chat-buf "open")
        (pilish--resume-selected-session proc chat-buf session-file))
      chat-buf)))

;;;###autoload
(defun pilish-toggle ()
  "Toggle pi coding agent window visibility for the current project.
If pi windows are visible in the current frame, hide them.
If hidden there but a session exists, show them.
If no session exists, signal an error."
  (interactive)
  (let* ((chat-buf (if (derived-mode-p 'pilish-chat-mode 'pilish-input-mode)
                       (pilish--get-chat-buffer)
                     (car (pilish-project-buffers))))
         (input-buf (and chat-buf
                         (buffer-local-value 'pilish--input-buffer chat-buf))))
    (cond
     ;; No session at all
     ((null chat-buf)
      (user-error "No pi session for this project"))
     ;; Session visible in current frame: hide it
     ((or (get-buffer-window-list chat-buf nil)
          (and input-buf (get-buffer-window-list input-buf nil)))
      (with-current-buffer chat-buf
        (pilish--hide-session-windows)))
     ;; Session hidden: show it (chat only when input is shown on demand)
     (t
      (pilish--display-buffers
       chat-buf input-buf
       (pilish--input-window-on-demand-p))))))

(provide 'pilish)
;;; pilish.el ends here
