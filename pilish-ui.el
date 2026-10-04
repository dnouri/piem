;;; pilish-ui.el --- Shared state, faces, and UI primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pilish

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

;; Foundation module for Pilish: shared state, faces, customization,
;; buffer management, display primitives, header-line, and major modes.
;;
;; This is the base layer that all other Pilish modules require.
;; It provides:
;; - Customization options and face definitions
;; - Buffer-local session variables (the shared mutable state)
;; - Buffer creation, naming, and navigation
;; - Display primitives (append-to-chat, scroll preservation, separators)
;; - Header-line formatting and activity phases
;; - Sending infrastructure (send-prompt, abort-send)
;; - Major mode definitions (chat-mode, input-mode)

;;; Code:

(require 'pilish-core)
(require 'cl-lib)
(require 'project)
(require 'md-ts-mode)
(require 'pilish-grammars)
(require 'color)
(require 'diff-mode)


;; Forward declarations: keymaps bind functions defined in other modules.
;; Grouped by target module for easy cross-referencing.

;; pilish-render.el (chat buffer commands)
(declare-function pilish-toggle-tool-section "pilish-render")
(declare-function pilish-shell-command-at-point "pilish-render")
(declare-function pilish-visit-file "pilish-render")
(declare-function pilish-copy-file-path "pilish-render")
(declare-function pilish--dispatch-button "pilish-render")
(declare-function pilish--cleanup-on-kill "pilish-render")
(declare-function pilish--process-followup-queue "pilish-render")
(declare-function pilish--restore-tool-properties "pilish-render")
(declare-function pilish--fontify-with-hover-help "pilish-render")
(declare-function pilish--hover-clear-live-state "pilish-render")
(declare-function pilish--release-nested-tool-state "pilish-render")
(declare-function pilish--maybe-refresh-hot-tail-tables "pilish-table")

;; pilish-input.el (input buffer commands)
(declare-function pilish-quit "pilish-input")
(declare-function pilish-send "pilish-input")
(declare-function pilish-attach-image "pilish-input")
(declare-function pilish-abort "pilish-input")
(declare-function pilish-previous-input "pilish-input")
(declare-function pilish-next-input "pilish-input")
(declare-function pilish-history-isearch-backward "pilish-input")
(declare-function pilish-queue-steering "pilish-input")
(declare-function pilish-input-mode "pilish-input")

;; pilish-browse.el (session browser)
(declare-function pilish-session-browser "pilish-browse")

;; pilish-menu.el (menu and session commands)
(declare-function pilish-menu "pilish-menu" nil t)
(declare-function pilish-new-session "pilish-menu")
(declare-function pilish-export-html "pilish-menu")
(declare-function pilish-compact "pilish-menu")
(declare-function pilish-select-model "pilish-menu")
(declare-function pilish-cycle-thinking "pilish-menu")
(declare-function pilish-fork-at-point "pilish-menu")
(declare-function pilish-copy-last-message "pilish-menu")

;;;; Customization Group

(defgroup pilish nil
  "Emacs frontend for pi coding agent."
  :group 'tools
  :prefix "pilish-")

;;;; Customization

(defcustom pilish-executable '("pi")
  "Command to invoke the pi binary, as a list of strings.
The first element is the program; remaining elements are passed
before \"--mode rpc\", `pilish-extra-args', and the project
trust flag selected by `pilish-project-trust-policy'.

For npx users:
  (setq pilish-executable
        \\='(\"npx\" \"-y\" \"@earendil-works/pi-coding-agent@latest\"))"
  :type '(repeat string)
  :group 'pilish)

(defcustom pilish-project-trust-policy 'approve
  "How to pass Pi project trust flags when starting RPC sessions.
Pi does not show its built-in project trust prompt in RPC mode.  The
Emacs frontend therefore approves project-local Pi inputs by default so
`.pi' prompts, skills, settings, themes, and extensions are available.

Allowed values are:
- `approve'     Pass --approve and trust project-local files for this run.
- `default'     Pass no trust flag and let Pi use trust.json and
                defaultProjectTrust.
- `no-approve'  Pass --no-approve and ignore project-local files for this run."
  :type '(choice (const :tag "Approve project-local files" approve)
                 (const :tag "Use Pi's trust default" default)
                 (const :tag "Ignore project-local files" no-approve))
  :group 'pilish)

(defcustom pilish-rpc-timeout 30
  "Default timeout in seconds for synchronous RPC calls.
Some operations like model loading may need more time."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-session-inactivity-timeout 300
  "Seconds without Pi stdout before warning in the input activity status.
Use a positive number, or nil to disable.  Only streaming and compacting
sessions are monitored.  Any nonempty stdout counts, even partial output
or RPC responses; silence does not prove Pi has stopped working.
Changes take effect at the next header refresh, retaining the output age.
Timing uses approximate wall time, including suspend and Emacs delays."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'pilish)

(defcustom pilish-input-window-height 10
  "Height of the input window.
An integer specifies an absolute number of lines.
A float between 0.0 and 1.0 (exclusive) specifies a fraction of the
total window height, e.g. 0.3 means 30% for input."
  :type '(choice (natnum :tag "Lines")
                 (float :tag "Fraction (0.0–1.0)"))
  :group 'pilish)

(defcustom pilish-input-window-display 'always
  "How the input window is displayed alongside the chat window.
When `always' (the default), the input window is shown whenever the
session is displayed.

When `on-demand', the input window is shown when a session is first
launched, hidden after each send, and reopened with
\[pilish-open-input].  Redisplaying an existing session
\(e.g. with `pilish-toggle') shows only the chat window.

When `hidden', a session launches with only the chat window visible.
In every other respect this is like `on-demand': the input is hidden
after each send, reopened with `pilish-open-input', and
redisplaying an existing session shows only the chat window.  This
suits a chat-centric workflow where you compose in the input only
when needed."
  :type '(choice (const :tag "Always visible" always)
                 (const :tag "On demand (hide after send)" on-demand)
                 (const :tag "Hidden at launch (chat only; open on demand)" hidden))
  :group 'pilish)

(defcustom pilish-activity-phase-functions nil
  "Functions called after a session activity phase is applied.
Each function is called with five arguments:

  CHAT-BUFFER INPUT-BUFFER OLD-PHASE NEW-PHASE REASON

NEW-PHASE is one of \"thinking\", \"replying\", \"running\",
\"queued\", \"compact\", or \"idle\".  INPUT-BUFFER may be nil or dead during
session teardown.

REASON is one of `phase-change', `reset', `teardown',
`input-link', or `input-unlink'.  This lets handlers distinguish a
real session phase change from buffer lifecycle events that merely
reapply or clean up buffer-local UI.

This is an abnormal hook.  Functions should be idempotent because
Pilish may call them again with the same OLD-PHASE and
NEW-PHASE when session buffers are relinked or reset."
  :type 'hook
  :group 'pilish)

(defcustom pilish-separator-width 72
  "Total width of section separators in chat buffer."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-tool-preview-lines 10
  "Maximum visual lines before collapsing tool output or JavaScript.
Also the maximum child summaries shown in a collapsed tool call list."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-bash-preview-lines 5
  "Maximum visual lines to show for bash output before collapsing.
Bash output is typically more verbose, so fewer lines are shown."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-preview-max-bytes 51200
  "Maximum bytes for tool output preview (50KB default).
Prevents huge single-line outputs from blowing up the chat buffer."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-image-preview-max-width 900
  "Maximum pixel width for inline image previews.
Previews are also constrained to the visible chat window."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-image-preview-max-bytes (* 10 1024 1024)
  "Maximum source bytes decoded for one inline image preview.
Larger user-message or tool-result images use a textual placeholder."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-prompt-image-max-bytes (* 3 1024 1024)
  "Maximum source bytes for the image attached to a prompt draft."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-context-warning-threshold 70
  "Context usage percentage at which to show warning color."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-context-error-threshold 90
  "Context usage percentage at which to show error color."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-visit-file-other-window t
  "Whether RET requests the native opener for another window.
When non-nil, RET on a strict tool row, plain path reference, or local Markdown
link label calls `find-file-other-window'; when nil, it calls `find-file'.  A
prefix argument inverts that request.  Emacs display policy may redirect the
final window placement."
  :type 'boolean
  :group 'pilish)

(defcustom pilish-input-markdown-highlighting t
  "Whether to enable markdown syntax highlighting in the input buffer.
When non-nil, the input buffer gets tree-sitter markdown highlighting
\(bold, italic, code spans, fenced blocks) while keeping raw markdown
markup visible.  When nil, the input buffer uses plain `text-mode'.

Takes effect for new sessions; existing input buffers keep their mode."
  :type 'boolean
  :group 'pilish)

(defcustom pilish-copy-raw-markdown nil
  "Whether to copy raw markdown from the chat buffer.
When non-nil, copy commands (`kill-ring-save', `kill-region') preserve
raw markdown — bold markers (**), backticks, code fences, and setext
underlines are kept.  Useful for pasting into docs or other markdown-aware
contexts.

When nil (the default), only the visible text is copied."
  :type 'boolean
  :group 'pilish)

(defcustom pilish-extension-status-faces nil
  "Alist mapping extension status keys to faces in the header line.
Keys are exact `statusKey' strings sent by extension `setStatus' requests,
not necessarily extension package names.  Hovering header status text shows
the key to use here.  Values are face symbols or face attribute plists
accepted by `propertize'.

For example:
  \='((\"sub-status:usage\" . (:foreground \"#c6a0f6\"))
    (\"solveit-mode\" . warning))"
  :type '(alist :key-type string
                :value-type (choice (face :tag "Face")
                                     (plist :tag "Face attributes"
                                            :key-type symbol
                                            :value-type sexp)))
  :group 'pilish)

(defcustom pilish-quit-without-confirmation nil
  "Whether quitting skips confirmation for a live process.
When non-nil, closing a session never asks whether a running pi process
should be terminated.  When nil, `pilish-quit', direct buffer
kills, and exiting Emacs all prompt before killing a live process."
  :type 'boolean
  :group 'pilish)

(defcustom pilish-hot-tail-turn-count 3
  "How many recent headed chat turns stay hot for redisplay refreshes.
The hot tail is the suffix of the chat buffer beginning at the Nth newest
`You' or `Assistant' setext heading.  Resize-sensitive features refresh only
inside that suffix; older history stays frozen until explicitly rebuilt."
  :type 'natnum
  :group 'pilish)

(defcustom pilish-thinking-display 'visible
  "Default display mode for completed assistant thinking in new chat buffers.
New chat buffers copy this user preference into a buffer-local session value.
Later per-buffer toggles affect only that chat buffer; they do not change this
user option.

Allowed values are:
- `visible'  Keep completed thinking expanded as blockquote markdown.
- `hidden'   Collapse completed thinking to a short stub line.

Live streaming thinking is always shown while the assistant is still working.
Per-block TAB toggles are temporary local overrides and are cleared by buffer
rebuilds, reloads, or whole-chat display-mode changes."
  :type '(choice (const :tag "Visible" visible)
                 (const :tag "Hidden" hidden))
  :group 'pilish)

(defcustom pilish-thinking-hidden-preview t
  "Whether hidden completed thinking should preview its first line.
When non-nil, collapsed completed thinking shows the first non-empty trimmed
line when the normalized thinking spans more than one line, is at least
3 characters long, and shorter than 72 characters. Otherwise the hidden block
falls back to a generic line-count label."
  :type 'boolean
  :group 'pilish)

(defcustom pilish-prettify-tables t
  "Whether display-only markdown tables use prettier visible separators.
When non-nil, table overlays replace raw markdown pipes and separator rows
with Unicode box-drawing characters in the visible display.  The underlying
buffer text stays canonical markdown, so copy, search, and session history
still operate on the raw table source."
  :type 'boolean
  :group 'pilish)

;;;; Faces

(defface pilish-timestamp
  '((t :inherit shadow))
  "Face for timestamps in message headers."
  :group 'pilish)

(defface pilish-tool-name
  '((t :inherit font-lock-function-name-face :weight bold :slant italic))
  "Face for tool names (BASH, READ, etc.) in pi chat."
  :group 'pilish)

(defface pilish-tool-command
  '((t :inherit font-lock-function-name-face :slant italic))
  "Face for tool commands and arguments."
  :group 'pilish)

(defface pilish-tool-output
  '((t :inherit shadow))
  "Face for tool output text."
  :group 'pilish)

(defface pilish-tool-block
  '((t :extend t))
  "Face for tool blocks.
Subtle blue-tinted background derived from the current theme."
  :group 'pilish)

(defface pilish-tool-block-error
  '((t :extend t))
  "Face for tool blocks after failed completion.
Background is derived from the current theme so syntax faces stay visible."
  :group 'pilish)

(defface pilish-diff-line-added
  '((t :extend t))
  "Face for added edit-diff lines.
Background is derived from the current theme so syntax faces stay visible."
  :group 'pilish)

(defface pilish-diff-line-removed
  '((t :extend t))
  "Face for removed edit-diff lines.
Background is derived from the current theme so syntax faces stay visible."
  :group 'pilish)

(defface pilish-collapsed-indicator
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for collapsed content indicators."
  :group 'pilish)

(defface pilish-model-name
  '((t :inherit font-lock-type-face))
  "Face for model name in header line."
  :group 'pilish)

(defface pilish-activity-phase
  '((t :inherit shadow))
  "Face for activity phase label in header line."
  :group 'pilish)

(defface pilish-retry-notice
  '((t :inherit warning :slant italic))
  "Face for retry notifications (rate limit, overloaded, etc.)."
  :group 'pilish)

(defface pilish-error-notice
  '((t :inherit error))
  "Face for error notifications from the server."
  :group 'pilish)

;;;; Dynamic Face Computation

(defun pilish--blend-color (base target amount)
  "Blend BASE color toward TARGET by AMOUNT (0.0–1.0).
Returns a hex color string.  AMOUNT of 0.0 returns BASE unchanged;
1.0 returns TARGET."
  (apply #'color-rgb-to-hex
         (cl-mapcar (lambda (b tgt)
                      (+ (* (- 1.0 amount) b) (* amount tgt)))
                    (color-name-to-rgb base)
                    (color-name-to-rgb target))))

(defun pilish--dark-color-p (color)
  "Return non-nil when COLOR has low lightness."
  (< (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb color))) 0.5))

(defun pilish--theme-face-background (face)
  "Return FACE background color from the current theme, or nil."
  (let ((bg (face-background face nil t)))
    (and bg (color-defined-p bg) bg)))

(defun pilish--theme-face-foreground (face)
  "Return FACE foreground color from the current theme, or nil."
  (let ((fg (face-foreground face nil t)))
    (and fg (color-defined-p fg) fg)))

(defun pilish--theme-diff-background (diff-face indicator-face)
  "Return a syntax-friendly line background derived from DIFF-FACE.
Prefer DIFF-FACE's own background.  If the theme only colors diff
foregrounds, blend the default background toward DIFF-FACE's foreground,
falling back to INDICATOR-FACE when needed."
  (or (pilish--theme-face-background diff-face)
      (when-let* ((bg (pilish--theme-face-background 'default))
                  (tint (or (pilish--theme-face-foreground diff-face)
                            (pilish--theme-face-foreground indicator-face))))
        (pilish--blend-color
         bg tint (if (pilish--dark-color-p bg) 0.20 0.10)))))

(defun pilish--set-face-background-only (face background)
  "Set FACE to contribute only BACKGROUND so syntax foregrounds stay visible."
  (set-face-attribute face nil
                      :inherit nil
                      :foreground 'unspecified
                      :background (or background 'unspecified)
                      :extend t))

(defun pilish--update-tool-block-face ()
  "Set `pilish-tool-block' background from theme."
  (when-let* ((bg (pilish--theme-face-background 'default)))
    (let* ((dark-p (pilish--dark-color-p bg))
           (tint (if dark-p "#5555cc" "#3333aa"))
           (amount (if dark-p 0.12 0.08)))
      (set-face-attribute
       'pilish-tool-block nil
       :background
       (pilish--blend-color bg tint amount)))))

(defun pilish--update-tool-block-error-face ()
  "Set `pilish-tool-block-error' background from theme."
  (pilish--set-face-background-only
   'pilish-tool-block-error
   (pilish--theme-diff-background
    'diff-removed 'diff-indicator-removed)))

(defun pilish--update-edit-diff-faces ()
  "Set edit-diff line faces from the current theme."
  (pilish--set-face-background-only
   'pilish-diff-line-added
   (pilish--theme-diff-background
    'diff-added 'diff-indicator-added))
  (pilish--set-face-background-only
   'pilish-diff-line-removed
   (pilish--theme-diff-background
    'diff-removed 'diff-indicator-removed)))

(defun pilish--update-theme-derived-faces (&rest _)
  "Set internal faces derived from the current theme.
Updates tool blocks plus edit-diff overlays.  Called from mode setup and
on theme changes."
  (dolist (update '(pilish--update-tool-block-face
                    pilish--update-tool-block-error-face
                    pilish--update-edit-diff-faces))
    (condition-case-unless-debug nil
        (funcall update)
      (error nil))))

;; Recompute when theme changes (Emacs 29+)
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions
            #'pilish--update-theme-derived-faces))

;;;; Language Detection

(defconst pilish--extension-language-alist
  '(("ts" . "typescript") ("tsx" . "typescript")
    ("js" . "javascript") ("jsx" . "javascript") ("mjs" . "javascript")
    ("py" . "python") ("pyw" . "python")
    ("rb" . "ruby") ("rake" . "ruby")
    ("rs" . "rust")
    ("go" . "go")
    ("el" . "emacs-lisp") ("lisp" . "lisp") ("cl" . "lisp")
    ("sh" . "bash") ("bash" . "bash") ("zsh" . "zsh")
    ("c" . "c") ("h" . "c")
    ("cpp" . "cpp") ("cc" . "cpp") ("cxx" . "cpp") ("hpp" . "cpp")
    ("java" . "java")
    ("kt" . "kotlin") ("kts" . "kotlin")
    ("swift" . "swift")
    ("cs" . "csharp")
    ("php" . "php")
    ("json" . "json")
    ("yaml" . "yaml") ("yml" . "yaml")
    ("toml" . "toml")
    ("xml" . "xml")
    ("html" . "html") ("htm" . "html")
    ("css" . "css") ("scss" . "scss") ("sass" . "sass")
    ("sql" . "sql")
    ("md" . "markdown")
    ("org" . "org")
    ("lua" . "lua")
    ("r" . "r") ("R" . "r")
    ("pl" . "perl") ("pm" . "perl")
    ("hs" . "haskell")
    ("ml" . "ocaml") ("mli" . "ocaml")
    ("ex" . "elixir") ("exs" . "elixir")
    ("erl" . "erlang")
    ("clj" . "clojure") ("cljs" . "clojure")
    ("scala" . "scala")
    ("vim" . "vim")
    ("dockerfile" . "dockerfile")
    ("makefile" . "makefile") ("mk" . "makefile"))
  "Alist mapping file extensions to language names for syntax highlighting.")

(defsubst pilish--tool-path (args)
  "Extract file path from tool ARGS.
Checks both :path and :file_path keys for compatibility."
  (or (plist-get args :path)
      (plist-get args :file_path)))

(defun pilish--path-to-language (path)
  "Return language name for PATH based on file extension.
Returns \"text\" for unrecognized extensions to ensure consistent fencing.
Return nil when PATH is not a string."
  (when (stringp path)
    (let ((ext (downcase (or (file-name-extension path) ""))))
      (or (cdr (assoc ext pilish--extension-language-alist))
          "text"))))

;;;; Major Modes

(defvar pilish-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'pilish-quit)
    (define-key map (kbd "C-c C-p") #'pilish-menu)
    (define-key map (kbd "C-c C-k") #'pilish-abort)
    (define-key map (kbd "C-c C-n") #'pilish-new-session)
    (define-key map (kbd "C-c C-r") #'pilish-session-browser)
    (define-key map (kbd "C-c C-e") #'pilish-export-html)
    (define-key map (kbd "C-c C-c") #'pilish-compact)
    (define-key map (kbd "C-c C-m") #'pilish-select-model)
    (define-key map (kbd "C-c C-t") #'pilish-cycle-thinking)
    (define-key map (kbd "C-c C-y") #'pilish-copy-last-message)
    (define-key map (kbd "n") #'pilish-next-message)
    (define-key map (kbd "p") #'pilish-previous-message)
    (define-key map (kbd "f") #'pilish-fork-at-point)
    (define-key map (kbd "TAB") #'pilish-toggle-tool-section)
    (define-key map (kbd "<tab>") #'pilish-toggle-tool-section)
    (define-key map (kbd "!") #'pilish-shell-command-at-point)
    (define-key map (kbd "w") #'pilish-copy-file-path)
    (define-key map (kbd "RET") #'pilish-visit-file)
    (define-key map (kbd "<return>") #'pilish-visit-file)
    (define-key map [remap push-button] #'pilish--dispatch-button)
    map)
  "Keymap for `pilish-chat-mode'.")

;;;; You Heading Detection

(defconst pilish--you-heading-re
  "^You\\( · .*\\)?$"
  "Regex matching the first line of a user turn setext heading.
Matches `You' at line start, optionally followed by ` · <timestamp>'.
Must be verified with `pilish--at-you-heading-p' to confirm
the next line is a setext underline (===), avoiding false matches on
user message text starting with \"You\".")

(defun pilish--at-you-heading-p ()
  "Return non-nil if current line is a You setext heading.
Checks that the current line matches `pilish--you-heading-re'
and the next line is a setext underline (three or more `=' characters)."
  (and (save-excursion
         (beginning-of-line)
         (looking-at pilish--you-heading-re))
       (save-excursion
         (forward-line 1)
         (looking-at "^=\\{3,\\}$"))))

(defvar-local pilish--hot-tail-start nil
  "Marker at the start of the recent hot-tail suffix.
Tables and future redisplay-sensitive subsystems refresh only at or after
this boundary.")

(defconst pilish--turn-heading-re
  "^\\(?:You\\(?: · .*\\)?\\|Assistant\\)$"
  "Regex matching headed chat turns that define the hot-tail boundary.")

(defun pilish--at-turn-heading-p ()
  "Return non-nil if current line is a hot-tail turn heading.
A turn heading is a `You' or `Assistant' setext heading whose next line is
an underline of three or more `=' characters."
  (and (save-excursion
         (beginning-of-line)
         (looking-at pilish--turn-heading-re))
       (save-excursion
         (forward-line 1)
         (looking-at "^=\\{3,\\}$"))))

;;;; Turn Detection

(defun pilish--collect-you-headings ()
  "Return list of buffer positions of all You setext headings.
Scans from `point-min', returns positions in chronological order."
  (let (headings)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pilish--you-heading-re nil t)
        (let ((pos (match-beginning 0)))
          (save-excursion
            (goto-char pos)
            (when (pilish--at-you-heading-p)
              (push pos headings))))))
    (nreverse headings)))

(defun pilish--user-turn-index-at-point (&optional headings)
  "Return 0-based index of the user turn at or before point.
HEADINGS is an optional pre-computed list from
`pilish--collect-you-headings'; when nil, the buffer is scanned.
Returns nil if point is before the first You heading."
  (let ((headings (or headings (pilish--collect-you-headings)))
        (limit (point))
        (index 0)
        (result nil))
    (dolist (h headings)
      (when (<= h limit)
        (setq result index))
      (setq index (1+ index)))
    result))

(defun pilish--update-hot-tail-boundary ()
  "Move `pilish--hot-tail-start' to the recent headed-turn suffix.
The marker lands on the Nth newest `You' or `Assistant' heading, where N is
`pilish-hot-tail-turn-count'.  If there are at most N headed turns,
all content stays hot and the marker moves to `point-min'.  A count of 0
makes the hot region empty by moving the marker to `point-max'."
  (let ((headings nil)
        (count pilish-hot-tail-turn-count))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pilish--turn-heading-re nil t)
        (let ((candidate (match-beginning 0)))
          (save-excursion
            (goto-char candidate)
            (when (pilish--at-turn-heading-p)
              (push candidate headings))))))
    (setq headings (nreverse headings))
    (move-marker
     pilish--hot-tail-start
     (cond
      ((zerop count) (point-max))
      ((<= (length headings) count) (point-min))
      (t (nth (- (length headings) count) headings)))
     (current-buffer))))

(defun pilish--in-hot-tail-p (pos)
  "Return non-nil when POS is inside the hot tail."
  (>= pos (marker-position pilish--hot-tail-start)))

;;;; Chat Navigation

(defun pilish--find-you-heading (search-fn)
  "Find the next You setext heading using SEARCH-FN.
SEARCH-FN is `re-search-forward' or `re-search-backward'.
Returns the position of the heading line start, or nil if not found."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (funcall search-fn pilish--you-heading-re nil t))
        (let ((candidate (match-beginning 0)))
          (save-excursion
            (goto-char candidate)
            (when (pilish--at-you-heading-p)
              (setq found candidate)))))
      found)))

(defun pilish-next-message ()
  "Move to the next user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (forward-line 1)
               (pilish--find-you-heading #'re-search-forward))))
    (if pos
        (progn
          (goto-char pos)
          (when (get-buffer-window) (recenter 0)))
      (message "No more messages"))))

(defun pilish-previous-message ()
  "Move to the previous user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (beginning-of-line)
               (pilish--find-you-heading #'re-search-backward))))
    (if pos
        (progn
          (goto-char pos)
          (when (get-buffer-window) (recenter 0)))
      (message "No previous message"))))

;;;; Copy Visible Text

(defun pilish--visible-text-span-p (position)
  "Return non-nil when buffer text at POSITION contributes visible text.
This deliberately follows the package's existing visible-copy semantics:
active `invisible' text and text whose `display' property is the empty string
are omitted; nonempty display replacements and overlay display strings are not
expanded into synthetic buffer characters."
  (let ((invisible (get-text-property position 'invisible))
        (display (get-text-property position 'display)))
    (and (not (and invisible (invisible-p invisible)))
         (not (equal display "")))))

(defun pilish--position-inside-omitted-text-p (position beg end)
  "Return non-nil when POSITION has no visible boundary in BEG..END.
A position strictly inside one omitted run, or between adjacent omitted property
runs, is hidden because neither neighboring character contributes visible text.
The outer run boundaries remain usable as adjacent visible positions."
  (and (< position end)
       (> position beg)
       (not (pilish--visible-text-span-p position))
       (not (pilish--visible-text-span-p (1- position)))))

(defun pilish--visible-text (beg end)
  "Return visible text between BEG and END, preserving text properties.
Skips characters with `invisible' property matching `buffer-invisibility-spec'
and characters with `display' property equal to the empty string.
The returned string carries face properties from font-lock, which
display overlay strings render faithfully (bold, italic, code, etc.)."
  (let ((result nil)
        (pos beg))
    (while (< pos end)
      (let ((next (min
                   (next-single-char-property-change pos 'invisible nil end)
                   (next-single-char-property-change pos 'display nil end))))
        (when (pilish--visible-text-span-p pos)
          (push (buffer-substring pos next) result))
        (setq pos next)))
    (apply #'concat (nreverse result))))

(defun pilish--visible-text-with-position-map (beg end position)
  "Project visible buffer text from BEG to END and map POSITION into it.
Return a plist with `:text', `:positions', and `:index'.
`:positions' is a vector parallel to `:text': element N is the exact buffer
position of visible character N.  `:index' is the visible boundary at POSITION,
namely the number of projected characters whose source positions precede it.
A nonempty visible half-open range [A,B) maps back to the real buffer envelope
from `(aref POSITIONS A)' through one past `(aref POSITIONS (1- B))'.  This
preserves hidden inline spans inside a visible candidate while excluding hidden
prefixes and suffixes from its bounds.

The caller owns bounding BEG and END; this helper never widens or fontifies the
buffer.  Its visibility rule is exactly `pilish--visible-text''s and,
like that function, does not interpret overlay display replacement strings."
  (unless (and (<= beg position) (<= position end))
    (error "Position %s is outside visible input range %s..%s"
           position beg end))
  (let ((chunks nil)
        (source-positions (make-vector (- end beg) nil))
        (visible-count 0)
        (pos beg)
        index index-set)
    (while (< pos end)
      (let ((next (min
                   (next-single-char-property-change pos 'invisible nil end)
                   (next-single-char-property-change pos 'display nil end))))
        (if (pilish--visible-text-span-p pos)
            (progn
              (push (buffer-substring-no-properties pos next) chunks)
              (unless index-set
                (when (<= position next)
                  (setq index (+ visible-count
                                 (max 0 (min (- position pos)
                                             (- next pos)))))
                  (setq index-set t)))
              (let ((source pos))
                (while (< source next)
                  (aset source-positions visible-count source)
                  (setq source (1+ source)
                        visible-count (1+ visible-count)))))
          (unless index-set
            (when (<= position next)
              (setq index visible-count
                    index-set t))))
        (setq pos next)))
    (list :text (apply #'concat (nreverse chunks))
          :positions (cl-subseq source-positions 0 visible-count)
          :index (or index visible-count))))

(defun pilish--filter-buffer-substring (beg end &optional delete)
  "Filter function for `filter-buffer-substring-function' in chat buffers.
When `pilish-copy-raw-markdown' is nil, returns only visible
text between BEG and END.  If DELETE is non-nil, also removes the region.
Raw copying keeps Markdown characters but strips internal render properties."
  (if pilish-copy-raw-markdown
      (substring-no-properties (buffer-substring--filter beg end delete))
    (prog1 (substring-no-properties (pilish--visible-text beg end))
      (when delete (delete-region beg end)))))

(defvar-local pilish--canonical-buffer-name nil
  "Stable session buffer name for this chat buffer.
A chat buffer may also be backed by a transcript file, but session lookup
still uses this name to find the live conversation.")

(defvar-local pilish--canonical-session-directory nil
  "Stable session directory for this chat buffer.
Project lookup, window toggling, and path completion use this directory even
when the buffer is also backed by a transcript file elsewhere.")

(defvar-local pilish--canonical-session-name nil
  "Optional named-session suffix for this chat buffer.")

(defvar pilish--chat-buffer)

(defun pilish--chat-session-buffer-name (&optional buffer)
  "Return the stable session buffer name for chat BUFFER.
Falls back to the live `buffer-name' when BUFFER has no canonical name yet."
  (with-current-buffer (or buffer (current-buffer))
    (or pilish--canonical-buffer-name
        (buffer-name))))

(defun pilish--chat-session-directory (&optional buffer)
  "Return the stable session directory for chat BUFFER.
Falls back to BUFFER's `default-directory' when no canonical directory is
recorded yet."
  (with-current-buffer (or buffer (current-buffer))
    (or pilish--canonical-session-directory
        default-directory)))

(defun pilish--chat-session-name (&optional buffer)
  "Return the optional named-session suffix for chat BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    pilish--canonical-session-name))

(defun pilish--set-chat-session-identity (dir &optional session)
  "Record the stable session identity for the current chat buffer.
DIR is the session directory and SESSION is the optional named-session suffix."
  (setq pilish--canonical-buffer-name
        (pilish--buffer-name :chat dir session)
        pilish--canonical-session-directory dir
        pilish--canonical-session-name session
        default-directory dir))

(defun pilish--restore-chat-buffer-read-only ()
  "Restore the normal read-only contract for chat buffers after saving."
  (setq buffer-read-only t))

(define-derived-mode pilish-chat-mode md-ts-mode "Pi-Chat"
  "Major mode for displaying pi conversation.
Derives from `md-ts-mode' for tree-sitter syntax highlighting.
This is a read-only buffer showing the conversation history."
  :group 'pilish
  (setq-local buffer-read-only t)
  ;; Chat buffers are generated read-only views.  Recording every incremental
  ;; streaming and rendering update retains large undo trees for content the
  ;; user cannot edit, so keep undo disabled for the lifetime of the buffer.
  (buffer-disable-undo)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; Hide markdown markup (**, `, ```) for cleaner display
  (setq-local md-ts-hide-markup t)
  (md-ts--set-hide-markup t)
  ;; Strip hidden markup from copy operations (M-w, C-w)
  (setq-local filter-buffer-substring-function
              #'pilish--filter-buffer-substring)
  (setq-local pilish--thinking-display pilish-thinking-display)
  (setq-local pilish--tool-args-cache (make-hash-table :test 'equal))
  (setq-local pilish--live-tool-blocks (make-hash-table :test 'equal))
  (setq-local pilish--tool-block-order-counter 0)
  (setq-local pilish--thinking-block-order-counter 0)
  (setq-local pilish--history-load-generation 0)
  (setq-local pilish--session-transition-generation 0)
  (setq-local pilish--session-transition-active nil)
  (setq-local pilish--model-change-generation 0)
  (setq-local pilish--model-change-active-token nil)
  (setq-local pilish--local-user-message-region nil)
  ;; Disable hl-line-mode: its post-command-hook overlay update causes
  ;; scroll oscillation in buffers with invisible text + variable heights.
  (setq-local global-hl-line-mode nil)
  (hl-line-mode -1)
  ;; Make window-point follow inserted text (like comint does).
  ;; This is key for natural scroll behavior during streaming.
  (setq-local window-point-insertion-type t)
  ;; Recent content is hot by default in a fresh chat buffer.
  (setq-local pilish--hot-tail-start (copy-marker (point-min) nil))

  ;; Run after font-lock to undo markdown damage in tool overlays.
  (jit-lock-register #'pilish--restore-tool-properties)
  ;; Let native Markdown help take precedence over completed-block fallback.
  (add-function :around (local 'font-lock-fontify-region-function)
                #'pilish--fontify-with-hover-help)

  ;; Compute theme-derived faces used by chat overlays.
  (pilish--update-theme-derived-faces)

  ;; Saving a transcript should not make the live chat editable.
  (add-hook 'after-save-hook #'pilish--restore-chat-buffer-read-only nil t)
  (add-hook 'window-configuration-change-hook
            #'pilish--maybe-refresh-hot-tail-tables nil t)
  (add-hook 'window-size-change-functions
            #'pilish--maybe-rebalance-windows)
  (add-hook 'kill-buffer-query-functions
            #'pilish--session-kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook #'pilish--cleanup-on-kill nil t))

(put 'pilish-chat-mode 'mode-class 'special)

(defun pilish-complete ()
  "Complete at point, suppressing help text in the *Completions* buffer.
This wraps `completion-at-point' with `completion-show-help' bound to nil,
removing the instructional header that would otherwise appear."
  (interactive)
  (let ((completion-show-help nil))
    (completion-at-point)))

(defvar pilish-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pilish-send)
    (define-key map (kbd "TAB") #'pilish-complete)
    (define-key map (kbd "C-c C-k") #'pilish-abort)
    (define-key map (kbd "C-c C-p") #'pilish-menu)
    (define-key map (kbd "C-c C-r") #'pilish-session-browser)
    (define-key map (kbd "M-p") #'pilish-previous-input)
    (define-key map (kbd "M-n") #'pilish-next-input)
    (define-key map (kbd "<C-up>") #'pilish-previous-input)
    (define-key map (kbd "<C-down>") #'pilish-next-input)
    (define-key map (kbd "C-r") #'pilish-history-isearch-backward)
    ;; Message queuing (steering only - follow-up handled by C-c C-c)
    (define-key map (kbd "C-c C-s") #'pilish-queue-steering)
    map)
  "Keymap for `pilish-input-mode'.")

;;;; Session Directory Detection

(defun pilish--session-directory ()
  "Determine directory for the current pi session context.
Inside pi buffers, uses the chat buffer's stable session directory so manual
transcript saves do not retarget the live session.  Elsewhere, uses the
current project root when available, falling back to `default-directory'.
Always returns an expanded absolute path; remote TRAMP home text is preserved."
  (pilish--route-preserving-expand-file-name
   (cond
    ((derived-mode-p 'pilish-chat-mode)
     (pilish--chat-session-directory))
    ((derived-mode-p 'pilish-input-mode)
     (if (buffer-live-p pilish--chat-buffer)
         (with-current-buffer pilish--chat-buffer
           (pilish--chat-session-directory))
       default-directory))
    (t
     (or (when-let* ((proj (project-current)))
           ;; `project-current' may return an instance whose backend
           ;; never defined a `project-root' method (older projectile
           ;; returns (projectile . DIR)).  Recover the root from that
           ;; cons shape, else degrade to `default-directory' instead
           ;; of crashing session startup on `cl-no-applicable-method'.
           (condition-case nil
               (project-root proj)
             (cl-no-applicable-method
              (when (and (consp proj) (stringp (cdr proj)))
                (cdr proj)))))
         default-directory)))))

;;;; Buffer Naming & Creation

(defun pilish--buffer-name (type dir &optional session)
  "Generate buffer name for TYPE (:chat or :input) in DIR.
Optional SESSION name creates a named session.
Uses abbreviated directory for readability in buffer lists."
  (let ((type-str (pcase type
                    (:chat "chat")
                    (:input "input")))
        (abbrev-dir (pilish--route-preserving-abbreviate-file-name
                     dir)))
    (if (and session (not (string-empty-p session)))
        (format "*pilish-%s:%s<%s>*" type-str abbrev-dir session)
      (format "*pilish-%s:%s*" type-str abbrev-dir))))

(defun pilish--find-session (dir &optional session)
  "Find existing chat buffer for DIR and SESSION.
Matches the chat buffer's stable session identity, even when the buffer is
also visiting a transcript file and therefore has a different live name."
  (let ((target-name (pilish--buffer-name :chat dir session)))
    (cl-find-if
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p 'pilish-chat-mode)
                   (equal (pilish--chat-session-buffer-name)
                          target-name)))))
     (buffer-list))))

(defun pilish--get-or-create-buffer (type dir &optional session)
  "Get or create buffer of TYPE for DIR and optional SESSION.
TYPE is :chat or :input.  Returns the buffer.
Existing buffers keep their state; session metadata is refreshed explicitly
by session setup code."
  (let* ((name (pilish--buffer-name type dir session))
         (existing (if (eq type :chat)
                       (pilish--find-session dir session)
                     (get-buffer name)))
         (buf (or existing (generate-new-buffer name))))
    (unless existing
      (with-current-buffer buf
        (pcase type
          (:chat
           (pilish-chat-mode)
           (pilish--set-chat-session-identity dir session))
          (:input
           (pilish-input-mode)
           (setq default-directory dir)))))
    buf))

;;;; Project Buffer Discovery

(defun pilish--normalize-directory (dir)
  "Normalize DIR for exact path comparisons.
Returns an expanded absolute path with a trailing slash."
  (pilish--route-preserving-file-name-as-directory
   (pilish--route-preserving-expand-file-name dir)))

(defun pilish-project-buffers ()
  "Return pi chat buffers for the current project directory.
Matches buffers by their stable session directory, not by the live buffer name
or transcript file location.  Returns a list ordered by `buffer-list'
recency, with the most recent buffer first."
  (let ((target-dir (pilish--normalize-directory
                     (pilish--session-directory))))
    (cl-remove-if-not
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p 'pilish-chat-mode)
                   (stringp (pilish--chat-session-directory))
                   (string=
                    (pilish--normalize-directory
                     (pilish--chat-session-directory))
                    target-dir)))))
     (buffer-list))))

;;;; Window Hiding

(defun pilish--hide-session-windows ()
  "Hide the current pi session in the selected frame.
Preserves this frame's window layout by deleting input windows (the
child splits created by `pilish--display-buffers') and
replacing chat windows with their previous buffers via `bury-buffer'.

Must be called from a pi chat or input buffer.  Only affects windows
of the current session in the selected frame."
  (let ((chat-buf (pilish--get-chat-buffer))
        (input-buf (pilish--get-input-buffer)))
    (when (buffer-live-p input-buf)
      (dolist (win (get-buffer-window-list input-buf nil))
        (when (and (window-live-p win)
                   (window-deletable-p win))
          (delete-window win))))
    (when (buffer-live-p chat-buf)
      (dolist (win (get-buffer-window-list chat-buf nil))
        (with-selected-window win
          (bury-buffer))))))

;;;; Buffer-Local Session Variables

(defvar-local pilish--process nil
  "The pi RPC subprocess for this session.")

(defvar-local pilish--process-version nil
  "Detected pi CLI version for the current process.")

(defvar-local pilish--model-change-generation 0
  "Monotonic generation for asynchronous model-change callbacks.")

(defvar-local pilish--model-change-active-token nil
  "Process-bound token owned by the active model change, or nil.")

(defun pilish--begin-model-change (process &optional chat-buffer)
  "Begin a model change through PROCESS in CHAT-BUFFER and return its token.
Return nil if PROCESS is no longer current.  CHAT-BUFFER defaults to the
current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process pilish--process)
          (setq pilish--model-change-generation
                (1+ (or pilish--model-change-generation 0)))
          (setq pilish--model-change-active-token
                (cons pilish--model-change-generation process)))))))

(defun pilish--model-change-owned-p (token &optional chat-buffer)
  "Return non-nil when TOKEN owns CHAT-BUFFER's model-change gate.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (and token
         (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (eq token pilish--model-change-active-token)
                (eql (car token) pilish--model-change-generation))))))

(defun pilish--model-change-current-p (token &optional chat-buffer)
  "Return non-nil when TOKEN owns CHAT-BUFFER's current-process model change.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (and (pilish--model-change-owned-p token buffer)
         (with-current-buffer buffer
           (eq (cdr token) pilish--process)))))

(defun pilish--finish-model-change (token &optional chat-buffer)
  "Finish CHAT-BUFFER's model change only when TOKEN still owns it.
Unlike applying its response, cleanup does not require TOKEN's process to
remain current.  CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (pilish--model-change-owned-p token buffer)
      (with-current-buffer buffer
        (setq pilish--model-change-active-token nil))
      t)))

(defun pilish--invalidate-model-change (&optional chat-buffer)
  "Invalidate any model change in CHAT-BUFFER and return the new generation.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq pilish--model-change-generation
              (1+ (or pilish--model-change-generation 0))
              pilish--model-change-active-token nil)
        pilish--model-change-generation))))

(defun pilish--model-change-pending-p (&optional chat-buffer)
  "Return whether CHAT-BUFFER has an active model change.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (buffer-local-value 'pilish--model-change-active-token buffer)
         t)))

(defun pilish--cancel-model-change-and-restore-followups
    (&optional chat-buffer)
  "Cancel CHAT-BUFFER's model change and restore text queued behind it."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (and (buffer-live-p buffer)
               (pilish--model-change-pending-p buffer))
      (with-current-buffer buffer
        (pilish--invalidate-model-change)
        (pilish--restore-followup-queue-to-input))
      t)))

(defun pilish--set-process (process)
  "Set the pi RPC subprocess PROCESS for this session.
Resets cached process version and starts a delayed version probe for
new live processes in interactive sessions."
  (unless (eq process pilish--process)
    (pilish--cancel-inactivity-timer)
    (when (processp process)
      (process-put process 'pilish-last-output-time (float-time)))
    (pilish--invalidate-model-change)
    (pilish--invalidate-prompt-start-wait)
    ;; Reload can replace a process without its exit handler or a successful
    ;; subsequent history refresh.  End live measurements at this boundary.
    (when (fboundp 'pilish--hover-clear-live-state)
      (pilish--hover-clear-live-state))
    (when (fboundp 'pilish--release-nested-tool-state)
      (pilish--release-nested-tool-state t))
    (force-mode-line-update t))
  (setq pilish--process process
        pilish--process-version nil)
  (pilish--reconcile-inactivity-timer)
  (when (and (processp process)
             (process-live-p process)
             (not noninteractive))
    (pilish--probe-process-version-async (current-buffer))))

(defvar-local pilish--chat-buffer nil
  "Reference to the chat buffer for this session.")

(defun pilish--set-chat-buffer (buffer)
  "Set the chat BUFFER reference for this session.
In input buffers, also store BUFFER in `other-window-scroll-buffer'
so built-in other-window scrolling commands target the linked chat."
  (setq pilish--chat-buffer buffer)
  (when (derived-mode-p 'pilish-input-mode)
    (setq-local other-window-scroll-buffer buffer)))

(defvar-local pilish--input-buffer nil
  "Reference to the input buffer for this session.")

(defvar pilish--activity-phase)

(defun pilish--set-input-buffer (buffer)
  "Set the input BUFFER reference for this session."
  (let ((old-buffer pilish--input-buffer)
        (phase pilish--activity-phase))
    (unless (eq old-buffer buffer)
      (when (and old-buffer (buffer-live-p old-buffer))
        (pilish--run-activity-phase-functions
         (current-buffer) old-buffer phase "idle" 'input-unlink))
      (setq pilish--input-buffer buffer)
      (when (and buffer (buffer-live-p buffer))
        (pilish--set-activity-phase phase 'input-link t)))))

(defvar-local pilish--thinking-display nil
  "Completed-thinking display mode for this chat buffer.
One of the symbols `visible' or `hidden'. Live streaming thinking is always
shown while the assistant is still working; this mode is applied when a
thinking block completes and whenever completed thinking is redisplayed later.
Temporary per-block TAB toggles do not change this buffer-local preference.")

(defun pilish--set-thinking-display (mode)
  "Set completed-thinking display MODE for the current chat buffer."
  (setq pilish--thinking-display mode))

(defun pilish--thinking-display-mode ()
  "Return the active completed-thinking display mode for this chat buffer."
  (or pilish--thinking-display
      pilish-thinking-display
      'visible))

(defvar-local pilish--canonical-messages nil
  "Canonical session messages cached for idle history rebuilds.
This is updated from successful history loads and completed agent turns.  It is
used when the buffer needs a canonical transcript again, such as reload,
resume, fork, or explicit history rerenders, so the buffer does not have to
parse rendered text back into message structure.")

(defun pilish--set-canonical-messages (messages)
  "Set canonical session MESSAGES for the current chat buffer."
  (setq pilish--canonical-messages messages))

(defvar-local pilish--history-load-generation 0
  "Monotonic generation number for in-flight canonical history loads.
Each new history request or local outbound send bumps this counter so stale
callbacks cannot rebuild the chat buffer over newer session state.")

(defun pilish--set-history-load-generation (generation)
  "Set canonical history-load GENERATION for the current chat buffer."
  (setq pilish--history-load-generation generation))

(defun pilish--invalidate-history-loads ()
  "Invalidate pending canonical history requests and return the new generation."
  (let ((next (1+ (or pilish--history-load-generation 0))))
    (pilish--set-history-load-generation next)
    next))

(defvar-local pilish--session-transition-generation 0
  "Monotonic generation for async session-transition callbacks.
Each session switch, fork, or reset bumps this counter so stale callbacks
cannot apply older session identity or header state over a newer session view.")

(defvar-local pilish--session-transition-active nil
  "Non-nil while a session switch or fork RPC is in flight.")

(defvar-local pilish--session-transition-process nil
  "Process allowed to complete the active session transition.")

(defun pilish--set-session-transition-generation (generation)
  "Set session-transition GENERATION for the current chat buffer."
  (setq pilish--session-transition-generation generation))

(defun pilish--begin-session-transition (&optional proc)
  "Invalidate pending session-transition callbacks and return the new generation.
Optional PROC may complete the transition before it becomes the current process."
  (pilish--cancel-inactivity-timer)
  (let ((next (1+ (or pilish--session-transition-generation 0))))
    (pilish--set-session-transition-generation next)
    (setq pilish--session-transition-active t
          pilish--session-transition-process proc)
    next))

(defun pilish--finish-session-transition (generation)
  "Mark session transition GENERATION finished when it is still current."
  (when (= generation pilish--session-transition-generation)
    (setq pilish--session-transition-active nil
          pilish--session-transition-process nil)
    (pilish--reconcile-inactivity-timer)))

(defun pilish--session-transition-active-p (&optional chat-buf)
  "Return non-nil when CHAT-BUF is switching sessions or forking."
  (with-current-buffer (or chat-buf (current-buffer))
    (and pilish--session-transition-active t)))

(defun pilish--session-transition-current-p (chat-buf proc generation)
  "Return non-nil when CHAT-BUF still expects PROC at GENERATION.
This keeps async session-transition callbacks from older switches, forks, or
resets from overwriting the current chat buffer state."
  (and (buffer-live-p chat-buf)
       (with-current-buffer chat-buf
         (and (or (eq pilish--process proc)
                  (eq pilish--session-transition-process proc))
              (= generation pilish--session-transition-generation)))))

(defvar-local pilish--streaming-marker nil
  "Marker for current streaming insertion point.")

(defun pilish--set-streaming-marker (marker)
  "Set the streaming insertion point MARKER."
  (setq pilish--streaming-marker marker))

(defvar-local pilish--in-code-block nil
  "Non-nil when streaming inside a fenced code block.
Used to suppress ATX heading transforms inside code.")

(defvar-local pilish--in-thinking-block nil
  "Non-nil while processing a thinking block for the current message.
Used for lifecycle resets when new messages or turns begin.")

(defvar-local pilish--thinking-marker nil
  "Marker for insertion point inside the current thinking block.
Unlike `pilish--streaming-marker', this marker stays anchored
in thinking text when other content blocks (for example, tool headers)
interleave during streaming.")

(defvar-local pilish--thinking-start-marker nil
  "Marker for the start of the current thinking block.
Used to rewrite thinking content in place after whitespace normalization.")

(defvar-local pilish--thinking-raw nil
  "Accumulated raw thinking deltas for the current thinking block.
Normalized and re-rendered incrementally to avoid excess whitespace.")

(defvar-local pilish--thinking-prev-rendered nil
  "Previously rendered blockquote text for the current thinking block.
Used for incremental rendering: when the new rendered text extends the
previous text, only the suffix is inserted instead of replacing the
entire region.  Reset by `pilish--reset-thinking-state'.")

(defvar-local pilish--line-parse-state 'line-start
  "Parsing state for current line during streaming.
Values:
  `line-start' - at beginning of line, ready for heading or fence
  `fence-1'    - seen one backtick at line start
  `fence-2'    - seen two backticks at line start
  `mid-line'   - somewhere in middle of line

Starts as `line-start' because content begins after separator newline.")

;; pilish--status is defined in pilish-core.el as the single source of truth
;; for session activity state (idle, sending, streaming, compacting)

(defvar-local pilish--activity-phase "idle"
  "Fine-grained activity phase for header-line display.
One of \"thinking\", \"replying\", \"running\",
\"queued\", \"compact\", or \"idle\".
Always populated; normally rendered in a fixed-width slot.")

(defvar-local pilish--inactivity-timer nil
  "Active-session timer invalidating only the linked input header.
Keep refreshing even when the warning option is nil, so reenabling it
while Pi is silent needs neither a setter nor an RPC.")

(defun pilish--inactivity-eligible-p ()
  "Return whether this chat owns a live streaming or compacting process."
  (and (processp pilish--process)
       (process-live-p pilish--process)
       (memq pilish--status '(streaming compacting))
       (not pilish--session-transition-active)))

(defun pilish--cancel-inactivity-timer ()
  "Cancel this chat's header observer without changing its output clock."
  (when pilish--inactivity-timer
    (cancel-timer pilish--inactivity-timer)
    (setq pilish--inactivity-timer nil)))

(defun pilish--reset-inactivity-observation ()
  "Reset observation for an adopted session on this chat's current process.
Cancel the old timer identity; the next state reconciliation may rearm it."
  (pilish--cancel-inactivity-timer)
  (when (processp pilish--process)
    (process-put pilish--process 'pilish-last-output-time (float-time))))

(defun pilish--reconcile-inactivity-timer ()
  "Keep one header refresh timer only while this chat is eligible.
Arming never overwrites the stdout receipt that preceded a state event."
  (if (not (pilish--inactivity-eligible-p))
      (pilish--cancel-inactivity-timer)
    (unless pilish--inactivity-timer
      (let ((chat (current-buffer)) timer)
        (setq timer
              (run-at-time
               1 1
               (lambda ()
                 ;; Replacement cancels before assigning the process, so timer
                 ;; identity covers process ownership.  Retire only our timer
                 ;; if locals were reset, the chat died, or a new owner took over.
                 (if (not (and (buffer-live-p chat)
                               (eq timer (buffer-local-value
                                          'pilish--inactivity-timer chat))))
                     (cancel-timer timer)
                   (with-current-buffer chat
                     (if (not (pilish--inactivity-eligible-p))
                         (pilish--cancel-inactivity-timer)
                       (let ((now (float-time))
                             (last (process-get pilish--process
                                                'pilish-last-output-time)))
                         (when (and last (< now last))
                           (process-put pilish--process 'pilish-last-output-time now)))
                       (when (buffer-live-p pilish--input-buffer)
                         (with-current-buffer pilish--input-buffer
                           (force-mode-line-update))))))))
              pilish--inactivity-timer timer)))))

(defun pilish--inactivity-status (phase)
  "Format PHASE for this chat's input header, without changing any state."
  (let* ((last (and (pilish--inactivity-eligible-p)
                    (process-get pilish--process 'pilish-last-output-time)))
         (age (and last (max 0 (- (float-time) last)))))
    (when (and age pilish-session-inactivity-timeout
               (>= age pilish-session-inactivity-timeout))
      (propertize
       (format "%s (no output %s)" phase
               (if (< age 60) (format "%ds" (floor age))
                 (format "%dm" (floor (/ age 60)))))
       'face 'warning
       'help-echo
       (format (concat "No stdout received from Pi for %d seconds. "
                       "Pi may still be working. Use M-x pilish-abort "
                       "(normally C-c C-k) in this session to stop and "
                       "discard queued continuations.")
               (floor age))))))

(defun pilish--run-activity-phase-functions
    (chat-buf input-buf old-phase new-phase reason)
  "Run activity phase functions for CHAT-BUF and INPUT-BUF.
OLD-PHASE is the previously applied phase.  NEW-PHASE is the phase that
is now applied.  REASON explains why the application happened.  User functions
are isolated so a customization error cannot break rendering or state
transitions."
  (dolist (fn pilish-activity-phase-functions)
    (condition-case-unless-debug err
        (funcall fn chat-buf input-buf old-phase new-phase reason)
      (error
       (display-warning
        'pilish
        (format "Activity phase function %S failed: %s"
                fn (error-message-string err))
        :error)))))

(defun pilish--set-activity-phase (phase &optional reason force)
  "Set activity PHASE for header-line display in current chat buffer.
PHASE should be one of \"thinking\", \"replying\",
\"running\", \"queued\", \"compact\", or \"idle\".  REASON defaults to
`phase-change'.  When FORCE is non-nil, rerun
`pilish-activity-phase-functions' even if PHASE did not change.
Returns non-nil when the phase changed."
  (let ((chat-buf (pilish--get-chat-buffer))
        (reason (or reason 'phase-change)))
    (if (and chat-buf
             (buffer-live-p chat-buf)
             (not (eq chat-buf (current-buffer))))
        (with-current-buffer chat-buf
          (pilish--set-activity-phase phase reason force))
      (let* ((old-phase pilish--activity-phase)
             (changed (not (equal old-phase phase))))
        (when (or changed force)
          (setq pilish--activity-phase phase)
          (when changed
            (force-mode-line-update t))
          (pilish--run-activity-phase-functions
           (current-buffer) pilish--input-buffer old-phase phase reason))
        changed))))

(defvar-local pilish--cached-stats nil
  "Cached session statistics for header-line display.
Updated after each agent turn completes.")

(defvar-local pilish--aborted nil
  "Non-nil while an explicit local stop still owns the current operation.
Retained across compaction and delayed agent_start until settlement, prompt
rejection, no-turn completion, or process exit.")

(defun pilish--set-aborted (value)
  "Set the aborted flag to VALUE."
  (setq pilish--aborted value))

(defvar-local pilish--message-start-marker nil
  "Marker for start of current message content.
Used to replace raw markdown with rendered Org on message completion.")

(defun pilish--set-message-start-marker (marker)
  "Set the message start MARKER."
  (setq pilish--message-start-marker marker))

(defvar-local pilish--tool-args-cache nil
  "Hash table mapping toolCallId to authoritative execution args.
Needed because `tool_execution_end' events do not include args.  This is
per-turn state and is cleared on turn end, history rebuild, and session reset.")

(defvar-local pilish--live-tool-blocks nil
  "Hash table mapping toolCallId to live tool block records.
Concurrent preview and execution lifecycle work is keyed through this
registry so each live block keeps its own output and metadata.")

(defvar-local pilish--tool-block-order-counter 0
  "Monotonic counter used to stamp tool block ordering metadata.")

(defvar-local pilish--thinking-block-order-counter 0
  "Monotonic counter used to stamp completed thinking block metadata.")

(defvar-local pilish--pending-tool-overlay nil
  "Compatibility overlay slot for legacy non-keyed helper paths.
Keyed live block helpers are authoritative for concurrent preview and
execution; this slot remains only for older single-tool flows.")

(defvar-local pilish--assistant-header-shown nil
  "Non-nil if Assistant header has been shown for current prompt.
Used to avoid duplicate headers during retry sequences.")

(cl-defstruct (pilish--prompt-image
               (:constructor pilish--make-prompt-image))
  "Materialized image attached to one input-buffer prompt draft."
  name
  mime-type
  byte-size
  data)

(defvar-local pilish--draft-prompt-image nil
  "Materialized prompt image attached to the current input draft.")

(defun pilish--get-prompt-image (&optional input-buffer)
  "Return the draft prompt image in INPUT-BUFFER or the current buffer."
  (let ((buffer (or input-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (buffer-local-value 'pilish--draft-prompt-image buffer))))

(defun pilish--set-prompt-image (image &optional input-buffer)
  "Set IMAGE as the draft prompt image in INPUT-BUFFER or current buffer."
  (let ((buffer (or input-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq pilish--draft-prompt-image image)
        (force-mode-line-update t)))
    image))

(defun pilish--clear-prompt-image (&optional input-buffer)
  "Clear the draft prompt image in INPUT-BUFFER or the current buffer."
  (pilish--set-prompt-image nil input-buffer))

(defun pilish--prompt-image-content-block (image)
  "Return the RPC image content block for prompt IMAGE."
  (list :type "image"
        :data (pilish--prompt-image-data image)
        :mimeType (pilish--prompt-image-mime-type image)))

(defun pilish--input-draft-nonempty-snapshot (input-buffer)
  "Return a snapshot when INPUT-BUFFER has text or a prompt image.
Whitespace-only text is blank.  Inspect the whole buffer despite any
narrowing, and return nil for a dead buffer or blank image-free draft.
The snapshot is ephemeral: it references the existing materialized
image and neither copies nor consumes draft state."
  (when (buffer-live-p input-buffer)
    (with-current-buffer input-buffer
      (save-restriction
        (widen)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max)))
              (image (pilish--get-prompt-image input-buffer)))
          (when (or image
                    (not (string-empty-p (string-trim text))))
            (list input-buffer text image)))))))

(defun pilish--replace-input-draft (input-buffer text)
  "Replace INPUT-BUFFER's draft with TEXT and clear its prompt image."
  (when (buffer-live-p input-buffer)
    (with-current-buffer input-buffer
      (erase-buffer)
      (when text
        (insert text))
      (pilish--clear-prompt-image)
      (goto-char (point-max)))))

(defvar-local pilish--followup-queue nil
  "List of follow-up messages queued while agent is busy.
Messages are added when the user sends while streaming, compacting, or
waiting for local prompt preflight, run settlement, or automatic retry.  The
oldest message is sent after the session settles, and dropped only after
prompt preflight accepts it.  This is simpler than using pi's RPC follow_up
command.")

(defun pilish--push-followup (message)
  "Push MESSAGE onto the follow-up queue."
  (prog1 (push message pilish--followup-queue)
    (force-mode-line-update t)))

(defun pilish--dequeue-followup ()
  "Dequeue and return the oldest follow-up message, or nil if empty.
Follow-ups are processed in FIFO order: first pushed, first sent."
  (when pilish--followup-queue
    (let ((text (car (last pilish--followup-queue))))
      (setq pilish--followup-queue
            (butlast pilish--followup-queue))
      (force-mode-line-update t)
      text)))

(defun pilish--clear-followup-queue ()
  "Clear all pending follow-up messages."
  (when pilish--followup-queue
    (setq pilish--followup-queue nil)
    (force-mode-line-update t))
  nil)

(defun pilish--followups-in-fifo-order ()
  "Return queued follow-up messages in the order they would be sent."
  (reverse pilish--followup-queue))

(defun pilish--restore-input-text (text &optional prompt-image)
  "Restore TEXT and optional PROMPT-IMAGE to the linked input buffer.
Recovered text is older than any draft currently in the input buffer, so it is
placed first and separated from the draft by a blank line."
  (when-let* ((input-buf pilish--input-buffer)
              ((buffer-live-p input-buf)))
    (with-current-buffer input-buf
      (let ((draft (buffer-string)))
        (erase-buffer)
        (insert text)
        (unless (string-empty-p draft)
          (insert "\n\n" draft))
        (when prompt-image
          (pilish--set-prompt-image prompt-image))
        (goto-char (point-max))))))

(defun pilish--restore-followup-queue-to-input ()
  "Move all queued follow-ups back to the input buffer and clear the queue.
If the linked input buffer is gone, leave the queue intact rather than losing
user text."
  (when-let* (((buffer-live-p pilish--input-buffer))
              ((consp pilish--followup-queue)))
    (let ((text (mapconcat #'identity
                           (pilish--followups-in-fifo-order)
                           "\n\n")))
      (pilish--clear-followup-queue)
      (pilish--restore-input-text text))))

(defun pilish--peek-followup ()
  "Return the oldest queued follow-up message without removing it."
  (car (last pilish--followup-queue)))

(defun pilish--drop-followup (message)
  "Remove MESSAGE when it is the oldest queued follow-up.
Return non-nil when a message was removed.  Follow-ups are acknowledged after
prompt preflight succeeds, so rejected queued prompts remain available."
  (when (and pilish--followup-queue
             (equal (pilish--peek-followup) message))
    (pilish--dequeue-followup)
    t))

(defvar-local pilish--local-user-message nil
  "Locally displayed user turn awaiting pi's authoritative echo.
A string records an existing text-only turn.  An image turn stores its full
normalized content vector so text and image blocks must both match.  Nil means
there is no local echo to suppress.  The value is cleared on message_start;
when the authoritative turn differs, pi's version is also displayed (for
example, after prompt or image transformation).")

(defvar-local pilish--local-user-message-region nil
  "Marker pair bounding the locally displayed user turn awaiting pi's echo.")

(defun pilish--clear-local-user-message-region ()
  "Detach and clear markers for the locally displayed pending user turn."
  (when (consp pilish--local-user-message-region)
    (set-marker (car pilish--local-user-message-region) nil)
    (set-marker (cdr pilish--local-user-message-region) nil))
  (setq pilish--local-user-message-region nil))

(cl-defstruct (pilish--prompt-wait (:constructor pilish--make-prompt-wait))
  "Ownership of one prompt request, separate from observed run activity."
  process accepted started echoed restore-followups disposition)

(defvar-local pilish--prompt-wait nil
  "Current prompt request record, or nil after its ownership is released.
An unacknowledged extension command retains this record even after its run
settles.  Record identity and process identity invalidate stale callbacks.")

(defun pilish--prompt-start-wait-active-p ()
  "Return non-nil when a current prompt request still owns local submission."
  (and pilish--prompt-wait
       (eq (pilish--prompt-wait-process pilish--prompt-wait)
           (pilish--get-process))))

(defun pilish--prompt-local-echo-p ()
  "Return non-nil when acceptance may still display a speculative user turn."
  (not (and pilish--prompt-wait
            (or (member (pilish--prompt-wait-disposition pilish--prompt-wait)
                        '("handled" "queued"))
                (pilish--prompt-wait-started pilish--prompt-wait)
                (pilish--prompt-wait-echoed pilish--prompt-wait)))))

(defun pilish--session-busy-p (&optional chat-buf)
  "Return non-nil when CHAT-BUF has active or locally pending work.
When CHAT-BUF is nil, inspect the current buffer.  This includes Pi-owned
activity from `pilish--status' plus model changes, session
transitions, prompt requests and correlated manual compaction requests."
  (with-current-buffer (or chat-buf (current-buffer))
    (or (memq pilish--status '(sending streaming compacting))
        (pilish--model-change-pending-p)
        (pilish--session-transition-active-p)
        (pilish--prompt-start-wait-active-p)
        (pilish--command-pending-p (pilish--get-process) "compact"))))

(defun pilish--session-steerable-p (&optional chat-buf)
  "Return non-nil when CHAT-BUF has a run that can receive backend steering.
When CHAT-BUF is nil, inspect the current buffer.  An unstarted local prompt
or an announced retry can receive steering for its upcoming run.  A bare
sending status may instead be waiting for settlement after Pi stopped
checking its backend queues."
  (with-current-buffer (or chat-buf (current-buffer))
    (or (eq pilish--status 'streaming)
        (and (eq pilish--status 'sending)
             (or (plist-get pilish--state :is-retrying)
                 (and (pilish--prompt-start-wait-active-p)
                      (not (pilish--prompt-wait-started pilish--prompt-wait))))))))

(defun pilish--recover-followups-after-retry-failure ()
  "Restore unsent follow-ups without recovering an unresolved FIFO owner.
An extension may have started a run before acknowledging its prompt.  Keep
that request's text out of the editor until the correlated response decides
whether to accept or restore it."
  (if (and (pilish--prompt-start-wait-active-p)
           (not (pilish--prompt-wait-accepted pilish--prompt-wait)))
      (setf (pilish--prompt-wait-restore-followups pilish--prompt-wait) t)
    (pilish--restore-followup-queue-to-input)))

(defun pilish--canonical-rerender-safe-p ()
  "Return non-nil when the chat buffer may rebuild from canonical messages.
A locally displayed user prompt awaiting pi's echo is newer than the cached
canonical history, so rebuilding now would erase that visible turn."
  (and (eq pilish--status 'idle)
       (not (pilish--prompt-start-wait-active-p))
       (not (pilish--command-pending-p (pilish--get-process) "compact"))
       (null pilish--local-user-message)))

(defvar-local pilish--extension-status nil
  "Alist of extension status messages for header-line display.
Keys are extension identifiers (strings), values are status text.")

(defvar-local pilish--working-message nil
  "Transient extension working message for header-line display.")

(defvar-local pilish--unsupported-extension-ui-methods-warned nil
  "Unsupported extension UI method names already warned for this pi session.")

(defun pilish--record-unsupported-extension-ui-warning (method)
  "Record an unsupported extension UI warning for METHOD.
Return non-nil when METHOD had not already been warned for this pi session."
  (unless (member method pilish--unsupported-extension-ui-methods-warned)
    (push method pilish--unsupported-extension-ui-methods-warned)
    t))

(defun pilish--clear-unsupported-extension-ui-warnings ()
  "Forget unsupported extension UI warnings for the current pi session."
  (setq pilish--unsupported-extension-ui-methods-warned nil))

(defvar-local pilish--session-name nil
  "Cached session name for header-line display.
Extracted from session_info entries when session is loaded or switched.")

(defvar-local pilish--commands nil
  "List of available commands from pi.
Each entry is a plist with :name, :source, and :description.
Optional :location (\"user\" or \"project\") and :path may be present.
Source is \"prompt\", \"extension\", or \"skill\".")

(defvar pilish--builtin-commands
  '(("compact" :handler pilish-compact       :args optional)
    ("new"     :handler pilish-new-session)
    ("model"   :handler pilish-select-model  :args optional)
    ("session" :handler pilish-session-stats)
    ("name"    :handler pilish-set-session-name :args required)
    ("fork"    :handler pilish-fork)
    ("resume"  :handler pilish-session-browser)
    ("reload"  :handler pilish-reload)
    ("export"  :handler pilish-export-html  :args optional)
    ("copy"    :handler pilish-copy-last-message)
    ("quit"    :handler pilish-quit))
  "Built-in slash commands dispatched client-side.
Each entry is (NAME . PLIST) where PLIST has:
  :handler  Function to call (symbol)
  :args     nil (no args), `optional', or `required'

Commands with :args `optional' pass the trailing text (or nil) to the
handler.  Commands with :args `required' prompt interactively when no
argument is given (the handler's `interactive' spec handles this).
Descriptions come from the handler's docstring.")

(defun pilish--builtin-command-name (text)
  "Return the built-in slash command name in TEXT, or nil."
  (when (and (stringp text)
             (string-prefix-p "/" text))
    (let* ((without-slash (substring text 1))
           (words (split-string without-slash))
           (name (car words)))
      (and (assoc name pilish--builtin-commands)
           name))))

(defun pilish--builtin-command-text-p (text)
  "Return non-nil when TEXT names a client-side built-in command."
  (and (pilish--builtin-command-name text) t))

(defun pilish--commands-by-source (source)
  "Return commands filtered by SOURCE, sorted alphabetically."
  (sort (seq-filter (lambda (c) (equal (plist-get c :source) source))
                    pilish--commands)
        (lambda (a b)
          (string< (plist-get a :name) (plist-get b :name)))))

(defun pilish--set-commands (commands)
  "Set COMMANDS in current buffer and propagate to sibling session buffers.
COMMANDS is a list of plists with :name, :source, and optional
:description, :location, and normalized Emacs :path metadata.
Both chat and input buffers share the same commands list, so this
setter updates all of them to keep them in sync."
  (setq pilish--commands commands)
  (let ((chat-buf (pilish--get-chat-buffer))
        (input-buf (pilish--get-input-buffer)))
    (dolist (buf (list chat-buf input-buf))
      (when (and (buffer-live-p buf)
                 (not (eq buf (current-buffer))))
        (with-current-buffer buf
          (setq pilish--commands commands))))
    ;; The banner summary reads buffer-local commands, so refresh it in the
    ;; chat buffer wherever the RPC callback happened to run.
    (when (buffer-live-p chat-buf)
      (with-current-buffer chat-buf
        (pilish--refresh-startup-banner)))))

;;;; Buffer Navigation

(defun pilish--get-chat-buffer ()
  "Get the chat buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pilish-chat-mode)
      (current-buffer)
    pilish--chat-buffer))

(defun pilish--get-input-buffer ()
  "Get the input buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pilish-input-mode)
      (current-buffer)
    pilish--input-buffer))

(defun pilish--get-process ()
  "Get the pi process for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pilish-chat-mode)
      pilish--process
    (and pilish--chat-buffer
         (buffer-local-value 'pilish--process pilish--chat-buffer))))

(defun pilish--session-live-process-p (proc)
  "Return non-nil when PROC is a live process object."
  (and (processp proc) (process-live-p proc)))

(defun pilish--process-kill-confirmation-required-p (proc)
  "Return non-nil when killing PROC should ask the user first."
  (and (pilish--session-live-process-p proc)
       (not pilish-quit-without-confirmation)
       (not (process-get proc 'pilish-skip-kill-confirmation))))

(defun pilish--skip-process-kill-confirmation (proc)
  "Suppress pi's own kill confirmation for PROC during intentional teardown."
  (when (processp proc)
    (process-put proc 'pilish-skip-kill-confirmation t)))

(defun pilish--session-kill-buffer-query ()
  "Ask before killing a session buffer would terminate a live pi process."
  (let ((proc (pilish--get-process)))
    (or (not (pilish--process-kill-confirmation-required-p proc))
        (yes-or-no-p "Pi session has a running process; kill it? "))))

(defun pilish--session-kill-emacs-query ()
  "Ask before exiting Emacs terminates a live pi session process.
Session processes are started with `:noquery', so Emacs' own exit query
does not see them; this function replaces it with pi's confirmation.
Return nil to abort the exit."
  (or (not (cl-some (lambda (proc)
                      (and (process-get proc 'pilish-chat-buffer)
                           (pilish--process-kill-confirmation-required-p
                            proc)))
                    (process-list)))
      (yes-or-no-p "Pi session has a running process; exit anyway? ")))

;; Closing the last frame kills Emacs without killing session buffers, so
;; `kill-buffer-query-functions' never runs.  Guard that path explicitly.
(add-hook 'kill-emacs-query-functions
          #'pilish--session-kill-emacs-query)

(defun pilish--retarget-session-buffers (dir)
  "Retarget the current chat/input session buffers to DIR."
  (let* ((chat-buf (pilish--get-chat-buffer))
         (session (and (buffer-live-p chat-buf)
                       (pilish--chat-session-name chat-buf)))
         (input-buf (and (buffer-live-p chat-buf)
                         (buffer-local-value 'pilish--input-buffer
                                             chat-buf)))
         (existing (pilish--find-session dir session)))
    (unless (buffer-live-p chat-buf)
      (user-error "No pi session buffer"))
    (when (and existing (not (eq existing chat-buf)))
      (user-error "Pi session already open for: %s" dir))
    (with-current-buffer chat-buf
      (pilish--set-chat-session-identity dir session)
      (rename-buffer pilish--canonical-buffer-name))
    (when (buffer-live-p input-buf)
      (with-current-buffer input-buf
        (setq default-directory dir)
        (rename-buffer (pilish--buffer-name :input dir session))
        (pilish--set-chat-buffer chat-buf)))))

;;;; Display

(defun pilish--window-can-split-for-input-p (window)
  "Return non-nil if WINDOW can be split into chat and input windows."
  (>= (window-total-height window)
      (* 2 window-min-height)))

(defun pilish--input-height-for-window-height (total)
  "Compute input pane height for a container of TOTAL lines.
When `pilish-input-window-height' is an integer, use it directly.
When it is a float, compute the height as that fraction of TOTAL.
In both cases, clamp the result to the range
\[`window-min-height', TOTAL - `window-min-height']."
  (let* ((max-input-height (- total window-min-height))
         (raw (if (floatp pilish-input-window-height)
                  (round (* pilish-input-window-height total))
                pilish-input-window-height)))
    (max window-min-height
         (min raw max-input-height))))

(defun pilish--input-height-for-window (window)
  "Return input pane height to use when splitting WINDOW."
  (pilish--input-height-for-window-height
   (window-total-height window)))

(defun pilish--rebalance-input-window (chat-win input-win)
  "Adjust INPUT-WIN height to match the configured ratio.
CHAT-WIN and INPUT-WIN must be a vertically stacked pair.
Only resizes when `pilish-input-window-height' is a float."
  (when (and (floatp pilish-input-window-height)
             (window-live-p chat-win)
             (window-live-p input-win))
    (let* ((total (+ (window-total-height chat-win)
                     (window-total-height input-win)))
           (target (pilish--input-height-for-window-height total))
           (current (window-total-height input-win))
           (delta (- target current)))
      (unless (zerop delta)
        (window-resize input-win delta nil t)))))

(defun pilish--maybe-rebalance-windows (_frame)
  "Rebalance pi chat/input window pairs after a frame size change.
Intended for `window-size-change-functions'."
  (when (floatp pilish-input-window-height)
    (dolist (win (window-list nil 'no-mini))
      (when-let* ((input-buf (buffer-local-value
                              'pilish--input-buffer
                              (window-buffer win)))
                  (input-win (get-buffer-window input-buf)))
        (unless (eq win input-win)
          (pilish--rebalance-input-window win input-win))))))

(defun pilish--windows-by-height (&optional windows)
  "Return live WINDOWS sorted by descending height.
If WINDOWS is nil, use all non-minibuffer windows in the selected frame."
  (sort (cl-remove-if-not #'window-live-p
                          (copy-sequence (or windows (window-list nil 'no-mini))))
        (lambda (a b)
          (> (window-total-height a)
             (window-total-height b)))))

(defun pilish--window-with-most-height (&optional windows)
  "Return the tallest window from WINDOWS.
If WINDOWS is nil, use all non-minibuffer windows in the selected frame."
  (car (pilish--windows-by-height windows)))

(defun pilish--best-display-window (&optional preferred)
  "Return best window for displaying chat+input.
Use PREFERRED when it can be split, else pick the tallest splittable
window in the frame.  Falls back to PREFERRED or selected window."
  (or (and preferred
           (window-live-p preferred)
           (pilish--window-can-split-for-input-p preferred)
           preferred)
      (cl-find-if #'pilish--window-can-split-for-input-p
                  (pilish--windows-by-height))
      preferred
      (selected-window)))

(defun pilish--preferred-display-window (chat-wins input-wins selected)
  "Return preferred base window for displaying chat+input.
CHAT-WINS and INPUT-WINS are existing session windows.  SELECTED is the
currently selected window."
  (cond
   ;; Input-only visible: prefer selected non-input window so we can
   ;; replace it cleanly and avoid duplicate input windows.
   ((and input-wins (not chat-wins)
         (not (memq selected input-wins))
         (pilish--window-can-split-for-input-p selected))
    selected)
   (chat-wins (pilish--window-with-most-height chat-wins))
   (input-wins (pilish--window-with-most-height input-wins))
   (t selected)))

(defun pilish--delete-extra-input-windows (input-wins target)
  "Delete windows in INPUT-WINS except TARGET."
  (dolist (win input-wins)
    (unless (eq win target)
      (when (window-deletable-p win)
        (delete-window win)))))

(defun pilish--paired-input-window (chat-win input-buf)
  "Return input window below CHAT-WIN showing INPUT-BUF, or nil."
  (when (window-live-p chat-win)
    (let ((below (window-in-direction 'below chat-win)))
      (and below
           (eq (window-buffer below) input-buf)
           below))))

(defun pilish--best-input-window (chat-buf input-buf)
  "Return best visible window for INPUT-BUF in current frame, or nil.
Prefer the input window below the selected CHAT-BUF window, then the
selected input window, then the tallest input window."
  (when-let* ((input-wins (get-buffer-window-list input-buf nil)))
    (let* ((selected (selected-window))
           (selected-chat-win (and (eq (window-buffer selected) chat-buf)
                                   selected)))
      (or (pilish--paired-input-window selected-chat-win input-buf)
          (and (memq selected input-wins)
               selected)
          (pilish--window-with-most-height input-wins)))))

(defun pilish--focus-input-window (chat-buf input-buf)
  "Select a visible INPUT-BUF window for the CHAT-BUF session."
  (when-let* ((win (pilish--best-input-window chat-buf input-buf)))
    (select-window win)))

(defun pilish--split-input-below-chat (chat-buf input-buf)
  "Show INPUT-BUF in a new window below the best visible CHAT-BUF window.
The new window is soft-dedicated so `display-buffer' never targets it.
Return the new input window, or nil when no chat window can be split."
  (when-let* ((chat-win
               (or (and (eq (window-buffer (selected-window)) chat-buf)
                        (pilish--window-can-split-for-input-p
                         (selected-window))
                        (selected-window))
                   (cl-find-if #'pilish--window-can-split-for-input-p
                               (pilish--windows-by-height
                                (get-buffer-window-list chat-buf nil))))))
    (let ((input-win
           (split-window
            chat-win (- (pilish--input-height-for-window chat-win))
            'below)))
      (set-window-buffer input-win input-buf)
      ;; Soft-dedicate the input window so `display-buffer' never
      ;; targets it (magit, help, compilation, etc.).  The 'side
      ;; value still allows `switch-to-buffer' and `C-x o'.
      (set-window-dedicated-p input-win 'side)
      input-win)))

(defun pilish-open-input ()
  "Open the input window below the chat window and select it.
If an input window is already visible, select it instead.  If no chat
window is visible either, restore the full session layout."
  (interactive)
  (let* ((chat-buf (pilish--get-chat-buffer))
         (input-buf (pilish--get-input-buffer)))
    (unless (and (buffer-live-p chat-buf) (buffer-live-p input-buf))
      (user-error "No pi session for this buffer"))
    (cond
     ((pilish--focus-input-window chat-buf input-buf))
     ((when-let* ((input-win
                   (pilish--split-input-below-chat chat-buf input-buf)))
        (select-window input-win)))
     (t (pilish--display-buffers chat-buf input-buf)))))

(defun pilish--input-window-on-demand-p ()
  "Return non-nil when the input window is shown on demand.
True for the `on-demand' and `hidden' values of
`pilish-input-window-display', which both hide the input
after each send and reopen it with `pilish-open-input'."
  (memq pilish-input-window-display '(on-demand hidden)))

(defun pilish--maybe-hide-input-window ()
  "Hide the input window when it is shown on demand.
Intended to run after `pilish-send' accepts input.  When the
selected window is deleted, select a chat window instead."
  (when (pilish--input-window-on-demand-p)
    (let* ((input-buf (pilish--get-input-buffer))
           (input-wins (and (buffer-live-p input-buf)
                            (get-buffer-window-list input-buf nil)))
           (selected (selected-window)))
      (dolist (win input-wins)
        (when (window-parent win)
          (delete-window win)))
      (when (and (memq selected input-wins)
                 (not (window-live-p selected)))
        (when-let* ((chat-buf (pilish--get-chat-buffer))
                    (chat-win (get-buffer-window chat-buf)))
          (select-window chat-win))))))

(defun pilish--display-buffers (chat-buf input-buf &optional chat-only)
  "Ensure CHAT-BUF and INPUT-BUF are visible.
When CHAT-ONLY is non-nil, show only the chat window.
Uses a split window with chat above and input below.  Falls back to a
larger window when the selected one cannot be split."
  (let* ((chat-wins (get-buffer-window-list chat-buf nil))
         (input-wins (get-buffer-window-list input-buf nil))
         (selected (selected-window))
         (preferred (pilish--preferred-display-window
                     chat-wins input-wins selected))
         (target (pilish--best-display-window preferred))
         (input-win nil))
    ;; Remove stale input windows when restoring from an input-only view.
    (when (and input-wins (not chat-wins))
      (pilish--delete-extra-input-windows input-wins target))
    (with-selected-window target
      (unless chat-only
        (unless (pilish--window-can-split-for-input-p target)
          (delete-other-windows target))
        (unless (pilish--window-can-split-for-input-p target)
          (user-error "Window too small for chat + input layout")))
      (switch-to-buffer chat-buf)
      (with-current-buffer chat-buf
        (goto-char (point-max)))
      (unless chat-only
        (setq input-win
              (pilish--split-input-below-chat chat-buf input-buf))))
    (when (window-live-p input-win)
      (select-window input-win))))

;;; Scroll Behavior
;;
;; During streaming, windows "following" output (window-point at buffer end)
;; scroll to show new content. Windows where the user scrolled up stay put.
;;
;; Key mechanism: `window-point-insertion-type' is set to t in pilish-chat-mode,
;; making window-point move with inserted text. We track which windows are
;; following before each insert, then restore point for non-following windows
;; afterward. Emacs naturally scrolls to keep point visible.

(defun pilish--window-following-p (window)
  "Return non-nil if WINDOW is following output (point at end of buffer)."
  (>= (window-point window) (1- (point-max))))

(defmacro pilish--with-scroll-preservation (&rest body)
  "Execute BODY preserving scroll for windows not following output.
Windows at buffer end will scroll to show new content.
Windows where user scrolled up stay in place.
Valid for append-only inserts; for mid-buffer rewrites like tool cooling, see
`pilish--capture-tool-cooling-view' in pilish-render.el."
  (declare (indent 0) (debug t))
  `(let* ((windows (get-buffer-window-list (current-buffer) nil t))
          (following (cl-remove-if-not #'pilish--window-following-p windows))
          (saved-points (mapcar (lambda (w) (cons w (window-point w)))
                                (cl-remove-if #'pilish--window-following-p windows))))
     ,@body
     ;; Restore point for non-following windows
     (dolist (pair saved-points)
       (when (window-live-p (car pair))
         (set-window-point (car pair) (cdr pair))))
     ;; Move following windows to new end
     (dolist (win following)
       (when (window-live-p win)
         (set-window-point win (point-max))))))

(defun pilish--append-to-chat (text)
  "Append TEXT to the chat buffer.
Windows following the output (point at end) will scroll to show new text.
Windows where user scrolled up (point earlier) stay in place."
  (let ((inhibit-read-only t))
    (pilish--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        (insert text)))))

(defun pilish--make-separator (label &optional timestamp)
  "Create a setext-style H1 heading separator with LABEL.
If TIMESTAMP (Emacs time value) is provided, append it after \" · \".
Returns a markdown setext heading: label line followed by === underline.
Fontification is handled by `md-ts-mode'.

Using setext headings enables outline/imenu navigation and keeps our
turn markers as H1 while LLM ATX headings are leveled down to H2+."
  (let* ((timestamp-str (when timestamp
                          (pilish--format-message-timestamp timestamp)))
         (header-line (if timestamp-str
                          (concat label " · " timestamp-str)
                        label))
         ;; Underline must be at least 3 chars, and at least as long as header
         (underline-len (max 3 (length header-line)))
         (underline (make-string underline-len ?=)))
    (concat header-line "\n" underline "\n")))

;;;; Formatting Utilities

(defun pilish--format-number (n)
  "Format number N with thousands separators."
  (let ((str (number-to-string n)))
    (replace-regexp-in-string
     "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
     "\\1,\\2\\3"
     (replace-regexp-in-string
      "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
      "\\1,\\2,\\3\\4" str))))

(defun pilish--truncate-string (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "…")
    str))

(defun pilish--ms-to-time (ms)
  "Convert milliseconds MS to Emacs time value.
Returns nil if MS is nil."
  (and ms (seconds-to-time (/ ms 1000.0))))

(defun pilish--format-message-timestamp (time)
  "Format TIME for message headers as YYYY-MM-DD HH:MM."
  (format-time-string "%Y-%m-%d %H:%M" time))

;;;; Dependency Checking

(defconst pilish--pi-package "@earendil-works/pi-coding-agent"
  "Npm package name for the pi CLI supported by Pilish.")

(defconst pilish--minimum-pi-version "1.0.0"
  "Minimum supported pi CLI version.")

(defun pilish--pi-install-command ()
  "Return the npm command to install the supported pi CLI."
  (format "npm install -g %s" pilish--pi-package))

(defun pilish--dependency-directory (&optional directory)
  "Return the directory where process dependencies should be checked.
Use DIRECTORY when non-nil.  In pi buffers, prefer the active session
directory; otherwise use `default-directory'."
  (or directory
      (if (derived-mode-p 'pilish-chat-mode
                          'pilish-input-mode)
          (pilish--session-directory)
        default-directory)))

(defun pilish--multi-hop-remote-prefix-p (prefix)
  "Return non-nil when PREFIX is a TRAMP multi-hop route."
  (and (stringp prefix)
       (string-search "|" prefix)))

(defun pilish--remote-exec-path-directory
    (entry directory remote-prefix)
  "Return ENTRY from the function `exec-path' under remote DIRECTORY.
REMOTE-PREFIX is DIRECTORY's full TRAMP prefix.  Nil and empty entries mean the
remote DIRECTORY itself.  Process-local absolute entries are re-prefixed with
REMOTE-PREFIX so multi-hop routes are not collapsed by generic file helpers."
  (cond
   ((or (null entry) (equal entry ""))
    (pilish--route-preserving-file-name-as-directory directory))
   ((not (stringp entry))
    nil)
   ((pilish--remote-prefix-for-path entry)
    (when (equal (pilish--remote-prefix-for-path entry)
                 remote-prefix)
      (pilish--route-preserving-file-name-as-directory entry)))
   ((file-name-absolute-p entry)
    (pilish--route-preserving-file-name-as-directory
     (concat remote-prefix entry)))
   (t
    (pilish--route-preserving-file-name-as-directory
     (pilish--route-preserving-expand-file-name entry directory)))))

(defun pilish--remote-executable-path
    (program directory remote-prefix)
  "Return PROGRAM as an Emacs path in remote DIRECTORY.
REMOTE-PREFIX is DIRECTORY's full TRAMP prefix."
  (cond
   ((pilish--remote-prefix-for-path program)
    (and (equal (pilish--remote-prefix-for-path program)
                remote-prefix)
         program))
   ((file-name-absolute-p program)
    (concat remote-prefix program))
   (t
    (pilish--route-preserving-expand-file-name program directory))))

(defun pilish--remote-executable-file-p (path)
  "Return non-nil when remote PATH names an executable file.
This intentionally uses ordinary file predicates so TRAMP performs real I/O in
normal operation; tests should stub this predicate or `file-executable-p' for
fake hosts."
  (ignore-errors (file-executable-p path)))

(defun pilish--remote-executable-find (program directory)
  "Find PROGRAM on remote DIRECTORY while preserving its full TRAMP route.
This is a focused replacement for `executable-find' on multi-hop remotes,
where generic file-name operations can collapse `/ssh:bastion|sudo:host:' to
`/sudo:host:'.  It binds `default-directory' to DIRECTORY, asks the function
`exec-path' for the process-local remote PATH, and returns the first executable
candidate re-prefixed with DIRECTORY's full TRAMP route."
  (let* ((remote-prefix (pilish--remote-prefix directory))
         (directory (pilish--route-preserving-file-name-as-directory
                     (pilish--route-preserving-expand-file-name
                      directory)))
         (path-entries (let ((default-directory directory))
                         (exec-path)))
         (suffixes (or exec-suffixes '(""))))
    (when (and (stringp program)
               (not (string-empty-p program))
               remote-prefix)
      (catch 'found
        (if (string-search "/" program)
            (dolist (suffix suffixes)
              (when-let* ((candidate
                           (pilish--remote-executable-path
                            (concat program suffix)
                            directory remote-prefix))
                          ((pilish--remote-executable-file-p
                            candidate)))
                (throw 'found candidate)))
          (dolist (entry path-entries)
            (when-let* ((dir (pilish--remote-exec-path-directory
                              entry directory remote-prefix)))
              (dolist (suffix suffixes)
                (let ((candidate
                       (pilish--route-preserving-expand-file-name
                        (concat program suffix) dir)))
                  (when (pilish--remote-executable-file-p candidate)
                    (throw 'found candidate)))))))))))

(defun pilish--check-pi (&optional directory)
  "Check if pi binary is available in DIRECTORY's execution context.
Bind `default-directory' to DIRECTORY and use that execution context.  For
multi-hop remote directories, ask the function `exec-path' for remote PATH
entries and re-prefix candidates; otherwise delegate to `executable-find'.
Returns t if available, nil otherwise."
  (let* ((directory (pilish--dependency-directory directory))
         (default-directory directory)
         (program (car pilish-executable))
         (remote-prefix (pilish--remote-prefix directory)))
    (and program
         (if (pilish--multi-hop-remote-prefix-p remote-prefix)
             (pilish--remote-executable-find program directory)
           (executable-find program t))
         t)))

(defun pilish--check-dependencies (&optional directory)
  "Check all required dependencies.
When DIRECTORY is non-nil, perform process dependency checks there.  Displays
warnings for missing dependencies."
  (let ((directory (pilish--dependency-directory directory)))
    (unless (pilish--check-pi directory)
      (display-warning 'pi (format "%s not found in %s. Install with: %s"
                                   (car pilish-executable)
                                   (if-let* ((remote-prefix (pilish--remote-prefix directory)))
                                       (format "remote PATH (%s)" remote-prefix)
                                     "PATH")
                                   (pilish--pi-install-command))
                       :error)))
  (pilish--maybe-install-essential-grammars)
  (pilish--maybe-warn-incompatible-markdown-grammar)
  (pilish--maybe-install-optional-grammars))

;;;; Startup Header

(defconst pilish-version "3.2.1"
  "Version of Pilish.")

(defconst pilish--version-probe-delay 0.1
  "Seconds to wait before probing `pi --version' for a new process.")

(defun pilish--extract-pi-version (output)
  "Extract a standalone semantic pi version from OUTPUT, or nil."
  (when (stringp output)
    (catch 'version
      (dolist (line (split-string output "[\r\n]+" t))
        (let ((trimmed (string-trim line)))
          (when (string-match
                 "\\`v?\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\'"
                 trimmed)
            (throw 'version (match-string 1 trimmed))))))))

(defun pilish--pi-version-outdated-p (version)
  "Return non-nil when VERSION is older than supported pi."
  (and (stringp version)
       (condition-case nil
           (version< version pilish--minimum-pi-version)
         (error nil))))

(defun pilish--warn-if-pi-version-outdated (version)
  "Warn when VERSION is older than `pilish--minimum-pi-version'."
  (when (pilish--pi-version-outdated-p version)
    (display-warning
     'pi
     (format "Pilish requires Pi %s or later; found Pi %s. Upgrade with: %s"
             pilish--minimum-pi-version
             version
             (pilish--pi-install-command))
     :warning)))

(defun pilish--finish-pi-version-process (proc)
  "Collect `pi --version' output from PROC and invoke its callback."
  (let ((callback (process-get proc 'pilish-version-callback))
        (stdout-buf (process-get proc 'pilish-version-stdout-buf))
        (stderr-buf (process-get proc 'pilish-version-stderr-buf)))
    (unwind-protect
        (let* ((stdout (when (buffer-live-p stdout-buf)
                         (with-current-buffer stdout-buf
                           (buffer-string))))
               (stderr (when (buffer-live-p stderr-buf)
                         (with-current-buffer stderr-buf
                           (buffer-string))))
               (output (concat (or stdout "") "\n" (or stderr ""))))
          (when callback
            (funcall callback (pilish--extract-pi-version output))))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf)))))

(defun pilish--run-pi-version-once-async (callback &optional directory)
  "Run `pi --version' asynchronously and call CALLBACK with version or nil.
Run in DIRECTORY, defaulting to `default-directory'.  Process creation uses
`default-directory' file handlers when present, with separate stdout and stderr
buffers."
  (let ((stdout-buf (generate-new-buffer " *pilish-version-stdout*"))
        (stderr-buf (generate-new-buffer " *pilish-version-stderr*"))
        (directory (or directory default-directory)))
    (condition-case nil
        (let* ((default-directory directory)
               (proc (make-process
                      :name "pi-version"
                      :command `(,@pilish-executable "--version")
                      :connection-type 'pipe
                      :file-handler t
                      :buffer stdout-buf
                      :stderr stderr-buf
                      :noquery t
                      :sentinel
                      (lambda (proc _event)
                        (when (memq (process-status proc) '(exit signal))
                          (pilish--finish-pi-version-process proc))))))
          (process-put proc 'pilish-version-callback callback)
          (process-put proc 'pilish-version-stdout-buf stdout-buf)
          (process-put proc 'pilish-version-stderr-buf stderr-buf)
          proc)
      (error
       (when (buffer-live-p stdout-buf)
         (kill-buffer stdout-buf))
       (when (buffer-live-p stderr-buf)
         (kill-buffer stderr-buf))
       (funcall callback nil)))))

(defun pilish--request-pi-version-async (callback)
  "Resolve pi CLI version asynchronously and call CALLBACK with string or nil."
  (let ((directory default-directory))
    (run-at-time pilish--version-probe-delay nil
                 #'pilish--run-pi-version-once-async
                 callback directory)))

(defun pilish--probe-process-version-async (chat-buf)
  "Probe and cache CLI version for CHAT-BUF's process.
Stores the result in CHAT-BUF and emits a minibuffer notice when available."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (let ((default-directory (pilish--chat-session-directory chat-buf)))
        (pilish--request-pi-version-async
         (lambda (version)
           (when (and version (buffer-live-p chat-buf))
             (with-current-buffer chat-buf
               (setq pilish--process-version version)
               (message "Pi: version %s" version)
               (pilish--warn-if-pi-version-outdated version)
               ;; The banner summary shows the probed version; the callback
               ;; already re-established the chat-buffer context above.
               (pilish--refresh-startup-banner)))))))))

;;;; Startup Logo

(defconst pilish--logo-file
  (expand-file-name
   "assets/pilish-logo.svg"
   (file-name-directory
    (or load-file-name
        (symbol-file 'pilish--make-separator 'defun)
        (locate-library "pilish-ui")
        "pilish-ui.el")))
  "Absolute path of the canonical Hornbridge logo SVG shipped with Pilish.
Resolved from the installed pilish-ui library location, never from
`default-directory', so the logo also works from package installs,
byte-compiled load paths, and TRAMP chat buffers.")

(defvar pilish--logo-svg-cache nil
  "Cached adapted SVG source for the startup logo, or nil when unread.
See `pilish--logo-svg-data'.")

(defun pilish--logo-svg-data ()
  "Return adapted SVG source text for the startup logo, or nil.
Reads `pilish--logo-file' and adapts it in memory: the dark backplate
rectangle is dropped and the three body path fills become `currentColor'
so the logo body tracks the heading face foreground on every theme.
The violet horn fills and all path geometry stay identical to the
shipped asset, which remains the single canonical source.  Returns nil
when the asset cannot be read; callers then omit the logo silently."
  (or pilish--logo-svg-cache
      (setq pilish--logo-svg-cache
            (ignore-errors
              (with-temp-buffer
                (insert-file-contents pilish--logo-file)
                (goto-char (point-min))
                (when (re-search-forward
                       "[ \t]*<rect[^>]*fill=\"#07100F\"[^>]*/>[ \t]*\n?" nil t)
                  (replace-match ""))
                (goto-char (point-min))
                (while (search-forward "#EEF2E8" nil t)
                  (replace-match "currentColor" t t))
                (buffer-string))))))

(defun pilish--startup-logo-displayable-p ()
  "Return non-nil when the display being drawn can show the startup logo.
Used inside conditional display specifications, where redisplay
evaluates it for the frame actually being drawn.  Both image-capable
display and SVG support are required: batch and terminal Emacs can
report SVG available without being able to display any image."
  (and (display-images-p) (image-type-available-p 'svg)))

(defun pilish--startup-logo-line-prefix ()
  "Return the display-only prefix for the startup heading line.
Contains the logo image glyph and one font-relative gap glyph, both
carrying the `md-ts-heading-1' face so the body fill (currentColor)
matches the heading.  Each glyph's display value is a list pairing its
real spec, wrapped in `(when (pilish--startup-logo-displayable-p)
...)', with a zero-width space fallback: text terminals and graphical
displays without SVG render no logo and no gap, while the same buffer
shows the logo on image-capable frames.  The glyphs never enter the
buffer text or the kill ring."
  (when-let* ((data (pilish--logo-svg-data)))
    (let* ((image (list 'image :type 'svg :data data
                        :height '(1.5 . em) :scale 1 :ascent 'center))
           (conditional
            (lambda (spec)
              (list (cons 'when
                          (cons '(pilish--startup-logo-displayable-p) spec))
                    '(space . (:width 0))))))
      (propertize
       (concat (propertize " " 'display (funcall conditional image))
               (propertize " " 'display
                           (funcall conditional '(space . (:width 0.75)))))
       'face 'md-ts-heading-1))))

(defun pilish--startup-heading-with-logo (separator)
  "Return SEPARATOR with the startup logo prefix on its heading line.
The label line (through its newline) alone receives the display-only
`line-prefix'; the returned string content and the setext underline
stay unchanged.  Returns SEPARATOR as-is when the logo is unavailable."
  (let* ((newline (string-match "\n" separator))
         (prefix (and newline (pilish--startup-logo-line-prefix))))
    (when prefix
      (put-text-property 0 (1+ newline) 'line-prefix prefix separator))
    separator))

(defun pilish--format-startup-header ()
  "Format the startup header string with separator and session summary.
The heading line carries the Hornbridge logo as a display-only prefix
on image-capable displays.  Ends with the compact, toggleable banner
line built by `pilish--format-startup-banner-compact'."
  (let ((separator (pilish--startup-heading-with-logo
                    (pilish--make-separator "Pilish"))))
    (concat
     separator "\n"
     "C-c C-c   send prompt\n"
     "C-c C-k   abort\n"
     "C-c C-r   sessions\n"
     "C-c C-p   menu\n\n"
     (pilish--format-startup-banner-compact) "\n")))

(defun pilish--display-startup-header ()
  "Display the startup header in the chat buffer."
  (pilish--append-to-chat (pilish--format-startup-header)))

;;;; Startup Banner

(defconst pilish--startup-context-candidates
  '("AGENTS.override.md" "AGENTS.md" "AGENTS.MD" "CLAUDE.md" "CLAUDE.MD")
  "Context file names pi loads, in per-directory precedence order.
AGENTS.override.md shadows AGENTS.md within the same directory.")

(defun pilish--startup-context-file-in-dir (directory)
  "Return the first existing context file path in DIRECTORY, or nil.
Candidates from `pilish--startup-context-candidates' are checked in order
and only regular files count."
  (catch 'found
    (dolist (name pilish--startup-context-candidates nil)
      (let ((path (expand-file-name name directory)))
        (when (and (file-exists-p path) (file-regular-p path))
          (throw 'found path))))))

(defun pilish--startup-context-files (&optional directory user-agent-dir)
  "Return existing AGENTS-family context files pi would load.
The user-level file from USER-AGENT-DIR (default \"~/.pi/agent\", expanded)
comes first when present, then one file per ancestor of DIRECTORY walking up
to the root, nearest directory first.  DIRECTORY defaults to the chat
buffer's session directory.  Only the first existing candidate is returned
per directory."
  (let* ((user-dir (expand-file-name (or user-agent-dir "~/.pi/agent")))
         (start-dir (expand-file-name
                     (or directory (pilish--chat-session-directory))))
         (files (when-let* ((user-file
                             (pilish--startup-context-file-in-dir user-dir)))
                  (list user-file)))
         (dir (directory-file-name start-dir)))
    (catch 'done
      (while t
        (when-let* ((file (pilish--startup-context-file-in-dir dir)))
          (unless (member file files)
            (setq files (append files (list file)))))
        (let ((parent (directory-file-name (file-name-directory dir))))
          (when (string= parent dir)
            (throw 'done nil))
          (setq dir parent))))
    files))

(defun pilish--startup-banner-version-segments ()
  "Return version segments for the startup banner summary.
Includes \"pi V\" only when `pilish--process-version' is set and always
includes \"pilish V\" from `pilish-version'."
  (delq nil
        (list (when pilish--process-version
                (concat "pi v" pilish--process-version))
              (concat "pilish " pilish-version))))

(defun pilish--propertize-startup-banner (text state)
  "Return TEXT tagged as a startup banner shown in display STATE.
STATE is `compact' or `expanded'; the `pilish-startup-banner' text property
marks the toggleable span and records its current display, mirroring how
completed thinking blocks tag their spans."
  (propertize text
              'pilish-startup-banner state
              'help-echo "TAB: toggle session details"))

(defun pilish--format-startup-banner-compact ()
  "Return the propertized compact startup banner summary line.
Count segments are omitted entirely while `pilish--commands' is unset, so a
session that has not loaded commands yet never shows misleading zero
counts."
  (let ((segments (pilish--startup-banner-version-segments)))
    (when pilish--commands
      (setq segments
            (append segments
                    (list (format "%d skills"
                                  (length (pilish--commands-by-source "skill")))
                          (format "%d prompts"
                                  (length (pilish--commands-by-source "prompt")))))))
    (pilish--propertize-startup-banner
     (concat (mapconcat #'identity segments " · ") " · TAB details")
     'compact)))

(defun pilish--startup-banner-region ()
  "Return the startup banner span in the current buffer as (START . END).
Returns nil when the buffer contains no banner."
  (let ((pos (point-min)))
    (catch 'found
      (while (< pos (point-max))
        (if (get-text-property pos 'pilish-startup-banner)
            (throw 'found
                   (cons pos
                         (next-single-property-change
                          pos 'pilish-startup-banner nil (point-max))))
          (setq pos (next-single-property-change
                     pos 'pilish-startup-banner nil (point-max)))))
      nil)))

(defun pilish--refresh-startup-banner ()
  "Re-render the compact startup banner in place from current buffer state.
No-op when the buffer has no banner or the banner is expanded, so a
background refresh never collapses details the user opened.  Idempotent."
  (when-let* ((region (pilish--startup-banner-region))
              (state (get-text-property (car region)
                                        'pilish-startup-banner)))
    (when (eq state 'compact)
      (let ((inhibit-read-only t)
            (start (car region))
            (end (cdr region)))
        (pilish--with-scroll-preservation
          (save-excursion
            (goto-char start)
            (delete-region start end)
            (insert (pilish--format-startup-banner-compact))
            (condition-case-unless-debug nil
                (font-lock-ensure start (point))
              (error nil))))))))

;;;; Header Line

(defun pilish--format-tokens-compact (n)
  "Format token count N compactly (e.g., 50k, 1.2M)."
  (cond
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.0fk" (/ n 1000.0)))
   (t (number-to-string n))))

(defun pilish--shorten-model-name (name)
  "Shorten model NAME for display.
Removes common prefixes like \"Claude \" and suffixes like \" (latest)\"."
  (thread-last name
    (replace-regexp-in-string "^[Cc]laude " "")
    (replace-regexp-in-string " (latest)$" "")
    (replace-regexp-in-string "^claude-" "")))

;;; Header-Line Formatting

(defvar pilish--header-model-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'pilish-select-model)
    (define-key map [header-line mouse-2] #'pilish-select-model)
    map)
  "Keymap for clicking model name in header-line.")

(defvar pilish--header-thinking-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'pilish-cycle-thinking)
    (define-key map [header-line mouse-2] #'pilish-cycle-thinking)
    map)
  "Keymap for clicking thinking level in header-line.")

(defun pilish--header-format-context (percent context-window)
  "Format context usage for header-line display.
PERCENT is context usage (0–100), CONTEXT-WINDOW is the max tokens.
When PERCENT is nil, usage is unknown and rendered as \"?\".
Returns nil if CONTEXT-WINDOW is 0."
  (when (> context-window 0)
    (if (null percent)
        (format " ?/%s" (pilish--format-tokens-compact context-window))
      (let ((pct-str (pilish--header-escape-text
                      (format " %.1f%%/%s" percent
                              (pilish--format-tokens-compact context-window)))))
        (propertize pct-str
                    'face (cond
                           ((> percent pilish-context-error-threshold) 'error)
                           ((> percent pilish-context-warning-threshold) 'warning)
                           (t nil)))))))

(defun pilish--header-format-stats (stats)
  "Format compact header stats from STATS.
Shows cumulative session cost and server-provided context percentage.
Returns nil if STATS is nil."
  (when stats
    (let* ((cost (or (plist-get stats :cost) 0))
           (ctx (plist-get stats :contextUsage))
           (raw-tokens (and ctx (plist-get ctx :tokens)))
           (percent (if (or (null raw-tokens)
                            (pilish--json-null-p raw-tokens))
                        nil
                      (plist-get ctx :percent)))
           (context-window (or (and ctx (plist-get ctx :contextWindow)) 0)))
      (concat
       " │"
       (format " $%.2f" cost)
       (pilish--header-format-context percent context-window)))))

(defun pilish--queue-message-preview (message)
  "Return a bounded, single-line plain-text preview of MESSAGE."
  ;; Bound processing as well as the result, even for very large prompts.
  (let* ((prefix (substring-no-properties message 0 (min 512 (length message))))
         (text (string-trim (replace-regexp-in-string "[[:space:]]+" " " prefix))))
    (pilish--truncate-string
     (if (> (length message) 512) (concat text "…") text) 120)))

(defun pilish--queue-tooltip-group (heading messages)
  "Format HEADING and up to three previews from MESSAGES.
Return nil when MESSAGES is empty; otherwise include its exact count."
  (let ((count (length messages)))
    (when (> count 0)
      (concat (format "%s (%d)\n" heading count)
              (mapconcat (lambda (message)
                           (concat "• " (pilish--queue-message-preview message)))
                         (seq-take messages 3) "\n")
              (when (> count 3)
                (format "\nand %d more" (- count 3)))))))

(defun pilish--header-format-queue (chat-buffer)
  "Format known queued messages belonging to CHAT-BUFFER.
An unavailable backend snapshot contributes no entries, not a claim that
its queues are empty.  Follow-ups retain backend order then FIFO order."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (let* ((snapshot (pilish--process-queue-snapshot pilish--process))
             (followups (append (plist-get snapshot :followUp)
                                (pilish--followups-in-fifo-order)))
             (steering (plist-get snapshot :steering))
             (count (+ (length followups) (length steering))))
        (when (> count 0)
          (let ((help (mapconcat
                       #'identity
                       (delq nil (list (pilish--queue-tooltip-group "Follow-ups" followups)
                                       (pilish--queue-tooltip-group "Steering" steering)))
                       "\n\n")))
            ;; Help text is literal user input, not command-key substitution.
            (put-text-property 0 1 'help-echo-inhibit-substitution t help)
            (propertize (format " queued %d" count)
                        'help-echo help 'mouse-face 'highlight)))))))

(defun pilish--header-escape-text (text)
  "Escape TEXT for use in `header-line-format'."
  (replace-regexp-in-string "%" "%%" text t t))

(defun pilish--header-format-extension-status (ext-status)
  "Format EXT-STATUS alist for header-line display.
Returns extension statuses joined with \" · \", or empty string."
  (if (null ext-status)
      ""
    (mapconcat (lambda (pair)
                 (let* ((key (car pair))
                        (text (pilish--header-escape-text (cdr pair)))
                        (face (cdr (assoc key pilish-extension-status-faces)))
                        (properties (and (stringp key)
                                         (list 'help-echo key
                                               'mouse-face 'highlight))))
                   (when face
                     (setq properties (append properties (list 'face face))))
                   (if properties
                       (apply #'propertize text properties)
                     text)))
               ext-status
               " · ")))

(defun pilish--header-format-identity (model-short thinking activity-phase-str)
  "Format identity group from MODEL-SHORT, THINKING, and ACTIVITY-PHASE-STR."
  (concat
   (propertize model-short
               'face 'pilish-model-name
               'mouse-face 'highlight
               'help-echo "mouse-1: Select model"
               'local-map pilish--header-model-map)
   (if (string-empty-p thinking)
       ""
     (concat " • "
             (propertize thinking
                         'mouse-face 'highlight
                         'help-echo "mouse-1: Cycle thinking level"
                         'local-map pilish--header-thinking-map)))
   " " activity-phase-str))

(defun pilish--header-format-context-group (session-name)
  "Format context group from SESSION-NAME.
Returns a leading-pipe group string or empty string
when no session name exists."
  (if (and session-name (not (string-empty-p session-name)))
      (concat " │ " (pilish--truncate-string session-name 30))
    ""))

(defun pilish--header-format-extension-group (ext-status working-message)
  "Format extension group from EXT-STATUS and WORKING-MESSAGE.
Returns a leading-pipe group string or empty string
when no extension info exists."
  (let* ((status-str (pilish--header-format-extension-status ext-status))
         (working-str (if (and working-message (not (string-empty-p working-message)))
                          (propertize (pilish--header-escape-text working-message)
                                      'face 'shadow)
                        ""))
         (parts nil))
    (unless (string-empty-p status-str)
      (push status-str parts))
    (unless (string-empty-p working-str)
      (push working-str parts))
    (if parts
        (concat " │ " (mapconcat #'identity (nreverse parts) " · "))
      "")))

(defun pilish--header-format-prompt-image (image)
  "Format a leading-pipe header group for prompt IMAGE."
  (if (not image)
      ""
    (let ((name (pilish--header-escape-text
                 (pilish--prompt-image-name image)))
          (size (file-size-human-readable
                 (pilish--prompt-image-byte-size image)
                 'iec " " "B")))
      (format " │ image: %s (%s)" name size))))

(defun pilish--header-line-string ()
  "Return formatted header-line string for input buffer.
Accesses state from the linked chat buffer."
  (let* ((chat-buf (cond
                    ;; In input buffer with valid link to chat
                    ((and pilish--chat-buffer (buffer-live-p pilish--chat-buffer))
                     pilish--chat-buffer)
                    ;; In chat buffer itself
                    ((derived-mode-p 'pilish-chat-mode)
                     (current-buffer))
                    ;; No valid chat buffer yet
                    (t nil)))
         (state (and chat-buf (buffer-local-value 'pilish--state chat-buf)))
         (stats (and chat-buf (buffer-local-value 'pilish--cached-stats chat-buf)))
         (ext-status (and chat-buf (buffer-local-value 'pilish--extension-status chat-buf)))
         (working-message (and chat-buf (buffer-local-value 'pilish--working-message chat-buf)))
         (session-name (and chat-buf (buffer-local-value 'pilish--session-name chat-buf)))
         (model-obj (plist-get state :model))
         (model-name (cond
                      ((stringp model-obj) model-obj)
                      ((plist-get model-obj :name))
                      (t "")))
         (model-short (if (string-empty-p model-name) "..."
                        (pilish--shorten-model-name model-name)))
         (thinking (or (plist-get state :thinking-level) ""))
         (activity-phase (or (and chat-buf
                                  (buffer-local-value 'pilish--activity-phase chat-buf))
                             "idle"))
         (activity-label
          (if (and chat-buf
                   (with-current-buffer chat-buf
                     (and (eq pilish--status 'sending)
                          (pilish--prompt-start-wait-active-p)
                          (not (pilish--prompt-wait-accepted pilish--prompt-wait))
                          (not (pilish--prompt-wait-started pilish--prompt-wait))
                          (not (pilish--prompt-wait-echoed pilish--prompt-wait)))))
              (propertize "waiting for Pi"
                          'help-echo "MCP servers may be connecting during startup.")
            activity-phase))
         (activity-phase-str
          (or (and chat-buf
                   (with-current-buffer chat-buf
                     (pilish--inactivity-status activity-phase)))
              (propertize (format "%-8s" activity-label)
                          'face 'pilish-activity-phase))))
    (concat
     (pilish--header-format-identity model-short thinking activity-phase-str)
     (pilish--header-format-stats stats)
     (pilish--header-format-queue chat-buf)
     (pilish--header-format-context-group session-name)
     (pilish--header-format-extension-group ext-status working-message)
     (pilish--header-format-prompt-image
      (pilish--get-prompt-image)))))

;;; State Management

(defun pilish--refresh-header ()
  "Refresh header-line by fetching and caching session stats."
  (when-let* ((proc (pilish--get-process))
             (chat-buf (pilish--get-chat-buffer)))
    (let ((input-buf (buffer-local-value 'pilish--input-buffer chat-buf)))
      (pilish--rpc-async proc '(:type "get_session_stats")
                     (lambda (response)
                       (when (eq (plist-get response :success) t)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (setq pilish--cached-stats (plist-get response :data))))
                         ;; Update the input buffer's header line
                         (when (buffer-live-p input-buf)
                           (dolist (win (get-buffer-window-list input-buf nil t))
                             (with-selected-window win
                               (force-mode-line-update))))))))))

(defun pilish--apply-state-response (chat-buf response)
  "Apply get_state RESPONSE to CHAT-BUF.
Updates buffer-local state variables and refreshes mode-line.
Safely handles dead buffers by checking liveness first."
  (when (and (eq (plist-get response :success) t)
             (buffer-live-p chat-buf))
    (with-current-buffer chat-buf
      (let* ((old-session-id (plist-get pilish--state :session-id))
             (new-state (pilish--extract-state-from-response
                         response
                         (pilish--chat-session-directory chat-buf)))
             (new-session-id (plist-get new-state :session-id)))
        ;; Retry activity is event-owned, not reported by get_state.
        (setq new-state
              (plist-put new-state :is-retrying
                         (plist-get pilish--state :is-retrying)))
        (when (and old-session-id
                   new-session-id
                   (not (equal old-session-id new-session-id)))
          (pilish--clear-unsupported-extension-ui-warnings))
        (let ((new-status
               (pilish--merge-state-response-status
                (plist-get new-state :status))))
          (plist-put new-state :status new-status)
          (setq pilish--status new-status
                pilish--state new-state)))
      (pilish--reconcile-inactivity-timer)
      (force-mode-line-update t))))

;;;; Sending Infrastructure

(defun pilish--send-abort ()
  "Clear backend queues before aborting the current operation.
Send both commands in wire order.  Their acknowledgments must not release
local ownership or mutate a later operation on the same process."
  (when-let* ((proc (pilish--get-process)))
    (pilish--rpc-async proc '(:type "clear_queue") #'ignore)
    (pilish--rpc-async proc '(:type "abort") #'ignore)))

(defconst pilish--prompt-start-timeout 0.5
  "Seconds before probing Pi state after acceptance with no agent_start.
Elapsed time alone does not return the frontend to idle.")

(defvar-local pilish--prompt-start-timer nil
  "Timer waiting for agent_start after prompt preflight success.")

(defun pilish--cancel-prompt-start-timer ()
  "Cancel any pending prompt-start fallback timer."
  (when (timerp pilish--prompt-start-timer)
    (cancel-timer pilish--prompt-start-timer))
  (setq pilish--prompt-start-timer nil))

(defun pilish--invalidate-prompt-start-wait ()
  "Cancel the prompt probe and invalidate the current request record."
  (pilish--cancel-prompt-start-timer)
  (setq pilish--prompt-wait nil))

(defun pilish--begin-prompt-start-wait ()
  "Reserve local submission for a prompt and return its opaque request record."
  (pilish--invalidate-prompt-start-wait)
  (setq pilish--prompt-wait
        (pilish--make-prompt-wait :process (pilish--get-process))))

(defun pilish--prompt-start-current-p (wait)
  "Return non-nil when WAIT still owns the current process's prompt request."
  (and wait (eq wait pilish--prompt-wait)
       (pilish--prompt-start-wait-active-p)))

(defun pilish--prompt-start-needed-p (wait)
  "Return non-nil when accepted WAIT has not produced an observed start."
  (and (pilish--prompt-start-current-p wait)
       (pilish--prompt-wait-accepted wait)
       (not (pilish--prompt-wait-started wait))))

(defun pilish--note-prompt-start ()
  "Record an observed agent_start without losing an unacknowledged request."
  (pilish--cancel-prompt-start-timer)
  (when (pilish--prompt-start-wait-active-p)
    (setf (pilish--prompt-wait-started pilish--prompt-wait) t)
    (when (pilish--prompt-wait-accepted pilish--prompt-wait)
      (pilish--invalidate-prompt-start-wait))))

(defun pilish--note-prompt-echo ()
  "Record an authoritative user echo so late acceptance cannot duplicate it."
  (when (pilish--prompt-start-wait-active-p)
    (setf (pilish--prompt-wait-echoed pilish--prompt-wait) t)))

(defun pilish--finish-prompt-without-agent-start
    (chat-buf wait on-no-agent-start)
  "Finish CHAT-BUF prompt WAIT after Pi confirms no agent turn.
Call ON-NO-AGENT-START after releasing local ownership."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (when (and (pilish--prompt-start-needed-p wait)
                 (not (memq pilish--status '(streaming compacting))))
        (pilish--invalidate-prompt-start-wait)
        (when (eq pilish--status 'sending)
          (setq pilish--status 'idle)
          (pilish--set-activity-phase "idle"))
        (when pilish--aborted
          (pilish--clear-followup-queue))
        (setq pilish--aborted nil)
        (when on-no-agent-start
          (funcall on-no-agent-start))))))

(defun pilish--probe-prompt-start-state (chat-buf wait on-no-agent-start)
  "Ask whether CHAT-BUF's accepted WAIT produced no agent turn.
Call ON-NO-AGENT-START only after Pi reports idle with no newer local start
or pending queued input."
  (let ((proc (pilish--prompt-wait-process wait)))
    (when (and proc (process-live-p proc))
      (condition-case nil
          (pilish--rpc-async
           proc '(:type "get_state")
           (lambda (response)
             (when (buffer-live-p chat-buf)
               (with-current-buffer chat-buf
                 (when (pilish--prompt-start-needed-p wait)
                   (let ((data (plist-get response :data)))
                     (if (or (memq pilish--status '(streaming compacting))
                             (not (eq (plist-get response :success) t))
                             (pilish--normalize-boolean (plist-get data :isStreaming))
                             (pilish--normalize-boolean (plist-get data :isCompacting))
                             (and (equal (pilish--prompt-wait-disposition wait) "queued")
                                  (> (or (plist-get data :pendingMessageCount) 0) 0)))
                         (pilish--schedule-prompt-start-fallback
                          chat-buf wait on-no-agent-start)
                       (pilish--finish-prompt-without-agent-start
                        chat-buf wait on-no-agent-start))))))))
        (error
         (pilish--schedule-prompt-start-fallback chat-buf wait on-no-agent-start))))))

(defun pilish--clear-sending-if-no-agent-start
    (chat-buf wait &optional on-no-agent-start)
  "Probe accepted WAIT in CHAT-BUF when no agent_start was observed.
Elapsed time alone does not release ownership or invoke ON-NO-AGENT-START."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (when (pilish--prompt-start-needed-p wait)
        (setq pilish--prompt-start-timer nil)
        (if (memq pilish--status '(streaming compacting))
            (pilish--schedule-prompt-start-fallback chat-buf wait on-no-agent-start)
          (pilish--probe-prompt-start-state chat-buf wait on-no-agent-start))))))

(defun pilish--schedule-prompt-start-fallback
    (chat-buf wait &optional on-no-agent-start)
  "Schedule a probe for CHAT-BUF's accepted WAIT with no observed agent_start.
ON-NO-AGENT-START is called only if the probe confirms no turn."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (when (pilish--prompt-start-needed-p wait)
        (pilish--cancel-prompt-start-timer)
        (setq pilish--prompt-start-timer
              (run-at-time pilish--prompt-start-timeout nil
                           #'pilish--clear-sending-if-no-agent-start
                           chat-buf wait on-no-agent-start))))))

(defun pilish--handle-prompt-send-failure
    (chat-buf wait on-failure &optional error-text)
  "Finish the current failed prompt request owned by WAIT.
Restore input through ON-FAILURE in CHAT-BUF and report ERROR-TEXT without
idling independently observed work.  Return non-nil only for the current WAIT."
  (let ((current-failure
         (and (buffer-live-p chat-buf)
              (with-current-buffer chat-buf
                (pilish--prompt-start-current-p wait)))))
    (when current-failure
      (pilish--abort-send chat-buf on-failure)
      (message "Pi: Send failed%s"
               (if error-text (format ": %s" error-text) "")))
    current-failure))

(defun pilish--send-prompt
    (text &optional on-success on-failure on-no-agent-start prompt-image)
  "Send TEXT and optional PROMPT-IMAGE to the pi process.
Slash commands are sent literally - pi handles expansion.
ON-SUCCESS runs in the chat buffer upon acceptance, which may follow a run.
ON-FAILURE restores a rejected request or synchronous scheduling failure.
ON-NO-AGENT-START runs only for accepted requests confirmed to have no turn."
  (let ((proc (pilish--get-process))
        (chat-buf (pilish--get-chat-buffer))
        wait)
    (cond
     ((null proc)
      (pilish--abort-send chat-buf on-failure)
      (message "Pi: No process available - try M-x pilish-reload or C-c C-p R"))
     ((not (process-live-p proc))
      (pilish--abort-send chat-buf on-failure)
      (message "Pi: Process died - try M-x pilish-reload or C-c C-p R"))
     (t
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (setq wait (pilish--begin-prompt-start-wait))
          (setq pilish--status 'sending)
          (pilish--set-activity-phase "thinking")))
      (condition-case err
          (pilish--rpc-async
           proc
           (append (list :type "prompt" :message text)
                   (when prompt-image
                     (list :images
                           (vector (pilish--prompt-image-content-block prompt-image)))))
           (lambda (response)
             (if (eq (plist-get response :success) t)
                 (when (buffer-live-p chat-buf)
                   (with-current-buffer chat-buf
                     (when (pilish--prompt-start-current-p wait)
                       (setf (pilish--prompt-wait-accepted wait) t
                             (pilish--prompt-wait-disposition wait)
                             (plist-get (plist-get response :data) :disposition))
                       (force-mode-line-update t)
                       (unwind-protect
                           (when on-success (funcall on-success))
                         (when (pilish--prompt-start-current-p wait)
                           ;; Acceptance has now removed any FIFO owner.  A
                           ;; retry failure restores only its unsent successors.
                           (when (pilish--prompt-wait-restore-followups wait)
                             (if pilish--aborted
                                 (pilish--clear-followup-queue)
                               (pilish--restore-followup-queue-to-input)))
                           (when (equal (pilish--prompt-wait-disposition wait) "queued")
                             (message "Pi: Prompt acknowledged as queued"))
                           (if (pilish--prompt-wait-started wait)
                               (progn
                                 (pilish--invalidate-prompt-start-wait)
                                 (unless (pilish--session-busy-p)
                                   (when pilish--aborted (pilish--clear-followup-queue))
                                   (setq pilish--aborted nil))
                                 (pilish--process-followup-queue))
                             ;; Dispositions describe this input, not newer
                             ;; work.  Queued is an acknowledgment, not a snapshot.
                             (pcase (pilish--prompt-wait-disposition wait)
                               ("handled"
                                (pilish--finish-prompt-without-agent-start
                                 chat-buf wait on-no-agent-start))
                               ("queued"
                                (when (and (eq pilish--status 'sending)
                                           (not (pilish--prompt-wait-echoed wait)))
                                  (pilish--set-activity-phase "queued"))))
                             (pilish--schedule-prompt-start-fallback
                              chat-buf wait on-no-agent-start)))))))
               (pilish--handle-prompt-send-failure
                chat-buf wait on-failure (plist-get response :error)))))
        ((error quit)
         (if (eq (car err) 'quit)
             (unwind-protect
                 (pilish--handle-prompt-send-failure
                  chat-buf wait on-failure (error-message-string err))
               (signal (car err) (cdr err)))
           (pilish--handle-prompt-send-failure
            chat-buf wait on-failure (error-message-string err)))))))))

(defun pilish--abort-send (chat-buf &optional on-failure)
  "Release a failed request in CHAT-BUF and restore input through ON-FAILURE.
Only its unstarted sending window becomes idle; independently observed runs
or compaction retain their activity and stop intent, even if restoration fails."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (let ((started (and pilish--prompt-wait
                          (pilish--prompt-wait-started pilish--prompt-wait))))
        (pilish--invalidate-prompt-start-wait)
        (unwind-protect
            (progn
              ;; Restore FIFO first so the direct owner can prepend its text.
              (if pilish--aborted
                  (pilish--clear-followup-queue)
                (pilish--restore-followup-queue-to-input))
              (when on-failure (funcall on-failure)))
          (setq pilish--local-user-message nil)
          (pilish--clear-local-user-message-region)
          (unless started
            (when (eq pilish--pre-compaction-status 'sending)
              (setq pilish--pre-compaction-status 'idle))
            (when (eq pilish--status 'sending)
              (setq pilish--status 'idle)))
          (when (eq pilish--status 'idle)
            (setq pilish--pre-compaction-status nil pilish--aborted nil)
            (pilish--set-activity-phase "idle")))))))


(provide 'pilish-ui)
;;; pilish-ui.el ends here
