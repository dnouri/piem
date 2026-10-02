;;; pilish-browse-test.el --- Tests for browsing module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for pilish-browse.el — session and tree browser
;; helper functions and response parsing.

;;; Code:

(require 'ert)
(require 'json)
(require 'transient)
(require 'pilish-browse)
(require 'pilish-jsonl)
(require 'pilish-test-common)

;;;; Test Fixtures

(defun pilish-test--fixture-sessions ()
  "Session items in the browse dialect, from browse-sessions.json.
Stands in for the dropped `pilish--parse-session-list'."
  (append (plist-get (plist-get (pilish-test--read-json-fixture
                                 "browse-sessions.json")
                                :data)
                     :sessions)
          nil))

;;;; Session Display

(ert-deftest pilish-test-session-display-name ()
  "Session display name prefers name over firstMessage."
  ;; Named session
  (should (equal (pilish--session-display-name
                  '(:name "My Session" :firstMessage "some prompt"))
                 "My Session"))
  ;; Unnamed session
  (should (equal (pilish--session-display-name
                  '(:firstMessage "Fix the bug in login.py"))
                 "Fix the bug in login.py"))
  ;; No name, no firstMessage
  (should (equal (pilish--session-display-name
                  '(:id "abc-123"))
                 "[empty session]"))
  ;; Newlines in firstMessage collapsed to spaces
  (should (equal (pilish--session-display-name
                  '(:firstMessage "Fix the bug\nin login.py"))
                 "Fix the bug in login.py"))
  ;; Multiple newlines and surrounding whitespace collapsed
  (should (equal (pilish--session-display-name
                  '(:firstMessage "First line\n\nSecond line\n  Third"))
                 "First line Second line Third"))
  ;; Newlines in name also collapsed
  (should (equal (pilish--session-display-name
                  '(:name "My\nSession" :firstMessage "prompt"))
                 "My Session")))

(ert-deftest pilish-test-first-nonempty-line ()
  "Extract first non-empty line from a string."
  ;; Single line
  (should (equal (pilish--first-nonempty-line "hello") "hello"))
  ;; Multi-line returns first
  (should (equal (pilish--first-nonempty-line "first\nsecond") "first"))
  ;; Skips leading blank lines
  (should (equal (pilish--first-nonempty-line "\n\nactual") "actual"))
  ;; Nil returns empty string
  (should (equal (pilish--first-nonempty-line nil) ""))
  ;; Empty string returns empty string
  (should (equal (pilish--first-nonempty-line "") ""))
  ;; Only whitespace returns empty string
  (should (equal (pilish--first-nonempty-line "\n  \n") "")))

;;;; Tree Parsing

(ert-deftest pilish-test-parse-tree ()
  "Parse get_tree response into tree data."
  (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
         (tree-data (pilish--parse-tree response)))
    (should tree-data)
    (should (equal (plist-get tree-data :leafId) "node-8"))
    ;; Tree has two roots
    (let ((roots (plist-get tree-data :tree)))
      (should (= (length roots) 2))
      ;; First root is a user message
      (let ((first (aref roots 0)))
        (should (equal (plist-get first :type) "message"))
        (should (equal (plist-get first :role) "user"))))))

(ert-deftest pilish-test-parse-tree-error ()
  "Return nil for failed get_tree response."
  (let ((response '(:type "response" :command "get_tree"
                    :success :false :error "no session")))
    (should (null (pilish--parse-tree response)))))

;;;; Margin Age Formatting

(ert-deftest pilish-test-margin-age-seconds ()
  "Margin age format for seconds."
  (should (equal (pilish--margin-age 1) '(1 . "second")))
  (should (equal (pilish--margin-age 30) '(30 . "second")))
  (should (equal (pilish--margin-age 59) '(59 . "second"))))

(ert-deftest pilish-test-margin-age-minutes ()
  "Margin age format for minutes."
  (should (equal (pilish--margin-age 60) '(1 . "minute")))
  (should (equal (pilish--margin-age 120) '(2 . "minute")))
  (should (equal (pilish--margin-age 3599) '(59 . "minute"))))

(ert-deftest pilish-test-margin-age-hours ()
  "Margin age format for hours."
  (should (equal (pilish--margin-age 3600) '(1 . "hour")))
  (should (equal (pilish--margin-age 7200) '(2 . "hour")))
  (should (equal (pilish--margin-age 86399) '(23 . "hour"))))

(ert-deftest pilish-test-margin-age-days ()
  "Margin age format for days."
  (should (equal (pilish--margin-age 86400) '(1 . "day")))
  (should (equal (pilish--margin-age 604799) '(6 . "day"))))

(ert-deftest pilish-test-margin-age-weeks ()
  "Margin age format for weeks."
  (should (equal (pilish--margin-age 604800) '(1 . "week")))
  (should (equal (pilish--margin-age 2629799) '(4 . "week"))))

(ert-deftest pilish-test-margin-age-months ()
  "Margin age format for months."
  (should (equal (pilish--margin-age 2629800) '(1 . "month")))
  (should (equal (pilish--margin-age 31557599) '(11 . "month"))))

(ert-deftest pilish-test-margin-age-years ()
  "Margin age format for years."
  (should (equal (pilish--margin-age 31557600) '(1 . "year")))
  (should (equal (pilish--margin-age 63115200) '(2 . "year"))))

(ert-deftest pilish-test-margin-age-zero ()
  "Margin age of zero seconds."
  (should (equal (pilish--margin-age 0) '(0 . "second"))))

(ert-deftest pilish-test-format-margin-age ()
  "Format margin age as aligned string."
  ;; Singular: no trailing s
  (should (equal (pilish--format-margin-age 1) " 1 second "))
  ;; Plural: trailing s
  (should (equal (pilish--format-margin-age 120) " 2 minutes"))
  ;; Right-justified count
  (should (equal (pilish--format-margin-age 3600) " 1 hour   "))
  ;; Large count
  (should (equal (pilish--format-margin-age 86400) " 1 day    "))
  ;; Multi-digit count (10 minutes)
  (should (equal (pilish--format-margin-age 600) "10 minutes"))
  ;; Week boundary
  (should (equal (pilish--format-margin-age 604800) " 1 week   ")))

(ert-deftest pilish-test-format-margin-age-from-iso ()
  "Format ISO timestamp as margin age string."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (encode-time '(0 0 12 24 2 2026 nil nil 0)))))
    ;; 5 minutes ago
    (should (equal (pilish--format-margin-age-from-iso
                    "2026-02-24T11:55:00.000Z")
                   " 5 minutes"))
    ;; 2 hours ago
    (should (equal (pilish--format-margin-age-from-iso
                    "2026-02-24T10:00:00.000Z")
                   " 2 hours  "))))

;;;; Margin Infrastructure

(ert-deftest pilish-test-propertize-face ()
  "Propertize-face sets both face and font-lock-face."
  (let ((s (pilish--propertize-face "hello" 'bold)))
    (should (equal (get-text-property 0 'face s) 'bold))
    (should (equal (get-text-property 0 'font-lock-face s) 'bold))))

(ert-deftest pilish-test-session-margin-width ()
  "Session margin width is computed from age spec."
  ;; Width = count(4) + " msgs "(5) + age(2+1+max-unit-len) = 19
  ;; With 1 char padding = 20
  (should (integerp pilish--session-margin-width))
  (should (>= pilish--session-margin-width 19)))

(ert-deftest pilish-test-tree-margin-width ()
  "Tree margin width accommodates labels."
  (should (integerp pilish--tree-margin-width))
  (should (>= pilish--tree-margin-width 14)))

(ert-deftest pilish-test-make-margin-overlay ()
  "Make-margin-overlay creates overlay with correct properties."
  (with-temp-buffer
    (insert "first line\n")
    (insert "second line\n")
    ;; Create overlay on the second line (point is after it)
    (pilish--make-margin-overlay "test margin")
    (let* ((ovs (overlays-in (point-min) (point-max)))
           (o (car ovs)))
      (should o)
      ;; Evaporate property set
      (should (overlay-get o 'evaporate))
      ;; Before-string contains the display spec
      (let* ((bs (overlay-get o 'before-string))
             (display (get-text-property 0 'display bs)))
        (should display)
        ;; Display spec is ((margin right-margin) STRING)
        (should (equal (car display) '(margin right-margin)))
        (should (equal (cadr display) "test margin"))))))

(ert-deftest pilish-test-make-margin-overlay-nil-string ()
  "Make-margin-overlay with nil uses a space."
  (with-temp-buffer
    (insert "a line\n")
    (pilish--make-margin-overlay nil)
    (let* ((ovs (overlays-in (point-min) (point-max)))
           (o (car ovs))
           (bs (overlay-get o 'before-string))
           (display (get-text-property 0 'display bs)))
      (should (equal (cadr display) " ")))))

(ert-deftest pilish-test-browse-apply-margins ()
  "Apply-margins sets the right margin on the window showing the buffer."
  (let ((buf (generate-new-buffer " *test-margins*"))
        (prev-buf (window-buffer (selected-window)))
        (prev-margins (window-margins (selected-window))))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (setq pilish--browse-margin-width 20)
            (pilish--browse-apply-margins))
          (should (equal (cdr (window-margins (selected-window))) 20)))
      (set-window-margins (selected-window)
                          (car prev-margins) (cdr prev-margins))
      (set-window-buffer (selected-window) prev-buf)
      (kill-buffer buf))))

(ert-deftest pilish-test-browse-mode-sets-right-margin-width ()
  "Browse mode sets buffer-local `right-margin-width'.
This ensures margins are cleaned up when `quit-window' switches to
another buffer — Emacs resets window margins from the new buffer's
`right-margin-width' during `set-window-buffer'."
  (let ((tree-buf (generate-new-buffer " *test-tree*"))
        (session-buf (generate-new-buffer " *test-sessions*")))
    (unwind-protect
        (progn
          (with-current-buffer tree-buf
            (pilish-tree-browser-mode)
            (should (= right-margin-width
                       pilish--tree-margin-width)))
          (with-current-buffer session-buf
            (pilish-session-browser-mode)
            (should (= right-margin-width
                       pilish--session-margin-width))))
      (kill-buffer tree-buf)
      (kill-buffer session-buf))))

(ert-deftest pilish-test-browse-mode-no-margin-leak ()
  "Mode setup must not set margins on unrelated windows.
When the browse buffer is created via `with-current-buffer' (not yet
displayed), `--browse-apply-margins' must not touch `selected-window'."
  (let ((other-buf (current-buffer))
        (browse-buf (generate-new-buffer " *test-tree-leak*")))
    (unwind-protect
        (progn
          ;; Record the current window's margins before mode setup
          (set-window-margins (selected-window) nil nil)
          (should-not (cdr (window-margins (selected-window))))
          ;; Create browse buffer in background (not displayed)
          (with-current-buffer browse-buf
            (pilish-tree-browser-mode))
          ;; The selected window (showing other-buf) must NOT have margins
          (should-not (cdr (window-margins (selected-window)))))
      (kill-buffer browse-buf))))

;;;; Active Path Detection

(ert-deftest pilish-test-active-path-ids ()
  "Compute set of node IDs on the active path from root to leaf."
  (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
         (tree-data (pilish--parse-tree response))
         (active (pilish--active-path-ids
                  (plist-get tree-data :tree)
                  (plist-get tree-data :leafId))))
    ;; The path from root to node-8: node-1 → node-2 → node-3 → node-4 → node-5 → node-6 → node-7 → node-8
    (should (gethash "node-1" active))
    (should (gethash "node-8" active))
    (should (gethash "node-4" active))
    ;; Abandoned branch node should NOT be on active path
    (should-not (gethash "node-9" active))
    ;; Compaction root node-10 is not on active path
    (should-not (gethash "node-10" active))))

(ert-deftest pilish-test-active-path-stops-at-ambiguous-parent ()
  "A unique current leaf is marked, but uncertain ancestry is not.
The leaf id itself is truthful.  Its bare parent id names conflicting
physical entries, so neither that row nor ancestors reached only by the
canonical parent choice may receive an active marker."
  (let* ((tree
          [(:id "top" :type "message" :role "user" :preview "top"
            :children
            [(:id "dup" :parentId "top" :ambiguousId t
              :type "message" :role "assistant" :preview "ambiguous"
              :children
              [(:id "leaf" :parentId "dup" :type "message"
                :role "assistant" :preview "unique leaf" :children [])])])])
         (active (pilish--active-path-ids tree "leaf")))
    (should (gethash "leaf" active))
    (should-not (gethash "dup" active))
    (should-not (gethash "top" active))
    (let ((parents (pilish--tree-parent-index tree)))
      (should (equal (gethash "leaf" parents) "dup"))
      (should-not (gethash "dup" parents))))
  ;; Projection promotes children of filtered bookkeeping.  Preserve the
  ;; same boundary explicitly when the ambiguous row itself is absent.
  (let* ((promoted
          [(:id "top" :type "message" :role "user" :preview "top"
            :children
            [(:id "leaf" :parentId "top" :ambiguousParent t
              :type "message" :role "assistant" :preview "unique leaf"
              :children [])])])
         (active (pilish--active-path-ids promoted "leaf")))
    (should (gethash "leaf" active))
    (should-not (gethash "top" active))
    (should-not (gethash "leaf" (pilish--tree-parent-index promoted)))))

(ert-deftest pilish-test-active-path-duplicate-id-cycle-renders-bounded ()
  "Duplicate ids fail closed before markers, sections, or RET identity.
The nested value is finite, but its ids imply dup → mid → dup and its
root/leaf are distinct `dup' occurrences.  Active-path construction
must remain bounded and return no alleged path.  Rendering exposes no
ambiguous rows, so there are zero @ markers and zero duplicate Magit
section identities; RET cannot take the cached-id fast no-op."
  (let* ((tree
          [(:id "dup" :type "message" :role "user" :preview "root dup"
            :children
            [(:id "mid" :parentId "dup" :type "message" :role "assistant"
              :preview "middle" :children
              [(:id "dup" :parentId "mid" :type "message" :role "user"
                :preview "leaf dup" :children [])])])])
         (real-puthash (symbol-function 'puthash))
         (writes 0)
         active)
    (cl-letf (((symbol-function 'puthash)
               (lambda (key value table)
                 (cl-incf writes)
                 (when (> writes 24)
                   (ert-fail "active-path parent cycle did not terminate"))
                 (funcall real-puthash key value table))))
      (setq active (pilish--active-path-ids tree "dup")))
    (should (= (hash-table-count active) 0))
    (with-temp-buffer
      (pilish-tree-browser-mode)
      (setq pilish--tree-browser-tree tree
            pilish--tree-browser-leaf-id "dup"
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      (should (= pilish--tree-browser-visible-count 0))
      (should (string-match-p "duplicate entry ids" (buffer-string)))
      (should-not (cdr (pilish--tree-rendered-section-index)))
      (should-not (string-match-p "[@*] " (buffer-string)))
      ;; A programmatic call with the ambiguous bare id must enter the
      ;; authoritative disk path, not claim either occurrence is current.
      (let (delegated)
        (cl-letf (((symbol-function 'pilish--browse-navigate-noncurrent)
                   (lambda (node-id) (setq delegated node-id))))
          (pilish--browse-navigate "dup"))
        (should (equal delegated "dup"))))))

;;;; Deep Tree Safety

(defun pilish-test--make-deep-tree (n)
  "Create a single-chain tree of N nodes for depth testing."
  (let ((node (list :id (format "node-%d" n)
                    :type "message" :role "user"
                    :preview (format "message %d" n)
                    :timestamp "2026-01-01T00:00:00Z"
                    :children (vector))))
    (cl-loop for i from (1- n) downto 1
             do (setq node (list :id (format "node-%d" i)
                                 :type "message"
                                 :role (if (= (mod i 2) 1) "user" "assistant")
                                 :preview (format "message %d" i)
                                 :timestamp "2026-01-01T00:00:00Z"
                                 :children (vector node))))
    (vector node)))

(ert-deftest pilish-test-flatten-tree-deep-chain ()
  "Flatten a linear chain deeper than max-lisp-eval-depth."
  (let* ((n 2000)
         (tree (pilish-test--make-deep-tree n))
         (leaf-id (format "node-%d" n))
         (flat (pilish--flatten-tree-for-display
                tree leaf-id 'default)))
    (should (= (length flat) n))))

(ert-deftest pilish-test-tree-search-deep-chain ()
  "Search topology stays iterative when all hidden ancestors are bypassed."
  (let* ((n 2000)
         (tree (pilish-test--make-deep-tree n))
         (leaf-id (format "node-%d" n))
         (flat (pilish--flatten-tree-for-display
                tree leaf-id 'default
                (list (format "message %d" n)))))
    (should (= (length flat) 1))
    (should (equal (plist-get (caar flat) :id) leaf-id))
    (should (equal (nth 2 (car flat)) ""))))

(ert-deftest pilish-test-subtree-contains-active-deep ()
  "Subtree-contains-active-p works on chains deeper than max-lisp-eval-depth."
  (let* ((n 2000)
         (tree (pilish-test--make-deep-tree n))
         (active-ids (make-hash-table :test 'equal)))
    (puthash (format "node-%d" n) t active-ids)
    (should (pilish--subtree-contains-active-p
             (aref tree 0) active-ids))))

;;;; Tree Flattening

(ert-deftest pilish-test-flatten-tree-for-display ()
  "Flatten tree into display-ordered list with indent levels and prefixes."
  (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
         (tree-data (pilish--parse-tree response))
         (flat (pilish--flatten-tree-for-display
                (plist-get tree-data :tree)
                (plist-get tree-data :leafId)
                'default)))
    ;; Should return a list of (node indent prefix) lists
    (should (listp flat))
    (should (> (length flat) 0))
    ;; First item should be the first root
    (let* ((first-entry (car flat))
           (node (nth 0 first-entry))
           (indent (nth 1 first-entry))
           (prefix (nth 2 first-entry)))
      (should (equal (plist-get node :id) "node-1"))
      (should (= indent 0))
      (should (stringp prefix)))))

(ert-deftest pilish-test-flatten-tree-connector-prefixes ()
  "Branch children get ├─/└─ connectors; chain nodes get gutter continuation."
  (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
         (tree-data (pilish--parse-tree response))
         (flat (pilish--flatten-tree-for-display
                (plist-get tree-data :tree)
                (plist-get tree-data :leafId)
                'default))
         ;; Build alist of (id . prefix) for easy lookup
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat)))
    ;; Root-level single-child chain: no prefix
    (should (equal (alist-get "node-1" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-2" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-3" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-4" prefix-alist nil nil #'equal) ""))
    ;; Branch point children: first gets ├─, last gets └─
    ;; node-5 is first (active branch), node-9 is last
    (should (equal (alist-get "node-5" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "node-9" prefix-alist nil nil #'equal) "└─ "))
    ;; Descendants within active branch: gutter continuation
    (should (equal (alist-get "node-6" prefix-alist nil nil #'equal) "│  "))
    (should (equal (alist-get "node-7" prefix-alist nil nil #'equal) "│  "))
    (should (equal (alist-get "node-8" prefix-alist nil nil #'equal) "│  "))
    ;; Second root and its child: no prefix (no top-level connectors)
    (should (equal (alist-get "node-10" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-11" prefix-alist nil nil #'equal) ""))))

(ert-deftest pilish-test-flatten-tree-connectors-no-tools-filter ()
  "Connectors work when tool nodes are filtered out."
  (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
         (tree-data (pilish--parse-tree response))
         (flat (pilish--flatten-tree-for-display
                (plist-get tree-data :tree)
                (plist-get tree-data :leafId)
                'no-tools))
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat))
         (id-list (mapcar (lambda (entry) (plist-get (nth 0 entry) :id)) flat)))
    ;; Tool nodes should be absent
    (should-not (member "node-3" id-list))
    (should-not (member "node-6" id-list))
    ;; Branch connectors still correct (node-5 first, node-9 last)
    (should (equal (alist-get "node-5" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "node-9" prefix-alist nil nil #'equal) "└─ "))
    ;; Chain descendant of active branch still gets gutter
    (should (equal (alist-get "node-7" prefix-alist nil nil #'equal) "│  "))
    (should (equal (alist-get "node-8" prefix-alist nil nil #'equal) "│  "))))

(ert-deftest pilish-test-flatten-tree-connectors-single-root ()
  "Single-root tree has no top-level connectors."
  (let* ((tree (list '(:id "r1" :type "message" :role "user"
                       :children [(:id "c1" :type "message" :role "assistant"
                                  :preview "hi" :children [])])))
         (flat (pilish--flatten-tree-for-display tree "c1" 'default))
         (prefixes (mapcar (lambda (e) (nth 2 e)) flat)))
    ;; Both nodes at root level, single-child chain — no connectors
    (should (equal prefixes '("" "")))))

(ert-deftest pilish-test-flatten-tree-connectors-nested-branches ()
  "Nested branch points produce correct multi-level gutter stacks."
  (let* ((tree (list
                '(:id "root" :type "message" :role "user" :preview "root"
                  :children
                  [(:id "a1" :type "message" :role "assistant" :preview "a1"
                    :children
                    [(:id "u2" :type "message" :role "user" :preview "u2"
                      :children [])
                     (:id "u3" :type "message" :role "user" :preview "u3"
                      :children [])])
                   (:id "a2" :type "message" :role "assistant" :preview "a2"
                    :children [])])))
         ;; leaf is u2 so a1 branch is active
         (flat (pilish--flatten-tree-for-display tree "u2" 'default))
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat)))
    ;; root: no prefix
    (should (equal (alist-get "root" prefix-alist nil nil #'equal) ""))
    ;; First branch children: a1 (active, first), a2 (last)
    (should (equal (alist-get "a1" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "a2" prefix-alist nil nil #'equal) "└─ "))
    ;; Nested branch under a1: u2 (active, first), u3 (last)
    ;; Gutter from outer branch (│) + inner connector
    (should (equal (alist-get "u2" prefix-alist nil nil #'equal) "│  ├─ "))
    (should (equal (alist-get "u3" prefix-alist nil nil #'equal) "│  └─ "))))

(ert-deftest pilish-test-flatten-tree-connectors-three-siblings ()
  "Three siblings at a branch point: ├─, ├─, └─."
  (let* ((tree (list
                '(:id "root" :type "message" :role "user" :preview "q"
                  :children
                  [(:id "c1" :type "message" :role "assistant"
                    :preview "first" :children [])
                   (:id "c2" :type "message" :role "assistant"
                    :preview "second" :children [])
                   (:id "c3" :type "message" :role "assistant"
                    :preview "third" :children [])])))
         (flat (pilish--flatten-tree-for-display tree "c1" 'default))
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat)))
    (should (equal (alist-get "root" prefix-alist nil nil #'equal) ""))
    ;; Active child first, then others in order
    (should (equal (alist-get "c1" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "c2" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "c3" prefix-alist nil nil #'equal) "└─ "))))

(ert-deftest pilish-test-tree-search-recomputes-last-sibling ()
  "Search makes the final matching sibling terminate with └─."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "root" :type "message" :role "user" :preview "keep root"
            :children
            [(:id "c1" :parentId "root" :type "message" :role "assistant"
              :preview "keep one" :children [])
             (:id "c2" :parentId "root" :type "message" :role "assistant"
              :preview "keep two" :children [])
             (:id "c3" :parentId "root" :type "message" :role "assistant"
              :preview "discard three" :children [])])]
          pilish--tree-browser-leaf-id "c1"
          pilish--tree-browser-filter 'default
          pilish--tree-browser-search-query "keep"
          pilish--tree-browser-search-tokens '("keep"))
    (pilish--tree-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "^├─ .*keep one" text))
      (should (string-match-p "^└─ .*keep two" text))
      (should-not (string-match-p "discard three" text)))))

(ert-deftest pilish-test-tree-search-removes-orphaned-gutter ()
  "A lone search result is a visible root with no inherited gutter."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "root" :type "message" :role "user" :preview "root"
            :children
            [(:id "branch" :parentId "root" :type "message"
              :role "assistant" :preview "branch"
              :children
              [(:id "target" :parentId "branch" :type "message"
                :role "user" :preview "needle target" :children [])])
             (:id "sibling" :parentId "root" :type "message"
              :role "assistant" :preview "sibling" :children [])])]
          pilish--tree-browser-leaf-id "target"
          pilish--tree-browser-filter 'default
          pilish--tree-browser-search-query "needle"
          pilish--tree-browser-search-tokens '("needle"))
    (pilish--tree-browser-rerender)
    (should (string-match-p "needle target" (buffer-string)))
    (should-not (string-match-p "[│├└]" (buffer-string)))))

(ert-deftest pilish-test-tree-filter-and-search-promote-visible-siblings ()
  "Filter and query jointly derive one coherent visible sibling set."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "root" :type "message" :role "user" :preview "keep root"
            :children
            [(:id "direct" :parentId "root" :type "message"
              :role "assistant" :preview "keep direct" :children [])
             (:id "tool" :parentId "root" :type "tool_result"
              :toolName "read" :preview "keep hidden tool"
              :children
              [(:id "promoted-1" :parentId "tool" :type "message"
                :role "assistant" :preview "keep promoted one" :children [])
               (:id "promoted-2" :parentId "tool" :type "message"
                :role "assistant" :preview "keep promoted two" :children [])])])]
          pilish--tree-browser-leaf-id "direct"
          pilish--tree-browser-filter 'no-tools
          pilish--tree-browser-search-query "keep"
          pilish--tree-browser-search-tokens '("keep"))
    (pilish--tree-browser-rerender)
    (let ((text (buffer-string)))
      (should-not (string-match-p "hidden tool" text))
      (should (string-match-p "^├─ .*keep direct" text))
      (should (string-match-p "^├─ .*keep promoted one" text))
      (should (string-match-p "^└─ .*keep promoted two" text)))))

;;;; Filter Predicates

(ert-deftest pilish-test-filter-default ()
  "Default filter shows messages, tool results, compaction, branch summary."
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "user") 'default))
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "hello") 'default))
  (should (pilish--browse-node-visible-p
           '(:type "tool_result") 'default))
  (should (pilish--browse-node-visible-p
           '(:type "compaction") 'default))
  (should (pilish--browse-node-visible-p
           '(:type "branch_summary") 'default))
  ;; Model change hidden in default
  (should-not (pilish--browse-node-visible-p
               '(:type "model_change") 'default))
  ;; Thinking level change hidden in default
  (should-not (pilish--browse-node-visible-p
               '(:type "thinking_level_change") 'default)))

(ert-deftest pilish-test-filter-no-tools ()
  "No-tools filter hides tool_result entries."
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "user") 'no-tools))
  (should-not (pilish--browse-node-visible-p
               '(:type "tool_result") 'no-tools)))

(ert-deftest pilish-test-filter-user-only ()
  "User-only filter shows only user messages."
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "user") 'user-only))
  (should-not (pilish--browse-node-visible-p
               '(:type "message" :role "assistant" :preview "hello") 'user-only))
  (should-not (pilish--browse-node-visible-p
               '(:type "tool_result") 'user-only)))

(ert-deftest pilish-test-filter-labeled-only ()
  "Labeled-only filter shows only entries with labels."
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "user" :label "checkpoint") 'labeled-only))
  (should-not (pilish--browse-node-visible-p
               '(:type "message" :role "user") 'labeled-only)))

(ert-deftest pilish-test-filter-all ()
  "All filter shows settings entries that other modes hide."
  (should (pilish--browse-node-visible-p
           '(:type "model_change") 'all))
  (should (pilish--browse-node-visible-p
           '(:type "thinking_level_change") 'all)))

(ert-deftest pilish-test-filter-system-message-role ()
  "System-role message nodes hide like settings under default/no-tools.
Visible under `all', mirroring the model_change treatment."
  (should-not (pilish--browse-node-visible-p
               '(:type "message" :role "system" :preview "system prompt")
               'default))
  (should-not (pilish--browse-node-visible-p
               '(:type "message" :role "system" :preview "system prompt")
               'no-tools))
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "system" :preview "system prompt")
           'all)))

(ert-deftest pilish-test-filter-empty-assistant ()
  "Empty assistant messages are hidden (unless they are the leaf)."
  ;; Empty assistant with no useful content
  (should-not (pilish--browse-node-visible-p
               '(:type "message" :role "assistant" :preview "") 'default))
  ;; Aborted assistant is shown
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "" :stopReason "aborted") 'default))
  ;; Assistant with error is shown
  (should (pilish--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "" :errorMessage "rate limit") 'default)))

(ert-deftest pilish-test-empty-assistant-hidden-in-all-modes ()
  "Empty assistant messages are hidden in ALL filter modes.
Per TUI tree-selector.ts:282-293 and PLAN-BROWSING.md line 560:
empty assistants are a universal pre-filter, not mode-specific."
  (let ((empty-ast '(:type "message" :role "assistant" :preview "(no content)"))
        (empty-ast-blank '(:type "message" :role "assistant" :preview "")))
    (dolist (mode '(default no-tools all))
      (should-not (pilish--browse-node-visible-p empty-ast mode))
      (should-not (pilish--browse-node-visible-p empty-ast-blank mode)))))

(ert-deftest pilish-test-empty-assistant-shown-when-aborted-all-modes ()
  "Aborted/error assistant messages are shown even if empty, in all modes."
  (let ((aborted '(:type "message" :role "assistant" :preview ""
                          :stopReason "aborted"))
        (errored '(:type "message" :role "assistant" :preview ""
                          :errorMessage "rate limit")))
    (dolist (mode '(default no-tools all))
      (should (pilish--browse-node-visible-p aborted mode))
      (should (pilish--browse-node-visible-p errored mode)))))

;;;; Search/Filter

(ert-deftest pilish-test-matches-filter-p ()
  "Space-separated regexp token matching."
  ;; Single token
  (should (pilish--matches-filter-p "Fix the login bug" '("login")))
  ;; Multiple tokens (AND)
  (should (pilish--matches-filter-p "Fix the login bug" '("login" "bug")))
  ;; Non-match
  (should-not (pilish--matches-filter-p "Fix the login bug" '("database")))
  ;; Regexp token
  (should (pilish--matches-filter-p "Fix the login bug" '("log.*bug")))
  ;; Empty tokens list matches everything
  (should (pilish--matches-filter-p "anything" nil)))

(ert-deftest pilish-test-tree-node-searchable-text-semantic-fields ()
  "Tree search uses projected semantic fields, not arbitrary tool JSON."
  (let ((message (pilish--tree-node-searchable-text
                  '(:type "message" :role "assistant"
                    :label "release-candidate" :preview "fixed parser"))))
    (dolist (needle '("message" "assistant"
                      "release-candidate" "fixed parser"))
      (should (string-match-p (regexp-quote needle) message))))
  (let ((summary (pilish--tree-node-searchable-text
                  '(:type "branch_summary"
                    :summary "First line\nsecond-line-needle"))))
    (should (string-match-p "second-line-needle" summary)))
  (let ((compaction (pilish--tree-node-searchable-text
                     '(:type "compaction"
                       :summary "compact-summary-needle"
                       :tokensBefore 42000))))
    (should (string-match-p "compact-summary-needle" compaction))
    (should (string-match-p "compacted (42k tokens)" compaction)))
  ;; Built-in recognition follows the formatter's case-sensitive names:
  ;; a custom tool called "Read" still uses the arbitrary-JSON fallback.
  (dolist (name '("custom_tool" "Read"))
    (let ((tool (pilish--tree-node-searchable-text
                 (list :type "tool_result" :toolName name
                       :toolArgs '(:secret "hidden-json-needle")
                       :formattedToolCall
                       (format "[%s: hidden-json-needle]" name)
                       :preview
                       (format "[%s: hidden-json-needle]" name)))))
      (should (string-match-p (regexp-quote name) tool))
      (should-not (string-match-p "hidden-json-needle" tool))))
  (let ((model (pilish--tree-node-searchable-text
                '(:type "model_change" :provider "anthropic"
                  :modelId "claude-searchable")))
        (thinking (pilish--tree-node-searchable-text
                   '(:type "thinking_level_change"
                     :thinkingLevel "xhigh-searchable"))))
    (should (string-match-p "anthropic" model))
    (should (string-match-p "claude-searchable" model))
    (should (string-match-p "anthropic/claude-searchable" model))
    (should (string-match-p "xhigh-searchable" thinking))))

(ert-deftest pilish-test-tree-search-finds-and-shows-label ()
  "A label is both searchable and present in keyboard-readable text."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "labeled" :type "message" :role "user"
            :label "release-candidate" :preview "ordinary text"
            :children [])]
          pilish--tree-browser-leaf-id "labeled"
          pilish--tree-browser-filter 'default
          pilish--tree-browser-search-query "release-candidate"
          pilish--tree-browser-search-tokens '("release-candidate"))
    (pilish--tree-browser-rerender)
    (should (= pilish--tree-browser-visible-count 1))
    (should (string-match-p "\\[release-candidate\\]" (buffer-string)))))

;;;; Session Views

(ert-deftest pilish-test-session-view-cycle ()
  "View cycles threaded → recent → messages → threaded."
  (should (eq (pilish--session-view-next 'threaded) 'recent))
  (should (eq (pilish--session-view-next 'recent) 'messages))
  (should (eq (pilish--session-view-next 'messages) 'threaded)))

(ert-deftest pilish-test-session-view-labels ()
  "Views carry honest user-facing labels.
The message-count view is Most messages — never relevance or Fuzzy,
which would promise a ranking the browser does not compute."
  (should (equal (pilish--session-view-label 'threaded)
                 "Threaded (fork families)"))
  (should (equal (pilish--session-view-label 'recent)
                 "Recent activity"))
  (should (equal (pilish--session-view-label 'messages)
                 "Most messages")))

(ert-deftest pilish-test-session-scope-labels ()
  "Scopes carry user-facing labels."
  (should (equal (pilish--session-scope-label 'current)
                 "This project"))
  (should (equal (pilish--session-scope-label 'all)
                 "All projects")))

;;;; Browser Default Options

(ert-deftest pilish-test-browser-default-options-shipped-values ()
  "Shipped defaults: this project, Threaded, all names, tree no-tools."
  (should (eq pilish-session-browser-default-scope 'current))
  (should (eq pilish-session-browser-default-view 'threaded))
  (should (null pilish-session-browser-default-named-only))
  (should (eq pilish-tree-browser-default-filter 'no-tools)))

(ert-deftest pilish-test-session-browser-new-buffer-uses-default-options ()
  "A newly created session browser starts from the default options."
  (let* ((dir (pilish-test--make-temp-directory "pi-browse-defs-"))
         (buf (let ((pilish-session-browser-default-scope 'all)
                    (pilish-session-browser-default-view 'messages)
                    (pilish-session-browser-default-named-only t))
                (pilish--get-or-create-session-browser dir))))
    (unwind-protect
        (with-current-buffer buf
          (should (eq pilish--session-browser-scope 'all))
          (should (eq pilish--session-browser-view 'messages))
          (should pilish--session-browser-named-only))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest pilish-test-session-browser-existing-buffer-keeps-local-state ()
  "Reopening an existing session browser keeps its local state.
`q' hides the browser without killing it, so the next open must not
reset scope, view, named-only, or the active query."
  (let* ((dir (pilish-test--make-temp-directory "pi-browse-keep-"))
         (buf (pilish--get-or-create-session-browser dir)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq pilish--session-browser-scope 'all
                  pilish--session-browser-view 'recent
                  pilish--session-browser-named-only t
                  pilish--session-browser-search-query "alias"
                  pilish--session-browser-search-tokens '("alias")))
          ;; The same live buffer is reused, not recreated.
          (should (eq (pilish--get-or-create-session-browser dir) buf))
          (with-current-buffer buf
            (should (eq pilish--session-browser-scope 'all))
            (should (eq pilish--session-browser-view 'recent))
            (should pilish--session-browser-named-only)
            (should (equal pilish--session-browser-search-tokens
                           '("alias")))))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest pilish-test-session-browser-mode-applies-defaults-before-hooks ()
  "Mode initialization applies the default options before hooks run.
Direct mode activation starts from the public defaults (not only the
entry-point buffers), mode hooks observe the configured values, and
a hook's overrides survive the initialization."
  (let ((observed nil)
        (named-in-hook-buffer nil)
        (pilish-session-browser-default-scope 'all)
        (pilish-session-browser-default-view 'messages))
    (let ((hook (lambda ()
                 (push (list pilish--session-browser-scope
                             pilish--session-browser-view)
                       observed)
                 ;; A hook override must not be clobbered.
                 (setq pilish--session-browser-named-only t))))
      ;; A global hook, the way users add them: `kill-all-local-variables'
      ;; at mode start would wipe a buffer-local hook binding first.
      (add-hook 'pilish-session-browser-mode-hook hook)
      (unwind-protect
          (with-temp-buffer
            (pilish-session-browser-mode)
            ;; Buffer-locals are only readable inside their buffer.
            (setq named-in-hook-buffer
                  pilish--session-browser-named-only))
        (remove-hook 'pilish-session-browser-mode-hook hook)))
    (should (equal observed '((all messages))))
    (should named-in-hook-buffer)))

(ert-deftest pilish-test-tree-browser-mode-applies-defaults-before-hooks ()
  "Tree mode initialization applies the default filter before hooks run."
  (let ((observed nil)
        (filter-in-hook-buffer nil)
        (pilish-tree-browser-default-filter 'user-only))
    (let ((hook (lambda ()
                 (push pilish--tree-browser-filter observed)
                 (setq pilish--tree-browser-filter 'labeled-only))))
      (add-hook 'pilish-tree-browser-mode-hook hook)
      (unwind-protect
          (with-temp-buffer
            (pilish-tree-browser-mode)
            (setq filter-in-hook-buffer
                  pilish--tree-browser-filter))
          (remove-hook 'pilish-tree-browser-mode-hook hook)))
    (should (equal observed '(user-only)))
    (should (eq filter-in-hook-buffer 'labeled-only))))

(ert-deftest pilish-test-session-browser-cycle-sort-obsolete-alias ()
  "The old public cycle command still resolves and still cycles views.
`pilish-session-browser-cycle-sort' was public API; it stays as an
obsolete alias so existing bindings and calls keep working."
  (should (eq (symbol-function 'pilish-session-browser-cycle-sort)
              'pilish-session-browser-cycle-view))
  (should (commandp 'pilish-session-browser-cycle-sort))
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items nil)
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) messages))))
        (call-interactively #'pilish-session-browser-cycle-sort))
      ;; The aliased command reports views, never sorts.
      (should (equal messages '("Pi: View: Recent activity"))))
    (should (eq pilish--session-browser-view 'recent))))

(ert-deftest pilish-test-tree-browser-new-buffer-uses-default-filter ()
  "A newly created tree browser starts from the default filter."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-defs-"))
         (buf (let ((pilish-tree-browser-default-filter 'labeled-only))
                (pilish--get-or-create-tree-browser dir))))
    (unwind-protect
        (with-current-buffer buf
          (should (eq pilish--tree-browser-filter 'labeled-only)))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest pilish-test-tree-browser-existing-buffer-keeps-local-filter ()
  "Reopening an existing tree browser keeps its locally chosen filter."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-keep-"))
         (buf (pilish--get-or-create-tree-browser dir)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq pilish--tree-browser-filter 'all))
          (should (eq (pilish--get-or-create-tree-browser dir) buf))
          (with-current-buffer buf
            (should (eq pilish--tree-browser-filter 'all))))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest pilish-test-session-sort-recent ()
  "Sort by recent puts newest modified first."
  (let ((items (list '(:modified "2026-02-20T10:00:00Z" :id "old")
                     '(:modified "2026-02-24T10:00:00Z" :id "new")
                     '(:modified "2026-02-22T10:00:00Z" :id "mid"))))
    (let ((sorted (pilish--session-sort-items items 'recent)))
      (should (equal (plist-get (nth 0 sorted) :id) "new"))
      (should (equal (plist-get (nth 1 sorted) :id) "mid"))
      (should (equal (plist-get (nth 2 sorted) :id) "old")))))

(ert-deftest pilish-test-session-sort-recent-nil-and-ties ()
  "Recent ordering survives missing mtimes and breaks ties by identity.
A nil `:modified' is the oldest known value, like the subtree
activity fallback, and equal mtimes order by canonical path
ascending instead of scan order."
  (let* ((items (list '(:modified "2026-02-22T10:00:00Z"
                        :path "/sess/z.jsonl" :id "tie-z")
                     '(:path "/sess/nil.jsonl" :id "nil-mod")
                     '(:modified "2026-02-24T10:00:00Z"
                      :path "/sess/new.jsonl" :id "new")
                     '(:modified "2026-02-22T10:00:00Z"
                      :path "/sess/a.jsonl" :id "tie-a")))
         (sorted (pilish--session-sort-items items 'recent)))
    (should (equal (mapcar (lambda (item) (plist-get item :id)) sorted)
                   '("new" "tie-a" "tie-z" "nil-mod")))))

(ert-deftest pilish-test-session-sort-messages ()
  "Most messages view puts highest message count first."
  (let ((items (list '(:messageCount 10 :id "small")
                     '(:messageCount 500 :id "big")
                     '(:messageCount 100 :id "med"))))
    (let ((sorted (pilish--session-sort-items items 'messages)))
      (should (equal (plist-get (nth 0 sorted) :id) "big"))
      (should (equal (plist-get (nth 1 sorted) :id) "med"))
      (should (equal (plist-get (nth 2 sorted) :id) "small")))))

(ert-deftest pilish-test-session-sort-messages-ties ()
  "Equal message counts order by identity ascending, like Recent."
  (let ((items (list '(:messageCount 5 :path "/sess/z.jsonl" :id "z")
                     '(:messageCount 9 :path "/sess/big.jsonl" :id "big")
                     '(:messageCount 5 :path "/sess/a.jsonl" :id "a"))))
    (should (equal (mapcar (lambda (item) (plist-get item :id))
                           (pilish--session-sort-items items 'messages))
                   '("big" "a" "z")))))

;;;; Session Threading

(ert-deftest pilish-test-session-threading ()
  "Thread items into parent-child structure."
  (let* ((items (pilish-test--fixture-sessions))
         (threaded (pilish--session-thread-items items)))
    (should (> (length threaded) 0))
    ;; Root rows carry no connector.
    (let ((roots (cl-remove-if-not (lambda (e) (equal (nth 1 e) ""))
                                   threaded)))
      (should (>= (length roots) 3)))
    ;; Session ccc-333 is a child of bbb-222 and connects to it.
    (let ((child (cl-find-if (lambda (e)
                               (equal (plist-get (nth 0 e) :id) "ccc-333"))
                             threaded)))
      (should child)
      (should (equal (nth 1 child) "\u2514\u2500 ")))))

(ert-deftest pilish-test-session-thread-subtree-activity-order ()
  "Families order by the latest activity anywhere in each subtree.
Discovery order (lexical, usually oldest first) must not leak
through: an old parent promoted by a newly active child outranks a
younger but colder root, and the parent renders before the child."
  (let* ((items (list
                 ;; Discovery order disagrees with activity order:
                 ;; the younger root is listed before the old parent,
                 ;; and the newest activity is a fork, not a root.
                 '(:path "/sess/mid-root.jsonl" :id "mid-root"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/sess/old-parent.jsonl" :id "old-parent"
                   :modified "2026-01-01T00:00:00Z")
                 '(:path "/sess/new-child.jsonl" :id "new-child"
                   :parentSessionPath "/sess/old-parent.jsonl"
                   :modified "2026-01-03T00:00:00Z")))
         (threaded (pilish--session-thread-items items)))
    (should (equal (mapcar (lambda (e) (plist-get (nth 0 e) :id)) threaded)
                   '("old-parent" "new-child" "mid-root")))
    (should (equal (mapcar (lambda (e) (nth 1 e)) threaded)
                   '("" "\u2514\u2500 " "")))))

(ert-deftest pilish-test-session-thread-sibling-order-and-connectors ()
  "Siblings sort by subtree activity and only the last gets the
final connector."
  (let* ((items (list
                 ;; Discovery order disagrees with sibling activity.
                 '(:path "/sess/p.jsonl" :id "p"
                   :modified "2026-01-01T00:00:00Z")
                 '(:path "/sess/k1.jsonl" :id "k1"
                   :parentSessionPath "/sess/p.jsonl"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/sess/k2.jsonl" :id "k2"
                   :parentSessionPath "/sess/p.jsonl"
                   :modified "2026-01-04T00:00:00Z")
                 '(:path "/sess/k3.jsonl" :id "k3"
                   :parentSessionPath "/sess/p.jsonl"
                   :modified "2026-01-03T00:00:00Z")))
         (threaded (pilish--session-thread-items items)))
    (should (equal (mapcar (lambda (e) (plist-get (nth 0 e) :id)) threaded)
                   '("p" "k2" "k3" "k1")))
    ;; Every earlier sibling branches; only the last one terminates.
    (should (equal (mapcar (lambda (e) (nth 1 e)) threaded)
                   '("" "├─ " "├─ " "└─ ")))))

(ert-deftest pilish-test-session-thread-nested-connectors ()
  "Nested forks get a continuing-ancestor gutter under their parent's
branch."
  (let* ((items (list
                 ;; Discovery order lists the older child first.
                 '(:path "/sess/p.jsonl" :id "p"
                   :modified "2026-01-01T00:00:00Z")
                 '(:path "/sess/a.jsonl" :id "a"
                   :parentSessionPath "/sess/p.jsonl"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/sess/b.jsonl" :id "b"
                   :parentSessionPath "/sess/p.jsonl"
                   :modified "2026-01-05T00:00:00Z")
                 '(:path "/sess/g1.jsonl" :id "g1"
                   :parentSessionPath "/sess/b.jsonl"
                   :modified "2026-01-03T00:00:00Z")
                 '(:path "/sess/g2.jsonl" :id "g2"
                   :parentSessionPath "/sess/b.jsonl"
                   :modified "2026-01-04T00:00:00Z")))
         (threaded (pilish--session-thread-items items)))
    ;; p's family: b (newest child) first with grandchildren nested,
    ;; then a; b is not last, so its descendants keep a gutter bar.
    (should (equal (mapcar (lambda (e)
                             (list (plist-get (nth 0 e) :id)
                                   (nth 1 e)))
                           threaded)
                   '(("p" "")
                     ("b" "├─ ")
                     ("g2" "│ ├─ ")
                     ("g1" "│ └─ ")
                     ("a" "└─ "))))))

(ert-deftest pilish-test-session-thread-activity-ties-break-by-path ()
  "Equal subtree activity falls back to canonical path ascending, so
ordering never depends on scan order."
  (let* ((items (list
                 '(:path "/sess/z-root.jsonl" :id "z-root"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/sess/a-root.jsonl" :id "a-root"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/sess/p.jsonl" :id "p"
                   :modified "2026-01-01T00:00:00Z")
                 '(:path "/sess/k-z.jsonl" :id "k-z"
                   :parentSessionPath "/sess/p.jsonl"
                   :modified "2026-01-01T12:00:00Z")
                 '(:path "/sess/k-a.jsonl" :id "k-a"
                   :parentSessionPath "/sess/p.jsonl"
                   :modified "2026-01-01T12:00:00Z")))
         (threaded (pilish--session-thread-items items)))
    (should (equal (mapcar (lambda (e) (plist-get (nth 0 e) :id)) threaded)
                   '("a-root" "z-root" "p" "k-a" "k-z")))))

(ert-deftest pilish-test-session-thread-missing-parent-is-root ()
  "A fork whose parent is absent from the item set is an ordinary
root: no connector, no nesting, no implied ancestor."
  (let* ((items (list
                 '(:path "/sess/gone-parent.jsonl" :id "gone"
                   :modified "2026-01-05T00:00:00Z")
                 '(:path "/sess/orphan.jsonl" :id "orphan"
                   :parentSessionPath "/sess/no-such-parent.jsonl"
                   :modified "2026-01-01T00:00:00Z")))
         (threaded (pilish--session-thread-items items)))
    (should (equal (mapcar (lambda (e)
                             (list (plist-get (nth 0 e) :id)
                                   (nth 1 e)))
                           threaded)
                   '(("gone" "")
                     ("orphan" ""))))))

(ert-deftest pilish-test-session-thread-equivalent-path-spellings ()
  "Equivalent local path spellings are one family identity.
A parent discovered through one symlink alias threads a child whose
recorded parent path uses a different alias of the same directory."
  (let* ((base (pilish-test--make-temp-directory "pi-thread-alias-"))
         (real (expand-file-name "real/sessions/" base))
         (alias-a (expand-file-name "alias-a/" base))
         (alias-b (expand-file-name "alias-b/" base)))
    (unwind-protect
        (progn
          (make-directory real t)
          (make-directory alias-a t)
          (make-directory alias-b t)
          (make-symbolic-link (directory-file-name real)
                              (expand-file-name "sessions" alias-a))
          (make-symbolic-link (directory-file-name real)
                              (expand-file-name "sessions" alias-b))
          (let* ((items (list
                         '(:path "/sess/other.jsonl" :id "other"
                           :modified "2026-01-01T00:00:00Z")
                         (list :path (expand-file-name "parent.jsonl"
                                                       (concat alias-a "sessions/"))
                               :id "parent"
                               :modified "2026-01-02T00:00:00Z")
                         (list :path (expand-file-name "child.jsonl"
                                                       (concat alias-a "sessions/"))
                               :id "child"
                               :parentSessionPath
                               (expand-file-name "parent.jsonl"
                                                 (concat alias-b "sessions/"))
                               :modified "2026-01-03T00:00:00Z")))
                 (threaded (pilish--session-thread-items items)))
            ;; The parent's family (activity Jan 3, via the child)
            ;; outranks the older root; the alias-spelled parent link
            ;; still threads the child under the parent.
            (should (equal (mapcar (lambda (e)
                                     (list (plist-get (nth 0 e) :id)
                                           (nth 1 e)))
                                   threaded)
                           '(("parent" "")
                             ("child" "└─ ")
                             ("other" ""))))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-thread-remote-parent-by-localname ()
  "A pi-local parent path threads under its TRAMP-prefixed discovery
spelling.  Family identity compares remote local names as pure
strings, so no remote host is ever contacted."
  (let* ((items (list
                 '(:path "/ssh:pi-host:/home/u/.pi/sessions/--d--/p.jsonl"
                   :id "p" :modified "2026-01-05T00:00:00Z")
                 '(:path "/ssh:pi-host:/home/u/.pi/sessions/--d--/c.jsonl"
                   :id "c"
                   :parentSessionPath "/home/u/.pi/sessions/--d--/p.jsonl"
                   :modified "2026-01-01T00:00:00Z")))
         (threaded (pilish--session-thread-items items)))
    (should (equal (mapcar (lambda (e)
                             (list (plist-get (nth 0 e) :id)
                                   (nth 1 e)))
                           threaded)
                   '(("p" "")
                     ("c" "└─ "))))))

(ert-deftest pilish-test-session-thread-remote-parent-not-local-canonicalized ()
  "A remote family never consults this machine's filesystem.
The discovered parent path is TRAMP-prefixed while the child's fork
header records the identical pi-local spelling; a local symlink
shadowing that spelling must not split the exact pair."
  (let* ((base (pilish-test--make-temp-directory "pi-thread-remote-"))
         (real (expand-file-name "real-home/sessions" base))
         (link-home (file-name-as-directory (expand-file-name "link-home" base))))
    (unwind-protect
        (progn
          (make-directory real t)
          ;; A LOCAL symlink that would rewrite the shared spelling
          ;; under `file-truename' on this machine.
          (make-symbolic-link (directory-file-name
                               (file-name-as-directory real))
                              (directory-file-name link-home))
          (let* ((shared (expand-file-name "p.jsonl"
                                           (concat link-home "sessions/")))
                 (items (list
                         (list :path (concat "/ssh:pi-host:" shared)
                               :id "P" :modified "2026-01-02T00:00:00Z")
                         (list :path (concat "/ssh:pi-host:"
                                             (expand-file-name
                                              "c.jsonl"
                                              (concat link-home "sessions/")))
                               :id "C"
                               :parentSessionPath shared
                               :modified "2026-01-01T00:00:00Z")))
                 (threaded (pilish--session-thread-items items)))
            (should (equal (mapcar (lambda (e)
                                     (list (plist-get (nth 0 e) :id)
                                           (nth 1 e)))
                                   threaded)
                           '(("P" "") ("C" "\u2514\u2500 "))))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-thread-remote-routes-stay-distinct ()
  "Distinct TRAMP routes sharing a local name stay distinct families.
A route-exact parent link threads under its own route's item only,
and no row renders twice."
  (let* ((items (list
                 '(:path "/ssh:a-host:/s/p.jsonl" :id "PA"
                   :modified "2026-01-03T00:00:00Z")
                 '(:path "/ssh:b-host:/s/p.jsonl" :id "PB"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/ssh:a-host:/s/c.jsonl" :id "C"
                   :parentSessionPath "/ssh:a-host:/s/p.jsonl"
                   :modified "2026-01-01T00:00:00Z")))
         (ids (mapcar (lambda (e) (plist-get (nth 0 e) :id))
                      (pilish--session-thread-items items))))
    ;; Every row renders exactly once.
    (should (equal (cl-sort (copy-sequence ids) #'string<)
                   '("C" "PA" "PB")))
    ;; The route-exact link threads C under PA's family only.
    (should (equal ids '("PA" "C" "PB")))))

(ert-deftest pilish-test-session-thread-remote-route-anchors-parent ()
  "A prefix-free fork header anchors to the child's own TRAMP route.
With two routes sharing a local name, the child joins its own
route's family regardless of input order, never the other route's."
  (let* ((pa '(:path "/ssh:a-host:/s/p.jsonl" :id "PA"
               :modified "2026-01-03T00:00:00Z"))
         (pb '(:path "/ssh:b-host:/s/p.jsonl" :id "PB"
               :modified "2026-01-02T00:00:00Z"))
         (c '(:path "/ssh:a-host:/s/c.jsonl" :id "C"
              :parentSessionPath "/s/p.jsonl"
              :modified "2026-01-01T00:00:00Z")))
    (dolist (items (list (list pa pb c) (list pb pa c)))
      (should (equal (mapcar (lambda (e)
                               (list (plist-get (nth 0 e) :id)
                                     (nth 1 e)))
                             (pilish--session-thread-items items))
                     '(("PA" "") ("C" "\u2514\u2500 ") ("PB" "")))))))

(ert-deftest pilish-test-session-thread-all-remote-skips-local-canonicalization ()
  "An all-remote family never consults this machine's filesystem.
`file-truename' must not run at all while threading remote items."
  (let* ((items (list
                 '(:path "/ssh:a-host:/s/p.jsonl" :id "P"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/ssh:a-host:/s/c.jsonl" :id "C"
                   :parentSessionPath "/s/p.jsonl"
                   :modified "2026-01-01T00:00:00Z")))
         (calls 0))
    (cl-letf (((symbol-function 'file-truename)
               (lambda (&rest _)
                 (cl-incf calls)
                 (error "local canonicalization of a remote family"))))
      (should (equal (mapcar (lambda (e)
                               (list (plist-get (nth 0 e) :id)
                                     (nth 1 e)))
                             (pilish--session-thread-items items))
                     '(("P" "") ("C" "\u2514\u2500 ")))))
    (should (zerop calls))))

(ert-deftest pilish-test-session-canonicalize-items-dedupes-and-keys ()
  "Ingestion computes one keyed item per session identity.
Equally canonical spellings of one file collapse to the first
spelling, every retained item carries its `:canonicalPath', and the
child's stored key matches its parent's, so family and live-marker
lookups reuse stored identities without per-render filesystem work."
  (let* ((base (pilish-test--make-temp-directory "pi-thread-dupes-"))
         (elsewhere (expand-file-name "elsewhere" base)))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (let* ((target (expand-file-name "parent.jsonl" elsewhere))
                 (alias1 (expand-file-name "alias1" base))
                 (alias2 (expand-file-name "alias2" base))
                 (p1-path (expand-file-name "parent.jsonl" alias1))
                 (c-path (expand-file-name "child.jsonl" alias1)))
            (write-region "" nil target)
            (make-directory alias1 t)
            (make-directory alias2 t)
            (make-symbolic-link target (expand-file-name "parent.jsonl" alias1))
            (make-symbolic-link target (expand-file-name "parent.jsonl" alias2))
            (let* ((raw (list
                         (list :path p1-path :id "P1"
                               :modified "2026-01-02T00:00:00Z")
                         (list :path (expand-file-name "parent.jsonl" alias2)
                               :id "P2" :modified "2026-01-02T00:00:00Z")
                         (list :path c-path :id "C"
                               :parentSessionPath target
                               :modified "2026-01-01T00:00:00Z")))
                   (items (pilish--session-canonicalize-items raw)))
              ;; One item per identity; the first spelling survives.
              (should (equal (mapcar (lambda (item) (plist-get item :id))
                                     items)
                             '("P1" "C")))
              ;; Stored keys are canonical and consistent (canonicalized
              ;; expectations: portable under a symlinked temp root).
              (should (equal (plist-get (nth 0 items) :canonicalPath)
                             (pilish--canonical-session-path target)))
              (should (equal (plist-get (nth 1 items) :canonicalPath)
                             (pilish--canonical-session-path c-path)))
              ;; Input plists are not mutated.
              (should-not (plist-get (nth 1 raw) :canonicalPath))
              ;; Families thread through the stored identities.
              (should (equal (mapcar (lambda (e)
                                       (list (plist-get (nth 0 e) :id)
                                             (nth 1 e)))
                                     (pilish--session-thread-items items))
                             '(("P1" "") ("C" "\u2514\u2500 ")))))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-browser-views-single-alias-row ()
  "Ingested aliases render one row in every view and across query
transitions, and point survives those transitions on the retained
row."
  (let* ((base (pilish-test--make-temp-directory "pi-views-dupes-"))
         (elsewhere (expand-file-name "elsewhere" base)))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (let* ((target (expand-file-name "parent.jsonl" elsewhere))
                 (alias1 (expand-file-name "alias1" base))
                 (alias2 (expand-file-name "alias2" base))
                 (p1-path (expand-file-name "parent.jsonl" alias1)))
            (write-region "" nil target)
            (make-directory alias1 t)
            (make-directory alias2 t)
            (make-symbolic-link target (expand-file-name "parent.jsonl" alias1))
            (make-symbolic-link target (expand-file-name "parent.jsonl" alias2))
            (with-temp-buffer
              (pilish-session-browser-mode)
              ;; The production scan path stores items through
              ;; `pilish--session-browser-apply-scan'.
              (pilish--session-browser-apply-scan
               (current-buffer)
               (list (list :path p1-path :name "Alias Parent"
                           :messageCount 3 :modified "2026-01-02T00:00:00Z")
                     (list :path (expand-file-name "parent.jsonl" alias2)
                           :name "Alias Parent"
                           :messageCount 3 :modified "2026-01-02T00:00:00Z")
                     (list :path (expand-file-name "child.jsonl" alias1)
                           :name "Alias Child"
                           :parentSessionPath target
                           :messageCount 1 :modified "2026-01-01T00:00:00Z"))
               nil nil)
              (should (= 2 (length pilish--session-browser-items)))
              ;; Unqueried Threaded: one family.
              (should (string-match-p "└─ Alias Child" (buffer-string)))
              (goto-char (point-min))
              (search-forward "Alias Parent")
              ;; Sections carry canonical identities (canonicalized
              ;; expectation: portable under a symlinked temp root);
              ;; actions see the retained raw spelling.
              (should (equal (oref (magit-current-section) value)
                             (pilish--canonical-session-path target)))
              (should (equal (pilish--session-browser-path-at-point)
                             p1-path))
              ;; A matching query still renders each session once.
              (setq pilish--session-browser-search-query "Alias"
                    pilish--session-browser-search-tokens '("Alias"))
              (pilish--session-browser-rerender)
              (should (equal (count-matches "Alias Parent"
                                            (point-min) (point-max))
                             1))
              ;; Clearing the query restores the family and keeps
              ;; point on the retained parent row.
              (setq pilish--session-browser-search-query nil
                    pilish--session-browser-search-tokens nil)
              (pilish--session-browser-rerender)
              (should (string-match-p "└─ Alias Child" (buffer-string)))
              (should (equal (oref (magit-current-section) value)
                             (pilish--canonical-session-path target)))
              ;; Cycling to the Recent view keeps point anchored to
              ;; the same session row.
              (setq pilish--session-browser-view 'recent)
              (pilish--session-browser-rerender)
              (should (equal (oref (magit-current-section) value)
                             (pilish--canonical-session-path target)))
              (setq pilish--session-browser-view 'threaded))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-browser-refresh-keeps-point-across-alias-change ()
  "Point survives a refresh that retains a different alias spelling.
Sections carry canonical identities, so the same session keeps its
point anchor while actions see the newly retained raw spelling."
  (let* ((base (pilish-test--make-temp-directory "pi-repr-alias-"))
         (elsewhere (expand-file-name "elsewhere" base)))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (let* ((target (expand-file-name "p.jsonl" elsewhere))
                 (alias-a (expand-file-name "alias-a" base))
                 (alias-b (expand-file-name "alias-b" base))
                 (a-path (expand-file-name "p.jsonl" alias-a))
                 (b-path (expand-file-name "p.jsonl" alias-b)))
            (write-region "" nil target)
            (make-directory alias-a t)
            (make-directory alias-b t)
            (make-symbolic-link target a-path)
            (make-symbolic-link target b-path)
            (with-temp-buffer
              (pilish-session-browser-mode)
              (pilish--session-browser-apply-scan
               (current-buffer)
               (list (list :path a-path :name "Same session"
                           :messageCount 3 :modified "2026-01-02T00:00:00Z")
                     '(:path "/repr/other.jsonl" :name "Zeta other"
                       :messageCount 9 :modified "2026-01-03T00:00:00Z"))
               nil nil)
              (goto-char (point-min))
              (search-forward "Same session")
              (beginning-of-line)
              ;; The next scan no longer finds alias A but finds the
              ;; equivalent alias B: same canonical session.
              (pilish--session-browser-apply-scan
               (current-buffer)
               (list '(:path "/repr/other.jsonl" :name "Zeta other"
                       :messageCount 9 :modified "2026-01-03T00:00:00Z")
                     (list :path b-path :name "Same session"
                           :messageCount 3 :modified "2026-01-02T00:00:00Z"))
               nil nil)
              ;; Still the same session under point, not the top row.
              (should (string-match-p "Same session"
                                      (thing-at-point 'line)))
              ;; The section identity is the canonical file —
              ;; canonicalized here so a symlinked
              ;; `temporary-file-directory' (e.g. macOS /var) keeps
              ;; the test portable — and actions see the retained
              ;; raw spelling.
              (should (equal (oref (magit-current-section) value)
                             (pilish--canonical-session-path target)))
              (should (equal (pilish--session-browser-path-at-point)
                             b-path)))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-browser-scan-quit-leaves-coherent-state ()
  "A quit during canonicalization reports an interrupted scan.
The loading and error state stay coherent instead of stranding a
Loading render under cleared flags."
  (let* ((root (pilish-test--make-temp-directory "pi-scan-quit-"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (path (expand-file-name "s.jsonl" dir)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (pilish-test--write-session-lines
           path (list (pilish-test--make-session-header "sid-quit")))
          (with-temp-buffer
            (pilish-session-browser-mode)
            (let ((process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                (directory-file-name root))
                         process-environment)))
              (cl-letf (((symbol-function 'pilish--session-list-directory)
                         (lambda (&optional _chat-buf) dir))
                        ((symbol-function 'pilish--session-directory)
                         (lambda () "/home/fake/a/"))
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args)))
                        ((symbol-function 'file-truename)
                         (lambda (&rest _)
                           (signal 'quit nil))))
                (pilish--session-browser-fetch-and-render))
              (should-not pilish--session-browser-loading)
              (should (equal pilish--session-browser-error
                             "Session scan was interrupted"))
              (should (string-match-p "interrupted" (buffer-string)))
              (should-not (string-match-p "Loading sessions"
                                          (buffer-string))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest pilish-test-session-browser-rerender-without-filesystem-io ()
  "Rerenders do no per-row, per-family, or remote canonicalization.
Family identity, including fork parent links, is resolved once
during the scan; toggling views and queries never calls
`file-truename' (only live-process paths may canonicalize locally,
and none are live here)."
  (let* ((root (pilish-test--make-temp-directory "pi-scan-pure-"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (parent-path (expand-file-name "parent.jsonl" dir))
         (child-path (expand-file-name "child.jsonl" dir))
         (calls 0))
    (unwind-protect
        (progn
          (make-directory dir t)
          (pilish-test--write-session-lines
           parent-path
           (list (pilish-test--make-session-header "sid-parent")))
          (pilish-test--write-session-lines
           child-path
           (list (pilish-test--make-session-header
                  "sid-child" :parentSession parent-path)))
          (with-temp-buffer
            (pilish-session-browser-mode)
            (let ((process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                (directory-file-name root))
                         process-environment)))
              (cl-letf (((symbol-function 'pilish--session-list-directory)
                         (lambda (&optional _chat-buf) dir))
                        ((symbol-function 'pilish--session-directory)
                         (lambda () "/home/fake/a/"))
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args))))
                (pilish--session-browser-fetch-and-render))
              (should (= 2 (length pilish--session-browser-items)))
              (should (string-match-p "└─" (buffer-string)))
              ;; View and query rerenders with the filesystem cut off.
              (cl-letf (((symbol-function 'file-truename)
                         (lambda (&rest _)
                           (cl-incf calls)
                           (error "rerender touched the filesystem"))))
                (setq pilish--session-browser-view 'recent)
                (pilish--session-browser-rerender)
                (setq pilish--session-browser-view 'threaded)
                (pilish--session-browser-rerender)
                (setq pilish--session-browser-search-query "fix"
                      pilish--session-browser-search-tokens '("fix"))
                (pilish--session-browser-rerender)
                (setq pilish--session-browser-search-query nil
                      pilish--session-browser-search-tokens nil)
                (pilish--session-browser-rerender)
                (should (string-match-p "└─" (buffer-string))))
              (should (zerop calls)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest pilish-test-session-file-matches-multi-hop-routes-distinct ()
  "Live/current identity keeps complete TRAMP routes distinct.
A final-hop-only spelling is not the multi-hop session's file."
  (let ((chat-buf (generate-new-buffer "*pilish-test-match-route*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state
                  (list :session-file
                        "/ssh:bastion|ssh:pi-host:/s/p.jsonl")))
          (should-not (pilish--browse-session-file-matches-p
                       chat-buf "/ssh:pi-host:/s/p.jsonl"))
          (should (pilish--browse-session-file-matches-p
                   chat-buf "/ssh:bastion|ssh:pi-host:/s/p.jsonl")))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-session-file-matches-local-alias ()
  "Live/current identity unifies local symlink aliases."
  (let* ((base (pilish-test--make-temp-directory "pi-match-alias-"))
         (elsewhere (expand-file-name "elsewhere" base))
         (chat-buf (generate-new-buffer "*pilish-test-match-alias*")))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (let ((target (expand-file-name "live.jsonl" elsewhere))
                (alias-dir (expand-file-name "alias" base)))
            (make-directory alias-dir t)
            (write-region "" nil target)
            (make-symbolic-link target
                                (expand-file-name "live.jsonl" alias-dir))
            (with-current-buffer chat-buf
              (setq pilish--state (list :session-file target)))
            (should (pilish--browse-session-file-matches-p
                     chat-buf
                     (expand-file-name "live.jsonl" alias-dir)))))
      (kill-buffer chat-buf)
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-browser-scan-malformed-parent-isolated ()
  "A malformed fork header degrades to an orphan, not a failed scan.
A NUL byte in one file's parentSessionPath must not abort the scan
or discard the healthy files around it."
  (let* ((root (pilish-test--make-temp-directory "pi-scan-nul-"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (parent-path (expand-file-name "parent.jsonl" dir))
         (bad-path (expand-file-name "bad.jsonl" dir))
         (child-path (expand-file-name "child.jsonl" dir)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (pilish-test--write-session-lines
           parent-path
           (list (pilish-test--make-session-header "sid-parent")))
          (pilish-test--write-session-lines
           bad-path
           (list (pilish-test--make-session-header
                  "sid-bad" :parentSession "/bad\0parent.jsonl")))
          (pilish-test--write-session-lines
           child-path
           (list (pilish-test--make-session-header
                  "sid-child" :parentSession parent-path)))
          (with-temp-buffer
            (pilish-session-browser-mode)
            (let ((process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                (directory-file-name root))
                         process-environment)))
              (cl-letf (((symbol-function 'pilish--session-list-directory)
                         (lambda (&optional _chat-buf) dir))
                        ((symbol-function 'pilish--session-directory)
                         (lambda () "/home/fake/a/"))
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args))))
                (pilish--session-browser-fetch-and-render))
              (should-not pilish--session-browser-error)
              (should (= 3 (length pilish--session-browser-items)))
              ;; The malformed fork degrades to a plain root row; the
              ;; healthy family still threads.
              (let ((rows (pilish--session-thread-items
                           pilish--session-browser-items)))
                (should (= 3 (length rows)))
                (dolist (row rows)
                  (let ((id (plist-get (nth 0 row) :id)))
                    (cond
                     ((equal id "sid-bad")
                      (should (equal (nth 1 row) "")))
                     ((equal id "sid-child")
                      (should (equal (nth 1 row) "\u2514\u2500 "))))))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest pilish-test-session-parent-anchor-is-recorded-cwd ()
  "Relative fork headers resolve against the child's recorded cwd.
Pi resolves relative paths against its process working directory —
the session header's `cwd' — so threading is independent of the
ambient buffer's `default-directory', locally and over TRAMP."
  (let* ((items (list
                 '(:path "/anchor/parent.jsonl" :id "P"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/sess-other/child.jsonl" :id "C"
                   :cwd "/anchor"
                   :parentSessionPath "parent.jsonl"
                   :modified "2026-01-01T00:00:00Z")))
         (child-row
          (lambda (rows)
            (cl-find "C" rows :test #'equal
                     :key (lambda (e) (plist-get (nth 0 e) :id))))))
    (dolist (ambient '("/anchor/" "/somewhere/else/"))
      (let ((default-directory ambient))
        (should (equal (nth 1 (funcall child-row
                                       (pilish--session-thread-items items)))
                       "\u2514\u2500 "))))
    ;; Remote child: the recorded cwd is pi-local, and the resolved
    ;; identity rides the child's own TRAMP route.
    (let* ((remote-items
            (list '(:path "/ssh:pi-host:/home/u/co/parent.jsonl" :id "P"
                   :modified "2026-01-02T00:00:00Z")
                  '(:path "/ssh:pi-host:/home/u/.pi/s/c.jsonl" :id "C"
                    :cwd "/home/u/co"
                    :parentSessionPath "parent.jsonl"
                    :modified "2026-01-01T00:00:00Z"))))
      (dolist (ambient '("/local/ambient/" "/ssh:other:/ambient/"))
        (let ((default-directory ambient))
          (should (equal (nth 1 (funcall child-row
                                         (pilish--session-thread-items
                                          remote-items)))
                         "\u2514\u2500 ")))))))

(ert-deftest pilish-test-session-file-matches-local-hardlink ()
  "Live/current identity still detects local hardlinks.
Canonical spellings differ for two names of one inode; the local
same-file fallback catches what `file-truename' cannot."
  (let* ((base (pilish-test--make-temp-directory "pi-match-hardlink-"))
         (elsewhere (expand-file-name "elsewhere" base))
         (chat-buf (generate-new-buffer "*pilish-test-match-hardlink*")))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (let* ((target (expand-file-name "live.jsonl" elsewhere))
                 (linked (expand-file-name "linked.jsonl" base)))
            (write-region "" nil target)
            (add-name-to-file target linked)
            (with-current-buffer chat-buf
              (setq pilish--state (list :session-file target)))
            (should (pilish--browse-session-file-matches-p chat-buf linked))))
      (kill-buffer chat-buf)
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-file-matches-remote-never-stats ()
  "Remote identity never falls back to `file-equal-p'.
Distinct routes stay distinct without contacting any host."
  (let ((chat-buf (generate-new-buffer "*pilish-test-match-remote2*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state
                  (list :session-file
                        "/ssh:bastion|ssh:pi-host:/s/p.jsonl")))
          (cl-letf (((symbol-function 'file-equal-p)
                     (lambda (&rest _)
                       (error "remote identity must not stat"))))
            (should-not (pilish--browse-session-file-matches-p
                         chat-buf "/ssh:pi-host:/s/p.jsonl"))
            (should (pilish--browse-session-file-matches-p
                     chat-buf
                     "/ssh:bastion|ssh:pi-host:/s/p.jsonl"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-session-parent-anchor-multi-hop-cwd-less ()
  "A cwd-less multi-hop child anchors relatives to its own route.
The child session file's directory keeps the complete TRAMP route,
so a relative fork header resolves to the full-route parent
identity."
  (let* ((items (list
                 '(:path "/ssh:bastion|ssh:pi-host:/s/parent.jsonl" :id "P"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/ssh:bastion|ssh:pi-host:/s/child.jsonl" :id "C"
                   :parentSessionPath "parent.jsonl"
                   :modified "2026-01-01T00:00:00Z")))
         (child (pilish--session-enrich-item (nth 1 items))))
    ;; Enrichment stores the full-route parent identity.
    (should (equal (plist-get child :canonicalParentSession)
                   "/ssh:bastion|ssh:pi-host:/s/parent.jsonl"))
    ;; And the family threads through it.
    (should (equal (mapcar (lambda (e)
                             (list (plist-get (nth 0 e) :id)
                                   (nth 1 e)))
                           (pilish--session-thread-items items))
                   '(("P" "") ("C" "└─ "))))))

(ert-deftest pilish-test-session-parent-anchor-relative-cwd-invalid ()
  "A relative recorded cwd never anchors fork resolution.
It would expand against the ambient buffer; the child session file's
own directory anchors instead, independent of `default-directory'."
  (let* ((items (list
                 '(:path "/proj-sessions/parent.jsonl" :id "P"
                   :modified "2026-01-02T00:00:00Z")
                 '(:path "/proj-sessions/child.jsonl" :id "C"
                   :cwd "rel-proj"
                   :parentSessionPath "parent.jsonl"
                   :modified "2026-01-01T00:00:00Z"))))
    (dolist (ambient '("/ambient/one/" "/proj-sessions/"))
      (let ((default-directory ambient))
        (should (equal (nth 1 (cl-find "C"
                                       (pilish--session-thread-items items)
                                       :key (lambda (e)
                                              (plist-get (nth 0 e) :id))
                                       :test #'equal))
                       "└─ "))))))

(ert-deftest pilish-test-session-thread-cyclic-symlinks-degrade-not-abort ()
  "Cyclic symlink spellings degrade to lexical identities, not render
aborts.  A parent link through a cycle becomes an orphan root; a
cyclic item path still renders."
  (let* ((base (pilish-test--make-temp-directory "pi-thread-cycle-"))
         (cyc (expand-file-name "cycle" base)))
    (unwind-protect
        (progn
          (make-directory cyc t)
          (make-symbolic-link "b.jsonl" (expand-file-name "a.jsonl" cyc))
          (make-symbolic-link "a.jsonl" (expand-file-name "b.jsonl" cyc))
          ;; Canonicalization falls back to the lexical spelling.
          (should (equal (pilish--canonical-session-path
                          (expand-file-name "a.jsonl" cyc))
                         (expand-file-name "a.jsonl" cyc)))
          (let* ((items (list
                         (list :path (expand-file-name "a.jsonl" cyc)
                               :id "CYC" :modified "2026-01-05T00:00:00Z")
                         (list :path (expand-file-name "ok.jsonl" base)
                               :id "OK" :modified "2026-01-02T00:00:00Z")
                         (list :path (expand-file-name "c.jsonl" base)
                               :id "C"
                               :parentSessionPath (expand-file-name "ok.jsonl" base)
                               :modified "2026-01-01T00:00:00Z")
                         (list :path (expand-file-name "d.jsonl" base)
                               :id "D"
                               :parentSessionPath (expand-file-name "b.jsonl" cyc)
                               :modified "2026-01-04T00:00:00Z")))
                 (rows (mapcar (lambda (e)
                                 (list (plist-get (nth 0 e) :id)
                                       (nth 1 e)))
                               (pilish--session-thread-items items))))
            ;; D's parent link names a cyclic spelling no item has, so
            ;; it degrades to an orphan root; the healthy family still
            ;; threads; the cyclic item still renders; nothing aborts.
            (should (equal rows
                           '(("CYC" "") ("D" "") ("OK" "")
                             ("C" "\u2514\u2500 "))))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-thread-symlinked-session-file ()
  "A symlinked session file matches a parent header naming the real
target, like pi's realpath identity."
  (let* ((base (pilish-test--make-temp-directory "pi-thread-filelink-"))
         (sessions (expand-file-name "sessions" base))
         (elsewhere (expand-file-name "elsewhere" base)))
    (unwind-protect
        (progn
          (make-directory sessions t)
          (make-directory elsewhere t)
          (let ((real-target (expand-file-name "real-parent.jsonl" elsewhere)))
            (write-region "" nil real-target)
            (make-symbolic-link
             real-target (expand-file-name "parent.jsonl" sessions))
            (let* ((items (list
                           (list :path (expand-file-name "parent.jsonl"
                                                         sessions)
                                 :id "P" :modified "2026-01-02T00:00:00Z")
                           (list :path (expand-file-name "child.jsonl"
                                                         sessions)
                                 :id "C"
                                 :parentSessionPath real-target
                                 :modified "2026-01-01T00:00:00Z")))
                   (threaded (pilish--session-thread-items items)))
              (should (equal (mapcar (lambda (e)
                                       (list (plist-get (nth 0 e) :id)
                                             (nth 1 e)))
                                     threaded)
                             '(("P" "") ("C" "\u2514\u2500 ")))))))
      (when (file-directory-p base)
        (delete-directory base t)))))

;;;; Session Filter

(ert-deftest pilish-test-session-filter-named ()
  "Named filter keeps only sessions with a name."
  (let* ((items (pilish-test--fixture-sessions))
         (named (pilish--session-filter-named items)))
    ;; Only bbb-222 and ddd-444 have names
    (should (= (length named) 2))
    (should (cl-every (lambda (item)
                        (plist-get item :name))
                      named))))

(ert-deftest pilish-test-session-filter-search ()
  "Search filter matches against name and first message."
  (let ((items (pilish-test--fixture-sessions)))
    ;; Search for "database"
    (let ((found (pilish--session-filter-search items '("database"))))
      (should (= (length found) 2))  ; bbb-222 and ccc-333 mention database
      )
    ;; Search for "CI" matches Setup CI/CD
    (let ((found (pilish--session-filter-search items '("CI"))))
      (should (>= (length found) 1)))))

;;;; Time Groups

(defun pilish-test--local-time-iso (time)
  "Return TIME as a UTC ISO string that round-trips to the same instant.
Built from local wall-clock input, so calendar-date expectations hold in
every runner timezone."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(ert-deftest pilish-test-session-time-group ()
  "Time group labels use one pinned production clock."
  (let ((fixed-now (encode-time '(0 0 8 11 3 2026 nil nil nil))))
    (cl-letf (((symbol-function 'current-time) (lambda () fixed-now)))
      ;; Exercise the default NOW path, not only the explicit test seam.
      (should (equal (pilish--session-time-group
                      (pilish-test--local-time-iso fixed-now))
                     "Today"))
      (should (equal (pilish--session-time-group
                      (pilish-test--local-time-iso
                       (time-subtract fixed-now (days-to-time 30))))
                     "Older")))))

(ert-deftest pilish-test-session-time-group-calendar-boundaries ()
  "Group labels follow local calendar dates, not rolling 24-hour windows.
Pinned NOW is Wednesday 2026-03-11 08:00 local; Monday 2026-03-09
starts the current Monday-start week and Friday 2026-03-06 falls in
the previous one.  Under rolling durations a 23:00 session from nine
hours ago claimed Today, a two-day-old session claimed Yesterday,
and last Friday claimed This Week."
  (let* ((now (encode-time '(0 0 8 11 3 2026 nil nil nil)))
         (group (lambda (hms)
                   (pilish--session-time-group
                    (pilish-test--local-time-iso (apply #'encode-time hms))
                    now))))
    ;; Same local date, however many hours ago: Today.
    (should (equal (funcall group '(0 0 7 11 3 2026)) "Today"))
    ;; Yesterday 23:00 — nine hours ago, previous calendar day.
    (should (equal (funcall group '(0 0 23 10 3 2026)) "Yesterday"))
    ;; Yesterday 00:30 — 31.5 hours ago, still one calendar day back.
    (should (equal (funcall group '(30 0 0 10 3 2026)) "Yesterday"))
    ;; Monday 23:00 — two calendar days back, current Monday-start week.
    (should (equal (funcall group '(0 0 23 9 3 2026)) "This Week"))
    ;; Previous Friday 23:00 — under five days ago, previous ISO week.
    (should (equal (funcall group '(0 0 23 6 3 2026)) "Older"))))

(ert-deftest pilish-test-session-time-group-dst-yesterday ()
  "Yesterday stays one calendar day back across DST transitions.
Europe/Berlin springs forward on 2026-03-29 (02:00→03:00).  Just
after midnight on the 30th, yesterday arithmetic that reuses the
current numeric offset encodes the 29th's wall time an hour early and
computes March 28 — grouping the 29th's late sessions as This Week
or Older instead of Yesterday.  Noon-anchored calendar arithmetic
asks the local zone for the target date itself."
  (let ((saved (getenv "TZ")))
    (unwind-protect
        (progn
          (set-time-zone-rule "Europe/Berlin")
          ;; Now: Mon 2026-03-30 00:30 CEST, the hour after the night the
          ;; clocks jumped.  Yesterday is Sun 2026-03-29, the transition day.
          (let* ((now (encode-time 30 0 0 30 3 2026))
                 (ts (pilish-test--local-time-iso
                      (encode-time 0 0 23 29 3 2026))))
            (should (equal (pilish--session-time-group ts now)
                           "Yesterday")))
          ;; Fall-back edge: 2026-10-25 (03:00→02:00); now Mon Oct 26 00:30.
          (let* ((now (encode-time 30 0 0 26 10 2026))
                 (ts (pilish-test--local-time-iso
                      (encode-time 0 0 23 25 10 2026))))
            (should (equal (pilish--session-time-group ts now)
                           "Yesterday"))))
      (if saved
          (set-time-zone-rule saved)
        (set-time-zone-rule nil)))))

(ert-deftest pilish-test-session-time-group-future-dates ()
  "A later calendar date groups as Future, whatever the week.
Clock-skewed future mtimes otherwise land in This Week — repeating
the heading noncontiguously between Today and Yesterday — or in
Older for a future date next week.  A same-date future hour stays
Today: the label, like the others, is calendar-semantic."
  (let* ((now (encode-time '(0 0 8 11 3 2026 nil nil nil)))
         (group (lambda (hms)
                   (pilish--session-time-group
                    (pilish-test--local-time-iso (apply #'encode-time hms))
                    now))))
    ;; Same date, hours ahead: Today, not Future.
    (should (equal (funcall group '(0 0 20 11 3 2026)) "Today"))
    ;; Tomorrow: Future.
    (should (equal (funcall group '(0 0 23 12 3 2026)) "Future"))
    ;; A future date in the next ISO week: Future, never Older.
    (should (equal (funcall group '(0 0 9 18 3 2026)) "Future"))))

;;;; Session Browser Rendering

(ert-deftest pilish-test-session-browser-render-flat ()
  "Render sessions as flat list in a buffer."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :firstMessage "Fix the bug"
                  :messageCount 10 :modified "2026-02-23T10:00:00Z")))
    (setq pilish--session-browser-view 'messages)
    (pilish--session-browser-rerender)
    ;; Buffer should contain session names
    (should (string-match-p "Session A" (buffer-string)))
    (should (string-match-p "Fix the bug" (buffer-string)))
    ;; Session A has more messages, so it comes first under Most messages
    (let ((pos-a (string-match "Session A" (buffer-string)))
          (pos-b (string-match "Fix the bug" (buffer-string))))
      (should (< pos-a pos-b)))
    ;; Count and age should NOT be in buffer text (they're in margins)
    (should-not (string-match-p "42 msgs" (buffer-string)))
    (should-not (string-match-p "10 msgs" (buffer-string)))))

(ert-deftest pilish-test-session-browser-render-threaded ()
  "Render sessions with threading connectors."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq pilish--session-browser-view 'threaded)
    (pilish--session-browser-rerender)
    ;; Should contain threading connector
    (should (string-match-p "└─" (buffer-string)))
    ;; Parent before child
    (let ((pos-p (string-match "Parent Session" (buffer-string)))
          (pos-c (string-match "Child branch" (buffer-string))))
      (should (< pos-p pos-c)))))

(ert-deftest pilish-test-session-browser-fork-prefix-flat ()
  "Forked sessions show `fork:' prefix in non-threaded modes."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq pilish--session-browser-view 'messages)
    (pilish--session-browser-rerender)
    ;; Fork prefix should appear before child session
    (should (string-match-p "fork:" (buffer-string)))
    ;; But NOT before parent
    (let ((text (buffer-string)))
      (should-not (string-match-p "fork:.*Parent Session" text)))))

(ert-deftest pilish-test-session-browser-fork-prefix-threaded ()
  "Forked sessions do NOT show `fork:' prefix in threaded mode."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq pilish--session-browser-view 'threaded)
    (pilish--session-browser-rerender)
    ;; Threading connector should appear, but NOT fork: prefix
    (should (string-match-p "└─" (buffer-string)))
    (should-not (string-match-p "fork:" (buffer-string)))))

;;;; All-Projects Project Identity and Bounded Tokens

(defconst pilish-test--token-width 16
  "Display width of the project token field on All-projects rows.")

(defun pilish-test--project-item (path cwd name)
  "Return a loaded-like browse item for PATH, CWD, and NAME.
Real scan results already carry their canonical session key, so the
fixture does too and project-field tests perform no incidental
filesystem canonicalization."
  (list :path path :canonicalPath path :cwd cwd :name name
        :messageCount 1 :modified "2026-03-11T10:00:00Z"))

(defun pilish-test--pad-display (string width)
  "Return STRING left-justified by spaces to display WIDTH columns."
  (concat string (make-string (max 0 (- width (string-width string)))
                              ?\s)))

(defun pilish-test--row (token title &optional live)
  "Return the expected All-projects row prefix for TOKEN and TITLE.
LIVE non-nil marks the row live.  The row is the fixed-width token
field, the two-column live field, then the title."
  (concat (pilish-test--pad-display token pilish-test--token-width)
          (if live "● " "  ")
          title))

(defun pilish-test--display-prefix (line width)
  "Return LINE's prefix occupying the first WIDTH display columns."
  (let ((chars nil) (w 0))
    (catch 'done
      (dotimes (i (length line))
        (let ((cw (string-width (substring line i (1+ i)))))
          (when (> (+ w cw) width) (throw 'done nil))
          (push (aref line i) chars)
          (setq w (+ w cw)))))
    (concat (nreverse chars))))

(ert-deftest pilish-test-session-browser-all-scope-token-field-layout ()
  "All-projects rows carry a bounded, globally unique project token
and the live marker before any unbounded content, on a fixed layout:
the token field is padded to a fixed display width so connector
indentation and titles start at the same column on every row, and
depth-18 connectors, long common-prefix labels, wide project names,
and long titles can never push identity or live status out of a
32-column body."
  (let* ((deep-dir "/home/u/long-common-prefix-abcdef")
         (items
          (append
           ;; Two over-bound labels sharing a long common prefix.
           (list (pilish-test--project-item
                  "/a/one.jsonl" (concat deep-dir "/app") "One")
                 (pilish-test--project-item
                  "/a/two.jsonl" "/home/u/long-common-prefix-abcdeg/app"
                  "Two"))
           ;; A wide-character project name.
           (list (pilish-test--project-item
                  "/a/wide.jsonl" "/home/u/\u4e2d\u6587\u9879\u76ee"
                  "Wide"))
           ;; A depth-18 fork chain under the first project's session.
           (let ((chain nil) (parent "/a/one.jsonl"))
             (dotimes (i 18)
               (let ((path (format "/a/deep-%02d.jsonl" i)))
                 (push (list :path path
                             :cwd (concat deep-dir "/app")
                             :name (format "Deep %d" i)
                             :parentSessionPath parent
                             :messageCount 1
                             :modified "2026-03-11T10:00:00Z")
                       chain)
                 (setq parent path)))
             (nreverse chain)))))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-scope 'all
            pilish--session-browser-items items)
      (pilish--session-browser-rerender)
      (let* ((text (buffer-string))
             (rows (seq-remove
                    (lambda (line)
                      (or (string-empty-p line)
                          ;; Time-group and time rows are absent here;
                          ;; every rendered line is a session row.
                          (not (string-match-p
                                "One\\|Two\\|Wide\\|Deep" line))))
                    (split-string text "\n"))))
        (should (= (length rows) (length items)))
        (dolist (line rows)
          ;; The token occupies exactly the fixed display width and is
          ;; itself within it — never truncated, never wider.
          (let* ((token (pilish-test--display-prefix
                         line pilish-test--token-width))
                 (field (pilish-test--display-prefix
                         line (+ pilish-test--token-width 2))))
            (should (= (string-width token) pilish-test--token-width))
            ;; The two-column live field follows at a constant column,
            ;; before any connector or title.
            (should (member (substring field (length token))
                            '("● " "  ")))))))))

(ert-deftest pilish-test-session-browser-all-scope-token-field-live-first ()
  "The live marker sits in its fixed field before connectors and
titles, so a depth-18 live fork stays visibly live in a narrow body,
and non-live rows keep the same connector column."
  (let* ((path "/test/live-deep.jsonl")
         (chat-buf (generate-new-buffer "*pilish-test-token-live*"))
         (proc (start-process "pilish-token-live" nil "sleep" "30"))
         (parent "/test/p0.jsonl")
         (chain nil))
    (dotimes (i 17)
      (let ((p (format "/test/p%d.jsonl" (1+ i))))
        (push (list :path p :cwd "/home/u/site" :name (format "Anc %d" i)
                    :parentSessionPath parent :messageCount 1
                    :modified "2026-03-11T10:00:00Z")
              chain)
        (setq parent p)))
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'pilish-chat-buffer chat-buf)
    (with-current-buffer chat-buf
      (setq pilish--process proc
            pilish--state (list :session-file path)))
    (unwind-protect
        (with-temp-buffer
          (pilish-session-browser-mode)
          (setq pilish--session-browser-scope 'all
                pilish--session-browser-items
                (append (nreverse chain)
                        (list (list :path path :cwd "/home/u/site"
                                    :name "Live deep" :messageCount 1
                                    :modified "2026-03-11T10:00:00Z"))))
          (setq pilish--session-browser-view 'threaded)
          (pilish--session-browser-rerender)
          (let* ((text (buffer-string))
                 (line (cl-find "Live deep"
                                (split-string text "\n")
                                :test #'string-match-p)))
            (should line)
            ;; Marker inside the fixed field at the constant column.
            (should (equal (substring-no-properties
                            line pilish-test--token-width
                            (+ pilish-test--token-width 2))
                           "\u25cf "))
            ;; The depth-18 connector follows the field, and the
            ;; identity token stays fully inside the field.
            (should (string-match-p
                     (format "^%s"
                             (regexp-quote
                              (pilish-test--display-prefix
                               line pilish-test--token-width)))
                     line))))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-session-browser-all-scope-tokens-natural ()
  "Within the bound, tokens stay human-readable compact labels:
unique projects keep the bare name, colliding basenames grow parent
components, and the root labels as /."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item
                 "/a/one.jsonl" "/home/u/client-a/app" "One")
                (pilish-test--project-item
                 "/a/two.jsonl" "/home/u/client-b/app" "Two")
                (pilish-test--project-item "/a/three.jsonl"
                                           "/home/u/site" "Three")
                (pilish-test--project-item "/a/r.jsonl" "/" "Root")))
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p
               (regexp-quote (pilish-test--row "client-a/app" "One"))
               text))
      (should (string-match-p
               (regexp-quote (pilish-test--row "client-b/app" "Two"))
               text))
      (should (string-match-p
               (regexp-quote (pilish-test--row "site" "Three")) text))
      (should (string-match-p
               (regexp-quote (pilish-test--row "/" "Root")) text)))))

(ert-deftest pilish-test-session-browser-all-scope-tokens-filter-stable ()
  "Tokens are built from all loaded projects, not filtered rows, so a
query never silently relabels the rows it leaves behind."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item
                 "/a/one.jsonl" "/home/u/client-a/app" "Unique words One")
                (pilish-test--project-item
                 "/a/two.jsonl" "/home/u/client-b/app" "Other words Two"))
          pilish--session-browser-search-query "Unique"
          pilish--session-browser-search-tokens '("Unique"))
    (unwind-protect
        (progn
          (pilish--session-browser-rerender)
          (should (string-match-p
                   (regexp-quote
                    (pilish-test--row "client-a/app" "Unique words One"))
                   (buffer-string)))
          (should-not (string-match-p "Other words" (buffer-string))))
      (setq pilish--session-browser-search-query nil
            pilish--session-browser-search-tokens nil))))

(ert-deftest pilish-test-session-browser-all-scope-tokens-unique ()
  "Final identity is the exact padded 16-column field, checked
globally: a project name with a trailing space pads to the same
field as its unspaced twin, so BOTH go to the generated class with
distinct ordinals — assertions never trim away the distinction."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item
                 "/a/plain.jsonl" "/home/u/app" "Plain")
                (pilish-test--project-item
                 "/a/spaced.jsonl" "/home/u/app " "Trailing space")))
    (pilish--session-browser-rerender)
    (let* ((text (buffer-string))
           (fields
            (delq nil
                  (mapcar (lambda (line)
                            (and (string-match-p "Plain\\|Trailing space"
                                                 line)
                                 (pilish-test--display-prefix
                                  line pilish-test--token-width)))
                          (split-string text "\n")))))
      ;; Both rows render, with distinct exact fields — no trimming.
      (should (= (length fields) 2))
      (should (= (length (delete-dups fields)) 2))
      ;; Neither field is the plain readable name: the collision
      ;; moved both twins to the generated namespace.
      (dolist (field fields)
        (should (string-prefix-p "#" field))
        (should (string-match-p "\\`#[0-9a-z]\\{1,\\} " field)))
      ;; The ordinals differ.
      (should-not (equal (nth 0 fields) (nth 1 fields))))))

(ert-deftest pilish-test-session-browser-all-scope-tokens-deterministic ()
  "The identity-to-field mapping depends only on the loaded set, not
on hash-table iteration or input order: reversed input yields the
identical mapping, and a readable-collision group never lets an
arbitrary winner keep the natural spelling."
  (let ((fields-for
         (lambda (items)
           (with-temp-buffer
             (pilish-session-browser-mode)
             (setq pilish--session-browser-scope 'all
                   pilish--session-browser-items items)
             (pilish--session-browser-rerender)
             (let ((map nil))
               (dolist (line (split-string (buffer-string) "\n"))
                 (dolist (title '("One" "Two" "Three"))
                   (when (string-match-p
                          (concat (regexp-quote title) "\\'") line)
                     (push (cons title
                                 (pilish-test--display-prefix
                                  line pilish-test--token-width))
                           map))))
               (sort map (lambda (a b) (string< (car a) (car b)))))))))
    (let* ((items (list (pilish-test--project-item
                         "/a/one.jsonl" "/home/u/client-a/app" "One")
                        (pilish-test--project-item
                         "/a/two.jsonl" "/home/u/client-b/app" "Two")
                        (pilish-test--project-item
                         "/a/three.jsonl" "/home/u/site" "Three")))
           (forward (funcall fields-for items))
           (reversed (funcall fields-for (reverse items))))
      (should (= (length forward) 3))
      (should (equal forward reversed))
      ;; Readable distinct labels survive; collisions resolved by
      ;; parents — the mapping itself, in both orders.
      (should (equal (cdr (assoc "One" forward))
                     (pilish-test--pad-display
                      "client-a/app" pilish-test--token-width)))
      (should (equal (cdr (assoc "Three" forward))
                     (pilish-test--pad-display
                      "site" pilish-test--token-width))))))

(ert-deftest pilish-test-session-browser-all-scope-tokens-generated-ordinals ()
  "Generated tokens are `#ORD tail' with fixed-width base-36
ordinals over sorted identities: over-bound labels and natural
spelling collisions all land there together, ordered and unique."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item
                 "/a/x.jsonl" "/home/u/long-common-prefix-abcdef/app"
                 "X")
                (pilish-test--project-item
                 "/a/y.jsonl" "/home/u/long-common-prefix-abcdeg/app"
                 "Y")
                (pilish-test--project-item
                 "/a/nat.jsonl" "/home/u/#nat" "Hash-named")
                (pilish-test--project-item
                 "/a/z.jsonl" "/home/u/site" "Z")))
    (pilish--session-browser-rerender)
    (let* ((text (buffer-string))
           (fields
            (delq nil
                  (mapcar (lambda (line)
                            (and (string-match-p "X\\'\\|Y\\'\\|Hash-named\\'\\|Z\\'"
                                                 line)
                                 (pilish-test--display-prefix
                                  line pilish-test--token-width)))
                          (split-string text "\n")))))
      (should (= (length fields) 4))
      (should (= (length (delete-dups fields)) 4))
      ;; The unique natural project keeps its readable field.
      (should (member (pilish-test--pad-display
                       "site" pilish-test--token-width)
                      fields))
      ;; Over-bound labels and the #-leading reserved spelling go to
      ;; the generated namespace — three ordinal tokens, sorted
      ;; identities meaning ascending ordinals, distinct fields.
      (let ((generated (seq-filter
                        (lambda (f) (string-prefix-p "#" f)) fields)))
        (should (= (length generated) 3))
        (dolist (g generated)
          (should (string-match-p "\\`#[0-9a-z]\\{1,\\} " g)))
        (should (= (length (delete-dups generated)) 3))))))

(ert-deftest pilish-test-session-route-host ()
  "Parse every route hop with Emacs 30's default TRAMP lexical grammar.
The production parser remains pure string syntax, but its method,
user, host, numeric-port, and bracketed-IPv6 boundaries match the
installed grammar: a greedy user ends at the last @, so @ and # may
occur inside it.  Every hop is validated, a first host is explicit,
and later hostless hops inherit the nearest explicit host."
  ;; Any `file-remote-p' call would let machine-local TRAMP defaults
  ;; determine the answer; this helper must use only ROUTE's text.
  (cl-letf (((symbol-function 'file-remote-p)
             (lambda (&rest _)
               (ert-fail "route host parser consulted file-remote-p"))))
    ;; Valid table includes all established routes plus the grammar
    ;; boundaries confirmed against `tramp-dissect-file-name'.
    (dolist (case '(("/ssh:host:" "host")
                    ("/ssh:h:" "h")
                    ("/ssh:user@host:" "host")
                    ("/ssh:user@example.com@host:" "host")
                    ("/ssh:user#tag@host:" "host")
                    ("/ssh:user#tag@host#22:" "host")
                    ("/ssh:user!tag@host:" "host")
                    ("/ssh:host#2222:" "host")
                    ("/-:h:" "h")
                    ("/äx:h:" "h")
                    ("/ssh:b|sudo:root@host:" "host")
                    ("/ssh:[::1]:" "[::1]")
                    ("/ssh:b|sudo::" "b")
                    ("/ssh:u@b#22|sudo:root@:" "b")
                    ("/ssh:u@[2001:db8::1]#22|sudo::"
                     "[2001:db8::1]")))
      (should (equal (pilish--session-route-host (car case))
                     (cadr case))))
    ;; Required incompatibility repros plus malformed whole-hop cases.
    (dolist (route '("/ssh:host#ssh:"
                     "/ssh:host#:"
                     "/ssh:host#22x:"
                     "/s:h:"
                     "/!:h:"
                     "/ssh:foo!bar:"
                     "/ssh:[garbage]:"
                     "/ssh:[fe80::1%eth0]:"
                     "/ssh:|sudo:root@h:"
                     "/ssh:h|bad|sudo:x:"
                     "/ssh::"))
      (should-not (pilish--session-route-host route)))
    ;; Unsafe/default-invisible route text never becomes a field label.
    (dolist (route (list "/ssh:u\t@h:"
                         (concat "/ssh:h" (string #x202e) ":")
                         (concat "/ssh:h" (string #x034f) ":")))
      (should-not (pilish--session-route-host route)))
    (should (equal (pilish--session-route-host
                    "/ssh:no-such.invalid:")
                   "no-such.invalid"))))

(ert-deftest pilish-test-session-ordinal36-dynamic-width ()
  "One-based ordinals widen at exact powers instead of truncating.
There are only 35 positive one-digit base-36 ordinals and 1295
positive two-digit ordinals because zero is not allocated."
  (should (equal (mapcar #'pilish--session-ordinal-width
                         '(35 36 37 1295 1296 1297))
                 '(1 2 2 2 3 3)))
  (should (equal (pilish--session-ordinal36 1 1) "#1"))
  (should (equal (pilish--session-ordinal36 35 1) "#z"))
  (should (equal (pilish--session-ordinal36 36 2) "#10"))
  (should (equal (pilish--session-ordinal36 1295 2) "#zz"))
  (should (equal (pilish--session-ordinal36 1296 3) "#100"))
  ;; A wide case proven at the helper seam: 36^5 needs six digits.
  (should (equal (pilish--session-ordinal36 (expt 36 5) 6) "#100000"))
  (should-error (pilish--session-ordinal36 36 1)))

(defun pilish-test--generated-project-items (count)
  "Return COUNT loaded-like items that all require generated tokens.
Stored canonical paths avoid filesystem work, as real scanned items
already carry them; unique over-width basenames force the production
allocator's generated-token path."
  (let (items)
    (dotimes (i count (nreverse items))
      (let ((path (format "/sessions/boundary-%04d.jsonl" i)))
        (push (list :path path :canonicalPath path
                    :cwd (format
                          "/projects/project-%04d-abcdefghijklmnop" i)
                    :name (format "Boundary %04d" i)
                    :messageCount 1
                    :modified "2026-03-11T10:00:00Z")
              items)))))

(defun pilish-test--allocated-project-fields (items)
  "Return ITEMS' sorted key-to-exact-field allocation."
  (let ((table (pilish--session-project-fields items))
        result)
    (dolist (item items)
      (let ((key (pilish--session-item-key item)))
        (push (cons key
                    (pilish--session-pad-display
                     (gethash key table) pilish-test--token-width))
              result)))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun pilish-test--assert-generated-project-boundary (count ordinal-width)
  "Assert allocation and rendering at COUNT with ORDINAL-WIDTH digits."
  (let* ((items (pilish-test--generated-project-items count))
         (forward (pilish-test--allocated-project-fields items))
         (reversed (pilish-test--allocated-project-fields
                    (reverse items)))
         (fields (mapcar #'cdr forward))
         (ordinal-re (format "\\`#[0-9a-z]\\{%d\\} " ordinal-width)))
    ;; Allocation is input-order independent and every final padded
    ;; field is exactly 16 display columns, generated, and unique.
    (should (= (length forward) count))
    (should (equal forward reversed))
    (should (= (length (delete-dups (copy-sequence fields))) count))
    (dolist (field fields)
      (should (= (string-width field) pilish-test--token-width))
      (should (string-match-p ordinal-re field)))
    ;; Exercise the full browser rendering seam as well as allocation.
    ;; A rollover error used to erase the buffer before signaling.
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-scope 'all
            pilish--session-browser-view 'messages
            pilish--session-browser-items items)
      (pilish--session-browser-rerender)
      (should-not (string-empty-p (buffer-string)))
      (let* ((rows (seq-remove #'string-empty-p
                               (split-string (buffer-string) "\n")))
             (rendered-fields
              (mapcar (lambda (line)
                        (pilish-test--display-prefix
                         line pilish-test--token-width))
                      rows)))
        (should (= (length rows) count))
        (should (= (length (delete-dups
                            (copy-sequence rendered-fields)))
                   count))
        (should (equal (sort rendered-fields #'string<)
                       (sort (copy-sequence fields) #'string<)))))))

(ert-deftest pilish-test-session-browser-generated-ordinal-lower-boundaries ()
  "Render 35, 36, and 37 generated projects across one-digit rollover."
  (dolist (case '((35 1) (36 2) (37 2)))
    (pilish-test--assert-generated-project-boundary
     (nth 0 case) (nth 1 case))))

(ert-deftest pilish-test-session-browser-generated-ordinal-upper-boundaries ()
  "Render 1295, 1296, and 1297 projects across two-digit rollover."
  (dolist (case '((1295 2) (1296 3) (1297 3)))
    (pilish-test--assert-generated-project-boundary
     (nth 0 case) (nth 1 case))))

(ert-deftest pilish-test-session-browser-project-fields-visible-equivalence ()
  "Visually unsafe or equivalent readable labels become generated fields.
A whitespace-only basename cannot produce a blank natural field; a
zero-width combining grapheme joiner cannot hide an identity suffix;
and canonically equivalent NFC/NFD labels cannot receive visually
identical natural fields.  Their legal cwd identities remain distinct,
while exact 16-column output stays unique and input-order independent."
  (let* ((items
          (list (pilish-test--project-item
                 "/a/space.jsonl" "/ " "Whitespace")
                (pilish-test--project-item
                 "/a/plain.jsonl" "/x" "Plain x")
                (pilish-test--project-item
                 "/a/cgj.jsonl" (concat "/x" (string #x034f)) "CGJ x")
                (pilish-test--project-item
                 "/a/nfc.jsonl" "/é" "NFC e")
                (pilish-test--project-item
                 "/a/nfd.jsonl" (concat "/e" (string #x0301)) "NFD e")))
         (forward (pilish-test--allocated-project-fields items))
         (reversed (pilish-test--allocated-project-fields
                    (reverse items)))
         (fields (mapcar #'cdr forward)))
    (should (equal forward reversed))
    (should (= (length (delete-dups (copy-sequence fields))) 5))
    (dolist (field fields)
      (should (= (string-width field) pilish-test--token-width)))
    (dolist (path '("/a/space.jsonl" "/a/cgj.jsonl"
                    "/a/nfc.jsonl" "/a/nfd.jsonl"))
      (should (string-prefix-p "#" (cdr (assoc path forward)))))
    ;; The safe, noncolliding plain label remains readable.
    (should (equal (cdr (assoc "/a/plain.jsonl" forward))
                   (pilish-test--pad-display
                    "x" pilish-test--token-width)))
    ;; The production renderer emits the same exact unique fields.
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-scope 'all
            pilish--session-browser-view 'messages
            pilish--session-browser-items items)
      (pilish--session-browser-rerender)
      (let ((rendered
             (mapcar (lambda (line)
                       (pilish-test--display-prefix
                        line pilish-test--token-width))
                     (seq-remove #'string-empty-p
                                 (split-string (buffer-string) "\n")))))
        (should (= (length rendered) 5))
        (should (= (length (delete-dups (copy-sequence rendered))) 5))
        (should (equal (sort rendered #'string<)
                       (sort (copy-sequence fields) #'string<)))))))

(ert-deftest pilish-test-session-browser-project-fields-compatibility-blanks ()
  "Blank/filler glyphs and compatibility-equivalent labels are generated.
Unicode names ending in BLANK or FILLER identify conservative
blank-looking natural labels even when their general category and
column width look printable.  NFKC display keys also group ordinary
space and EN SPACE spellings, and reserve compatibility variants of
`#' from colliding with generated ordinals.  Legal cwd identities stay
distinct, and every exact padded field remains deterministic and unique."
  (let* ((en-space (string #x2002))
         (items
          (list (pilish-test--project-item
                 "/b/braille.jsonl" (concat "/" (string #x2800))
                 "Braille blank")
                (pilish-test--project-item
                 "/b/hangul.jsonl" (concat "/" (string #x3164))
                 "Hangul filler")
                (pilish-test--project-item
                 "/b/choseong.jsonl" (concat "/" (string #x115f))
                 "Choseong filler")
                (pilish-test--project-item
                 "/b/halfwidth.jsonl" (concat "/" (string #xffa0))
                 "Halfwidth filler")
                (pilish-test--project-item
                 "/b/space.jsonl" "/a b" "ASCII space")
                (pilish-test--project-item
                 "/b/en-space.jsonl" (concat "/a" en-space "b")
                 "EN SPACE")
                (pilish-test--project-item
                 "/b/question.jsonl" "/?" "Question placeholder")
                (pilish-test--project-item
                 "/b/compat-hash.jsonl"
                 (concat "/" (string #xfe5f) "1 ?")
                 "Compatibility hash")
                (pilish-test--project-item
                 "/b/plain.jsonl" "/plain" "Plain")))
         (forward (pilish-test--allocated-project-fields items))
         (reversed (pilish-test--allocated-project-fields (reverse items)))
         (generated-paths '("/b/braille.jsonl" "/b/hangul.jsonl"
                            "/b/choseong.jsonl" "/b/halfwidth.jsonl"
                            "/b/space.jsonl" "/b/en-space.jsonl"
                            "/b/question.jsonl"
                            "/b/compat-hash.jsonl"))
         (fields (mapcar #'cdr forward)))
    (should (equal forward reversed))
    (should (= (length (delete-dups (copy-sequence fields)))
               (length items)))
    (should (= (length (delete-dups
                        (mapcar #'ucs-normalize-NFKC-string fields)))
               (length items)))
    (dolist (field fields)
      (should (= (string-width field) pilish-test--token-width)))
    (dolist (path generated-paths)
      (should (string-prefix-p "#" (cdr (assoc path forward)))))
    (should (equal (cdr (assoc "/b/plain.jsonl" forward))
                   (pilish-test--pad-display
                    "plain" pilish-test--token-width)))
    ;; Assert the full renderer emits exactly the allocator's fields.
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-scope 'all
            pilish--session-browser-view 'messages
            pilish--session-browser-items items)
      (pilish--session-browser-rerender)
      (let ((rendered
             (mapcar (lambda (line)
                       (pilish-test--display-prefix
                        line pilish-test--token-width))
                     (seq-remove #'string-empty-p
                                 (split-string (buffer-string) "\n")))))
        (should (equal (sort rendered #'string<)
                       (sort (copy-sequence fields) #'string<)))))))

(ert-deftest pilish-test-session-browser-all-scope-placeholder-field-reserved ()
  "The placeholder's exact padded field is reserved like the #
namespace: a project literally named ? is real and gets a generated
token, while malformed rows keep the bare placeholder — their exact
fields differ."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item "/a/q.jsonl" "/?" "Question project")
                (pilish-test--project-item "/a/bad.jsonl" "not-absolute"
                                           "Malformed twin")))
    (pilish--session-browser-rerender)
    (let* ((text (buffer-string))
           (field-of
            (lambda (title)
              (pilish-test--display-prefix
               (cl-find title (split-string text "\n") :test #'string-match-p)
               pilish-test--token-width))))
      (let ((question (funcall field-of "Question project"))
            (malformed (funcall field-of "Malformed twin")))
        ;; The valid ? project is generated, not the placeholder.
        (should (string-prefix-p "#" question))
        (should (equal (string-trim-right malformed)
                       pilish--session-project-placeholder))
        (should-not (equal question malformed))))))

(ert-deftest pilish-test-session-browser-all-scope-cwd-tramp-localnames ()
  "A TRAMP-spelled cwd splits into route and localname first and the
LOCALNAME is validated: an empty localname (/ssh:h:) and an explicit
root (/ssh:h:/) are different spellings and only the root is a
usable project; a relative localname and a remote-home spelling are
rejected.  A single-hop route with an empty host would be completed
from TRAMP's local defaults — an identity that shifts with
configuration — and yields the truthful placeholder instead.
Rejected rows still reserve the full bounded token field."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item
                 "/ssh:h:/s/a.jsonl" "/ssh:h:relative" "Relative localname")
                (pilish-test--project-item
                 "/ssh:h:/s/b.jsonl" "/ssh:h:~/proj" "Remote home")
                (pilish-test--project-item
                 "/ssh:h:/s/c.jsonl" "/ssh:h:" "Empty localname")
                (pilish-test--project-item
                 "/ssh:h:/s/d.jsonl" "/ssh:h:/" "Explicit root")
                (pilish-test--project-item
                 "/ssh::/s/e.jsonl" "/ssh::/x/app" "Empty host")))
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      ;; Rejected metadata still gets the bounded placeholder field —
      ;; the rows are uniform, never bare titles.
      (dolist (title '("Relative localname" "Remote home"
                       "Empty localname" "Empty host"))
        (should (string-match-p
                 (regexp-quote (pilish-test--row "?" title)) text)))
      (should-not (string-match-p ":app" text))
      ;; The explicit remote root is a real project: host:/.
      (should (string-match-p
               (regexp-quote (pilish-test--row "h:/" "Explicit root"))
               text)))))

(ert-deftest pilish-test-session-browser-all-scope-cwd-tramp-inherited-host ()
  "Hostless final hops render the preceding explicit route host.
Both ordinary and user/port predecessors inherit lexically, as does
a bracketed IPv6 predecessor; the bounded field shows that host
rather than the rejected-project placeholder."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item
                 "/ssh:b|sudo::/s/a.jsonl"
                 "/ssh:b|sudo::/work/bare" "Hostless sudo")
                (pilish-test--project-item
                 "/ssh:u@b#22|sudo:root@:/s/b.jsonl" "/work/user-port"
                 "Hostless sudo user")
                (pilish-test--project-item
                 "/ssh:[::1]|sudo::/s/c.jsonl"
                 "/ssh:[::1]|sudo::/work/v6" "Hostless sudo IPv6")))
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p
               (regexp-quote
                (pilish-test--row "b:bare" "Hostless sudo"))
               text))
      (should (string-match-p
               (regexp-quote
                (pilish-test--row "b:user-port" "Hostless sudo user"))
               text))
      (should (string-match-p
               (regexp-quote
                (pilish-test--row "[::1]:v6" "Hostless sudo IPv6"))
               text))
      (should-not (string-match-p
                   (regexp-quote (pilish-test--row "?" "Hostless"))
                   text)))))

(ert-deftest pilish-test-session-browser-all-scope-cwd-tramp-invalid-routes ()
  "Whole-route validation rejects contextual or unsafe project hosts.
Direct project specs, allocated fields, and rendered rows all use
the placeholder for a hostless first hop, malformed middle hop, or
unsafe/default-invisible route text.  A valid inherited route remains
usable, and no TRAMP configuration, network, or filesystem function is
consulted anywhere along this loaded-item rendering path."
  (let* ((unsafe-bidi (concat "/ssh:h" (string #x202e) ":/s/bidi.jsonl"))
         (unsafe-cgj (concat "/ssh:h" (string #x034f) ":/s/cgj.jsonl"))
         (items
          (list (pilish-test--project-item
                 "/ssh:b|sudo::/s/good.jsonl"
                 "/ssh:b|sudo::/work/good" "Valid inherited")
                (pilish-test--project-item
                 "/ssh:|sudo:root@h:/s/default.jsonl"
                 "/work/default" "Default first hop")
                (pilish-test--project-item
                 "/ssh:h|bad|sudo:x:/s/malformed.jsonl"
                 "/work/malformed" "Malformed middle hop")
                (pilish-test--project-item
                 "/ssh:u\t@h:/s/tab.jsonl" "/work/tab" "Tab route")
                (pilish-test--project-item
                 unsafe-bidi "/work/bidi" "Bidi route")
                (pilish-test--project-item
                 unsafe-cgj "/work/cgj" "Invisible route"))))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (&rest _)
                 (ert-fail "project fields consulted TRAMP")))
              ((symbol-function 'file-truename)
               (lambda (&rest _)
                 (ert-fail "loaded project fields touched filesystem"))))
      (should (equal (pilish--session-project-spec (car items))
                     '("/ssh:b|sudo::/work/good" "b" ("work" "good"))))
      ;; A colon in a real local POSIX component remains legal; only a
      ;; TRAMP-routed session treats a malformed route-looking cwd as such.
      (should (equal
               (pilish--session-project-spec
                (pilish-test--project-item
                 "/local/session.jsonl" "/foo:bar/app" "Local colon"))
               '("/foo:bar/app" nil ("foo:bar" "app"))))
      (dolist (item (cdr items))
        (should-not (pilish--session-project-spec item)))
      (let ((fields (pilish--session-project-fields items)))
        (should (equal (gethash "/ssh:b|sudo::/s/good.jsonl" fields)
                       "b:good"))
        (dolist (item (cdr items))
          (should (equal (gethash (plist-get item :canonicalPath) fields)
                         pilish--session-project-placeholder))))
      (with-temp-buffer
        (pilish-session-browser-mode)
        (setq pilish--session-browser-scope 'all
              pilish--session-browser-view 'messages
              pilish--session-browser-items items)
        (pilish--session-browser-rerender)
        (let ((text (buffer-string)))
          (should (string-match-p
                   (regexp-quote
                    (pilish-test--row "b:good" "Valid inherited"))
                   text))
          (dolist (title '("Default first hop" "Malformed middle hop"
                           "Tab route" "Bidi route" "Invisible route"))
            (should (string-match-p
                     (regexp-quote (pilish-test--row "?" title))
                     text))))))))

(ert-deftest pilish-test-session-project-cwd-local-symlink-identity ()
  "Scan enrichment canonicalizes local cwd aliases to one project.
Two real session rows retain distinct session identities, but a real
directory and symlink spelling of the same cwd receive one canonical
project spec and the same rendered token."
  (let* ((base (pilish-test--make-temp-directory "pi-project-alias-"))
         (real (expand-file-name "real/app" base))
         (alias (expand-file-name "alias" base)))
    (unwind-protect
        (progn
          (make-directory real t)
          (make-symbolic-link (expand-file-name "real" base) alias)
          (let* ((a (pilish--session-enrich-item
                     (pilish-test--project-item
                      (expand-file-name "a.jsonl" base)
                      real "Real cwd")))
                 (b (pilish--session-enrich-item
                     (pilish-test--project-item
                      (expand-file-name "b.jsonl" base)
                      (expand-file-name "app" alias) "Alias cwd")))
                 (items (list a b))
                 (a-spec (pilish--session-project-spec a))
                 (b-spec (pilish--session-project-spec b))
                 (fields (pilish--session-project-fields items)))
            (should (equal a-spec b-spec))
            (should (equal (gethash (pilish--session-item-key a) fields)
                           (gethash (pilish--session-item-key b) fields)))
            (with-temp-buffer
              (pilish-session-browser-mode)
              (setq pilish--session-browser-scope 'all
                    pilish--session-browser-view 'messages
                    pilish--session-browser-items items)
              (pilish--session-browser-rerender)
              (let ((text (buffer-string)))
                (should (string-match-p
                         (regexp-quote
                          (pilish-test--row "app" "Real cwd"))
                         text))
                (should (string-match-p
                         (regexp-quote
                          (pilish-test--row "app" "Alias cwd"))
                         text)))))
      (when (file-directory-p base)
        (delete-directory base t))))))

(ert-deftest pilish-test-session-project-cwd-symlink-before-dot-collapse ()
  "Resolve an ordinary local cwd's symlinks before its dot segments.
For a/link -> ../b/inner, a/link/../project names b/project under
kernel path-walk semantics; lexical collapse first would incorrectly
turn it into a/project."
  (let* ((base (pilish-test--make-temp-directory "pi-project-dotlink-"))
         (a (expand-file-name "a" base))
         (inner (expand-file-name "b/inner" base))
         (target (expand-file-name "b/project" base))
         (link (expand-file-name "link" a))
         ;; Do not use `expand-file-name' here: it would erase the
         ;; very dot segment whose ordering this regression exercises.
         (raw (concat link "/../project")))
    (unwind-protect
        (progn
          (make-directory a t)
          (make-directory inner t)
          (make-directory target t)
          (make-symbolic-link "../b/inner" link)
          (let* ((item (pilish--session-enrich-item
                        (pilish-test--project-item
                         (expand-file-name "session.jsonl" base)
                         raw "Symlink then dot")))
                 (spec (pilish--session-project-spec item))
                 (expected (file-truename target)))
            (should (equal (car spec) expected))
            (should (equal (nth 2 spec) (split-string expected "/" t)))
            (should-not (equal (car spec)
                               (expand-file-name "a/project" base)))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-project-cwd-handler-safe-boundary ()
  "Local cwd canonicalization cannot dispatch arbitrary file handlers.
A matching handler records no call.  Even a compromised/native
`file-truename' result that looks remote is rejected in favor of the
validated lexical local identity; remote, UNC, and Windows spellings
continue to bypass local canonicalization entirely."
  (let* ((base (pilish-test--make-temp-directory "pi-project-handler-"))
         (cwd (expand-file-name "handled/project" base))
         (calls 0)
         (handler
          (lambda (operation &rest _args)
            (cl-incf calls)
            (if (eq operation 'file-truename)
                "/ssh:handler.example:/escaped"
              (ert-fail (format "unexpected handler operation %S"
                                operation))))))
    (unwind-protect
        (progn
          (make-directory cwd t)
          (let ((file-name-handler-alist
                 (cons (cons (concat "\\`" (regexp-quote base)) handler)
                       file-name-handler-alist)))
            (should (equal (pilish--session-canonical-project-spec
                            (pilish-test--project-item
                             "/sessions/local.jsonl" cwd "Handled"))
                           (list cwd nil (split-string cwd "/" t))))
            (should (= calls 0)))
          ;; Result validation is independent of handler inhibition.
          (cl-letf (((symbol-function 'file-truename)
                     (lambda (_path) "/ssh:h:/escaped")))
            (should (equal (pilish--session-canonical-project-spec
                            (pilish-test--project-item
                             "/sessions/local.jsonl" cwd "Escaped"))
                           (list cwd nil (split-string cwd "/" t)))))
          ;; These lexical classes must never cross the local boundary.
          (cl-letf (((symbol-function 'file-truename)
                     (lambda (&rest _)
                       (ert-fail "nonlocal cwd reached file-truename"))))
            (dolist (item (list
                           (pilish-test--project-item
                            "/ssh:h:/s/x.jsonl" "/work/app" "Remote")
                           (pilish-test--project-item
                            "/s/u.jsonl" "//server/share/app" "UNC")
                           (pilish-test--project-item
                            "/s/w.jsonl" "C:/Users/u/app" "Windows")))
              (should (pilish--session-canonical-project-spec item)))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-project-cwd-canonical-fallback-and-remote ()
  "Local cwd canonicalization falls back lexically; remote cwd never stats.
The scan-enrichment seam attempts a local project cwd once and keeps
its normalized lexical spec when canonicalization fails.  A remote
route reaches the same seam without passing any remote spelling to
`file-truename'."
  (let ((calls nil))
    (cl-letf (((symbol-function 'file-truename)
               (lambda (path)
                 (push path calls)
                 (if (equal path "/project/alias")
                     (error "canonicalization failed")
                   path))))
      (let* ((item (pilish--session-enrich-item
                    (pilish-test--project-item
                     "/sessions/local.jsonl" "/project/alias" "Local")))
             (spec (pilish--session-project-spec item)))
        (should (member "/project/alias" calls))
        (should (equal spec
                       '("/project/alias" nil ("project" "alias")))))
      (setq calls nil)
      (let ((item (pilish--session-enrich-item
                   (pilish-test--project-item
                    "/ssh:h:/sessions/remote.jsonl"
                    "/project/remote" "Remote"))))
        (should (equal (pilish--session-project-spec item)
                       '("/ssh:h:/project/remote" "h"
                         ("project" "remote"))))
        (should-not calls)))))

(ert-deftest pilish-test-session-browser-all-scope-cwd-windows ()
  "Windows cwd spellings parse lexically with anchored dot-dot: the
drive and the UNC server/share never pop, a bare drive is
drive-relative and rejected while C:/ is the drive root, and ///a
collapses to the POSIX /a instead of a malformed UNC."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item
                 "/w/a.jsonl" "C:/Users/dan/app" "Drive slash")
                (pilish-test--project-item
                 "/w/b.jsonl" "C:\\Users\\dan\\app" "Drive backslash")
                (pilish-test--project-item
                 "/w/c.jsonl" "D:\\Users\\dan\\app" "Other drive")
                (pilish-test--project-item
                 "/w/d.jsonl" "C:/../x/app" "Anchored drive dotdot")
                (pilish-test--project-item
                 "/w/e.jsonl" "//server/share/../app" "Anchored UNC dotdot")
                (pilish-test--project-item
                 "/w/f.jsonl" "\\\\server\\share\\app" "UNC backslash")
                (pilish-test--project-item
                 "/w/g.jsonl" "//server/share/app" "UNC slash")
                (pilish-test--project-item
                 "/w/h.jsonl" "///lookalike" "Triple slash")
                (pilish-test--project-item
                 "/w/i.jsonl" "C:" "Bare drive")
                (pilish-test--project-item
                 "/w/j.jsonl" "/server/share/app" "POSIX lookalike")))
    (pilish--session-browser-rerender)
    (let* ((text (buffer-string))
           (token-of
            (lambda (title)
              (string-trim-right
               (pilish-test--display-prefix
                (cl-find title (split-string text "\n")
                         :test #'string-match-p)
                pilish-test--token-width)))))
      ;; C:/... and C:\... are one project sharing one token; the
      ;; colliding C:/D: basenames disambiguate through their drive
      ;; components; dot-dot never pops the drive.
      (should (equal (funcall token-of "Drive slash")
                     "C:/Users/dan/app"))
      (should (equal (funcall token-of "Drive backslash")
                     "C:/Users/dan/app"))
      (should (equal (funcall token-of "Other drive")
                     "D:/Users/dan/app"))
      ;; The shortest distinguishing label: x/app already separates
      ;; it from dan/app and share/app.
      (should (equal (funcall token-of "Anchored drive dotdot")
                     "x/app"))
      ;; UNC dot-dot never pops server/share: the row shares the
      ;; plain-UNC identity, generated token included.
      (should (equal (funcall token-of "Anchored UNC dotdot")
                     (funcall token-of "UNC backslash")))
      ;; /// collapses to POSIX /lookalike, whose unique basename
      ;; needs no parents to distinguish it here.
      (should (equal (funcall token-of "Triple slash") "lookalike"))
      ;; A bare drive is drive-relative: placeholder, not a project.
      (should (string-match-p
               (regexp-quote (pilish-test--row "?" "Bare drive")) text))
      ;; UNC and the rooted POSIX lookalike never alias: the whole
      ;; exhausted collision group — both of them — moves to the
      ;; generated namespace with distinct ordinals, and neither
      ;; aliases the other's identity.
      (let ((unc-b (funcall token-of "UNC backslash"))
            (unc-s (funcall token-of "UNC slash"))
            (posix (funcall token-of "POSIX lookalike")))
        (should (equal unc-b unc-s))
        (should (= (length (delete-dups (list unc-b posix))) 2))
        (should (equal 2 (cl-count-if
                          (lambda (tok)
                            (string-prefix-p "#" tok))
                          (list unc-b posix))))))))

(ert-deftest pilish-test-session-browser-all-scope-cwd-unc-anchors ()
  "UNC anchors distinguish normalization from malformed spellings.
Repeated separators after the two-separator introducer are an
accepted alias, while fewer than two nonempty anchors and dot or
dot-dot anchors are rejected rather than aliased to another share."
  (should (equal (pilish--session-cwd-parts "//server//share/app")
                 (pilish--session-cwd-parts "//server/share/app")))
  (should-not (pilish--session-cwd-parts "//server"))
  (should-not (pilish--session-cwd-parts "//server/"))
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-items
          (list (pilish-test--project-item "/u/a.jsonl" "//./share/app"
                                           "Dot server")
                (pilish-test--project-item "/u/b.jsonl" "//server/../app"
                                           "Dotdot share")
                (pilish-test--project-item "/u/c.jsonl" "//server/./app"
                                           "Dot share")
                (pilish-test--project-item "/u/d.jsonl" "//server"
                                           "Missing share")
                (pilish-test--project-item "/u/e.jsonl" "//server/share/app"
                                           "Valid UNC")
                (pilish-test--project-item "/u/f.jsonl" "//server//share/app"
                                           "Repeated separator")))
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (dolist (title '("Dot server" "Dotdot share" "Dot share"
                       "Missing share"))
        (should (string-match-p
                 (regexp-quote (pilish-test--row "?" title)) text)))
      ;; Repeated separators collapse to the canonical identity, so
      ;; both accepted rows use the same readable exact field.
      (should (string-match-p
               (regexp-quote (pilish-test--row "app" "Valid UNC"))
               text))
      (should (string-match-p
               (regexp-quote
                (pilish-test--row "app" "Repeated separator"))
               text))
      (should (= 2 (cl-count-if
                    (lambda (line)
                      (string-prefix-p "app " line))
                    (split-string text "\n")))))))

(ert-deftest pilish-test-session-browser-all-scope-cwd-unicode-categories ()
  "Unsafe display characters are rejected by Unicode general category
— Cc, Cf, Zl, Zp — not by hand-listed ranges: Arabic letter mark,
bidi isolates, and every previously listed control or format
character all suppress the token without breaking the row."
  (dolist (code (list #x00 #x0a #x7f #x061c #x2066 #x2069 #x206e
                      #x2028 #x2029 #x200b #x202e #x0085 #x009c
                      #xfeff))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-scope 'all
            pilish--session-browser-items
            (list (pilish-test--project-item
                   "/u/x.jsonl" (format "/home/u/p%sq" (string code))
                   (format "Code %04X" code))))
      (pilish--session-browser-rerender)
      ;; Rejected metadata gets the bounded placeholder field — the
      ;; row stays uniform and single-line, with no unsafe character.
      (let ((line (substring-no-properties (buffer-string))))
        (should (equal (pilish-test--row "?" (format "Code %04X" code))
                       (string-trim-right line "\n")))))))

(ert-deftest pilish-test-session-browser-margin-overlays ()
  "Session entries have right-margin overlays with count and age."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")))
    (setq pilish--session-browser-view 'messages)
    (pilish--session-browser-rerender)
    ;; Should have at least one overlay
    (let ((ovs (overlays-in (point-min) (point-max))))
      (should (> (length ovs) 0))
      ;; Find our margin overlay (has before-string with margin display)
      (let* ((margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs))
             (ov (car margin-ovs))
             (bs (overlay-get ov 'before-string))
             (display (get-text-property 0 'display bs))
             (content (cadr display)))
        (should (equal (car display) '(margin right-margin)))
        ;; Content should contain message count
        (should (string-match-p "42 msgs" content))))))

(ert-deftest pilish-test-session-browser-no-name-truncation ()
  "Session names are not truncated."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (let ((long-name (make-string 80 ?x)))
      (setq pilish--session-browser-items
            (list (list :path "/test/a.jsonl" :name long-name
                        :messageCount 1 :modified "2026-02-24T10:00:00Z")))
      (setq pilish--session-browser-view 'messages)
      (pilish--session-browser-rerender)
      ;; Full name should appear, not truncated
      (should (string-match-p long-name (buffer-string))))))

(ert-deftest pilish-test-session-browser-live-marker ()
  "Sessions open in a live Pilish process get the live marker; others do not.
Killing the process and rerendering drops the marker."
  (let* ((path "/test/live-session.jsonl")
         (chat-buf (generate-new-buffer "*pilish-test-live-marker-chat*"))
         (proc (start-process "pilish-live-marker-test" nil "sleep" "30")))
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'pilish-chat-buffer chat-buf)
    (with-current-buffer chat-buf
      (setq pilish--process proc
            pilish--state (list :session-file path)))
    (unwind-protect
        (with-temp-buffer
          (pilish-session-browser-mode)
          (setq pilish--session-browser-items
                (list (list :path path :name "Live session"
                            :messageCount 1 :modified "2026-02-24T10:00:00Z")
                      (list :path "/test/cold-session.jsonl" :name "Cold session"
                            :messageCount 1 :modified "2026-02-24T10:00:00Z")))
          (setq pilish--session-browser-view 'messages)
          (pilish--session-browser-rerender)
          (should (string-match-p "● Live session" (buffer-string)))
          (should-not (string-match-p "● Cold session" (buffer-string)))
          (delete-process proc)
          (pilish--session-browser-rerender)
          (should-not (string-match-p "● Live session" (buffer-string))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-session-browser-live-marker-threaded ()
  "The live marker follows the threading connector in the Threaded view."
  (let* ((path "/test/live-child.jsonl")
         (chat-buf (generate-new-buffer "*pilish-test-live-marker-chat*"))
         (proc (start-process "pilish-live-marker-test" nil "sleep" "30")))
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'pilish-chat-buffer chat-buf)
    (with-current-buffer chat-buf
      (setq pilish--process proc
            pilish--state (list :session-file path)))
    (unwind-protect
        (with-temp-buffer
          (pilish-session-browser-mode)
          (setq pilish--session-browser-items
                (list (list :path "/test/parent.jsonl" :name "Parent session"
                            :messageCount 10 :modified "2026-02-24T10:00:00Z")
                      (list :path path :name "Child session"
                            :parentSessionPath "/test/parent.jsonl"
                            :messageCount 2 :modified "2026-02-24T11:00:00Z")))
          (setq pilish--session-browser-view 'threaded)
          (pilish--session-browser-rerender)
          (should (string-match-p "└─ ● Child session" (buffer-string)))
          (should-not (string-match-p "● Parent session" (buffer-string))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-session-browser-live-marker-alias-spelling ()
  "The live marker matches a session through symlink alias spellings.
The process holds the real target while the browser row retained an
alias spelling of the same file; both canonicalize to one identity."
  (let* ((base (pilish-test--make-temp-directory "pi-live-alias-"))
         (elsewhere (expand-file-name "elsewhere" base))
         (chat-buf (generate-new-buffer "*pilish-test-live-alias-chat*"))
         (proc (start-process "pilish-live-alias-test" nil "sleep" "30")))
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'pilish-chat-buffer chat-buf)
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (let ((target (expand-file-name "live.jsonl" elsewhere))
                (alias-dir (expand-file-name "alias" base)))
            (write-region "" nil target)
            (make-directory alias-dir t)
            (make-symbolic-link target
                                (expand-file-name "live.jsonl" alias-dir))
            (with-current-buffer chat-buf
              (setq pilish--process proc
                    pilish--state (list :session-file target)))
            (with-temp-buffer
              (pilish-session-browser-mode)
              (setq pilish--session-browser-items
                    (list (list :path (expand-file-name "live.jsonl"
                                                       alias-dir)
                                :name "Alias session"
                                :messageCount 3
                                :modified "2026-02-24T10:00:00Z")))
              (setq pilish--session-browser-view 'messages)
              (pilish--session-browser-rerender)
              (should (string-match-p "● Alias session" (buffer-string))))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf)
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-browser-live-marker-multi-hop-routes ()
  "The live marker respects complete TRAMP routes.
A session file on a multi-hop route is live; the same local name
reached without the bastion hop is a different route's file and must
not be cross-marked."
  (let* ((chat-buf (generate-new-buffer "*pilish-test-live-route-chat*"))
         (proc (start-process "pilish-live-route-test" nil "sleep" "30")))
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'pilish-chat-buffer chat-buf)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--process proc
                  pilish--state
                  (list :session-file
                        "/ssh:bastion|ssh:pi-host:/s/live.jsonl")))
          (with-temp-buffer
            (pilish-session-browser-mode)
            (setq pilish--session-browser-items
                  (list '(:path "/ssh:pi-host:/s/live.jsonl"
                         :name "Final hop session"
                         :messageCount 3 :modified "2026-02-24T10:00:00Z")
                        '(:path "/ssh:bastion|ssh:pi-host:/s/live.jsonl"
                         :name "Routed session"
                         :messageCount 3 :modified "2026-02-24T11:00:00Z")))
            (setq pilish--session-browser-view 'messages)
            (pilish--session-browser-rerender)
            ;; Only the same-route row is live.
            (should (string-match-p "● Routed session" (buffer-string)))
            (should-not (string-match-p "● Final hop session"
                                        (buffer-string)))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-session-browser-render-threaded-activity-order ()
  "Threaded render orders families by subtree activity.
Mirrors the upstream Pi selector case: an old parent promoted by a new
child renders above a newer root, before its own descendant."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          ;; Discovery order lists the younger root first.
          (list '(:path "/sess/mid-root.jsonl" :name "Mid Root"
                  :messageCount 10 :modified "2026-01-02T00:00:00Z")
                '(:path "/sess/old-parent.jsonl" :name "Old Parent"
                  :messageCount 10 :modified "2026-01-01T00:00:00Z")
                '(:path "/sess/new-child.jsonl" :name "New Child"
                  :parentSessionPath "/sess/old-parent.jsonl"
                  :messageCount 10 :modified "2026-01-03T00:00:00Z")))
    (setq pilish--session-browser-view 'threaded)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (let ((pos-p (string-match "Old Parent" text))
            (pos-c (string-match "New Child" text))
            (pos-r (string-match "Mid Root" text)))
        (should (and pos-p pos-c pos-r))
        (should (< pos-p pos-c pos-r)))
      (should (string-match-p "└─ New Child" text)))))

(ert-deftest pilish-test-session-browser-threaded-query-flattens ()
  "A query in Threaded view shows flat newest-first rows.
A partial match set must not draw family connectors or nest fork
children; ordering follows activity instead of archive order."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/sess/r.jsonl" :name "Query Root"
                  :messageCount 10 :modified "2026-01-01T00:00:00Z")
                '(:path "/sess/p.jsonl" :name "Query Parent"
                  :messageCount 10 :modified "2026-01-03T00:00:00Z")
                '(:path "/sess/c.jsonl" :name "Query Child"
                  :parentSessionPath "/sess/p.jsonl"
                  :messageCount 10 :modified "2026-01-02T00:00:00Z")
                '(:path "/sess/x.jsonl" :name "Unrelated Session"
                  :messageCount 10 :modified "2026-01-04T00:00:00Z")))
    (setq pilish--session-browser-view 'threaded)
    (setq pilish--session-browser-search-query "Query"
          pilish--session-browser-search-tokens '("Query"))
    (unwind-protect
        (progn
          (pilish--session-browser-rerender)
          (let ((text (buffer-string)))
            ;; Flat newest-first over the matching set.
            (let ((pos-p (string-match "Query Parent" text))
                  (pos-c (string-match "Query Child" text))
                  (pos-r (string-match "Query Root" text)))
              (should (and pos-p pos-c pos-r))
              (should (< pos-p pos-c pos-r)))
            ;; No ancestry implied between partial matches.
            (should-not (string-match-p "├─" text))
            (should-not (string-match-p "└─" text))
            ;; The non-matching session is filtered out entirely.
            (should-not (string-match-p "Unrelated" text))))
      (setq pilish--session-browser-search-query nil
            pilish--session-browser-search-tokens nil))))

(ert-deftest pilish-test-session-browser-render-loading ()
  "Render one loading indicator without resolving unused live paths.
A loading screen has no rows to mark.  Avoiding live-session identity
work also removes a file-handler reentrancy point that could let an
obsolete outer render append after a newer request rendered."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-loading t)
    (cl-letf (((symbol-function 'pilish--browse-live-session-paths)
               (lambda ()
                 (ert-fail "loading render resolved live paths"))))
      (pilish--session-browser-rerender))
    (should (equal (buffer-string) "Loading sessions...\n"))))

(ert-deftest pilish-test-session-browser-row-render-yields-to-reentrant-fetch ()
  "A row render never appends after a nested newer fetch rendered status.
Live-path resolution reentrantly starts request B.  B's loading render
owns the incremented generation; when the suspended row render resumes,
it leaves B's single loading line intact rather than duplicating it."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 1 :modified "2026-03-11T10:00:00Z")))
    (let ((first t)
          pending)
      (cl-letf (((symbol-function 'pilish--browse-live-session-paths)
                 (lambda ()
                   (when first
                     (setq first nil)
                     (pilish--session-browser-fetch-and-render))
                   (make-hash-table :test 'equal)))
                ((symbol-function 'pilish--browse-load-sessions)
                 (lambda (_scope callback &optional _generation)
                   (setq pending callback))))
        (pilish--session-browser-rerender))
      (should pending)
      (should pilish--session-browser-loading)
      (should (equal (buffer-string) "Loading sessions...\n")))))

(ert-deftest pilish-test-session-browser-visibility-hook-render-is-fenced ()
  "A newer session fetch cannot nest inside an old Magit section parent.
The second stale row's visibility hook requests B after A already inserted
one row.  A must unwind, paint B's loading state cleanly, and later publish
only B's final row and fold metadata."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (let* ((parent "/test/stale-parent.jsonl")
           (child "/test/stale-child.jsonl")
           (pending nil)
           (session-hooks 0)
           (armed t)
           (magit-section-set-visibility-hook
            (list
             (lambda (section)
               (when (eq (oref section type) 'session)
                 (setq session-hooks (1+ session-hooks))
                 (when (and armed (= session-hooks 2))
                   (setq armed nil)
                   (pilish--session-browser-fetch-and-render)))
               nil))))
      (setq pilish--session-browser-fetch-token 1
            pilish--session-browser-view 'threaded
            pilish--session-browser-items
            (list (list :path parent :canonicalPath parent :name "STALE A"
                        :modified "2026-01-01T00:00:00Z")
                  (list :path child :canonicalPath child :name "STALE child"
                        :parentSessionPath parent
                        :canonicalParentSession parent
                        :modified "2026-01-02T00:00:00Z")))
      (puthash parent t pilish--browse-fold-state)
      (cl-letf (((symbol-function 'pilish--browse-live-session-paths)
                 (lambda () (make-hash-table :test #'equal)))
                ((symbol-function 'pilish--browse-load-sessions)
                 (lambda (_scope callback &optional _generation)
                   (setq pending callback))))
        (pilish--session-browser-rerender))
      (should-not armed)
      (should pending)
      (should pilish--session-browser-loading)
      (should (equal (buffer-string) "Loading sessions...\n"))
      (should-not pilish--browse-fold-rows)
      (funcall pending
               (list '(:path "/test/final-b.jsonl" :name "FINAL B"
                       :modified "2026-01-03T00:00:00Z"))
               nil)
      (should-not pilish--session-browser-loading)
      (should (string-match-p "FINAL B" (buffer-string)))
      (should-not (string-match-p "STALE" (buffer-string)))
      (should (equal (mapcar (lambda (row) (plist-get row :value))
                             pilish--browse-fold-rows)
                     '("/test/final-b.jsonl")))
      (should-not (gethash parent pilish--browse-fold-state)))))

(ert-deftest pilish-test-session-browser-item-key-reentrancy-keeps-newer-body ()
  "A hand-built row key cannot insert after its handler starts request B.
The item deliberately lacks `:canonicalPath', so its one canonical-key
lookup dispatches a harmless file-name handler.  That handler starts B;
A must yield without inserting STALE CURRENT above B's loading body."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-view 'messages
          pilish--session-browser-items
          (list '(:path "/pilish-reentrant-key/session.jsonl"
                  :name "STALE CURRENT" :messageCount 1
                  :modified "2026-03-11T10:00:00Z")))
    (let ((first t)
          (handler-calls 0)
          (pending nil)
          handler)
      (setq handler
            (lambda (operation &rest args)
              (if (eq operation 'file-truename)
                  (progn
                    (cl-incf handler-calls)
                    (when first
                      (setq first nil)
                      (pilish--session-browser-fetch-and-render))
                    (car args))
                ;; Standard file-handler delegation without recursion.
                (let ((inhibit-file-name-handlers
                       (cons handler
                             (and (eq inhibit-file-name-operation operation)
                                  inhibit-file-name-handlers)))
                      (inhibit-file-name-operation operation))
                  (apply operation args)))))
      (let ((file-name-handler-alist
             (cons (cons "\\`/pilish-reentrant-key/" handler)
                   file-name-handler-alist)))
        (cl-letf (((symbol-function 'pilish--browse-live-session-paths)
                   (lambda () (make-hash-table :test 'equal)))
                  ((symbol-function 'pilish--browse-load-sessions)
                   (lambda (_scope callback &optional _generation)
                     (setq pending callback))))
          (pilish--session-browser-rerender)))
      (should (= handler-calls 1))
      (should pending)
      (should pilish--session-browser-loading)
      (should (= pilish--session-browser-fetch-token 1))
      (should-not (string-match-p "STALE CURRENT" (buffer-string)))
      (should (equal (buffer-string) "Loading sessions...\n")))))

(ert-deftest pilish-test-session-browser-stale-apply-yields-during-keying ()
  "Generation A cannot publish after item canonicalization starts B.
A hand-built callback item lacks `:canonicalPath'.  Its key lookup
reentrantly starts B; A must leave B's loading state and the prior
snapshot untouched."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-fetch-token 1
          pilish--session-browser-items
          (list '(:path "/kept.jsonl" :canonicalPath "/kept.jsonl"
                  :name "Kept snapshot")))
    (let ((first t)
          pending)
      (cl-letf (((symbol-function 'file-truename)
                 (lambda (path)
                   (when first
                     (setq first nil)
                     (pilish--session-browser-fetch-and-render))
                   path))
                ((symbol-function 'pilish--browse-load-sessions)
                 (lambda (_scope callback &optional _generation)
                   (setq pending callback))))
        (pilish--session-browser-apply-scan
         (current-buffer)
         (list '(:path "/stale-apply.jsonl" :name "STALE APPLY"))
         nil nil 'current 1))
      (should pending)
      (should (= pilish--session-browser-fetch-token 2))
      (should pilish--session-browser-loading)
      (should (equal (plist-get (car pilish--session-browser-items) :name)
                     "Kept snapshot"))
      (should-not (string-match-p "STALE APPLY" (buffer-string)))
      (should (equal (buffer-string) "Loading sessions...\n")))))

(ert-deftest pilish-test-session-thread-preparation-stops-when-stale ()
  "Thread preparation stops after its first parent lookup supersedes it."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-fetch-token 1)
    (let ((calls nil)
          (items
           (list '(:path "/one.jsonl" :canonicalPath "/one.jsonl"
                   :parentSessionPath "/parent-one.jsonl")
                 '(:path "/two.jsonl" :canonicalPath "/two.jsonl"
                   :parentSessionPath "/parent-two.jsonl"))))
      (cl-letf (((symbol-function 'pilish--thread-parent-identity)
                 (lambda (parent &rest _)
                   (push parent calls)
                   (cl-incf pilish--session-browser-fetch-token)
                   nil)))
        (should-not
         (pilish--session-thread-items items (current-buffer) 1)))
      (should (equal calls '("/parent-one.jsonl"))))))

(defconst pilish-test--narrow-body-columns 32
  "Usable text columns of the narrowest supported session browser.
A 52-column terminal with the mode's 20-column right margin leaves
32 body columns; empty-state hint lines must fit, since a truncated
hint hides the recovery key it names.")

(defun pilish-test--empty-lines (text)
  "Return the nonblank lines of rendered empty-state TEXT."
  (seq-remove #'string-empty-p (split-string text "\n")))

(defun pilish-test--assert-hints-visible (text)
  "Assert every empty-state line in TEXT fits the narrow body width."
  (dolist (line (pilish-test--empty-lines text))
    (should (<= (length line)
                pilish-test--narrow-body-columns))))

(ert-deftest pilish-test-session-browser-render-empty ()
  "Empty scan in this-project scope suggests switching scope.
Each hint is its own short line so the recovery key survives narrow
windows."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items nil)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "No sessions in this project" text))
      (should (string-match-p "\nt to list all projects" text))
      (pilish-test--assert-hints-visible text))))

(ert-deftest pilish-test-session-browser-render-empty-all-scope ()
  "Empty scan across all projects is the honest terminal state.
No widening action exists, so none of the real key hints is offered."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items nil
          pilish--session-browser-scope 'all)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "No sessions found" text))
      (should-not (string-match-p "t to list all projects" text))
      (should-not (string-match-p "f to show all names" text))
      (should-not (string-match-p "/ to clear the query" text)))))

(ert-deftest pilish-test-session-browser-render-empty-named-only ()
  "Named-only emptiness suggests clearing it, plus a scope switch
only when a wider scope exists."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/test/a.jsonl" :firstMessage "Unnamed work"
             :messageCount 2 :modified "2026-03-11T10:00:00Z"))
          pilish--session-browser-named-only t
          pilish--session-browser-scope 'current)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "No named sessions" text))
      (should (string-match-p "\nf to show all names" text))
      (should (string-match-p "\nt to list all projects" text))
      (pilish-test--assert-hints-visible text)))
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/test/a.jsonl" :firstMessage "Unnamed work"
             :messageCount 2 :modified "2026-03-11T10:00:00Z"))
          pilish--session-browser-named-only t
          pilish--session-browser-scope 'all)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "No named sessions" text))
      (should (string-match-p "\nf to show all names" text))
      (should-not (string-match-p "t to list all projects" text))
      (pilish-test--assert-hints-visible text))))

(ert-deftest pilish-test-session-browser-render-empty-query ()
  "Query emptiness suggests clearing the query first, then the next
widening state: named-only when active, otherwise the scope switch
when one exists."
  ;; Query + this project: clear the query or search everywhere.
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/test/a.jsonl" :name "Session A"
             :messageCount 2 :modified "2026-03-11T10:00:00Z"))
          pilish--session-browser-search-query "zzz-no-match"
          pilish--session-browser-search-tokens '("zzz-no-match")
          pilish--session-browser-scope 'current)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "No matching sessions" text))
      (should (string-match-p "\n/ to clear the query" text))
      (should (string-match-p "\nt to search all projects" text))
      ;; The query hint precedes the scope hint.
      (should (< (string-match "\n/ to clear the query" text)
                 (string-match "\nt to search all projects" text)))
      (pilish-test--assert-hints-visible text)))
  ;; Query + all projects: only the query can widen.
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/test/a.jsonl" :name "Session A"
             :messageCount 2 :modified "2026-03-11T10:00:00Z"))
          pilish--session-browser-search-query "zzz-no-match"
          pilish--session-browser-search-tokens '("zzz-no-match")
          pilish--session-browser-scope 'all)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "No matching sessions" text))
      (should (string-match-p "\n/ to clear the query" text))
      (should-not (string-match-p "t to list all projects" text))
      (pilish-test--assert-hints-visible text)))
  ;; Query + named-only: the query hint comes first, named-only second,
  ;; and the scope hint yields to the two-hint maximum.
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/test/a.jsonl" :name "Session A"
             :messageCount 2 :modified "2026-03-11T10:00:00Z"))
          pilish--session-browser-search-query "zzz-no-match"
          pilish--session-browser-search-tokens '("zzz-no-match")
          pilish--session-browser-named-only t
          pilish--session-browser-scope 'current)
    (pilish--session-browser-rerender)
    (let ((text (buffer-string)))
      (should (string-match-p "No matching sessions" text))
      (should (< (string-match "\n/ to clear the query" text)
                 (string-match "\nf to show all names" text)))
      (should-not (string-match-p "t to list all projects" text))
      (pilish-test--assert-hints-visible text))))

(ert-deftest pilish-test-session-browser-render-recent-future-group-first ()
  "Future mtimes head the Recent view as one contiguous group.
Without a Future label, newest-first sorting interleaves a future
row's This Week heading between Today and Yesterday — the same
heading appears twice, noncontiguously.  The zone is pinned and the
tomorrow mtime is constructed on the decoded calendar (adding
86400 seconds would land on the wrong day across a transition)."
  (let ((saved (getenv "TZ")))
    (unwind-protect
        (progn
          (set-time-zone-rule "Europe/Berlin")
          ;; Pin the render clock as well as the zone: the Recent
          ;; renderer captures `current-time' once per render.
          (cl-letf (((symbol-function 'current-time)
                     (lambda ()
                       (encode-time 30 0 8 11 3 2026))))
          (let* ((dec (decode-time (current-time)))
                 (today-early (encode-time 5 0 0
                                           (decoded-time-day dec)
                                           (decoded-time-month dec)
                                           (decoded-time-year dec)))
                 (yesterday-late (time-subtract today-early 600))
                 (tomorrow-noon
                  (encode-time 0 0 12
                               (1+ (decoded-time-day dec))
                               (decoded-time-month dec)
                               (decoded-time-year dec))))
            (with-temp-buffer
              (pilish-session-browser-mode)
              (setq pilish--session-browser-items
                    (list (list :path "/test/late.jsonl" :name "Late night"
                                :messageCount 2
                                :modified (pilish-test--local-time-iso yesterday-late))
                          (list :path "/test/skew.jsonl" :name "Clock skew"
                                :messageCount 1
                                :modified (pilish-test--local-time-iso tomorrow-noon))
                          (list :path "/test/early.jsonl" :name "Early morning"
                                :messageCount 3
                                :modified (pilish-test--local-time-iso today-early))))
              (setq pilish--session-browser-view 'recent)
              (pilish--session-browser-rerender)
              (let ((text (buffer-string)))
                ;; Headings appear once each, in the honest total order.
                (dolist (heading '("Future" "Today" "Yesterday"))
                  (should (equal 1 (cl-count-if
                                    (lambda (line)
                                      (equal line (concat "▾ " heading)))
                                    (split-string text "\n")))))
                (should (< (string-match "\\`▾ Future\n" text)
                           (string-match "Clock skew" text)
                           (string-match "\n▾ Today\n" text)
                           (string-match "Early morning" text)
                           (string-match "\n▾ Yesterday\n" text)
                           (string-match "Late night" text))))))))
      (if saved
          (set-time-zone-rule saved)
        (set-time-zone-rule nil)))))

(ert-deftest pilish-test-session-browser-render-recent-calendar-groups ()
  "Recent view groups rows under calendar Today/Yesterday headings.
A late-night session belongs to Yesterday the moment the calendar day
turns, not twenty-four hours later; rows sort newest-first inside
and across the groups.  The renderer's production clock is pinned."
  (let ((fixed-now (encode-time '(0 0 8 11 3 2026 nil nil nil))))
    (cl-letf (((symbol-function 'current-time) (lambda () fixed-now)))
      (let* ((dec (decode-time fixed-now))
             (today-early (encode-time 5 0 0
                                       (decoded-time-day dec)
                                       (decoded-time-month dec)
                                       (decoded-time-year dec)))
             (yesterday-late (time-subtract today-early 600)))
        (with-temp-buffer
          (pilish-session-browser-mode)
          (setq pilish--session-browser-items
                (list (list :path "/test/late.jsonl" :name "Late night"
                            :messageCount 2
                            :modified
                            (pilish-test--local-time-iso yesterday-late))
                      (list :path "/test/early.jsonl" :name "Early morning"
                            :messageCount 3
                            :modified
                            (pilish-test--local-time-iso today-early))))
          (setq pilish--session-browser-view 'recent)
          (pilish--session-browser-rerender)
          (let ((text (buffer-string)))
            ;; Calendar grouping: yesterday 23:55 is Yesterday even minutes
            ;; after midnight; today 00:05 is Today.
            (should (string-match-p "\\`▾ Today
" text))
            (should (string-match-p "
▾ Yesterday
" text))
            ;; Newest first within and across groups.
            (should (< (string-match "Early morning" text)
                       (string-match "Late night" text)))
            ;; Each row sits under its own group heading.
            (should (< (string-match "\\`▾ Today
" text)
                       (string-match "Early morning" text)
                       (string-match "
▾ Yesterday
" text)
                       (string-match "Late night" text)))))))))

(ert-deftest pilish-test-session-browser-header-line ()
  "Header-line shows scope, view, named-only state, and an explicit
total count.  The count is labeled `total' because it counts scanned
sessions in scope — not the rows surviving the current query and
named-only filter, so an empty result next to `(2 total)' is not
mistaken for a contradiction."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'current
          pilish--session-browser-items-scope 'current
          pilish--session-browser-view 'threaded
          pilish--session-browser-items '((:id "a") (:id "b")))
    (let ((header (pilish--session-browser-header-line)))
      (should (string-match-p "Sessions \\[This project\\]" header))
      (should (string-match-p "view:Threaded (fork families)" header))
      (should (string-match-p "(2 total)" header))
      (should-not (string-match-p "sort" header))
      ;; A changed view changes the label, not just the raw value.
      (setq pilish--session-browser-view 'messages
            pilish--session-browser-named-only t)
      (should (string-match-p "view:Most messages"
                              (pilish--session-browser-header-line)))
      (should (string-match-p "named-only"
                              (pilish--session-browser-header-line))))))

(ert-deftest pilish-test-session-browser-failed-scan-hides-total ()
  "An errored or unowned snapshot has no confirmed total to report.
A successful empty scan may truthfully show `(0 total)', but the same
empty list accompanied by an error — or no owning scope yet — must not
claim that zero sessions were confirmed."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'current
          pilish--session-browser-items nil
          pilish--session-browser-items-scope 'current
          pilish--session-browser-loading nil
          pilish--session-browser-error "Cannot list sessions: denied")
    (should-not (string-match-p "total"
                                (pilish--session-browser-header-line)))
    (pilish--session-browser-rerender)
    (should (string-match-p "Cannot list sessions: denied"
                            (buffer-string)))
    ;; Clearing the error confirms this owned empty snapshot.
    (setq pilish--session-browser-error nil)
    (should (string-match-p "(0 total)"
                            (pilish--session-browser-header-line)))
    ;; Nil ownership is still unconfirmed rather than a trusted zero.
    (setq pilish--session-browser-items-scope nil)
    (should-not (string-match-p "total"
                                (pilish--session-browser-header-line)))))

(ert-deftest pilish-test-session-browser-scope-transition-hides-stale-total ()
  "An in-flight scope change never labels the old snapshot as new scope.
The prior This-project count remains owned by that scope while the
All-projects scan is pending, so the new header hides it; the completed
callback publishes the new owner and total."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'current
          pilish--session-browser-items-scope 'current
          pilish--session-browser-view 'messages
          pilish--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 2 :modified "2026-03-11T10:00:00Z")
                '(:path "/test/b.jsonl" :name "Session B"
                  :messageCount 1 :modified "2026-03-10T10:00:00Z")))
    (pilish--session-browser-rerender)
    (should (string-match-p "(2 total)"
                            (pilish--session-browser-header-line)))
    (let (pending requested-scope)
      (cl-letf (((symbol-function 'pilish--browse-load-sessions)
                 (lambda (scope callback &optional _generation)
                   (setq requested-scope scope
                         pending callback)))
                ((symbol-function 'message) #'ignore))
        (pilish-session-browser-toggle-scope))
      (should (eq requested-scope 'all))
      (should pilish--session-browser-loading)
      (should (string-match-p
               (regexp-quote "Sessions [All projects]")
               (pilish--session-browser-header-line)))
      (should-not (string-match-p "(2 total)"
                                  (pilish--session-browser-header-line)))
      (should-not (string-match-p "total"
                                  (pilish--session-browser-header-line)))
      (should (string-match-p "Loading sessions" (buffer-string)))
      (funcall pending
               (list '(:path "/test/all.jsonl" :name "All Session"
                       :messageCount 1 :modified "2026-03-12T10:00:00Z"))
               nil)
      (should-not pilish--session-browser-loading)
      (should (eq pilish--session-browser-items-scope 'all))
      (should (string-match-p "(1 total)"
                              (pilish--session-browser-header-line))))))

(ert-deftest pilish-test-session-browser-query-keeps-view-order ()
  "Only a queried Threaded view is flattened to newest-first rows.
Recent is newest-first by definition; Most messages keeps its count
ordering under a query — the flattening is the Threaded contract,
not a general query rule."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/many.jsonl" :name "Many"
                  :messageCount 50 :modified "2026-01-01T00:00:00Z")
                '(:path "/test/few.jsonl" :name "Few"
                  :messageCount 2 :modified "2026-02-01T00:00:00Z")))
    (setq pilish--session-browser-search-query "Many\\|Few"
          pilish--session-browser-search-tokens '("Many\\|Few"))
    ;; Queried Most messages: count order wins over recency.
    (setq pilish--session-browser-view 'messages)
    (pilish--session-browser-rerender)
    (should (< (string-match "Many" (buffer-string))
               (string-match "Few" (buffer-string))))
    ;; Queried Threaded: flat rows, newest first, no connectors.
    (setq pilish--session-browser-view 'threaded)
    (pilish--session-browser-rerender)
    (should (< (string-match "Few" (buffer-string))
               (string-match "Many" (buffer-string))))
    (should-not (string-match-p "[└├]─" (buffer-string)))))

;;;; Tree Node Formatting

(ert-deftest pilish-test-tree-node-face ()
  "Correct face for each node type."
  (should (eq (pilish--tree-node-face
               '(:type "message" :role "user"))
              'pilish-tree-user))
  (should (eq (pilish--tree-node-face
               '(:type "message" :role "assistant"))
              'pilish-tree-assistant))
  (should (eq (pilish--tree-node-face
               '(:type "tool_result"))
              'pilish-tree-tool))
  (should (eq (pilish--tree-node-face
               '(:type "compaction"))
              'pilish-tree-compaction))
  (should (eq (pilish--tree-node-face
               '(:type "branch_summary"))
              'pilish-tree-summary)))

(ert-deftest pilish-test-tree-node-type-label ()
  "Short type labels for tree nodes."
  (should (equal (pilish--tree-node-type-label
                  '(:type "message" :role "user"))
                 "you"))
  (should (equal (pilish--tree-node-type-label
                  '(:type "message" :role "assistant"))
                 "ast"))
  (should (equal (pilish--tree-node-type-label
                  '(:type "tool_result" :toolName "Read"))
                 "Read"))
  (should (equal (pilish--tree-node-type-label
                  '(:type "compaction"))
                 "compact")))

;;;; Tool Preview Unpacking

(ert-deftest pilish-test-tree-strip-bracket-preview-formatted ()
  "Strip bracket wrapper from formattedToolCall."
  (should (equal (pilish--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "read"
                    :formattedToolCall "[read: ~/file.py:10-29]"
                    :preview "[read: ~/file.py:10-29]"))
                 "~/file.py:10-29")))

(ert-deftest pilish-test-tree-strip-bracket-preview-read ()
  "Read tool strips wrapper, shows path."
  (should (equal (pilish--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "Read"
                    :preview "[Read: db/connection.py]"))
                 "db/connection.py")))

(ert-deftest pilish-test-tree-strip-bracket-preview-bash ()
  "Bash tool strips wrapper, shows command."
  (should (equal (pilish--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "bash"
                    :formattedToolCall "[bash: git status]"
                    :preview "[bash: git status]"))
                 "git status")))

(ert-deftest pilish-test-tree-strip-bracket-preview-no-args ()
  "Tool with no args returns empty string."
  (should (equal (pilish--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "unknown"
                    :preview "[unknown]"))
                 "")))

(ert-deftest pilish-test-tree-strip-bracket-preview-plain-text ()
  "Preview without brackets returned as-is."
  (should (equal (pilish--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "custom"
                    :preview "some plain output"))
                 "some plain output")))

(ert-deftest pilish-test-tree-strip-bracket-preview-in-node-line ()
  "Tool result in formatted node line shows unwrapped preview."
  (let ((line (pilish--tree-format-node-line
               '(:type "tool_result" :toolName "Read"
                 :preview "[Read: db/connection.py]")
               nil)))
    ;; Should NOT have the bracketed format
    (should-not (string-match-p "\\[Read:" line))
    ;; Should have the unwrapped path
    (should (string-match-p "db/connection.py" line))))

(ert-deftest pilish-test-tree-node-preview-message ()
  "Regular message nodes return preview as-is."
  (should (equal (pilish--tree-node-preview
                  '(:type "message" :role "user" :preview "hello world"))
                 "hello world"))
  (should (equal (pilish--tree-node-preview
                  '(:type "message" :role "assistant" :preview "sure thing"))
                 "sure thing"))
  ;; Missing preview returns empty string
  (should (equal (pilish--tree-node-preview
                  '(:type "message" :role "user"))
                 "")))

(ert-deftest pilish-test-tree-node-preview-branch-summary ()
  "Branch summary nodes return first line of summary, not full text."
  ;; Multi-line summary returns only first line
  (should (equal (pilish--tree-node-preview
                  '(:type "branch_summary"
                    :summary "The user explored TDD.\n\n## Goal\nLearn testing."))
                 "The user explored TDD."))
  ;; Single-line summary returned as-is
  (should (equal (pilish--tree-node-preview
                  '(:type "branch_summary"
                    :summary "Short summary"))
                 "Short summary"))
  ;; Missing summary returns empty string
  (should (equal (pilish--tree-node-preview
                  '(:type "branch_summary"))
                 ""))
  ;; Summary starting with blank lines skips to first non-empty line
  (should (equal (pilish--tree-node-preview
                  '(:type "branch_summary"
                    :summary "\n\nActual summary here\nMore text"))
                 "Actual summary here")))

(ert-deftest pilish-test-tree-node-preview-bash-execution ()
  "Bash execution message strips bracket wrapper from preview.
Upstream changed format from `[bash]: cmd' to `[bash: cmd]'.
The type label already shows `sh', so brackets are redundant."
  ;; tree-node-preview strips the wrapper
  (should (equal (pilish--tree-node-preview
                  '(:type "message" :role "bashExecution"
                    :preview "[bash: git status]"))
                 "git status"))
  ;; Formatted node line shows stripped preview
  (let ((line (pilish--tree-format-node-line
               '(:type "message" :role "bashExecution"
                 :preview "[bash: git log --oneline]")
               nil)))
    (should-not (string-match-p "\\[bash:" line))
    (should (string-match-p "git log --oneline" line))))

(ert-deftest pilish-test-tree-format-node-distinguishes-active-and-current ()
  "Text markers distinguish an active ancestor from the current entry."
  (let ((active (pilish--tree-format-node-line
                 '(:type "message" :role "user" :preview "ancestor")
                 t nil))
        (current (pilish--tree-format-node-line
                  '(:type "message" :role "assistant" :preview "leaf")
                  t t)))
    (should (string-prefix-p "* " active))
    (should (string-prefix-p "@ " current))
    (should (string-match-p "ancestor" active))
    (should (string-match-p "leaf" current))))

(ert-deftest pilish-test-tree-format-node-inactive ()
  "Inactive nodes get a blank marker column."
  (let ((line (pilish--tree-format-node-line
               '(:type "message" :role "user" :preview "hello") nil nil)))
    (should (string-prefix-p "  " line))
    (should-not (string-match-p "[@*]" (substring line 0 2)))
    (should (string-match-p "hello" line))))

(ert-deftest pilish-test-tree-format-node-with-label ()
  "Labeled nodes include their label in keyboard-readable line text."
  (let ((line (pilish--tree-format-node-line
               '(:type "message" :role "user" :preview "hello"
                 :label "checkpoint")
               nil)))
    (should (string-match-p "\\[checkpoint\\]" line))
    (should (string-match-p "hello" line))))

;;;; Tree Browser Rendering

(ert-deftest pilish-test-tree-browser-render ()
  "Render tree from fixture data."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      ;; Buffer should contain node content
      (should (string-match-p "refactor" (buffer-string)))
      ;; The current leaf and its active ancestors have distinct text markers.
      (should (string-match-p "@ ast" (buffer-string)))
      (should (string-match-p "\\* you" (buffer-string)))
      ;; Labels stay in buffer text as well as the right margin, so
      ;; keyboard users can discover and search what the row displays.
      (should (string-match-p "\\[checkpoint\\]" (buffer-string))))))

(ert-deftest pilish-test-tree-browser-render-connectors ()
  "Tree connectors appear in rendered buffer at branch points."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      (let ((text (buffer-string)))
        ;; Branch connectors should appear
        (should (string-match-p "├─" text))
        (should (string-match-p "└─" text))
        ;; Gutter continuation should appear
        (should (string-match-p "│" text))
        ;; Marker column follows, rather than becoming part of, topology.
        (should (string-match-p "├─ \\*" text))
        (should (string-match-p "│  @" text))
        ;; Last branch child: connector plus a blank marker (inactive).
        (should (string-match-p "└─   " text))))))

(ert-deftest pilish-test-tree-browser-render-v087-session-visibility ()
  "The pi 0.87 fixture renders with the new shapes kept out of sight.
The projected tree never contains the `usage'/`context_edit' records,
so no mode can render them as sections or text.  The leading system
message hides under `default'/`no-tools' like settings noise and shows
under `all'; ordinary turns stay visible everywhere.

Fixture: test/fixtures/browse-session-v087.jsonl (ids: system
3c4d5e6f, usage 8b92c5d6, context_edit 9ca3d7e8)."
  (dolist (mode '(default no-tools all))
    (with-temp-buffer
      (pilish-tree-browser-mode)
      (let* ((projected (pilish-jsonl-project-session-file
                         (expand-file-name "browse-session-v087.jsonl"
                                           pilish-test--fixture-dir)))
             (tree (plist-get projected :tree)))
        (should projected)
        ;; Projection-filtered records cannot become nodes at all.
        (should-not (pilish-test--tree-find-node tree "8b92c5d6"))
        (should-not (pilish-test--tree-find-node tree "9ca3d7e8"))
        (setq pilish--tree-browser-tree tree
              pilish--tree-browser-leaf-id (plist-get projected :leafId)
              pilish--tree-browser-filter mode)
        (pilish--tree-browser-rerender)
        (let* ((sections (car (pilish--tree-rendered-section-index)))
               (text (buffer-string)))
          ;; The system prompt row follows the settings treatment.
          (if (eq mode 'all)
              (should (gethash "3c4d5e6f" sections))
            (should-not (gethash "3c4d5e6f" sections)))
          ;; usage/context_edit never own sections in any mode...
          (should-not (gethash "8b92c5d6" sections))
          (should-not (gethash "9ca3d7e8" sections))
          ;; ...and their payload never reaches rendered text.
          (should-not (string-match-p "cache_warm\\|warming" text))
          ;; The system prompt text appears only where the row does.
          (if (eq mode 'all)
              (should (string-match-p "system prompt" text))
            (should-not (string-match-p "system prompt" text)))
          ;; Ordinary turns survive every filter.
          (should (string-match-p "hello" text))
          (should (string-match-p "continue from the summary" text)))))))

(ert-deftest pilish-test-tree-browser-fold-indicator-follows-long-type-label ()
  "A fold indicator does not split a type label wider than seven columns."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((tool-name "LongCustomToolName")
           (child (list :id "child" :type "message" :role "assistant"
                        :preview "child" :children (vector)))
           (parent (list :id "parent" :type "tool_result"
                         :toolName tool-name :preview "parent"
                         :children (vector child))))
      (setq pilish--tree-browser-tree (vector parent)
            pilish--tree-browser-leaf-id "child"
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      (let* ((row (pilish-test--browse-fold-row "parent"))
             (section (plist-get row :section))
             (indicator (+ (oref section start)
                           (plist-get row :indicator-offset))))
        (should (string-match-p
                 (concat (regexp-quote tool-name) " ▾")
                 (buffer-string)))
        (should (= (char-after indicator) (string-to-char "▾")))
        (should (= (plist-get row :indicator-offset)
                   (+ 3 (length tool-name))))))))

(ert-deftest pilish-test-tree-browser-filtered-current-marker-is-truthful ()
  "A hidden current leaf does not turn its visible ancestor into current."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            ;; node-8 is an assistant; node-7 is its visible user ancestor.
            pilish--tree-browser-filter 'user-only)
      (pilish--tree-browser-rerender)
      (should (equal (oref (magit-current-section) value) "node-7"))
      (should-not (string-match-p "^@ " (buffer-string)))
      (goto-char (point-min))
      (search-forward "That looks good")
      (beginning-of-line)
      (should (looking-at-p "\\* you")))))

(ert-deftest pilish-test-tree-browser-label-in-margin ()
  "Labels remain in right-margin overlays in addition to inline text."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      ;; Find margin overlays
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs)))
        ;; Should have at least one margin overlay (for the labeled node)
        (should (> (length margin-ovs) 0))
        ;; Find the one containing "checkpoint"
        (should (cl-some
                 (lambda (o)
                   (let* ((bs (overlay-get o 'before-string))
                          (display (get-text-property 0 'display bs))
                          (content (cadr display)))
                     (string-match-p "checkpoint" content)))
                 margin-ovs))))))

(ert-deftest pilish-test-tree-browser-label-truncation ()
  "Long labels are truncated with ellipsis to fit the right margin."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let ((tree (vector (list :id "n1" :type "message" :role "user"
                              :preview "hello" :timestamp "2026-01-01T00:00:00Z"
                              :label "this-is-a-very-long-label-name"
                              :children (vector)))))
      (setq pilish--tree-browser-tree tree
            pilish--tree-browser-leaf-id "n1"
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      ;; Find the margin overlay
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs))
             (content (when margin-ovs
                        (let* ((bs (overlay-get (car margin-ovs) 'before-string))
                               (display (get-text-property 0 'display bs)))
                          (cadr display)))))
        ;; Should exist and be truncated
        (should content)
        ;; Should contain ellipsis
        (should (string-match-p "…" content))
        ;; Total formatted length should fit: [truncated…] ≤ margin width
        (should (<= (length content) pilish--tree-margin-width))
        ;; Should NOT contain the full label
        (should-not (string-match-p "this-is-a-very-long-label-name" content))))))

(ert-deftest pilish-test-tree-browser-short-label-not-truncated ()
  "Short labels are not truncated."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let ((tree (vector (list :id "n1" :type "message" :role "user"
                              :preview "hello" :timestamp "2026-01-01T00:00:00Z"
                              :label "ok"
                              :children (vector)))))
      (setq pilish--tree-browser-tree tree
            pilish--tree-browser-leaf-id "n1"
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs))
             (content (when margin-ovs
                        (let* ((bs (overlay-get (car margin-ovs) 'before-string))
                               (display (get-text-property 0 'display bs)))
                          (cadr display)))))
        ;; Should contain the full label
        (should (string-match-p "\\[ok\\]" content))
        ;; Should NOT contain ellipsis
        (should-not (string-match-p "…" content))))))

(ert-deftest pilish-test-tree-browser-render-empty ()
  "Render empty tree."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree nil)
    (pilish--tree-browser-rerender)
    (should (string-match-p "No conversation tree" (buffer-string)))))

(ert-deftest pilish-test-tree-browser-render-user-filter ()
  "User-only filter shows only user messages."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            pilish--tree-browser-filter 'user-only)
      (pilish--tree-browser-rerender)
      ;; Should have user nodes
      (should (string-match-p "you" (buffer-string)))
      ;; Should NOT have assistant nodes
      (should-not (string-match-p "\\bast\\b" (buffer-string))))))

(ert-deftest pilish-test-tree-browser-mode-uses-default-filter ()
  "Direct tree mode activation starts from the default filter."
  (let ((pilish-tree-browser-default-filter 'user-only))
    (with-temp-buffer
      (pilish-tree-browser-mode)
      (should (eq pilish--tree-browser-filter 'user-only)))))

(ert-deftest pilish-test-tree-browser-header-line ()
  "Header-line shows filter mode and count."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            pilish--tree-browser-filter 'no-tools)
      (let ((header (pilish--tree-browser-header-line)))
        (should (string-match-p "no-tools" header))
        (should (string-match-p "([0-9]+)" header))))))

;;;; Error States

(ert-deftest pilish-test-session-browser-rpc-error ()
  "Session browser shows error when loading failed."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-error
          "session scan failed")
    (pilish--session-browser-rerender)
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "scan failed" (buffer-string)))))

(ert-deftest pilish-test-session-browser-rpc-error-cleared-on-success ()
  "A successful fetch clears a stale error state.
Phase 2: the fetch reads sessions from disk, so the process mock is
vestigial and none is consulted.  The environment is isolated to an
empty sessions root and timers run synchronously so the chunked scan
completes in-call."
  (let ((root (pilish-test--make-temp-directory "pi-err-clear")))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-error "some error")
      (cl-letf (((symbol-function 'pilish--get-process)
                 (lambda () 'fake))
                ((symbol-function 'pilish--session-list-directory)
                 (lambda (&optional _chat-buf) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args) (apply fn args))))
        (let ((process-environment
               (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                     process-environment)))
          (pilish--session-browser-fetch-and-render)))
      (should-not pilish--session-browser-error)
      (should-not pilish--session-browser-loading)
      (should (string-match-p "No sessions in this project"
                              (buffer-string))))))

;;;; Tree Find Label

(ert-deftest pilish-test-tree-find-label ()
  "Find label for a node ID in the tree."
  (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
         (tree (plist-get (plist-get response :data) :tree)))
    ;; node-7 has label "checkpoint"
    (should (equal (pilish--tree-find-label tree "node-7")
                   "checkpoint"))
    ;; node-1 has no label
    (should (null (pilish--tree-find-label tree "node-1")))))

;;;; Session Browser Dispatch Transient

(ert-deftest pilish-test-session-browser-dispatch-binding ()
  "Session browser binds its direct delete and dispatch keys."
  (should (eq (lookup-key pilish-session-browser-mode-map "d")
              'pilish-session-browser-delete))
  (should (eq (lookup-key pilish-session-browser-mode-map "?")
              'pilish-session-browser-dispatch))
  (should (eq (lookup-key pilish-session-browser-mode-map "h")
              'pilish-session-browser-dispatch)))

(ert-deftest pilish-test-session-browser-dispatch-is-transient ()
  "Session browser dispatch is a transient prefix command."
  (should (commandp 'pilish-session-browser-dispatch))
  (should (get 'pilish-session-browser-dispatch 'transient--prefix)))

(ert-deftest pilish-test-session-browser-dispatch-suffixes ()
  "Session browser dispatch wires all keys to the correct commands."
  (let ((expected
         '(("RET" . pilish-session-browser-switch)
           ("r"   . pilish-session-browser-rename)
           ("d"   . pilish-session-browser-delete)
           ("s"   . pilish-session-browser-cycle-view)
           ("f"   . pilish-session-browser-toggle-named)
           ("t"   . pilish-session-browser-toggle-scope)
           ("/"   . pilish-session-browser-search)
           ("g"   . pilish-browse-refresh)
           ("q"   . quit-window))))
    (dolist (pair expected)
      (let* ((key (car pair))
             (cmd (cdr pair))
             (suffix (transient-get-suffix
                      'pilish-session-browser-dispatch key))
             (actual (plist-get (cdr suffix) :command)))
        (should (eq actual cmd))))))

(ert-deftest pilish-test-session-dispatch-heading ()
  "Session dispatch heading reflects buffer-local state."
  (with-temp-buffer
    (pilish-session-browser-mode)
    ;; Default state: scope before view, no named-only
    (should (equal (pilish--session-dispatch-heading)
                   "scope:This project │ view:Threaded (fork families)"))
    ;; All state active
    (setq pilish--session-browser-view 'recent
          pilish--session-browser-scope 'all
          pilish--session-browser-named-only t)
    (should (equal (pilish--session-dispatch-heading)
                   "scope:All projects │ view:Recent activity │ named-only"))))

;;;; Tree Browser Dispatch Transient

(ert-deftest pilish-test-tree-browser-dispatch-binding ()
  "Tree browser binds `?' and `h' to the dispatch transient."
  (should (eq (lookup-key pilish-tree-browser-mode-map "?")
              'pilish-tree-browser-dispatch))
  (should (eq (lookup-key pilish-tree-browser-mode-map "h")
              'pilish-tree-browser-dispatch)))

(ert-deftest pilish-test-tree-browser-dispatch-is-transient ()
  "Tree browser dispatch is a transient prefix command."
  (should (commandp 'pilish-tree-browser-dispatch))
  (should (get 'pilish-tree-browser-dispatch 'transient--prefix)))

(ert-deftest pilish-test-tree-browser-dispatch-suffixes ()
  "Tree browser dispatch wires actions and all direct filter choices.
The summarize (`S') and abort (`C-c C-k') suffixes were dropped with
the summarize feature (needs navigate_tree RPC)."
  (let ((expected
         '(("RET" . pilish-tree-browser-navigate)
           ("l"   . pilish-tree-browser-set-label)
           ("f"   . pilish-tree-browser-cycle-filter)
           ("d"   . pilish--tree-browser-filter-default)
           ("n"   . pilish--tree-browser-filter-no-tools)
           ("u"   . pilish--tree-browser-filter-user-only)
           ("L"   . pilish--tree-browser-filter-labeled-only)
           ("a"   . pilish--tree-browser-filter-all)
           ("/"   . pilish-tree-browser-search)
           ("g"   . pilish-browse-refresh)
           ("q"   . quit-window))))
    (dolist (pair expected)
      (let* ((key (car pair))
             (cmd (cdr pair))
             (suffix (transient-get-suffix
                      'pilish-tree-browser-dispatch key))
             (actual (plist-get (cdr suffix) :command)))
        (should (eq actual cmd)))))
  ;; RET changes where the conversation continues; "navigate" does
  ;; not tell the user what the consequential action actually does.
  (let ((ret (transient-get-suffix
              'pilish-tree-browser-dispatch "RET")))
    (should (equal (plist-get (cdr ret) :description)
                   "continue from selected turn"))))

(ert-deftest pilish-test-tree-dispatch-heading ()
  "Tree dispatch heading reflects buffer-local filter state."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    ;; Default state (initial filter is no-tools)
    (let ((heading (pilish--tree-dispatch-heading)))
      (should (string-match-p "filter:no-tools" heading)))
    ;; Change state
    (setq pilish--tree-browser-filter 'user-only)
    (let ((heading (pilish--tree-dispatch-heading)))
      (should (string-match-p "filter:user-only" heading)))))

(ert-deftest pilish-test-dispatch-headings-read-shadowed-buffer ()
  "Both dispatch headings read the invoking browser's buffer-locals on
transient's real rendering path.
`transient--insert-group' formats group descriptions inside
`transient-with-shadowed-buffer' — with the INVOKING buffer current,
not the transient's own temp buffer — so the headings' buffer-local
reads are correct there.  This pins that contract the way transient
exercises it: evaluated with an unrelated buffer current and only the
shadowed binding pointing at the browser.  A refactor that breaks the
dependency (e.g. resolving the state from the wrong buffer) fails
here."
  ;; Session heading: shadowed to a browser with every toggle set.
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'all
          pilish--session-browser-view 'recent
          pilish--session-browser-named-only t)
    (let ((browser-buf (current-buffer)))
      (with-temp-buffer
        ;; Stands in for transient's temp buffer: some unrelated
        ;; buffer is current; only the shadowed binding names the
        ;; invoking browser.
        (let ((transient--shadowed-buffer browser-buf))
          (should (equal (transient-with-shadowed-buffer
                           (pilish--session-dispatch-heading))
                         "scope:All projects │ view:Recent activity │ named-only"))))))
  ;; Tree heading: same path, distinct filter state.
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-filter 'user-only)
    (let ((browser-buf (current-buffer)))
      (with-temp-buffer
        (let ((transient--shadowed-buffer browser-buf))
          (should (equal (transient-with-shadowed-buffer
                           (pilish--tree-dispatch-heading))
                         "filter:user-only")))))))

;;;; Header-Line Help Hint

(ert-deftest pilish-test-session-browser-header-line-help-hint ()
  "Session browser header-line includes `?:help' hint."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items '((:id "a")))
    (let ((header (pilish--session-browser-header-line)))
      (should (string-match-p "?:help" header)))))

(ert-deftest pilish-test-tree-browser-header-line-help-hint ()
  "Tree browser header-line includes `?:help' hint."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let ((header (pilish--tree-browser-header-line)))
      (should (string-match-p "?:help" header)))))

;;;; Startup Message

(ert-deftest pilish-test-session-browser-startup-message ()
  "Session browser shows help hint message on first creation."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pilish--session-browser-fetch-and-render)
               #'ignore)
              ((symbol-function 'pilish--get-chat-buffer)
               (lambda () nil))
              ((symbol-function 'pilish--session-directory)
               (lambda () "/tmp/pi-test/")))
      (pilish-session-browser)
      (unwind-protect
          (should (member "Pi: Press ? for available commands" messages))
        (when-let ((buf (get-buffer
                         (pilish--session-browser-buffer-name
                          "/tmp/pi-test/"))))
          (kill-buffer buf))))))

(ert-deftest pilish-test-tree-browser-startup-message ()
  "Tree browser shows help hint message on first creation.
Phase 3's entry-point guard requires a live chat link, so the test
provides one — a plain live buffer suffices; only liveness is
checked, and the fetch seam stays stubbed out."
  (let ((messages nil)
        (chat-buf (generate-new-buffer " *test-tree-startup-chat*")))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages)))
                  ((symbol-function 'pilish--tree-browser-fetch-and-render)
                   #'ignore)
                  ((symbol-function 'pilish--get-chat-buffer)
                   (lambda () chat-buf))
                  ((symbol-function 'pilish--session-directory)
                   (lambda () "/tmp/pi-test/")))
          (pilish-tree-browser)
          (should (member "Pi: Press ? for available commands" messages)))
      (when-let ((buf (get-buffer
                       (pilish--tree-browser-buffer-name
                        "/tmp/pi-test/"))))
        (kill-buffer buf))
      (kill-buffer chat-buf))))

;;;; Point Restoration (Phase 0 fix)

(ert-deftest pilish-test-session-browser-rerender-restores-point ()
  "Rerender restores point to the same section and column.
pr-145's docstring claim was false: erasing the buffer always moved
point to bob.  Phase 0 restores it by section identity (value match)."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :name "Session B"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")
                '(:path "/test/c.jsonl" :name "Session C"
                  :messageCount 10 :modified "2026-02-22T10:00:00Z")))
    (setq pilish--session-browser-view 'messages)
    (pilish--session-browser-rerender)
    ;; Move point into session B's line, a few columns past bol
    (goto-char (point-min))
    (search-forward "Session B")
    (beginning-of-line)
    (forward-char 2)
    (let ((column (current-column)))
      (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
      (pilish--session-browser-rerender)
      ;; Same section under point, same column
      (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
      (should (= (current-column) column)))))

(ert-deftest pilish-test-session-browser-rerender-point-min-when-gone ()
  "Rerender falls back to point-min when the section at point disappears."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :firstMessage "Unnamed prompt"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")))
    (setq pilish--session-browser-view 'messages)
    (pilish--session-browser-rerender)
    ;; Point on the unnamed session
    (goto-char (point-min))
    (search-forward "Unnamed prompt")
    (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
    ;; Named-only filter removes it; its section is gone after rerender
    (setq pilish--session-browser-named-only t)
    (pilish--session-browser-rerender)
    (should (= (point) (point-min)))))

(ert-deftest pilish-test-browse-rerender-syncs-window-point ()
  "Rerender restores `window-point', not just the buffer's own point.
The final fetch render runs from a timer while ANOTHER window is
selected: `goto-char' inside `with-current-buffer' moves the buffer's
point only, and every window displaying the browser buffer keeps its
own point — which `erase-buffer' already collapsed to bob.  The pane
shows point-at-top although the restore did work on the buffer's own
point (the intermittent instrumentation-vs-pane disagreement from
E2E).  The rerender must also `set-window-point' on live windows
displaying the buffer (same idiom as
`pilish--with-scroll-preservation' in ui.el)."
  (let* ((browser (get-buffer-create " *pi-test-browser-winpoint*"))
         (scratch (get-buffer-create " *pi-test-scratch-winpoint*"))
         (w (selected-window))
         (other (split-window))
         (orig-buffer (window-buffer w)))
    (unwind-protect
        (progn
          (set-window-buffer w browser)
          (set-window-buffer other scratch)
          (with-current-buffer browser
            (pilish-session-browser-mode)
            (setq pilish--session-browser-items
                  (list '(:path "/test/a.jsonl" :name "Session A"
                          :messageCount 42 :modified "2026-02-24T10:00:00Z")
                        '(:path "/test/b.jsonl" :name "Session B"
                          :messageCount 20 :modified "2026-02-23T10:00:00Z")
                        '(:path "/test/c.jsonl" :name "Session C"
                          :messageCount 10 :modified "2026-02-22T10:00:00Z")))
            (setq pilish--session-browser-view 'messages)
            (pilish--session-browser-rerender))
          ;; Put W's point on the middle row (Most-messages order is A, B, C),
          ;; a few columns past bol, while W is selected so the buffer's
          ;; own point follows.
          (select-window w)
          (with-current-buffer browser
            (goto-char (point-min))
            (search-forward "Session B")
            (beginning-of-line)
            (forward-char 2))
          ;; Sanity: W's point sits on session B's section.
          (should (equal (oref (with-current-buffer browser
                                 (magit-section-at (window-point w)))
                               value)
                         "/test/b.jsonl"))
          ;; Timer-like context: the rerender runs with ANOTHER window
          ;; selected (the mechanism is window selection, not the timer).
          (select-window other)
          (should-not (eq (selected-window) w))
          (with-current-buffer browser
            (pilish--session-browser-rerender))
          ;; W's window-point must sit on the same section again (the
          ;; section is looked up by buffer position, in the browser
          ;; buffer — `magit-section-at' reads text properties in the
          ;; current buffer).
          (let ((pos (window-point w)))
            (should (equal (oref (with-current-buffer browser
                                   (magit-section-at pos))
                                 value)
                           "/test/b.jsonl"))
            (should (= (with-current-buffer browser
                         (save-excursion (goto-char pos) (current-column)))
                       2))))
      (delete-other-windows)
      (set-window-buffer (selected-window) orig-buffer)
      (kill-buffer browser)
      (kill-buffer scratch))))

(ert-deftest pilish-test-session-browser-fetch-claims-generation-before-render ()
  "A loading render cannot reverse reentrant fetch generation order.
Request A claims generation 1 before rendering.  Its render starts B,
which claims generation 2 and queues the only scan; when A resumes, it
is already stale and performs no directory resolution."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (let ((first-render t)
          (directory-calls 0)
          (queue nil))
      (cl-letf (((symbol-function 'pilish--session-browser-rerender)
                 (lambda (&optional _fallback)
                   (when first-render
                     (setq first-render nil)
                     (pilish--session-browser-fetch-and-render))))
                ((symbol-function 'pilish--browse-session-directories)
                 (lambda (_scope &optional _buf _token)
                   (cl-incf directory-calls)
                   nil))
                ((symbol-function 'pilish--browse-session-files)
                 (lambda (_dirs &optional _buf _token) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest args)
                   (push (cons function args) queue))))
        (pilish--session-browser-fetch-and-render)
        (should (= pilish--session-browser-fetch-token 2))
        (should (= directory-calls 1))
        (should (= (length queue) 1))
        (should pilish--session-browser-loading)
        (let ((job (pop queue)))
          (apply (car job) (cdr job))))
      (should-not pilish--session-browser-loading)
      (should-not pilish--session-browser-error)
      (should (eq pilish--session-browser-items-scope 'current)))))

(ert-deftest pilish-test-session-browser-fetch-preserves-point ()
  "The full fetch cycle (`g' refresh) keeps point on the same row.
`--session-browser-fetch-and-render' renders an intermediate loading
state with no session sections before the final items render; point
must survive the whole cycle, not just a plain rerender (E2E defect
A4: `g' dropped point to bob because the loading render's rerender
lost the captured section ident)."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                         :messageCount 42 :modified "2026-02-24T10:00:00Z")
                       '(:path "/test/b.jsonl" :name "Session B"
                         :messageCount 20 :modified "2026-02-23T10:00:00Z")
                       '(:path "/test/c.jsonl" :name "Session C"
                         :messageCount 10 :modified "2026-02-22T10:00:00Z"))))
      (setq pilish--session-browser-items items
            pilish--session-browser-view 'messages)
      (pilish--session-browser-rerender)
      ;; Point on the middle row (Most-messages order is A, B, C), a few
      ;; columns past bol
      (goto-char (point-min))
      (search-forward "Session B")
      (beginning-of-line)
      (forward-char 2)
      (let ((column (current-column)))
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        ;; Refresh: the scan returns the SAME items, synchronously
        (cl-letf (((symbol-function 'pilish--browse-load-sessions)
                   (lambda (_scope callback &optional _generation)
                     (funcall callback items nil)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))
          (pilish--session-browser-fetch-and-render))
        (should-not pilish--session-browser-loading)
        ;; Same section under point, same column, after the final render
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (should (= (current-column) column))))))

(ert-deftest pilish-test-session-browser-fetch-point-min-when-gone ()
  "The fetch cycle falls back to point-min when the row at point is gone.
A refresh whose new item set no longer contains the pointed-at session
must leave point at bob, not on a stale neighbor — the same fallback a
plain rerender already guarantees."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :name "Session B"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")
                '(:path "/test/c.jsonl" :name "Session C"
                  :messageCount 10 :modified "2026-02-22T10:00:00Z")))
    (setq pilish--session-browser-view 'messages)
    (pilish--session-browser-rerender)
    ;; Point on the middle row
    (goto-char (point-min))
    (search-forward "Session B")
    (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
    ;; Refresh returns a set without session B (the named-only effect,
    ;; via a different item set)
    (cl-letf (((symbol-function 'pilish--browse-load-sessions)
               (lambda (_scope callback &optional _generation)
                 (funcall callback
                          (list '(:path "/test/a.jsonl" :name "Session A"
                                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                                '(:path "/test/c.jsonl" :name "Session C"
                                  :messageCount 10 :modified "2026-02-22T10:00:00Z"))
                          nil)))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args))))
      (pilish--session-browser-fetch-and-render))
    (should (= (point) (point-min)))))

(ert-deftest pilish-test-session-browser-refresh-during-load-preserves-point ()
  "Pressing `g' while a load is in flight keeps point on its row.
A refresh issued during another refresh used to lose point twice
over: the first fetch's loading render already destroyed the session
sections (point sits on the bare loading line under the root
section, so the second fetch captures no anchor), and the
fetch token drops the older scan before its callback ever renders.
The surviving fetch's final render then has neither a fresh anchor
nor a fallback, and point lands at bob.  Point must survive a refresh
issued during another refresh."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                         :messageCount 42 :modified "2026-02-24T10:00:00Z")
                       '(:path "/test/b.jsonl" :name "Session B"
                         :messageCount 20 :modified "2026-02-23T10:00:00Z")
                       '(:path "/test/c.jsonl" :name "Session C"
                         :messageCount 10 :modified "2026-02-22T10:00:00Z")))
          (in-flight-callback nil))
      (setq pilish--session-browser-items items
            pilish--session-browser-view 'messages)
      (pilish--session-browser-rerender)
      ;; Point on the middle row (Most-messages order is A, B, C), a few
      ;; columns past bol
      (goto-char (point-min))
      (search-forward "Session B")
      (beginning-of-line)
      (forward-char 2)
      (let ((column (current-column)))
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (cl-letf (((symbol-function 'pilish--browse-load-sessions)
                   ;; Fetch A: return control with the scan mid-flight —
                   ;; capture the callback, funcall nothing yet.
                   (lambda (_scope callback &optional _generation)
                     (setq in-flight-callback callback)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))
          (pilish--session-browser-fetch-and-render)
          ;; The first fetch is still loading; its loading render left
          ;; no session sections to anchor to.
          (should pilish--session-browser-loading)
          (should (string-match-p "Loading" (buffer-string)))
          (should in-flight-callback)
          ;; While loading, press `g' again: fetch B reports the same
          ;; items synchronously (and, as with the real fetch token,
          ;; fetch A's callback never runs — it is dropped, not queued).
          (cl-letf (((symbol-function 'pilish--browse-load-sessions)
                     (lambda (_scope callback &optional _generation)
                       (funcall callback items nil))))
            (pilish--session-browser-fetch-and-render))
          (should-not pilish--session-browser-loading))
        ;; The final render lands on the same middle row.
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (should (= (current-column) column))))))

(ert-deftest pilish-test-session-browser-fetch-renders-in-browser-buffer ()
  "The fetch callback renders in the browser buffer, not the caller's.
The real async scan reports back from a timer in whatever buffer
happens to be current; the final rerender must land in the browser
buffer (latent defect: it used to run outside `with-current-buffer',
leaving the browser stuck on its loading state)."
  (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                       :messageCount 42 :modified "2026-02-24T10:00:00Z")))
        (other (get-buffer-create " *pi-test-fetch-other*")))
    (unwind-protect
        (with-temp-buffer
          (pilish-session-browser-mode)
          (cl-letf (((symbol-function 'pilish--browse-load-sessions)
                     (lambda (_scope callback &optional _generation)
                       ;; Callback fires with some OTHER buffer current.
                       (with-current-buffer other
                         (funcall callback items nil))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (apply fn args))))
            (pilish--session-browser-fetch-and-render))
          ;; The browser buffer got the rows and cleared loading...
          (should-not pilish--session-browser-loading)
          (should (string-match-p "Session A" (buffer-string)))
          ;; ...and the other buffer got no render.
          (should-not (string-match-p "Session A"
                                      (with-current-buffer other
                                        (buffer-string)))))
      (kill-buffer other))))

(ert-deftest pilish-test-tree-browser-fetch-renders-in-browser-buffer ()
  "The tree fetch callback renders in the browser buffer, not the caller's.
Same latent defect as the session browser: the deferred disk read calls
back from a timer in whatever buffer is current.  The Phase 3 seam
callback receives (TREE LEAF-ID MESSAGE); the mock reports success, so
MESSAGE is nil and no process mock is needed anywhere (the tree comes
from disk, not the RPC)."
  (let* ((tree-data (pilish--parse-tree
                     (pilish-test--read-json-fixture "browse-tree.json")))
         (other (get-buffer-create " *pi-test-tree-other*")))
    (unwind-protect
        (with-temp-buffer
          (pilish-tree-browser-mode)
          (cl-letf (((symbol-function 'pilish--browse-load-tree)
                     (lambda (callback &optional _path _generation)
                       ;; Callback fires with some OTHER buffer current.
                       (with-current-buffer other
                         (funcall callback
                                  (plist-get tree-data :tree)
                                  (plist-get tree-data :leafId)
                                  nil))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (apply fn args))))
            (pilish--tree-browser-fetch-and-render))
          ;; The browser buffer got the tree and cleared loading...
          (should-not pilish--tree-browser-loading)
          (should (string-match-p "Actually" (buffer-string)))
          ;; ...and the other buffer got no render.
          (should-not (string-match-p "Actually"
                                      (with-current-buffer other
                                        (buffer-string)))))
      (kill-buffer other))))

(ert-deftest pilish-test-tree-browser-first-render-selects-active-leaf ()
  "A fresh tree render puts point on the active projected leaf."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      (should (equal (oref (magit-current-section) value) "node-8")))))

(ert-deftest pilish-test-tree-browser-first-render-selects-visible-active-ancestor ()
  "A filtered current tool/setting leaf selects its nearest visible ancestor."
  (let ((tree
         [(:id "u1" :type "message" :role "user" :preview "root"
           :children
           [(:id "a1" :parentId "u1" :type "message" :role "assistant"
             :preview "answer"
             :children
             [(:id "t1" :parentId "a1" :type "tool_result" :toolName "read"
               :preview "[read: file]"
               :children
               [(:id "m1" :parentId "t1" :type "model_change"
                 :provider "test" :modelId "model"
                 :children
                 [(:id "h1" :parentId "m1" :type "thinking_level_change"
                   :thinkingLevel "high" :children [])])])])])]))
    (dolist (case '(("t1" no-tools "a1")
                    ("m1" default "t1")
                    ("h1" default "t1")
                    ("h1" no-tools "a1")))
      (with-temp-buffer
        (pilish-tree-browser-mode)
        (setq pilish--tree-browser-tree tree
              pilish--tree-browser-leaf-id (nth 0 case)
              pilish--tree-browser-filter (nth 1 case))
        (pilish--tree-browser-rerender)
        (should (equal (oref (magit-current-section) value)
                       (nth 2 case)))))))

(ert-deftest pilish-test-tree-browser-first-render-has-deterministic-fallback ()
  "When the whole active path is hidden, select the first visible row."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "root" :type "message" :role "user" :preview "root"
            :children
            [(:id "active" :parentId "root" :type "message"
              :role "assistant" :preview "active" :children [])
             (:id "labeled" :parentId "root" :type "message"
              :role "assistant" :preview "saved" :label "keep"
              :children [])])]
          pilish--tree-browser-leaf-id "active"
          pilish--tree-browser-filter 'labeled-only)
    (pilish--tree-browser-rerender)
    (should (equal (oref (magit-current-section) value) "labeled"))))

(ert-deftest pilish-test-tree-browser-first-render-deep-active-leaf ()
  "Fresh orientation remains iterative for a deeply nested active path."
  (let* ((count 1500)
         (leaf (format "node-%d" count)))
    (with-temp-buffer
      (pilish-tree-browser-mode)
      (setq pilish--tree-browser-tree (pilish-test--make-deep-tree count)
            pilish--tree-browser-leaf-id leaf
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      (should (equal (oref (magit-current-section) value) leaf)))))

(ert-deftest pilish-test-tree-browser-rerender-restores-point ()
  "A user move wins over active-leaf orientation on later rerenders."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((response (pilish-test--read-json-fixture "browse-tree.json"))
           (tree-data (pilish--parse-tree response)))
      (setq pilish--tree-browser-tree (plist-get tree-data :tree)
            pilish--tree-browser-leaf-id (plist-get tree-data :leafId)
            pilish--tree-browser-filter 'default)
      (pilish--tree-browser-rerender)
      (should (equal (oref (magit-current-section) value) "node-8"))
      ;; The user moves to node-4; it survives the no-tools filter.
      (goto-char (point-min))
      (search-forward "Actually")
      (should (equal (oref (magit-current-section) value) "node-4"))
      (setq pilish--tree-browser-filter 'no-tools)
      (pilish--tree-browser-rerender)
      (should (equal (oref (magit-current-section) value) "node-4")))))

(ert-deftest pilish-test-tree-browser-missing-selection-uses-path-then-active ()
  "Filter/search loss selects an ancestor, then the active-path target."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "root" :type "message" :role "user" :preview "common root"
            :children
            [(:id "active-parent" :parentId "root" :type "message"
              :role "assistant" :preview "active parent"
              :children
              [(:id "active-leaf" :parentId "active-parent" :type "message"
                :role "user" :preview "active target" :children [])])
             (:id "other-parent" :parentId "root" :type "message"
              :role "assistant" :preview "inactive ancestor"
              :children
              [(:id "other-tool" :parentId "other-parent" :type "tool_result"
                :toolName "read" :preview "[read: hidden child]"
                :children [])])])]
          pilish--tree-browser-leaf-id "active-leaf"
          pilish--tree-browser-filter 'default)
    (pilish--tree-browser-rerender)
    ;; Select an inactive-branch tool.  It survives a matching query.
    (goto-char (point-min))
    (search-forward "hidden child")
    (should (equal (oref (magit-current-section) value) "other-tool"))
    (setq pilish--tree-browser-search-query "hidden"
          pilish--tree-browser-search-tokens '("hidden"))
    (pilish--tree-browser-rerender)
    (should (equal (oref (magit-current-section) value) "other-tool"))
    ;; no-tools removes it; its nearest visible projected ancestor wins.
    (setq pilish--tree-browser-search-query nil
          pilish--tree-browser-search-tokens nil
          pilish--tree-browser-filter 'no-tools)
    (pilish--tree-browser-rerender)
    (should (equal (oref (magit-current-section) value) "other-parent"))
    ;; A query can remove that whole path; then orient to the visible
    ;; active-path target rather than an unrelated numeric row.
    (setq pilish--tree-browser-search-query "active target"
          pilish--tree-browser-search-tokens '("active" "target"))
    (pilish--tree-browser-rerender)
    (should (equal (oref (magit-current-section) value) "active-leaf"))))

(ert-deftest pilish-test-tree-browser-refresh-uses-old-selected-lineage ()
  "A vanished selection resolves through its old, not replacement, lineage.
The old root has active and selected children; the replacement keeps
only root → active.  Root is the nearest surviving old ancestor and
must win over the replacement's active leaf."
  (let ((old-tree
         [(:id "root" :type "message" :role "user" :preview "root"
           :children
           [(:id "active" :parentId "root" :type "message"
             :role "assistant" :preview "active" :children [])
            (:id "selected" :parentId "root" :type "message"
             :role "assistant" :preview "selected" :children [])])])
        (new-tree
         [(:id "root" :type "message" :role "user" :preview "root"
           :children
           [(:id "active" :parentId "root" :type "message"
             :role "assistant" :preview "active" :children [])])])
        (chat-buf (generate-new-buffer " *test-tree-old-lineage-chat*"))
        (responses nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state '(:session-file "/tmp/old-lineage.jsonl")))
          (setq responses (list (list old-tree "active")
                                (list new-tree "active")))
          (pilish-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'redisplay) #'ignore)
                      ((symbol-function 'pilish--browse-load-tree)
                       (lambda (callback &optional _path _generation)
                         (pcase-let ((`(,tree ,leaf) (pop responses)))
                           (funcall callback tree leaf nil)))))
              (pilish--tree-browser-fetch-and-render)
              (goto-char (point-min))
              (search-forward "selected")
              (should (equal (oref (magit-current-section) value)
                             "selected"))
              (pilish--tree-browser-fetch-and-render)
              (should (equal (oref (magit-current-section) value)
                             "root")))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-browser-empty-search-remembers-selection ()
  "An empty search result does not discard the last section identity."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "u1" :type "message" :role "user" :preview "first"
            :children
            [(:id "a1" :parentId "u1" :type "message" :role "assistant"
              :preview "second" :children [])])]
          pilish--tree-browser-leaf-id "a1"
          pilish--tree-browser-filter 'default)
    (pilish--tree-browser-rerender)
    (goto-char (point-min))
    (should (equal (oref (magit-current-section) value) "u1"))
    (setq pilish--tree-browser-search-query "absent"
          pilish--tree-browser-search-tokens '("absent"))
    (pilish--tree-browser-rerender)
    (should (string-match-p "No matching entries" (buffer-string)))
    (setq pilish--tree-browser-search-query nil
          pilish--tree-browser-search-tokens nil)
    (pilish--tree-browser-rerender)
    (should (equal (oref (magit-current-section) value) "u1"))))

;;;; Phase 0 Stub Seams

(ert-deftest pilish-test-browse-stub-loaders-render-empty-states ()
  "Seam callbacks render empty and error states without signaling.
Both browsers read from disk, so neither needs a live process or a
--get-process mock: the session browser (Phase 2 disk scan) renders
its empty state with no sessions, and the tree browser (Phase 3 disk
read) with no linked chat renders its link-error message."
  (let ((session-buf (generate-new-buffer " *test-sessions*"))
        (tree-buf (generate-new-buffer " *test-tree*"))
        (root (pilish-test--make-temp-directory "pi-stub-root")))
    (unwind-protect
        (progn
          (with-current-buffer session-buf
            (pilish-session-browser-mode)
            ;; No --get-process mock: reading from disk needs no process.
            (let ((default-directory root)
                  (process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                (directory-file-name root))
                         process-environment)))
              (cl-letf (((symbol-function 'pilish--session-list-directory)
                         (lambda (&optional _chat-buf) nil))
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args))))
                (pilish--session-browser-fetch-and-render)))
            (should-not pilish--session-browser-loading)
            (should-not pilish--session-browser-error)
            (should (string-match-p "No sessions in this project"
                                    (buffer-string))))
          (with-current-buffer tree-buf
            (pilish-tree-browser-mode)
            ;; No process mock and no chat link: the fetch renders the
            ;; link-error state instead of signaling.
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (apply fn args))))
              (pilish--tree-browser-fetch-and-render))
            (should-not pilish--tree-browser-loading)
            (should (string-match-p "No linked pi chat session"
                                    (buffer-string)))))
      (kill-buffer session-buf)
      (kill-buffer tree-buf))))

(ert-deftest pilish-test-browse-stub-actions-signal-user-error ()
  "Action seam contracts without a linked chat session.
The switch seam's Phase 2 contract is the no-session error when no
chat buffer is linked; labeling went live in Phase 3 and reports
Cannot-label instead of signaling.  Navigate went live in Phase 4 —
its guard contracts are pinned by the navigate tests below, so only
the RET routing stays pinned here, a structural binding check."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (should (equal (error-message-string
                    (should-error
                     (pilish--browse-switch-session "/test/a.jsonl")
                     :type 'user-error))
                   "No pi session to switch to")))
  ;; RET still routes the tree browser — mode map and node sections —
  ;; into the navigate command.
  (should (eq (lookup-key pilish-tree-browser-mode-map (kbd "RET"))
              'pilish-tree-browser-navigate))
  (should (eq (lookup-key pilish-tree-node-section-map (kbd "RET"))
              'pilish-tree-browser-navigate))
  ;; Labeling without a resolvable session file: message, no signal.
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (pilish--browse-set-label "node-1" "label"))
    (should (cl-some (lambda (m)
                       (string-match-p "Cannot label: no session file" m))
                     messages))))

;;;; Phase 2: Raw Session File Helpers

(defconst pilish-test--browse-timestamp "2026-03-02T10:00:00.000Z"
  "Fixed entry timestamp for browse test session lines.")

(defconst pilish-test--iso-second-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'"
  "Regexp for a second-resolution UTC ISO-8601 timestamp.")

(defun pilish-test--jsonl-line (type id parent &rest payload)
  "Return a raw JSONL line for an entry of TYPE, ID, and PARENT id.
PARENT is an id string or nil (JSON null).  PAYLOAD is the plist tail
(:message, :targetId, :name, ...)."
  (json-encode (append (list :type type :id id :parentId parent
                             :timestamp pilish-test--browse-timestamp)
                       payload)))

(defun pilish-test--user-line (id parent text)
  "Return a raw user message JSONL line with content TEXT."
  (pilish-test--jsonl-line
   "message" id parent :message `(:role "user" :content ,text)))

(defun pilish-test--write-session-lines (path lines &optional omit-final-newline)
  "Write raw string LINES to PATH, separated by single newlines.
OMIT-FINAL-NEWLINE skips the trailing newline, producing the shape of a
crashed or hand-edited session file (rename must re-add the separator)."
  (with-temp-file path
    (when lines
      (insert (mapconcat #'identity lines "\n"))
      (unless omit-final-newline (insert "\n")))))

(defun pilish-test--file-contents (path &optional literally)
  "Return PATH's contents.
When LITERALLY is non-nil, return unibyte file bytes with no coding or
end-of-line conversion; otherwise return decoded text."
  (with-temp-buffer
    (if literally
        (progn
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path))
      (insert-file-contents path))
    (buffer-string)))

(defun pilish-test--make-session-header (id &rest extra)
  "Return a raw session header line with id ID and EXTRA plist tail."
  (json-encode (append (list :type "session" :version 3 :id id
                             :timestamp pilish-test--browse-timestamp
                             :cwd "/home/fake/a")
                       extra)))

(defmacro pilish-test--with-browse-link (chat-buf &rest body)
  "Run BODY in a session-browser buffer linked to CHAT-BUF."
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (pilish-session-browser-mode)
     (setq pilish--chat-buffer ,chat-buf)
     ,@body))

(ert-deftest pilish-test-session-browser-mode-reinit-scan-generation-monotonic ()
  "A mode reset cannot revive an older same-number session scan.
A scans the current-project directory and queues its real JSONL reader.
The mode is re-run, then B scans both directories for All projects.
Timers run B before A, reproducing the collision in which both requests
used generation 1: only B may publish, and the displayed snapshot must
remain owned by its requested All-projects scope."
  (let* ((root (pilish-test--make-temp-directory "pi-reinit-scan-"))
         (current-dir (expand-file-name "--current--" root))
         (other-dir (expand-file-name "--other--" root))
         (current-path (expand-file-name "current.jsonl" current-dir))
         (other-path (expand-file-name "other.jsonl" other-dir))
         (queue nil)
         (requested-scopes nil)
         generation-a
         generation-after-reset
         generation-b)
    (make-directory current-dir t)
    (make-directory other-dir t)
    (pilish-test--write-session-lines
     current-path
     (list (pilish-test--make-session-header "sid-reinit-current")
           (pilish-test--user-line "current" nil "CURRENT SCAN A")))
    (pilish-test--write-session-lines
     other-path
     (list (pilish-test--make-session-header "sid-reinit-other")
           (pilish-test--user-line "other" nil "ALL SCAN B ONLY")))
    (unwind-protect
        (with-temp-buffer
          (let ((pilish-session-browser-default-scope 'current)
                (pilish-session-browser-default-view 'messages)
                (pilish-session-browser-default-named-only nil)
                (real-float-time (symbol-function 'float-time)))
            (pilish-session-browser-mode)
            (cl-letf (((symbol-function 'pilish--browse-session-directories)
                       (lambda (scope &optional _buf _token)
                         (push scope requested-scopes)
                         (if (eq scope 'all)
                             (list current-dir other-dir)
                           (list current-dir))))
                      ((symbol-function 'pilish--browse-live-session-paths)
                       (lambda () (make-hash-table :test #'equal)))
                      ((symbol-function 'float-time)
                       (lambda (&optional time)
                         ;; Keep each real scanner invocation in one slice;
                         ;; preserve normal age calculations that pass TIME.
                         (if time (funcall real-float-time time) 0.0)))
                      ((symbol-function 'run-at-time)
                       (lambda (_seconds _repeat function &rest args)
                         (when (eq function
                                   #'pilish--browse-scan-session-files)
                           (push (cons function args) queue)))))
              (pilish--session-browser-fetch-and-render)
              (setq generation-a pilish--session-browser-fetch-token)
              (should (= (length queue) 1))

              ;; The documented reset flow reapplies defaults.  Request B
              ;; then deliberately chooses the other scope.
              (pilish-session-browser-mode)
              (setq generation-after-reset
                    pilish--session-browser-fetch-token
                    pilish--session-browser-scope 'all)
              (pilish--session-browser-fetch-and-render)
              (setq generation-b pilish--session-browser-fetch-token)
              (should (= (length queue) 2))

              ;; `push' puts B first.  B publishes its two rows, then A's
              ;; obsolete timer gets its chance to overwrite them.
              (while queue
                (let ((job (pop queue)))
                  (apply (car job) (cdr job))))

              (should-not pilish--session-browser-loading)
              (should-not pilish--session-browser-error)
              (should (eq pilish--session-browser-scope 'all))
              (should (eq pilish--session-browser-items-scope 'all))
              (should (= (length pilish--session-browser-items) 2))
              (should (string-match-p "ALL SCAN B ONLY" (buffer-string)))
              (should (equal (reverse requested-scopes) '(current all)))
              (should (> generation-after-reset generation-a))
              (should (> generation-b generation-after-reset)))))
      (delete-directory root t))))

(ert-deftest pilish-test-session-browser-mode-reinit-without-scan-still-fetches ()
  "A plain mode reset reapplies defaults and permits a fresh fetch.
No request is in flight at reset time; this protects the documented
restore-defaults flow from generation-lifetime bookkeeping changes."
  (with-temp-buffer
    (let ((pilish-session-browser-default-scope 'all)
          (pilish-session-browser-default-view 'messages)
          (pilish-session-browser-default-named-only t)
          requested)
      (pilish-session-browser-mode)
      (setq pilish--session-browser-scope 'current
            pilish--session-browser-view 'threaded
            pilish--session-browser-named-only nil
            pilish--session-browser-search-query "old query"
            pilish--session-browser-search-tokens '("old"))
      (pilish-session-browser-mode)
      (should (eq pilish--session-browser-scope 'all))
      (should (eq pilish--session-browser-view 'messages))
      (should pilish--session-browser-named-only)
      (should-not pilish--session-browser-search-query)
      (should-not pilish--session-browser-search-tokens)
      (cl-letf (((symbol-function 'pilish--browse-load-sessions)
                 (lambda (scope callback &optional generation)
                   (setq requested (list scope generation))
                   (funcall callback
                            (list '(:path "/tmp/fresh-reset.jsonl"
                                    :cwd "/tmp/fresh-project"
                                    :name "Fresh after reset"
                                    :messageCount 1
                                    :modified "2026-03-12T10:00:00Z"))
                            nil)))
                ((symbol-function 'pilish--browse-live-session-paths)
                 (lambda () (make-hash-table :test #'equal))))
        (pilish--session-browser-fetch-and-render))
      (should (eq (car requested) 'all))
      (should (eq (cadr requested)
                  pilish--session-browser-fetch-token))
      (should-not pilish--session-browser-loading)
      (should (eq pilish--session-browser-items-scope 'all))
      (should (string-match-p "Fresh after reset" (buffer-string))))))

;;;; Phase 2: Disk Scan and Chunked Loading

(ert-deftest pilish-test-browse-current-session-directory-without-menu ()
  "Fall back to the munged current-project directory without menu.el."
  (let ((saved-function
         (symbol-function 'pilish--session-list-directory))
        (sandbox (make-temp-file "pi-browse-current-" t)))
    (unwind-protect
        (let ((agent-root (expand-file-name "agent" sandbox))
              (project (expand-file-name "project" sandbox)))
          (make-directory agent-root)
          (make-directory project)
          (fmakunbound 'pilish--session-list-directory)
          (let ((default-directory (file-name-as-directory project))
                (pilish--chat-buffer nil)
                (process-environment (copy-sequence process-environment)))
            (setenv "PI_CODING_AGENT_DIR" agent-root)
            (should
             (equal
              (pilish--browse-current-session-directory)
              (pilish-jsonl-session-dir-for-cwd
               default-directory)))))
      (fset 'pilish--session-list-directory saved-function)
      (when (file-directory-p sandbox)
        (delete-directory sandbox t)))))

(ert-deftest pilish-test-scan-discovers-tree ()
  "--browse-load-sessions scans the sessions tree from disk.
scope=all finds every munged --…-- directory under the sessions root,
threads forks through :parentSessionPath, reads names from
session_info, and skips non-session JSONL files, .subagents sidecars,
and non-munged directories.  scope=current scans one directory."
  (let* ((root (pilish-test--make-temp-directory "pi-scan-root"))
         (sessions (expand-file-name "sessions" root))
         (dir-a (expand-file-name "--home-fake-a--" sessions))
         (dir-b (expand-file-name "--home-fake-b--" sessions))
         (subagents (expand-file-name ".subagents" dir-a))
         (stray (expand-file-name "straydir" sessions))
         (root-path (expand-file-name "root.jsonl" dir-a))
         (fork-path (expand-file-name "fork.jsonl" dir-a))
         (other-path (expand-file-name "other.jsonl" dir-b))
         (broken-path (expand-file-name "broken.jsonl" dir-b))
         (sub-path (expand-file-name "sub.jsonl" subagents))
         (stray-path (expand-file-name "stray.jsonl" stray))
         (calls nil))
    (make-directory dir-a t)
    (make-directory dir-b t)
    (make-directory subagents t)
    (make-directory stray t)
    (pilish-test--write-session-lines
     root-path
     (list (pilish-test--make-session-header "sid-root")
           (pilish-test--user-line "m1" nil "fix the parser")
           (pilish-test--jsonl-line
            "message" "m2" "m1"
            :message '(:role "toolResult" :toolCallId "tc1" :toolName "read"))
           (pilish-test--jsonl-line
            "message" "m3" "m2"
            :message '(:role "assistant" :content "done"))
           (pilish-test--jsonl-line
            "session_info" "s1" "m3" :name "Root work")))
    (pilish-test--write-session-lines
     fork-path
     (list (pilish-test--make-session-header
            "sid-fork" :parentSession root-path)
           (pilish-test--user-line "f1" nil "try the other way")))
    (pilish-test--write-session-lines
     other-path
     (list (pilish-test--make-session-header "sid-other")
           (pilish-test--user-line "o1" nil "unrelated work")))
    ;; Decoy: a .jsonl file that is not a session (no header line).
    (pilish-test--write-session-lines
     broken-path
     (list (pilish-test--user-line "x1" nil "decoy")))
    ;; Valid sessions in excluded locations: a .subagents sidecar (the
    ;; scan is single-level inside munged dirs) and a non-munged dir.
    (pilish-test--write-session-lines
     sub-path (list (pilish-test--make-session-header "sid-sub")))
    (pilish-test--write-session-lines
     stray-path (list (pilish-test--make-session-header "sid-stray")))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'pilish--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ;; Synchronous timers: the chunked scan completes here.
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (pilish--browse-load-sessions
           'all (lambda (items error) (push (list items error) calls))))
        (should (eq (length calls) 1))
        (pcase-let ((`(,items ,error) (car calls)))
          (should-not error)
          (should (= (length items) 3))
          (let ((paths (mapcar (lambda (item) (plist-get item :path)) items)))
            (should (member root-path paths))
            (should (member fork-path paths))
            (should (member other-path paths))
            (should-not (member broken-path paths))
            (should-not (member sub-path paths))
            (should-not (member stray-path paths)))
          (let ((root-item (cl-find root-path items
                                    :key (lambda (i) (plist-get i :path))
                                    :test #'equal))
                (fork-item (cl-find fork-path items
                                    :key (lambda (i) (plist-get i :path))
                                    :test #'equal)))
            (should root-item)
            (should fork-item)
            (should (equal (plist-get root-item :id) "sid-root"))
            (should (equal (plist-get root-item :cwd) "/home/fake/a"))
            (should (equal (plist-get root-item :created)
                           pilish-test--browse-timestamp))
            (should (equal (plist-get root-item :name) "Root work"))
            (should (equal (plist-get root-item :firstMessage) "fix the parser"))
            (should (= (plist-get root-item :messageCount) 3))
            (should (string-match-p pilish-test--iso-second-re
                                    (plist-get root-item :modified)))
            ;; The fork threads to its parent session file.
            (should (equal (plist-get fork-item :parentSessionPath) root-path))
            (should-not (plist-get fork-item :name))))
        ;; scope=current scans exactly one default project directory;
        ;; align the browser's cwd with the fixture's munged directory.
        (setq calls nil)
        (cl-letf (((symbol-function 'pilish--session-list-directory)
                   (lambda (&optional _chat-buf) dir-a))
                  ((symbol-function 'pilish--session-directory)
                   (lambda () "/home/fake/a/"))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (pilish--browse-load-sessions
           'current (lambda (items error) (push (list items error) calls))))
        (pcase-let ((`(,items ,error) (car calls)))
          (should-not error)
          (should (equal (sort (mapcar (lambda (i) (plist-get i :path)) items)
                               #'string<)
                         (sort (list root-path fork-path) #'string<))))))))

(ert-deftest pilish-test-scan-discovers-flat-custom-session-dir ()
  "Custom sessionDir scans flat JSONL and scopes by the header cwd."
  (let* ((sandbox (pilish-test--make-temp-directory "pi-flat-scan-"))
         (dir (expand-file-name "custom-sessions" sandbox))
         (project-a (expand-file-name "project-a" sandbox))
         (project-b (expand-file-name "project-b" sandbox))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (broken (expand-file-name "broken.jsonl" dir))
         (nested (expand-file-name "--old-project--" dir))
         (open-session-info (symbol-function 'pilish-jsonl-open-session-info))
         (opened nil)
         (calls nil))
    (make-directory dir t)
    (make-directory project-a)
    (make-directory project-b)
    (make-directory nested)
    (pilish-test--write-session-lines
     path-a (list (json-encode (list :type "session" :version 3 :id "sid-a"
                                     :timestamp pilish-test--browse-timestamp
                                     :cwd project-a))
                  (pilish-test--user-line "a1" nil "Project A")))
    (pilish-test--write-session-lines
     path-b (list (json-encode (list :type "session" :version 3 :id "sid-b"
                                     :timestamp pilish-test--browse-timestamp
                                     :cwd project-b))
                  (pilish-test--user-line "b1" nil "Project B")))
    (pilish-test--write-session-lines
     broken (list (pilish-test--user-line "not-a-header" nil "decoy")))
    (pilish-test--write-session-lines
     (expand-file-name "old.jsonl" nested)
     (list (json-encode (list :type "session" :version 3 :id "sid-old"
                              :timestamp pilish-test--browse-timestamp
                              :cwd project-a))))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (let ((default-directory (file-name-as-directory project-a)))
        (should (equal (pilish--session-directory)
                       (file-name-as-directory project-a)))
        (cl-letf (((symbol-function 'pilish--session-list-directory)
                   (lambda (&optional _chat-buf) dir))
                  ((symbol-function 'pilish-jsonl-open-session-info)
                   (lambda (path &optional search-text)
                     (push path opened)
                     (funcall open-session-info path search-text)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (pilish--browse-load-sessions
           'current (lambda (items error) (push (list items error) calls)))
          (pcase-let ((`(,items ,error) (car calls)))
            (should-not error)
            (should (equal (mapcar (lambda (item) (plist-get item :path)) items)
                           (list path-a)))
            ;; Unrelated and invalid files need only a short header read.
            (should (equal opened (list path-a))))
          (setq calls nil opened nil)
          (pilish--browse-load-sessions
           'all (lambda (items error) (push (list items error) calls)))
          (pcase-let ((`(,items ,error) (car calls)))
            (should-not error)
            (should (equal (sort (mapcar (lambda (item) (plist-get item :path))
                                         items)
                                 #'string<)
                           (list path-a path-b)))))))))

(ert-deftest pilish-test-flat-session-dir-with-munged-name ()
  "A custom --archives-- directory is flat, regardless of its basename."
  (let* ((sandbox (pilish-test--make-temp-directory "pi-flat-munged-"))
         (flat (expand-file-name "--archives--" sandbox))
         (project-a (expand-file-name "project-a" sandbox))
         (project-b (expand-file-name "project-b" sandbox))
         (path-a (expand-file-name "a.jsonl" flat))
         (path-b (expand-file-name "b.jsonl" flat))
         (chat (generate-new-buffer " *pilish-flat-munged-chat*"))
         (original-open (symbol-function 'pilish-jsonl-open-session-info))
         opened)
    (unwind-protect
        (progn
          (make-directory flat)
          (make-directory project-a)
          (make-directory project-b)
          (pilish-test--write-session-file path-a "Project A" project-a)
          (pilish-test--write-session-file path-b "Project B" project-b)
          (with-current-buffer chat
            (setq default-directory (file-name-as-directory project-a)
                  pilish--canonical-session-directory default-directory
                  pilish--state (list :session-file path-a)))
          (pilish-test--with-browse-link chat
            (let ((default-directory (file-name-as-directory project-a))
                  (process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                 (expand-file-name "agent" sandbox))
                         process-environment)))
              (should (equal (directory-file-name
                              (pilish--session-list-directory)) flat))
              (cl-letf (((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args)))
                        ((symbol-function 'pilish-jsonl-open-session-info)
                         (lambda (path &optional search-text)
                           (push path opened)
                           (funcall original-open path search-text))))
                (dolist (case `((current ,(list path-a))
                                (all ,(list path-a path-b))))
                  (let (calls)
                    (setq opened nil)
                    (pilish--browse-load-sessions
                     (car case)
                     (lambda (items error) (push (list items error) calls)))
                    (should (= (length calls) 1))
                    (should-not (cadar calls))
                    (should (equal (sort (mapcar (lambda (item)
                                                   (plist-get item :path))
                                                 (caar calls))
                                         #'string<)
                                   (cadr case)))
                    ;; Even with this name, current must not open B's history.
                    (when (eq (car case) 'current)
                      (should (equal opened (list path-a))))))))))
      (kill-buffer chat)
      (delete-directory sandbox t))))

(ert-deftest pilish-test-flat-session-current-uses-linked-git-subdirectory ()
  "The linked chat's cwd, not its Git root, identifies current sessions."
  (skip-unless (executable-find "git"))
  (let* ((sandbox (pilish-test--make-temp-directory "pi-flat-git-"))
         (flat (expand-file-name "custom-sessions" sandbox))
         (project (expand-file-name "project" sandbox))
         (nested (expand-file-name "nested" project))
         (path-a (expand-file-name "a.jsonl" flat))
         (path-b (expand-file-name "b.jsonl" flat))
         (chat (generate-new-buffer " *pilish-flat-git-chat*"))
         (original-open (symbol-function 'pilish-jsonl-open-session-info))
         opened calls)
    (unwind-protect
        (progn
          (make-directory flat)
          (make-directory nested t)
          (should (= (call-process "git" nil nil nil "-C" project "init" "-q") 0))
          (pilish-test--write-session-file path-a "Nested work" nested)
          ;; A separate pi cwd inside the same Git project is not current.
          (pilish-test--write-session-file path-b "Root work" project)
          (with-current-buffer chat
            (setq default-directory (file-name-as-directory nested)
                  pilish--canonical-session-directory default-directory
                  pilish--state (list :session-file path-a)))
          (pilish-test--with-browse-link chat
            (let ((default-directory (file-name-as-directory nested))
                  (process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                 (expand-file-name "agent" sandbox))
                         process-environment)))
              (should (equal (directory-file-name
                              (expand-file-name
                               (project-root (project-current))))
                             project))
              (should (equal (directory-file-name
                              (pilish--session-list-directory)) flat))
              (cl-letf (((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args)))
                        ((symbol-function 'pilish-jsonl-open-session-info)
                         (lambda (path &optional search-text)
                           (push path opened)
                           (funcall original-open path search-text))))
                (pilish--browse-load-sessions
                 'current (lambda (items error)
                            (push (list items error) calls)))
                (should (= (length calls) 1))
                (should-not (cadar calls))
                (should (equal (mapcar (lambda (item) (plist-get item :path))
                                       (caar calls))
                               (list path-a)))
                (should (equal opened (list path-a)))))))
      (kill-buffer chat)
      (delete-directory sandbox t))))

(ert-deftest pilish-test-flat-session-current-keeps-remote-route ()
  "Flat current scopes keep single-hop and multi-hop TRAMP identities."
  (dolist (route '("/ssh:target:" "/ssh:jump|ssh:target:"))
    (let* ((chat (generate-new-buffer " *pilish-flat-remote-chat*"))
           (dir (concat route "/flat/"))
           (path-a (concat dir "a.jsonl"))
           (path-b (concat dir "b.jsonl"))
           opened calls)
      (unwind-protect
          (progn
            (with-current-buffer chat
              (setq pilish--canonical-session-directory (concat route "/work/")
                    pilish--state (list :session-file path-a)))
            (pilish-test--with-browse-link chat
              ;; Keep filesystem IO synthetic, but use real TRAMP path
              ;; operations and real project identity/filtering code.
              (cl-letf (((symbol-function 'pilish--browse-session-files)
                         (lambda (dirs &rest _)
                           (should (equal dirs (list dir)))
                           (list path-a path-b)))
                        ((symbol-function 'pilish-jsonl-read-session-header)
                         (lambda (path)
                           (list :type "session" :cwd
                                 (if (equal path path-a) "/work" "/other"))))
                        ((symbol-function 'pilish-jsonl-open-session-info)
                         (lambda (path &rest _) (push path opened) path))
                        ((symbol-function 'pilish-jsonl-step-session-info)
                         (lambda (path &rest _)
                           (cons 'done (list :path path :cwd "/work"))))
                        ((symbol-function 'pilish-jsonl-close-session-info)
                         #'ignore)
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args))))
                (pilish--browse-load-sessions
                 'current (lambda (items error) (push (list items error) calls)))))
            (should (= (length calls) 1))
            (should-not (cadar calls))
            (should (equal (mapcar (lambda (item) (plist-get item :path))
                                   (caar calls))
                           (list path-a)))
            (should (equal opened (list path-a))))
        (kill-buffer chat)))))

(ert-deftest pilish-test-load-sessions-chunked ()
  "--browse-load-sessions chunks long scans and reports once.
The resumable reader is slowed so the scan spans several slices.
Synchronous timers deliver exactly one final callback with every item, and a superseded fetch's callback is dropped
by the fetch token."
  (let* ((root (pilish-test--make-temp-directory "pi-chunk-root"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (step-session-info (symbol-function 'pilish-jsonl-step-session-info))
         (paths nil))
    (make-directory dir t)
    (dotimes (i 60)
      (let ((path (expand-file-name (format "s%03d.jsonl" i) dir)))
        (push path paths)
        (pilish-test--write-session-lines
         path (list (pilish-test--make-session-header
                     (format "sid-%03d" i))))))
    (setq paths (nreverse paths))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'pilish--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ;; 2 ms per step: 60 files need several 10 ms slices.
                  ((symbol-function 'pilish-jsonl-step-session-info)
                   (lambda (state &optional deadline)
                     (sleep-for 0 2)
                     (funcall step-session-info state deadline))))
          ;; Synchronous timers: one final callback, all 60 items.
          (let ((calls nil))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args) (apply fn args))))
              (pilish--browse-load-sessions
               'all (lambda (items error) (push (list items error) calls))))
            (should (eq (length calls) 1))
            (pcase-let ((`(,items ,error) (car calls)))
              (should-not error)
              (should (= (length items) 60))
              (should (equal (sort (mapcar (lambda (i) (plist-get i :path)) items)
                                   #'string<)
                             (sort (copy-sequence paths) #'string<)))))
          ;; Deferred timers: the older fetch is superseded before any
          ;; slice runs, so its callback is dropped by the fetch token.
          (let ((calls-a nil) (calls-b nil) (queue nil))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (push (cons fn args) queue))))
              (pilish--browse-load-sessions
               'all (lambda (items error) (push (list items error) calls-a)))
              (pilish--browse-load-sessions
               'all (lambda (items error) (push (list items error) calls-b)))
              (while queue
                (let ((job (pop queue)))
                  (apply (car job) (cdr job)))))
            (should-not calls-a)
            (should (eq (length calls-b) 1))
            (pcase-let ((`(,items ,error) (car calls-b)))
              (should-not error)
              (should (= (length items) 60))))
          ;; Mid-flight supersession: A completes one slice (a few files)
          ;; before B supersedes it; the token still drops A at its next
          ;; slice boundary, and B reports alone with every item.
          (let ((calls-a nil) (calls-b nil) (queue nil))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (push (cons fn args) queue))))
              (pilish--browse-load-sessions
               'all (lambda (items error) (push (list items error) calls-a)))
              (let ((job (pop queue)))
                (apply (car job) (cdr job)))
              (pilish--browse-load-sessions
               'all (lambda (items error) (push (list items error) calls-b)))
              (while queue
                (let ((job (pop queue)))
                  (apply (car job) (cdr job)))))
            (should-not calls-a)
            (should (eq (length calls-b) 1))
            (pcase-let ((`(,items ,error) (car calls-b)))
              (should-not error)
              (should (= (length items) 60))
              (should (equal (sort (mapcar (lambda (i) (plist-get i :path)) items)
                                   #'string<)
                             (sort (copy-sequence paths) #'string<))))))))))

(ert-deftest pilish-test-load-sessions-error-as-string ()
  "Directory resolution failures surface as an error string, not a signal."
  (let ((calls nil))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (cl-letf (((symbol-function 'pilish--session-list-directory)
                 (lambda (&optional _chat-buf)
                   (signal 'file-error '("Cannot access sessions directory"))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args) (apply fn args))))
        (pilish--browse-load-sessions
         'current (lambda (items error) (push (list items error) calls))))
      (should (eq (length calls) 1))
      (pcase-let ((`(,items ,error) (car calls)))
        (should-not items)
        (should (stringp error))
        (should (string-match-p "Cannot list sessions" error))))))

(ert-deftest pilish-test-load-sessions-reentrant-directory-error-is-stale ()
  "A superseded synchronous directory error cannot finish the newer fetch.
Request A's directory resolver reentrantly starts request B and then
signals.  B owns the incremented generation and remains loading until
its queued scan completes; A must neither clear loading nor publish its
stale error."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'current
          pilish--session-browser-items-scope 'current
          pilish--session-browser-items
          (list '(:path "/old.jsonl" :name "Old"
                  :messageCount 1 :modified "2026-03-11T10:00:00Z")))
    (let ((first t)
          (queue nil))
      (cl-letf (((symbol-function 'pilish--browse-session-directories)
                 (lambda (_scope &optional _buf _token)
                   (if first
                       (progn
                         (setq first nil)
                         ;; Reentrant request B supersedes A while A is
                         ;; still inside synchronous directory resolution.
                         (pilish--session-browser-fetch-and-render)
                         (error "request A directory failure"))
                     nil)))
                ((symbol-function 'pilish--browse-session-files)
                 (lambda (_dirs &optional _buf _token) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest args)
                   (push (cons function args) queue))))
        (pilish--session-browser-fetch-and-render)
        (should (= pilish--session-browser-fetch-token 2))
        (should pilish--session-browser-loading)
        (should-not pilish--session-browser-error)
        (should (= (length queue) 1))
        ;; Complete request B's empty but successful scan.
        (let ((job (pop queue)))
          (apply (car job) (cdr job)))))
      (should-not pilish--session-browser-loading)
      (should-not pilish--session-browser-error)
      (should (eq pilish--session-browser-items-scope 'current))
      (should (string-match-p "(0 total)"
                              (pilish--session-browser-header-line)))))

(ert-deftest pilish-test-load-sessions-reentrant-directory-success-is-stale ()
  "A superseded successful directory lookup performs no stale file scan.
Request A's resolver starts B and then returns normally.  The generation
is checked again at that synchronous boundary, so only B lists files and
queues a scan continuation."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-scope 'current)
    (let ((first t)
          (file-list-calls nil)
          (queue nil))
      (cl-letf (((symbol-function 'pilish--browse-session-directories)
                 (lambda (_scope &optional _buf _token)
                   (if first
                       (progn
                         (setq first nil)
                         (pilish--session-browser-fetch-and-render)
                         '("/stale-a"))
                     nil)))
                ((symbol-function 'pilish--browse-session-files)
                 (lambda (dirs &optional _buf _token)
                   (push dirs file-list-calls)
                   nil))
                ((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest args)
                   (push (cons function args) queue))))
        (pilish--session-browser-fetch-and-render)
        (should (= pilish--session-browser-fetch-token 2))
        ;; B's nil directory list is the sole file-list call; A's
        ;; stale /stale-a result never crosses the next IO boundary.
        (should (equal file-list-calls '(nil)))
        (should (= (length queue) 1))
        (let ((job (pop queue)))
          (apply (car job) (cdr job))))
      (should-not pilish--session-browser-loading)
      (should-not pilish--session-browser-error))))

(ert-deftest pilish-test-load-sessions-cancels-between-directories ()
  "Cancellation during the first directory listing skips later directories.
The stale generation publishes no callback and schedules no timer."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (let ((listed nil)
          (callbacks nil)
          (timers nil))
      (cl-letf (((symbol-function 'pilish--browse-session-directories)
                 (lambda (_scope &optional _buf _token)
                   '("/scan/one" "/scan/two")))
                ((symbol-function 'directory-files)
                 (lambda (directory &rest _)
                   (push directory listed)
                   (when (equal directory "/scan/one")
                     (cl-incf pilish--session-browser-fetch-token))
                   (list (concat directory "/session.jsonl"))))
                ((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest args)
                   ;; Ignore unrelated editor timers; only a stale scan
                   ;; continuation would violate this boundary.
                   (when (eq function #'pilish--browse-scan-session-files)
                     (push (cons function args) timers)))))
        (pilish--browse-load-sessions
         'all (lambda (&rest args) (push args callbacks))))
      (should (equal listed '("/scan/one")))
      (should-not callbacks)
      (should-not timers))))

(ert-deftest pilish-test-load-sessions-cancels-within-first-file ()
  "Open/read/enrich cancellation stops the slice at its first file.
Each phase is callback-capable.  Once it supersedes the generation,
the current state is closed exactly once, the second file is untouched,
and no stale callback or continuation timer is emitted."
  (dolist (phase '(open read enrich))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-fetch-token 1)
      (let ((opened nil)
            (read nil)
            (enriched nil)
            (closed nil)
            (callbacks nil)
            (timers nil))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) 0.0))
                  ((symbol-function 'pilish-jsonl-open-session-info)
                   (lambda (file &optional _search)
                     (push file opened)
                     (when (eq phase 'open)
                       (cl-incf pilish--session-browser-fetch-token))
                     (list :file file)))
                  ((symbol-function 'pilish-jsonl-step-session-info)
                   (lambda (state &optional _deadline)
                     (push (plist-get state :file) read)
                     (when (eq phase 'read)
                       (cl-incf pilish--session-browser-fetch-token))
                     (cons 'done
                           (list :path (plist-get state :file)
                                 :name "First"))))
                  ((symbol-function 'pilish--session-enrich-item)
                   (lambda (item)
                     (push (plist-get item :path) enriched)
                     (when (eq phase 'enrich)
                       (cl-incf pilish--session-browser-fetch-token))
                     item))
                  ((symbol-function 'pilish-jsonl-close-session-info)
                   (lambda (state)
                     (when state
                       (push (plist-get state :file) closed))))
                  ((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest args)
                     (when (eq function #'pilish--browse-scan-session-files)
                       (push (cons function args) timers)))))
          (pilish--browse-scan-session-files
           (current-buffer) 1 '("/scan/one.jsonl" "/scan/two.jsonl") nil
           (lambda (&rest args) (push args callbacks))))
        (should (equal opened '("/scan/one.jsonl")))
        (pcase phase
          ('open
           (should-not read)
           (should-not enriched))
          ('read
           (should (equal read '("/scan/one.jsonl")))
           (should-not enriched))
          ('enrich
           (should (equal read '("/scan/one.jsonl")))
           (should (equal enriched '("/scan/one.jsonl")))))
        (should (equal closed '("/scan/one.jsonl")))
        (should-not callbacks)
        (should-not timers)))))

(ert-deftest pilish-test-flat-session-setup-failure-clears-loading ()
  "Project lookup failures finish a flat scan instead of leaving it loading."
  (dolist (failure '(quit error))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (cl-letf (((symbol-function 'pilish--browse-current-session-directory)
                 (lambda () "/flat/"))
                ((symbol-function 'pilish--browse-session-files) #'ignore)
                ((symbol-function 'pilish--browse-project-directory)
                 (lambda () (signal failure '("broken cwd")))))
        (condition-case nil
            (pilish--session-browser-fetch-and-render)
          ((error quit) nil)))
      (should-not pilish--session-browser-loading)
      (should (equal pilish--session-browser-error
                     (if (eq failure 'quit)
                         "Session scan was interrupted"
                       "Cannot list sessions: broken cwd"))))))

(ert-deftest pilish-test-flat-session-setup-cancellation ()
  "Cancellation during scan setup stops before the next IO boundary."
  (dolist (phase '(resolve classify project files))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (let (cancelled after-cancellation callbacks timers)
        (cl-labels ((observe (stage)
                      (when cancelled (push stage after-cancellation))
                      (when (eq stage phase)
                        (setq cancelled t)
                        (cl-incf pilish--session-browser-fetch-token))))
          (cl-letf (((symbol-function 'pilish--browse-session-directories)
                     (lambda (&rest _) (observe 'resolve) '("/flat/")))
                    ((symbol-function 'pilish--browse-flat-session-directory-p)
                     (lambda (_) (observe 'classify) t))
                    ((symbol-function 'pilish--browse-project-directory)
                     (lambda () "/work/"))
                    ((symbol-function 'pilish--session-canonical-project-spec)
                     (lambda (_) (observe 'project) '("/work" nil ("work"))))
                    ((symbol-function 'pilish--browse-session-files)
                     (lambda (&rest _) (observe 'files) '("/flat/a.jsonl")))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest args) (push args timers))))
            (pilish--browse-load-sessions
             'current (lambda (&rest args) (push args callbacks)))))
        (should cancelled)
        (should-not after-cancellation)
        (should-not callbacks)
        (should-not timers)))))

(ert-deftest pilish-test-session-directories-cancel-during-classification ()
  "All-projects classification cannot return stale paths or enter root IO."
  (dolist (flat '(nil t))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-fetch-token 1)
      (let (root-io)
        (cl-letf (((symbol-function 'pilish--browse-current-session-directory)
                   (lambda () "/flat/"))
                  ((symbol-function 'pilish--browse-flat-session-directory-p)
                   (lambda (_dir)
                     (cl-incf pilish--session-browser-fetch-token)
                     flat))
                  ((symbol-function 'pilish-jsonl-sessions-root)
                   (lambda (&rest _) (push t root-io) "/sessions/")))
          (should-not (pilish--browse-session-directories
                       'all (current-buffer) 1)))
        (should-not root-io)))))

(ert-deftest pilish-test-flat-session-prefilter-cancellation ()
  "A superseded header probe never opens the file or reports results."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-fetch-token 1)
    (let (opened callbacks timers)
      (cl-letf (((symbol-function 'pilish-jsonl-open-session-info)
                 (lambda (&rest args) (push args opened)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest args) (push args timers))))
        (pilish--browse-scan-session-files
         (current-buffer) 1 '("/scan/one.jsonl" "/scan/two.jsonl") nil
         (lambda (&rest args) (push args callbacks)) nil
         (lambda (_file)
           (cl-incf pilish--session-browser-fetch-token)
           t)))
      (should-not opened)
      (should-not callbacks)
      (should-not timers))))

(ert-deftest pilish-test-load-sessions-interrupted-by-quit ()
  "A quit during a scan slice reports an error state, not a stuck
loading render (session-side analog of
`pilish-test-load-tree-interrupted-by-quit').  C-g against a
slow scan raises `quit' — not `error' — inside the slice loop; the
seam must still call back exactly once so the loading state clears
and the browser names the interruption."
  (let* ((root (pilish-test--make-temp-directory "pi-scan-quit"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (path (expand-file-name "session.jsonl" dir))
         (calls nil))
    (make-directory dir t)
    (pilish-test--write-session-lines
     path (list (pilish-test--make-session-header "sid-quit")))
    (with-temp-buffer
      (pilish-session-browser-mode)
      ;; The fetch cycle reads the buffer-local scope; `all' sees the
      ;; munged directory below (`current' would munge the temp root
      ;; itself, which holds no sessions).
      (setq pilish--session-browser-scope 'all)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'pilish--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ((symbol-function 'pilish-jsonl-step-session-info)
                   (lambda (&rest _) (signal 'quit nil)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          ;; The seam reports the interruption exactly once.
          (pilish--browse-load-sessions
           'all (lambda (items error) (push (list items error) calls)))
          (should (eq (length calls) 1))
          (pcase-let ((`(,items ,error) (car calls)))
            (should-not items)
            (should (string-match-p "interrupted" error)))
          ;; The full fetch cycle clears the loading state and shows
          ;; the interruption instead of "Loading sessions...".
          (pilish--session-browser-fetch-and-render))
        (should-not pilish--session-browser-loading)
        (should (string-match-p "interrupted"
                                pilish--session-browser-error))
        (should (string-match-p "interrupted" (buffer-string)))))))

;;;; Full-message Search Regressions

(ert-deftest pilish-test-browse-search-late-messages-from-disk ()
  "The real browser and search command find later text on either disk branch."
  (let* ((root (pilish-test--make-temp-directory "pi-search-disk"))
         (default-directory root)
         (process-environment (copy-sequence process-environment))
         browser)
    (setenv "PI_CODING_AGENT_DIR" root)
    (let* ((dir (pilish-jsonl-session-dir-for-cwd root))
           (path (expand-file-name "session.jsonl" dir)))
      (make-directory dir t)
      (pilish-test--write-session-lines
       path
       (list (pilish-test--make-session-header "search-disk")
             (pilish-test--user-line "opening" nil "plain opening")
             (pilish-test--jsonl-line
              "message" "inactive" "opening"
              :message '(:role "assistant"
                         :content [(:type "text" :text "quasar269token 東京")]))
             (pilish-test--user-line "active" "opening" "nebula269token")
             (pilish-test--jsonl-line
              "session_info" "name" "active" :name "Search fixture")))
      (save-window-excursion
        (unwind-protect
            (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
              ;; Real discovery, reader, timers, and final Magit rendering.
              (pilish-session-browser)
              (setq browser (current-buffer))
              (let ((deadline (+ (float-time) pilish-test-rpc-timeout)))
                (while (and pilish--session-browser-loading
                            (< (float-time) deadline))
                  (sit-for 0.01)))
              (should-not pilish--session-browser-loading)
              (should-not pilish--session-browser-error)
              (should (equal (mapcar (lambda (i) (plist-get i :path))
                                    pilish--session-browser-items)
                             (list path)))
              (dolist (query '("Search" "plain" "quasar269token" "nebula269token"
                               "東京" "Search nebula269token"
                               "quasar269token.*nebula269token"))
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) query)))
                  (call-interactively #'pilish-session-browser-search))
                (ert-info ((format "Disk-backed search query: %S" query))
                  (should-not (string-match-p "No matching sessions" (buffer-string)))
                  (should (string-match-p "Search fixture" (buffer-string))))))
          (when (buffer-live-p browser) (kill-buffer browser)))))))

(ert-deftest pilish-test-session-search-prepared-corpus-semantics ()
  "Prepared text preserves regexp AND, case folding, boundaries and anchors."
  (let* ((text "Name first first Alpha\nBeta omega 東京")
         (prepared (list :name "Name" :firstMessage "first" :searchText text))
         (legacy '(:name "Name" :firstMessage "first"
                   :allMessagesText "first Alpha\nBeta omega 東京")))
    (dolist (case-fold-search '(t nil))
      (dolist (tokens '(nil ("Name") ("first.*Alpha") ("Beta.*omega")
                           ("Name" "東京") ("alpha") ("Alpha")
                           ("\\`Name") ("東京\\'") ("Alpha.*Beta")
                           ("missing") ("Name" "missing")))
        (should (eq (not (null (pilish--session-filter-search (list prepared) tokens)))
                    (not (null (pilish--session-filter-search (list legacy) tokens)))))))))

(ert-deftest pilish-test-session-search-prepared-corpus-not-rejoined ()
  "Searching a prepared item does not copy its corpus for each query."
  (let* ((item '(:name "Name" :firstMessage "first"
                 :searchText "Name first first late"))
         (original (symbol-function 'concat))
         (copies 0))
    (cl-letf (((symbol-function 'concat)
               (lambda (&rest strings)
                 (cl-incf copies)
                 (apply original strings))))
      (should (equal (pilish--session-filter-search (list item) '("late"))
                     (list item))))
    (should (= copies 0))))

(ert-deftest pilish-test-session-search-loading-error-do-not-filter-old-items ()
  "Loading and error screens don't search the previous full-text snapshot."
  (dolist (state '(loading error))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-items
            '((:name "Old session" :searchText "old full text"))
            pilish--session-browser-search-tokens '("full")
            pilish--session-browser-loading (eq state 'loading)
            pilish--session-browser-error (and (eq state 'error) "test failure"))
      (let ((filters 0)
            (original (symbol-function 'pilish--session-filter-search)))
        (cl-letf (((symbol-function 'pilish--session-filter-search)
                   (lambda (&rest args)
                     (cl-incf filters)
                     (apply original args))))
          (pilish--session-browser-render (current-buffer)))
        (should (string-match-p (if (eq state 'loading) "Loading sessions" "test failure")
                                (buffer-string)))
        (should (= filters 0))))))

(defmacro pilish-test--with-search-scan (&rest body)
  "Run BODY with a disk PATH, owner BROWSER, queued timers and scan tracking.
QUEUE contains (DELAY FUNCTION . ARGS); CALLS records final deliveries.
SCAN-BUFFERS tracks actual file-read buffers, not private scanner fields.
The controlled clock forces line-level yielding without elapsed-time assertions."
  (declare (indent 0) (debug t))
  `(let* ((dir (pilish-test--make-temp-directory "pi-search-slice"))
          (path (expand-file-name "session.jsonl" dir))
          (browser (generate-new-buffer " *pi-search-owner*"))
          (queue nil) (calls nil) (scan-buffers nil) (clock 0.0)
          (read-file (symbol-function 'insert-file-contents)))
     (pilish-test--write-session-lines
      path
      (append (list (pilish-test--make-session-header "sliced")
                    (pilish-test--user-line "first" nil "opening"))
              (cl-loop for n below 40 collect
                       (pilish-test--jsonl-line
                        "message" (format "a%d" n) "first"
                        :message (list :role "assistant" :content (format "text%d" n))))
              (list (pilish-test--user-line "last" "first" "finál 東京 🚀"))))
     (unwind-protect
         (with-current-buffer browser
           (pilish-session-browser-mode)
           (setq pilish--session-browser-fetch-token 1)
           (cl-letf (((symbol-function 'float-time)
                      (lambda (&optional _time) (cl-incf clock 0.002)))
                     ((symbol-function 'run-at-time)
                      (lambda (delay _repeat fn &rest args)
                        (setq queue (nconc queue (list (cons delay (cons fn args)))))))
                     ((symbol-function 'insert-file-contents)
                      (lambda (file &rest args)
                        (when (equal file path) (push (current-buffer) scan-buffers))
                        (apply read-file file args))))
             ,@body))
       (when (buffer-live-p browser) (kill-buffer browser))
       ;; Dispose queued work even when an assertion interrupted BODY.
       (dolist (job queue) (apply (cadr job) (cddr job)))
       (dolist (buffer scan-buffers)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest pilish-test-session-search-scan-yields-within-one-file ()
  "A single file stays incomplete across positive-delay continuations."
  (pilish-test--with-search-scan
    (pilish--browse-scan-session-files
     browser 1 (list path) nil (lambda (items error) (push (list items error) calls)))
    (should-not calls)
    (should (= (length queue) 1))
    (should (cl-some #'buffer-live-p scan-buffers))
    (let ((slices 0))
      (while queue
        (should (< (cl-incf slices) 200))
        (let ((job (pop queue)))
          (should (> (car job) 0))
          (apply (cadr job) (cddr job))))
      (should (> slices 1)))
    (should (= (length calls) 1))
    (should-not (cadar calls))
    (should (equal (mapcar (lambda (i) (plist-get i :path)) (caar calls)) (list path)))
    (should (pilish--session-filter-search (caar calls) '("text0.*text39" "finál" "東京" "🚀")))
    (should-not (cl-some #'buffer-live-p scan-buffers))))

(ert-deftest pilish-test-session-search-scan-stale-and-dead-cleanup ()
  "Superseded and dead owners drop in-flight same-file work and its buffer."
  (dolist (cancel '(supersede kill))
    (pilish-test--with-search-scan
      (pilish--browse-scan-session-files
       browser 1 (list path) nil (lambda (&rest args) (push args calls)))
      (should queue)
      (should-not calls)
      (should (cl-some #'buffer-live-p scan-buffers))
      (if (eq cancel 'supersede)
          (cl-incf pilish--session-browser-fetch-token)
        (kill-buffer browser))
      (let ((job (pop queue))) (apply (cadr job) (cddr job)))
      (should-not queue)
      (should-not calls)
      (should-not (cl-some #'buffer-live-p scan-buffers)))))

(ert-deftest pilish-test-session-search-scan-reentrant-invalidation ()
  "An owner invalidated inside file IO cannot receive even a final callback."
  (dolist (cancel '(supersede kill))
    (pilish-test--with-search-scan
      (let ((original (symbol-function 'insert-file-contents)))
        (cl-letf (((symbol-function 'insert-file-contents)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (if (eq cancel 'supersede)
                           (with-current-buffer browser
                             (cl-incf pilish--session-browser-fetch-token))
                         (kill-buffer browser))))))
          (pilish--browse-scan-session-files
           browser 1 (list path) nil (lambda (&rest args) (push args calls)))))
      (should-not calls)
      (should-not queue)
      (should-not (cl-some #'buffer-live-p scan-buffers)))))

(ert-deftest pilish-test-session-search-scan-callback-error-once-after-cleanup ()
  "A signaling consumer is called only once, after file resources are released."
  (pilish-test--with-search-scan
    (let ((callback (lambda (&rest args)
                      (push args calls)
                      (should-not (cl-some #'buffer-live-p scan-buffers))
                      (error "consumer failed"))))
      ;; No artificial deadline: isolate delivery from scheduling assertions.
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 0.0)))
        (should (equal
                 (should-error
                  (pilish--browse-scan-session-files browser 1 (list path) nil callback)
                  :type 'error)
                 '(error "consumer failed")))))
    (should (= (length calls) 1))
    (should (= (length (caar calls)) 1))
    (should-not (cadar calls))
    (should-not queue)))

(ert-deftest pilish-test-session-search-scan-late-error-skips-file ()
  "An ordinary late parse failure skips its file, not the remaining sessions."
  (pilish-test--with-search-scan
    (let ((good (expand-file-name "good.jsonl" dir))
          (original (symbol-function 'pilish--jsonl-parse-current-line)))
      (pilish-test--write-session-lines
       good (list (pilish-test--make-session-header "survivor")))
      (cl-letf (((symbol-function 'pilish--jsonl-parse-current-line)
                 (lambda ()
                   (when (looking-at-p ".*text5") (error "injected late failure"))
                   (funcall original))))
        (pilish--browse-scan-session-files
         browser 1 (list path (expand-file-name "missing.jsonl" dir) good) nil
         (lambda (&rest args) (push args calls)))
        (while queue
          (let ((job (pop queue))) (apply (cadr job) (cddr job)))))
      (should (= (length calls) 1))
      (should-not (cadar calls))
      (should (equal (mapcar (lambda (i) (plist-get i :id)) (caar calls))
                     '("survivor")))
      (should-not (cl-some #'buffer-live-p scan-buffers)))))

(ert-deftest pilish-test-session-search-scan-late-quit-cleans-up ()
  "Quit during later text extraction reports once rather than leaving loading."
  (pilish-test--with-search-scan
    (let ((original (symbol-function 'pilish--jsonl-parse-current-line)))
      (cl-letf (((symbol-function 'pilish--jsonl-parse-current-line)
                 (lambda ()
                   (when (looking-at-p ".*text5") (signal 'quit nil))
                   (funcall original))))
        (pilish--browse-scan-session-files
         browser 1 (list path) nil (lambda (&rest args) (push args calls)))
        (while queue
          (let ((job (pop queue))) (apply (cadr job) (cddr job)))))
      (should (equal calls '((nil "Session scan was interrupted"))))
      (should-not (cl-some #'buffer-live-p scan-buffers)))))

(ert-deftest pilish-test-session-search-scan-slice-error-cleans-up ()
  "A scan-level scheduling failure releases the retained file and reports once."
  (pilish-test--with-search-scan
    (pilish--browse-scan-session-files
     browser 1 (list path) nil (lambda (&rest args) (push args calls)))
    (should queue)
    (should-not calls)
    (should (cl-some #'buffer-live-p scan-buffers))
    ;; The next slice owns an already open file, including while it sets
    ;; its deadline.  Budget/timer errors must not bypass that ownership.
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _) (error "clock failed"))))
      (let ((job (pop queue))) (apply (cadr job) (cddr job))))
    (should (equal calls '((nil "Session scan failed: clock failed"))))
    (should-not queue)
    (should-not (cl-some #'buffer-live-p scan-buffers))))

(ert-deftest pilish-test-session-search-quit-window-keeps-inflight-scan ()
  "The q binding hides the browser without cancelling its in-flight scan."
  (pilish-test--with-search-scan
    (save-window-excursion
      (pop-to-buffer browser)
      (pilish--browse-scan-session-files
       browser 1 (list path) nil (lambda (&rest args) (push args calls)))
      (should queue)
      (call-interactively (key-binding (kbd "q")))
      (should (buffer-live-p browser))
      (should-not (get-buffer-window browser t))
      (should (= (buffer-local-value 'pilish--session-browser-fetch-token browser) 1))
      (while queue
        (let ((job (pop queue))) (apply (cadr job) (cddr job))))
      (should (= (length calls) 1))
      (should (pilish--session-filter-search (caar calls) '("finál")))
      (should-not (get-buffer-window browser t))
      (should-not (cl-some #'buffer-live-p scan-buffers)))))

(ert-deftest pilish-test-session-search-whitespace-only-query ()
  "A whitespace-only query is semantically empty.
The state, the header, and the render must agree: no active query,
and Threaded keeps its fork-family hierarchy."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          (list '(:path "/sess/parent.jsonl" :name "Whitespace Parent"
                  :messageCount 3 :modified "2026-01-02T00:00:00Z")
                '(:path "/sess/child.jsonl" :name "Whitespace Child"
                  :parentSessionPath "/sess/parent.jsonl"
                  :messageCount 1 :modified "2026-01-01T00:00:00Z")))
    (setq pilish--session-browser-view 'threaded)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
      (call-interactively #'pilish-session-browser-search))
    ;; State: the query is cleared, not recorded as active.
    (should-not pilish--session-browser-search-query)
    (should-not pilish--session-browser-search-tokens)
    ;; Render: the family hierarchy is intact and the header carries
    ;; no query segment.
    (should (string-match-p "└─" (buffer-string)))
    (should-not (string-match-p "/" (pilish--session-browser-header-line)))))

(ert-deftest pilish-test-session-search-invalid-and-cleared-query ()
  "Invalid regexps preserve the previous query; clearing restores all rows."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/fake/one" :name "One" :searchText "One first late")
            (:path "/fake/two" :name "Two" :searchText "Two other text")))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "One")))
      (call-interactively #'pilish-session-browser-search))
    (let ((before (buffer-string)) (messages nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "["))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (call-interactively #'pilish-session-browser-search))
      (should (= (length messages) 1))
      (should (string-match-p "Pi: Invalid regexp:" (car messages)))
      (should (equal pilish--session-browser-search-query "One"))
      (should (equal pilish--session-browser-search-tokens '("One")))
      (should (equal (buffer-string) before)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
      (call-interactively #'pilish-session-browser-search))
    (should-not pilish--session-browser-search-tokens)
    (should (string-match-p "One" (buffer-string)))
    (should (string-match-p "Two" (buffer-string)))))

;;;; Phase 2: Fetch Relaxation

(ert-deftest pilish-test-fetch-without-process ()
  "The session browser fetch proceeds without a live pi process.
Phase 2 reads sessions from disk, so the Phase 0 no-process guard is
gone for the session browser (the tree browser keeps it until Phase 3)."
  (let ((root (pilish-test--make-temp-directory "pi-noproc-root")))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'pilish--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (pilish--session-browser-fetch-and-render)))
      (should-not pilish--session-browser-loading)
      (should-not pilish--session-browser-error)
      (should (string-match-p "No sessions in this project"
                              (buffer-string))))))

;;;; Phase 2: Switch

(ert-deftest pilish-test-switch-calls-resume ()
  "--browse-switch-session guards, then delegates to the resume flow.
The busy guard runs first with (CHAT-BUF \"switch\"); the delegation
receives (PROC CHAT-BUF PATH) verbatim."
  (let* ((chat-buf (generate-new-buffer " *test-switch-chat*"))
         (proc (start-process "pi-switch-test" nil "sleep" "30"))
         (ready-calls nil)
         (resume-calls nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--process proc))
          (pilish-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'pilish--session-transition-ready-p)
                       (lambda (chat-buf action)
                         (push (list chat-buf action) ready-calls)
                         t))
                      ((symbol-function 'pilish--resume-selected-session)
                       (lambda (proc chat-buf path)
                         (push (list proc chat-buf path) resume-calls))))
              (pilish--browse-switch-session "/tmp/some-session.jsonl")))
          (should (equal ready-calls (list (list chat-buf "switch"))))
          (should (equal resume-calls
                         (list (list proc chat-buf "/tmp/some-session.jsonl")))))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-switch-busy-guard ()
  "A busy chat session blocks the switch before any resume attempt."
  (let* ((chat-buf (generate-new-buffer " *test-busy-chat*"))
         (proc (start-process "pi-busy-test" nil "sleep" "30"))
         (resume-calls nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--process proc))
          (pilish-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'pilish--session-transition-ready-p)
                       (lambda (&rest _) nil))
                      ((symbol-function 'pilish--resume-selected-session)
                       (lambda (&rest _) (push t resume-calls))))
              ;; Returns quietly: the guard reports the reason itself.
              (pilish--browse-switch-session "/tmp/some-session.jsonl")))
          (should-not resume-calls))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-switch-active-transition-guard ()
  "An in-flight session transition blocks a second switch before any
resume attempt.  The transition latch keeps the status idle, so
`--session-transition-ready-p' cannot see it (same gate navigate
already has); without the explicit `--session-transition-active-p'
check, RET on two rows would start two racing switch_session
transitions."
  (let* ((chat-buf (generate-new-buffer " *test-switch-active-chat*"))
         (proc (start-process "pi-switch-active-test" nil "sleep" "30"))
         (ready-calls nil)
         (resume-calls nil)
         (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--process proc
                  ;; Mid-transition: the latch is set but the status is
                  ;; still idle, so the ready guard alone would pass.
                  pilish--session-transition-active t))
          (pilish-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'pilish--session-transition-ready-p)
                       (lambda (chat-buf action)
                         (push (list chat-buf action) ready-calls)
                         t))
                      ((symbol-function 'pilish--resume-selected-session)
                       (lambda (&rest _) (push t resume-calls)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (pilish--browse-switch-session "/tmp/some-session.jsonl")))
          (should (member "Pi: Cannot switch while switching sessions"
                          messages))
          ;; The active gate fires before the ready guard runs at all.
          (should-not ready-calls)
          (should-not resume-calls))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-switch-no-session ()
  "Switching with no linked chat session signals a `user-error'."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (should (equal (error-message-string
                    (should-error
                     (pilish--browse-switch-session "/test/a.jsonl")
                     :type 'user-error))
                   "No pi session to switch to"))))

(ert-deftest pilish-test-quit-when-settled ()
  "--browse-quit-when-settled waits out the transition, then quits only
when the chat landed on the requested session file AND the window still
shows a session browser.  Timers run synchronously; the transition looks
busy once, then settles.  A repurposed window, a dead chat buffer, and a
landed-elsewhere state all leave the window alone."
  (let* ((chat-buf (generate-new-buffer " *test-settled-chat*"))
         (win (selected-window))
         (orig-buf (window-buffer win))
         (browser-buf (generate-new-buffer " *test-settled-browser*"))
         (other-buf (generate-new-buffer " *test-settled-other*"))
         (path "/tmp/target-session.jsonl"))
    (unwind-protect
        (let ((quit-calls nil) (polls 0))
          (with-current-buffer browser-buf
            (pilish-session-browser-mode))
          (set-window-buffer win browser-buf)
          ;; Settled onto the target: one busy poll, then quit-window.
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (cl-letf (((symbol-function 'pilish--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pilish--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should (equal quit-calls (list (list nil win))))
          ;; Settled elsewhere: the browser stays open.
          (setq quit-calls nil polls 0)
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file "/tmp/other.jsonl")))
          (cl-letf (((symbol-function 'pilish--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pilish--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should-not quit-calls)
          ;; Window repurposed mid-poll (browse buffer killed): landing on
          ;; the target must NOT quit whatever the window shows now.
          (setq quit-calls nil polls 0)
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (set-window-buffer win other-buf)
          (cl-letf (((symbol-function 'pilish--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pilish--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should-not quit-calls)
          ;; Dead chat buffer: the poll ends quietly, no signal, no quit.
          (setq quit-calls nil)
          (set-window-buffer win browser-buf)
          (kill-buffer chat-buf)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pilish--browse-quit-when-settled chat-buf win path))
          (should-not quit-calls))
      (set-window-buffer win orig-buf)
      (kill-buffer browser-buf)
      (kill-buffer other-buf)
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf)))))

;;;; Session Delete

(defun pilish-test--session-command-at-point
    (item command &optional items prepare)
  "Run COMMAND at ITEM's rendered session-browser section.
ITEMS, when non-nil, is the browser's complete loaded snapshot.
PREPARE runs in the browser before rendering, for setting scope or
filters without bypassing the observed command path."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items (or items (list item)))
    (when prepare (funcall prepare))
    (pilish--session-browser-rerender)
    (goto-char (point-min))
    (search-forward (pilish--session-display-name item))
    (backward-char)
    (funcall command)))

(ert-deftest pilish-test-session-delete-trash-context-and-side-effects ()
  "Trash confirmation names the session/project and performs that operation."
  (let* ((path (make-temp-file "pilish-delete-session-" nil ".jsonl"))
         (filename (file-name-nondirectory path))
         (item (list :path path :cwd "/work/acme"
                     :name "Disposable session"
                     :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (real-delete (symbol-function 'delete-file))
         (delete-calls nil)
         (refreshes 0)
         (prompt nil)
         (messages nil))
    (unwind-protect
        (let ((delete-by-moving-to-trash t))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (text) (setq prompt text) t))
                    ((symbol-function 'delete-file)
                     (lambda (file &optional trash)
                       (push (list file trash) delete-calls)
                       ;; Keep the behavioral side effect deterministic;
                       ;; the argument above is the contract under test.
                       (funcall real-delete file nil)))
                    ((symbol-function 'pilish--session-browser-fetch-and-render)
                     (lambda () (setq refreshes (1+ refreshes))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (pilish-test--session-command-at-point
             item #'pilish-session-browser-delete))
          (should (string-prefix-p "Move session file to trash" prompt))
          (should (string-match-p "Other Emacs.*external processes.*not checked"
                                  prompt))
          (should (string-match-p "Disposable session" prompt))
          (should (string-match-p (regexp-quote "project \"acme\"")
                                  prompt))
          (should-not (string-match-p (regexp-quote path) prompt))
          (should-not (string-match-p (regexp-quote filename) prompt))
          (should-not (string-match-p "not deleted\\|become roots"
                                      prompt))
          (should (equal delete-calls (list (list path t))))
          (should-not (file-exists-p path))
          (should (= refreshes 1))
          (should (seq-some (lambda (text)
                              (and (string-match-p "Disposable session" text)
                                   (string-match-p "trash" (downcase text))))
                            messages)))
      (when (file-exists-p path)
        (funcall real-delete path nil)))))

(ert-deftest pilish-test-session-delete-permanent-context-and-side-effects ()
  "Permanent confirmation and delete call agree in All-projects scope."
  (let* ((path (make-temp-file "pilish-delete-permanent-" nil ".jsonl"))
         (item (list :path path :cwd "/clients/widgets"
                     :name "Old investigation"
                     :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (real-delete (symbol-function 'delete-file))
         (delete-calls nil)
         (refreshes 0)
         (prompt nil)
         (messages nil))
    (unwind-protect
        (let ((delete-by-moving-to-trash nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (text) (setq prompt text) t))
                    ((symbol-function 'delete-file)
                     (lambda (file &optional trash)
                       (push (list file trash) delete-calls)
                       (funcall real-delete file nil)))
                    ((symbol-function 'pilish--session-browser-fetch-and-render)
                     (lambda () (setq refreshes (1+ refreshes))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (pilish-test--session-command-at-point
             item #'pilish-session-browser-delete nil
             (lambda () (setq pilish--session-browser-scope 'all))))
          (should (string-prefix-p "Permanently delete session file" prompt))
          (should (string-match-p "Old investigation" prompt))
          (should (string-match-p (regexp-quote "project \"widgets\"")
                                  prompt))
          (should-not (string-match-p "trash" (downcase prompt)))
          (should-not (string-match-p (regexp-quote path) prompt))
          (should (equal delete-calls (list (list path nil))))
          (should-not (file-exists-p path))
          (should (= refreshes 1))
          (should (seq-some
                   (lambda (text)
                     (and (string-match-p "Old investigation" text)
                          (string-match-p "permanently" (downcase text))))
                   messages)))
      (when (file-exists-p path)
        (funcall real-delete path nil)))))

(ert-deftest pilish-test-session-delete-context-safe-for-unnamed-malformed-metadata ()
  "Confirmation reuses display fallbacks and never prints unsafe metadata."
  (let* ((path-a (make-temp-file "pilish-delete-unnamed-" nil ".jsonl"))
         (path-b (make-temp-file "pilish-delete-malformed-" nil ".jsonl"))
         (bidi (string #x202e))
         (item-a (list :path path-a
                       :cwd (concat "/work/" bidi "forged")
                       :name 7
                       :firstMessage (concat "Repair" bidi " plan\nnow")
                       :messageCount 1
                       :modified "2026-03-02T10:00:00Z"))
         (item-b (list :path path-b :cwd 42 :name '(not a string)
                       :firstMessage ["not" "a" "string"]
                       :messageCount 0
                       :modified "2026-03-02T10:00:00Z"))
         prompt-a prompt-b)
    (unwind-protect
        (let ((delete-by-moving-to-trash nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (text) (setq prompt-a text) nil)))
            (pilish-test--session-command-at-point
             item-a #'pilish-session-browser-delete nil
             (lambda () (setq pilish--session-browser-scope 'all))))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (text) (setq prompt-b text) nil)))
            (pilish-test--session-command-at-point
             item-b #'pilish-session-browser-delete nil
             (lambda () (setq pilish--session-browser-scope 'current))))
          ;; The first-message fallback remains recognizable, but its
          ;; newline and bidi control cannot forge the confirmation.
          (should (string-match-p "Repair" prompt-a))
          (should (string-match-p "plan now" prompt-a))
          (should-not (string-match-p bidi prompt-a))
          (should-not (string-match-p "forged" prompt-a))
          (should-not (string-match-p "\n" prompt-a))
          (should (string-match-p "unknown" (downcase prompt-a)))
          ;; Fully malformed name/first-message metadata uses the same
          ;; human fallback the row uses, in either scope.
          (should (string-match-p (regexp-quote "[empty session]") prompt-b))
          (should (string-match-p "unknown" (downcase prompt-b)))
          (should-not (string-match-p (regexp-quote path-b) prompt-b)))
      (delete-file path-a nil)
      (delete-file path-b nil))))

(ert-deftest pilish-test-session-delete-prompt-bounds-untrusted-fields ()
  "Policy and project lead a bounded prompt despite hostile long metadata."
  (let* ((bidi (string #x202e))
         (parent-path "/tmp/pilish-delete-long-parent.jsonl")
         (parent
          (list :path parent-path
                :cwd (concat "/work/" (make-string 200 ?界))
                :name (concat "Lead" bidi "\n"
                              (make-string 400 ?界)
                              (make-string 600 ?A) "TAIL")
                :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (child-a
          (list :path "/tmp/pilish-delete-long-child-a.jsonl"
                :cwd "/work/long" :parentSessionPath parent-path
                :name (concat "Child-A-" (make-string 500 ?B))
                :messageCount 1 :modified "2026-03-02T11:00:00Z"))
         (child-b
          (list :path "/tmp/pilish-delete-long-child-b.jsonl"
                :cwd "/work/long" :parentSessionPath parent-path
                :firstMessage (concat "子供" bidi (make-string 300 ?界))
                :messageCount 1 :modified "2026-03-02T12:00:00Z"))
         (child-c
          (list :path "/tmp/pilish-delete-long-child-c.jsonl"
                :cwd "/work/long" :parentSessionPath parent-path
                :name (concat "Child-C-" (make-string 500 ?C))
                :messageCount 1 :modified "2026-03-02T13:00:00Z"))
         (prompt nil)
         (delete-calls nil))
    (let ((delete-by-moving-to-trash nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (text) (setq prompt text) nil))
                ((symbol-function 'delete-file)
                 (lambda (&rest args) (push args delete-calls))))
        (pilish-test--session-command-at-point
         parent #'pilish-session-browser-delete
         (list parent child-a child-b child-c)
         (lambda () (setq pilish--session-browser-scope 'all)))))
    (should (string-prefix-p "Permanently delete session file" prompt))
    (let* ((project-pos (string-match "project" prompt))
           (project
            (and project-pos
                 (car (read-from-string
                       (substring prompt
                                  (+ project-pos (length "project "))))))))
      (should project-pos)
      (should (< project-pos 50))
      (should (stringp project))
      ;; The generated collision-safe token must not silently crop its
      ;; user-controlled project tail before the prompt bounds it.
      (should (string-match-p "…" project))
      (should (<= (string-width project)
                  (- pilish--session-delete-project-width 2))))
    (should (string-match-p "Lead" prompt))
    (should (string-match-p "Child-A" prompt))
    (should (string-match-p "子供" prompt))
    (should (string-match-p "Child-C" prompt))
    (should (string-match-p "3 direct child sessions" prompt))
    (should (string-match-p "not deleted" prompt))
    (should (string-match-p "become roots" prompt))
    (should (string-match-p "…" prompt))
    (should-not (string-match-p bidi prompt))
    (should-not (string-match-p "\n" prompt))
    (should-not (string-match-p "TAIL" prompt))
    (should-not (string-match-p (make-string 80 ?A) prompt))
    (should (<= (string-width prompt)
                pilish--session-delete-prompt-max-width))
    (should-not delete-calls)))

(ert-deftest pilish-test-session-delete-warns-direct-children-not-descendants ()
  "Deleting an archive symlink keeps its target and warns canonical children."
  (let* ((base (pilish-test--make-temp-directory "pilish-delete-family-"))
         (outside (pilish-test--make-temp-directory
                   "pilish-delete-family-target-"))
         (target (expand-file-name "external-parent.jsonl" outside))
         (parent-link (expand-file-name "parent.jsonl" base))
         (child-a (expand-file-name "child-a.jsonl" base))
         (child-b (expand-file-name "child-b.jsonl" base))
         (grandchild (expand-file-name "grandchild.jsonl" base))
         (parent-item (list :path parent-link :cwd "/work/family"
                            :name "Family parent" :messageCount 1
                            :modified "2026-03-02T10:00:00Z"))
         (items (list parent-item
                      (list :path child-a :cwd "/work/family"
                            :name "Alias child"
                            :parentSessionPath parent-link :messageCount 1
                            :modified "2026-03-02T11:00:00Z")
                      ;; The target spelling and archive-link spelling are
                      ;; one family identity, but only the selected raw link
                      ;; is the destructive action path.
                      (list :path child-b :cwd "/work/family"
                            :name "Direct child"
                            :parentSessionPath target :messageCount 1
                            :modified "2026-03-02T12:00:00Z")
                      (list :path grandchild :cwd "/work/family"
                            :name "Nested grandchild"
                            :parentSessionPath child-a :messageCount 1
                            :modified "2026-03-02T13:00:00Z")))
         (real-delete (symbol-function 'delete-file))
         (delete-calls nil)
         (refreshes 0)
         (prompt nil))
    (unwind-protect
        (progn
          (write-region "external session" nil target nil 'silent)
          (dolist (path (list child-a child-b grandchild))
            (write-region "" nil path nil 'silent))
          (make-symbolic-link target parent-link)
          (let ((delete-by-moving-to-trash nil))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (text) (setq prompt text) t))
                      ((symbol-function 'delete-file)
                       (lambda (file &optional trash)
                         (push (list file trash) delete-calls)
                         (funcall real-delete file nil)))
                      ((symbol-function 'pilish--session-browser-fetch-and-render)
                       (lambda () (cl-incf refreshes)))
                      ((symbol-function 'message) #'ignore))
              (pilish-test--session-command-at-point
               parent-item #'pilish-session-browser-delete items)))
          (should (string-match-p "2 direct child sessions" prompt))
          (should (string-match-p "Alias child" prompt))
          (should (string-match-p "Direct child" prompt))
          (should-not (string-match-p "Nested grandchild" prompt))
          (should (string-match-p "not deleted" prompt))
          (should (string-match-p "become roots" prompt))
          (should (equal delete-calls (list (list parent-link nil))))
          (should (= refreshes 1))
          (should-not (file-symlink-p parent-link))
          (should (file-exists-p target))
          (should (equal (pilish-test--file-contents target)
                         "external session"))
          (dolist (path (list child-a child-b grandchild))
            (should (file-exists-p path))))
      (when (file-directory-p base)
        (delete-directory base t))
      (when (file-directory-p outside)
        (delete-directory outside t)))))

(ert-deftest pilish-test-session-delete-retained-alias-target-keeps-family ()
  "An archived alias target can remain the parent after its link is deleted."
  (let* ((base (pilish-test--make-temp-directory
                "pilish-delete-retained-alias-"))
         (target (expand-file-name "z-parent-target.jsonl" base))
         (link (expand-file-name "a-parent-link.jsonl" base))
         (child (expand-file-name "child.jsonl" base))
         (parent-item (list :path link :cwd "/work/retained"
                            :name "Retained alias" :messageCount 1
                            :modified "2026-03-02T10:00:00Z"))
         (child-item (list :path child :cwd "/work/retained"
                           :name "Retained child"
                           :parentSessionPath target :messageCount 1
                           :modified "2026-03-02T11:00:00Z"))
         (target-item (list :path target :cwd "/work/retained"
                            :name "Canonical parent" :messageCount 1
                            :modified "2026-03-02T10:00:00Z"))
         (real-delete (symbol-function 'delete-file))
         (prompt nil))
    (unwind-protect
        (progn
          (write-region "parent" nil target nil 'silent)
          (write-region "child" nil child nil 'silent)
          (make-symbolic-link target link)
          (let ((delete-by-moving-to-trash nil))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (text) (setq prompt text) t))
                      ((symbol-function 'delete-file)
                       (lambda (file &optional _trash)
                         (funcall real-delete file nil)))
                      ((symbol-function
                        'pilish--session-browser-fetch-and-render)
                       #'ignore)
                      ((symbol-function 'message) #'ignore))
              (pilish-test--session-command-at-point
               parent-item #'pilish-session-browser-delete
               (list parent-item child-item))))
          (should-not (file-symlink-p link))
          (should (file-exists-p target))
          (should (file-exists-p child))
          (should (string-match-p
                   "become roots if their parent leaves the archive"
                   prompt))
          ;; A subsequent archive snapshot containing the retained target
          ;; still attaches the child instead of rendering it as a root.
          (let* ((threaded
                  (pilish--session-thread-items
                   (list target-item child-item)))
                 (child-row
                  (cl-find child threaded
                           :key (lambda (entry)
                                  (plist-get (car entry) :path))
                           :test #'equal)))
            (should child-row)
            (should-not (string-empty-p (cadr child-row)))))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-delete-more-than-three-skips-child-names ()
  "A count-only warning never formats names that it will omit."
  (let* ((parent-path "/tmp/pilish-delete-many-parent.jsonl")
         (parent (list :path parent-path :canonicalPath parent-path
                       :cwd "/work/many" :name "Many-child parent"
                       :messageCount 1
                       :modified "2026-03-02T10:00:00Z"))
         (children
          (cl-loop for n from 1 to 4
                   for path = (format "/tmp/pilish-delete-many-%d.jsonl" n)
                   collect (list :path path :canonicalPath path
                                 :canonicalParentSession parent-path
                                 :cwd "/work/many"
                                 :name (format "Omitted child %d" n)
                                 :messageCount 1
                                 :modified "2026-03-02T11:00:00Z")))
         (items (cons parent children))
         (real-display (symbol-function 'pilish--session-display-name))
         (prompt nil))
    (with-temp-buffer
      (pilish-session-browser-mode)
      (setq pilish--session-browser-items items)
      (pilish--session-browser-rerender)
      (goto-char (point-min))
      (search-forward "Many-child parent")
      (backward-char)
      (cl-letf (((symbol-function 'pilish--session-display-name)
                 (lambda (item)
                   (if (memq item children)
                       (ert-fail "count-only warning formatted a child name")
                     (funcall real-display item))))
                ((symbol-function 'y-or-n-p)
                 (lambda (text) (setq prompt text) nil)))
        (pilish-session-browser-delete)))
    (should (string-match-p "4 direct child sessions" prompt))
    (should-not (string-match-p "Omitted child" prompt))))

(ert-deftest pilish-test-session-delete-filter-does-not-hide-child-warning ()
  "Child warning uses the full snapshot, not the visible query result."
  (let* ((parent (make-temp-file "pilish-delete-filter-parent-" nil ".jsonl"))
         (child (make-temp-file "pilish-delete-filter-child-" nil ".jsonl"))
         (parent-item (list :path parent :cwd "/work/filter"
                            :name "Only target" :messageCount 1
                            :modified "2026-03-02T10:00:00Z"))
         (child-item (list :path child :cwd "/work/filter"
                           :firstMessage "Hidden fork"
                           :parentSessionPath parent :messageCount 1
                           :modified "2026-03-02T11:00:00Z"))
         (prompt nil)
         (delete-calls nil))
    (unwind-protect
        (let ((delete-by-moving-to-trash t))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (text) (setq prompt text) nil))
                    ((symbol-function 'delete-file)
                     (lambda (&rest args) (push args delete-calls))))
            (pilish-test--session-command-at-point
             parent-item #'pilish-session-browser-delete
             (list parent-item child-item)
             (lambda ()
               (setq pilish--session-browser-named-only t
                     pilish--session-browser-search-query "Only target"
                     pilish--session-browser-search-tokens
                     '("Only" "target")))))
          (should (string-match-p "1 direct child session" prompt))
          (should (string-match-p "Hidden fork" prompt))
          (should (string-match-p "not deleted" prompt))
          (should (string-match-p "become roots" prompt))
          (should-not delete-calls)
          (should (file-exists-p parent))
          (should (file-exists-p child)))
      (delete-file parent nil)
      (delete-file child nil))))

(ert-deftest pilish-test-session-delete-project-context-distinguishes-routes ()
  "All-projects confirmations reuse collision-safe project tokens.
Two routes with the same host and cwd label must remain distinguishable,
and an invisible cwd component must not enter the prompt verbatim."
  (let* ((cgj (string #x034f))
         (alice (list :path "/ssh:alice@host:/sessions/a.jsonl"
                      :cwd "/ssh:alice@host:/work/app"
                      :name "Alice session" :messageCount 1
                      :modified "2026-03-02T10:00:00Z"))
         (bob (list :path "/ssh:bob@host:/sessions/b.jsonl"
                    :cwd "/ssh:bob@host:/work/app"
                    :name "Bob session" :messageCount 1
                    :modified "2026-03-02T11:00:00Z"))
         (invisible (list :path "/sessions/invisible.jsonl"
                          :cwd (concat "/work/a" cgj "pp")
                          :name "Invisible project" :messageCount 1
                          :modified "2026-03-02T12:00:00Z"))
         (items (list alice bob invisible))
         ;; Exercise contextual formatting without contacting fixture hosts.
         (alice-prompt (pilish--session-delete-prompt alice items nil nil))
         (bob-prompt (pilish--session-delete-prompt bob items nil nil))
         (invisible-prompt
          (pilish--session-delete-prompt invisible items nil nil)))
    (let ((context
           (lambda (prompt)
             (when (string-match "in project " prompt)
               (condition-case nil
                   (car (read-from-string
                         (substring prompt (match-end 0))))
                 (error nil))))))
      (let ((alice-project (funcall context alice-prompt))
            (bob-project (funcall context bob-prompt))
            (invisible-project (funcall context invisible-prompt)))
        (should alice-project)
        (should bob-project)
        (should invisible-project)
        (should-not (equal alice-project bob-project))
        (should (string-prefix-p "#" alice-project))
        (should (string-prefix-p "#" bob-project))
        (should (string-prefix-p "#" invisible-project))
        (should-not (string-match-p cgj invisible-prompt))))))

(ert-deftest pilish-test-session-delete-cancelled ()
  "Declining deletion leaves the session file and browser untouched."
  (let* ((path (make-temp-file "pilish-keep-session-" nil ".jsonl"))
         (item (list :path path :cwd "/work/keep" :name "Keep this session"
                     :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (delete-calls nil)
         (refreshes 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
                    ((symbol-function 'delete-file)
                     (lambda (&rest args) (push args delete-calls)))
                    ((symbol-function 'pilish--session-browser-fetch-and-render)
                     (lambda () (setq refreshes (1+ refreshes)))))
            (pilish-test--session-command-at-point
             item #'pilish-session-browser-delete))
          (should (file-exists-p path))
          (should-not delete-calls)
          (should (= refreshes 0)))
      (delete-file path nil))))

(ert-deftest pilish-test-session-delete-error-keeps-file-and-browser ()
  "A failed delete neither refreshes nor removes the selected browser row."
  (let* ((path (make-temp-file "pilish-delete-error-" nil ".jsonl"))
         (item (list :path path :cwd "/work/errors" :name "Keep on error"
                     :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (refreshes 0))
    (unwind-protect
        (with-temp-buffer
          (pilish-session-browser-mode)
          (setq pilish--session-browser-items (list item))
          (pilish--session-browser-rerender)
          (goto-char (point-min))
          (search-forward "Keep on error")
          (let ((before (buffer-string))
                (delete-by-moving-to-trash nil))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                      ((symbol-function 'delete-file)
                       (lambda (&rest _)
                         (signal 'file-error '("delete refused"))))
                      ((symbol-function 'pilish--session-browser-fetch-and-render)
                       (lambda () (setq refreshes (1+ refreshes)))))
              (should-error (pilish-session-browser-delete)
                            :type 'file-error))
            (should (equal (buffer-string) before))
            (should (string-match-p "Keep on error" (buffer-string)))
            (should (equal pilish--session-browser-items (list item)))
            (should (= refreshes 0))
            (should (file-exists-p path))))
      (delete-file path nil))))

(ert-deftest pilish-test-session-delete-refuses-live-session ()
  "Canonical live identity blocks deletion of its selected symlink alias."
  (let* ((base (pilish-test--make-temp-directory "pilish-delete-live-"))
         (outside (pilish-test--make-temp-directory
                   "pilish-delete-live-target-"))
         (target (expand-file-name "live.jsonl" outside))
         (link (expand-file-name "live-link.jsonl" base))
         (item (list :path link :name "Open elsewhere"
                     :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (chat-buf (generate-new-buffer "*pilish-test-delete-live-chat*"))
         (proc (start-process "pilish-delete-live-test" nil "sleep" "30"))
         (prompted nil)
         (delete-calls nil)
         (refreshes 0))
    (write-region "live target" nil target nil 'silent)
    (make-symbolic-link target link)
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'pilish-chat-buffer chat-buf)
    (with-current-buffer chat-buf
      (setq pilish--process proc
            pilish--state (list :session-file target)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt)
                       (setq prompted t)
                       t))
                    ((symbol-function 'delete-file)
                     (lambda (&rest args) (push args delete-calls)))
                    ((symbol-function 'pilish--session-browser-fetch-and-render)
                     (lambda () (setq refreshes (1+ refreshes)))))
            (should
             (equal
              (error-message-string
               (should-error
                (pilish-test--session-command-at-point
                 item #'pilish-session-browser-delete)
                :type 'user-error))
              (format "Session is open in %s — close it first"
                      (buffer-name chat-buf)))))
          (should-not prompted)
          (should-not delete-calls)
          (should (file-symlink-p link))
          (should (file-exists-p target))
          (should (= refreshes 0)))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf)
      (when (file-directory-p base)
        (delete-directory base t))
      (when (file-directory-p outside)
        (delete-directory outside t)))))

(ert-deftest pilish-test-session-delete-rechecks-live-after-confirmation ()
  "A session becoming live in the prompt race is not deleted."
  (let* ((path (make-temp-file "pilish-delete-race-" nil ".jsonl"))
         (item (list :path path :cwd "/work/race" :name "Race target"
                     :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (chat-buf (generate-new-buffer "*pilish-test-delete-race-chat*"))
         (proc (start-process "pilish-delete-race-test" nil "sleep" "30"))
         (prompted 0)
         (delete-calls nil)
         (refreshes 0))
    (set-process-query-on-exit-flag proc nil)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt)
                       (cl-incf prompted)
                       ;; It was closed at the first guard and becomes a
                       ;; live Pilish session while the user decides.
                       (process-put proc 'pilish-chat-buffer chat-buf)
                       (with-current-buffer chat-buf
                         (setq pilish--process proc
                               pilish--state (list :session-file path)))
                       t))
                    ((symbol-function 'delete-file)
                     (lambda (&rest args) (push args delete-calls)))
                    ((symbol-function 'pilish--session-browser-fetch-and-render)
                     (lambda () (setq refreshes (1+ refreshes)))))
            (should-error
             (pilish-test--session-command-at-point
              item #'pilish-session-browser-delete)
             :type 'user-error))
          (should (= prompted 1))
          (should-not delete-calls)
          (should (= refreshes 0))
          (should (file-exists-p path)))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf)
      (delete-file path nil))))

(ert-deftest pilish-test-session-delete-rejects-observed-canonical-retarget ()
  "The final observation rejects a symlink retargeted during confirmation."
  (let* ((base (pilish-test--make-temp-directory
                "pilish-delete-canonical-retarget-"))
         (target-a (expand-file-name "original.jsonl" base))
         (target-b (expand-file-name "retargeted.jsonl" base))
         (link (expand-file-name "selected.jsonl" base))
         (item (list :path link :cwd "/work/retarget"
                     :name "Canonical retarget"
                     :messageCount 1 :modified "2026-03-02T10:00:00Z"))
         (real-delete (symbol-function 'delete-file))
         (delete-calls nil)
         (refreshes 0))
    (write-region "original" nil target-a nil 'silent)
    (write-region "retargeted" nil target-b nil 'silent)
    (make-symbolic-link target-a link)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt)
                       ;; Make the final canonical observation differ from
                       ;; the one captured before confirmation.
                       (funcall real-delete link nil)
                       (make-symbolic-link target-b link)
                       t))
                    ((symbol-function 'delete-file)
                     (lambda (&rest args) (push args delete-calls)))
                    ((symbol-function
                      'pilish--session-browser-fetch-and-render)
                     (lambda () (cl-incf refreshes))))
            (should
             (equal
              (error-message-string
               (should-error
                (pilish-test--session-command-at-point
                 item #'pilish-session-browser-delete)
                :type 'user-error))
              "Selected session changed while awaiting confirmation")))
          (should-not delete-calls)
          (should (= refreshes 0))
          (should (file-symlink-p link))
          (should (equal (file-truename link) (file-truename target-b)))
          (should (equal (pilish-test--file-contents target-a) "original"))
          (should (equal (pilish-test--file-contents target-b) "retargeted")))
      (when (file-directory-p base)
        (delete-directory base t)))))

(ert-deftest pilish-test-session-delete-rejects-changed-file ()
  "Confirmation does not authorize a changed or removed session file."
  (dolist (change '(mtime size replacement missing))
    (let* ((path (make-temp-file "pilish-delete-changed-" nil ".jsonl"))
           (item (list :path path :name "Changed session"))
           (time (seconds-to-time 100000))
           (refreshes 0))
      (unwind-protect
          (progn
            (write-region "original" nil path nil 'silent)
            (set-file-times path time)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_prompt)
                         (pcase change
                           ('mtime (set-file-times
                                    path (seconds-to-time 100001)))
                           ('size
                            (write-region " appended" nil path 'append 'silent)
                            (set-file-times path time))
                           ('replacement
                            (let ((replacement (make-temp-file
                                                "pilish-delete-replacement-")))
                              (write-region "original" nil replacement nil 'silent)
                              (set-file-times replacement time)
                              (rename-file replacement path t)))
                           ('missing (delete-file path nil)))
                         t))
                      ((symbol-function 'pilish--session-browser-fetch-and-render)
                       (lambda () (cl-incf refreshes))))
              (should-error
               (pilish-test--session-command-at-point
                item #'pilish-session-browser-delete)
               :type 'user-error))
            (should (= refreshes 0))
            (if (eq change 'missing)
                (should-not (file-exists-p path))
              (should (equal (pilish-test--file-contents path)
                             (if (eq change 'size)
                                 "original appended"
                               "original")))))
        (when (file-exists-p path) (delete-file path nil))))))

(ert-deftest pilish-test-session-delete-rejects-changed-symlink-target ()
  "A write through the selected alias vetoes deleting that alias."
  (let* ((directory (pilish-test--make-temp-directory "pilish-delete-alias-"))
         (target (expand-file-name "target.jsonl" directory))
         (link (expand-file-name "alias.jsonl" directory))
         (item (list :path link :name "Changing alias"))
         (refreshes 0))
    (unwind-protect
        (progn
          (write-region "original" nil target nil 'silent)
          (make-symbolic-link target link)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt)
                       (write-region " appended" nil target 'append 'silent)
                       t))
                    ((symbol-function 'pilish--session-browser-fetch-and-render)
                     (lambda () (cl-incf refreshes))))
            (should-error
             (pilish-test--session-command-at-point
              item #'pilish-session-browser-delete)
             :type 'user-error))
          (should (file-symlink-p link))
          (should (equal (pilish-test--file-contents target)
                         "original appended"))
          (should (= refreshes 0)))
      (delete-directory directory t))))

(ert-deftest pilish-test-session-delete-rejects-cached-remote-change ()
  "Confirmation checks fresh TRAMP metadata, not a cached pre-prompt stat."
  (require 'tramp-sh)
  (let* ((backing (make-temp-file "pilish-delete-remote-" nil ".jsonl"))
         (path "/ssh:pilish-test@example.invalid:/sessions/file.jsonl")
         (item (list :path path :name "Remote session"))
         (tramp-cache-data (make-hash-table :test #'equal))
         (tramp-verbose 0)
         ;; Keep cache entries valid indefinitely so this test has no deadline.
         (remote-file-name-inhibit-cache nil)
         (real-delete (symbol-function 'delete-file))
         (refreshes 0))
    (unwind-protect
        (progn
          (write-region "original" nil backing nil 'silent)
          ;; Attribute conversion and caching stay real.  The remote stat
          ;; transport and destructive dispatch use the backing fixture.
          (cl-letf (((symbol-function 'tramp-get-remote-stat)
                     (lambda (_vec) "stat"))
                    ((symbol-function 'tramp-get-remote-gid)
                     (lambda (_vec _format) 1000))
                    ((symbol-function 'tramp-get-device)
                     (lambda (_vec) 1))
                    ((symbol-function 'tramp-do-file-attributes-with-stat)
                     (lambda (_vec _localname)
                       (let ((attributes (file-attributes backing)))
                         (setf (nth 2 attributes) '("test" . 1000)
                               (nth 3 attributes) '("test" . 1000))
                         attributes)))
                    ((symbol-function 'delete-file)
                     (lambda (selected &optional trash)
                       (should (equal selected path))
                       (funcall real-delete backing trash)))
                    ((symbol-function 'y-or-n-p)
                     (lambda (_prompt)
                       (write-region " appended" nil backing 'append 'silent)
                       t))
                    ((symbol-function 'pilish--session-browser-fetch-and-render)
                     (lambda () (cl-incf refreshes)))
                    ((symbol-function 'message) #'ignore))
            (should-error
             (pilish-test--session-command-at-point
              item #'pilish-session-browser-delete)
             :type 'user-error))
          (should (equal (pilish-test--file-contents backing)
                         "original appended"))
          (should (= refreshes 0)))
      (when (file-exists-p backing) (funcall real-delete backing nil)))))

(ert-deftest pilish-test-session-delete-ignores-access-time ()
  "Reading the file during confirmation is not a content change."
  (let* ((path (make-temp-file "pilish-delete-read-" nil ".jsonl"))
         (item (list :path path :name "Read session"))
         (refreshes 0))
    (unwind-protect
        (progn
          (write-region "original" nil path nil 'silent)
          (set-file-times path (seconds-to-time 100000))
          (let ((delete-by-moving-to-trash nil))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_prompt)
                         (should (equal (pilish-test--file-contents path)
                                        "original"))
                         t))
                      ((symbol-function 'pilish--session-browser-fetch-and-render)
                       (lambda () (cl-incf refreshes)))
                      ((symbol-function 'message) #'ignore))
              (pilish-test--session-command-at-point
               item #'pilish-session-browser-delete)))
          (should-not (file-exists-p path))
          (should (= refreshes 1)))
      (when (file-exists-p path) (delete-file path nil)))))

(ert-deftest pilish-test-session-delete-ignores-non-session-section ()
  "Delete on a grouping header does not treat its value as a file path."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/tmp/not-used.jsonl" :name "Grouped session"
             :messageCount 1 :modified "2026-03-02T10:00:00Z"))
          pilish--session-browser-view 'recent)
    (pilish--session-browser-rerender)
    (goto-char (point-min))
    (let ((prompted nil)
          (deleted nil)
          (messages nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'delete-file)
                 (lambda (&rest args) (push args deleted)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (pilish-session-browser-delete))
      (should-not prompted)
      (should-not deleted)
      (should (member "Pi: No session at point" messages)))))

;;;; Phase 2: Rename

(defun pilish-test--rename-at-point (item chat-buf input)
  "Run `session-browser-rename' with INPUT at ITEM's section.
ITEM is a session plist (its :name locates the section); CHAT-BUF is
the browse buffer's chat link.  The post-rename refresh is stubbed
out; callers mock the rename seams they assert on."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--chat-buffer chat-buf
          pilish--session-browser-items (list item))
    (pilish--session-browser-rerender)
    (goto-char (point-min))
    (search-forward (plist-get item :name))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) input))
              ((symbol-function 'pilish--session-browser-fetch-and-render)
               #'ignore))
      (pilish-session-browser-rename))))

(ert-deftest pilish-test-rename-other-session-appends ()
  "Renaming a non-current session appends exactly one session_info line.
The line carries a fresh 8-hex id, parents to the id of the file's last
line, a UTC ISO timestamp no older than the rename, and the cleaned
name.  A missing trailing newline gets a separator; prior bytes stay
byte-for-byte intact."
  (let* ((dir (pilish-test--make-temp-directory "pi-rename-append"))
         (path (expand-file-name "target.jsonl" dir))
         (current-path (expand-file-name "current.jsonl" dir))
         (before-lines
          (list (pilish-test--make-session-header "sid-target")
                (pilish-test--user-line "m1" nil "investigate the flaky test")
                (pilish-test--jsonl-line
                 "message" "m2" "m1"
                 :message '(:role "assistant" :content "found it"))
                (pilish-test--jsonl-line
                 "session_info" "s1" "m2" :name "Old name")))
         (chat-buf (generate-new-buffer " *test-rename-chat*"))
         (start-iso (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t)))
    ;; No trailing newline: the append must add the separator itself.
    (pilish-test--write-session-lines path before-lines t)
    (pilish-test--write-session-lines
     current-path (list (pilish-test--make-session-header "sid-current")))
    (unwind-protect
        (let ((before (pilish-test--file-contents path)))
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file current-path)))
          (pilish-test--rename-at-point
           (list :path path :name "Old name" :messageCount 2
                 :modified "2026-03-02T10:00:00Z")
           chat-buf
           "  Renamed\nSession  ")
          (let* ((after (pilish-test--file-contents path)))
            ;; Exactly one line was appended, after a separator.
            (should-not (equal after before))
            (let* ((appended (car (split-string (substring after (length before))
                                                 "\n" t)))
                   (entry (json-parse-string appended :object-type 'plist)))
              (should (string-prefix-p before after))
              (should (string-suffix-p (concat appended "\n") after))
              (should (equal (plist-get entry :type) "session_info"))
              (should (string-match-p "\\`[0-9a-f]\\{8\\}\\'"
                                     (plist-get entry :id)))
              (should (not (member (plist-get entry :id)
                                   '("m1" "m2" "s1"))))
              ;; parentId is the id of the last line before the append.
              (should (equal (plist-get entry :parentId) "s1"))
              ;; The name is trimmed with newlines collapsed.
              (should (equal (plist-get entry :name) "Renamed Session"))
              (let ((ts (plist-get entry :timestamp)))
                (should (string-match-p
                         "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}Z\\'"
                         ts))
                (should (not (string< ts start-iso)))))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-rename-append-garbage-tail ()
  "Renaming a session with a garbage final line parents past the garbage.
pi's loader skips malformed lines, so the append's :parentId must be the
id of the last PARSEABLE line — parenting to the garbage (or to nil)
would detach the whole conversation from the reload context.  The
garbage bytes stay byte-for-byte intact."
  (let* ((dir (pilish-test--make-temp-directory "pi-rename-garbage"))
         (path (expand-file-name "torn.jsonl" dir))
         (garbage "{\"type\":\"message\",\"id\":\"torn\",\"paren")
         (chat-buf (generate-new-buffer " *test-garbage-chat*")))
    (pilish-test--write-session-lines
     path
     (list (pilish-test--make-session-header "sid-g")
           (pilish-test--user-line "g1" nil "check the flaky test")
           (pilish-test--jsonl-line
            "message" "g2" "g1"
            :message '(:role "assistant" :content "fixed"))
           garbage))
    (unwind-protect
        (let ((before (pilish-test--file-contents path)))
          (with-current-buffer chat-buf
            (setq pilish--state
                  (list :session-file "/tmp/somewhere-else.jsonl")))
          (pilish-test--rename-at-point
           (list :path path :name "Torn tail session" :messageCount 2
                 :modified "2026-03-02T10:00:00Z")
           chat-buf "Fixed name")
          (let* ((after (pilish-test--file-contents path))
                 (appended (car (split-string (substring after (length before))
                                              "\n" t)))
                 (entry (json-parse-string appended :object-type 'plist))
                 (state (pilish--browse-session-file-state path)))
            (should (string-prefix-p before after))
            ;; The collision set sees every id in the file, torn line or not.
            (dolist (id '("sid-g" "g1" "g2" "torn"))
              (should (gethash id (plist-get state :ids))))
            (should-not (gethash "no-such-id" (plist-get state :ids)))
            ;; Parent is the last parseable line, not the torn one.
            (should (equal (plist-get entry :parentId) "g2"))
            ;; Fresh id: collides with nothing already in the file.
            (should (not (member (plist-get entry :id)
                                 '("sid-g" "g1" "g2" "torn"))))
            (should (equal (plist-get entry :name) "Fixed name"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-rename-append-unreadable-file ()
  "Renaming a session whose file vanished cancels with a message.
No line is appended, no file is created, and the browser is not
refreshed (the fetch-and-render seam stays silent)."
  (let* ((dir (pilish-test--make-temp-directory "pi-rename-missing"))
         (path (expand-file-name "ghost.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-missing-chat*"))
         (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state
                  (list :session-file "/tmp/somewhere-else.jsonl")))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (pilish-test--rename-at-point
             (list :path path :name "Ghost session" :messageCount 0
                   :modified "2026-03-02T10:00:00Z")
             chat-buf "Ghost name"))
          (should-not (file-exists-p path))
          (should (cl-some (lambda (m) (string-match-p "unreadable" m))
                           messages)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-rename-dispatch ()
  "Rename routes by current-vs-other session and cancels on empty input.
Current: `set-session-name' RPC only, no file append.  Other: file
append only, no RPC.  Empty (whitespace) input cancels with a message:
no RPC, no append (no clearing in Phase 2)."
  (let* ((dir (pilish-test--make-temp-directory "pi-rename-dispatch"))
         (path (expand-file-name "target.jsonl" dir))
         (current-path (expand-file-name "current.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-dispatch-chat*"))
         (set-name-calls nil)
         (messages nil))
    (pilish-test--write-session-lines
     path
     (list (pilish-test--make-session-header "sid-target")
           (pilish-test--user-line "m1" nil "other session")
           (pilish-test--jsonl-line
            "session_info" "s1" "m1" :name "Target name")))
    (pilish-test--write-session-lines
     current-path (list (pilish-test--make-session-header "sid-current")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file current-path)))
          ;; Current session: RPC rename, no append to any file.
          (let ((target-before (pilish-test--file-contents path))
                (current-before (pilish-test--file-contents current-path)))
            (cl-letf (((symbol-function 'pilish-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (pilish-test--rename-at-point
               (list :path current-path :name "Current session" :messageCount 0
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "  New\nName  "))
            (should (equal set-name-calls '(("New Name"))))
            (should (equal (pilish-test--file-contents path)
                           target-before))
            (should (equal (pilish-test--file-contents current-path)
                           current-before)))
          ;; Other session: append, no RPC.
          (setq set-name-calls nil)
          (let ((before (pilish-test--file-contents path)))
            (cl-letf (((symbol-function 'pilish-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (pilish-test--rename-at-point
               (list :path path :name "Target name" :messageCount 1
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "Fresh name"))
            (should-not set-name-calls)
            (should-not (equal (pilish-test--file-contents path) before))
            (should (string-match-p "Fresh name"
                                    (pilish-test--file-contents path))))
          ;; Empty input: cancelled for both paths; message only.
          (setq set-name-calls nil)
          (let ((target-before (pilish-test--file-contents path))
                (current-before (pilish-test--file-contents current-path)))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages)))
                      ((symbol-function 'pilish-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (pilish-test--rename-at-point
               (list :path path :name "Target name" :messageCount 1
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "  \n "))
            (should-not set-name-calls)
            (should (equal (pilish-test--file-contents path)
                           target-before))
            (should (equal (pilish-test--file-contents current-path)
                           current-before))
            (should (member "Pi: Rename cancelled" messages))))
      (kill-buffer chat-buf))))

;;;; Phase 3: Tree Browser Live (disk-based) + Labels

(defmacro pilish-test--with-tree-link (chat-buf &rest body)
  "Run BODY in a tree-browser buffer linked to CHAT-BUF."
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (pilish-tree-browser-mode)
     (setq pilish--chat-buffer ,chat-buf)
     ,@body))

(defun pilish-test--live-session-lines (&optional with-label)
  "Return raw session lines for a realistic small session.
Header, a user prompt, an assistant grep tool round-trip, and a final
assistant text turn.  WITH-LABEL appends one label line targeting the
user message, so the raw leaf is a filtered label entry and the
projected leaf resolves up to the last visible entry."
  (append
   (list (pilish-test--make-session-header "sid-live")
         (pilish-test--user-line "m1" nil "fix the parser")
         (pilish-test--jsonl-line
          "message" "m2" "m1"
          :message '(:role "assistant"
                     :content [(:type "text" :text "checking the Makefile")
                               (:type "toolCall" :id "tc1" :name "grep"
                                      :arguments (:pattern "ldflags"
                                                  :path "/srv/demo/Makefile"))]
                     :stopReason "tool_calls"))
         (pilish-test--jsonl-line
          "message" "m3" "m2"
          :message '(:role "toolResult" :toolCallId "tc1" :toolName "grep"
                     :output [(:type "text" :text "Makefile:14: LDFLAGS")]))
         (pilish-test--jsonl-line
          "message" "m4" "m3"
          :message '(:role "assistant" :content "done"
                             :stopReason "end_turn")))
   (when with-label
     (list (pilish-test--jsonl-line
            "label" "l1" "m4" :targetId "m1" :label "checkpoint")))))

(defun pilish-test--tree-find-node (tree id)
  "Find the projected node with :id ID in TREE; nil when absent.
Traversal is iterative."
  (let ((stack (append tree nil))
        (found nil))
    (while (and stack (not found))
      (let ((node (pop stack)))
        (if (equal (plist-get node :id) id)
            (setq found node)
          (setq stack (append (append (plist-get node :children) nil)
                              stack)))))
    found))

(defun pilish-test--margin-overlay-contains-p (text)
  "Return non-nil when a right-margin overlay shows TEXT in this buffer."
  (cl-some
   (lambda (o)
     (let* ((bs (overlay-get o 'before-string))
            (display (and bs (get-text-property 0 'display bs))))
       (and display (string-match-p (regexp-quote text) (cadr display)))))
   (overlays-in (point-min) (point-max))))

(defmacro pilish-test--sync-timers (body)
  "Run BODY with `run-at-time' shimmed to run its job synchronously."
  (declare (indent 0) (debug (lambda)))
  `(cl-letf (((symbol-function 'run-at-time)
              (lambda (_secs _repeat fn &rest args)
                (apply fn args))))
     (funcall ,body)))

(ert-deftest pilish-test-tree-browser-mode-reinit-generation-and-owner ()
  "A mode reset invalidates an older tree fetch across file owners.
A queues a real deferred read for file A.  After the reset, B claims
file B; A's timer runs first and must not read or publish, then B alone
installs its tree.  The generations must differ even though the file
owner check independently rejects A."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-reinit-"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-reinit-chat*"))
         (queue nil)
         (reads nil)
         generation-a
         generation-after-reset
         generation-b)
    (pilish-test--write-session-lines
     path-a
     (list (pilish-test--make-session-header "sid-tree-reinit-a")
           (pilish-test--user-line "a-root" nil "STALE TREE A")))
    (pilish-test--write-session-lines
     path-b
     (list (pilish-test--make-session-header "sid-tree-reinit-b")
           (pilish-test--user-line "b-root" nil "FRESH TREE B")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path-a)))
          (with-temp-buffer
            (pilish-tree-browser-mode)
            (setq pilish--chat-buffer chat-buf)
            (let ((project-file
                   (symbol-function 'pilish-jsonl-project-session-file)))
              (cl-letf (((symbol-function 'redisplay) #'ignore)
                        ((symbol-function 'run-at-time)
                         (lambda (_seconds _repeat function &rest args)
                           ;; The deferred tree read is a closure; ignore
                           ;; unrelated symbolic editor maintenance timers.
                           (unless (symbolp function)
                             (push (cons function args) queue))))
                        ((symbol-function 'pilish-jsonl-project-session-file)
                         (lambda (path)
                           (push path reads)
                           (funcall project-file path))))
                (pilish--tree-browser-fetch-and-render)
                (setq generation-a pilish--tree-browser-fetch-token)
                (should (= (length queue) 1))

                (pilish-tree-browser-mode)
                (setq generation-after-reset
                      pilish--tree-browser-fetch-token)
                ;; Major-mode initialization clears ordinary linkage;
                ;; the browser entry point likewise restores it before
                ;; starting a post-reset fetch.
                (setq pilish--chat-buffer chat-buf)
                (with-current-buffer chat-buf
                  (setq pilish--state (list :session-file path-b)))
                (pilish--tree-browser-fetch-and-render)
                (setq generation-b pilish--tree-browser-fetch-token)
                (should (= (length queue) 2))

                ;; B was pushed last.  Give A the first opportunity to
                ;; publish after B has claimed generation and owner.
                (let* ((job-b (pop queue))
                       (job-a (pop queue)))
                  (apply (car job-a) (cdr job-a))
                  (should pilish--tree-browser-loading)
                  (should-not pilish--tree-browser-loaded-file)
                  (should-not reads)
                  (apply (car job-b) (cdr job-b)))

                (should-not pilish--tree-browser-loading)
                (should-not pilish--tree-browser-error)
                (should (equal reads (list path-b)))
                (should (equal pilish--tree-browser-state-file path-b))
                (should (equal pilish--tree-browser-loaded-file path-b))
                (should (equal pilish--tree-browser-leaf-id "b-root"))
                (should (string-match-p "FRESH TREE B" (buffer-string)))
                (should-not (string-match-p "STALE TREE A"
                                            (buffer-string)))
                (should (> generation-after-reset generation-a))
                (should (> generation-b generation-after-reset))))))
      (kill-buffer chat-buf)
      (delete-directory dir t))))

(ert-deftest pilish-test-load-tree-reads-and-projects-session-file ()
  "--browse-load-tree reads and projects the linked chat's session file.
The seam callback receives (TREE LEAF-ID MESSAGE): TREE and LEAF-ID are
`equal' to the direct jsonl pipeline (read-file, build-tree,
project-tree) over the same file, and MESSAGE is nil on success.  The
label line folds onto its target and the raw leaf (the label entry)
resolves up to the last visible entry.  The fetch cycle records the
freshly resolved file in `--tree-browser-loaded-file' and renders it."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-live"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-live-chat*"))
         (calls nil))
    (pilish-test--write-session-lines
     path (pilish-test--live-session-lines t))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            ;; Direct seam call: the deferred read delivers synchronously
            ;; under the timer shim.
            (pilish-test--sync-timers
              (lambda ()
                (pilish--browse-load-tree
                 (lambda (tree leaf-id message)
                   (push (list tree leaf-id message) calls)))))
            (should (equal (length calls) 1))
            (pcase-let ((`(,tree ,leaf-id ,message) (car calls)))
              (should-not message)
              (let* ((session (pilish-jsonl-read-file path))
                     (built (pilish-jsonl-build-tree
                             (plist-get session :entries)))
                     (expected (pilish-jsonl-project-tree
                                (plist-get built :tree)
                                (plist-get built :leafId))))
                (should (equal tree (plist-get expected :tree)))
                (should (equal leaf-id (plist-get expected :leafId))))
              ;; The label folds onto its target; the raw leaf is the
              ;; label entry, whose projected leaf resolves up to m4.
              (should (equal (plist-get
                              (pilish-test--tree-find-node tree "m1")
                              :label)
                             "checkpoint"))
              (should (equal leaf-id "m4")))
            ;; Fetch cycle: the loaded file is recorded and the tree
            ;; renders in the browser buffer.
            (pilish-test--sync-timers
              (lambda ()
                (pilish--tree-browser-fetch-and-render)))
            (should (equal pilish--tree-browser-loaded-file path))
            (should-not pilish--tree-browser-loading)
            (should-not pilish--tree-browser-error)
            (should (string-match-p "fix the parser" (buffer-string)))
            ;; Raw leaf l1 is projected away; fresh point follows the
            ;; resolved projected leaf m4 rather than the first row.
            (should (equal (oref (magit-current-section) value) "m4"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-browser-ambiguous-duplicate-shows-diagnostic ()
  "Differing duplicate ids keep safe rows and show an honest warning.
The later `dup' occurrence is the one canonical display row, but gets
a non-string section identity and RET refuses it locally.  `safe'
remains addressable, and the ambiguous raw leaf produces no @/* claim.
Loading succeeds, so the loaded-file guard remains armed."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-ambiguous"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-ambiguous-chat*")))
    (pilish-test--write-session-lines
     path
     (list (pilish-test--make-session-header "sid-ambiguous")
           (pilish-test--user-line "dup" nil "first ambiguous row")
           (pilish-test--jsonl-line
            "message" "safe" nil
            :message '(:role "assistant" :content "safe unique row"
                       :stopReason "end_turn"))
           (pilish-test--user-line "dup" nil "later canonical row")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should-not pilish--tree-browser-error)
            (should (string-match-p
                     "dup" pilish--tree-browser-diagnostic))
            (should (equal pilish--tree-browser-loaded-file path))
            (should (string-match-p "duplicate entry id" (buffer-string)))
            (should (string-match-p "safe unique row" (buffer-string)))
            (should (string-match-p "later canonical row" (buffer-string)))
            (should-not (string-match-p "first ambiguous row"
                                        (buffer-string)))
            (should (= pilish--tree-browser-visible-count 2))
            (should (= (how-many "^@ " (point-min) (point-max)) 0))
            (should (= (how-many "^\\* " (point-min) (point-max)) 0))
            (let ((sections
                   (car (pilish--tree-rendered-section-index))))
              (should (= (hash-table-count sections) 1))
              (should (gethash "safe" sections))
              (should-not (gethash "dup" sections)))
            (goto-char (point-min))
            (search-forward "later canonical row")
            (beginning-of-line)
            (should (equal (oref (magit-current-section) value)
                           '(ambiguous-id . "dup")))
            (let (navigated refusal)
              (cl-letf (((symbol-function 'pilish--browse-navigate)
                         (lambda (&rest _args) (setq navigated t)))
                        ((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq refusal
                                 (apply #'format format-string args)))))
                (pilish-tree-browser-navigate))
              (should-not navigated)
              (should (string-match-p
                       "ambiguous duplicate entry id: dup" refusal)))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-load-tree-no-chat-link ()
  "A tree fetch with no linked chat renders the link error state.
The message names the pi chat session, the fetch renders it as text
with a zero visible count, and nothing is treated as loaded.  The
entry point guards the same condition with a `user-error' before any
browser buffer is created."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (pilish-test--sync-timers
      (lambda () (pilish--tree-browser-fetch-and-render)))
    (should (string-match-p "No linked pi chat session" (buffer-string)))
    (should-not pilish--tree-browser-loading)
    (should (string-match-p "chat" pilish--tree-browser-error))
    (should (= pilish--tree-browser-visible-count 0))
    (should-not pilish--tree-browser-loaded-file))
  ;; Entry point: the guard fires before any buffer is created.
  (let* ((created nil)
         (guard-buf (generate-new-buffer " *pi-test-tree-guard*")))
    (unwind-protect
        (cl-letf (((symbol-function 'pilish--get-chat-buffer)
                   (lambda () nil))
                  ((symbol-function 'pilish--get-or-create-tree-browser)
                   (lambda (&rest _)
                     (setq created t)
                     guard-buf))
                  ((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'pilish--browse-apply-margins)
                   #'ignore)
                  ((symbol-function
                    'pilish--tree-browser-fetch-and-render)
                   #'ignore))
          (should (equal (error-message-string
                          (should-error (pilish-tree-browser)
                                        :type 'user-error))
                         "No pi session to browse"))
          (should-not created))
      (kill-buffer guard-buf))))

(ert-deftest pilish-test-load-tree-no-session-file-yet ()
  "A live chat link whose session file does not exist yet renders the
not-yet state — both a state without :session-file and one naming a
nonexistent path.  No signal in either case; the visible count is zero."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-nofile"))
         (chat-buf (generate-new-buffer " *test-tree-nofile-chat*")))
    (unwind-protect
        (pilish-test--with-tree-link chat-buf
          ;; State without a :session-file key.
          (with-current-buffer chat-buf
            (setq pilish--state (list :messageCount 0)))
          (pilish-test--sync-timers
            (lambda () (pilish--tree-browser-fetch-and-render)))
          (should (string-match-p "No session file yet" (buffer-string)))
          (should (string-match-p "No session file yet"
                                  pilish--tree-browser-error))
          (should (= pilish--tree-browser-visible-count 0))
          (should-not pilish--tree-browser-loaded-file)
          ;; State naming a path that does not exist yet: the file is
          ;; only created on the first assistant reply.
          (with-current-buffer chat-buf
            (setq pilish--state
                  (list :session-file (expand-file-name "pending.jsonl" dir))))
          (pilish-test--sync-timers
            (lambda () (pilish--tree-browser-fetch-and-render)))
          (should (string-match-p "No session file yet" (buffer-string)))
          (should (string-match-p "first assistant"
                                  pilish--tree-browser-error)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-load-tree-unreadable-session-file ()
  "An existing session file that does not parse as a pi session renders
the unreadable state naming the file — garbage and headerless files
both read as nil, which is an error message, never a signal."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-garbage"))
         (garbage (expand-file-name "garbage.jsonl" dir))
         (headerless (expand-file-name "headerless.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-garbage-chat*")))
    (with-temp-file garbage
      (insert "this is not json\n{\"type\":\"message\",\"id\":\"g1\"}\n"))
    (pilish-test--write-session-lines
     headerless (list (pilish-test--user-line "g1" nil "decoy")))
    (unwind-protect
        (pilish-test--with-tree-link chat-buf
          (dolist (path (list garbage headerless))
            (with-current-buffer chat-buf
              (setq pilish--state (list :session-file path)))
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (string-match-p "unreadable" (buffer-string)))
            (should (string-match-p
                     (format "Session file is unreadable or not a pi session file: %s"
                             (regexp-quote path))
                     pilish--tree-browser-error))
            (should (= pilish--tree-browser-visible-count 0))
            (should-not pilish--tree-browser-loaded-file)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-load-tree-deferred-and-tokened ()
  "--browse-load-tree defers the disk read and honors the fetch token.
The read is queued through `run-at-time'; a superseding fetch bumps
the buffer's fetch token so the older timer drops itself without
calling back, and the newer one reports exactly once with the
projected tree and no error."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-defer"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-defer-chat*")))
    (pilish-test--write-session-lines
     path (pilish-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            ;; Deferred timers: A is queued, B supersedes it before any
            ;; timer runs; running the queue drops A and reports B once.
            (let ((calls-a nil) (calls-b nil) (queue nil))
              (cl-letf (((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (push (cons fn args) queue))))
                (pilish--browse-load-tree
                 (lambda (tree leaf-id message)
                   (push (list tree leaf-id message) calls-a)))
                (pilish--browse-load-tree
                 (lambda (tree leaf-id message)
                   (push (list tree leaf-id message) calls-b)))
                (while queue
                  (let ((job (pop queue)))
                    (apply (car job) (cdr job)))))
              (should-not calls-a)
              (should (equal (length calls-b) 1))
              (pcase-let ((`(,tree ,leaf-id ,message) (car calls-b)))
                (should-not message)
                (should (equal leaf-id "m4"))
                (should (equal (plist-get
                                (pilish-test--tree-find-node tree "m1")
                                :preview)
                               "fix the parser"))))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-browser-fetch-claims-generation-before-render ()
  "A loading render cannot reverse reentrant tree-fetch ownership.
Fetch A claims its generation and file owner before painting.  Its
loading rerender starts fetch B; after A resumes, only B may queue and
publish.  In particular, A must never pair its tree with B's
`pilish--tree-browser-state-file'."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-reentrant"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-reentrant-chat*")))
    (pilish-test--write-session-lines
     path-a
     (list (pilish-test--make-session-header "sid-reentrant-a")
           (pilish-test--user-line "a-root" nil "tree owned by A")))
    (pilish-test--write-session-lines
     path-b
     (list (pilish-test--make-session-header "sid-reentrant-b")
           (pilish-test--user-line "b-root" nil "tree owned by B")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path-a)))
          (pilish-test--with-tree-link chat-buf
            (let ((first-render t)
                  (queue nil))
              (cl-letf (((symbol-function 'pilish--tree-browser-rerender)
                         (lambda (&rest _)
                           (when first-render
                             (setq first-render nil)
                             (with-current-buffer chat-buf
                               (setq pilish--state
                                     (list :session-file path-b)))
                             (pilish--tree-browser-fetch-and-render))))
                        ((symbol-function 'redisplay) #'ignore)
                        ((symbol-function 'run-at-time)
                         (lambda (_seconds _repeat function &rest args)
                           (push (cons function args) queue))))
                (pilish--tree-browser-fetch-and-render)
                ;; Run in scheduling order: B was queued while A's
                ;; loading render was still on the stack.
                (dolist (job (nreverse queue))
                  (apply (car job) (cdr job)))))
            (should (= pilish--tree-browser-fetch-token 2))
            (should (equal pilish--tree-browser-state-file path-b))
            (should (equal pilish--tree-browser-loaded-file path-b))
            (should (equal pilish--tree-browser-leaf-id "b-root"))
            (should (equal (plist-get
                            (aref pilish--tree-browser-tree 0)
                            :preview)
                           "tree owned by B"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-browser-load-render-is-generation-fenced ()
  "A newer load landing inside real section insertion wins atomically.
A's second tree-node visibility hook publishes generation 2/B after
A's root row was already inserted and while generation 1 remains on
the stack.  When that hook returns, A must not resume or perform stale
orientation writes: the clean repaint and all ownership state are B."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((buf (current-buffer))
           (owner-a "owner-a")
           (owner-b "owner-b")
           (tree-a
            [(:id "a-root" :type "message" :role "user"
              :preview "A root" :children
              [(:id "a-leaf" :parentId "a-root" :type "message"
                :role "assistant" :preview "A leaf" :children [])])])
           (tree-b
            [(:id "b-root" :type "message" :role "user"
              :preview "B root" :children
              [(:id "b-leaf" :parentId "b-root" :type "message"
                :role "assistant" :preview "B leaf" :children [])])])
           (tree-node-hooks 0)
           (armed t)
           (magit-section-set-visibility-hook
            (list
             (lambda (section)
               (when (eq (oref section type) 'tree-node)
                 (setq tree-node-hooks (1+ tree-node-hooks))
                 (when (and armed (= tree-node-hooks 2))
                   (setq armed nil
                         pilish--tree-browser-fetch-token 2
                         pilish--tree-browser-state-file owner-b)
                   (pilish--tree-browser-apply-load
                    buf tree-b "b-leaf" nil nil nil owner-b 2)))
               nil))))
      (setq pilish--tree-browser-fetch-token 1
            pilish--tree-browser-state-file owner-a)
      (pilish--tree-browser-apply-load
       buf tree-a "a-leaf" nil nil nil owner-a 1)
      (should-not armed)
      (should (= pilish--tree-browser-fetch-token 2))
      (should (equal pilish--tree-browser-state-file owner-b))
      (should (equal pilish--tree-browser-loaded-file owner-b))
      (should (equal pilish--tree-browser-leaf-id "b-leaf"))
      (should pilish--tree-browser-point-oriented-p)
      (should (equal (oref (magit-current-section) value) "b-leaf"))
      (should (equal (pilish--tree-anchor-node-id
                      pilish--tree-browser-point-anchor)
                     "b-leaf"))
      (should (equal pilish--tree-browser-point-lineage
                     '("b-leaf" "b-root")))
      (should (string-match-p "B root" (buffer-string)))
      (should (string-match-p "B leaf" (buffer-string)))
      (should-not (string-match-p "A root" (buffer-string)))
      (should-not (string-match-p "A leaf" (buffer-string)))
      (should (= (how-many "^@ " (point-min) (point-max)) 1)))))

(ert-deftest pilish-test-tree-browser-ordinary-render-is-generation-fenced ()
  "A load landing inside a filter-style render replaces partial text.
Unlike the completed-load test, A enters through an ordinary rerender;
the buffer-wide render transaction must still queue B, abort A after
the visibility hook, and repaint only B."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((buf (current-buffer))
           (owner-a "ordinary-owner-a")
           (owner-b "ordinary-owner-b")
           (tree-a
            [(:id "ordinary-a-root" :type "message" :role "user"
              :preview "ordinary A root" :children
              [(:id "ordinary-a-leaf" :parentId "ordinary-a-root"
                :type "message" :role "assistant"
                :preview "ordinary A leaf" :children [])])])
           (tree-b
            [(:id "ordinary-b" :type "message" :role "assistant"
              :preview "ordinary B only" :children [])])
           (tree-node-hooks 0)
           (armed t)
           (magit-section-set-visibility-hook
            (list
             (lambda (section)
               (when (eq (oref section type) 'tree-node)
                 (setq tree-node-hooks (1+ tree-node-hooks))
                 (when (and armed (= tree-node-hooks 2))
                   (setq armed nil
                         pilish--tree-browser-fetch-token 2
                         pilish--tree-browser-state-file owner-b)
                   (pilish--tree-browser-apply-load
                    buf tree-b "ordinary-b" nil nil nil owner-b 2)))
               nil))))
      (setq pilish--tree-browser-fetch-token 1
            pilish--tree-browser-state-file owner-a
            pilish--tree-browser-loaded-file owner-a
            pilish--tree-browser-tree tree-a
            pilish--tree-browser-leaf-id "ordinary-a-leaf")
      (pilish--tree-browser-rerender)
      (should-not armed)
      (should (equal pilish--tree-browser-loaded-file owner-b))
      (should (equal pilish--tree-browser-leaf-id "ordinary-b"))
      (should (string-match-p "ordinary B only" (buffer-string)))
      (should-not (string-match-p "ordinary A" (buffer-string))))))

(ert-deftest pilish-test-tree-browser-loading-render-is-generation-fenced ()
  "A load landing inside the real loading paint wins without mixing.
The loading render is not itself a completed load, but it must own the
same buffer-wide transaction so B can queue at Magit's root visibility
hook and repaint after A aborts.  A's now-stale loader seam is harmless."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let* ((buf (current-buffer))
           (owner-a "loading-owner-a")
           (owner-b "loading-owner-b")
           (tree-b
            [(:id "loading-b" :type "message" :role "assistant"
              :preview "loading B only" :children [])])
           (armed t)
           (old-load-called nil)
           (magit-section-set-visibility-hook
            (list
             (lambda (section)
               (when (and armed
                          (eq (oref section type) 'root))
                 (setq armed nil
                       pilish--tree-browser-fetch-token 2
                       pilish--tree-browser-state-file owner-b)
                 (pilish--tree-browser-apply-load
                  buf tree-b "loading-b" nil nil nil owner-b 2))
               nil))))
      (cl-letf (((symbol-function 'pilish--tree-browser-chat-session-file)
                 (lambda () owner-a))
                ((symbol-function 'pilish--browse-load-tree)
                 (lambda (&rest _args)
                   (setq old-load-called t))))
        (pilish--tree-browser-fetch-and-render))
      (should-not armed)
      (should old-load-called)
      (should (equal pilish--tree-browser-loaded-file owner-b))
      (should (equal pilish--tree-browser-leaf-id "loading-b"))
      (should (string-match-p "loading B only" (buffer-string)))
      (should-not (string-match-p "Loading tree" (buffer-string))))))

(ert-deftest pilish-test-tree-file-switch-resets-shared-id-anchor ()
  "A different session file owns a fresh orientation even with shared ids.
The two same-project files share root id `shared'.  Point is moved to
and folded at that root in file A; switching the chat to file B must
ignore the still-valid old section identity and fold state, then select
B's projected active leaf.  An ordinary same-file refresh then preserves
a manual selection."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-owner"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-owner-chat*")))
    (pilish-test--write-session-lines
     path-a
     (list (pilish-test--make-session-header "sid-owner-a")
           (pilish-test--user-line "shared" nil "shared root A")
           (pilish-test--jsonl-line
            "message" "a-leaf" "shared"
            :message '(:role "assistant" :content "active A"
                       :stopReason "end_turn"))))
    (pilish-test--write-session-lines
     path-b
     (list (pilish-test--make-session-header "sid-owner-b")
           (pilish-test--user-line "shared" nil "shared root B")
           (pilish-test--jsonl-line
            "message" "b-leaf" "shared"
            :message '(:role "assistant" :content "active B"
                       :stopReason "end_turn"))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path-a)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (goto-char (point-min))
            (search-forward "shared root A")
            (should (equal (oref (magit-current-section) value) "shared"))
            (pilish-browse-toggle-fold)
            (should (gethash "shared" pilish--browse-fold-state))
            (with-current-buffer chat-buf
              (setq pilish--state (list :session-file path-b)))
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal pilish--tree-browser-loaded-file path-b))
            (should-not (gethash "shared" pilish--browse-fold-state))
            (should (equal (oref (magit-current-section) value) "b-leaf"))
            ;; Same-file refresh still honors a subsequent manual move.
            (goto-char (point-min))
            (search-forward "shared root B")
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal (oref (magit-current-section) value) "shared"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-fetch-without-process ()
  "Offline fetch orients once, then preserves a user's selected turn.
The tree comes from disk without a live process.  Its first completed
load selects active leaf m4.  After the user moves to m1, an ordinary
refresh preserves that section identity instead of stealing point back
to m4."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-noproc"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-noproc-chat*")))
    (pilish-test--write-session-lines
     path (pilish-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (string-match-p "fix the parser" (buffer-string)))
            (should (equal (oref (magit-current-section) value) "m4"))
            (should-not pilish--tree-browser-loading)
            (should-not pilish--tree-browser-error)
            ;; Move away from the active leaf before the disk refresh.
            (goto-char (point-min))
            (search-forward "fix the parser")
            (should (equal (oref (magit-current-section) value) "m1"))
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal (oref (magit-current-section) value) "m1"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-refresh-missing-selection-uses-old-parent ()
  "A disk refresh whose selected node vanished selects its old parent.
The user first selects off-branch b1.  An external rewrite removes b1
while leaving both its old parent u1 and current leaf c1; the nearest
surviving old ancestor wins before the fresh active path."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-refresh-point"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-refresh-chat*"))
         (initial (pilish-test--navigable-session-lines)))
    (pilish-test--write-session-lines path initial)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (goto-char (point-min))
            (search-forward "abandoned branch")
            (should (equal (oref (magit-current-section) value) "b1"))
            ;; Remove only b1; c1 stays the raw/projected active leaf,
            ;; while b1's old parent u1 survives.
            (pilish-test--write-session-lines
             path (append (cl-subseq initial 0 2) (nthcdr 3 initial)))
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal (oref (magit-current-section) value) "u1"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-label-appends-label-entry ()
  "Setting a label appends exactly one label entry to the session file.
The line carries a fresh 8-hex id colliding with nothing, :parentId is
the last parseable line's id, :targetId names the node, the timestamp
is ISO-ms no older than the call, and prior bytes stay byte-for-byte
intact (a missing trailing newline gains a separator first).  The
cached projected tree is patched and re-rendered in place — the file
is NOT read again — and the label shows as a margin overlay."
  (let* ((dir (pilish-test--make-temp-directory "pi-label-append"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-append-chat*"))
         (start-iso (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))
         (messages nil)
         (read-calls 0)
         (real-read (symbol-function 'pilish-jsonl-read-file)))
    ;; No trailing newline: the append must add the separator itself.
    (pilish-test--write-session-lines
     path (pilish-test--live-session-lines) t)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'pilish-jsonl-read-file)
                       (lambda (p)
                         (cl-incf read-calls)
                         (funcall real-read p))))
              (pilish-test--sync-timers
                (lambda () (pilish--tree-browser-fetch-and-render)))
              (let ((reads-after-fetch read-calls)
                    (before (pilish-test--file-contents path)))
                (cl-letf (((symbol-function 'message)
                           (lambda (fmt &rest args)
                             (push (apply #'format fmt args) messages))))
                  (pilish--browse-set-label "m2" "keep this"))
                ;; Exactly one line appended after a separator.
                (let* ((after (pilish-test--file-contents path))
                       (appended (car (split-string
                                       (substring after (length before))
                                       "\n" t)))
                       (entry (json-parse-string appended :object-type 'plist)))
                  (should (string-prefix-p before after))
                  (should (string-suffix-p (concat appended "\n") after))
                  (should (equal (plist-get entry :type) "label"))
                  (should (equal (plist-get entry :targetId) "m2"))
                  (should (equal (plist-get entry :label) "keep this"))
                  (should (string-match-p "\\`[0-9a-f]\\{8\\}\\'"
                                          (plist-get entry :id)))
                  (should (not (member (plist-get entry :id)
                                       '("sid-live" "m1" "m2" "m3" "m4"))))
                  ;; parentId is the last parseable line's id.
                  (should (equal (plist-get entry :parentId) "m4"))
                  (let ((ts (plist-get entry :timestamp)))
                    (should (string-match-p
                             "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}Z\\'"
                             ts))
                    (should (not (string< ts start-iso)))))
                ;; Patched in place: no re-read of the session file.
                (should (= read-calls reads-after-fetch))
                (should (equal (plist-get
                                (pilish-test--tree-find-node
                                 pilish--tree-browser-tree "m2")
                                :label)
                               "keep this"))
                (should (pilish-test--margin-overlay-contains-p
                         "keep this"))
                (should (cl-some (lambda (m) (string-match-p "Label set" m))
                                 messages))))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-label-clear-omits-label-key ()
  "Clearing a label appends a label entry with the label key omitted.
pi's appendLabelChange shape omits :label on a clear (the load-time
fold treats absent as clear), so the raw line has no \"label\" key at
all; the cached tree loses :label and the margin overlay disappears."
  (let* ((dir (pilish-test--make-temp-directory "pi-label-clear"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-clear-chat*"))
         (messages nil))
    (pilish-test--write-session-lines
     path (append (pilish-test--live-session-lines)
                  (list (pilish-test--jsonl-line
                         "label" "l1" "m4" :targetId "m2" :label "old tag"))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (pilish-test--margin-overlay-contains-p "old tag"))
            (let ((before (pilish-test--file-contents path)))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (pilish--browse-set-label "m2" nil))
              (let* ((after (pilish-test--file-contents path))
                     (appended (car (split-string
                                     (substring after (length before))
                                     "\n" t)))
                     (entry (json-parse-string appended :object-type 'plist)))
                (should (string-prefix-p before after))
                (should (equal (plist-get entry :type) "label"))
                (should (equal (plist-get entry :targetId) "m2"))
                ;; The clear omits the label key entirely — the only
                ;; "label" substring left is the "type":"label" pair.
                (should-not (plist-get entry :label))
                (should-not (string-match-p "\"label\":" appended))
                ;; parentId is the last parseable line's id (the old
                ;; label line).
                (should (equal (plist-get entry :parentId) "l1"))))
            ;; The cached tree and buffer lose the label.
            (should-not (plist-get
                         (pilish-test--tree-find-node
                          pilish--tree-browser-tree "m2")
                         :label))
            (should-not (pilish-test--margin-overlay-contains-p
                         "old tag"))
            (should (cl-some (lambda (m) (string-match-p "Label cleared" m))
                             messages))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-label-survives-refetch ()
  "An appended label survives a refetch and stays on the same leaf.
The label folds back from disk, and although the file's last line is
now the label entry itself (the new raw leaf), the projected leaf
still resolves up to the pre-label visible leaf."
  (let* ((dir (pilish-test--make-temp-directory "pi-label-refetch"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-refetch-chat*")))
    (pilish-test--write-session-lines
     path (pilish-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal pilish--tree-browser-leaf-id "m4"))
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal pilish--tree-browser-leaf-id "m4"))
            (pilish--browse-set-label "m1" "checkpoint")
            ;; The local patch and a fresh disk fold agree exactly: the
            ;; patched cached tree and leaf equal a whole fresh
            ;; projection of the file (label pair in the canonical
            ;; after-:timestamp slot, clear leaving no pair at all).
            (let ((fresh (pilish-jsonl-project-session-file path)))
              (should (equal pilish--tree-browser-tree
                             (plist-get fresh :tree)))
              (should (equal pilish--tree-browser-leaf-id
                             (plist-get fresh :leafId))))
            ;; The file's last line is now the label entry.
            (let* ((session (pilish-jsonl-read-file path))
                   (raw-leaf (plist-get session :leafId)))
              (should (string-match-p "\\`[0-9a-f]\\{8\\}\\'" raw-leaf))
              (should-not (member raw-leaf '("m1" "m2" "m3" "m4"))))
            ;; Refetch: folded from disk, leaf unchanged, still shown.
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal pilish--tree-browser-leaf-id "m4"))
            (should (equal (plist-get
                            (pilish-test--tree-find-node
                             pilish--tree-browser-tree "m1")
                            :label)
                           "checkpoint"))
            (should (pilish-test--margin-overlay-contains-p
                     "checkpoint"))
            (should-not pilish--tree-browser-error)))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-load-tree-interrupted-by-quit ()
  "A quit during the deferred read reports an error state, not a stuck
loading render.  C-g against a huge file raises `quit' — not `error' —
inside the blocking read; the seam must still call back so the loading
state clears and the buffer names the interruption."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-quit"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-quit-chat*")))
    (pilish-test--write-session-lines
     path (pilish-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'pilish-jsonl-project-session-file)
                       (lambda (_path) (signal 'quit nil))))
              (pilish-test--sync-timers
                (lambda () (pilish--tree-browser-fetch-and-render))))
            (should-not pilish--tree-browser-loading)
            (should (string-match-p "interrupted"
                                    pilish--tree-browser-error))
            (should (string-match-p "interrupted" (buffer-string)))
            (should (= pilish--tree-browser-visible-count 0))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-fetch-path-owned-before-loading-paint ()
  "A session switch during the loading paint cannot retarget the fetch.
The fetch claims file A before `redisplay'; if process/UI work changes
the linked chat to B during that paint, the loader must still read A so
tree, loaded-file guard, orientation, and anchor ownership all agree."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-paint-owner"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-paint-owner-chat*")))
    (pilish-test--write-session-lines
     path-a
     (list (pilish-test--make-session-header "sid-paint-a")
           (pilish-test--user-line "a-root" nil "owned by A")))
    (pilish-test--write-session-lines
     path-b
     (list (pilish-test--make-session-header "sid-paint-b")
           (pilish-test--user-line "b-root" nil "retargeted to B")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path-a)))
          (pilish-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'redisplay)
                       (lambda (&rest _)
                         (with-current-buffer chat-buf
                           (setq pilish--state (list :session-file path-b))))))
              (pilish-test--sync-timers
                (lambda () (pilish--tree-browser-fetch-and-render))))
            (should (equal pilish--tree-browser-state-file path-a))
            (should (equal pilish--tree-browser-loaded-file path-a))
            (should (string-match-p "owned by A" (buffer-string)))
            (should-not (string-match-p "retargeted to B" (buffer-string)))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-load-tree-mid-read-session-switch ()
  "A chat session switch between fetch start and the deferred read
leaves `--tree-browser-loaded-file' pinned to the file the fetch
actually read.  Resolving the link again at callback time would arm
the labeler against the NEW session while the browser still shows the
OLD tree, appending a node id from one file into the other; instead
labeling must refuse with the refresh message and touch nothing."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-midsw"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-midsw-chat*"))
         (messages nil))
    (pilish-test--write-session-lines
     path-a (pilish-test--live-session-lines))
    (pilish-test--write-session-lines
     path-b (pilish-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path-a)))
          (pilish-test--with-tree-link chat-buf
            (cl-letf* ((real (symbol-function 'pilish-jsonl-project-session-file))
                       ((symbol-function 'pilish-jsonl-project-session-file)
                        (lambda (p)
                          ;; The chat switches sessions while the
                          ;; deferred read runs; the read itself
                          ;; still returns path-a's projection.
                          (with-current-buffer chat-buf
                            (setq pilish--state
                                  (list :session-file path-b)))
                          (funcall real p))))
              (pilish-test--sync-timers
                (lambda () (pilish--tree-browser-fetch-and-render))))
            (should (equal pilish--tree-browser-loaded-file
                           path-a))
            ;; Fresh resolution now disagrees: labeling refuses.
            (let ((before-b (pilish-test--file-contents path-b)))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (pilish--browse-set-label "m1" "late"))
              (should (member
                       "Pi: Session changed since the tree was loaded — refresh with g"
                       messages))
              (should (equal (pilish-test--file-contents path-b)
                             before-b)))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-load-tree-mid-read-invalidation-drops-stale-publication ()
  "A newer owner started during callback-capable disk I/O wins.
Fetch A's projected-file read reentrantly switches the chat to B and
completes a B fetch.  When A's yielding read resumes, it must validate
both its generation and file owner before callback/publication; B's
state, tree, and loaded-file remain paired."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-midread-owner"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-midread-owner-chat*")))
    (pilish-test--write-session-lines
     path-a
     (list (pilish-test--make-session-header "sid-midread-a")
           (pilish-test--user-line "a-root" nil "stale tree A")))
    (pilish-test--write-session-lines
     path-b
     (list (pilish-test--make-session-header "sid-midread-b")
           (pilish-test--user-line "b-root" nil "winning tree B")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path-a)))
          (pilish-test--with-tree-link chat-buf
            (let ((first-read t))
              (cl-letf* ((real
                          (symbol-function
                           'pilish-jsonl-project-session-file))
                         ((symbol-function
                           'pilish-jsonl-project-session-file)
                          (lambda (path)
                            (when first-read
                              (setq first-read nil)
                              (with-current-buffer chat-buf
                                (setq pilish--state
                                      (list :session-file path-b)))
                              (pilish--tree-browser-fetch-and-render))
                            (funcall real path)))
                         ((symbol-function 'redisplay) #'ignore))
                (pilish-test--sync-timers
                  (lambda () (pilish--tree-browser-fetch-and-render)))))
            (should (= pilish--tree-browser-fetch-token 2))
            (should (equal pilish--tree-browser-state-file path-b))
            (should (equal pilish--tree-browser-loaded-file path-b))
            (should (equal pilish--tree-browser-leaf-id "b-root"))
            (should (equal (plist-get
                            (aref pilish--tree-browser-tree 0)
                            :preview)
                           "winning tree B"))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-tree-fetch-paints-loading-state ()
  "The loading render is painted before the deferred read is scheduled.
Emacs runs due 0-timers before redisplaying, so a single timer hop to
the read starves the loading paint entirely (verified mechanically:
mid-read the terminal still shows the previous buffer contents).  The
fetch must force a `redisplay' between the loading render and the
seam call."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let ((log nil))
      (cl-letf (((symbol-function 'redisplay)
                 (lambda (&rest _) (push :redisplay log)))
                ((symbol-function 'pilish--browse-load-tree)
                 (lambda (_callback &optional _path _generation)
                   (push :load log))))
        (pilish--tree-browser-fetch-and-render))
      ;; Strict order: the paint lands before the seam (and thus
      ;; before any deferred read) is even scheduled.
      (should (equal log '(:load :redisplay))))))

(ert-deftest pilish-test-set-label-rejects-stale-session ()
  "Labeling refuses when the chat moved to another session file.
The loaded-file guard compares against a fresh resolution of the chat
link: a mismatch messages instead of appending, and neither the loaded
nor the current file changes."
  (let* ((dir (pilish-test--make-temp-directory "pi-label-stale"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-stale-chat*"))
         (messages nil))
    (pilish-test--write-session-lines
     path-a (pilish-test--live-session-lines))
    (pilish-test--write-session-lines
     path-b (pilish-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path-a)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (equal pilish--tree-browser-loaded-file path-a))
            ;; The chat switches sessions behind the browser's back.
            (with-current-buffer chat-buf
              (setq pilish--state (list :session-file path-b)))
            (let ((before-a (pilish-test--file-contents path-a))
                  (before-b (pilish-test--file-contents path-b)))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (pilish--browse-set-label "m1" "late"))
              (should (member
                       "Pi: Session changed since the tree was loaded — refresh with g"
                       messages))
              (should (equal (pilish-test--file-contents path-a)
                             before-a))
              (should (equal (pilish-test--file-contents path-b)
                             before-b)))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-set-label-no-session-file ()
  "Labeling with no resolvable session file reports and writes nothing.
Both a dead chat link and a live link whose state has no :session-file
message \"Pi: Cannot label: no session file\"; no file is created or
appended anywhere."
  (let* ((dir (pilish-test--make-temp-directory "pi-label-nofile"))
         (chat-buf (generate-new-buffer " *test-label-nofile-chat*"))
         (messages nil))
    (unwind-protect
        (progn
          ;; Live link, state without a session file.
          (with-current-buffer chat-buf
            (setq pilish--state (list :messageCount 3)))
          (pilish-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (pilish--browse-set-label "m1" "nowhere")
              ;; No chat link at all.
              (setq pilish--chat-buffer nil)
              (pilish--browse-set-label "m1" "nowhere")
              (should (equal (cl-count-if
                              (lambda (m)
                                (string-match-p
                                 "\\`Pi: Cannot label: no session file\\'" m))
                              messages)
                             2))))
          ;; Nothing was written anywhere in the scratch directory.
          (should-not (directory-files dir nil "\\.jsonl\\'")))
      (kill-buffer chat-buf))))

;;;; Phase 4: Tree Navigation

(defun pilish-test--navigable-session-lines ()
  "Return raw session lines with a branch point for navigation tests.
u1 (root user) has two assistant children — b1, an abandoned sibling,
and a1 — whose historical user child u2 has a newer assistant c1.
Thus u2 is not the actual current projected entry.  Continuing from u2
must retain the established user re-edit rule and rewind the leaf to
a1: the header stays first, off-chain b1/u2/c1 keep their relative
order ahead of root chain u1/a1, and a1 becomes the new last line."
  (list (pilish-test--make-session-header "sid-nav")
        (pilish-test--user-line "u1" nil "fix the parser")
        (pilish-test--jsonl-line
         "message" "b1" "u1"
         :message '(:role "assistant" :content "abandoned branch"
                    :stopReason "end_turn"))
        (pilish-test--jsonl-line
         "message" "a1" "u1"
         :message '(:role "assistant" :content "checking"
                    :stopReason "end_turn"))
        (pilish-test--user-line "u2" "a1" "try the other way")
        (pilish-test--jsonl-line
         "message" "c1" "u2"
         :message '(:role "assistant" :content "current answer"
                    :stopReason "end_turn"))))

(defun pilish-test--navigate-rewritten-contents
    (lines &optional separator)
  "Return expected bytes after continuing from historical u2.
LINES start (header, u1, b1, a1, u2); every remaining line (normally
current child c1, plus any malformed lines) is off the target chain.
The stable partition is header, b1, u2, remaining…, u1, a1.  SEPARATOR
defaults to LF and is also appended once at end."
  (let ((separator (string-as-unibyte (or separator "\n"))))
    (concat (mapconcat #'string-as-unibyte
                       (append (list (nth 0 lines) (nth 2 lines)
                                     (nth 4 lines))
                               (nthcdr 5 lines)
                               (list (nth 1 lines) (nth 3 lines)))
                       separator)
            separator)))

(defmacro pilish-test--with-navigate-fixture
    (lines path chat-buf input-buf proc messages resume-calls quit-calls
     ready-calls &rest body)
  "Run BODY inside a tree browser wired for a `--browse-navigate' call.
LINES is an expression yielding the raw session lines written to a
fresh PATH in a temp directory.  CHAT-BUF's state names PATH, its
process is a live PROC, and its linked INPUT-BUF starts with a stale
draft.  The tree browser fetches synchronously first, so
`--tree-browser-loaded-file' is PATH.  message,
`--resume-selected-session', `--browse-quit-when-settled', the resume
cwd pre-flight (`--session-file-cwd-or-error', satisfied with the
session directory), and `--session-transition-ready-p' (which answers
ready) are spied into MESSAGES, RESUME-CALLS, QUIT-CALLS, and
READY-CALLS.  BODY runs inside the `cl-letf*', so a nested `cl-letf'
overrides any spy."
  (declare (indent 9))
  (let ((dir (gensym "nav-dir")))
    `(let* ((,dir (pilish-test--make-temp-directory "pi-nav"))
            (,path (expand-file-name "session.jsonl" ,dir))
            (,proc (start-process "pi-nav-test" nil "sleep" "30"))
            (,chat-buf (generate-new-buffer " *test-nav-chat*"))
            (,input-buf (generate-new-buffer " *test-nav-input*")))
       (unwind-protect
           (progn
             (pilish-test--write-session-lines ,path ,lines)
             (with-current-buffer ,chat-buf
               (setq pilish--state (list :session-file ,path)
                     pilish--process ,proc
                     pilish--input-buffer ,input-buf))
             (with-current-buffer ,input-buf
               (insert "stale draft"))
             (with-temp-buffer
               (pilish-tree-browser-mode)
               (setq pilish--chat-buffer ,chat-buf)
               (pilish-test--sync-timers
                 (lambda ()
                   (pilish--tree-browser-fetch-and-render)))
               (let ((,messages nil)
                     (,resume-calls nil)
                     (,quit-calls nil)
                     (,ready-calls nil))
                 (cl-letf* (((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (push (apply #'format fmt args) ,messages)))
                            ;; Existing navigation tests exercise the accepted
                            ;; path.  Draft-protection tests below override this
                            ;; default to inspect both answers and prompt order.
                            ((symbol-function 'y-or-n-p)
                             (lambda (&rest _) t))
                            ((symbol-function
                              'pilish--resume-selected-session)
                             (lambda (&rest args) (push args ,resume-calls)))
                            ((symbol-function
                              'pilish--browse-quit-when-settled)
                             (lambda (&rest args) (push args ,quit-calls)))
                            ((symbol-function
                              'pilish--session-file-cwd-or-error)
                             (lambda (&rest _) ,dir))
                            ((symbol-function
                              'pilish--session-transition-ready-p)
                             (lambda (&rest args)
                               (push args ,ready-calls)
                               t)))
                   ,@body))))
         (delete-process ,proc)
         (pilish-test--kill-live-buffers ,chat-buf ,input-buf)))))

(ert-deftest pilish-test-navigate-guards ()
  "Navigate refuses, in order, before touching anything: no linked
chat (`user-error'), no resolvable session file, a stale loaded file
(the chat switched sessions behind the browser), a dead process
(`user-error'), an active session transition, and a not-ready chat.
Every refusal leaves the session files byte-identical and the resume
flow uncalled."
  ;; No linked chat: user-error before any other guard.
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--chat-buffer nil)
    (should (equal (error-message-string
                    (should-error
                     (pilish--browse-navigate "u2")
                     :type 'user-error))
                   "No pi session to continue from selected turn")))
  ;; No session file: message, nothing written.
  (let* ((chat-buf (generate-new-buffer " *test-nav-nofile-chat*"))
         (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :messageCount 3)))
          (pilish-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (pilish--browse-navigate "u2"))
            (should (member
                     "Pi: Cannot continue from selected turn: no session file"
                     messages))))
      (kill-buffer chat-buf)))
  ;; Stale loaded file: the chat moved to another session.
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let* ((other (expand-file-name
                   "other.jsonl" (file-name-directory path)))
           (before (pilish-test--file-contents path)))
      (pilish-test--write-session-lines
       other (pilish-test--navigable-session-lines))
      (let ((other-before (pilish-test--file-contents other)))
        (with-current-buffer chat-buf
          (setq pilish--state (list :session-file other)))
        (pilish--browse-navigate "u2")
        (should (member
                 "Pi: Session changed since the tree was loaded — refresh with g"
                 messages))
        (should (equal (pilish-test--file-contents path) before))
        (should (equal (pilish-test--file-contents other)
                       other-before))
        (should-not resume-calls))))
  ;; Dead process: user-error, before the transition guards.
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (with-current-buffer chat-buf
        (setq pilish--process nil))
      (should (equal (error-message-string
                      (should-error
                       (pilish--browse-navigate "u2")
                       :type 'user-error))
                     "Pi process is not running"))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not ready-calls)))
  ;; Active transition: message, and the ready guard never runs.
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (with-current-buffer chat-buf
        (setq pilish--session-transition-active t))
      (pilish--browse-navigate "u2")
      (should (member
               "Pi: Cannot continue from selected turn while switching sessions"
               messages))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not ready-calls)))
  ;; Not ready: the guard reports its own refusal and continuation
  ;; returns quietly, passing the user-facing action wording.
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (cl-letf (((symbol-function
                  'pilish--session-transition-ready-p)
                 (lambda (chat-buf action)
                   (push (list chat-buf action) ready-calls)
                   nil)))
        (pilish--browse-navigate "u2"))
      (should (equal ready-calls
                     (list (list chat-buf "continue from selected turn"))))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest pilish-test-tree-legacy-projection-has-no-false-markers-or-ret-target ()
  "Actual legacy nil/empty-id rows are visible but unaddressable.
The projected leaf is nil.  Rendering must not infer absent == absent
as current/active or give an empty id a section identity; RET truthfully
explains that the selected legacy row cannot be continued from and never
calls the internal continuation seam."
  (let* ((dir (pilish-test--make-temp-directory "pi-tree-legacy"))
         (path (expand-file-name "old.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-legacy-chat*"))
         (messages nil)
         (continue-calls nil))
    (pilish-test--write-session-lines
     path
     (list (json-encode
            (list :type "session"
                  :id "sid-old"
                  :timestamp pilish-test--browse-timestamp
                  :cwd "/home/fake/a"))
           (json-encode
            (list :type "message"
                  :timestamp pilish-test--browse-timestamp
                  :message '(:role "user" :content "v1 prompt")))
           (json-encode
            (list :type "message"
                  :id ""
                  :timestamp pilish-test--browse-timestamp
                  :message '(:role "assistant"
                             :content "malformed empty id")))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          (pilish-test--with-tree-link chat-buf
            (pilish-test--sync-timers
              (lambda () (pilish--tree-browser-fetch-and-render)))
            (should (string-match-p "v1 prompt" (buffer-string)))
            (should (string-match-p "malformed empty id" (buffer-string)))
            (should-not pilish--tree-browser-leaf-id)
            (should-not (oref (magit-current-section) value))
            (should-not (string-match-p "^[@*] " (buffer-string)))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages)))
                      ((symbol-function 'pilish--browse-navigate)
                       (lambda (&rest args) (push args continue-calls))))
              (pilish-tree-browser-navigate))
            (should-not continue-calls)
            (should (member
                     (concat
                      "Pi: Cannot continue from selected turn: legacy entry "
                      "has no id; open it with pi once to migrate, then refresh with g")
                     messages))))
      (kill-buffer chat-buf))))

(ert-deftest pilish-test-navigate-unknown-node ()
  "A node id the session file does not carry refuses with the refresh
hint; the file is untouched and no switch is scheduled."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (pilish--browse-navigate "deadbeef")
      (should (member
               "Pi: Cannot continue from selected turn: no such tree node — refresh with g"
               messages))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest pilish-test-navigate-current-assistant-bypasses-all-live-guards ()
  "RET on the loaded @ assistant is a strict no-op before live guards.
Even if the linked chat is simultaneously offline, streaming, busy,
and switching, no process/read/guard/target or mutation seam runs.
The raw leaf is a trailing projected-away label resolving to a1."
  (pilish-test--with-navigate-fixture
      (list (pilish-test--make-session-header "sid-here")
            (pilish-test--user-line "u1" nil "fix the parser")
            (pilish-test--jsonl-line
             "message" "a1" "u1"
             :message '(:role "assistant" :content "checking"
                        :stopReason "end_turn"))
            (pilish-test--jsonl-line
             "label" "l1" "a1" :targetId "u1" :label "checkpoint"))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (should (equal (oref (magit-current-section) value) "a1"))
    (beginning-of-line)
    (should (looking-at-p "@ ast"))
    (with-current-buffer chat-buf
      (setq pilish--process nil
            pilish--status 'streaming
            pilish--session-transition-active t))
    (let ((before (pilish-test--file-contents path))
          (forbidden nil)
          (real-read (symbol-function 'pilish-jsonl-read-file))
          (real-target (symbol-function 'pilish-jsonl-navigation-target)))
      (cl-letf (((symbol-function 'pilish--session-live-process-p)
                 (lambda (&rest _) (push :process-guard forbidden) t))
                ((symbol-function 'pilish--browse-transition-refused-p)
                 (lambda (&rest _) (push :transition-guards forbidden) nil))
                ((symbol-function 'pilish-jsonl-read-file)
                 (lambda (p)
                   (push :disk-read forbidden)
                   (funcall real-read p)))
                ((symbol-function 'pilish-jsonl-navigation-target)
                 (lambda (&rest args)
                   (push :target forbidden)
                   (apply real-target args)))
                ((symbol-function 'pilish-jsonl-current-projected-id)
                 (lambda (&rest _) (push :fresh-current forbidden) "a1"))
                ((symbol-function 'pilish-jsonl-navigation-lines)
                 (lambda (&rest _) (push :lines forbidden) []))
                ((symbol-function 'pilish--browse-rewrite-session-file)
                 (lambda (&rest _) (push :rewrite forbidden) t))
                ((symbol-function 'pilish--resume-selected-session)
                 (lambda (&rest _) (push :resume forbidden)))
                ((symbol-function 'pilish--browse-prefill-input)
                 (lambda (&rest _) (push :prefill forbidden)))
                ((symbol-function 'pilish--browse-quit-when-settled)
                 (lambda (&rest _) (push :settle forbidden)))
                ((symbol-function 'pilish--session-file-cwd-or-error)
                 (lambda (&rest _) (push :cwd forbidden)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (push :draft-prompt forbidden) t)))
        (pilish-tree-browser-navigate))
      (should-not forbidden)
      (should (member "Pi: Already at current position" messages))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls)
      (should-not ready-calls)
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft")))))

(ert-deftest pilish-test-navigate-current-user-with-bookkeeping-is-no-op ()
  "Selecting the actual current projected user entry changes nothing.
Trailing label/session-info/custom records project away to u2.  RET on
u2 must not apply the historical-user rewind rule: no rewrite, resume,
prefill, settle wait, or draft loss."
  (pilish-test--with-navigate-fixture
      (list (pilish-test--make-session-header "sid-current-user")
            (pilish-test--user-line "u1" nil "root prompt")
            (pilish-test--jsonl-line
             "message" "a1" "u1"
             :message '(:role "assistant" :content "reply"
                        :stopReason "end_turn"))
            (pilish-test--user-line "u2" "a1" "current prompt")
            (pilish-test--jsonl-line
             "label" "l1" "u2" :targetId "u2" :label "current")
            (pilish-test--jsonl-line
             "session_info" "s1" "l1" :name "named")
            (pilish-test--jsonl-line
             "custom" "c1" "s1" :customType "bookkeeping" :data '(:ok t)))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (should (equal (oref (magit-current-section) value) "u2"))
    (beginning-of-line)
    (should (looking-at-p "@ you"))
    (with-current-buffer chat-buf
      (setq pilish--process nil
            pilish--status 'streaming
            pilish--session-transition-active t))
    (let ((before (pilish-test--file-contents path))
          (forbidden nil)
          (real-read (symbol-function 'pilish-jsonl-read-file))
          (real-target (symbol-function 'pilish-jsonl-navigation-target)))
      (cl-letf (((symbol-function 'pilish--session-live-process-p)
                 (lambda (&rest _) (push :process-guard forbidden) t))
                ((symbol-function 'pilish--browse-transition-refused-p)
                 (lambda (&rest _) (push :transition-guards forbidden) nil))
                ((symbol-function 'pilish-jsonl-read-file)
                 (lambda (p)
                   (push :disk-read forbidden)
                   (funcall real-read p)))
                ((symbol-function 'pilish-jsonl-navigation-target)
                 (lambda (&rest args)
                   (push :target forbidden)
                   (apply real-target args)))
                ((symbol-function 'pilish-jsonl-current-projected-id)
                 (lambda (&rest _) (push :fresh-current forbidden) "u2"))
                ((symbol-function 'pilish-jsonl-navigation-lines)
                 (lambda (&rest _) (push :lines forbidden) []))
                ((symbol-function 'pilish--browse-rewrite-session-file)
                 (lambda (&rest _) (push :rewrite forbidden) t))
                ((symbol-function 'pilish--resume-selected-session)
                 (lambda (&rest _) (push :resume forbidden)))
                ((symbol-function 'pilish--browse-prefill-input)
                 (lambda (&rest _) (push :prefill forbidden)))
                ((symbol-function 'pilish--browse-quit-when-settled)
                 (lambda (&rest _) (push :settle forbidden)))
                ((symbol-function 'pilish--session-file-cwd-or-error)
                 (lambda (&rest _) (push :cwd forbidden))))
        ;; Exercise RET at the oriented current user row.
        (pilish-tree-browser-navigate))
      (should (member "Pi: Already at current position" messages))
      (should-not forbidden)
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls)
      (should-not ready-calls)
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft")))))

(ert-deftest pilish-test-navigate-ambiguous-current-does-not-false-no-op ()
  "Two unresolved positions never suppress a requested branch change.
The unique user `u' rewinds to root bookkeeping `meta', which has no
visible resolution.  The current raw leaf `dup' is a differing duplicate
and is unresolved for a separate reason.  RET must rewrite/resume to
`meta' and prefill `u', not take the :current-p prefill-only path."
  (pilish-test--with-navigate-fixture
      (list (pilish-test--make-session-header "sid-unresolved-current")
            (pilish-test--jsonl-line
             "custom" "meta" nil :customType "root-meta")
            (pilish-test--user-line "u" "meta" "safe prompt")
            (pilish-test--jsonl-line
             "message" "dup" nil
             :message '(:role "assistant" :content "first"
                        :stopReason "end_turn"))
            (pilish-test--jsonl-line
             "message" "dup" nil
             :message '(:role "assistant" :content "later"
                        :stopReason "end_turn")))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (should-not pilish--tree-browser-leaf-id)
      (pilish--browse-navigate "u")
      (should-not (member "Pi: Already at current position" messages))
      (should (equal resume-calls (list (list proc chat-buf path))))
      (should (= (length quit-calls) 1))
      (should-not (equal (pilish-test--file-contents path) before))
      (should (equal (plist-get (pilish-jsonl-read-file path) :leafId)
                     "meta"))
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "safe prompt")))))

(ert-deftest pilish-test-navigate-current-root-user-is-no-op ()
  "A current root user is a no-op, while historical root still refuses."
  (pilish-test--with-navigate-fixture
      (list (pilish-test--make-session-header "sid-current-root")
            (pilish-test--user-line "u1" nil "only prompt"))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (pilish--browse-navigate "u1")
      (should (member "Pi: Already at current position" messages))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls)
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft")))))

(ert-deftest pilish-test-navigate-prefill-only ()
  "Re-editing a prompt the file already sits on (a previous navigate
put its parent last) prefills the input buffer and waits out the
settle — but writes nothing and switches nothing."
  (pilish-test--with-navigate-fixture
      (list (pilish-test--make-session-header "sid-again")
            (pilish-test--user-line "u2" "u1" "try the other way")
            (pilish-test--user-line "u1" nil "fix the parser"))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (pilish--browse-navigate "u2")
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "try the other way"))
      (should (member
               "Pi: Continued from selected turn: try the other way"
               messages))
      (should (equal quit-calls
                     (list (list chat-buf (selected-window) path))))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest pilish-test-navigate-protects-text-draft-before-rewrite ()
  "A declined draft prompt precedes and cancels rewrite and resume.
Accepting the same non-current assistant target then runs the normal
nil-prefill path, including clearing the draft."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path))
          (prompts nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   (should (equal (pilish-test--file-contents path) before))
                   (should-not resume-calls)
                   nil)))
        (pilish--browse-navigate "b1"))
      (should (= (length prompts) 1))
      (should (string-match-p "unsent draft" (car prompts)))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls)
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft"))
      (setq prompts nil)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   (should (equal (pilish-test--file-contents path) before))
                   (should-not resume-calls)
                   t)))
        (pilish--browse-navigate "b1"))
      (should (= (length prompts) 1))
      (should (equal resume-calls (list (list proc chat-buf path))))
      (should (equal (with-current-buffer input-buf (buffer-string)) ""))
      (should (equal (plist-get (pilish-jsonl-read-file path) :leafId)
                     "b1")))))

(ert-deftest pilish-test-navigate-protects-text-draft-before-prefill ()
  "A historical-user prefill asks before replacing an unsent draft.
Declining keeps the draft and performs no continuation side effect;
accepting replaces it without rewriting or resuming the session."
  (pilish-test--with-navigate-fixture
      (list (pilish-test--make-session-header "sid-prefill-guard")
            (pilish-test--user-line "u2" "u1" "try the other way")
            (pilish-test--user-line "u1" nil "fix the parser"))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path))
          (prompts nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   (should (equal (pilish-test--file-contents path) before))
                   (should-not resume-calls)
                   nil)))
        (pilish--browse-navigate "u2"))
      (should (= (length prompts) 1))
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft"))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls)
      (setq prompts nil)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   (should (equal (pilish-test--file-contents path) before))
                   t)))
        (pilish--browse-navigate "u2"))
      (should (= (length prompts) 1))
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "try the other way"))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should (= (length quit-calls) 1)))))

(ert-deftest pilish-test-navigate-protects-image-draft-on-both-paths ()
  "An attached image alone protects rewrite and prefill navigation.
Declining either prompt retains the image and causes no file, resume,
prefill, or settle side effect."
  (let ((image (pilish--make-prompt-image
                :name "draft.png" :mime-type "image/png"
                :byte-size 1 :data "AA==")))
    ;; Rewrite-and-resume path with a nil prefill.
    (pilish-test--with-navigate-fixture
        (pilish-test--navigable-session-lines)
        path chat-buf input-buf proc messages resume-calls quit-calls
        ready-calls
      (with-current-buffer input-buf
        (erase-buffer)
        (pilish--set-prompt-image image))
      (let ((before (pilish-test--file-contents path))
            (prompts 0))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) (cl-incf prompts) nil)))
          (pilish--browse-navigate "b1"))
        (should (= prompts 1))
        (should (equal (pilish-test--file-contents path) before))
        (should-not resume-calls)
        (should-not quit-calls)
        (should (eq (pilish--get-prompt-image input-buf) image))))
    ;; Historical-user prefill-only path.
    (pilish-test--with-navigate-fixture
        (list (pilish-test--make-session-header "sid-image-prefill")
              (pilish-test--user-line "u2" "u1" "try the other way")
              (pilish-test--user-line "u1" nil "fix the parser"))
        path chat-buf input-buf proc messages resume-calls quit-calls
        ready-calls
      (with-current-buffer input-buf
        (erase-buffer)
        (pilish--set-prompt-image image))
      (let ((before (pilish-test--file-contents path))
            (prompts 0))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) (cl-incf prompts) nil)))
          (pilish--browse-navigate "u2"))
        (should (= prompts 1))
        (should (equal (pilish-test--file-contents path) before))
        (should-not resume-calls)
        (should-not quit-calls)
        (should (eq (pilish--get-prompt-image input-buf) image))))))

(ert-deftest pilish-test-navigate-blank-draft-does-not-prompt ()
  "Whitespace without an image is blank and navigation proceeds directly."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (with-current-buffer input-buf
      (erase-buffer)
      (insert " \n\t")
      (pilish--clear-prompt-image))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (ert-fail "Blank draft triggered replacement prompt"))))
      (pilish--browse-navigate "b1"))
    (should (equal resume-calls (list (list proc chat-buf path))))
    (should (equal (with-current-buffer input-buf (buffer-string)) ""))))

(ert-deftest pilish-test-navigate-protects-draft-outside-narrowing ()
  "Draft detection widens before deciding that visible whitespace is blank."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((contents "hidden draft\n   ")
          (prompts 0)
          (before (pilish-test--file-contents path)))
      (with-current-buffer input-buf
        (erase-buffer)
        (insert contents)
        (narrow-to-region (- (point-max) 2) (point-max))
        (should (string-empty-p (string-trim (buffer-string)))))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (cl-incf prompts) nil)))
        (pilish--browse-navigate "b1"))
      (should (= prompts 1))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (with-current-buffer input-buf
        (save-restriction
          (widen)
          (should (equal (buffer-string) contents)))))))

(ert-deftest pilish-test-navigate-rechecks-draft-changed-during-prompt ()
  "An accepted answer cannot authorize a draft changed by the prompt.
The replacement draft gets its own question; declining that question
keeps it and prevents rewrite and resume."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path))
          (prompts 0))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt)
                   (cl-incf prompts)
                   (if (= prompts 1)
                       (progn
                         (with-current-buffer input-buf
                           (erase-buffer)
                           (insert "newer draft"))
                         t)
                     nil))))
        (pilish--browse-navigate "b1"))
      (should (= prompts 2))
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "newer draft"))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls))))

(ert-deftest pilish-test-navigate-revalidates-session-after-draft-prompt ()
  "An accepted answer reruns guards even when the draft became blank."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path))
          (prompts 0))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt)
                   (cl-incf prompts)
                   (with-current-buffer input-buf
                     (erase-buffer))
                   (with-current-buffer chat-buf
                     (setq pilish--session-transition-active t))
                   t)))
        (pilish--browse-navigate "b1"))
      (should (= prompts 1))
      (should (member
               "Pi: Cannot continue from selected turn while switching sessions"
               messages))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls)
      (should (equal (with-current-buffer input-buf (buffer-string)) "")))))

(ert-deftest pilish-test-navigate-confirmation-rejects-session-owner-switch ()
  "An accepted prompt cannot retarget navigation onto an ID-sharing fork.
Switching the linked chat and refreshing this browser during the prompt
invalidates the original owner: neither file changes, no resume runs,
and the original draft remains intact."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let* ((fork (expand-file-name "fork.jsonl" (file-name-directory path)))
           (original-before (pilish-test--file-contents path))
           (prompts 0))
      (pilish-test--write-session-lines
       fork (pilish-test--navigable-session-lines))
      (let ((fork-before (pilish-test--file-contents fork)))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt)
                     (cl-incf prompts)
                     (with-current-buffer chat-buf
                       (setq pilish--state (list :session-file fork)))
                     (pilish-test--sync-timers
                       (lambda ()
                         (pilish--tree-browser-fetch-and-render)))
                     (should (equal pilish--tree-browser-loaded-file fork))
                     t)))
          (pilish--browse-navigate "b1"))
        (should (= prompts 1))
        (should (member
                 (concat
                  "Pi: Cannot continue from selected turn: tree changed "
                  "during draft confirmation")
                 messages))
        (should (equal (pilish-test--file-contents path) original-before))
        (should (equal (pilish-test--file-contents fork) fork-before))
        (should-not resume-calls)
        (should-not quit-calls)
        (should (equal (with-current-buffer input-buf (buffer-string))
                       "stale draft"))))))

(ert-deftest pilish-test-navigate-confirmation-binds-prefill-owner ()
  "Historical-user prefill revalidation stays with its original tree owner."
  (pilish-test--with-navigate-fixture
      (list (pilish-test--make-session-header "sid-prefill-owner")
            (pilish-test--user-line "u2" "u1" "try the other way")
            (pilish-test--user-line "u1" nil "fix the parser"))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let* ((fork (expand-file-name "prefill-fork.jsonl"
                                   (file-name-directory path)))
           (lines (list (pilish-test--make-session-header "sid-prefill-fork")
                        (pilish-test--user-line
                         "u2" "u1" "fork prompt with the same id")
                        (pilish-test--user-line "u1" nil "fork root")))
           (original-before (pilish-test--file-contents path)))
      (pilish-test--write-session-lines fork lines)
      (let ((fork-before (pilish-test--file-contents fork)))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt)
                     (with-current-buffer chat-buf
                       (setq pilish--state (list :session-file fork)))
                     (pilish-test--sync-timers
                       (lambda ()
                         (pilish--tree-browser-fetch-and-render)))
                     t)))
          (pilish--browse-navigate "u2"))
        (should (member
                 (concat
                  "Pi: Cannot continue from selected turn: tree changed "
                  "during draft confirmation")
                 messages))
        (should (equal (pilish-test--file-contents path) original-before))
        (should (equal (pilish-test--file-contents fork) fork-before))
        (should-not resume-calls)
        (should-not quit-calls)
        (should (equal (with-current-buffer input-buf (buffer-string))
                       "stale draft"))))))

(ert-deftest pilish-test-navigate-confirmation-rejects-dead-browser-owner ()
  "Killing the originating browser cannot redirect accepted navigation.
Even when another live browser owns an ID-sharing tree, the original
file and the other tree stay unchanged and no resume or prefill runs."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let* ((original-browser (current-buffer))
           (fork (expand-file-name "other-tree.jsonl"
                                   (file-name-directory path)))
           (other-chat (generate-new-buffer " *test-nav-other-chat*"))
           (other-browser (generate-new-buffer " *test-nav-other-tree*"))
           (original-before (pilish-test--file-contents path))
           (prompts 0))
      (unwind-protect
          (progn
            (pilish-test--write-session-lines
             fork (pilish-test--navigable-session-lines))
            (with-current-buffer other-chat
              (setq pilish--state (list :session-file fork)
                    pilish--process proc
                    pilish--input-buffer input-buf))
            (with-current-buffer other-browser
              (pilish-tree-browser-mode)
              (setq pilish--chat-buffer other-chat)
              (pilish-test--sync-timers
                (lambda ()
                  (pilish--tree-browser-fetch-and-render)))
              (should (equal pilish--tree-browser-loaded-file fork)))
            (let ((fork-before (pilish-test--file-contents fork)))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (_prompt)
                           (cl-incf prompts)
                           (kill-buffer original-browser)
                           (set-buffer other-browser)
                           t)))
                (pilish--browse-navigate "b1"))
              (should (= prompts 1))
              (should (member
                       (concat
                        "Pi: Cannot continue from selected turn: tree changed "
                        "during draft confirmation")
                       messages))
              (should (equal (pilish-test--file-contents path)
                             original-before))
              (should (equal (pilish-test--file-contents fork) fork-before))
              (should-not resume-calls)
              (should-not quit-calls)
              (should (equal (with-current-buffer input-buf (buffer-string))
                             "stale draft"))))
        (pilish-test--kill-live-buffers other-browser other-chat)))))

(ert-deftest pilish-test-navigate-confirmation-allows-same-owner-refresh ()
  "A same-file browser refresh during confirmation remains a valid owner."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((prompts 0))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt)
                   (cl-incf prompts)
                   (pilish-test--sync-timers
                     (lambda ()
                       (pilish--tree-browser-fetch-and-render)))
                   (should (equal pilish--tree-browser-loaded-file path))
                   t)))
        (pilish--browse-navigate "b1"))
      (should (= prompts 1))
      (should (equal (plist-get (pilish-jsonl-read-file path) :leafId)
                     "b1"))
      (should (equal resume-calls (list (list proc chat-buf path))))
      (should (equal (with-current-buffer input-buf (buffer-string)) "")))))

(ert-deftest pilish-test-navigate-keeps-draft-changed-during-rewrite ()
  "A newer draft exposed by yielding rewrite work is never erased.
Navigation still resumes the already rewritten path, but skips prefill
and reports that it retained the newer input."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((prompts 0)
          (rewrites 0))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (cl-incf prompts) t))
                ((symbol-function 'pilish--browse-rewrite-session-file)
                 (lambda (&rest _)
                   (cl-incf rewrites)
                   (with-current-buffer input-buf
                     (erase-buffer)
                     (insert "newer draft"))
                   t)))
        (pilish--browse-navigate "b1"))
      (should (= prompts 1))
      (should (= rewrites 1))
      (should (equal resume-calls (list (list proc chat-buf path))))
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "newer draft"))
      (should (member
               (concat
                "Pi: Continued from selected turn; kept newer input draft "
                "(prefill skipped)")
               messages)))))

(ert-deftest pilish-test-navigate-rewrites-and-switches ()
  "The full navigate atomically moves the chain last and switches.
The fresh disk input is adversarial CRLF JSONL with a structurally
malformed line containing byte FF.  Every original line and byte,
including every CR, survives the stable partition exactly; a final
CRLF remains.  The resume flow gets (PROC CHAT-BUF PATH), input is
prefilled, success is messaged, settle-wait is scheduled, and no temp
file remains."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let* ((malformed
            (concat (string-as-unibyte
                     "{\"type\":\"message\",\"id\":\"torn\",\"payload\":\"")
                    (unibyte-string #xff)))
           (lines (append (pilish-test--navigable-session-lines)
                          (list malformed)))
           (original (concat (mapconcat #'string-as-unibyte lines
                                         (string-as-unibyte "\r\n"))
                             (string-as-unibyte "\r\n")))
           (expected (pilish-test--navigate-rewritten-contents
                      lines "\r\n")))
      ;; Replace only the temp fixture, never real session data.  The
      ;; browser cache is already loaded; navigate must use this fresh
      ;; on-disk CRLF shape for both target and line-order reads.
      (let ((coding-system-for-write 'no-conversion))
        (write-region original nil path nil 0))
      (pilish--browse-navigate "u2")
      (should (equal (pilish-test--file-contents path t) expected))
      (should (equal resume-calls (list (list proc chat-buf path))))
      (should (equal quit-calls
                     (list (list chat-buf (selected-window) path))))
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "try the other way"))
      (should (member
               "Pi: Continued from selected turn: try the other way"
               messages))
      (should-not (directory-files (file-name-directory path)
                                   nil "\\.pi-nav-")))))

(ert-deftest pilish-test-navigate-historical-assistant-targets-itself ()
  "A historical assistant continues from that assistant entry.
Unlike a historical user, it neither rewinds to its parent nor carries
prefill; the existing nil-prefill behavior clears the input draft after
the atomic rewrite and resume are scheduled."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (pilish--browse-navigate "b1")
    (let ((session (pilish-jsonl-read-file path))
          (projected (pilish-jsonl-project-session-file path)))
      (should (equal (plist-get session :leafId) "b1"))
      (should (equal (plist-get projected :leafId) "b1")))
    (should (equal resume-calls (list (list proc chat-buf path))))
    (should (equal (with-current-buffer input-buf (buffer-string)) ""))
    (should (member
             "Pi: Continued from selected turn: abandoned branch"
             messages))))

(ert-deftest pilish-test-navigate-shape ()
  "The navigated file reads back in the navigated shape: read-file's
:leafId is the computed leaf (a1), and the projection's active path
from that leaf holds exactly the expected visible ids — u1 and a1,
not the off-branch b1 nor the rewound-under u2."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (pilish--browse-navigate "u2")
    (let* ((session (pilish-jsonl-read-file path))
           (result (pilish-jsonl-project-session-file path))
           (active (pilish--active-path-ids
                    (plist-get result :tree) (plist-get result :leafId))))
      (should (equal (plist-get session :leafId) "a1"))
      (should (equal (plist-get result :leafId) "a1"))
      (should (gethash "u1" active))
      (should (gethash "a1" active))
      (should-not (gethash "u2" active))
      (should-not (gethash "b1" active)))))

(ert-deftest pilish-test-navigate-atomic-failure ()
  "Errors and quits before commit leave the original byte-identical.
For both write-region and rename-file legs, `unwind-protect' removes
any .pi-nav- temp, the switch and prefill do not run, errors report a
navigate failure, and quits propagate.  The quitting write first
creates a partial temp file, proving cleanup rather than non-creation."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path))
          (dir (file-name-directory path)))
      ;; write-region failure: the temp file never lands.
      (cl-letf (((symbol-function 'write-region)
                 (lambda (&rest _)
                   (signal 'file-error '("write failed")))))
        (pilish--browse-navigate "u2"))
      (should (equal (pilish-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      (should (cl-some
               (lambda (m)
                 (string-match-p
                  "\\`Pi: Could not continue from selected turn: " m))
               messages))
      ;; rename-file failure: the temp file is removed again, never
      ;; swapped in.
      (setq messages nil)
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _)
                   (signal 'file-error '("rename failed")))))
        (pilish--browse-navigate "u2"))
      (should (equal (pilish-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      (should (cl-some
               (lambda (m)
                 (string-match-p
                  "\\`Pi: Could not continue from selected turn: " m))
               messages))
      ;; A quit after a partial temp write is not an `error', so it
      ;; propagates; the unwind still removes the file.
      (let ((real-write (symbol-function 'write-region)))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (_start _end filename &rest _)
                     (funcall real-write "partial" nil filename nil 0)
                     (signal 'quit nil))))
          (should (eq (condition-case nil
                          (progn
                            (pilish--browse-navigate "u2")
                            'returned)
                        (quit 'quit))
                      'quit))))
      (should (equal (pilish-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      ;; The rename leg also owns a completed temp file when quit
      ;; arrives; its unwind removes that file and leaves PATH alone.
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (signal 'quit nil))))
        (should (eq (condition-case nil
                        (progn
                          (pilish--browse-navigate "u2")
                          'returned)
                      (quit 'quit))
                    'quit)))
      (should (equal (pilish-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      ;; No failed or interrupted attempt touched the input buffer.
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft")))))

(ert-deftest pilish-test-navigate-preflight-cwd ()
  "A resume cwd failure re-signals its `user-error' BEFORE any write:
the file is untouched, no temp file appears, and no switch runs."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (cl-letf (((symbol-function
                  'pilish--session-file-cwd-or-error)
                 (lambda (&rest _)
                   (user-error
                    "Stored session cwd is not an existing directory"))))
        (should (equal (error-message-string
                        (should-error
                         (pilish--browse-navigate "u2")
                         :type 'user-error))
                       "Stored session cwd is not an existing directory")))
      (should (equal (pilish-test--file-contents path) before))
      (should-not (directory-files (file-name-directory path)
                                   nil "\\.pi-nav-"))
      (should-not resume-calls))))

(ert-deftest pilish-test-navigate-root-user-message ()
  "Navigating to the root user message has no parent to rewind to:
the fork hint fires, nothing is written, no switch runs."
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (pilish-test--file-contents path)))
      (pilish--browse-navigate "u1")
      (should (member
               (concat
                "Pi: Cannot continue from selected turn: it has no parent; "
                "fork it from the chat instead")
               messages))
      (should (equal (pilish-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest pilish-test-navigate-preserves-permissions ()
  "The rewrite carries the original file's modes onto the replacement
(best effort): a 0600 session is still 0600 after navigating, and the
full flow ran (a switch was scheduled onto the rewritten file)."
  (skip-unless (not (zerop (user-uid))))
  (pilish-test--with-navigate-fixture
      (pilish-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (set-file-modes path #o600)
    (pilish--browse-navigate "u2")
    (should resume-calls)
    (should (equal (file-modes path) #o600))))

(ert-deftest pilish-test-quit-when-settled-tree-window ()
  "--browse-poll-settled also dismisses TREE browser windows: the
window check accepts any pi browse buffer via `derived-mode-p', not
just the session browser (V14)."
  (let* ((chat-buf (generate-new-buffer " *test-settled-tree-chat*"))
         (win (selected-window))
         (orig-buf (window-buffer win))
         (browser-buf (generate-new-buffer " *test-settled-tree*"))
         (path "/tmp/target-session.jsonl")
         (quit-calls nil)
         (polls 0))
    (unwind-protect
        (progn
          (with-current-buffer browser-buf
            (pilish-tree-browser-mode))
          (set-window-buffer win browser-buf)
          (with-current-buffer chat-buf
            (setq pilish--state (list :session-file path)))
          ;; Settled onto the target after one busy poll: the window
          ;; shows a TREE browser and must be quit.
          (cl-letf (((symbol-function
                      'pilish--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pilish--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should (equal quit-calls (list (list nil win)))))
      (set-window-buffer win orig-buf)
      (kill-buffer browser-buf)
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf)))))

;;;; Flat-row folding

(defun pilish-test--browse-fold-row (value)
  "Return current fold-row metadata whose canonical VALUE matches."
  (cl-find value pilish--browse-fold-rows
           :key (lambda (row) (plist-get row :value))
           :test #'equal))

(defun pilish-test--browse-fold-row-start (value)
  "Return the current rendered row start for canonical VALUE."
  (oref (plist-get (pilish-test--browse-fold-row value) :section) start))

(ert-deftest pilish-test-tree-fold-promoted-extents-persist-and-prune ()
  "Tree folds follow filtered topology, survive search, and prune on publish."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "root" :type "message" :role "user" :preview "root"
            :children
            [(:id "middle" :parentId "root" :type "message"
              :role "assistant" :preview "middle"
              :children
              [(:id "target" :parentId "middle" :type "message"
                :role "user" :preview "needle target" :children [])])])
           (:id "other" :type "message" :role "user"
            :preview "other root" :children [])]
          pilish--tree-browser-leaf-id "target"
          pilish--tree-browser-filter 'user-only)
    (pilish--tree-browser-rerender)
    ;; user-only promotes TARGET through the hidden assistant directly
    ;; under ROOT, and ROOT's extent stops before the second root.
    (let ((root (pilish-test--browse-fold-row "root"))
          (target (pilish-test--browse-fold-row "target"))
          (other (pilish-test--browse-fold-row "other")))
      (should (eq (plist-get target :parent) root))
      (should (= (plist-get root :end)
                 (oref (plist-get other :section) start))))
    (goto-char (pilish-test--browse-fold-row-start "target"))
    (pilish-browse-goto-parent-row)
    (should (= (point) (pilish-test--browse-fold-row-start "root")))
    ;; The leaf rule folds its nearest containing ancestor and repairs point.
    (goto-char (pilish-test--browse-fold-row-start "target"))
    (pilish-browse-toggle-fold)
    (should (gethash "root" pilish--browse-fold-state))
    (should (= (point) (pilish-test--browse-fold-row-start "root")))
    (should (invisible-p (pilish-test--browse-fold-row-start "target")))
    ;; Search hides ROOT and promotes TARGET to a visible root.  ROOT's state
    ;; remains because it still belongs to the published source tree.
    (setq pilish--tree-browser-search-query "needle"
          pilish--tree-browser-search-tokens '("needle"))
    (pilish--tree-browser-rerender)
    (should (gethash "root" pilish--browse-fold-state))
    (let ((target (pilish-test--browse-fold-row "target")))
      (should-not (plist-get target :parent))
      (should-not (invisible-p (oref (plist-get target :section) start))))
    ;; Clearing search recomputes the old extent and reapplies the fold.
    (setq pilish--tree-browser-search-query nil
          pilish--tree-browser-search-tokens nil)
    (pilish--tree-browser-rerender)
    (should (invisible-p (pilish-test--browse-fold-row-start "target")))
    (should (= (point) (pilish-test--browse-fold-row-start "root")))
    ;; Replacing the published snapshot finally prunes ROOT.
    (setq pilish--tree-browser-tree
          [(:id "replacement" :type "message" :role "user"
            :preview "replacement" :children [])]
          pilish--tree-browser-leaf-id "replacement")
    (pilish--tree-browser-rerender)
    (should-not (gethash "root" pilish--browse-fold-state))))

(ert-deftest pilish-test-tree-fold-restoration-and-visible-section-motion ()
  "Refresh repairs hidden point, while native n/p skip folded rows."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "root" :type "message" :role "user" :preview "root"
            :children
            [(:id "child" :parentId "root" :type "message"
              :role "assistant" :preview "child" :children [])])
           (:id "other" :type "message" :role "user"
            :preview "other" :children [])]
          pilish--tree-browser-leaf-id "child"
          pilish--tree-browser-filter 'default)
    (pilish--tree-browser-rerender)
    ;; Arrange the adversarial refresh case directly: identity restoration
    ;; finds CHILD after the overlay pass, then must repair to ROOT without
    ;; calling `magit-section-show'.
    (goto-char (pilish-test--browse-fold-row-start "child"))
    (puthash "root" t pilish--browse-fold-state)
    (pilish--tree-browser-rerender)
    (should (= (point) (pilish-test--browse-fold-row-start "root")))
    (should (invisible-p (pilish-test--browse-fold-row-start "child")))
    ;; Keep the inherited Magit commands unchanged; their movement hook jumps
    ;; across the one flat invisible extent in either direction.
    (let ((this-command 'magit-section-forward))
      (magit-section-forward))
    (should (= (point) (pilish-test--browse-fold-row-start "other")))
    (let ((this-command 'magit-section-backward))
      (magit-section-backward))
    (should (= (point) (pilish-test--browse-fold-row-start "root")))))

(ert-deftest pilish-test-session-fold-hidden-actions-refused ()
  "A hidden row cannot be deleted, renamed, or switched before redisplay."
  (let* ((directory (pilish-test--make-temp-directory "pilish-fold-actions-"))
         (parent (expand-file-name "parent.jsonl" directory))
         (child (expand-file-name "child.jsonl" directory)))
    (unwind-protect
        (progn
          (dolist (path (list parent child))
            (pilish-test--write-session-lines
             path (list (pilish-test--make-session-header "session")
                        (pilish-test--user-line "user" nil "Question"))))
          (with-temp-buffer
            (pilish-session-browser-mode)
            (setq pilish--session-browser-items
                  (list (list :path parent :name "Parent")
                        (list :path child :name "Child" :parentSessionPath parent))
                  pilish--session-browser-view 'threaded
                  pilish--session-browser-scope 'all)
            (pilish--session-browser-rerender)
            (goto-char (pilish-test--browse-fold-row-start parent))
            (pilish-browse-toggle-fold)
            (let* ((hidden (pilish-test--browse-fold-row-start child))
                   (section (plist-get (pilish-test--browse-fold-row child) :section))
                   (before (pilish-test--file-contents child))
                   prompts switches messages)
              (should (invisible-p hidden))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (prompt) (push prompt prompts) t))
                        ((symbol-function 'read-string)
                         (lambda (prompt &rest _) (push prompt prompts) "Renamed"))
                        ((symbol-function 'pilish--browse-switch-session)
                         (lambda (path) (push path switches)))
                        ((symbol-function 'pilish--session-browser-fetch-and-render)
                         #'ignore)
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                ;; The command boundary must work even when redisplay has
                ;; not repaired point, or Magit's context target is hidden.
                (dolist (context '(point current stale))
                  (let ((magit--context-menu-section
                         (unless (eq context 'point) section)))
                    (when (eq context 'stale)
                      (pilish--session-browser-rerender)
                      (should (invisible-p
                               (pilish-test--browse-fold-row-start child))))
                    (dolist (command '(pilish-session-browser-delete
                                       pilish-session-browser-rename
                                       pilish-session-browser-switch))
                      (goto-char (if (eq context 'point) hidden (point-max)))
                      (funcall command)
                      (should (file-exists-p child))
                      (should (equal (pilish-test--file-contents child) before))
                      (should-not prompts)
                      (should-not switches))))
                (should (= (length messages) 9))
                (should (cl-every (lambda (text)
                                    (equal text "Pi: No session at point"))
                                  messages))))))
      (delete-directory directory t))))

(ert-deftest pilish-test-tree-fold-hidden-actions-refused ()
  "Hidden tree nodes cannot be labeled or selected for continuation."
  (let* ((path (make-temp-file "pilish-fold-tree-actions-" nil ".jsonl"))
         (chat (generate-new-buffer " *pilish-fold-tree-chat*")))
    (unwind-protect
        (progn
          (pilish-test--write-session-lines
           path (list (pilish-test--make-session-header "session")
                      (pilish-test--user-line "root" nil "Root")
                      (pilish-test--user-line "child" "root" "Child")))
          (with-current-buffer chat
            (setq pilish--state (list :session-file path)))
          (with-temp-buffer
            (pilish-tree-browser-mode)
            (setq pilish--chat-buffer chat
                  pilish--tree-browser-loaded-file path
                  pilish--tree-browser-tree
                  [(:id "root" :type "message" :role "user" :preview "Root"
                    :children [(:id "child" :type "message" :role "user"
                                :preview "Child" :children [])])]
                  pilish--tree-browser-filter 'default)
            (pilish--tree-browser-rerender)
            (goto-char (pilish-test--browse-fold-row-start "root"))
            (pilish-browse-toggle-fold)
            (let* ((hidden (pilish-test--browse-fold-row-start "child"))
                   (section (plist-get (pilish-test--browse-fold-row "child")
                                       :section))
                   (before (pilish-test--file-contents path))
                   prompts navigations messages)
              (should (invisible-p hidden))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (prompt &rest _) (push prompt prompts) "Label"))
                        ((symbol-function 'pilish--browse-navigate)
                         (lambda (id) (push id navigations)))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (dolist (context '(point current stale))
                  (let ((magit--context-menu-section
                         (unless (eq context 'point) section)))
                    (when (eq context 'stale)
                      (pilish--tree-browser-rerender)
                      (should (invisible-p
                               (pilish-test--browse-fold-row-start "child"))))
                    (goto-char (if (eq context 'point) hidden (point-max)))
                    (pilish-tree-browser-set-label)
                    (should (equal (pilish-test--file-contents path) before))
                    (should-not prompts)
                    (pilish-tree-browser-navigate)
                    (should-not navigations)))
                (should (member "Pi: No tree node at point" messages))))))
      (kill-buffer chat)
      (delete-file path nil))))

(ert-deftest pilish-test-tree-fold-ambiguous-row-is-never-target ()
  "A canonical ambiguous-id display row has no fold action or state."
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree
          [(:id "dup" :ambiguousId t :type "message" :role "user"
            :preview "ambiguous" :children
            [(:id "child" :parentId "dup" :ambiguousParent t
              :type "message" :role "assistant" :preview "child"
              :children [])])]
          pilish--tree-browser-leaf-id "dup"
          pilish--tree-browser-filter 'default)
    (pilish--tree-browser-rerender)
    (let ((row (car pilish--browse-fold-rows)))
      (should (equal (plist-get row :value) '(ambiguous-id . "dup")))
      (should-not (plist-get row :foldable))
      (goto-char (oref (plist-get row :section) start))
      (should-error (pilish-browse-toggle-fold) :type 'user-error)
      (should-not (gethash "dup" pilish--browse-fold-state))
      (should-not (get-text-property
                   (point) 'pilish-browse-fold-indicator)))))

(ert-deftest pilish-test-session-fold-threaded-query-round-trip-and-parent ()
  "Threaded family folds persist through flat query results; ^ finds root."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (setq pilish--session-browser-items
          '((:path "/tmp/pilish-fold-parent.jsonl" :name "Parent"
             :modified "2026-01-01T00:00:00Z")
            (:path "/tmp/pilish-fold-child.jsonl" :name "Needle child"
             :parentSessionPath "/tmp/pilish-fold-parent.jsonl"
             :modified "2026-01-02T00:00:00Z"))
          pilish--session-browser-view 'threaded
          pilish--session-browser-scope 'all)
    (pilish--session-browser-rerender)
    (let ((parent (pilish-test--browse-fold-row
                   "/tmp/pilish-fold-parent.jsonl"))
          (child (pilish-test--browse-fold-row
                  "/tmp/pilish-fold-child.jsonl")))
      (should (eq (plist-get child :parent) parent))
      (goto-char (oref (plist-get child :section) start))
      (pilish-browse-goto-parent-row)
      (should (= (point) (oref (plist-get parent :section) start)))
      (goto-char (oref (plist-get child :section) start))
      (pilish-browse-toggle-fold))
    (should (gethash "/tmp/pilish-fold-parent.jsonl"
                     pilish--browse-fold-state))
    ;; In All-projects layout the fixed project/live fields precede the
    ;; connector; indicator updates still target the marked glyph itself.
    (let* ((row (pilish-test--browse-fold-row
                 "/tmp/pilish-fold-parent.jsonl"))
           (section (plist-get row :section))
           (indicator (+ (oref section start)
                         (plist-get row :indicator-offset))))
      (should (equal (get-text-property
                      indicator 'pilish-browse-fold-indicator)
                     "/tmp/pilish-fold-parent.jsonl"))
      (should (= (char-after indicator) (string-to-char "▸"))))
    (should (invisible-p
             (pilish-test--browse-fold-row-start
              "/tmp/pilish-fold-child.jsonl")))
    ;; A query deliberately flattens Threaded.  It removes the parent row but
    ;; not its published identity, so clearing it restores the same fold.
    (setq pilish--session-browser-search-query "Needle"
          pilish--session-browser-search-tokens '("Needle"))
    (pilish--session-browser-rerender)
    (should (gethash "/tmp/pilish-fold-parent.jsonl"
                     pilish--browse-fold-state))
    (should-not (invisible-p
                 (pilish-test--browse-fold-row-start
                  "/tmp/pilish-fold-child.jsonl")))
    (setq pilish--session-browser-search-query nil
          pilish--session-browser-search-tokens nil)
    (pilish--session-browser-rerender)
    (should (invisible-p
             (pilish-test--browse-fold-row-start
              "/tmp/pilish-fold-child.jsonl")))))

(ert-deftest pilish-test-session-fold-recent-group-and-flat-exclusion ()
  "Recent rows fold their group; Most-messages rows have no false target."
  (let ((now (encode-time '(0 0 12 18 9 2026 nil nil nil))))
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (with-temp-buffer
        (pilish-session-browser-mode)
        (setq pilish--session-browser-items
              '((:path "/tmp/pilish-recent-a.jsonl" :name "A"
                 :modified "2026-09-18T10:00:00+02:00")
                (:path "/tmp/pilish-recent-b.jsonl" :name "B"
                 :modified "2026-09-18T09:00:00+02:00")
                (:path "/tmp/pilish-recent-old.jsonl" :name "Old"
                 :modified "2026-09-17T09:00:00+02:00"))
              pilish--session-browser-view 'recent)
        (pilish--session-browser-rerender)
        (let ((session (pilish-test--browse-fold-row
                        "/tmp/pilish-recent-a.jsonl"))
              (today (pilish-test--browse-fold-row "Today"))
              (yesterday (pilish-test--browse-fold-row "Yesterday")))
          (should (= (plist-get today :end)
                     (oref (plist-get yesterday :section) start)))
          (goto-char (oref (plist-get session :section) start))
          (pilish-browse-goto-parent-row)
          (should (equal (plist-get (pilish--browse-current-fold-row) :value)
                         "Today"))
          (goto-char (oref (plist-get session :section) start))
          (pilish-browse-toggle-fold))
        (should (= (point) (pilish-test--browse-fold-row-start "Today")))
        (should (invisible-p
                 (pilish-test--browse-fold-row-start
                  "/tmp/pilish-recent-a.jsonl")))
        (should-not (invisible-p
                     (pilish-test--browse-fold-row-start "Yesterday")))
        (should-not (invisible-p
                     (pilish-test--browse-fold-row-start
                      "/tmp/pilish-recent-old.jsonl")))
        (setq pilish--session-browser-view 'messages)
        (pilish--session-browser-rerender)
        (goto-char (point-min))
        (should-error (pilish-browse-toggle-fold) :type 'user-error)))))

(ert-deftest pilish-test-fold-deep-and-broad-extents-stack-safe ()
  "Deep and broad flat extents are structurally complete and stack safe."
  ;; A 501-node chain has one extent per non-leaf and fold-all needs only
  ;; one maximal overlay, avoiding both recursive Magit sections and an
  ;; overlap stack.
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (setq pilish--tree-browser-tree (pilish-test--make-deep-tree 501)
          pilish--tree-browser-leaf-id "node-501"
          pilish--tree-browser-filter 'default)
    (pilish--tree-browser-rerender)
    (should (= (length pilish--browse-fold-rows) 501))
    (should (= (cl-count-if
                (lambda (row)
                  (and (plist-get row :body-start)
                       (plist-get row :end)))
                pilish--browse-fold-rows)
               500))
    (pilish-browse-fold-all)
    (should (= (length pilish--browse-fold-overlays) 1))
    (let ((root-row (pilish-test--browse-fold-row "node-1")))
      (should (= (char-after
                  (+ (oref (plist-get root-row :section) start)
                     (plist-get root-row :indicator-offset)))
                 (string-to-char "▸"))))
    (should (invisible-p (pilish-test--browse-fold-row-start "node-501")))
    (pilish-browse-fold-all t)
    (should-not pilish--browse-fold-overlays)
    (should-not (invisible-p
                 (pilish-test--browse-fold-row-start "node-501"))))
  ;; Thirty roots with nine children each exercise 300 rows and 30 disjoint
  ;; family-sized extents without timing assertions.
  (with-temp-buffer
    (pilish-tree-browser-mode)
    (let (roots)
      (dotimes (root-index 30)
        (let (children)
          (dotimes (child-index 9)
            (push (list :id (format "r%d-c%d" root-index child-index)
                        :parentId (format "r%d" root-index)
                        :type "message" :role "assistant"
                        :preview "child" :children [])
                  children))
          (push (list :id (format "r%d" root-index)
                      :type "message" :role "user" :preview "root"
                      :children (vconcat (nreverse children)))
                roots)))
      (setq pilish--tree-browser-tree (vconcat (nreverse roots))
            pilish--tree-browser-leaf-id "r0-c0"
            pilish--tree-browser-filter 'default))
    (pilish--tree-browser-rerender)
    (should (= (length pilish--browse-fold-rows) 300))
    (should (= (cl-count-if
                (lambda (row)
                  (and (plist-get row :body-start)
                       (plist-get row :end)))
                pilish--browse-fold-rows)
               30))
    (pilish-browse-fold-all)
    (should (= (length pilish--browse-fold-overlays) 30))
    (dotimes (root-index 30)
      (should-not (invisible-p
                   (pilish-test--browse-fold-row-start
                    (format "r%d" root-index))))
      (should (invisible-p
               (pilish-test--browse-fold-row-start
                (format "r%d-c0" root-index)))))))

(ert-deftest pilish-test-session-fold-deep-threaded-family-stack-safe ()
  "A 600-session fork chain renders and folds without Lisp recursion."
  (with-temp-buffer
    (pilish-session-browser-mode)
    (let (items)
      (dotimes (index 600)
        (let ((path (format "/test/deep-thread-%03d.jsonl" index))
              (parent (and (> index 0)
                           (format "/test/deep-thread-%03d.jsonl"
                                   (1- index)))))
          (push (append
                 (list :path path :canonicalPath path
                       :name (format "Thread %d" index)
                       :modified "2026-01-01T00:00:00Z")
                 (and parent
                      (list :parentSessionPath parent
                            :canonicalParentSession parent)))
                items)))
      (setq pilish--session-browser-items (nreverse items)
            pilish--session-browser-view 'threaded))
    (pilish--session-browser-rerender)
    (should (= (length pilish--browse-fold-rows) 600))
    (should (= (cl-count-if
                (lambda (row)
                  (and (plist-get row :body-start)
                       (plist-get row :end)))
                pilish--browse-fold-rows)
               599))
    (pilish-browse-fold-all)
    (should (= (length pilish--browse-fold-overlays) 1))
    (should (invisible-p
             (pilish-test--browse-fold-row-start
              "/test/deep-thread-599.jsonl")))))

(ert-deftest pilish-test-browse-fold-keymaps-remove-false-affordances ()
  "Browser maps expose flat folds and remove recursive Magit controls."
  (dolist (map (list pilish-browse-mode-map
                     pilish-session-browser-mode-map
                     pilish-tree-browser-mode-map))
    (should (eq (lookup-key map (kbd "TAB"))
                #'pilish-browse-toggle-fold))
    (should (eq (lookup-key map [tab])
                #'pilish-browse-toggle-fold))
    (should (eq (lookup-key map (kbd "<backtab>"))
                #'pilish-browse-fold-all))
    (should (eq (lookup-key map (kbd "^"))
                #'pilish-browse-goto-parent-row))
    (should (eq (lookup-key map (kbd "n")) #'magit-section-forward))
    (should (eq (lookup-key map (kbd "p")) #'magit-section-backward))
    (dolist (key '("C-c TAB" "C-<tab>" "M-<tab>"
                   "1" "2" "3" "4" "M-1" "M-2" "M-3" "M-4"
                   "<left-fringe> <mouse-1>"
                   "<left-fringe> <mouse-2>"))
      (should-not (lookup-key map (kbd key))))))

(provide 'pilish-browse-test)
;;; pilish-browse-test.el ends here
