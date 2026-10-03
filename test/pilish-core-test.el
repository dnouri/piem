;;; pilish-core-test.el --- Tests for pilish-core -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the core Pilish functionality: JSON parsing,
;; line accumulation, and command encoding.

;;; Code:

(require 'ert)
(require 'pilish-core)
(require 'pilish-test-common)

;;;; Raw stdout observation

(ert-deftest pilish-test-inactivity-filter-counts-raw-chunks-before-dispatch ()
  "Every nonempty chunk is observed before parsing or response callbacks."
  (let ((proc (start-process "pilish-stdout-clock" nil "cat")))
    (unwind-protect
        (pilish-test-with-clock now
          (set-process-query-on-exit-flag proc nil)
          (let ((scheduled (copy-sequence timer-list)))
            (let ((frame (symbol-function 'pilish--accumulate-line-chunks)))
              (cl-letf (((symbol-function 'pilish--accumulate-line-chunks)
                         (lambda (partial output)
                           (should (equal (process-get proc 'pilish-last-output-time) now))
                           (funcall frame partial output))))
                (dolist (chunk '(" \n" "not JSON\n" "{\"type\":\"response\""
                                 ",\"success\":true}\n" "{\"type\":\"future_event\"}\n"))
                  (setq now (+ now 17))
                  (pilish--process-filter proc chunk)
                  (should (equal (process-get proc 'pilish-last-output-time) now)))))
            (let ((before (process-get proc 'pilish-last-output-time)))
              (setq now (+ now 17))
              (pilish--process-filter proc "")
              (should (equal (process-get proc 'pilish-last-output-time) before)))
            (let (seen)
              (puthash "clock-response"
                       (lambda (_response)
                         (push (process-get proc 'pilish-last-output-time) seen))
                       (pilish--get-pending-requests proc))
              (setq now (+ now 17))
              (pilish-test--stdout
               proc '(:type "response" :id "clock-response" :success t))
              (should (equal seen (list now))))
            (should (equal scheduled timer-list))))
      (delete-process proc))))

(ert-deftest pilish-test-inactivity-filter-is-process-local-before-adoption ()
  "Unadopted output records its own clock, never another process's clock."
  (let ((a (start-process "pilish-clock-a" nil "cat"))
        (b (start-process "pilish-clock-b" nil "cat")))
    (unwind-protect
        (pilish-test-with-clock now
          (let ((scheduled (copy-sequence timer-list)))
            (pilish--process-filter a "partial")
            (setq now 1100.0)
            (pilish--process-filter b "\n")
            (should (equal (process-get a 'pilish-last-output-time) 1000.0))
            (should (equal (process-get b 'pilish-last-output-time) 1100.0))
            (should (equal scheduled timer-list))))
      (delete-process a)
      (delete-process b))))

;;;; JSON Parsing Tests

(ert-deftest pilish-test-parse-json-response ()
  "Parse a valid JSON response."
  (let ((result (pilish--parse-json-line "{\"type\":\"response\",\"id\":\"req_1\",\"success\":true}")))
    (should (equal (plist-get result :type) "response"))
    (should (equal (plist-get result :id) "req_1"))
    (should (eq (plist-get result :success) t))))

(ert-deftest pilish-test-parse-json-event ()
  "Parse a valid JSON event (no id field)."
  (let ((result (pilish--parse-json-line "{\"type\":\"agent_start\"}")))
    (should (equal (plist-get result :type) "agent_start"))
    (should (null (plist-get result :id)))))

(ert-deftest pilish-test-parse-json-with-nested-data ()
  "Parse JSON with nested objects."
  (let ((result (pilish--parse-json-line "{\"type\":\"response\",\"data\":{\"model\":\"claude\",\"count\":42}}")))
    (should (equal (plist-get result :type) "response"))
    (let ((data (plist-get result :data)))
      (should (equal (plist-get data :model) "claude"))
      (should (equal (plist-get data :count) 42)))))

(ert-deftest pilish-test-parse-json-malformed ()
  "Malformed JSON returns nil, not an error."
  (should (null (pilish--parse-json-line "not valid json")))
  (should (null (pilish--parse-json-line "")))
  (should (null (pilish--parse-json-line "{")))
  (should (null (pilish--parse-json-line "{\"unterminated"))))

(ert-deftest pilish-test-parse-json-boolean-false ()
  "JSON false parses to :false, not nil."
  (let ((result (pilish--parse-json-line "{\"isStreaming\":false}")))
    (should (eq (plist-get result :isStreaming) :false))))

(ert-deftest pilish-test-parse-json-null ()
  "JSON null parses to :null, not nil."
  (let ((result (pilish--parse-json-line "{\"result\":null}")))
    (should (eq (plist-get result :result) :null))))

(ert-deftest pilish-test-json-false-p-accepts-both-sentinels ()
  "JSON false helper accepts both parser and encoder sentinels."
  (should (pilish--json-false-p :false))
  (should (pilish--json-false-p :json-false))
  (should-not (pilish--json-false-p nil))
  (should-not (pilish--json-false-p t)))

(ert-deftest pilish-test-normalize-string-or-null-preserves-nonempty-strings ()
  "String normalization preserves ordinary and whitespace-only strings."
  (dolist (value '("ordinary text" " \t\n"))
    (should (equal (pilish--normalize-string-or-null value)
                   value))))

(ert-deftest pilish-test-normalize-string-or-null-rejects-empty-and-nonstrings ()
  "String normalization maps empty, null, false, and non-strings to nil."
  (let ((values (list "" nil :null :false :json-false
                      17 2.5 ["vector"] '(:key "value"))))
    (should (equal (mapcar #'pilish--normalize-string-or-null values)
                   (make-list (length values) nil)))))

(ert-deftest pilish-test-parse-json-unicode ()
  "Unicode content is preserved correctly."
  (let ((result (pilish--parse-json-line "{\"msg\":\"Hello 世界 🌍\"}")))
    (should (equal (plist-get result :msg) "Hello 世界 🌍"))))

;;;; Line Accumulation Tests

(ert-deftest pilish-test-accumulate-complete-line ()
  "A complete line (ending with newline) is extracted."
  (let ((result (pilish--accumulate-lines "" "foo\n")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) ""))))

(ert-deftest pilish-test-accumulate-lines-treats-nil-as-empty ()
  "A nil accumulated fragment is treated as no previous input."
  (let ((result (pilish--accumulate-lines nil "foo\nbar")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) "bar"))))

(ert-deftest pilish-test-accumulate-partial-line ()
  "A partial line (no newline) is saved as remainder."
  (let ((result (pilish--accumulate-lines "" "foo")))
    (should (null (car result)))
    (should (equal (cdr result) "foo"))))

(ert-deftest pilish-test-accumulate-multiple-lines ()
  "Multiple complete lines are all extracted."
  (let ((result (pilish--accumulate-lines "" "foo\nbar\nbaz\n")))
    (should (equal (car result) '("foo" "bar" "baz")))
    (should (equal (cdr result) ""))))

(ert-deftest pilish-test-accumulate-partial-then-complete ()
  "Partial line followed by completion works correctly."
  (let* ((result1 (pilish--accumulate-lines "" "fo"))
         (result2 (pilish--accumulate-lines (cdr result1) "o\nbar\n")))
    (should (null (car result1)))
    (should (equal (cdr result1) "fo"))
    (should (equal (car result2) '("foo" "bar")))
    (should (equal (cdr result2) ""))))

(ert-deftest pilish-test-accumulate-mixed-complete-and-partial ()
  "Complete lines extracted, partial line saved."
  (let ((result (pilish--accumulate-lines "" "foo\nbar")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) "bar"))))

(ert-deftest pilish-test-accumulate-with-existing-remainder ()
  "Existing remainder is prepended to new chunk."
  (let ((result (pilish--accumulate-lines "hel" "lo\nworld\n")))
    (should (equal (car result) '("hello" "world")))
    (should (equal (cdr result) ""))))

(ert-deftest pilish-test-accumulate-line-chunks-materializes-on-newline ()
  "Chunk accumulation keeps partial lines chunked until a newline arrives."
  (let* ((result1 (pilish--accumulate-line-chunks nil "hel"))
         (result2 (pilish--accumulate-line-chunks (cdr result1) "lo\nworld"))
         (result3 (pilish--accumulate-line-chunks (cdr result2) "\n")))
    (should (null (car result1)))
    (should (equal (cdr result1) '("hel")))
    (should (equal (car result2) '("hello")))
    (should (equal (cdr result2) '("world")))
    (should (equal (car result3) '("world")))
    (should (null (cdr result3)))))

;;;; JSON Encoding Tests

(ert-deftest pilish-test-encode-simple-command ()
  "Encode a simple command to JSON."
  (let ((result (pilish--encode-command '(:type "prompt" :message "hello"))))
    (should (string-suffix-p "\n" result))
    (let ((parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
      (should (equal (plist-get parsed :type) "prompt"))
      (should (equal (plist-get parsed :message) "hello")))))

(ert-deftest pilish-test-encode-command-with-id ()
  "Encoded command includes id field."
  (let* ((result (pilish--encode-command '(:type "get_state" :id "req_1")))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "get_state"))
    (should (equal (plist-get parsed :id) "req_1"))))

(ert-deftest pilish-test-encode-nested-command ()
  "Encode command with nested data."
  (let* ((result (pilish--encode-command '(:type "set_model" :data (:provider "anthropic" :model "claude"))))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "set_model"))
    (let ((data (plist-get parsed :data)))
      (should (equal (plist-get data :provider) "anthropic")))))

(ert-deftest pilish-test-encode-command-with-array ()
  "Encode command with array values."
  (let* ((result (pilish--encode-command '(:type "prompt" :attachments [])))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :attachments) []))))

;;;; Path Boundary Helper Tests

(ert-deftest pilish-test-remote-prefix-detects-tramp-anchor ()
  "Remote prefix helper returns the TRAMP prefix without connecting."
  (let ((default-directory "/ssh:pi-host:/home/pi/project/"))
    (should (equal (pilish--remote-prefix)
                   "/ssh:pi-host:"))
    (should (equal (pilish--remote-prefix
                    "/ssh:other:/srv/project/")
                   "/ssh:other:"))
    (should (equal (pilish--remote-prefix
                    "/ssh:bastion|sudo:root@pi-host:/srv/project/")
                   "/ssh:bastion|sudo:root@pi-host:"))
    (should-not (pilish--remote-prefix "/tmp/project/"))))

(ert-deftest pilish-test-emacs-path-normalizes-remote-inbound-paths ()
  "Inbound process-local paths become Emacs/TRAMP paths at the boundary."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should (equal (pilish--emacs-path "/var/pi/session.jsonl" anchor)
                   "/ssh:pi-host:/var/pi/session.jsonl"))
    (should (equal (pilish--emacs-path "~/pi/session.jsonl" anchor)
                   "/ssh:pi-host:~/pi/session.jsonl"))
    (should (equal (pilish--emacs-path "~root/pi/session.jsonl" anchor)
                   "/ssh:pi-host:~root/pi/session.jsonl"))
    (should (equal (pilish--emacs-path "sessions/current.jsonl" anchor)
                   "/ssh:pi-host:/home/pi/project/sessions/current.jsonl"))
    (should (equal (pilish--emacs-path
                    "/ssh:pi-host:/tmp/remote.jsonl" anchor)
                   "/ssh:pi-host:/tmp/remote.jsonl"))
    (should-not (pilish--emacs-path "" anchor))))

(ert-deftest pilish-test-emacs-path-preserves-multi-hop-route ()
  "Inbound process-local paths keep the full multi-hop TRAMP anchor."
  (let ((anchor "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (should (equal (pilish--emacs-path
                    "sessions/current.jsonl" anchor)
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/sessions/current.jsonl"))
    (should (equal (pilish--emacs-path
                    "/var/pi/session.jsonl" anchor)
                   "/ssh:bastion|sudo:root@pi-host:/var/pi/session.jsonl"))))

(ert-deftest pilish-test-emacs-path-rejects-incompatible-tramp-inbound-paths ()
  "Inbound TRAMP paths must match the full session anchor route."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should-error (pilish--emacs-path
                   "/ssh:other:/tmp/remote.jsonl" anchor)
                  :type 'user-error)
    (should-error (pilish--emacs-path
                   "/ssh:pi-host:/tmp/remote.jsonl" "/tmp/project/")
                  :type 'user-error))
  (let ((anchor "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (should (equal (pilish--emacs-path "/etc/hosts" anchor)
                   "/ssh:bastion|sudo:root@pi-host:/etc/hosts"))
    (should-error (pilish--emacs-path
                   "/sudo:root@pi-host:/etc/hosts" anchor)
                  :type 'user-error)))

(ert-deftest pilish-test-path-helpers-reject-nul-strings ()
  "NUL-containing path strings are not usable file names."
  (let ((anchor "/ssh:pi-host:/home/pi/project/")
        (bad (concat "/tmp/bad" (string ?\0) "name.el")))
    (let ((err (should-error (pilish--emacs-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))
    (let ((err (should-error (pilish--process-local-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))
    (let ((err (should-error (pilish--shell-command-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))
    (let ((err (should-error (pilish--shell-quote-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))))

(ert-deftest pilish-test-passive-emacs-path-ignores-unsafe-metadata ()
  "Passive backend path metadata returns nil instead of signaling."
  (let ((anchor "/ssh:pi-host:/home/pi/project/")
        (bad (concat "/tmp/bad" (string ?\0) "name.el")))
    (should-not (pilish--passive-emacs-path bad anchor))
    (should-not (pilish--passive-emacs-path
                 "/ssh:other:/tmp/name.el" anchor))
    (should (equal (pilish--passive-emacs-path
                    "prompts/fix.md" anchor)
                   "/ssh:pi-host:/home/pi/project/prompts/fix.md"))))

(ert-deftest pilish-test-emacs-directory-normalizes-remote-inbound-directory ()
  "Inbound directories are normalized to Emacs paths with trailing slash."
  (should (equal (pilish--emacs-directory
                  "/home/pi/project" "/ssh:pi-host:/tmp/session.jsonl")
                 "/ssh:pi-host:/home/pi/project/")))

(ert-deftest pilish-test-emacs-directory-preserves-multi-hop-route ()
  "Directory normalization keeps full multi-hop TRAMP prefixes."
  (should (equal (pilish--emacs-directory
                  "/home/pi/project"
                  "/ssh:bastion|sudo:root@pi-host:/tmp/session.jsonl")
                 "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")))

(ert-deftest pilish-test-process-local-path-strips-tramp-outbound-paths ()
  "Outbound paths are converted into process-local paths for Pi."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should (equal (pilish--process-local-path
                    "/ssh:pi-host:/var/pi/session.jsonl" anchor)
                   "/var/pi/session.jsonl"))
    (should (equal (pilish--process-local-path
                    "/ssh:bastion|sudo:root@pi-host:/var/pi/session.jsonl"
                    "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
                   "/var/pi/session.jsonl"))
    (should (equal (pilish--process-local-path
                    "/var/pi/session.jsonl" anchor)
                   "/var/pi/session.jsonl"))
    (should (equal (pilish--process-local-path
                    "sessions/current.jsonl" anchor)
                   "/home/pi/project/sessions/current.jsonl"))
    (should (equal (pilish--process-local-path
                    "~/pi/session.jsonl" anchor)
                   "~/pi/session.jsonl"))
    (should-error (pilish--process-local-path
                   "~root/pi/session.jsonl" anchor)
                  :type 'user-error)
    (should-not (pilish--process-local-path "" anchor))))

(ert-deftest pilish-test-process-local-path-rejects-other-remote ()
  "Outbound remote paths must stay on the session remote."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should-error (pilish--process-local-path
                   "/ssh:other:/var/pi/session.jsonl" anchor)
                  :type 'user-error)
    (should-error (pilish--process-local-path
                   "/scp:pi-host:/var/pi/session.jsonl" anchor)
                  :type 'user-error)))

(ert-deftest pilish-test-process-local-path-rejects-remote-for-local-anchor ()
  "Outbound TRAMP paths are not stripped for a local Pi process."
  (should-error (pilish--process-local-path
                 "/ssh:pi-host:/var/pi/session.jsonl" "/tmp/project/")
                :type 'user-error))

(ert-deftest pilish-test-ensure-compatible-remote-path-rejects-local-anchor ()
  "Remote compatibility helper rejects TRAMP paths for local anchors."
  (should-not (pilish--ensure-compatible-remote-path
               "/tmp/session.jsonl" "/ssh:pi-host:/home/pi/project/"))
  (should-error (pilish--ensure-compatible-remote-path
                 "/ssh:pi-host:/tmp/session.jsonl" "/tmp/project/")
                :type 'user-error)
  (should-not (pilish--ensure-compatible-remote-path
               "/ssh:pi-host:/tmp/session.jsonl"
               "/ssh:pi-host:/home/pi/project/")))

(ert-deftest pilish-test-process-local-path-preserves-remote-home-paths ()
  "Remote outbound home paths stay process-local without guessed expansion."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should (equal (pilish--process-local-path "~" anchor) "~"))
    (should (equal (pilish--process-local-path "~/out.html" anchor)
                   "~/out.html"))
    (should-error (pilish--process-local-path "~root/out.html" anchor)
                  :type 'user-error)
    (should-error (pilish--process-local-path
                   "/ssh:pi-host:~root/out.html" anchor)
                  :type 'user-error)))

(ert-deftest pilish-test-process-local-path-preserves-home-without-tramp-handler ()
  "Remote outbound home paths never expand against Emacs's local home."
  (let ((anchor "/ssh:pi-host:/home/pi/project/")
        (file-name-handler-alist nil))
    (should (equal (pilish--process-local-path "~/out.html" anchor)
                   "~/out.html"))
    (should (equal (pilish--process-local-path
                    "/ssh:pi-host:/tmp/out.html" anchor)
                   "/tmp/out.html"))))

(ert-deftest pilish-test-process-local-path-expands-local-home-paths ()
  "Local outbound home paths are expanded before Pi receives JSON."
  (should (equal (pilish--process-local-path
                  "~/out.html" "/tmp/pi-project/")
                 (expand-file-name "~/out.html"))))

(ert-deftest pilish-test-process-local-path-expands-local-relative-paths ()
  "Local outbound relative paths expand under the local anchor."
  (should (equal (pilish--process-local-path
                  "sessions/current.jsonl" "/tmp/pi-project/")
                 "/tmp/pi-project/sessions/current.jsonl")))

(ert-deftest pilish-test-shell-command-path-normalizes-for-emacs-shell-host ()
  "Shell paths are local to the host where Emacs starts `shell-command'."
  (let ((home-path (expand-file-name "~/a b.el")))
    (dolist (case
             `(("src/a b.el" "/tmp/pi project/"
                "/tmp/pi project/src/a b.el")
               ("/tmp/a b.el" "/tmp/pi project/" "/tmp/a b.el")
               ("~/a b.el" "/tmp/pi project/" ,home-path)
               ("/:/tmp/a b.el" "/tmp/pi project/" "/tmp/a b.el")
               ("/:relative" "/tmp/pi project/"
                "/tmp/pi project/relative")
               ("src/a b.el" "/ssh:pi-host:/home/pi/project/"
                "/home/pi/project/src/a b.el")
               ("/ssh:pi-host:/tmp/a b.el"
                "/ssh:pi-host:/home/pi/project/" "/tmp/a b.el")
               ("~/a b.el" "/ssh:pi-host:/home/pi/project/" "~/a b.el")
               ("~root/a b.el" "/ssh:pi-host:/home/pi/project/"
                "~root/a b.el")
               ("/ssh:pi-host:/:/tmp/a b.el"
                "/ssh:pi-host:/home/pi/project/" "/tmp/a b.el")
               ("/ssh:bastion|sudo:root@pi-host:/tmp/a b.el"
                "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"
                "/tmp/a b.el")
               ("/ssh:bastion|sudo:root@pi-host:~daemon/a b.el"
                "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"
                "~daemon/a b.el")
               ("/ssh:bastion|sudo:root@pi-host:/:/tmp/a b.el"
                "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"
                "/tmp/a b.el")))
      (should (equal (pilish--shell-command-path
                      (nth 0 case) (nth 1 case))
                     (nth 2 case))))))

(ert-deftest pilish-test-shell-command-path-rejects-incompatible-remotes ()
  "Shell conversion never strips a different TRAMP route."
  (should-error
   (pilish--shell-command-path
    "/ssh:other:/tmp/a.el" "/ssh:pi-host:/home/pi/project/")
   :type 'user-error)
  (should-error
   (pilish--shell-command-path
    "/sudo:root@pi-host:/tmp/a.el"
    "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
   :type 'user-error)
  (should-error
   (pilish--shell-command-path
    "/ssh:pi-host:/tmp/a.el" "/tmp/project/")
   :type 'user-error))

(ert-deftest pilish-test-shell-command-path-rejects-unrooted-tramp-local-names ()
  "Explicit TRAMP local names must not become unsafe shell operands."
  (dolist (case '(("/ssh:pi-host:-rf"
                   "/ssh:pi-host:/home/pi/project/")
                  ("/ssh:pi-host:" "/ssh:pi-host:/home/pi/project/")
                  ("/ssh:bastion|sudo:root@pi-host:-rf"
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
                  ("/ssh:bastion|sudo:root@pi-host:"
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
                  ("~root;printf PWNED/a.el"
                   "/ssh:pi-host:/home/pi/project/")
                  ("~+" "/ssh:pi-host:/home/pi/project/")
                  ("~-/a.el" "/ssh:pi-host:/home/pi/project/")
                  ("~0/a.el" "/ssh:pi-host:/home/pi/project/")))
    (should-error
     (pilish--shell-command-path (nth 0 case) (nth 1 case))
     :type 'user-error)))

(ert-deftest pilish-test-shell-quote-path-preserves-remote-home-expansion ()
  "Quoting leaves only a safe remote tilde prefix visible to the shell."
  (let ((remote "/ssh:pi-host:/home/pi/project/")
        (multi "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (should (equal (pilish--shell-quote-path "~/a b.el" remote)
                   (concat "~/" (shell-quote-argument "a b.el" t))))
    (should (equal (pilish--shell-quote-path
                    "~root/a b.el" remote)
                   (concat "~root/" (shell-quote-argument "a b.el" t))))
    (should (equal (pilish--shell-quote-path
                    "~daemon/a b.el" multi)
                   (concat "~daemon/" (shell-quote-argument "a b.el" t))))
    (should (equal (pilish--shell-quote-path
                    "~root/a\nb.el" remote)
                   (concat "~root/" (shell-quote-argument "a\nb.el" t))))
    (should (equal (pilish--shell-quote-path
                    "/tmp/a b.el" remote)
                   (shell-quote-argument "/tmp/a b.el" t)))
    (should (equal (pilish--shell-quote-path
                    "/tmp/a b.el" "/tmp/project/")
                   (shell-quote-argument "/tmp/a b.el")))))

(ert-deftest pilish-test-shell-quote-path-rejects-unsafe-home-prefix ()
  "Shell-significant and reserved named-home prefixes are never exposed."
  (dolist (path '("~root;printf PWNED/a.el" "~+" "~-/a.el"
                  "~0/a.el" "~+0/a.el"))
    (should-error
     (pilish--shell-quote-path
      path "/ssh:pi-host:/home/pi/project/")
     :type 'user-error)))

(ert-deftest pilish-test-shell-quote-path-hides-terminal-ampersand-from-emacs ()
  "Quoted operands cannot trigger `shell-command' trailing-& detection."
  (dolist (case '(("/tmp/report&" "/tmp/project/" nil)
                  ("/tmp/report&" "/ssh:pi-host:/home/pi/project/" t)
                  ("~/report&" "/ssh:pi-host:/home/pi/project/" t)))
    (let* ((path (nth 0 case))
           (anchor (nth 1 case))
           (posix (nth 2 case))
           (quoted (pilish--shell-quote-path path anchor))
           (ordinary (if (string-prefix-p "~/" path)
                         (concat "~/"
                                 (shell-quote-argument
                                  (substring path 2) t))
                       (shell-quote-argument path posix))))
      (should (equal quoted
                     (concat ordinary (shell-quote-argument "" posix))))
      (should-not (string-suffix-p "&" quoted)))))

;;;; Process Cleanup Tests

(ert-deftest pilish-test-process-exit-clears-pending ()
  "Process exit clears pending request state."
  (let ((pilish--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pilish--get-pending-requests fake-proc))
              (pending-types (pilish--get-pending-command-types fake-proc)))
          (puthash "req_1" #'ignore pending)
          (puthash "req_2" #'ignore pending)
          (puthash "req_1" "get_tree" pending-types)
          (puthash "req_2" "get_state" pending-types)
          (pilish--handle-process-exit fake-proc "finished\n")
          (should (= (hash-table-count pending) 0))
          (should (= (hash-table-count pending-types) 0)))
      (ignore-errors (delete-process fake-proc)))))

(ert-deftest pilish-test-process-exit-calls-callbacks-with-error ()
  "Process exit calls pending callbacks with error response."
  (let ((pilish--request-id-counter 0)
        (received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pilish--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pilish--handle-process-exit fake-proc "finished\n")
          (should received)
          (should (eq (plist-get received :success) :false))
          (should (plist-get received :error)))
      (ignore-errors (delete-process fake-proc)))))

(ert-deftest pilish-test-process-exit-runs-exit-handler-after-callbacks ()
  "Process exit lets pending callbacks recover input before frontend cleanup."
  (let ((events nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pilish--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (_r) (push 'callback events)) pending)
          (process-put fake-proc 'pilish-exit-handler
                       (lambda (_r) (push 'exit-handler events)))
          (pilish--handle-process-exit fake-proc "finished\n")
          (should (equal (reverse events) '(callback exit-handler))))
      (ignore-errors (delete-process fake-proc)))))

;;;; Response Dispatch Tests

(ert-deftest pilish-test-core-passes-disposition-to-correlated-callback ()
  "Out-of-order prompt/steer acknowledgments keep their exact response data."
  (pilish-test-with-rpc-session (_chat _input proc commands)
    (let (prompt-response steer-response)
      (pilish--rpc-async
       proc '(:type "prompt" :message "prompt input")
       (lambda (response) (setq prompt-response response)))
      (let ((prompt-id (plist-get (car commands) :id)))
        (pilish--rpc-async
         proc '(:type "steer" :message "steer input")
         (lambda (response) (setq steer-response response)))
        (let* ((steer-id (plist-get (car commands) :id))
               (pending (pilish--get-pending-requests proc))
               (types (pilish--get-pending-command-types proc)))
          (should (= 2 (hash-table-count pending)))
          (pilish-test--stdout
           proc (list :type "response" :id steer-id :command "steer" :success t
                      :data '(:disposition "queued")))
          (should-not prompt-response)
          (should (equal steer-response
                         (list :type "response" :id steer-id :command "steer"
                               :success t :data '(:disposition "queued"))))
          (should (gethash prompt-id pending))
          (should-not (gethash steer-id pending))
          (should-not (gethash steer-id types))
          (pilish-test--stdout
           proc (list :type "response" :id prompt-id :command "prompt" :success t
                      :data '(:disposition "handled")))
          (should (equal prompt-response
                         (list :type "response" :id prompt-id :command "prompt"
                               :success t :data '(:disposition "handled"))))
          (should (= 0 (hash-table-count pending)))
          (should (= 0 (hash-table-count types))))))))

(ert-deftest pilish-test-dispatch-response-calls-callback ()
  "Response with matching ID calls stored callback."
  (let ((received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pilish--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pilish--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should received)
          (should (eq (plist-get received :success) t)))
      (delete-process fake-proc))))

(ert-deftest pilish-test-dispatch-response-removes-callback ()
  "Response removes callback from pending requests after calling."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pilish--get-pending-requests fake-proc)))
          (puthash "req_1" #'ignore pending)
          (pilish--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should (null (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-dispatch-idless-response-to-sole-pending ()
  "Id-less response routes to sole pending callback."
  (let ((received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pilish--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pilish--dispatch-response
           fake-proc
           '(:type "response" :command "get_tree" :success nil :error "Unknown command: get_tree"))
          (should (equal (plist-get received :error) "Unknown command: get_tree"))
          (should (= (hash-table-count pending) 0)))
      (delete-process fake-proc))))

(ert-deftest pilish-test-dispatch-idless-response-matches-command ()
  "Id-less response with :command routes to matching request."
  (let ((pilish--request-id-counter 0)
        (received-tree nil)
        (received-state nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string) #'ignore))
          (pilish--rpc-async
           fake-proc
           '(:type "get_tree")
           (lambda (response)
             (setq received-tree response)))
          (pilish--rpc-async
           fake-proc
           '(:type "get_state")
           (lambda (response)
             (setq received-state response)))
          (pilish--dispatch-response
           fake-proc
           '(:type "response" :command "get_tree" :success nil :error "Unknown command: get_tree"))
          (should (equal (plist-get received-tree :error) "Unknown command: get_tree"))
          (should-not received-state)
          (should (= (hash-table-count (pilish--get-pending-requests fake-proc)) 1)))
      (delete-process fake-proc))))

(ert-deftest pilish-test-dispatch-event-calls-handler ()
  "Events call the process handler."
  (let ((event-received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'pilish-display-handler
                       (lambda (e) (setq event-received e)))
          (pilish--dispatch-response fake-proc '(:type "agent_start"))
          (should event-received)
          (should (equal (plist-get event-received :type) "agent_start")))
      (delete-process fake-proc))))

(ert-deftest pilish-test-dispatch-unknown-id-no-crash ()
  "Unknown response IDs do not crash."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (should (null (pilish--dispatch-response fake-proc '(:type "response" :id "unknown" :success t))))
      (delete-process fake-proc))))

;;;; RPC Send Tests

(ert-deftest pilish-test-rpc-async-stores-callback ()
  "Sending a command stores the callback in process's pending requests."
  (let ((pilish--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (pilish--rpc-async fake-proc '(:type "get_state") #'ignore)
          (let ((pending (pilish--get-pending-requests fake-proc)))
            (should (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-rpc-async-sends-json-with-id ()
  "Sending a command writes JSON with ID to process."
  (let ((pilish--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (pilish--rpc-async fake-proc '(:type "get_state") #'ignore)
                (should
                 (pilish-test-wait-until
                  (lambda ()
                    (with-current-buffer output-buffer
                      (> (buffer-size) 0)))
                  pilish-test-short-wait
                  pilish-test-poll-interval
                  fake-proc))
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :type) "get_state"))
                    (should (equal (plist-get json :id) "req_1")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer))))

(ert-deftest pilish-test-rpc-async-scheduling-error-cleans-pending-state ()
  "Encoding and sending failures leave no orphaned pending request state."
  (let ((pilish--request-id-counter 0)
        (real-encode (symbol-function 'pilish--encode-command))
        (real-send (symbol-function 'pilish--send-string))
        results)
    (dolist (failure '(encode send))
      (let ((fake-proc (start-process "cat" nil "cat"))
            attempted)
        (unwind-protect
            (cl-letf (((symbol-function 'pilish--encode-command)
                       (lambda (command)
                         (if (eq failure 'encode)
                             (progn
                               (setq attempted t)
                               (error "synchronous encode failure"))
                           (funcall real-encode command))))
                      ((symbol-function 'pilish--send-string)
                       (lambda (process string)
                         (if (eq failure 'send)
                             (progn
                               (setq attempted t)
                               (error "synchronous send failure"))
                           (funcall real-send process string)))))
              (condition-case nil
                  (pilish--rpc-async
                   fake-proc '(:type "get_state") #'ignore)
                (error nil))
              (push
               (list failure attempted
                     (hash-table-count
                      (pilish--get-pending-requests fake-proc))
                     (hash-table-count
                      (pilish--get-pending-command-types fake-proc)))
               results))
          (ignore-errors (delete-process fake-proc)))))
    (should (equal (nreverse results)
                   '((encode t 0 0) (send t 0 0))))))

(ert-deftest pilish-test-remote-rpc-queue-flushes-after-ready-marker ()
  "Remote RPC writes queue until the ready marker, then flush FIFO."
  (let ((pilish--request-id-counter 0)
        (sent nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc string) (push string sent))))
          (process-put fake-proc 'pilish-awaiting-ready-marker t)
          (pilish--rpc-async fake-proc '(:type "get_state") #'ignore)
          (pilish--rpc-async fake-proc '(:type "get_commands") #'ignore)
          (should-not sent)
          (should (= (length (process-get fake-proc
                                          'pilish-outbound-queue))
                     2))
          (pilish--process-filter
           fake-proc (concat pilish--remote-ready-marker "\n"))
          (should (process-get fake-proc 'pilish-ready))
          (should-not (process-get fake-proc
                                   'pilish-awaiting-ready-marker))
          (should-not (process-get fake-proc 'pilish-outbound-queue))
          (should (equal (mapcar (lambda (string)
                                   (plist-get
                                    (json-parse-string (string-trim string)
                                                       :object-type 'plist)
                                    :type))
                                 (nreverse sent))
                         '("get_state" "get_commands"))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-extension-ui-response-queues-until-ready-marker ()
  "Extension UI responses share the remote ready queue."
  (let ((sent nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc string) (push string sent))))
          (process-put fake-proc 'pilish-awaiting-ready-marker t)
          (pilish--send-extension-ui-response
           fake-proc '(:type "extension_ui_response" :id "ui_1" :value "ok"))
          (should-not sent)
          (should (= (length (process-get fake-proc
                                          'pilish-outbound-queue))
                     1))
          (pilish--process-filter
           fake-proc (concat pilish--remote-ready-marker "\n"))
          (should-not (process-get fake-proc 'pilish-outbound-queue))
          (let ((json (json-parse-string (string-trim (car sent))
                                         :object-type 'plist)))
            (should (equal (plist-get json :type) "extension_ui_response"))
            (should (equal (plist-get json :id) "ui_1"))))
      (delete-process fake-proc))))

;;;; Request ID Management Tests

(ert-deftest pilish-test-request-id-increments ()
  "Request IDs increment with each call."
  (let ((pilish--request-id-counter 0))
    (should (equal (pilish--next-request-id) "req_1"))
    (should (equal (pilish--next-request-id) "req_2"))
    (should (equal (pilish--next-request-id) "req_3"))))

(ert-deftest pilish-test-pending-requests-table ()
  "Each process gets its own pending requests table."
  (let ((proc1 (start-process "cat1" nil "cat"))
        (proc2 (start-process "cat2" nil "cat")))
    (unwind-protect
        (let ((pending1 (pilish--get-pending-requests proc1))
              (pending2 (pilish--get-pending-requests proc2)))
          ;; Tables should be separate
          (puthash "req_1" #'ignore pending1)
          (should (gethash "req_1" pending1))
          (should (null (gethash "req_1" pending2)))
          ;; Calling get again returns the same table
          (should (eq pending1 (pilish--get-pending-requests proc1))))
      (ignore-errors (delete-process proc1))
      (ignore-errors (delete-process proc2)))))

;;;; State Event Handling Tests

(ert-deftest pilish-test-event-agent-start-sets-streaming ()
  "agent_start event sets pilish--status to streaming."
  (let ((pilish--status 'idle)
        (pilish--state nil))
    (pilish--update-state-from-event '(:type "agent_start"))
    (should (eq pilish--status 'streaming))))

(ert-deftest pilish-test-lifecycle-agent-end-waits-for-settled ()
  "Only agent_settled releases a completed run, regardless of willRetry."
  (dolist (retry '(nil :false t))
    (let ((pilish--status 'streaming)
          (pilish--state nil))
      (pilish--update-state-from-event
       `(:type "agent_end" :messages [] :willRetry ,retry))
      (should (eq pilish--status 'sending))
      (pilish--update-state-from-event '(:type "agent_settled"))
      (should (eq pilish--status 'idle)))))

(ert-deftest pilish-test-event-agent-end-will-retry-keeps-sending ()
  "agent_end with willRetry keeps the session busy for Pi's retry."
  (let ((pilish--status 'streaming)
        (pilish--state nil))
    (pilish--update-state-from-event
     '(:type "agent_end" :messages [] :willRetry t))
    (should (eq pilish--status 'sending))))

(ert-deftest pilish-test-event-agent-end-stores-messages ()
  "agent_end event stores messages in state."
  (let ((pilish--status 'streaming)
        (pilish--state (list :messages nil))
        (msgs [(:role "user" :content "hi") (:role "assistant" :content "hello")]))
    (pilish--update-state-from-event (list :type "agent_end" :messages msgs))
    (should (plist-get pilish--state :messages))))

(ert-deftest pilish-test-event-message-start-creates-current-message ()
  "message_start event creates current-message in state."
  (let ((pilish--state (list :current-message nil))
        (msg '(:role "assistant" :content [])))
    (pilish--update-state-from-event (list :type "message_start" :message msg))
    (should (plist-get pilish--state :current-message))))

(ert-deftest pilish-test-event-message-update-is-state-noop ()
  "message_update event does not modify state.
Display is handled by the display handler, not by state updates."
  (let ((pilish--state (list :current-message '(:role "assistant" :content "Hello"))))
    (pilish--update-state-from-event
     '(:type "message_update"
       :message (:role "assistant")
       :assistantMessageEvent (:type "text_delta" :delta " world")))
    (should (equal (plist-get (plist-get pilish--state :current-message) :content)
                   "Hello"))))

(ert-deftest pilish-test-event-message-end-clears-current-message ()
  "message_end event clears current-message."
  (let ((pilish--state (list :current-message '(:role "assistant" :content "done"))))
    (pilish--update-state-from-event '(:type "message_end" :message (:role "assistant")))
    (should (null (plist-get pilish--state :current-message)))))

(ert-deftest pilish-test-event-tool-start-tracks-active-tool ()
  "tool_execution_start adds tool to active-tools."
  (let ((pilish--state (list :active-tools nil)))
    (pilish--update-state-from-event
     '(:type "tool_execution_start"
       :toolCallId "call_123"
       :toolName "bash"
       :args (:command "ls")))
    (should (gethash "call_123" (plist-get pilish--state :active-tools)))))

(ert-deftest pilish-test-event-tool-update-stores-partial-result ()
  "tool_execution_update stores partial result."
  (let* ((tools (make-hash-table :test 'equal))
         (pilish--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (pilish--update-state-from-event
     '(:type "tool_execution_update"
       :toolCallId "call_123"
       :partialResult (:content "output")))
    (let ((tool (gethash "call_123" tools)))
      (should (plist-get tool :partial-result)))))

(ert-deftest pilish-test-event-tool-end-removes-active-tool ()
  "tool_execution_end removes tool from active-tools."
  (let* ((tools (make-hash-table :test 'equal))
         (pilish--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (pilish--update-state-from-event
     '(:type "tool_execution_end"
       :toolCallId "call_123"
       :result (:content "done")
       :isError :false))
    (should (null (gethash "call_123" tools)))))

(ert-deftest pilish-test-event-compaction-start-sets-compacting ()
  "compaction_start event sets pilish--status to compacting."
  (let ((pilish--status 'idle)
        (pilish--state nil))
    (pilish--update-state-from-event '(:type "compaction_start" :reason "threshold"))
    (should (eq pilish--status 'compacting))))

(ert-deftest pilish-test-review-fixes-unpaired-overflow-keeps-sending ()
  "Exhausted overflow recovery can end compaction without a start event."
  (let ((pilish--status 'streaming)
        (pilish--pre-compaction-status nil)
        (pilish--state nil))
    (pilish--update-state-from-event '(:type "agent_end" :messages []))
    (pilish--update-state-from-event
     '(:type "compaction_end" :reason "overflow" :aborted :false :willRetry :false
       :result :null :errorMessage "Context overflow recovery failed after one retry"))
    (should (eq pilish--status 'sending))
    (pilish--update-state-from-event '(:type "agent_settled"))
    (should (eq pilish--status 'idle))))

(ert-deftest pilish-test-lifecycle-manual-compaction-returns-idle ()
  "Standalone manual compaction is terminal even on failure or cancellation."
  (dolist (outcome '((:aborted :false :result (:summary "Summary"))
                     (:aborted :false :result :null :errorMessage "quota")
                     (:aborted t :result :null)))
    (let ((pilish--status 'idle)
          (pilish--pre-compaction-status nil)
          (pilish--state nil))
      (pilish--update-state-from-event
       '(:type "compaction_start" :reason "manual"))
      (pilish--update-state-from-event
       (append '(:type "compaction_end" :willRetry :false) outcome))
      (should (eq pilish--status 'idle))
      (should-not pilish--pre-compaction-status))))

(ert-deftest pilish-test-event-compaction-end-will-retry-sets-sending ()
  "Overflow recovery after agent_end keeps the session busy."
  (let ((pilish--status 'sending)
        (pilish--pre-compaction-status nil)
        (pilish--state nil))
    (pilish--update-state-from-event '(:type "compaction_start" :reason "overflow"))
    (pilish--update-state-from-event
     '(:type "compaction_end"
       :reason "overflow"
       :aborted :false
       :willRetry t
       :result (:tokensBefore 1000 :summary "Summary")))
    (should (eq pilish--status 'sending))))

(ert-deftest pilish-test-event-compaction-end-will-retry-without-result-sets-idle ()
  "willRetry without a result is not a retrying success."
  (let ((pilish--status 'compacting)
        (pilish--pre-compaction-status nil)
        (pilish--state nil))
    (pilish--update-state-from-event
     '(:type "compaction_end"
       :reason "overflow"
       :aborted :false
       :willRetry t
       :result :null
       :errorMessage "recovery failed"))
    (should (eq pilish--status 'idle))))

(ert-deftest pilish-test-lifecycle-interturn-compaction-restores-streaming ()
  "Compaction between turns continues the same run without agent_start."
  (dolist (outcome '((:aborted :false :result (:summary "Summary"))
                     (:aborted :false :result :null :errorMessage "quota")
                     (:aborted t :result :null)))
    (ert-info ((format "Compaction outcome: %S" outcome))
      (let ((pilish--status 'idle)
            (pilish--pre-compaction-status nil)
            (pilish--state nil))
        (dolist (event '((:type "agent_start")
                         (:type "turn_start")
                         (:type "turn_end")
                         (:type "compaction_start" :reason "threshold")))
          (pilish--update-state-from-event event))
        (should (eq pilish--status 'compacting))
        (pilish--update-state-from-event
         (append '(:type "compaction_end" :willRetry :false) outcome))
        (should (eq pilish--status 'streaming))
        (pilish--update-state-from-event '(:type "turn_start"))
        (should (eq pilish--status 'streaming))
        (should-not pilish--pre-compaction-status)))))

(ert-deftest pilish-test-event-compaction-preserves-prompt-preflight-sending ()
  "Successful compaction during prompt preflight resumes that prompt."
  (let ((pilish--status 'sending)
        (pilish--pre-compaction-status nil)
        (pilish--state nil))
    (pilish--update-state-from-event
     '(:type "compaction_start" :reason "threshold"))
    (should (eq pilish--status 'compacting))
    (should (eq pilish--pre-compaction-status 'sending))
    (pilish--update-state-from-event
     '(:type "compaction_end"
       :reason "threshold"
       :aborted :false
       :willRetry :false
       :result (:tokensBefore 1000 :summary "Summary")))
    (should (eq pilish--status 'sending))
    (should (null pilish--pre-compaction-status))))

(ert-deftest pilish-test-lifecycle-failed-compaction-resumes-preflight-sending ()
  "Failed preflight compaction can still continue the original prompt."
  (let ((pilish--status 'sending)
        (pilish--pre-compaction-status nil)
        (pilish--state nil))
    (pilish--update-state-from-event
     '(:type "compaction_start" :reason "threshold"))
    (pilish--update-state-from-event
     '(:type "compaction_end"
       :reason "threshold"
       :aborted :false
       :willRetry :false
       :result :null
       :errorMessage "quota exceeded"))
    (should (eq pilish--status 'sending))
    (should (null pilish--pre-compaction-status))))

(ert-deftest pilish-test-lifecycle-cancelled-compaction-resumes-preflight-sending ()
  "An extension cancelling preflight compaction does not cancel the prompt."
  (let ((pilish--status 'sending)
        (pilish--pre-compaction-status nil)
        (pilish--state nil))
    (pilish--update-state-from-event
     '(:type "compaction_start" :reason "threshold"))
    (pilish--update-state-from-event
     '(:type "compaction_end"
       :reason "threshold"
       :aborted t
       :willRetry :false
       :result nil))
    (should (eq pilish--status 'sending))
    (should (null pilish--pre-compaction-status))))

(ert-deftest pilish-test-ensure-active-tools-from-nil ()
  "pilish--ensure-active-tools works when pilish--state is nil."
  (let ((pilish--state nil))
    (let ((tools (pilish--ensure-active-tools)))
      (should (hash-table-p tools))
      (should (hash-table-p (plist-get pilish--state :active-tools))))))

(ert-deftest pilish-test-response-set-model-updates-state ()
  "set_model response updates model in state."
  (let ((pilish--state (list :model nil)))
    (pilish--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success t
       :data (:id "claude" :name "Claude")))
    (should (plist-get pilish--state :model))
    (should (equal (plist-get (plist-get pilish--state :model) :id) "claude"))))

(ert-deftest pilish-test-response-cycle-thinking-updates-state ()
  "cycle_thinking_level response updates thinking-level."
  (let ((pilish--state (list :thinking-level "off")))
    (pilish--update-state-from-response
     '(:type "response"
       :command "cycle_thinking_level"
       :success t
       :data (:level "high")))
    (should (equal (plist-get pilish--state :thinking-level) "high"))))

(ert-deftest pilish-test-response-failed-does-not-update ()
  "Failed responses do not update state."
  (let ((pilish--state (list :model '(:id "original"))))
    (pilish--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success :false
       :error "Model not found"))
    (should (equal (plist-get (plist-get pilish--state :model) :id) "original"))))

(ert-deftest pilish-test-event-dispatch-updates-state ()
  "Events update buffer-local state via handler."
  (let ((pilish--status 'idle)
        (pilish--state nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          ;; Register a handler that updates state
          (process-put fake-proc 'pilish-display-handler
                       (lambda (e)
                         (pilish--update-state-from-event e)))
          (pilish--handle-event fake-proc '(:type "agent_start"))
          (should (eq pilish--status 'streaming)))
      (delete-process fake-proc))))

;;;; State Management Tests

(ert-deftest pilish-test-lifecycle-state-response-preserves-busy-status ()
  "All snapshots preserve busy phases; idle initialization adopts remote status."
  (dolist (status '(idle sending streaming compacting))
    (dolist (snapshot '((:false :false idle) (t :false streaming)
                        (:false t compacting) (t t streaming)))
      (let ((pilish--status status)
            (pilish--state nil)
            (expected (if (eq status 'idle) (nth 2 snapshot) status)))
        (pilish--update-state-from-response
         (list :type "response" :command "get_state" :success t
               :data (list :isStreaming (car snapshot)
                           :isCompacting (cadr snapshot) :thinkingLevel "high")))
        (should (equal (plist-get pilish--state :thinking-level) "high"))
        (should (eq pilish--status expected))
        (should (eq (plist-get pilish--state :status) expected))))))

(ert-deftest pilish-test-state-from-get-state-response ()
  "State is initialized from get_state response data."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:model (:id "claude" :name "Claude")
                           :thinkingLevel "medium"
                           :isStreaming :false
                           :sessionId "test-123"
                           :messageCount 0))))
    (let ((state (pilish--extract-state-from-response response)))
      (should state)
      (should (equal (plist-get state :session-id) "test-123"))
      (should (equal (plist-get state :thinking-level) "medium"))
      (should (eq (plist-get state :status) 'idle))
      (should (plist-get state :model)))))

(ert-deftest pilish-test-state-extract-normalizes-remote-session-file ()
  "State extraction converts inbound remote process-local session files."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:isStreaming :false
                           :isCompacting :false
                           :sessionFile "/home/pi/.pi/sessions/current.jsonl"))))
    (let ((state (pilish--extract-state-from-response
                  response
                  "/ssh:pi-host:/home/pi/project/")))
      (should (equal (plist-get state :session-file)
                     "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl")))))

(ert-deftest pilish-test-state-extract-normalizes-relative-session-file ()
  "State extraction resolves inbound relative session files under the anchor."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:isStreaming :false
                           :isCompacting :false
                           :sessionFile "sessions/current.jsonl"))))
    (let ((state (pilish--extract-state-from-response
                  response
                  "/ssh:pi-host:/home/pi/project/")))
      (should (equal (plist-get state :session-file)
                     "/ssh:pi-host:/home/pi/project/sessions/current.jsonl")))))

(ert-deftest pilish-test-state-extract-ignores-unsafe-session-file ()
  "Unsafe passive sessionFile metadata is not stored as a navigable path."
  (let* ((bad (concat "/tmp/a" (string ?\0) "b.jsonl"))
         (response (list :type "response"
                         :command "get_state"
                         :success t
                         :data (list :isStreaming :false
                                     :isCompacting :false
                                     :sessionId "bad-session"
                                     :sessionFile bad))))
    (let ((state (pilish--extract-state-from-response response)))
      (should (equal (plist-get state :session-id) "bad-session"))
      (should-not (plist-get state :session-file)))))

(ert-deftest pilish-test-state-extract-status-idle ()
  "Extracted state has status idle when not streaming or compacting."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting :false))))
    (let ((state (pilish--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'idle)))))

(ert-deftest pilish-test-state-extract-status-streaming ()
  "Extracted state has status streaming when isStreaming is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming t :isCompacting :false))))
    (let ((state (pilish--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'streaming)))))

(ert-deftest pilish-test-state-extract-status-compacting ()
  "Extracted state has status compacting when isCompacting is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting t))))
    (let ((state (pilish--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'compacting)))))

;;;; Auto-Retry Event State Tests

(ert-deftest pilish-test-event-auto-retry-start-sets-retrying ()
  "auto_retry_start event sets is-retrying to t."
  (let ((pilish--status 'streaming)
        (pilish--state (list :is-retrying nil)))
    (pilish--update-state-from-event
     '(:type "auto_retry_start"
       :attempt 1
       :maxAttempts 3
       :delayMs 2000
       :errorMessage "429 rate_limit_error"))
    (should (eq (plist-get pilish--state :is-retrying) t))
    (should (equal (plist-get pilish--state :retry-attempt) 1))
    (should (equal (plist-get pilish--state :last-error) "429 rate_limit_error"))))

(ert-deftest pilish-test-event-auto-retry-end-success-clears-retrying ()
  "auto_retry_end with success clears is-retrying."
  (let ((pilish--status 'streaming)
        (pilish--state (list :is-retrying t :retry-attempt 2)))
    (pilish--update-state-from-event
     '(:type "auto_retry_end"
       :success t
       :attempt 2))
    (should (eq (plist-get pilish--state :is-retrying) nil))))

(ert-deftest pilish-test-event-auto-retry-end-failure-stores-error ()
  "auto_retry_end with failure stores final error."
  (let ((pilish--status 'streaming)
        (pilish--state (list :is-retrying t)))
    (pilish--update-state-from-event
     '(:type "auto_retry_end"
       :success :false
       :attempt 3
       :finalError "529 overloaded_error: Overloaded"))
    (should (eq (plist-get pilish--state :is-retrying) nil))
    (should (equal (plist-get pilish--state :last-error) "529 overloaded_error: Overloaded"))))

(ert-deftest pilish-test-event-extension-error-stores-error ()
  "extension_error event stores error message in state."
  (let ((pilish--status 'streaming)
        (pilish--state (list :last-error nil)))
    (pilish--update-state-from-event
     '(:type "extension_error"
       :extensionPath "/path/to/extension.ts"
       :event "tool_call"
       :error "TypeError: undefined is not a function"))
    (should (equal (plist-get pilish--state :last-error)
                   "TypeError: undefined is not a function"))))

(ert-deftest pilish-test-event-agent-start-clears-error-state ()
  "agent_start event clears error and retry state."
  (let ((pilish--status 'idle)
        (pilish--state (list :is-retrying t
                         :last-error "Previous error")))
    (pilish--update-state-from-event '(:type "agent_start"))
    (should (eq pilish--status 'streaming))
    (should (eq (plist-get pilish--state :is-retrying) nil))
    (should (eq (plist-get pilish--state :last-error) nil))))

(ert-deftest pilish-test-event-agent-end-clears-retry-state ()
  "agent_end event clears retry state."
  (let ((pilish--status 'streaming)
        (pilish--state (list :is-retrying t)))
    (pilish--update-state-from-event '(:type "agent_end" :messages []))
    (should (eq pilish--status 'sending))
    (should (eq (plist-get pilish--state :is-retrying) nil))))

;;;; Test Utilities

(ert-deftest pilish-test-wait-until-succeeds ()
  "wait-until returns the predicate value when it becomes true."
  (let ((flag nil))
    (run-at-time 0.05 nil (lambda () (setq flag t)))
    (let ((result (pilish-test-wait-until (lambda () flag) 0.5 0.01)))
      (should (eq result t)))))

(ert-deftest pilish-test-wait-until-times-out ()
  "wait-until returns nil when the predicate stays false."
  (let ((result (pilish-test-wait-until (lambda () nil) 0.05 0.01)))
    (should (null result))))

(ert-deftest pilish-test-format-elapsed-rounds-to-millis ()
  "Elapsed formatter rounds to milliseconds with suffix."
  (should (equal (pilish-test-format-elapsed 1.23456) "1.235s")))

;;;; Executable Customization Tests

(defun pilish-test--capture-process-launch (executable extra-args &optional trust-policy directory)
  "Return the launch plist that `--start-process' passes to make-process.
Mocks `make-process' to capture all arguments, binding
`pilish-executable' to EXECUTABLE,
`pilish-extra-args' to EXTRA-ARGS,
`pilish-project-trust-policy' to TRUST-POLICY or `approve',
and starting in DIRECTORY or `/tmp/'."
  (let ((pilish-executable executable)
        (pilish-extra-args extra-args)
        (pilish-project-trust-policy (or trust-policy 'approve))
        (captured nil)
        (dummy-proc (start-process "pilish-capture" nil "cat")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured
                             (append args
                                     (list :captured-default-directory
                                           default-directory)))
                       dummy-proc)))
            (ignore-errors (pilish--start-process (or directory "/tmp/"))))
          captured)
      (when-let* ((stderr-buf (plist-get captured :stderr)))
        (when (buffer-live-p stderr-buf)
          (kill-buffer stderr-buf)))
      (when (process-live-p dummy-proc)
        (delete-process dummy-proc)))))

(ert-deftest pilish-test-start-process-uses-custom-executable ()
  "start-process builds command from `pilish-executable'."
  (should (equal (plist-get (pilish-test--capture-process-launch '("npx" "pi") nil)
                            :command)
                 '("npx" "pi" "--mode" "rpc" "--approve"))))

(ert-deftest pilish-test-start-process-custom-executable-with-extra-args ()
  "start-process combines custom executable, trust flag, and extra-args."
  (should (equal (plist-get (pilish-test--capture-process-launch
                             '("npx" "pi") '("-e" "/path/to/ext.ts"))
                            :command)
                 '("npx" "pi" "--mode" "rpc" "-e" "/path/to/ext.ts" "--approve"))))

(ert-deftest pilish-test-start-process-project-trust-default-omits-flag ()
  "A `default' project trust policy leaves Pi's own trust default in control."
  (should (equal (plist-get (pilish-test--capture-process-launch
                             '("pi") nil 'default)
                            :command)
                 '("pi" "--mode" "rpc"))))

(ert-deftest pilish-test-start-process-project-trust-no-approve-flag ()
  "A `no-approve' project trust policy tells Pi to ignore project-local files."
  (should (equal (plist-get (pilish-test--capture-process-launch
                             '("pi") nil 'no-approve)
                            :command)
                 '("pi" "--mode" "rpc" "--no-approve"))))

(ert-deftest pilish-test-start-process-captures-stderr-separately ()
  "start-process routes stderr away from the JSON-RPC stdout pipe."
  (let ((launch (pilish-test--capture-process-launch '("pi") nil)))
    (should (bufferp (plist-get launch :stderr)))))

(ert-deftest pilish-test-start-process-uses-default-directory-file-handler ()
  "start-process lets `default-directory' file handlers create the process."
  (let ((launch (pilish-test--capture-process-launch
                 '("pi") nil 'approve "/ssh:test:/home/me/proj/")))
    (should (eq (plist-get launch :file-handler) t))
    (should (equal (plist-get launch :captured-default-directory)
                   "/ssh:test:/home/me/proj/"))))

(ert-deftest pilish-test-start-process-remote-wraps-command-with-ready-marker ()
  "Remote start command emits a ready marker and preserves original argv."
  (let* ((launch (pilish-test--capture-process-launch
                  '("npx" "pi") '("-e" "/path/to/ext.ts")
                  'approve "/ssh:test:/home/me/proj/"))
         (command (plist-get launch :command))
         (script (nth 2 command)))
    (should (equal (nth 0 command) "sh"))
    (should (equal (nth 1 command) "-c"))
    (should (string-match-p (regexp-quote pilish--remote-ready-marker)
                            script))
    (should (string-match-p (regexp-quote "exec \"$0\" \"$@\"") script))
    (should (equal (nthcdr 3 command)
                   '("npx" "pi" "--mode" "rpc"
                     "-e" "/path/to/ext.ts" "--approve")))))

(ert-deftest pilish-test-start-process-disables-main-query ()
  "start-process disables kill prompts for Emacs's main Pi process."
  (let ((pilish-executable '("sh" "-c" "sleep 5"))
        (pilish-extra-args nil)
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (pilish--start-process "/tmp/"))
          (should (process-live-p proc))
          (should-not (process-query-on-exit-flag proc)))
      (when (processp proc)
        (pilish--cleanup-process-stderr-buffer proc)
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest pilish-test-inactivity-stderr-transport-does-not-refresh-stdout ()
  "Actual stderr arrival leaves the stdout clock alone; stdout advances it."
  (let ((pilish-executable
         '("sh" "-c" "while IFS= read -r line; do
case \"$line\" in
stdout) printf ' \\n' ;;
stderr) printf 'stderr-only\\n' >&2 ;;
esac
done"))
        (pilish-extra-args nil) proc)
    (unwind-protect
        (progn
          (setq proc (pilish--start-process temporary-file-directory))
          (process-send-string proc "stdout\n")
          (should (pilish-test-wait-until
                   (lambda () (process-get proc 'pilish-last-output-time))))
          (let ((last (process-get proc 'pilish-last-output-time))
                (stderr (process-get proc 'pilish-stderr-buf)))
            (should (processp (get-buffer-process stderr)))
            (process-send-string proc "stderr\n")
            (should (pilish-test-wait-until
                     (lambda ()
                       (with-current-buffer stderr
                         (string-match-p "stderr-only" (buffer-string))))))
            (should (process-live-p proc))
            (should (equal last (process-get proc 'pilish-last-output-time)))
            (process-send-string proc "stdout\n")
            (should (pilish-test-wait-until
                     (lambda () (> (process-get proc 'pilish-last-output-time) last))))))
      (when (processp proc)
        (pilish--cleanup-process-stderr-buffer proc)
        (when (process-live-p proc) (delete-process proc))))))

(ert-deftest pilish-test-start-process-disables-stderr-query ()
  "start-process disables kill prompts for Emacs's stderr pipe process."
  (let ((pilish-executable '("sh" "-c" "sleep 5"))
        (pilish-extra-args nil)
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (pilish--start-process "/tmp/"))
          (let* ((stderr-buf (process-get proc 'pilish-stderr-buf))
                 (stderr-proc (and stderr-buf (get-buffer-process stderr-buf))))
            (should (process-live-p proc))
            (should (buffer-live-p stderr-buf))
            (should stderr-proc)
            (should-not (process-query-on-exit-flag stderr-proc))))
      (when (processp proc)
        (pilish--cleanup-process-stderr-buffer proc)
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest pilish-test-cleanup-stderr-buffer-kills-stderr-process ()
  "stderr cleanup kills Emacs's stderr pipe process before killing its buffer."
  (let* ((stderr-buf (generate-new-buffer " *pilish-test-stderr-cleanup*"))
         (proc (make-process :name "pilish-cleanup-stderr"
                             :command '("sh" "-c" "sleep 5")
                             :connection-type 'pipe
                             :stderr stderr-buf)))
    (unwind-protect
        (let ((stderr-proc (get-buffer-process stderr-buf))
              (asked nil))
          (should stderr-proc)
          (should (process-query-on-exit-flag stderr-proc))
          (process-put proc 'pilish-stderr-buf stderr-buf)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (setq asked t)
                       (error "stderr cleanup should not prompt"))))
            (pilish--cleanup-process-stderr-buffer proc))
          (should-not asked)
          (should-not (buffer-live-p stderr-buf))
          (should-not (process-live-p stderr-proc)))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest pilish-test-process-exit-includes-exit-code ()
  "Process exit errors include the process exit code."
  (let ((fake-proc (start-process "pilish-exit-code" nil
                                  "sh" "-c" "exit 127"))
        (response nil))
    (unwind-protect
        (progn
          (while (process-live-p fake-proc)
            (accept-process-output fake-proc 0.05))
          (puthash "req_1" (lambda (r) (setq response r))
                   (pilish--get-pending-requests fake-proc))
          (pilish--handle-process-exit
           fake-proc "exited abnormally with code 127")
          (should (eq (plist-get response :processExit) t))
          (should (equal (plist-get response :exitCode) 127)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest pilish-test-process-exit-includes-stderr-excerpt ()
  "Process exit errors include stderr when available."
  (let ((fake-proc (start-process "pilish-exit" nil "cat"))
        (stderr-buf (generate-new-buffer " *pilish-test-stderr*"))
        (response nil))
    (unwind-protect
        (progn
          (with-current-buffer stderr-buf
            (insert "/tmp/undici.js:245\n"
                    "InvalidArgumentError: Invalid URL protocol: the URL must start with `http:` or `https:`.\n"
                    "    at parseURL (/tmp/undici.js:245:11)\n"
                    "Node.js v24.9.0\n"))
          (process-put fake-proc 'pilish-stderr-buf stderr-buf)
          (puthash "req_1" (lambda (r) (setq response r))
                   (pilish--get-pending-requests fake-proc))
          (pilish--handle-process-exit fake-proc "exited abnormally with code 1")
          (should (equal (plist-get response :error)
                         "Process exited: exited abnormally with code 1"))
          (should (string-match-p "Invalid URL protocol"
                                  (plist-get response :stderr)))
          (should-not (buffer-live-p stderr-buf)))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest pilish-test-process-exit-truncates-long-stderr ()
  "Long stderr excerpts keep the beginning and end without growing unbounded."
  (let ((fake-proc (start-process "pilish-exit-long" nil "cat"))
        (stderr-buf (generate-new-buffer " *pilish-test-stderr-long*"))
        (response nil))
    (unwind-protect
        (progn
          (with-current-buffer stderr-buf
            (insert "START InvalidArgumentError: Invalid URL protocol\n")
            (insert (make-string 5000 ?x))
            (insert "\nEND Node.js v24.9.0\n"))
          (process-put fake-proc 'pilish-stderr-buf stderr-buf)
          (puthash "req_2" (lambda (r) (setq response r))
                   (pilish--get-pending-requests fake-proc))
          (pilish--handle-process-exit fake-proc "exited abnormally with code 1")
          (let ((stderr (plist-get response :stderr)))
            (should (string-match-p "START InvalidArgumentError" stderr))
            (should (string-match-p "stderr truncated" stderr))
            (should (string-match-p "END Node.js" stderr))
            (should (< (length stderr) 4100))))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

;;;; Process Filter Tests

(ert-deftest pilish-test-process-filter-inhibits-redisplay ()
  "Process filter binds `inhibit-redisplay' to t during dispatch.
This batches N JSON lines delivered in one read() into a single
redisplay cycle instead of triggering N separate redraws."
  (let ((captured-inhibit nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'pilish-display-handler
                       (lambda (_e) (setq captured-inhibit inhibit-redisplay)))
          (pilish--process-filter
           fake-proc "{\"type\":\"agent_start\"}\n")
          (should (eq captured-inhibit t)))
      (delete-process fake-proc))))

(ert-deftest pilish-test-process-filter-continues-after-callback-error ()
  "A failing response callback does not drop later complete lines."
  (let ((second-called nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pilish--get-pending-requests fake-proc))
              (pending-types (pilish--get-pending-command-types fake-proc)))
          (puthash "req_1" (lambda (_response) (error "boom")) pending)
          (puthash "req_2" (lambda (_response) (setq second-called t)) pending)
          (puthash "req_1" "get_state" pending-types)
          (puthash "req_2" "get_history" pending-types)
          (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
            (pilish--process-filter
             fake-proc
             (concat "{\"type\":\"response\",\"id\":\"req_1\",\"success\":true}\n"
                     "{\"type\":\"response\",\"id\":\"req_2\",\"success\":true}\n")))
          (should second-called)
          (should-not (gethash "req_1" pending))
          (should-not (gethash "req_2" pending)))
      (delete-process fake-proc))))

(ert-deftest pilish-test-process-filter-ready-marker-not-dispatched ()
  "The remote ready marker is consumed before JSON/event dispatch."
  (let ((events nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'pilish-awaiting-ready-marker t)
          (process-put fake-proc 'pilish-display-handler
                       (lambda (event) (push event events)))
          (pilish--process-filter
           fake-proc (concat pilish--remote-ready-marker "\n"))
          (should (process-get fake-proc 'pilish-ready))
          (should-not events))
      (delete-process fake-proc))))

(ert-deftest pilish-test-process-filter-chunked-ready-marker-flushes ()
  "A ready marker split across filter calls still flushes queued writes."
  (let ((sent nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc string) (push string sent))))
          (process-put fake-proc 'pilish-awaiting-ready-marker t)
          (pilish--send-string fake-proc "first\n")
          (let* ((marker pilish--remote-ready-marker)
                 (split (/ (length marker) 2)))
            (pilish--process-filter fake-proc (substring marker 0 split))
            (should-not sent)
            (should-not (process-get fake-proc 'pilish-ready))
            (pilish--process-filter
             fake-proc (concat (substring marker split) "\n")))
          (should (process-get fake-proc 'pilish-ready))
          (should (equal (nreverse sent) '("first\n"))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-process-filter-keeps-partial-output-chunked ()
  "Process filter dispatches only complete JSON lines and clears chunk state."
  (let ((events nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'pilish-display-handler
                       (lambda (event) (push event events)))
          (process-put fake-proc 'pilish-partial-output-chunks
                       '("{\"type\":\"agent"))
          (pilish--process-filter fake-proc "_start\"")
          (should (null events))
          (should (equal (process-get fake-proc
                                      'pilish-partial-output-chunks)
                         '("_start\"" "{\"type\":\"agent")))
          (pilish--process-filter
           fake-proc "}\n{\"type\":\"agent_stop\"}\npartial")
          (should (equal (mapcar (lambda (event) (plist-get event :type))
                                 (nreverse events))
                         '("agent_start" "agent_stop")))
          (should (equal (process-get fake-proc
                                      'pilish-partial-output-chunks)
                         '("partial")))
          (setq events nil)
          (pilish--process-filter fake-proc "\n")
          (should (null events))
          (should (null (process-get fake-proc
                                     'pilish-partial-output-chunks))))
      (delete-process fake-proc))))

(ert-deftest pilish-test-process-filter-get-state-ignores-nul-session-file ()
  "A get_state response with NUL sessionFile does not escape the filter."
  (let ((pilish--status 'idle)
        (pilish--state nil)
        (callback-called nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (puthash "req_state"
                   (lambda (response)
                     (setq callback-called t)
                     (pilish--update-state-from-response response))
                   (pilish--get-pending-requests fake-proc))
          (puthash "req_state" "get_state"
                   (pilish--get-pending-command-types fake-proc))
          (let ((err nil))
            (condition-case caught
                (pilish--process-filter
                 fake-proc
                 "{\"type\":\"response\",\"id\":\"req_state\",\"command\":\"get_state\",\"success\":true,\"data\":{\"isStreaming\":false,\"isCompacting\":false,\"sessionId\":\"nul-session\",\"sessionFile\":\"/tmp/a\\u0000b.jsonl\"}}\n")
              (error (setq err caught)))
            (should-not err))
          (should callback-called)
          (should (equal (plist-get pilish--state :session-id)
                         "nul-session"))
          (should-not (plist-get pilish--state :session-file)))
      (delete-process fake-proc))))

(provide 'pilish-core-test)
;;; pilish-core-test.el ends here
