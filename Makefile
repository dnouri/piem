# Pilish Makefile

EMACS ?= emacs
export EMACS
# Keep package state project-local and separate incompatible bytecode by Emacs
# major version so every child lane resolves the same dependency tree without
# touching ~/.emacs.d/elpa.
EMACS_MAJOR_VERSION := $(shell $(EMACS) --batch -Q --eval '(princ emacs-major-version)' 2>/dev/null)
PACKAGE_USER_DIR ?= $(abspath .cache/elpa/$(EMACS_MAJOR_VERSION))
export PACKAGE_USER_DIR
PACKAGE_USER_DIR_INIT = --eval "(let ((dir (getenv \"PACKAGE_USER_DIR\"))) (when dir (setq package-user-dir (directory-file-name (expand-file-name dir)))))"
BATCH = $(EMACS) --batch -Q -L . \
	$(PACKAGE_USER_DIR_INIT) \
	--eval "(add-to-list 'treesit-extra-load-path (expand-file-name \"~/.emacs.d/tree-sitter\"))"
# Keep this checkout first in load-path even after package-initialize.
LOCAL_LOAD_PATH = --eval "(setq load-path (cons (expand-file-name \".\") load-path))"

# Pi CLI version — single source of truth (workflows extract this automatically)
PI_VERSION ?= 1.0.0
PI_PACKAGE ?= @earendil-works/pi-coding-agent
PI_BIN ?= .cache/pi/node_modules/.bin/pi
PI_BIN_DIR = $(abspath $(dir $(PI_BIN)))

# Test selector: exported unchanged and interpreted by ERT as an Emacs regexp
# Examples: make test SELECTOR=toolcall-delta
#           make test SELECTOR='abort\|followup'  # one regexp backslash
SELECTOR ?=

# Verbose output for tests (show full ERT output, including passed lines)
# Example: make test VERBOSE=1
VERBOSE ?=

.PHONY: test test-unit test-core test-ui test-render test-table test-input test-menu test-browse test-jsonl test-build
.PHONY: test-integration test-integration-fake test-integration-real test-integration-ci test-integration-ci-real test-gui test-gui-ci test-all
.PHONY: bench bench-batch bench-reload-resume bench-reload-resume-batch bench-reload-resume-smoke
.PHONY: bench-tool-update bench-tool-update-batch bench-tool-update-smoke
.PHONY: bench-stream-delta bench-stream-delta-batch bench-stream-delta-smoke
.PHONY: bench-agent-end-cooling bench-agent-end-cooling-batch bench-agent-end-cooling-smoke
.PHONY: check check-parens compile lint lint-checkdoc lint-package clean clean-cache help
.PHONY: ollama-start ollama-stop ollama-status setup-pi install-hooks

help:
	@echo "Targets:"
	@echo "  make test             All unit tests (SELECTOR=pattern, VERBOSE=1 for full output)"
	@echo "  make test-core        Core/RPC tests only"
	@echo "  make test-ui          UI foundation tests only"
	@echo "  make test-render      Render tests only"
	@echo "  make test-table       Table decoration tests only"
	@echo "  make test-input       Input buffer tests only"
	@echo "  make test-menu        Menu/session tests only"
	@echo "  make test-browse      Session/tree browser tests only"
	@echo "  make test-jsonl        JSONL reader/tree/projection tests only"
	@echo "  make test-build       Build/dependency helper tests only"
	@echo "  make test-unit        Compile + all unit tests"
	@echo "  make test-integration Shared integration tests (fake first, then real; local target starts Ollama for the real lane)"
	@echo "  make test-integration-fake Shared integration tests against fake backend only"
	@echo "  make test-integration-real Shared integration tests against real backend only (local target starts Ollama)"
	@echo "  make test-gui         Deterministic fake-backed GUI tests (SELECTOR=pattern; no Docker)"
	@echo "  make bench                         Table rendering benchmarks (GUI via xvfb)"
	@echo "  make bench-batch                   Table rendering benchmarks (batch, secondary lane)"
	@echo "  make bench-reload-resume           Reload/resume benchmarks (GUI via xvfb)"
	@echo "  make bench-reload-resume-batch     Reload/resume benchmarks (batch, secondary lane)"
	@echo "  make bench-reload-resume-smoke     Reload/resume smoke benchmark (batch, no timing thresholds)"
	@echo "  make bench-tool-update             Tool-update storm benchmarks (GUI via xvfb)"
	@echo "  make bench-tool-update-batch       Tool-update storm benchmarks (batch, secondary lane)"
	@echo "  make bench-tool-update-smoke       Tool-update storm smoke benchmark (batch, no timing thresholds)"
	@echo "  make bench-stream-delta            Stream-delta benchmark (GUI via xvfb)"
	@echo "  make bench-stream-delta-batch      Stream-delta benchmark (batch, secondary lane)"
	@echo "  make bench-stream-delta-smoke      Stream-delta smoke benchmark (batch, no timing thresholds)"
	@echo "  make bench-agent-end-cooling       Deferred agent_end cooling benchmark (GUI via xvfb)"
	@echo "  make bench-agent-end-cooling-batch Deferred agent_end cooling benchmark (batch, secondary lane)"
	@echo "  make bench-agent-end-cooling-smoke Cheap deferred cooling smoke (batch, no timing thresholds)"
	@echo "  make lint             Checkdoc + package-lint"
	@echo "  make check            Compile, lint, unit tests (pre-commit)"
	@echo "  make install-hooks    Set up git pre-commit hook"
	@echo "  make clean            Remove generated files"
	@echo ""
	@echo "CI targets:"
	@echo "  make test-unit              (used by Unit Tests workflow)"
	@echo "  make lint                   (used by Lint workflow)"
	@echo "  make test-integration-ci    (CI-shaped integration run: fake lane, then real lane; expects Ollama already running)"
	@echo "  make test-integration-ci-real (real integration lane with Ollama already running)"
	@echo "  make test-gui-ci            (fake-backed GUI lane under xvfb/headless)"

# ============================================================
# Dependencies
# ============================================================

# Install package dependencies (sentinel file avoids re-running every time).
# Requirements come from pilish.el's Package-Requires header.
# The helper upgrades built-in packages when Emacs ships an older version
# than the package requires (for example transient on Emacs 29/30).  Keep the
# sentinel with the selected package directory so overrides cannot reuse a
# stamp created for another dependency tree or Emacs lane.
DEPS_STAMP = $(PACKAGE_USER_DIR)/.deps-stamp
DEPS_INPUTS = Makefile scripts/install-deps.el scripts/pilish-build.el pilish.el
.PHONY: .deps-stamp
.deps-stamp: $(DEPS_STAMP)

$(DEPS_STAMP): $(DEPS_INPUTS)
	@mkdir -p "$(PACKAGE_USER_DIR)"
	@$(BATCH) -L scripts -l scripts/install-deps.el
	@touch "$@"

deps: .deps-stamp

# ============================================================
# Unit tests
# ============================================================

SHELL = bash
export SELECTOR
ERT_RUN = --eval '(let ((selector (getenv "SELECTOR"))) (if (and selector (> (length selector) 0)) (progn (require (quote ert)) (unless (ert-select-tests selector t) (error "SELECTOR matched no tests: %s" selector)) (ert-run-tests-batch-and-exit selector)) (ert-run-tests-batch-and-exit t)))'
GUI_SELECTOR_ARG = $(if $(SELECTOR),$(SELECTOR),)

test: .deps-stamp
	@echo "=== Unit Tests ==="
	@set -o pipefail; \
	OUTPUT=$$(mktemp); \
	$(BATCH) -L test \
		--eval "(setq load-prefer-newer t)" \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOCAL_LOAD_PATH) \
		-l pilish \
		-l pilish-core-test \
		-l pilish-ui-test \
		-l pilish-render-test \
		-l pilish-table-test \
		-l pilish-input-test \
		-l pilish-menu-test \
		-l pilish-browse-test \
		-l pilish-jsonl-test \
		-l pilish-build-test \
		-l pilish-evil-test \
		-l pilish-fake-pi-test \
		-l pilish-gui-test-utils-test \
		-l pilish-integration-test-common-test \
		-l pilish-test \
		$(ERT_RUN) \
		>$$OUTPUT 2>&1; \
	STATUS=$$?; \
	if [ "$(VERBOSE)" = "1" ] || [ $$STATUS -ne 0 ]; then \
		cat $$OUTPUT; \
	else \
		grep -v "^   passed\|^Pi: \|^Running [0-9]\|^$$" $$OUTPUT; \
	fi; \
	rm -f $$OUTPUT; \
	exit $$STATUS

# Per-module test targets: run tests for a single module in isolation.
# Usage: make test-render (much faster than `make test` during development)
BATCH_TEST = $(BATCH) -L test --eval "(setq load-prefer-newer t)" \
	--eval "(require 'package)" --eval "(package-initialize)" \
	$(LOCAL_LOAD_PATH) \
	-l pilish

test-core: .deps-stamp
	@$(BATCH_TEST) -l pilish-core-test $(ERT_RUN)
test-ui: .deps-stamp
	@$(BATCH_TEST) -l pilish-ui-test $(ERT_RUN)
test-render: .deps-stamp
	@$(BATCH_TEST) -l pilish-render-test $(ERT_RUN)
test-table: .deps-stamp
	@$(BATCH_TEST) -l pilish-table-test $(ERT_RUN)
test-input: .deps-stamp
	@$(BATCH_TEST) -l pilish-input-test $(ERT_RUN)
test-menu: .deps-stamp
	@$(BATCH_TEST) -l pilish-menu-test $(ERT_RUN)
test-browse: .deps-stamp
	@$(BATCH_TEST) -l pilish-browse-test $(ERT_RUN)

test-jsonl: .deps-stamp
	@$(BATCH_TEST) -l pilish-jsonl-test $(ERT_RUN)

test-build: .deps-stamp
	@$(BATCH_TEST) -l pilish-build-test $(ERT_RUN)

test-unit: compile test

# ============================================================
# Setup helpers
# ============================================================

install-hooks:
	@git config core.hooksPath hooks
	@echo "Git hooks installed (using hooks/)"

setup-pi:
	@if [ -x "$(PI_BIN)" ]; then \
		CURRENT=$$($(PI_BIN) --version 2>&1 | tr -d '\r' | grep -Eo '^[0-9]+[.][0-9]+[.][0-9]+' | tail -1); \
		if [ "$$CURRENT" != "$(PI_VERSION)" ] && [ "$(PI_VERSION)" != "latest" ]; then \
			echo "Cached pi@$$CURRENT differs from requested $(PI_VERSION), reinstalling..."; \
			rm -rf .cache/pi; \
		fi; \
	fi
	@if [ ! -x "$(PI_BIN)" ]; then \
		echo "Installing $(PI_PACKAGE)@$(PI_VERSION) to .cache/pi/..."; \
		rm -rf .cache/pi; \
		npm install --prefix .cache/pi --ignore-scripts $(PI_PACKAGE)@$(PI_VERSION) --silent; \
	fi
	@echo "Using pi: $(PI_BIN)"
	@$(PI_BIN) --version || (echo "ERROR: pi not working"; exit 1)

# ============================================================
# Integration tests
# ============================================================

INTEGRATION_BATCH = $(BATCH) -L test \
	--eval "(setq load-prefer-newer t)" \
	--eval "(require 'package)" \
	--eval "(package-initialize)" \
	$(LOCAL_LOAD_PATH) \
	-l pilish -l pilish-integration-test \
	$(ERT_RUN)
# Reuse CI's session directory when provided, but stay locally runnable by
# creating and cleaning up a temporary session directory otherwise.
REAL_INTEGRATION_RUN = \
	SESSION_DIR="$$PI_CODING_AGENT_DIR"; \
	CLEANUP_SESSION_DIR=0; \
	if [ -z "$$SESSION_DIR" ]; then \
		SESSION_DIR=$$(mktemp -d); \
		CLEANUP_SESSION_DIR=1; \
	else \
		mkdir -p "$$SESSION_DIR"; \
	fi; \
	cp test/fixtures/ollama-models.json "$$SESSION_DIR/models.json"; \
	env PATH="$(PI_BIN_DIR):$$PATH" PI_CODING_AGENT_DIR="$$SESSION_DIR" PI_RUN_INTEGRATION=1 PI_INTEGRATION_BACKENDS=real \
		$(INTEGRATION_BATCH); \
	status=$$?; \
	if [ "$$CLEANUP_SESSION_DIR" = "1" ]; then rm -rf "$$SESSION_DIR"; fi; \
	exit $$status

# Local default: fake lane first, then the slower real compatibility lane.
test-integration:
	@$(MAKE) --no-print-directory test-integration-fake
	@$(MAKE) --no-print-directory test-integration-real

# Local: fake backend only (no pi install or Ollama needed)
test-integration-fake: .deps-stamp
	@echo "=== Integration Tests (fake backend only) ==="
	@env PI_RUN_INTEGRATION=1 PI_INTEGRATION_BACKENDS=fake \
		$(INTEGRATION_BATCH)

# Local: real backend only
test-integration-real: .deps-stamp setup-pi
	@echo "=== Integration Tests (real backend only, pi@$(PI_VERSION)) ==="
	@./scripts/ollama.sh start
	@$(REAL_INTEGRATION_RUN)

# CI-shaped default: fast fake lane first, then the real backend lane.
test-integration-ci:
	@$(MAKE) --no-print-directory test-integration-fake
	@$(MAKE) --no-print-directory test-integration-ci-real

# CI: Ollama already running via services block for the real lane.
test-integration-ci-real: .deps-stamp setup-pi
	@echo "=== Integration Tests CI (real backend only, pi@$(PI_VERSION)) ==="
	@$(REAL_INTEGRATION_RUN)

# ============================================================
# GUI tests
# ============================================================

# Local: deterministic fake-backed GUI regressions (no Docker or pi install).
test-gui: .deps-stamp
	@echo "=== GUI Tests (fake backend only) ==="
	@./test/run-gui-tests.sh $(GUI_SELECTOR_ARG)

# CI: same fake-backed suite under xvfb/headless.
test-gui-ci: .deps-stamp
	@echo "=== GUI Tests CI (fake backend only) ==="
	@PI_HEADLESS=1 ./test/run-gui-tests.sh --headless $(GUI_SELECTOR_ARG)

# ============================================================
# All tests
# ============================================================

test-all: test test-integration test-gui

# ============================================================
# Benchmarks
# ============================================================

# Primary lane: GUI via xvfb (realistic string-width / font metrics).
bench: .deps-stamp
	@./bench/run-bench.sh

# Secondary lane: batch mode (faster, no font engine).
bench-batch: .deps-stamp
	@./bench/run-bench.sh --batch

# Primary lane: GUI via xvfb for real reload/resume rendering behavior.
bench-reload-resume: .deps-stamp
	@./bench/run-reload-resume-bench.sh

# Secondary lane: batch mode; useful for CI artifacts and quick comparisons.
bench-reload-resume-batch: .deps-stamp
	@./bench/run-reload-resume-bench.sh --batch

# Cheap correctness/regression smoke; no timing thresholds are enforced.
bench-reload-resume-smoke: .deps-stamp
	@./bench/run-reload-resume-bench.sh --batch --scenario smoke -c 1

# Primary lane: GUI via xvfb; measures rendering of tool_execution_update
# storms against the stock frontend plus main-thread blocking.
bench-tool-update: .deps-stamp
	@./bench/run-tool-update-bench.sh

# Secondary lane: batch mode; useful for CI artifacts and quick comparisons.
bench-tool-update-batch: .deps-stamp
	@./bench/run-tool-update-bench.sh --batch

# Cheap correctness/regression smoke; no timing thresholds are enforced.
bench-tool-update-smoke: .deps-stamp
	@./bench/run-tool-update-bench.sh --batch --scenario smoke -c 1

# Primary lane: GUI via xvfb for representative coalesced stream rendering.
bench-stream-delta: .deps-stamp
	@./bench/run-stream-delta-bench.sh

# Secondary lane: batch mode; useful for CI artifacts and quick comparisons.
bench-stream-delta-batch: .deps-stamp
	@./bench/run-stream-delta-bench.sh --batch

# Cheap correctness/regression smoke; no timing thresholds are enforced.
bench-stream-delta-smoke: .deps-stamp
	@./bench/run-stream-delta-bench.sh --batch --scenario smoke -c 1

# Deferred agent_end regression: a 90-overlay cohort drains through the real
# process filter and production one-shot cooling timers.  Timing is diagnostic.
bench-agent-end-cooling: .deps-stamp
	@./bench/run-tool-update-bench.sh --scenario agent-end-cooling \
		--out-dir tmp/agent-end-cooling-bench/gui

bench-agent-end-cooling-batch: .deps-stamp
	@./bench/run-tool-update-bench.sh --batch --scenario agent-end-cooling \
		--out-dir tmp/agent-end-cooling-bench/batch

bench-agent-end-cooling-smoke: .deps-stamp
	@./bench/run-tool-update-bench.sh --batch --scenario agent-end-cooling-smoke -c 1 \
		--out-dir tmp/agent-end-cooling-bench/smoke

# ============================================================
# Ollama management (local development)
# ============================================================

ollama-start:
	@./scripts/ollama.sh start

ollama-stop:
	@./scripts/ollama.sh stop

ollama-status:
	@./scripts/ollama.sh status

# ============================================================
# Code quality
# ============================================================

check-parens:
	@echo "=== Check Parens ==="
	@OUTPUT=$$($(BATCH) --eval '(condition-case err (dolist (f (list "scripts/pilish-build.el" "scripts/install-deps.el" "scripts/install-ts-grammars.el" "pilish-core.el" "pilish-jsonl.el" "pilish-grammars.el" "pilish-ui.el" "pilish-table.el" "pilish-render.el" "pilish-input.el" "pilish-menu.el" "pilish-browse.el" "pilish-evil.el" "pilish.el")) (with-current-buffer (find-file-noselect f) (check-parens) (message "%s OK" f))) (user-error (message "FAIL: %s" (error-message-string err)) (kill-emacs 1)))' 2>&1); \
	echo "$$OUTPUT" | grep -E "OK$$|FAIL:"; \
	echo "$$OUTPUT" | grep -q "FAIL:" && exit 1 || true

compile: .deps-stamp
	@rm -f *.elc scripts/*.elc
	@echo "=== Byte-compile ==="
	@$(BATCH) -L scripts \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOCAL_LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile scripts/pilish-build.el scripts/install-deps.el scripts/install-ts-grammars.el pilish-core.el pilish-jsonl.el pilish-grammars.el pilish-ui.el pilish-table.el pilish-render.el pilish-input.el pilish-menu.el pilish-browse.el pilish-evil.el pilish.el

lint: lint-checkdoc lint-package

lint-checkdoc:
	@echo "=== Checkdoc ==="
	@OUTPUT=$$($(BATCH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq sentence-end-double-space nil)" \
		--eval "(checkdoc-file \"scripts/pilish-build.el\")" \
		--eval "(checkdoc-file \"scripts/install-deps.el\")" \
		--eval "(checkdoc-file \"scripts/install-ts-grammars.el\")" \
		--eval "(checkdoc-file \"pilish-core.el\")" \
		--eval "(checkdoc-file \"pilish-jsonl.el\")" \
		--eval "(checkdoc-file \"pilish-grammars.el\")" \
		--eval "(checkdoc-file \"pilish-ui.el\")" \
		--eval "(checkdoc-file \"pilish-table.el\")" \
		--eval "(checkdoc-file \"pilish-render.el\")" \
		--eval "(checkdoc-file \"pilish-input.el\")" \
		--eval "(checkdoc-file \"pilish-menu.el\")" \
		--eval "(checkdoc-file \"pilish-browse.el\")" \
		--eval "(checkdoc-file \"pilish-evil.el\")" \
		--eval "(checkdoc-file \"pilish.el\")" 2>&1); \
	WARNINGS=$$(echo "$$OUTPUT" | grep -A1 "^Warning" | grep -v "^Warning\|^--$$"); \
	if [ -n "$$WARNINGS" ]; then echo "$$WARNINGS"; exit 1; else echo "OK"; fi

lint-package:
	@echo "=== Package-lint ==="
	@$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(let ((desc (cadr (assq 'package-lint package-archive-contents)))) \
		          (when (and desc (not (package-installed-p 'package-lint (package-desc-version desc)))) \
		            (package-install 'package-lint)))" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"pilish.el\")" \
		-f package-lint-batch-and-exit pilish.el pilish-ui.el pilish-table.el pilish-render.el pilish-input.el pilish-menu.el pilish-browse.el pilish-core.el pilish-jsonl.el pilish-grammars.el

check: compile lint test

# ============================================================
# Cleanup
# ============================================================

clean:
	@rm -f *.elc scripts/*.elc test/*.elc .deps-stamp "$(DEPS_STAMP)"

clean-cache:
	@./scripts/ollama.sh stop 2>/dev/null || true
	@rm -rf .cache
