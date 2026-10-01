# Pilish — Development Guide

Emacs frontend for the [pi coding agent](https://pi.dev).
Two-window UI: markdown chat buffer + prompt composition buffer.
Communicates with the pi CLI via JSON-over-stdio (RPC).

## Module Architecture

Ten production source modules form a dependency DAG (no cycles), plus
an optional Evil integration module.  Direct internal `require` edges are:

```
pilish.el -> menu, input, browse
menu.el           -> jsonl, render
input.el          -> render
browse.el         -> core, jsonl, ui
render.el         -> table, ui
table.el          -> ui
ui.el             -> core, grammars
jsonl.el          -> core
core.el           -> (none)
grammars.el       -> (none)

optional evil.el  -> ui, input, menu
```

In particular, `menu -> jsonl` is direct, both browsers use `jsonl` and
`ui`, and `ui` does **not** depend on `jsonl`.  Menu and UI only declare
browser entry points; they do not require `browse`.  Browse similarly
declares the menu transition functions it calls.  The top-level load order
makes those commands available without introducing a cycle.

External package dependency:

- `md-ts-mode` ← tree-sitter markdown major mode used by chat buffers
  (loading `pilish` must not globally claim unrelated Markdown files)

`menu.el` and `input.el` are siblings — neither requires the other.  Shared
session state lives in `ui.el`.  `table.el` requires `ui.el` for visible-text
extraction and scroll preservation, while `render.el` requires both.

Cross-module state mutations use accessor functions defined in `ui.el`
(e.g., `--set-process`, `--set-aborted`, `--push-followup`).  Within a
module, direct `setq` is fine.

`agent_end` finishes the low-level loop and finalizes tools. `agent_settled`
releases waiting (`sending`) work, not observably newer streaming/compaction:
Pi emits it *after* awaited extension hooks, which can start another operation.
If that newer run has also ended, settlements have no wire identity and cannot
be distinguished universally (even with `get_state`). Do not invent run IDs or
timer-based settlement guarantees. Automatic compaction preserves its surrounding
owner; explicit manual compaction does not inherit an older run's sending state.
Locally initiated manual compaction reserves submission via the existing pending
RPC table until its correlated response. Prompt request records separately track
acceptance and observed start/echo, since an extension's acknowledgment can follow
its run's settlement. Stop sends `clear_queue` before `abort` and reapplies its
latch on delayed agent/compaction starts.

## Source Files

| File | Purpose |
|------|---------|
| `pilish.el` | Entry point, autoloads, `--setup-session` |
| `pilish-core.el` | JSON parsing, line buffering, RPC request correlation, and process protocol |
| `pilish-ui.el` | Shared session/buffer state, accessors, faces, customization, chat/input modes and keymaps, header/activity UI, and local slash-command dispatch; requires core and grammars, not jsonl |
| `pilish-render.el` | Streaming and history rendering for user, assistant, branch-summary, and compaction messages; tool output, deferred completed-tool cooling outside the hot tail, fontification, diffs, and deferred history table postprocessing |
| `pilish-table.el` | Display-only pipe table decoration, wrapping, overlay management, and resize refresh over UI visible-text/scroll seams |
| `pilish-input.el` | Input history/isearch, send/abort, file/path/slash completion, queuing, and local `/resume` dispatch to the session browser |
| `pilish-menu.el` | Transient menu; guarded new/reload/resume transitions; canonical jsonl cwd/name metadata; model, thinking, command, export, and stats actions; `r` sessions and `w` tree entries |
| `pilish-browse.el` | Persistent magit-section session/tree browsers: time-sliced disk session discovery, filters/search/sort/scope, guarded switching, rename, and deletion; disk tree projection, filters/search, labels, and guarded navigation through an atomic local rewrite plus the menu resume flow. Browsing and labels need no live process; switching/navigation do. TRAMP non-atomicity and independent-writer races are documented constraints |
| `pilish-grammars.el` | Tree-sitter grammar recipes, install prompts, `M-x pilish-install-grammars` |
| `pilish-jsonl.el` | Pure core-only JSONL APIs: whole-file reading, canonical regex-first session metadata, sessions-root/cwd directory mapping, raw tree building and display projection, tool-call previews, and byte-preserving navigation target/line calculations. Production browsers consume these disk APIs rather than `get_tree`/`get_entries` RPCs |
| `pilish-evil.el` | Optional Evil keybindings; auto-loaded by `pilish--maybe-load-evil-integration` when a session is set up while Evil is present. Leaf module: requires `ui`, `input`, and `menu` directly (never the top-level feature, to avoid a recursive require during auto-load). Must byte-compile and load without Evil installed |

## Test Files

| File | Covers |
|------|--------|
| `test/pilish-core-test.el` | Core/RPC protocol, framing, request lifecycle, and JSON normalization |
| `test/pilish-ui-test.el` | Buffer naming/modes, session directories, direct browser key bindings, startup header, slash dispatch, and grammar install |
| `test/pilish-render-test.el` | Streaming/history response display, branch summaries, tools, tables, file actions, fontification, and diffs |
| `test/pilish-table-test.el` | Table decoration, overlays, streaming, resize |
| `test/pilish-input-test.el` | History, send/abort, queuing, completion, and local `/resume` browser routing |
| `test/pilish-menu-test.el` | Session transitions and cwd guards, canonical jsonl name metadata, transient browser entries, model/command actions, and reconnect |
| `test/pilish-browse-test.el` | Session/tree helpers, magit rendering and point restoration; asynchronous disk scans, cancellation/error states, search/filter/sort/scope, switch guards, rename, and deletion; disk tree loading, labels, and navigation guards/targets, atomic rewrite failures, prefill, and settle/quit behavior |
| `test/pilish-jsonl-test.el` | JSONL reading, canonical session metadata, raw tree/projection and golden fixtures, tool-call formatting, session discovery, navigation targets, and byte-preserving line reordering across branches/malformed input |
| `test/pilish-fake-pi-test.el` | Black-box fake subprocess contract: strict framing/events, valid v3 persistence, entry/tree/message RPC projections, transactional switching, and full resume/history choreography |
| `test/pilish-build-test.el` | Batch helper scripts for dependency and grammar installation |
| `test/pilish-evil-test.el` | Optional Evil integration: initial states, motion-state keymap registrations for chat and browser modes, snipe disabling, and the copy-raw-markdown default (all skip when Evil is absent) |
| `test/pilish-test.el` | Entry point / cross-module integration |
| `test/pilish-test-common.el` | Shared fixtures: mock-session macro, toolcall helpers, fake-pi launch helpers |
| `test/pilish-integration-test-common.el` | Shared integration backend helpers and contract macros |
| `test/pilish-integration-test-common-test.el` | Unit tests for shared integration helper macros |
| `test/pilish-integration-rpc-smoke-test.el` | Cheap shared fake/real RPC canaries |
| `test/pilish-integration-prompt-contract-test.el` | Shared fake/real prompt lifecycle + abort contracts |
| `test/pilish-integration-session-contract-test.el` | Shared fake/real session-file persistence contract |
| `test/pilish-integration-steering-contract-test.el` | Shared fake/real steering contract |
| `test/pilish-integration-tool-contract-test.el` | Shared fake/real tool execution contract |
| `test/pilish-integration-test.el` | Integration suite entry point (loads all shared contract modules) |
| `test/pilish-gui-tests.el` | GUI tests (require display or xvfb) |

## Other Files

| File | Purpose |
|------|---------|
| `Makefile` | Build, test, lint targets |
| `bench/pilish-bench.el` | Table rendering benchmark harness (xvfb GUI or batch) |
| `bench/run-bench.sh` | Table benchmark runner script; `--batch` for headless lane |
| `bench/pilish-reload-resume-bench.el` | Synthetic reload/resume harness; the resume lane opens the real async disk-backed session browser, selects the target magit section, switches through browser RET behavior, and checks rebuilt history |
| `bench/fake-pi-reload-resume.py` | Fake JSON-over-stdio backend for reload/resume benchmark state, switch, history, commands, and content-free traffic evidence |
| `bench/run-reload-resume-bench.sh` | Reload/resume benchmark runner; GUI uses `xvfb-run`, `--batch` for headless lane |
| `bench/pilish-tool-update-bench.el` | Synthetic tool-update storm and deferred agent_end cooling benchmark harness |
| `bench/fake-pi-tool-update-storm.py` | Fake JSON-over-stdio pi backend emitting tool-update storm and cooling scenarios |
| `bench/run-tool-update-bench.sh` | Tool-update/cooling benchmark runner; GUI uses `xvfb-run`, `--batch` for headless lane |
| `bench/pilish-stream-delta-bench.el` | Synthetic history plus coalesced text/thinking/toolcall stream benchmark harness |
| `bench/fake-pi-stream-delta.py` | Fake JSON-over-stdio backend for deterministic stream bursts and backlog |
| `bench/run-stream-delta-bench.sh` | Stream-delta benchmark runner; GUI uses `xvfb-run`, `--batch` for headless lane |
| `bench/fixtures/tables.md` | Sample pipe tables used by the table benchmark |
| `test/support/fake_pi.py` | Deterministic JSONL RPC subprocess double with scenario-driven events, valid v3 persistence, inspection RPCs, and transactional session switching |
| `test/support/fake-pi-contract.md` | Maintainer-facing wire, scenario/event/tool, v3 record, projection, and switch contract for `fake_pi.py` |
| `scripts/check.sh` | Pre-commit hook: byte-compile + lint + tests |
| `scripts/pilish-build.el` | Shared batch helpers for dependency and grammar installation |
| `scripts/install-deps.el` | Batch script: install required Emacs package dependencies |
| `scripts/install-ts-grammars.el` | Batch script: install tree-sitter grammars |

## Running Tests

Run all unit tests:
```bash
make test
```

The unit suite runs in batch only; running the full unit suite inside an
interactive (tty) Emacs is not supported.  Some table tests rely on the
batch visibility exemption in `pilish--chat-buffer-hidden-p`, and native
kill-buffer prompts on modified fixtures block unattended runs.  For
windowed behavior, use the GUI lane:
```bash
make test-gui
```

Run tests for a single module:
```bash
make test-core
make test-ui
make test-render
make test-input
make test-menu
make test-browse
make test-jsonl
make test-build
```

Run shared integration contracts:
```bash
make test-integration          # fake + real
make test-integration-fake     # fake only, fast
make test-integration-real     # real only
```

Run a filtered subset by ERT pattern:
```bash
make test SELECTOR=toolcall-delta
make test SELECTOR='abort\|followup'
make test-integration-fake SELECTOR=rpc-smoke
make test-integration-real SELECTOR=steering-contract
```

The `SELECTOR` value is exported unchanged and interpreted by ERT as an
Emacs regexp matched against test names.  For alternation, single-quote the
shell value and use one backslash as above; `SELECTOR='abort\\|followup'`
passes two backslashes and does not mean regexp alternation.

`make test` is intentionally terse on green runs (summary-focused output).
For full raw ERT output, use:
```bash
make test VERBOSE=1
make test VERBOSE=1 SELECTOR=toolcall-delta
```

When tests intentionally trigger minibuffer `message` output, capture/mock
`message` in the test and assert on it. This keeps batch logs concise
without losing behavioral coverage.

## Benchmarks

```bash
make bench                         # table GUI lane via xvfb (font metrics matter)
make bench-batch                   # table batch lane (no display, secondary)
make bench-reload-resume           # reload/resume GUI lane via xvfb (primary)
make bench-reload-resume-batch     # reload/resume batch lane (secondary)
make bench-reload-resume-smoke     # cheap synthetic correctness smoke
make bench-tool-update             # tool-update storm GUI lane via xvfb (primary)
make bench-tool-update-batch       # tool-update storm batch lane (secondary)
make bench-tool-update-smoke       # cheap synthetic correctness smoke
make bench-stream-delta            # stream-delta GUI lane via xvfb (primary)
make bench-stream-delta-batch      # stream-delta batch lane (secondary)
make bench-stream-delta-smoke      # cheap stream-delta correctness smoke
./bench/run-stream-delta-bench.sh --scenario toolcall -c 1 # long bash argument stream
make bench-agent-end-cooling       # deferred agent_end cooling GUI lane (primary)
make bench-agent-end-cooling-batch # deferred cooling batch lane (secondary)
make bench-agent-end-cooling-smoke # cheap deferred cooling correctness smoke
```

The GUI lanes are the primary measurements; batch lanes are quick sanity
checks and CI artifact generators.  Reload/resume benchmarks use synthetic
JSONL fixtures only and fail on correctness errors, not timing thresholds.
Tool-update storm benchmarks replay a deterministic synthetic
`tool_execution_update` storm against a fake pi and likewise fail only on
correctness errors.  Stream-delta benchmarks replay synthetic history before
measuring coalesced text/thinking bursts and a single-filter backlog.  The
`toolcall` preset streams a long bash command in 32-character argument deltas
over a roughly 450 KB transcript; smoke includes a shorter toolcall stream.
Toolcall paint counts and exact final command text are correctness checks,
not timing thresholds.  The
deferred agent_end scenario reuses the tool-update harness and fake backend to
cross a 90-overlay cohort at the final real process-filter event, then observes
production one-shot cooling timers and routed scroll
heartbeats without enforcing timing thresholds.  Its runner deliberately uses
`-Q`: slice/root timings are structural diagnostics, zero root calls is valid,
and these results must not be cited as evidence that md-ts root cost was
reduced.
Table fixtures live in `bench/fixtures/tables.md`.  Reload/resume artifacts are
written under `tmp/reload-resume-bench/`, tool-update artifacts under
`tmp/tool-update-bench/`, stream-delta artifacts under
`tmp/stream-delta-bench/{gui,batch,smoke}/`, and agent-end-cooling artifacts
under `tmp/agent-end-cooling-bench/{gui,batch,smoke}/` by default.  Runners
select lane directories when no `--out-dir` is given; dedicated smoke targets write
under `smoke/`, so public lanes do not overwrite one another.

## Linting

```bash
make lint              # checkdoc + package-lint
make lint-checkdoc     # docstring warnings only
make lint-package      # MELPA package conventions only
make check-parens      # verify balanced parentheses in all source files
make check             # byte-compile + lint + all tests (= pre-commit hook)
```

## Dependencies

`make test` auto-installs Emacs package deps (`transient`, `magit-section`, `md-ts-mode`) on first
run and caches via `.deps-stamp`. To force reinstall: `make clean` then `make test`.

## Pre-commit Hook

The git pre-commit hook runs `scripts/check.sh` (byte-compile + checkdoc +
package-lint + all unit tests, ~12s). Install with `make install-hooks`.

To skip for WIP commits: `git commit --no-verify`

## Tmux Testing (Spike Scripts)

For reproducing visual bugs or testing interactive behavior, write a spike
script in `./tmp/` (gitignored) and load it into Emacs inside tmux.

Every spike script needs this boilerplate at the top (use the actual
absolute path to your checkout):
```elisp
(setq inhibit-startup-screen t)
(add-to-list 'load-path "/absolute/path/to/pilish")
(require 'package)
(package-initialize)
(require 'pilish)
```

`package-initialize` is required here so installed dependencies like
`md-ts-mode` and `transient` are on `load-path` before `require` runs.

Launch with (from the project root):
```bash
tmux new-session -d -s test -x 120 -y 40 \
  "emacs -nw -Q -l $PWD/tmp/spike.el 2>tmp/spike.log"
sleep 2 && tmux capture-pane -t test -p
```

To start a full interactive `Pilish` session in tmux:
```bash
tmux new-session -d -s test -x 120 -y 40 \
  "emacs -nw -Q --eval \"(progn (require 'package) (package-initialize) \
    (add-to-list 'load-path \\\"$PWD\\\") \
    (require 'pilish) (pilish))\""
```

Common gotchas:
- **`-Q` is required** but skips package init — the boilerplate above fixes that
- **Sleep timing**: use `sleep 2` for UI ops, `sleep 10`+ for LLM responses
- **Buffer names** follow `*pilish-{chat,input}:<dir>*` (abbreviated),
  e.g. `*pilish-chat:~/co/pilish/*`
- **Window focus**: the `Pilish` layout has two windows; `C-x o` switches between them.
  Prefer spike scripts over interactive `tmux send-keys` when possible —
  they're reproducible, debuggable, and don't require tracking focus state

## Reference: pi CLI and RPC Protocol

The pi CLI (TypeScript) is the reference implementation. When implementing
new RPC commands, understanding event formats, or checking how the TUI
handles something, consult its source.

**Finding the checkout:** Look for a local clone, or clone one:
```bash
PI_MONO=$(find ~/co ~/src ~/projects /tmp -maxdepth 2 -name "pi-mono" -type d 2>/dev/null | head -1)
if [ -z "$PI_MONO" ]; then
  git clone git@github.com:badlogic/pi-mono.git /tmp/pi-mono
  PI_MONO=/tmp/pi-mono
fi
```

**Key files** (under `$PI_MONO/packages/coding-agent/`):

| File | When to consult |
|------|-----------------|
| `docs/rpc.md` | RPC command/event format, protocol overview |
| `src/modes/rpc/rpc-types.ts` | Type definitions for all RPC commands and events |
| `src/modes/rpc/rpc-mode.ts` | How RPC commands are dispatched and events emitted |
| `src/modes/interactive/interactive-mode.ts` | How the TUI handles events, tool display, streaming |
| `src/modes/interactive/components/tool-execution.ts` | TUI tool output rendering |
| `src/core/agent-session.ts` | Session lifecycle, forking, message handling |

**Other useful packages:**

| Path | Contents |
|------|----------|
| `$PI_MONO/packages/agent/src/agent-loop.ts` | Core agent loop, tool execution |
| `$PI_MONO/packages/agent/src/types.ts` | Agent-level type definitions |
| `$PI_MONO/packages/ai/src/types.ts` | AI provider types (messages, tools, content blocks) |

**When to look:** Before implementing a new RPC command, when an event format
is unclear, when the Emacs behavior should match the TUI, or when debugging
protocol mismatches.

## Git Hygiene

Always use `git add <specific-files>` instead of `git add -A`. The latter
stages everything including spike scripts, test artifacts, and `.pi/` files.
Run `git status` before committing to verify what's staged.

## Key Conventions

- All public symbols are prefixed `pilish-`
- Internal symbols use `pilish--` (double dash)
- Tests are named `pilish-test-<description>`
- Test files require `pilish-test-common` for shared fixtures
