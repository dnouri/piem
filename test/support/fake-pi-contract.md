# Fake pi contract for deterministic frontend tests

This note defines the supported fake-pi surface used by deterministic tests.
The fake is a protocol double for the RPC subprocess boundary, not a mock
of internal Emacs functions. It targets Pi 1.0.0; references to older releases
below are historical comparisons, not compatibility guarantees.
The fake's persisted sessions deliberately omit the record kinds pi 0.86+
added (usage, context_edit, leading system message); tolerance for those
shapes is covered by the JSONL golden fixture tests.

## Scope and seam

The Emacs frontend already has the right seam:

- `pilish-executable`
- `pilish-extra-args`
- `pilish--start-process`
- the real process filter / sentinel / display handler path

The fake must enter through that seam unchanged. Test helpers may bind the
executable and extra args, but production startup code should not grow a
special fake-only branch.

## File layout

- Harness executable: `test/support/fake_pi.py`
- Harness contract note: `test/support/fake-pi-contract.md`
- Scenario fixtures / transcripts: `test/fixtures/fake-pi/`
- Scenario fixture format notes: `test/fixtures/fake-pi/README.md`
- One-off experiments: `tmp/`

The harness speaks strict JSONL on the wire with a small set of data-driven
prompt behaviors. Its one nested-tool scenario replays literal wire records;
it is not a general event-program framework.

## Why this fake exists

Two distinct risks need coverage:

1. The Emacs frontend must keep working against a pi-like RPC subprocess.
2. The real pi CLI may drift from the double.

So the fake is for deterministic GUI and integration scenarios, while a
thinner real-backend suite remains as a compatibility backstop.

## Current slow-test value review

### Still boundary-valuable

These are still worth covering at the real subprocess boundary:

- Integration RPC smoke:
  - process spawn / lifecycle
  - `get_state`
  - `get_commands`
  - `new_session`
  - `get_fork_messages`
- Integration prompt lifecycle:
  - ordinary `prompt` acceptance with `data.disposition: "started"` plus streamed events
  - `agent_start` / `message_start` / `message_update` / `message_end` / `agent_end` / `agent_settled`
  - idle state after completion
  - persisted message count change
- Integration distinct behaviors:
  - `abort`
  - `steer`
  - session-name persistence through a real session file
- GUI-only regressions:
  - follow-scroll when the window is already at end
  - preserve scroll while scrolled up
  - visible tool rendering / overlay boundaries
  - extension UI round-trips in a real chat buffer

### Already strongly shadowed by unit coverage

These have strong direct coverage outside slow suites and should only stay in
GUI/integration form when they still prove a real boundary risk:

- linked chat/input buffer kill behavior
- many markdown / fence / blank-line rendering rules
- most extension UI method dispatch details
- menu / command list shaping logic after `get_commands`

## Wire-level rules the fake must obey

- Strict JSONL with `\n` as the record delimiter
- Accept optional trailing `\r` on input lines
- Flush each output record promptly
- Successful `prompt` responses carry `data.disposition: "started"|"queued"|"handled"`
- Successful `steer` responses carry `data.disposition: "queued"|"handled"`
- Ordinary fake agent prompts acknowledge before streamed events; handled extension
  commands acknowledge after their work, so dialog/custom-message events can precede
  the response. There is exactly one correlated response per accepted command
- Ordinary stream events are uncorrelated; `extension_ui_request` carries its
  dialog id
- Responses use `type: "response"` and mirror the request `id` when present
- Unsupported commands should fail loudly with `success: false`

## Supported command surface

The current fake supports:

- `get_state`
- `get_commands`
- `prompt`
- `abort`
- `clear_queue`
- `steer`
- `new_session`
- `get_fork_messages`
- `get_entries`
- `get_tree`
- `get_messages`
- `switch_session`
- `set_session_name`
- `set_model`
- `set_thinking_level`
- `extension_ui_response`

`follow_up` and queued `prompt` with `streamingBehavior: "followUp"` are
explicitly rejected. Conversation navigation/mutation RPCs,
compaction/retry/bash RPCs, session listing, export, and HTML remain out of
scope.

## Supported event surface

Required now:

- `agent_start`
- `agent_end`
- `agent_settled`
- `queue_update` (the empty queue notification from `clear_queue`)
- `message_start`
- `message_update`
- `message_end`
- `tool_execution_start`
- `tool_execution_update`
- `tool_execution_end`
- `extension_ui_request`

The fake does not need `turn_start`, `turn_end`, retry, compaction, or other
higher-level events until a test genuinely needs them.

## Required fields by surface

### `get_state`

Fields the current Emacs code or assertions actively read:

- `model` (the fake model advertises `input: ["text", "image"]`)
- `thinkingLevel`
- `isStreaming`
- `isCompacting`
- `sessionId`
- `sessionFile`
- `messageCount`
- `pendingMessageCount`

Useful for fidelity but not currently required by the Emacs frontend:

- `sessionName`
- `steeringMode`
- `followUpMode`
- `autoCompactionEnabled`

### `get_commands`

Required shape:

- response `data.commands` must be a JSON array
- each command used by assertions needs at least:
  - `name`
  - `source`

`description` and `sourceInfo` may be omitted in scenario data. Supplied
`sourceInfo` is returned unchanged, including `path`, `source`, `scope`,
`origin`, and any extra fields. Synthetic paths such as `builtin:mcp` name no
file and are not expanded. Command `source` describes the command kind, not
its resource source: the `input-dispositions` fixture advertises `mcp` with
command source `extension` and resource source `builtin`, scope `temporary`,
and origin `top-level`. This is discovery metadata only; the fake does not
implement an MCP command or runtime.

### `prompt` happy path

Required behavior:

1. send success response with `data: {"disposition": "started"}`
2. later emit `agent_start`
3. emit `message_start`
4. emit one or more `message_update` events with
   `assistantMessageEvent.type: "text_delta"`
5. emit `message_end`
6. emit `agent_end`
7. expose idle state and emit `agent_settled` only after all continuations finish
8. persist enough session data to back session-file and `messageCount` assertions

`agent_end` is a low-level completion boundary, not permission to send a new
prompt. Consumers waiting for the whole run must wait for `agent_settled`.
Completed tool finalization and cooling in the frontend still happen at
`agent_end`.

A `prompt` may include `images`, which must be a JSON array.  Every item must
be an object with `type: "image"`, nonempty string `data`, and nonempty string
`mimeType`.  The fake validates only this upstream RPC shape: it neither
decodes base64 nor restricts MIME values.  Valid blocks are detached from the
request and persisted/emitted after the prompt's text block in request order.

For `text_stream`, images belong only to the initial user turn; steering is
text-only and image-bearing `steer` commands fail.  `tool_stream` preserves
prompt images on its ordinary user message, as does the initial user echo in
`nested_tools`.  The extension-owned
`extension_dialog` and `custom_message` prompt behaviors reject nonempty image
arrays before reporting prompt success.  No new scenario type is implied.

### Assistant thinking metadata

Generated text, tool-call, final, and aborted assistant messages include the
current session `thinkingLevel` (default `"off"`). The authoritative
`message_end` and `agent_end` payloads, `get_messages`, and persisted message
entries retain the same field. Upstream makes this field optional for legacy
or unmanaged responses: switched disk messages without it remain valid and
keep its absence, rather than inheriting the current session level.

### Input dispositions and bounded steering

The `input-dispositions` fixture consumes one exact `handled_input` text as
`handled` for either `prompt` or `steer`, before any busy/no-run rejection.
It emits no messages or agent lifecycle events and persists no user entry.
This is a data-driven interception case, not an extension runtime.

During streaming, an ordinary raw `prompt` must explicitly carry
`streamingBehavior: "steer"` to return `queued`; without it, the command fails.
Only the existing text-stream steering slot is supported. Nonempty prompt
images cannot enter that slot. A normal `steer` during a text stream also
returns `queued`; intercepted steering returns `handled` without altering the
slot. The fake still rejects normal steering when idle.

The slot holds the last accepted text, not a full queue. Its
`pendingMessageCount` is 1 while occupied and 0 after take/clear; persisting
messages does not change it. Pilish's production sends are unchanged: it
uses ordinary `prompt`, without a `streamingBehavior` option, for its local FIFO.

### Abort and queue clearing

`clear_queue` removes the pending text-only steering message and returns
`data: {"steering": ["removed text"], "followUp": []}` (empty arrays when
nothing was queued). It emits `queue_update` with empty arrays before its
correlated response. The fake does not model a complete queue-update stream.

`abort` waits for the worker to settle before acknowledging. Like Pi, abort
alone does not discard a pending steering continuation: the text-stream
scenario emits the aborted low-level `agent_end`, starts that continuation,
and emits exactly one `agent_settled` after the final run. Sending `clear_queue`
before `abort` prevents that continuation. The existing fake still has one
pending steering slot, not a general follow-up queue. The fake's stop
acknowledgment is a worker-join boundary, not a guarantee about real Pi's
abort acknowledgment ordering.

### Tool execution path

For deterministic GUI and benchmark tests, the fake must emit the current
lifecycle:

1. an assistant `message_start` with pending, empty content;
2. delta-only `message_update` events for `toolcall_start`, optional
   `toolcall_delta`, and authoritative `toolcall_end`;
3. the authoritative assistant `message_end` before execution starts;
4. `tool_execution_start`, optional updates with accumulated `partialResult`,
   and `tool_execution_end`;
5. a correlated `toolResult` message; and
6. the final assistant response before `agent_end`, then `agent_settled`.

Every `message_update` carries cumulative `usage`, and carries neither the
legacy top-level `message` nor nested `partial` fields. `toolcall_start`
carries `id` and `toolName`; `toolcall_end.toolCall` remains authoritative.
The renderer's missing-metadata reconciliation handles incomplete events
defensively; it does not promise support for below-minimum Pi releases.

Required fields currently consumed by Emacs rendering:

- `toolCallId`
- `toolName`
- `args`
- `partialResult`
- `result`
- `isError`

### One literal nested-tool replay

`nested-tools.json` uses `prompt.type: "nested_tools"` and an ordered
`prompt.records` array of literal wire objects. The harness acknowledges with
`started`, emits `agent_start` and the initial persisted user echo, then
replays those objects unchanged. Only authoritative assistant and parent
`toolResult` `message_end` payloads append conversation entries. Child tool
execution events do not create messages or disk entries.

This fixture pins two different upstream records:

- Nested execution events carry the immediate `parentToolCallId: "parent"`.
  Three real children use `parent/1` (read `/tmp/CHILD-READ`), `parent/2`
  (bash `CHILD-ERROR`), and `parent/3` (bash `CHILD-LATE`).
- Codemode's parent details-only update has `content: []` and display-string
  `args`. Its running tool rows share the placeholder `parent/?`. Model rows
  use `parent/models.classify/1` (`fake/classifier`, `ok`, cost `0.002`) and
  `parent/models.generateImages/2` (`fake/image`, `cancelled`). These are
  details only, not fictional child tool events or tool results. The classifier
  usage is retained on the parent result.

The parent result message already carries the authoritative `nestedCalls`
snapshot at `message_start`; the same payload is persisted at `message_end`.
Saved rows use `arguments` or `argumentsBytes`, and only `ok`, `error`, or
`unfinished`. The error child's compact argument JSON is exactly 9000 UTF-8
bytes, so that saved row has `argumentsBytes: 9000` and no `arguments`. The
late child is saved as `unfinished`. Both conditions make `complete: false`;
there is no `totalCount` or omitted-call count. The late child's real error end
follows `FINAL-AFTER-PARENT`, `agent_end`, and `agent_settled`, without rewriting
that snapshot or any disk bytes.

`isStreaming` stays true through tool results and `agent_end`, then becomes
false at the literal `agent_settled`. The existing worker remains owned until
playback actually returns. `abort`, `new_session`, and successful
`switch_session` join it even when streaming is already false, so no old
records arrive after their success response. A second nested prompt while
that worker is alive is rejected; this bounded fake does not model overlapping
runs. Interrupted playback drops the remaining records. Before the literal
`agent_end`, it closes the lifecycle with one end and one settlement. After
that end but before settlement, it emits only the owed `agent_settled`, never
another `agent_end`; after settlement it emits neither. It does not manufacture
child cancellations, results, or partial messages.

Playback uses fixed 30 ms spacing and a 500 ms pause after settlement so tests
can inspect disk or stop the late replay. These are fixture pacing, not Pi
ordering/timing guarantees. IDs, assistant metadata, message timestamps, and
`agent_end.messages` are literal; the latter contains the fixture's two
assistant messages and parent result, not the separately echoed input. Real
Pi includes the initial prompt in a normal run's `agent_end.messages`. No
input, model, or thinking substitutions occur inside the records. This is one
representative wire flow, not a complete capture of every callback a script
would generate. The script, tools, model calls, extensions, and MCP never run.

Source authority: `~/co/pi-mono` at `v1.0.0` (`a13d35a74`), especially
`packages/coding-agent/src/core/nested-tool-calls.ts`,
`src/core/agent-session.ts:1075–1135`, `src/extensions/codemode/tool.ts:103–126`,
`src/extensions/codemode/execute.ts:342–400,572–588`, and
`packages/ai/src/types.ts:573–607`. Saved summaries are copies; late execution
completion does not mutate the message's snapshot. The source's limits are
256 calls, 8192 bytes per call, 32768 argument bytes total, and 500 error
characters; this fixture exercises per-call omission and unfinished work,
not every limit boundary.

### Nested frontend acceptance

`pilish-fake-pi-test-nested-tools-live-history-contract` starts this literal
fixture through a real Pilish session and its production process filter/display
handler, then renders `get_messages` in a fresh chat buffer through the ordinary
history seam. The shared visible-summary selector compares the completed read
and error rows, excluding only duration spans. It does not deduplicate rows or
remove status/error text. Full visible summaries occur once per child; the
three tool rows precede the two model rows, and the late row stays before
`FINAL-AFTER-PARENT`. Native JSON argument bytes are decoded as UTF-8 before
character-oriented display escaping, so non-ASCII live arguments agree with
saved details previews.

Both views retain real script newlines and the honest
`Incomplete saved call summary` footer, without an invented omitted-call count.
The fixture's late **error** is visible live, while the disk snapshot and
reloaded row stay **unfinished when saved**. W2's separate renderer regression
covers late success; this literal fixture must not be changed to manufacture it.
Received child output opens through public TAB live. History instead shows
`Child outputs are not saved in sessions.` and the separate
`saved arguments omitted (9000 bytes)` suffix. Neither is part of the summary
comparison. This boundary test is deterministic frontend acceptance, not proof
that a real codemode script or model ran.

### Fork messages

Required shape and semantics:

- response `data.messages` is a JSON array
- each item is exactly `{ "entryId": ENTRY_ID, "text": TEXT }`
- entries cover every raw user-message record with nonempty text in append
  order, including users on abandoned branches (matching Pi's fork selector)
- `entryId` is the raw session entry id; `text` concatenates textual content
  blocks (or passes through string content)

### Session naming

Required behavior:

- `set_session_name` requires a string, collapses CR/LF runs to one space,
  trims it, and succeeds only when the result is nonempty
- naming appends a complete `session_info` entry with `id`, `parentId`,
  `timestamp`, and `name`; `get_state.sessionName` updates immediately
- before the first conversation message, that entry stays in memory without
  creating the allocated session file; after materialization it appends to disk
- latest `session_info` wins; whitespace is trimmed and a blank/null latest name
  clears `sessionName`

### Extension UI

Required request methods for current test coverage:

- `confirm`
- `input`
- `select`
- `editor`
- fire-and-forget methods the frontend already handles, especially
  `notify`, `setStatus`, and `set_editor_text`

Required response shape:

- `type: "extension_ui_response"`
- matching request `id`
- one of `confirmed`, `value`, or `cancelled`

The dialog request precedes the prompt acknowledgment. The fake's existing
worker waits without setting `isStreaming` or emitting `agent_start`,
`agent_end`, or `agent_settled`. After the answer (or timeout/cancellation), its
custom-message output precedes the one `handled` response. Command text is not
persisted as a user message. Overlapping dialogs are rejected: this double has
one worker, not a concurrent extension engine. Stop/switch joins that worker,
so any custom output and pending prompt acknowledgment precede stop/switch success.

Likewise, `/test-message` emits its custom message before `handled` acceptance,
while `/test-noop` emits no messages, lifecycle events, or session entries.

Timeouts for dialog requests should be explicit scenario data, not hidden magic
constants in the harness. Fast defaults are good for automated tests, but the
manual-debugging path should be able to extend or disable those timeouts from
the CLI so a human can inspect the UI before responding.

## Valid v3 session files and inspection RPCs

Fresh startup and `new_session` allocate a string `sessionFile` path and a v3
header in memory, but leave the file absent. Naming and custom-only messages
update the in-memory entries and inspection RPCs without writing bytes. The
first user **or assistant** message creates the file exclusively and flushes
the header plus all accumulated entries once. Later entries append normally.
This keeps the first user on disk even if its assistant never completes.
File existence is checked under the existing session lock; there is no second
persistence-state owner.

Materialized files use strict LF-delimited UTF-8 JSONL. The switch loader also
accepts blank lines and an optional CR before LF, but every nonblank line must
be strict JSON. Real Pi emits `message_end` before appending that message;
consumers must poll disk bytes rather than assume the event is a write barrier.
The fake's ordinary messages are persisted before their end events, so the
shared contract tests explicitly allow the real ordering.

### Header and entry invariants

The first nonblank record is exactly one current-version header with this base
shape:

```json
{"type":"session","version":3,"id":"SESSION_ID","timestamp":"2026-02-03T04:05:00.000Z","cwd":"/absolute/path"}
```

`id` is nonempty, `cwd` names an existing absolute process-local path without
NUL, and `timestamp` is a valid UTC timestamp in the exact
`YYYY-MM-DDTHH:MM:SS.mmmZ` form.

Every later record is a nonheader entry with the base fields:

```json
{"type":"session_info","id":"ENTRY_ID","parentId":null,"timestamp":"2026-02-03T04:05:01.000Z","name":"Example"}
```

Entry ids are nonempty and unique among nonheader entries.  `parentId` must be present
and is either any string or JSON null.  Parent references need not precede the
entry or resolve, so crafted branches and orphans are supported.  Entry
timestamps use the same strict UTC form.  Generated entries form a linear
chain, use monotonic timestamps, and parent each append to the previous current
leaf.  The current `leafId` is the id of the physically last nonheader entry,
including bookkeeping entries; it is JSON null for a header-only session.

Accepted entry types and required payloads are:

- `message`: `message` object with a string `role`
- `thinking_level_change`: string `thinkingLevel`
- `model_change`: string `provider` and `modelId`
- `compaction`: string `summary`, string `firstKeptEntryId`, and finite numeric
  (non-boolean) `tokensBefore`
- `branch_summary`: string `summary` and string `fromId`
- `custom`: string `customType`
- `custom_message`: string `customType`, boolean `display`, and string-or-array
  `content`; optional `details` is preserved
- `label`: string `targetId` and an optional string/null `label`
- `session_info`: optional string/null `name`

Unknown extra fields are retained.  Unsupported record types, malformed
required payloads, duplicate ids, invalid timestamps, non-v3 headers, and
non-UTF-8 or malformed JSONL make a nonempty switch target invalid.  This
strict switch subset is intentional: Pi 0.84.2 can migrate older versions and
skips some malformed JSONL records, while the fake keeps deterministic
transactional failure for test-crafted targets.

### `get_entries`

The request has no payload beyond optional `since: ENTRY_ID`.  Success is:

```json
{"type":"response","command":"get_entries","success":true,"data":{"entries":[],"leafId":null}}
```

`entries` preserves physical append order and excludes the header.  With
`since`, it contains entries strictly after that raw id while `leafId` remains
the session's current leaf.  `since` must be a string naming an existing entry;
a wrong type or unknown id returns `success:false` with `error` and no `data`.
An empty session returns `entries:[]` and `leafId:null`.

### `get_tree`

Success is:

```json
{"type":"response","command":"get_tree","success":true,"data":{"tree":[],"leafId":null}}
```

Every raw nonheader entry, including `label`, `session_info`, `custom`, branch
summary, and compaction bookkeeping, appears once as a node.  Roots are null,
self-parented, unknown-parent/orphan, or defensive cycle-break nodes.  Roots
keep append order; each child array is stably sorted by parsed entry timestamp,
with append order breaking ties.

Labels are folded over all label records in append order.  The latest nonempty
label for a target adds `label` and `labelTimestamp` to that target's node;
an omitted, null, or empty latest label clears both fields.  Label records
remain ordinary tree nodes.  `leafId` is still the raw physical leaf, not a
projected visible node.

### `get_messages`

Success has this shape:

```json
{"type":"response","command":"get_messages","success":true,"data":{"messages":[]}}
```

The fake walks parent ids from the current leaf with cycle protection, reverses
that chain to active-path order, and excludes abandoned siblings.  It then applies
the latest active-path compaction: emit that compaction summary first, retain
the pre-compaction range beginning at `firstKeptEntryId` when present, then
include entries after the compaction.

Projection semantics are:

- `message` contributes its `message` payload; like Pi 0.84.2, a user,
  assistant, or tool-result payload with null/missing `content` gets `content:[]`
- `custom_message` contributes role `custom` with `customType`, `content`,
  `display`, optional `details`, and the entry timestamp converted to Unix
  milliseconds
- a nonempty `branch_summary` contributes role `branchSummary` with `summary`,
  `fromId`, and millisecond timestamp
- `compaction` contributes role `compactionSummary` with `summary`,
  `tokensBefore`, and millisecond timestamp
- labels, session info, raw `custom`, model changes, and thinking-level changes
  contribute no message

`get_state.messageCount` is the length of this same projected message array.
A header-only session returns `messages:[]`.

### `switch_session`

The request must carry a nonempty, NUL-free absolute string `sessionPath`.
This is the frontend-facing subset: Pi 0.84.2 also resolves relative paths,
but the Emacs switch choreography always sends an absolute process-local path.
A successful switch returns:

```json
{"type":"response","command":"switch_session","success":true,"data":{"cancelled":false}}
```

An existing nonempty target is fully parsed and validated as v3 before the
current run is stopped or any in-memory session state changes.  On success the
fake installs the target's raw entries, current leaf, projected messages, all
raw fork users, latest name, id, path, and projected message count.  Switching
to the same path reloads after stopping an active run so an authoritative
aborted append is not lost.

Invalid targets return `success:false` with `error` and no `data`: this includes
non-string, relative, empty-string, or NUL paths; directories or non-regular files; and
nonempty malformed or invalid-v3 files.  Such failures are transactional: the
current session state and active worker remain unchanged.

A deliberate deterministic initialization rule supports resume edge tests: a
nonexistent absolute target and an existing zero-byte regular file are
materialized as a valid header-only v3 session, then selected.  Missing parent
directories are created.  The result has empty entries/tree/messages, null
leaf, no session name, and message count zero. This explicitly materialized
switch target is a deliberate deviation from Pi 1.0.0: Pi initializes an
existing empty file but leaves a missing file absent until its first
conversation message, and runtime setup may append bookkeeping entries.
The fake's header-only result is the bounded edge-test contract, distinct
from normal startup and `new_session`. Subsequent entries append to these
materialized switch targets even without a conversation.

## Backend helper API

Keep backend choice explicit in tests.

The shared helper returns a backend plist with:

- backend symbol: `real` or `fake`
- backend label for failure output
- executable command list
- extra args
- optional fake scenario name

The important design point is visibility: a failing test should say which
backend and which scenario was running.

## Intentionally out of scope

The fake should not try to model all of pi.  Out of scope until a concrete test
needs it:

- full prompt/template/skill expansion fidelity
- conversation navigation, branch creation, and mutation RPCs beyond switching
- compaction command and retry flows (persisted compaction projection is covered)
- bash RPC command semantics
- session listing across projects
- provider/model discovery parity with the real backend
- extension runtime behavior beyond the RPC UI sub-protocol
- every event and field documented in upstream `rpc.md`

