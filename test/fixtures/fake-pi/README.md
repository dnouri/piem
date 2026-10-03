# fake-pi scenario fixtures

`test/support/fake_pi.py` loads one JSON file per named scenario from this directory.

Manual runs:

```bash
uv run --script test/support/fake_pi.py --scenario prompt-lifecycle
./test/support/fake_pi.py --scenario extension-confirm --extension-timeout-ms 10000
```

Current prompt kinds:

## `text_stream`

Streams one assistant reply in chunks, writes real session-file messages, supports
`abort`, and can deliver one queued `steer` turn after the current reply. Ordinary
prompt acceptance carries `data.disposition: "started"`; steering acceptance
carries `"queued"`.

Example:

```json
{
  "prompt": {
    "type": "text_stream",
    "assistant_text": "Fake reply for: {message}",
    "steer_assistant_text": "Steered fake reply for: {message}",
    "handled_input": "consume without a run",
    "chunk_count": 6,
    "delay_ms": 30,
    "echo_user": true
  }
}
```

Optional `handled_input` consumes that exact text for both `prompt` and `steer`
with `data.disposition: "handled"`, without messages, a run, user persistence, or
queue changes. The `input-dispositions.json` fixture covers this Pi 1.0 case.

A raw prompt during streaming must explicitly carry
`streamingBehavior: "steer"` to queue text into the same single slot. Missing
behavior fails; `"followUp"` is unsupported, as is the `follow_up` command.
The slot holds the last accepted text. `pendingMessageCount` stays 1 until it
is taken/cleared, including across message persistence. This does not change
Pilish's ordinary prompt sends or add an extension engine.

## `extension_dialog`

Emits an `extension_ui_request` before acknowledging the prompt and waits for a
matching `extension_ui_response`. Its custom-message output precedes the one
`data.disposition: "handled"` acknowledgment. No agent lifecycle events or
synthetic user entry are emitted, and the waiting worker is not streaming.
Only one dialog worker is supported; overlapping dialog prompts fail.
The scenario owns the default timeout, but manual
runs can override it with `--extension-timeout-ms <ms>`.  Pass `0` to disable
that timeout for tmux debugging.

Example:

```json
{
  "prompt": {
    "type": "extension_dialog",
    "command_name": "/test-confirm",
    "method": "confirm",
    "title": "Spike Confirm",
    "message": "Approve fake extension flow?",
    "timeout_ms": 100,
    "response_messages": {
      "confirmed": "CONFIRMED",
      "declined": "CANCELLED",
      "cancelled": "CANCELLED",
      "timeout": "TIMED OUT"
    }
  }
}
```

## `custom_message`

A slash-command scenario that optionally emits one visible custom message
without an agent run. Its custom-message output precedes the one
`data.disposition: "handled"` acknowledgment; the command is not persisted as a
user entry. `/test-noop` acknowledges as handled without emitting messages,
lifecycle events, or session entries. These fixtures replay extension-like
wire behavior; they do not execute extensions.

Example:

```json
{
  "prompt": {
    "type": "custom_message",
    "command_name": "/test-message",
    "message_text": "Test message from extension"
  }
}
```

## `tool_stream`

Emits the current delta-only lifecycle: `toolcall_start`, argument
`toolcall_delta` events, authoritative `toolcall_end` and assistant
`message_end`, tool execution events, a correlated tool-result message, and the
final streamed assistant message before `agent_end`, followed by `agent_settled`.

Example:

```json
{
  "prompt": {
    "type": "tool_stream",
    "tool_name": "read",
    "tool_args": {"path": "/tmp/example.txt"},
    "partial_result_text": "line 1\n",
    "result_text": "line 1\nline 2\n",
    "assistant_text": "Read complete",
    "delay_ms": 30,
    "echo_user": true
  }
}
```

## `nested_tools`

`nested-tools.json` is one bounded literal wire replay. Its `prompt` has only
`type: "nested_tools"` and `records`, an ordered array of wire objects. The
harness acknowledges the prompt as `started`, emits the initial agent/user
events, and replays the array unchanged. It persists assistant and parent
`toolResult` `message_end` payloads, not child execution events.

The fixture contains:

- one multiline codemode script and parent tool call with fixed id `parent`;
- read child `parent/1` (`/tmp/CHILD-READ`), error bash child `parent/2`
  (`CHILD-ERROR`), and late bash child `parent/3` (`CHILD-LATE`);
- a child update and a parent details-only update with `content: []`, three
  running `parent/?` placeholders, and display-string `args`;
- details-only model rows `parent/models.classify/1` (`fake/classifier`,
  `ok`, cost `0.002`) and `parent/models.generateImages/2` (`fake/image`,
  `cancelled`), with no fictional child event or result;
- a parent result carrying classifier usage and a saved `nestedCalls` snapshot
  with statuses `ok`, `error`, and `unfinished`;
- `FINAL-AFTER-PARENT`, `agent_end`, `agent_settled`, then the late child end.

Child 2's literal event arguments serialize to exactly 9000 UTF-8 bytes
(the command includes 4477 two-byte `é` characters). Its saved row omits
`arguments` and supplies `argumentsBytes: 9000`. Child 3 remains `unfinished`
in the saved snapshot. `complete: false` states that the snapshot is
incomplete; there is no count of missing calls. A late execution end does not
rewrite the parent result or disk. Codemode details may say `cancelled`, while
saved `nestedCalls` supports only `ok|error|unfinished`; they are different
records, not interchangeable status lists.

Fixed pacing is 30 ms per record plus a 500 ms post-settlement pause. Streaming
ends at settlement, but the one worker finishes only after replay. Abort,
new-session, and successful switch commands join that worker before success;
an overlapping nested prompt is rejected even in that idle-but-replaying
window. There are no templates, per-record directives, JavaScript execution,
tool scheduling, renderer imports, model service calls, or MCP runtime.

IDs, assistant metadata, timestamps, and `agent_end.messages` remain literal
on every replay. The separately echoed user is not templated into that array
(a bounded difference from a normal Pi run). This selected flow does not
claim to reproduce every callback of the displayed script. See the
[wire contract](../../support/fake-pi-contract.md#one-literal-nested-tool-replay)
for the source references and interruption limits.

## Assistant and command metadata

Generated assistant messages include the current session `thinkingLevel`
(default `"off"`), including tool-call and aborted messages. Their authoritative
live payloads, `get_messages`, and disk entries retain the field unchanged.
It is optional in upstream assistant messages: switching to an older disk
session without it remains supported and does not add the field.

Commands exposed by `get_commands` live in the top-level `commands` array and
use `name`, `source`, optional `description`, and optional `sourceInfo`. The
fake returns the supplied `sourceInfo` object literally, without expanding
paths or dropping extra fields. The `input-dispositions.json` fixture includes:

```json
{
  "name": "mcp",
  "source": "extension",
  "sourceInfo": {
    "path": "builtin:mcp",
    "source": "builtin",
    "scope": "temporary",
    "origin": "top-level"
  }
}
```

`builtin:mcp` names no file. This entry is discovery metadata only, not an
implemented fake MCP command or runtime.
