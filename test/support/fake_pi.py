#!/usr/bin/env -S uv run --no-project --script
# /// script
# requires-python = ">=3.13"
# ///
"""Fake pi RPC harness for deterministic frontend tests.

This script is a small protocol double for pi's JSONL RPC mode.  It keeps the
wire contract strict while offering a tiny, data-driven scenario layer.

Manual usage examples:

    uv run --script test/support/fake_pi.py --scenario prompt-lifecycle
    ./test/support/fake_pi.py --scenario extension-confirm \
        --extension-timeout-ms 10000 --log-file /tmp/fake-pi.log

Scenario files live in ``test/fixtures/fake-pi/`` and currently support five
prompt behaviors:

``text_stream``
    Streams a simple assistant text reply in chunks and supports queued
    ``steer`` messages plus mid-stream ``abort``.

``extension_dialog``
    Emits an ``extension_ui_request`` and waits for a matching
    ``extension_ui_response``.  The timeout is scenario data and can be
    overridden (or disabled with ``--extension-timeout-ms 0``) for manual tmux
    debugging.

``custom_message``
    A slash command that optionally emits one visible custom message without a
    full assistant turn.

``tool_stream``
    Emits the streamed tool-call and tool-execution event surface, then ends
    with optional assistant text.

``nested_tools``
    Replays one literal nested-tool wire flow, including a parent result
    snapshot and a child end after settlement.  It executes no tools or scripts.
"""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
import math
import re
import sys
import tempfile
import threading
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from collections.abc import Callable, Iterator
from typing import Any, BinaryIO, Literal, cast

JsonDict = dict[str, Any]
DialogMethod = Literal["confirm", "input", "select", "editor"]


@dataclass(frozen=True)
class SlashCommand:
    """A slash command returned by ``get_commands``."""

    name: str
    source: Literal["extension", "prompt", "skill"]
    description: str | None = None
    source_info: JsonDict | None = None

    def to_rpc(self) -> JsonDict:
        """Return this command with its literal discovery metadata."""
        data: JsonDict = {"name": self.name, "source": self.source}
        if self.description is not None:
            data["description"] = self.description
        if self.source_info is not None:
            data["sourceInfo"] = self.source_info
        return data


@dataclass(frozen=True)
class PromptImageContent:
    """Validated immutable image content from one prompt command."""

    data: str
    mime_type: str

    def to_rpc(self) -> JsonDict:
        """Return a fresh RPC content block."""
        return {"type": "image", "data": self.data, "mimeType": self.mime_type}


@dataclass(frozen=True)
class TextStreamPrompt:
    """Scenario data for a simple streamed text reply."""

    type: Literal["text_stream"]
    assistant_text: str
    chunk_count: int = 4
    delay_ms: int = 30
    echo_user: bool = True
    steer_assistant_text: str | None = None
    handled_input: str | None = None


@dataclass(frozen=True)
class ExtensionDialogPrompt:
    """Scenario data for an extension dialog round-trip."""

    type: Literal["extension_dialog"]
    command_name: str
    method: DialogMethod
    title: str
    message: str | None = None
    placeholder: str | None = None
    options: list[str] = field(default_factory=list)
    prefill: str | None = None
    timeout_ms: int | None = None
    response_messages: dict[str, str] = field(default_factory=dict)


@dataclass(frozen=True)
class CustomMessagePrompt:
    """Scenario data for a slash command that may emit one custom message."""

    type: Literal["custom_message"]
    command_name: str
    message_text: str | None = None


@dataclass(frozen=True)
class ToolStreamPrompt:
    """Scenario data for a streamed tool execution flow."""

    type: Literal["tool_stream"]
    tool_name: str
    tool_args: JsonDict
    partial_result_text: str = ""
    result_text: str = ""
    assistant_text: str = ""
    delay_ms: int = 30
    echo_user: bool = True


@dataclass(frozen=True)
class NestedToolPrompt:
    """Scenario data for one literal nested-tool wire replay."""

    type: Literal["nested_tools"]
    records: tuple[JsonDict, ...]


PromptBehavior = (
    TextStreamPrompt
    | ExtensionDialogPrompt
    | CustomMessagePrompt
    | ToolStreamPrompt
    | NestedToolPrompt
)


@dataclass(frozen=True)
class Scenario:
    """Fully parsed scenario fixture."""

    name: str
    description: str
    commands: list[SlashCommand]
    prompt: PromptBehavior


@dataclass
class SessionState:
    """Mutable fake session state exposed by ``get_state``."""

    model: JsonDict
    thinking_level: str = "off"
    is_streaming: bool = False
    is_compacting: bool = False
    steering_mode: Literal["all", "one-at-a-time"] = "one-at-a-time"
    follow_up_mode: Literal["all", "one-at-a-time"] = "one-at-a-time"
    session_id: str = ""
    session_file: str = ""
    session_name: str | None = None
    auto_compact_enabled: bool = False
    message_count: int = 0
    pending_message_count: int = 0

    def to_rpc(self) -> JsonDict:
        """Return this state in RPC response shape."""
        data: JsonDict = {
            "model": self.model,
            "thinkingLevel": self.thinking_level,
            "isStreaming": self.is_streaming,
            "isCompacting": self.is_compacting,
            "steeringMode": self.steering_mode,
            "followUpMode": self.follow_up_mode,
            "sessionFile": self.session_file,
            "sessionId": self.session_id,
            "autoCompactionEnabled": self.auto_compact_enabled,
            "messageCount": self.message_count,
            "pendingMessageCount": self.pending_message_count,
        }
        if self.session_name is not None:
            data["sessionName"] = self.session_name
        return data


def now_ms() -> int:
    """Return the current Unix timestamp in milliseconds."""
    return int(time.time() * 1000)


def iter_jsonl_commands(stream: BinaryIO) -> Iterator[JsonDict]:
    """Yield strict LF-delimited JSON objects from ``stream``.

    This intentionally reads bytes and splits on ``b"\n"`` only, because
    Python's text-mode line iteration treats lone ``\r`` as a newline and would
    drift from pi's RPC framing contract. EOF does not terminate an incomplete
    record: without a final LF, the trailing bytes are ignored.
    """
    buffer = b""
    while chunk := cast(Any, stream).read1(4096):
        buffer += chunk
        while True:
            newline_index = buffer.find(b"\n")
            if newline_index == -1:
                break
            line = buffer[:newline_index]
            buffer = buffer[newline_index + 1 :]
            if line.endswith(b"\r"):
                line = line[:-1]
            if not line:
                continue
            yield json.loads(line.decode("utf-8"))


def default_scenario_dir() -> Path:
    """Return the default fixture directory for fake-pi scenarios."""
    return Path(__file__).resolve().parent.parent / "fixtures" / "fake-pi"


def load_scenario(path: Path, name: str) -> Scenario:
    """Load and validate a scenario fixture from ``path``."""
    data = json.loads(path.read_text(encoding="utf-8"))
    commands = []
    for item in data.get("commands", []):
        commands.append(
            SlashCommand(
                name=item["name"],
                source=item["source"],
                description=item.get("description"),
                source_info=item.get("sourceInfo"),
            )
        )
    prompt_data = data["prompt"]
    prompt_type = prompt_data["type"]
    if prompt_type == "text_stream":
        prompt: PromptBehavior = TextStreamPrompt(
            type="text_stream",
            assistant_text=prompt_data["assistant_text"],
            chunk_count=int(prompt_data.get("chunk_count", 4)),
            delay_ms=int(prompt_data.get("delay_ms", 30)),
            echo_user=bool(prompt_data.get("echo_user", True)),
            steer_assistant_text=prompt_data.get("steer_assistant_text"),
            handled_input=prompt_data.get("handled_input"),
        )
    elif prompt_type == "extension_dialog":
        prompt = ExtensionDialogPrompt(
            type="extension_dialog",
            command_name=prompt_data["command_name"],
            method=prompt_data["method"],
            title=prompt_data["title"],
            message=prompt_data.get("message"),
            placeholder=prompt_data.get("placeholder"),
            options=list(prompt_data.get("options", [])),
            prefill=prompt_data.get("prefill"),
            timeout_ms=(
                int(prompt_data["timeout_ms"])
                if prompt_data.get("timeout_ms") is not None
                else None
            ),
            response_messages=dict(prompt_data.get("response_messages", {})),
        )
    elif prompt_type == "custom_message":
        prompt = CustomMessagePrompt(
            type="custom_message",
            command_name=prompt_data["command_name"],
            message_text=prompt_data.get("message_text"),
        )
    elif prompt_type == "tool_stream":
        prompt = ToolStreamPrompt(
            type="tool_stream",
            tool_name=prompt_data["tool_name"],
            tool_args=dict(prompt_data["tool_args"]),
            partial_result_text=prompt_data.get("partial_result_text", ""),
            result_text=prompt_data.get("result_text", ""),
            assistant_text=prompt_data.get("assistant_text", ""),
            delay_ms=int(prompt_data.get("delay_ms", 30)),
            echo_user=bool(prompt_data.get("echo_user", True)),
        )
    elif prompt_type == "nested_tools":
        prompt = NestedToolPrompt(
            type="nested_tools", records=tuple(prompt_data["records"])
        )
    else:
        raise ValueError(f"Unsupported prompt type: {prompt_type}")
    return Scenario(
        name=name,
        description=data.get("description", name),
        commands=commands,
        prompt=prompt,
    )


class FakePiHarness:
    """Protocol double that speaks pi's JSONL RPC protocol."""

    def __init__(
        self,
        *,
        scenario: Scenario,
        session_dir: str | None,
        log_file: str | None,
        extension_timeout_ms: int | None,
        pre_start_delay_ms: int,
        split_responses: dict[str, int],
    ) -> None:
        self.scenario = scenario
        self.log_file = Path(log_file) if log_file else None
        self.extension_timeout_ms = extension_timeout_ms
        self.pre_start_delay_ms = pre_start_delay_ms
        self.split_responses = split_responses
        self._write_lock = threading.Lock()
        self._session_lock = threading.RLock()
        self._abort_requested = threading.Event()
        self._extension_waiter = threading.Event()
        self._extension_response: JsonDict | None = None
        self._pending_extension_id: str | None = None
        self._pending_steer_message: str | None = None
        self._run_thread: threading.Thread | None = None
        self._message_serial = 0
        self._last_entry_timestamp_ms = 0
        self._session_header: JsonDict = {}
        self._session_entries: list[JsonDict] = []
        self._entry_by_id: dict[str, JsonDict] = {}
        self._leaf_id: str | None = None
        self._session_root_dir = tempfile.TemporaryDirectory(
            prefix="fake-pi-", dir=session_dir
        )
        self._session_root = Path(self._session_root_dir.name)
        model = {
            "id": "fake-model",
            "name": "Fake Model",
            "provider": "fake",
            "api": "fake-api",
            "contextWindow": 8192,
            "maxTokens": 1024,
            "input": ["text", "image"],
        }
        self.state = SessionState(model=model)
        self.user_messages: list[dict[str, str]] = []
        self._reset_session_file()

    def run(self) -> int:
        """Process stdin commands until EOF."""
        try:
            for command in iter_jsonl_commands(sys.stdin.buffer):
                self.handle(command)
            return 0
        finally:
            self._stop_active_run()
            self._session_root_dir.cleanup()

    def handle(self, command: JsonDict) -> None:
        """Handle a single RPC command."""
        self._log("in", command)
        command_type = command["type"]
        match command_type:
            case "get_state":
                with self._session_lock:
                    data = self.state.to_rpc()
                self._respond(command, data=data)
            case "get_commands":
                self._respond(
                    command,
                    data={
                        "commands": [item.to_rpc() for item in self.scenario.commands]
                    },
                )
            case "prompt":
                self._handle_prompt(command)
            case "abort":
                self._stop_active_run()
                self._respond(command)
            case "clear_queue":
                steering = self._take_pending_steer()
                self._write_json({"type": "queue_update", "steering": [], "followUp": []})
                self._respond(
                    command,
                    data={"steering": [steering] if steering is not None else [], "followUp": []},
                )
            case "steer":
                self._handle_steer(command)
            case "new_session":
                self._handle_new_session(command)
            case "get_fork_messages":
                with self._session_lock:
                    messages = list(self.user_messages)
                self._respond(command, data={"messages": messages})
            case "get_entries":
                self._handle_get_entries(command)
            case "get_tree":
                self._handle_get_tree(command)
            case "get_messages":
                self._handle_get_messages(command)
            case "switch_session":
                self._handle_switch_session(command)
            case "set_session_name":
                self._handle_set_session_name(command)
            case "set_model":
                self._handle_set_model(command)
            case "set_thinking_level":
                self.state.thinking_level = str(command["level"])
                self._respond(command)
            case "extension_ui_response":
                self._handle_extension_ui_response(command)
            case "follow_up":
                self._fail(
                    command, "follow_up is intentionally out of scope for this fake"
                )
            case _:
                self._fail(command, f"Unsupported fake-pi command: {command_type}")

    @staticmethod
    def _parse_prompt_images(command: JsonDict) -> tuple[PromptImageContent, ...]:
        """Validate and detach optional image content from a prompt COMMAND."""
        if "images" not in command:
            return ()
        raw_images = command["images"]
        if not isinstance(raw_images, list):
            raise ValueError("prompt images must be an array")
        images: list[PromptImageContent] = []
        for index, block in enumerate(raw_images):
            if not isinstance(block, dict):
                raise ValueError(f"prompt image {index} must be an object")
            block_type = block.get("type")
            data = block.get("data")
            mime_type = block.get("mimeType")
            if not isinstance(block_type, str) or block_type != "image":
                raise ValueError(f"prompt image {index} type must be 'image'")
            if not isinstance(data, str) or not data:
                raise ValueError(f"prompt image {index} data must be a nonempty string")
            if not isinstance(mime_type, str) or not mime_type:
                raise ValueError(
                    f"prompt image {index} mimeType must be a nonempty string"
                )
            images.append(PromptImageContent(data=data, mime_type=mime_type))
        return tuple(images)

    def _handle_prompt(self, command: JsonDict) -> None:
        """Validate input and report its scenario-specific disposition."""
        try:
            prompt_images = self._parse_prompt_images(command)
        except ValueError as exc:
            self._fail(command, str(exc))
            return
        behavior = self.scenario.prompt
        if prompt_images and isinstance(
            behavior, (ExtensionDialogPrompt, CustomMessagePrompt)
        ):
            self._fail(
                command,
                "Prompt images are not supported by extension-owned fake scenarios",
            )
            return
        message = str(command["message"])
        if isinstance(behavior, TextStreamPrompt) and message == behavior.handled_input:
            self._respond(command, data={"disposition": "handled"})
            return
        if self.state.is_streaming:
            streaming_behavior = command.get("streamingBehavior")
            if streaming_behavior is None:
                self._fail(
                    command,
                    "Fake pi is already streaming; specify streamingBehavior to queue input",
                )
            elif streaming_behavior != "steer":
                self._fail(
                    command,
                    f"streamingBehavior {streaming_behavior!r} is out of scope for this fake",
                )
            elif prompt_images:
                self._fail(command, "Steering images are out of scope for this fake")
            else:
                self._queue_steer(command)
            return
        if (
            isinstance(behavior, (ExtensionDialogPrompt, NestedToolPrompt))
            and self._run_thread is not None
        ):
            self._fail(
                command, "This fake supports only one active dialog or replay worker"
            )
            return
        self._abort_requested.clear()
        match behavior:
            case TextStreamPrompt() as behavior:
                self._respond(command, data={"disposition": "started"})
                self._start_run(
                    name=f"fake-pi-text-stream-{self.scenario.name}",
                    target=lambda: self._run_text_prompt(
                        message,
                        cast(TextStreamPrompt, behavior),
                        prompt_images=prompt_images,
                    ),
                )
            case ExtensionDialogPrompt() as behavior:
                if message != behavior.command_name:
                    self._fail(
                        command,
                        f"Scenario {self.scenario.name} only supports {behavior.command_name}",
                    )
                    return
                self._start_run(
                    name=f"fake-pi-dialog-{self.scenario.name}",
                    target=lambda: self._run_extension_dialog(
                        command, cast(ExtensionDialogPrompt, behavior)
                    ),
                    streaming=False,
                )
            case CustomMessagePrompt() as behavior:
                if message != behavior.command_name:
                    self._fail(
                        command,
                        f"Scenario {self.scenario.name} only supports {behavior.command_name}",
                    )
                    return
                self._run_custom_message_prompt(message, behavior)
                self._respond(command, data={"disposition": "handled"})
            case ToolStreamPrompt() as behavior:
                self._respond(command, data={"disposition": "started"})
                self._start_run(
                    name=f"fake-pi-tool-stream-{self.scenario.name}",
                    target=lambda: self._run_tool_prompt(
                        message, behavior, prompt_images=prompt_images
                    ),
                )
            case NestedToolPrompt() as behavior:
                user_message = self._build_user_message(message, prompt_images)
                self._respond(command, data={"disposition": "started"})
                self._start_run(
                    name=f"fake-pi-nested-tools-{self.scenario.name}",
                    target=lambda: self._run_nested_tool_prompt(user_message, behavior),
                )
            case _:
                raise AssertionError("Unknown prompt behavior")

    def _handle_steer(self, command: JsonDict) -> None:
        """Queue a text-only steering message for the active text stream."""
        if "images" in command:
            self._fail(command, "Steering images are out of scope for this fake")
            return
        behavior = self.scenario.prompt
        if (
            isinstance(behavior, TextStreamPrompt)
            and str(command["message"]) == behavior.handled_input
        ):
            self._respond(command, data={"disposition": "handled"})
            return
        if not self.state.is_streaming:
            self._fail(command, "Cannot steer when no prompt is streaming")
            return
        self._queue_steer(command)

    def _queue_steer(self, command: JsonDict) -> None:
        """Accept text into the scenario's single pending steering slot."""
        if not isinstance(self.scenario.prompt, TextStreamPrompt):
            self._fail(command, "Current fake scenario does not support steer")
            return
        with self._session_lock:
            self._pending_steer_message = str(command["message"])
            self.state.pending_message_count = 1
        self._respond(command, data={"disposition": "queued"})

    def _handle_new_session(self, command: JsonDict) -> None:
        """Reset the fake to a fresh session."""
        self._stop_active_run()
        self._reset_session_file()
        self._abort_requested.clear()
        self._respond(command, data={"cancelled": False})

    def _handle_get_entries(self, command: JsonDict) -> None:
        """Return raw session entries in append order, optionally after a cursor."""
        with self._session_lock:
            entries = list(self._session_entries)
            leaf_id = self._leaf_id
        if "since" in command:
            since = command["since"]
            if not isinstance(since, str):
                self._fail(command, "get_entries since must be a string entry id")
                return
            try:
                since_index = next(
                    index for index, entry in enumerate(entries) if entry["id"] == since
                )
            except StopIteration:
                self._fail(command, f"Entry not found: {since}")
                return
            entries = entries[since_index + 1 :]
        self._respond(command, data={"entries": entries, "leafId": leaf_id})

    def _handle_get_tree(self, command: JsonDict) -> None:
        """Return the complete raw session tree and current leaf."""
        with self._session_lock:
            tree = self._build_session_tree(self._session_entries)
            leaf_id = self._leaf_id
        self._respond(command, data={"tree": tree, "leafId": leaf_id})

    def _handle_get_messages(self, command: JsonDict) -> None:
        """Return the active, compaction-aware projected message history."""
        with self._session_lock:
            messages = self._project_session_messages(self._leaf_id, self._entry_by_id)
        self._respond(command, data={"messages": messages})

    def _handle_switch_session(self, command: JsonDict) -> None:
        """Validate and transactionally switch to an explicit v3 session file."""
        try:
            path, snapshot, initialization = self._prepare_session_switch(
                command.get("sessionPath")
            )
            if initialization is not None:
                self._materialize_empty_session(path, snapshot, initialization)
        except Exception as exc:
            self._fail(command, f"Cannot switch session: {exc}")
            return

        # Validation (and any required target initialization) must finish before
        # interrupting the current run.  A failed switch therefore leaves the
        # live session, including its worker, untouched.
        with self._session_lock:
            same_path = Path(self.state.session_file) == path
        self._stop_active_run()

        # Stopping a run can append an authoritative aborted message.  Reload a
        # same-path target so switching to the current file never installs a
        # stale pre-abort snapshot.  If an external writer races us, the already
        # validated snapshot remains the safe transaction value.
        if same_path:
            try:
                snapshot = self._load_session_snapshot(path)
            except Exception:
                pass

        self._apply_session_snapshot(snapshot)
        self._respond(command, data={"cancelled": False})

    def _handle_set_session_name(self, command: JsonDict) -> None:
        """Append a name, buffering it until the first conversation message."""
        if not isinstance(raw_name := command.get("name"), str):
            self._fail(command, "Session name must be a string")
            return
        name = re.sub(r"[\r\n]+", " ", raw_name).strip()
        if not name:
            self._fail(command, "Session name must be non-empty")
            return
        self._append_session_entry(
            {"type": "session_info", "name": name}, prefix="session-info"
        )
        self._respond(command)

    def _handle_set_model(self, command: JsonDict) -> None:
        """Update the fake model in place."""
        self.state.model = {
            **self.state.model,
            "provider": command["provider"],
            "id": command["modelId"],
            "name": command["modelId"],
        }
        self._respond(command, data=self.state.model)

    def _handle_extension_ui_response(self, command: JsonDict) -> None:
        """Resume a pending extension dialog request if the IDs match."""
        if self._pending_extension_id == command.get("id"):
            self._extension_response = command
            self._extension_waiter.set()
        self._log("extension-response", command)

    def _run_text_prompt(
        self,
        message: str,
        behavior: TextStreamPrompt,
        *,
        prompt_images: tuple[PromptImageContent, ...],
    ) -> None:
        """Run a streamed-text prompt, imaging only its initial user turn."""
        emitted_messages: list[JsonDict] = []
        current_message = message
        current_images = prompt_images
        self._write_json({"type": "agent_start"})
        while True:
            completed, assistant_message = self._emit_text_turn(
                current_message,
                behavior=behavior,
                prompt_images=current_images,
                assistant_text_template=(
                    behavior.assistant_text
                    if current_message == message
                    or behavior.steer_assistant_text is None
                    else behavior.steer_assistant_text
                ),
            )
            if not completed:
                if assistant_message:
                    self._persist_assistant_message(assistant_message)
                    self._write_json({"type": "message_end", "message": assistant_message})
                pending_steer = self._take_pending_steer()
                self._finish_run(
                    [assistant_message] if assistant_message else [],
                    settled=pending_steer is None,
                )
                if pending_steer is None:
                    return
                # Like Pi, abort alone does not discard queued continuations.
                self._abort_requested.clear()
                emitted_messages = []
                current_message = pending_steer
                current_images = ()
                self._write_json({"type": "agent_start"})
                continue
            emitted_messages.append(assistant_message)
            pending_steer = self._take_pending_steer()
            if pending_steer is None:
                break
            current_message = pending_steer
            current_images = ()
        self._finish_run(emitted_messages)

    def _emit_text_turn(
        self,
        user_text: str,
        *,
        behavior: TextStreamPrompt,
        prompt_images: tuple[PromptImageContent, ...],
        assistant_text_template: str,
    ) -> tuple[bool, JsonDict]:
        """Emit one user->assistant text exchange.

        Returns ``(completed, assistant_message)``.  After an assistant
        ``message_start``, an aborted result contains the authoritative partial
        message that must be emitted before ``agent_end``.
        """
        user_message = self._build_user_message(user_text, prompt_images)
        self._persist_user_message(user_message)
        if behavior.echo_user:
            self._write_json({"type": "message_start", "message": user_message})
            self._write_json({"type": "message_end", "message": user_message})
        assistant_text = assistant_text_template.format(message=user_text)
        assistant_placeholder: JsonDict = {"role": "assistant", "content": []}
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            return False, {}
        self._write_json({"type": "message_start", "message": assistant_placeholder})
        emitted_chunks: list[str] = []
        for chunk in self._chunk_text(assistant_text, behavior.chunk_count):
            if self._abort_requested.is_set():
                return False, self._build_aborted_assistant_message(
                    "".join(emitted_chunks)
                )
            self._write_message_update(
                {
                    "type": "text_delta",
                    "contentIndex": 0,
                    "delta": chunk,
                }
            )
            emitted_chunks.append(chunk)
            if not self._sleep_ms(behavior.delay_ms, abortable=True):
                return False, self._build_aborted_assistant_message(
                    "".join(emitted_chunks)
                )
        assistant_message = self._build_assistant_message(assistant_text)
        self._persist_assistant_message(assistant_message)
        self._write_json({"type": "message_end", "message": assistant_message})
        return True, assistant_message

    def _run_extension_dialog(
        self,
        command: JsonDict,
        behavior: ExtensionDialogPrompt,
    ) -> None:
        """Resolve a dialog and emit its custom output before handled acceptance."""
        request_id = f"ext-{uuid.uuid4().hex[:8]}"
        request = self._build_extension_request(request_id, behavior)
        response = self._wait_for_extension_response(
            request, self._dialog_timeout_ms(behavior)
        )
        result_key = self._dialog_result_key(behavior.method, response)
        message_text = behavior.response_messages.get(
            result_key,
            behavior.response_messages.get("default", result_key.upper()),
        )
        followup = self._build_custom_message(message_text)
        self._persist_custom_message(followup)
        self._write_json({"type": "message_start", "message": followup})
        self._write_json({"type": "message_end", "message": followup})
        self._respond(command, data={"disposition": "handled"})

    def _run_custom_message_prompt(
        self,
        command_text: str,
        behavior: CustomMessagePrompt,
    ) -> None:
        """Run a slash command that may emit one visible custom message."""
        if not behavior.message_text:
            return
        followup = self._build_custom_message(
            behavior.message_text.format(message=command_text)
        )
        self._persist_custom_message(followup)
        self._write_json({"type": "message_start", "message": followup})
        self._write_json({"type": "message_end", "message": followup})

    def _run_tool_prompt(
        self,
        message: str,
        behavior: ToolStreamPrompt,
        *,
        prompt_images: tuple[PromptImageContent, ...],
    ) -> None:
        """Run a prompt that emits tool-call and tool-execution events."""
        self._write_json({"type": "agent_start"})
        user_message = self._build_user_message(message, prompt_images)
        self._persist_user_message(user_message)
        if behavior.echo_user:
            self._write_json({"type": "message_start", "message": user_message})
            self._write_json({"type": "message_end", "message": user_message})
        if self._abort_requested.is_set():
            self._finish_aborted_run()
            return

        tool_call_id = f"call-{uuid.uuid4().hex[:8]}"
        tool_call = {
            "type": "toolCall",
            "id": tool_call_id,
            "name": behavior.tool_name,
            "arguments": behavior.tool_args,
        }
        tool_assistant_message: JsonDict = {
            "role": "assistant",
            "content": [tool_call],
            "api": self.state.model["api"],
            "provider": self.state.model["provider"],
            "model": self.state.model["id"],
            "thinkingLevel": self.state.thinking_level,
            "usage": self._zero_usage(),
            "timestamp": now_ms(),
            "stopReason": "toolUse",
        }
        tool_assistant_start = {
            **tool_assistant_message,
            "content": [],
            "stopReason": "pending",
        }
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            self._finish_aborted_run()
            return
        self._write_json({"type": "message_start", "message": tool_assistant_start})
        self._write_message_update(
            {
                "type": "toolcall_start",
                "contentIndex": 0,
                "id": tool_call_id,
                "toolName": behavior.tool_name,
            }
        )
        raw_arguments = json.dumps(
            behavior.tool_args, separators=(",", ":"), ensure_ascii=False
        )
        for chunk in self._chunk_text(raw_arguments, 3):
            self._write_message_update(
                {
                    "type": "toolcall_delta",
                    "contentIndex": 0,
                    "delta": chunk,
                }
            )
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            aborted_message = {
                **tool_assistant_message,
                "stopReason": "aborted",
                "errorMessage": "Request was aborted",
            }
            self._finish_aborted_run(aborted_message)
            return
        self._write_message_update(
            {
                "type": "toolcall_end",
                "contentIndex": 0,
                "toolCall": tool_call,
            }
        )
        self._persist_assistant_message(tool_assistant_message)
        self._write_json({"type": "message_end", "message": tool_assistant_message})
        self._write_json(
            {
                "type": "tool_execution_start",
                "toolCallId": tool_call_id,
                "toolName": behavior.tool_name,
                "args": behavior.tool_args,
            }
        )
        if behavior.partial_result_text:
            self._write_json(
                {
                    "type": "tool_execution_update",
                    "toolCallId": tool_call_id,
                    "toolName": behavior.tool_name,
                    "args": behavior.tool_args,
                    "partialResult": self._tool_result_payload(
                        behavior.partial_result_text
                    ),
                }
            )
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            self._finish_aborted_run()
            return
        result = self._tool_result_payload(behavior.result_text)
        self._write_json(
            {
                "type": "tool_execution_end",
                "toolCallId": tool_call_id,
                "toolName": behavior.tool_name,
                "result": result,
                "isError": False,
            }
        )
        tool_result_message: JsonDict = {
            "role": "toolResult",
            "toolCallId": tool_call_id,
            "toolName": behavior.tool_name,
            **result,
            "isError": False,
            "timestamp": now_ms(),
        }
        self._persist_tool_result_message(tool_result_message)
        self._write_json({"type": "message_start", "message": tool_result_message})
        self._write_json({"type": "message_end", "message": tool_result_message})

        final_message = self._build_assistant_message(behavior.assistant_text)
        final_message_start = {
            **final_message,
            "content": [],
            "stopReason": "pending",
        }
        self._write_json({"type": "message_start", "message": final_message_start})
        if behavior.assistant_text:
            for chunk in self._chunk_text(behavior.assistant_text, 2):
                self._write_message_update(
                    {
                        "type": "text_delta",
                        "contentIndex": 0,
                        "delta": chunk,
                    }
                )
        self._persist_assistant_message(final_message)
        self._write_json({"type": "message_end", "message": final_message})
        self._finish_run(
            [user_message, tool_assistant_message, tool_result_message, final_message]
        )

    def _run_nested_tool_prompt(
        self, user_message: JsonDict, behavior: NestedToolPrompt
    ) -> None:
        """Replay literal wire records, persisting only authoritative messages."""
        self._write_json({"type": "agent_start"})
        self._persist_user_message(user_message)
        self._write_json({"type": "message_start", "message": user_message})
        self._write_json({"type": "message_end", "message": user_message})
        loop_ended = False
        for record in behavior.records:
            if not self._sleep_ms(30, abortable=True):
                if self.state.is_streaming:
                    if loop_ended:
                        self._settle_run()
                    else:
                        self._finish_aborted_run()
                return
            record_type = record["type"]
            if record_type == "message_end":
                message = record["message"]
                if message["role"] == "assistant":
                    self._persist_assistant_message(message)
                elif message["role"] == "toolResult":
                    self._persist_tool_result_message(message)
            elif record_type == "agent_end":
                loop_ended = True
            elif record_type == "agent_settled":
                self.state.is_streaming = False
            self._write_json(record)
            # Settlement ends streaming, not this worker.  Leave a fixed pause
            # to observe the saved snapshot or join playback before its late end.
            if record_type == "agent_settled" and not self._sleep_ms(500, abortable=True):
                return

    def _build_extension_request(
        self, request_id: str, behavior: ExtensionDialogPrompt
    ) -> JsonDict:
        """Return the RPC event for an extension dialog request."""
        request: JsonDict = {
            "type": "extension_ui_request",
            "id": request_id,
            "method": behavior.method,
            "title": behavior.title,
        }
        timeout_ms = self._dialog_timeout_ms(behavior)
        if timeout_ms is not None:
            request["timeout"] = timeout_ms
        if behavior.method == "confirm":
            request["message"] = behavior.message or ""
        elif behavior.method == "input":
            if behavior.placeholder is not None:
                request["placeholder"] = behavior.placeholder
        elif behavior.method == "select":
            request["options"] = behavior.options
        elif behavior.method == "editor":
            if behavior.prefill is not None:
                request["prefill"] = behavior.prefill
        else:
            raise AssertionError(f"Unsupported dialog method: {behavior.method}")
        return request

    def _wait_for_extension_response(
        self, request: JsonDict, timeout_ms: int | None
    ) -> JsonDict | None:
        """Emit a dialog request with its waiter ready, then await its response."""
        self._pending_extension_id = request["id"]
        self._extension_response = None
        self._extension_waiter.clear()
        try:
            self._write_json(request)
            if timeout_ms is None:
                while not self._extension_waiter.wait(0.01):
                    if self._abort_requested.is_set():
                        return None
            else:
                deadline = time.monotonic() + (timeout_ms / 1000)
                while time.monotonic() < deadline:
                    if self._extension_waiter.wait(0.01):
                        break
                    if self._abort_requested.is_set():
                        return None
                else:
                    return None
            return self._extension_response
        finally:
            self._pending_extension_id = None
            self._extension_response = None
            self._extension_waiter.clear()

    def _dialog_timeout_ms(self, behavior: ExtensionDialogPrompt) -> int | None:
        """Return the effective extension dialog timeout for ``behavior``."""
        if self.extension_timeout_ms is not None:
            return None if self.extension_timeout_ms <= 0 else self.extension_timeout_ms
        return behavior.timeout_ms

    def _dialog_result_key(
        self, method: DialogMethod, response: JsonDict | None
    ) -> str:
        """Map a dialog response to a scenario result key."""
        if response is None:
            return "cancelled" if self._abort_requested.is_set() else "timeout"
        if response.get("cancelled") is True:
            return "cancelled"
        if method == "confirm":
            return "confirmed" if response.get("confirmed") is True else "declined"
        if "value" in response:
            return "value"
        return "cancelled"

    def _stop_active_run(self) -> None:
        """Stop and join the active worker thread, if any."""
        thread = self._run_thread
        if thread is None:
            self.state.is_streaming = False
            self._take_pending_steer()
            self._abort_requested.clear()
            return
        self._abort_requested.set()
        self._extension_waiter.set()
        thread.join()
        if self._run_thread is thread:
            self._run_thread = None
        self.state.is_streaming = False
        self._take_pending_steer()
        self._abort_requested.clear()

    def _start_run(
        self, *, name: str, target: Callable[[], None], streaming: bool = True
    ) -> None:
        """Start a daemon worker for prompt playback."""

        def runner() -> None:
            try:
                # Image the ack->agent_start window for actual agent runs only.
                if streaming:
                    self._sleep_ms(self.pre_start_delay_ms, abortable=False)
                target()
            finally:
                if self._run_thread is thread:
                    self._run_thread = None

        thread = threading.Thread(target=runner, name=name, daemon=True)
        self.state.is_streaming = streaming
        self._run_thread = thread
        thread.start()

    def _take_pending_steer(self) -> str | None:
        """Return and clear the queued steering message, if any."""
        with self._session_lock:
            message = self._pending_steer_message
            self._pending_steer_message = None
            self.state.pending_message_count = 0
            return message

    def _finish_run(self, messages: list[JsonDict], *, settled: bool = True) -> None:
        """End a low-level run, settling only when no continuation remains."""
        self._write_json(
            {"type": "agent_end", "messages": messages, "willRetry": False}
        )
        if settled:
            self._settle_run()

    def _settle_run(self) -> None:
        """Expose idle state and emit the final settlement."""
        self.state.is_streaming = False
        self._abort_requested.clear()
        self._take_pending_steer()
        self._write_json({"type": "agent_settled"})

    def _finish_aborted_run(self, message: JsonDict | None = None) -> None:
        """Finish the current run, emitting an active aborted MESSAGE first."""
        messages: list[JsonDict] = []
        if message:
            self._persist_assistant_message(message)
            self._write_json({"type": "message_end", "message": message})
            messages.append(message)
        self._finish_run(messages)

    def _sleep_ms(self, delay_ms: int, *, abortable: bool) -> bool:
        """Sleep for ``delay_ms`` milliseconds.

        Returns ``False`` when an abort interrupt was observed.
        """
        if delay_ms <= 0:
            return not self._abort_requested.is_set()
        deadline = time.monotonic() + (delay_ms / 1000)
        while time.monotonic() < deadline:
            if abortable and self._abort_requested.is_set():
                return False
            time.sleep(min(0.01, max(0.0, deadline - time.monotonic())))
        return not (abortable and self._abort_requested.is_set())

    def _write_message_update(self, event: JsonDict) -> None:
        """Emit a delta-only assistant message update in current RPC shape."""
        self._write_json(
            {
                "type": "message_update",
                "usage": self._zero_usage(),
                "assistantMessageEvent": event,
            }
        )

    @staticmethod
    def _zero_usage() -> JsonDict:
        """Return deterministic cumulative usage for fake streaming updates."""
        return {
            "input": 0,
            "output": 0,
            "cacheRead": 0,
            "cacheWrite": 0,
            "totalTokens": 0,
            "cost": {
                "input": 0,
                "output": 0,
                "cacheRead": 0,
                "cacheWrite": 0,
                "total": 0,
            },
        }

    @staticmethod
    def _tool_result_payload(text: str) -> JsonDict:
        """Return a minimal tool result payload for tool execution events."""
        return {
            "content": [{"type": "text", "text": text}],
            "details": {"truncation": None, "fullOutputPath": None},
        }

    def _build_user_message(
        self, text: str, images: tuple[PromptImageContent, ...] = ()
    ) -> JsonDict:
        """Return a user message with detached ordered image content."""
        content: list[JsonDict] = [{"type": "text", "text": text}]
        content.extend(image.to_rpc() for image in images)
        return {
            "role": "user",
            "content": content,
            "timestamp": now_ms(),
        }

    def _build_assistant_message(self, text: str) -> JsonDict:
        """Return an assistant message payload."""
        return {
            "role": "assistant",
            "content": [{"type": "text", "text": text}],
            "api": self.state.model["api"],
            "provider": self.state.model["provider"],
            "model": self.state.model["id"],
            "thinkingLevel": self.state.thinking_level,
            "usage": self._zero_usage(),
            "timestamp": now_ms(),
            "stopReason": "stop",
        }

    def _build_aborted_assistant_message(self, text: str) -> JsonDict:
        """Return an authoritative partial assistant message for an abort."""
        return {
            "role": "assistant",
            "content": [{"type": "text", "text": text}],
            "api": self.state.model["api"],
            "provider": self.state.model["provider"],
            "model": self.state.model["id"],
            "thinkingLevel": self.state.thinking_level,
            "usage": self._zero_usage(),
            "timestamp": now_ms(),
            "stopReason": "aborted",
            "errorMessage": "Request was aborted",
        }

    def _build_custom_message(self, text: str) -> JsonDict:
        """Return a displayable custom message payload."""
        return {
            "role": "custom",
            "customType": "fake-pi-test",
            "display": True,
            "content": text,
            "timestamp": now_ms(),
        }

    def _persist_user_message(self, message: JsonDict) -> None:
        """Append a user message as a valid v3 session entry."""
        self._append_session_entry(
            {"type": "message", "message": message}, prefix="user"
        )

    def _persist_assistant_message(self, message: JsonDict) -> None:
        """Append an assistant message as a valid v3 session entry."""
        self._append_session_entry(
            {"type": "message", "message": message}, prefix="assistant"
        )

    def _persist_custom_message(self, message: JsonDict) -> None:
        """Append an extension display message as Pi's raw custom-message entry."""
        payload: JsonDict = {
            "type": "custom_message",
            "customType": message["customType"],
            "content": message["content"],
            "display": message["display"],
        }
        if "details" in message:
            payload["details"] = message["details"]
        self._append_session_entry(payload, prefix="custom")

    def _persist_tool_result_message(self, message: JsonDict) -> None:
        """Append a tool-result message as a valid v3 session entry."""
        self._append_session_entry(
            {"type": "message", "message": message}, prefix="tool-result"
        )

    @staticmethod
    def _iso_timestamp(timestamp_ms: int) -> str:
        """Return a UTC ISO timestamp with Pi's millisecond precision."""
        return (
            datetime.fromtimestamp(timestamp_ms / 1000, tz=timezone.utc)
            .isoformat(timespec="milliseconds")
            .replace("+00:00", "Z")
        )

    @staticmethod
    def _timestamp_ms(timestamp: Any) -> int:
        """Validate a Pi ISO timestamp and convert it to Unix milliseconds."""
        if not isinstance(timestamp, str) or not re.fullmatch(
            r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z", timestamp
        ):
            raise ValueError(f"Invalid session timestamp: {timestamp!r}")
        try:
            parsed = datetime.strptime(timestamp, "%Y-%m-%dT%H:%M:%S.%fZ").replace(
                tzinfo=timezone.utc
            )
        except ValueError as exc:
            raise ValueError(f"Invalid session timestamp: {timestamp!r}") from exc
        return int(parsed.timestamp()) * 1000 + parsed.microsecond // 1000

    def _new_session_header(self, session_id: str, *, cwd: str) -> JsonDict:
        """Build a complete current-version session header."""
        return {
            "type": "session",
            "version": 3,
            "id": session_id,
            "timestamp": self._iso_timestamp(now_ms()),
            "cwd": cwd,
        }

    @staticmethod
    def _strict_json_loads(line: str) -> Any:
        """Parse one strict JSON value, rejecting JavaScript numeric constants."""

        def reject_constant(value: str) -> None:
            raise ValueError(f"Invalid JSON constant: {value}")

        return json.loads(line, parse_constant=reject_constant)

    def _read_session_records(self, path: Path) -> tuple[JsonDict, list[JsonDict]]:
        """Strictly parse and validate one nonempty v3 JSONL session file."""
        raw = path.read_bytes()
        if not raw:
            raise ValueError(f"Session file is empty: {path}")
        try:
            text = raw.decode("utf-8")
        except UnicodeDecodeError as exc:
            raise ValueError(f"Session file is not UTF-8: {path}") from exc

        records: list[Any] = []
        for line_number, line in enumerate(text.split("\n"), start=1):
            if line.endswith("\r"):
                line = line[:-1]
            if not line.strip():
                continue
            try:
                records.append(self._strict_json_loads(line))
            except (json.JSONDecodeError, ValueError) as exc:
                raise ValueError(
                    f"Malformed JSONL at {path}:{line_number}: {exc}"
                ) from exc
        if not records:
            raise ValueError(f"Session file has no JSON records: {path}")
        return self._validate_v3_records(records, path)

    def _validate_v3_records(
        self, records: list[Any], path: Path
    ) -> tuple[JsonDict, list[JsonDict]]:
        """Validate the v3 header and structural contract of every entry."""
        header = records[0]
        if not isinstance(header, dict) or header.get("type") != "session":
            raise ValueError(f"Session header is missing or malformed: {path}")
        if type(header.get("version")) is not int or header["version"] != 3:
            raise ValueError(
                f"Unsupported session version {header.get('version')!r}; expected 3"
            )
        if not isinstance(header.get("id"), str) or not header["id"]:
            raise ValueError("Session header id must be a nonempty string")
        self._timestamp_ms(header.get("timestamp"))
        cwd = header.get("cwd")
        if (
            not isinstance(cwd, str)
            or not cwd
            or "\x00" in cwd
            or not Path(cwd).is_absolute()
            or not Path(cwd).exists()
        ):
            raise ValueError("Session header cwd must name an existing path")

        entries: list[JsonDict] = []
        seen_ids: set[str] = set()
        for index, raw_entry in enumerate(records[1:], start=2):
            if not isinstance(raw_entry, dict):
                raise ValueError(f"Session entry {index} is not an object")
            self._validate_v3_entry(raw_entry, index)
            entry_id = raw_entry["id"]
            if entry_id in seen_ids:
                raise ValueError(f"Duplicate session entry id: {entry_id}")
            seen_ids.add(entry_id)
            entries.append(raw_entry)
        return header, entries

    def _validate_v3_entry(self, entry: JsonDict, index: int) -> None:
        """Validate one raw v3 nonheader entry without constraining its tree."""
        entry_type = entry.get("type")
        allowed_types = {
            "message",
            "thinking_level_change",
            "model_change",
            "compaction",
            "branch_summary",
            "custom",
            "custom_message",
            "label",
            "session_info",
        }
        if not isinstance(entry_type, str) or entry_type not in allowed_types:
            raise ValueError(
                f"Unsupported session entry type at record {index}: {entry_type!r}"
            )
        entry_id = entry.get("id")
        if not isinstance(entry_id, str) or not entry_id:
            raise ValueError(f"Session entry {index} has an invalid id")
        if "parentId" not in entry or not (
            entry["parentId"] is None or isinstance(entry["parentId"], str)
        ):
            raise ValueError(f"Session entry {entry_id} has an invalid parentId")
        self._timestamp_ms(entry.get("timestamp"))

        if entry_type == "message":
            message = entry.get("message")
            if not isinstance(message, dict) or not isinstance(
                message.get("role"), str
            ):
                raise ValueError(f"Message entry {entry_id} has an invalid message")
        elif entry_type == "thinking_level_change":
            if not isinstance(entry.get("thinkingLevel"), str):
                raise ValueError(f"Thinking-level entry {entry_id} is malformed")
        elif entry_type == "model_change":
            if not isinstance(entry.get("provider"), str) or not isinstance(
                entry.get("modelId"), str
            ):
                raise ValueError(f"Model-change entry {entry_id} is malformed")
        elif entry_type == "compaction":
            tokens_before = entry.get("tokensBefore")
            if (
                not isinstance(entry.get("summary"), str)
                or not isinstance(entry.get("firstKeptEntryId"), str)
                or isinstance(tokens_before, bool)
                or not isinstance(tokens_before, (int, float))
                or not math.isfinite(tokens_before)
            ):
                raise ValueError(f"Compaction entry {entry_id} is malformed")
        elif entry_type == "branch_summary":
            if not isinstance(entry.get("summary"), str) or not isinstance(
                entry.get("fromId"), str
            ):
                raise ValueError(f"Branch-summary entry {entry_id} is malformed")
        elif entry_type in {"custom", "custom_message"}:
            if not isinstance(entry.get("customType"), str):
                raise ValueError(f"Custom entry {entry_id} has no customType")
            if entry_type == "custom_message" and (
                not isinstance(entry.get("display"), bool)
                or not isinstance(entry.get("content"), (str, list))
            ):
                raise ValueError(f"Custom-message entry {entry_id} is malformed")
        elif entry_type == "label":
            label = entry.get("label")
            if not isinstance(entry.get("targetId"), str) or not (
                label is None or isinstance(label, str)
            ):
                raise ValueError(f"Label entry {entry_id} is malformed")
        elif entry_type == "session_info":
            name = entry.get("name")
            if not (name is None or isinstance(name, str)):
                raise ValueError(f"Session-info entry {entry_id} is malformed")

    def _load_session_snapshot(self, path: Path) -> JsonDict:
        """Parse PATH and return a fully projected transaction snapshot."""
        header, entries = self._read_session_records(path)
        return self._build_session_snapshot(path, header, entries)

    def _build_session_snapshot(
        self, path: Path, header: JsonDict, entries: list[JsonDict]
    ) -> JsonDict:
        """Build all mutable/public session projections from one raw entry list."""
        entry_by_id = {entry["id"]: entry for entry in entries}
        leaf_id = entries[-1]["id"] if entries else None
        message_count = len(self._project_session_messages(leaf_id, entry_by_id))

        session_name: str | None = None
        for entry in entries:
            if entry["type"] == "session_info":
                raw_name = entry.get("name")
                session_name = (
                    raw_name.strip() or None if isinstance(raw_name, str) else None
                )

        fork_messages: list[dict[str, str]] = []
        for entry in entries:
            if entry["type"] != "message":
                continue
            message = entry["message"]
            if message.get("role") != "user":
                continue
            text = self._message_content_text(message)
            if text:
                fork_messages.append({"entryId": entry["id"], "text": text})

        timestamp_values = [self._timestamp_ms(header["timestamp"])]
        timestamp_values.extend(
            self._timestamp_ms(entry["timestamp"]) for entry in entries
        )
        return {
            "path": path,
            "header": header,
            "entries": entries,
            "entryById": entry_by_id,
            "leafId": leaf_id,
            "messageCount": message_count,
            "sessionName": session_name,
            "forkMessages": fork_messages,
            "lastTimestampMs": max(timestamp_values),
        }

    @staticmethod
    def _message_content_text(message: JsonDict) -> str:
        """Extract concatenated text content for the public fork-message list."""
        content = message.get("content")
        if isinstance(content, str):
            return content
        if not isinstance(content, list):
            return ""
        pieces: list[str] = []
        for block in content:
            if (
                isinstance(block, dict)
                and block.get("type") == "text"
                and isinstance(block.get("text"), str)
            ):
                pieces.append(block["text"])
        return "".join(pieces)

    def _active_session_path(
        self,
        leaf_id: str | None,
        entry_by_id: dict[str, JsonDict],
    ) -> list[JsonDict]:
        """Return the active parent path iteratively, stopping safely at cycles."""
        if leaf_id is None:
            return []
        current = entry_by_id.get(leaf_id)
        reverse_path: list[JsonDict] = []
        seen: set[str] = set()
        while current is not None and current["id"] not in seen:
            seen.add(current["id"])
            reverse_path.append(current)
            parent_id = current["parentId"]
            current = entry_by_id.get(parent_id) if isinstance(parent_id, str) else None
        reverse_path.reverse()
        return reverse_path

    def _project_session_messages(
        self,
        leaf_id: str | None,
        entry_by_id: dict[str, JsonDict],
    ) -> list[JsonDict]:
        """Project the active path with Pi's latest-compaction retained range."""
        path = self._active_session_path(leaf_id, entry_by_id)
        compaction_index: int | None = None
        for index, entry in enumerate(path):
            if entry["type"] == "compaction":
                compaction_index = index

        context_entries = path
        if compaction_index is not None:
            compaction = path[compaction_index]
            context_entries = [compaction]
            found_first_kept = False
            for entry in path[:compaction_index]:
                if entry["id"] == compaction["firstKeptEntryId"]:
                    found_first_kept = True
                if found_first_kept:
                    context_entries.append(entry)
            context_entries.extend(path[compaction_index + 1 :])

        messages: list[JsonDict] = []
        for entry in context_entries:
            entry_type = entry["type"]
            if entry_type == "message":
                message = entry["message"]
                role = message["role"]
                if message.get("content") is None and role in (
                    "user",
                    "assistant",
                    "toolResult",
                ):
                    message = {**message, "content": []}
                messages.append(message)
            elif entry_type == "custom_message":
                custom_message: JsonDict = {
                    "role": "custom",
                    "customType": entry["customType"],
                    "content": entry["content"],
                    "display": entry["display"],
                    "timestamp": self._timestamp_ms(entry["timestamp"]),
                }
                if "details" in entry:
                    custom_message["details"] = entry["details"]
                messages.append(custom_message)
            elif entry_type == "branch_summary" and entry["summary"]:
                messages.append(
                    {
                        "role": "branchSummary",
                        "summary": entry["summary"],
                        "fromId": entry["fromId"],
                        "timestamp": self._timestamp_ms(entry["timestamp"]),
                    }
                )
            elif entry_type == "compaction":
                messages.append(
                    {
                        "role": "compactionSummary",
                        "summary": entry["summary"],
                        "tokensBefore": entry["tokensBefore"],
                        "timestamp": self._timestamp_ms(entry["timestamp"]),
                    }
                )
        return messages

    def _build_session_tree(self, entries: list[JsonDict]) -> list[JsonDict]:
        """Build the raw labeled forest iteratively with stable child ordering."""
        effective_labels: dict[str, tuple[str, str]] = {}
        for entry in entries:
            if entry["type"] != "label":
                continue
            target_id = entry["targetId"]
            label = entry.get("label")
            if isinstance(label, str) and label:
                effective_labels[target_id] = (label, entry["timestamp"])
            else:
                effective_labels.pop(target_id, None)

        nodes: dict[str, JsonDict] = {}
        order: dict[str, int] = {}
        for index, entry in enumerate(entries):
            node: JsonDict = {"entry": entry, "children": []}
            resolved_label = effective_labels.get(entry["id"])
            if resolved_label is not None:
                node["label"], node["labelTimestamp"] = resolved_label
            nodes[entry["id"]] = node
            order[entry["id"]] = index

        # A valid Pi tree is acyclic, but hand-authored files can contain a
        # parent cycle.  Break one edge per cycle at its earliest appended node
        # so every raw entry remains serializable exactly once.
        parent_by_id = {entry["id"]: entry["parentId"] for entry in entries}
        processed: set[str] = set()
        cycle_roots: set[str] = set()
        for entry in entries:
            start_id = entry["id"]
            if start_id in processed:
                continue
            trail: list[str] = []
            positions: dict[str, int] = {}
            current_id: str | None = start_id
            while (
                current_id is not None
                and current_id in nodes
                and current_id not in processed
            ):
                if current_id in positions:
                    cycle = trail[positions[current_id] :]
                    cycle_roots.add(min(cycle, key=order.__getitem__))
                    break
                positions[current_id] = len(trail)
                trail.append(current_id)
                parent_id = parent_by_id[current_id]
                if (
                    parent_id is None
                    or parent_id == current_id
                    or parent_id not in nodes
                ):
                    break
                current_id = parent_id
            processed.update(trail)

        roots: list[JsonDict] = []
        for entry in entries:
            entry_id = entry["id"]
            parent_id = entry["parentId"]
            node = nodes[entry_id]
            if (
                parent_id is None
                or parent_id == entry_id
                or parent_id not in nodes
                or entry_id in cycle_roots
            ):
                roots.append(node)
            else:
                nodes[parent_id]["children"].append(node)

        pending = list(roots)
        while pending:
            node = pending.pop()
            children = node["children"]
            children.sort(
                key=lambda child: self._timestamp_ms(child["entry"]["timestamp"])
            )
            pending.extend(children)
        return roots

    def _prepare_session_switch(
        self, raw_path: Any
    ) -> tuple[Path, JsonDict, str | None]:
        """Validate a switch target and prepare its side-effect-free snapshot."""
        if not isinstance(raw_path, str):
            raise ValueError("sessionPath must be a string")
        if not raw_path or "\x00" in raw_path:
            raise ValueError("sessionPath must be a nonempty path without NUL bytes")
        path = Path(raw_path)
        if not path.is_absolute():
            raise ValueError("sessionPath must be absolute")
        path = path.resolve(strict=False)

        if path.exists():
            if path.is_dir():
                raise ValueError(f"Session path is a directory: {path}")
            if not path.is_file():
                raise ValueError(f"Session path is not a regular file: {path}")
            if path.stat().st_size == 0:
                header = self._new_session_header(
                    f"fake-{uuid.uuid4().hex[:8]}", cwd=str(Path.cwd().resolve())
                )
                snapshot = self._build_session_snapshot(path, header, [])
                return path, snapshot, "empty"
            return path, self._load_session_snapshot(path), None

        header = self._new_session_header(
            f"fake-{uuid.uuid4().hex[:8]}", cwd=str(Path.cwd().resolve())
        )
        snapshot = self._build_session_snapshot(path, header, [])
        return path, snapshot, "missing"

    def _materialize_empty_session(
        self, path: Path, snapshot: JsonDict, initialization: str
    ) -> None:
        """Write a prepared empty header without clobbering a raced target."""
        content = (self._encode_json(snapshot["header"]) + "\n").encode("utf-8")
        if initialization == "missing":
            path.parent.mkdir(parents=True, exist_ok=True)
            with path.open("xb") as handle:
                handle.write(content)
            return
        if initialization != "empty":
            raise AssertionError(f"Unknown session initialization: {initialization}")
        with path.open("r+b") as handle:
            if handle.read(1):
                raise ValueError(f"Session file changed while switching: {path}")
            handle.seek(0)
            handle.write(content)
            handle.truncate()

    def _apply_session_snapshot(self, snapshot: JsonDict) -> None:
        """Atomically install one prepared raw/projection snapshot."""
        with self._session_lock:
            self._session_header = snapshot["header"]
            self._session_entries = snapshot["entries"]
            self._entry_by_id = snapshot["entryById"]
            self._leaf_id = snapshot["leafId"]
            self._last_entry_timestamp_ms = snapshot["lastTimestampMs"]
            self.user_messages = snapshot["forkMessages"]
            self.state.session_file = str(snapshot["path"])
            self.state.session_id = snapshot["header"]["id"]
            self.state.session_name = snapshot["sessionName"]
            self.state.message_count = snapshot["messageCount"]

    def _append_session_entry(self, payload: JsonDict, *, prefix: str) -> str:
        """Record a v3 entry, flushing buffered records on first conversation."""
        with self._session_lock:
            entry_id = self._entry_id(prefix)
            timestamp_ms = max(now_ms(), self._last_entry_timestamp_ms + 1)
            entry: JsonDict = {
                "type": payload["type"],
                "id": entry_id,
                "parentId": self._leaf_id,
                "timestamp": self._iso_timestamp(timestamp_ms),
            }
            entry.update(
                {key: value for key, value in payload.items() if key != "type"}
            )
            entries = [*self._session_entries, entry]
            path = Path(self.state.session_file)
            snapshot = self._build_session_snapshot(path, self._session_header, entries)
            if path.exists():
                with path.open("a", encoding="utf-8", newline="\n") as handle:
                    handle.write(self._encode_json(entry) + "\n")
            elif entry["type"] == "message" and entry["message"]["role"] in (
                "user", "assistant"
            ):
                with path.open("x", encoding="utf-8", newline="\n") as handle:
                    for record in (self._session_header, *entries):
                        handle.write(self._encode_json(record) + "\n")
            self._apply_session_snapshot(snapshot)
            return entry_id

    def _reset_session_file(self) -> None:
        """Allocate a fresh v3 header and path in memory without writing bytes."""
        with self._session_lock:
            self._message_serial = 0
            session_id = f"fake-{uuid.uuid4().hex[:8]}"
            path = self._session_root / f"{session_id}.jsonl"
            header = self._new_session_header(session_id, cwd=str(Path.cwd().resolve()))
            snapshot = self._build_session_snapshot(path, header, [])
            self._apply_session_snapshot(snapshot)

    def _entry_id(self, prefix: str) -> str:
        """Return a deterministic entry ID that does not collide after switches."""
        while True:
            self._message_serial += 1
            candidate = f"{prefix}-{self._message_serial}"
            if candidate not in self._entry_by_id:
                return candidate

    def _respond(self, command: JsonDict, *, data: JsonDict | None = None) -> None:
        """Emit a successful RPC response for ``command``."""
        response: JsonDict = {
            "type": "response",
            "command": command["type"],
            "success": True,
        }
        if "id" in command:
            response["id"] = command["id"]
        if data is not None:
            response["data"] = data
        self._write_json(
            response, split_at=self.split_responses.get(str(command["type"]))
        )

    def _fail(self, command: JsonDict, error: str) -> None:
        """Emit a failed RPC response for ``command``."""
        response: JsonDict = {
            "type": "response",
            "command": command["type"],
            "success": False,
            "error": error,
        }
        if "id" in command:
            response["id"] = command["id"]
        self._write_json(response)

    @staticmethod
    def _encode_json(value: Any) -> str:
        """Encode JSON iteratively so deep session trees cannot overflow Python."""
        output: list[str] = []
        stack: list[tuple[str, Any]] = [("value", value)]
        while stack:
            kind, current = stack.pop()
            if kind == "raw":
                output.append(current)
                continue
            if isinstance(current, dict):
                items = list(current.items())
                stack.append(("raw", "}"))
                for index in range(len(items) - 1, -1, -1):
                    key, item = items[index]
                    if not isinstance(key, str):
                        raise TypeError(f"JSON object key is not a string: {key!r}")
                    stack.append(("value", item))
                    stack.append(("raw", ":"))
                    stack.append(
                        ("raw", json.dumps(key, ensure_ascii=False, allow_nan=False))
                    )
                    if index > 0:
                        stack.append(("raw", ","))
                stack.append(("raw", "{"))
                continue
            if isinstance(current, (list, tuple)):
                stack.append(("raw", "]"))
                for index in range(len(current) - 1, -1, -1):
                    stack.append(("value", current[index]))
                    if index > 0:
                        stack.append(("raw", ","))
                stack.append(("raw", "["))
                continue
            output.append(
                json.dumps(
                    current,
                    separators=(",", ":"),
                    ensure_ascii=False,
                    allow_nan=False,
                )
            )
        return "".join(output)

    def _write_json(self, payload: JsonDict, *, split_at: int | None = None) -> None:
        """Write one JSONL record to stdout and flush promptly."""
        line = self._encode_json(payload) + "\n"
        self._log("out", payload)
        with self._write_lock:
            if split_at is not None and 0 < split_at < len(line):
                sys.stdout.write(line[:split_at])
                sys.stdout.flush()
                time.sleep(0.01)
                sys.stdout.write(line[split_at:])
                sys.stdout.flush()
            else:
                sys.stdout.write(line)
                sys.stdout.flush()

    def _log(self, direction: str, payload: JsonDict) -> None:
        """Append a debug log line when ``--log-file`` is enabled."""
        if self.log_file is None:
            return
        with self.log_file.open("a", encoding="utf-8") as handle:
            handle.write(
                self._encode_json({"direction": direction, "payload": payload}) + "\n"
            )

    @staticmethod
    def _chunk_text(text: str, chunk_count: int) -> list[str]:
        """Split ``text`` into ``chunk_count`` non-empty chunks."""
        chunk_count = max(1, chunk_count)
        if len(text) <= chunk_count:
            return [char for char in text if char] or [text]
        base, extra = divmod(len(text), chunk_count)
        chunks: list[str] = []
        start = 0
        for index in range(chunk_count):
            size = base + (1 if index < extra else 0)
            piece = text[start : start + size]
            if piece:
                chunks.append(piece)
            start += size
        return chunks or [text]


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse command-line arguments for the fake harness."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", default="rpc", choices=["rpc"])
    parser.add_argument("--approve", action="store_true")
    parser.add_argument("--no-approve", action="store_true")
    parser.add_argument("--scenario", required=True)
    parser.add_argument("--scenario-dir", default=str(default_scenario_dir()))
    parser.add_argument("--session-dir")
    parser.add_argument("--log-file")
    parser.add_argument(
        "--extension-timeout-ms",
        type=int,
        help="Override dialog timeout in milliseconds; 0 disables timeout",
    )
    parser.add_argument(
        "--pre-start-delay-ms",
        type=int,
        default=0,
        help="Sleep before the first agent_start of each run, in milliseconds",
    )
    parser.add_argument(
        "--split-response",
        action="append",
        default=[],
        metavar="COMMAND:INDEX",
        help="Split a response line at INDEX bytes for newline-framing tests",
    )
    return parser.parse_args(argv)


def parse_split_responses(items: list[str]) -> dict[str, int]:
    """Parse ``--split-response`` values into a command->index map."""
    result: dict[str, int] = {}
    for item in items:
        command, _, index = item.partition(":")
        if not command or not index:
            raise ValueError(f"Invalid --split-response value: {item}")
        result[command] = int(index)
    return result


def main(argv: list[str] | None = None) -> int:
    """Entry point for the fake-pi harness."""
    args = parse_args(argv)
    scenario_path = Path(args.scenario_dir) / f"{args.scenario}.json"
    try:
        scenario = load_scenario(scenario_path, args.scenario)
    except FileNotFoundError:
        print(f"fake-pi: scenario not found: {args.scenario}", file=sys.stderr)
        return 2
    except json.JSONDecodeError as exc:
        print(
            f"fake-pi: invalid JSON in scenario {args.scenario}: {exc}",
            file=sys.stderr,
        )
        return 2
    except (KeyError, TypeError, ValueError) as exc:
        print(f"fake-pi: invalid scenario {args.scenario}: {exc}", file=sys.stderr)
        return 2
    try:
        split_responses = parse_split_responses(args.split_response)
    except ValueError as exc:
        print(f"fake-pi: {exc}", file=sys.stderr)
        return 2
    harness = FakePiHarness(
        scenario=scenario,
        session_dir=args.session_dir,
        log_file=args.log_file,
        extension_timeout_ms=args.extension_timeout_ms,
        pre_start_delay_ms=args.pre_start_delay_ms,
        split_responses=split_responses,
    )
    return harness.run()


if __name__ == "__main__":
    raise SystemExit(main())
