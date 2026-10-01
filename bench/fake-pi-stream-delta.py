#!/usr/bin/env python3
"""Deterministic JSON-over-stdio backend for stream-delta benchmarks."""

from __future__ import annotations

import argparse
import json
import os
import sys
import threading
import time
from pathlib import Path
from typing import Any

Json = dict[str, Any]

MODEL: Json = {
    "id": "fake-model",
    "name": "Fake Model",
    "provider": "fake",
    "api": "fake-api",
    "contextWindow": 200000,
    "maxTokens": 4096,
}
TIMESTAMP_BASE_MS = 1704067200000
_WRITE_LOCK = threading.Lock()


def env_int(name: str, default: int) -> int:
    value = os.environ.get(name)
    return int(value) if value else default


def config_from_env() -> Json:
    return {
        "scenario": os.environ.get("PI_SD_BENCH_SCENARIO", "full"),
        "history_turns": env_int("PI_SD_BENCH_HISTORY_TURNS", 180),
        "history_text_bytes": env_int("PI_SD_BENCH_HISTORY_TEXT_BYTES", 1200),
        "timer_text_deltas": env_int("PI_SD_BENCH_TIMER_TEXT_DELTAS", 700),
        "text_burst": env_int("PI_SD_BENCH_TEXT_BURST", 20),
        "thinking_deltas": env_int("PI_SD_BENCH_THINKING_DELTAS", 80),
        "thinking_burst": env_int("PI_SD_BENCH_THINKING_BURST", 20),
        "backlog_deltas": env_int("PI_SD_BENCH_BACKLOG_DELTAS", 300),
        "toolcall_repeats": env_int("PI_SD_BENCH_TOOLCALL_REPEATS", 0),
        "burst_pause_ms": env_int("PI_SD_BENCH_BURST_PAUSE_MS", 80),
        "seed": env_int("PI_SD_BENCH_SEED", 20240817),
    }


def log_line(log_file: Path | None, payload: Json) -> None:
    if log_file is None:
        return
    log_file.parent.mkdir(parents=True, exist_ok=True)
    with log_file.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(payload, separators=(",", ":")) + "\n")


def encode_line(payload: Json) -> str:
    return json.dumps(payload, separators=(",", ":"), ensure_ascii=False) + "\n"


def write_payload(payload: Json) -> None:
    write_payloads([payload])


def write_payloads(payloads: list[Json]) -> None:
    """Write PAYLOADS as one locked stdout batch."""
    data = "".join(encode_line(payload) for payload in payloads)
    with _WRITE_LOCK:
        sys.stdout.write(data)
        sys.stdout.flush()


def respond(command: Json, data: Json | None = None) -> None:
    payload: Json = {
        "type": "response",
        "command": command.get("type"),
        "success": True,
    }
    if "id" in command:
        payload["id"] = command["id"]
    if data is not None:
        payload["data"] = data
    write_payload(payload)


def zero_usage() -> Json:
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


def sized_history_text(turn: int, role: str, size: int) -> str:
    """Return exactly SIZE ASCII characters of deterministic history text."""
    sentinel = f"HISTORY-{role.upper()}-{turn:04d} "
    rows: list[str] = [sentinel]
    row = 0
    while sum(map(len, rows)) < size:
        rows.append(
            f"synthetic {role} transcript turn {turn:04d} row {row:03d}; "
            "deterministic existing context for stream rendering.\n"
        )
        row += 1
    return "".join(rows)[:size]


def history_messages(config: Json) -> list[Json]:
    messages: list[Json] = []
    turns = int(config["history_turns"])
    size = int(config["history_text_bytes"])
    for turn in range(turns):
        user_text = sized_history_text(turn, "user", size)
        assistant_text = sized_history_text(turn, "assistant", size)
        messages.append(
            {
                "role": "user",
                "content": [{"type": "text", "text": user_text}],
                "timestamp": TIMESTAMP_BASE_MS + turn * 2000,
            }
        )
        messages.append(
            {
                "role": "assistant",
                "content": [{"type": "text", "text": assistant_text}],
                "timestamp": TIMESTAMP_BASE_MS + turn * 2000 + 1000,
                "stopReason": "stop",
            }
        )
    return messages


def state_rpc(config: Json) -> Json:
    return {
        "model": MODEL,
        "thinkingLevel": "medium",
        "isStreaming": False,
        "isCompacting": False,
        "steeringMode": "one-at-a-time",
        "followUpMode": "one-at-a-time",
        "sessionFile": "",
        "sessionId": "fake-stream-delta-session",
        "autoCompactionEnabled": False,
        "messageCount": int(config["history_turns"]) * 2,
        "pendingMessageCount": 0,
    }


def stats_rpc() -> Json:
    return {
        "totalCost": 0,
        "totalTokens": 0,
        "inputTokens": 0,
        "outputTokens": 0,
        "cacheReadTokens": 0,
        "cacheWriteTokens": 0,
        "contextTokens": 0,
        "contextWindow": 200000,
        "messageCount": 0,
    }


def text_line(index: int, seed: int) -> str:
    value = (seed + index * 7919) % 100000
    return f"SD-TEXT-{index:04d} value-{value:05d}\n"


def thinking_line(index: int, seed: int) -> str:
    value = (seed + index * 3571) % 100000
    return f"SD-THINK-{index:04d} thought-{value:05d}\n"


def backlog_line(index: int, seed: int) -> str:
    value = (seed + index * 6151) % 100000
    return f"SD-BACKLOG-{index:04d} value-{value:05d}\n"


def message_update(event: Json, *, phase: str) -> Json:
    payload: Json = {
        "type": "message_update",
        "usage": zero_usage(),
        "assistantMessageEvent": event,
        "benchmarkPhase": phase,
    }
    return payload


def emit_bursts(
    payloads: list[Json], burst_size: int, pause_ms: int
) -> None:
    if burst_size <= 0:
        raise ValueError("burst size must be positive")
    for start in range(0, len(payloads), burst_size):
        write_payloads(payloads[start : start + burst_size])
        time.sleep(pause_ms / 1000.0)


def run_stream(
    config: Json,
    log_file: Path | None,
    backlog_begin: threading.Event,
) -> None:
    seed = int(config["seed"])
    timer_count = int(config["timer_text_deltas"])
    thinking_count = int(config["thinking_deltas"])
    backlog_count = int(config["backlog_deltas"])
    pause_ms = int(config["burst_pause_ms"])

    text = "".join(text_line(index, seed) for index in range(timer_count))
    thinking = "".join(
        thinking_line(index, seed) for index in range(thinking_count)
    )
    backlog = "".join(
        backlog_line(index, seed) for index in range(backlog_count)
    )
    toolcall_repeats = int(config["toolcall_repeats"])
    command = "echo SD-BOUNDARY-TOOL"
    if toolcall_repeats:
        command += "; " + "".join(
            f"echo SD-CMD-{index:04d}; " for index in range(toolcall_repeats)
        )
    tool_call: Json = {
        "type": "toolCall",
        "id": "call-stream-boundary",
        "name": "bash",
        "arguments": {"command": command},
    }
    raw_args = json.dumps(tool_call["arguments"], separators=(",", ":"))
    chunks = (
        [raw_args[start : start + 32] for start in range(0, len(raw_args), 32)]
        if toolcall_repeats else [raw_args]
    )
    tool_deltas = [
        message_update(
            {"type": "toolcall_delta", "contentIndex": 3, "delta": chunk},
            phase="timer-toolcall" if toolcall_repeats else "tool-boundary",
        )
        for chunk in chunks
    ]
    tool_end = message_update(
        {"type": "toolcall_end", "contentIndex": 3, "toolCall": tool_call},
        phase="tool-boundary",
    )
    final_message: Json = {
        "role": "assistant",
        "content": [
            {"type": "text", "text": text},
            {"type": "thinking", "thinking": thinking},
            {"type": "text", "text": backlog},
            tool_call,
        ],
        "timestamp": TIMESTAMP_BASE_MS + 1_000_000,
        "stopReason": "toolUse",
    }

    write_payload({"type": "agent_start", "benchmarkPhase": "lifecycle"})
    write_payload(
        {
            "type": "message_start",
            "message": {
                "role": "assistant",
                "content": [],
                "timestamp": final_message["timestamp"],
                "stopReason": "pending",
            },
            "benchmarkPhase": "lifecycle",
        }
    )
    write_payload(
        message_update(
            {"type": "text_start", "contentIndex": 0}, phase="timer-text"
        )
    )

    text_events = [
        message_update(
            {
                "type": "text_delta",
                "contentIndex": 0,
                "delta": text_line(index, seed),
            },
            phase="timer-text"
        )
        for index in range(timer_count)
    ]
    emit_bursts(text_events, int(config["text_burst"]), pause_ms)
    write_payload(
        message_update(
            {"type": "text_end", "contentIndex": 0, "content": text},
            phase="timer-text-end",
        )
    )

    write_payload(
        message_update(
            {"type": "thinking_start", "contentIndex": 1},
            phase="timer-thinking",
        )
    )
    thinking_events = [
        message_update(
            {
                "type": "thinking_delta",
                "contentIndex": 1,
                "delta": thinking_line(index, seed),
            },
            phase="timer-thinking"
        )
        for index in range(thinking_count)
    ]
    emit_bursts(
        thinking_events, int(config["thinking_burst"]), pause_ms
    )
    write_payload(
        message_update(
            {
                "type": "thinking_end",
                "contentIndex": 1,
                "content": thinking,
            },
            phase="timer-thinking-end",
        )
    )

    write_payload(
        message_update(
            {"type": "text_start", "contentIndex": 2}, phase="backlog"
        )
    )
    write_payload(
        {
            "type": "benchmark_backlog_ready",
            "benchmarkPhase": "backlog-control",
        }
    )
    # Emit the backlog only after the harness acknowledges that its
    # backlog collector is installed.
    backlog_begin.wait()

    backlog_payloads = [
        message_update(
            {
                "type": "text_delta",
                "contentIndex": 2,
                "delta": backlog_line(index, seed),
            },
            phase="backlog"
        )
        for index in range(backlog_count)
    ]
    backlog_payloads.extend(
        [
            message_update(
                {"type": "text_end", "contentIndex": 2, "content": backlog},
                phase="backlog-boundary",
            ),
            message_update(
                {
                    "type": "toolcall_start",
                    "contentIndex": 3,
                    "id": tool_call["id"],
                    "toolName": tool_call["name"],
                },
                phase="tool-boundary",
            ),
        ]
    )
    if not toolcall_repeats:
        backlog_payloads.extend([*tool_deltas, tool_end])
    backlog_payloads.append(
        {
            "type": "benchmark_backlog_complete",
            "benchmarkMarker": "SD-BACKLOG-COMPLETE-CONTROL",
            "benchmarkPhase": "backlog-control",
        }
    )
    write_payloads(backlog_payloads)
    time.sleep(0.1)
    if toolcall_repeats:
        emit_bursts(tool_deltas, int(config["text_burst"]), pause_ms)
        write_payload(tool_end)

    write_payload(
        {
            "type": "message_end",
            "message": final_message,
            "benchmarkPhase": "lifecycle",
        }
    )
    write_payload(
        {
            "type": "agent_end",
            "messages": [final_message],
            "willRetry": False,
            "benchmarkPhase": "lifecycle",
        }
    )
    write_payload(
        {"type": "agent_settled", "benchmarkPhase": "lifecycle"}
    )
    log_line(
        log_file,
        {
            "event": "stream-complete",
            "textDeltas": timer_count + backlog_count,
            "thinkingDeltas": thinking_count,
            "toolcallDeltas": len(chunks),
            "backlogDeltas": backlog_count,
        },
    )


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", default="rpc")
    parser.add_argument("--approve", action="store_true")
    parser.add_argument("--no-approve", action="store_true")
    parser.add_argument("--log-file")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    raw_argv = list(sys.argv[1:] if argv is None else argv)
    if raw_argv == ["--version"]:
        print("0.85.0")
        return 0

    args = parse_args([arg for arg in raw_argv if arg != "--version"])
    config = config_from_env()
    log_file = Path(args.log_file) if args.log_file else None
    history = history_messages(config)
    backlog_begin = threading.Event()
    log_line(log_file, {"event": "fake-pi-start", "config": config})

    worker: threading.Thread | None = None
    for raw in sys.stdin.buffer:
        line = raw.decode("utf-8", "replace").strip()
        if not line:
            continue
        try:
            command = json.loads(line)
        except json.JSONDecodeError:
            continue
        command_type = command.get("type")
        log_line(
            log_file,
            {"direction": "in", "command": command_type, "id": command.get("id")},
        )
        if command_type == "get_state":
            respond(command, state_rpc(config))
        elif command_type == "get_commands":
            respond(command, {"commands": []})
        elif command_type == "get_messages":
            respond(command, {"messages": history})
        elif command_type == "get_session_stats":
            respond(command, stats_rpc())
        elif command_type == "get_fork_messages":
            respond(command, {"messages": []})
        elif command_type == "get_last_assistant_text":
            respond(command, {"text": ""})
        elif command_type == "prompt":
            respond(command)
            worker = threading.Thread(
                target=run_stream,
                args=(config, log_file, backlog_begin),
                daemon=True,
            )
            worker.start()
        elif command_type == "benchmark_backlog_begin":
            backlog_begin.set()
            respond(command)
        elif command_type == "clear_queue":
            respond(command, {"steering": [], "followUp": []})
        elif command_type in (
            "abort",
            "steer",
            "set_thinking_level",
            "new_session",
        ):
            respond(command, {"cancelled": False} if command_type == "new_session" else None)
        else:
            payload: Json = {
                "type": "response",
                "command": command_type,
                "success": False,
                "error": f"unsupported: {command_type}",
            }
            if "id" in command:
                payload["id"] = command["id"]
            write_payload(payload)

    if worker is not None:
        worker.join()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
