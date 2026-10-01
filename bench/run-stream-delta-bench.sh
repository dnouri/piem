#!/usr/bin/env bash
# run-stream-delta-bench.sh - Run Pilish coalesced stream benchmarks
#
# Usage:
#   ./bench/run-stream-delta-bench.sh                       # GUI/xvfb full preset
#   ./bench/run-stream-delta-bench.sh --batch               # batch full preset
#   ./bench/run-stream-delta-bench.sh --scenario smoke -c 1 # cheap CI smoke
#   ./bench/run-stream-delta-bench.sh --scenario toolcall  # bash argument stream
#   ./bench/run-stream-delta-bench.sh -c 3                  # repetitions
#   ./bench/run-stream-delta-bench.sh --out-dir tmp/sd      # custom artifacts
#
# GUI/xvfb is the primary lane; batch is secondary.  Correctness failures fail
# the run.  Timing values are diagnostics only and have no pass/fail threshold.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
EMACS_BIN="${EMACS:-emacs}"
if [ -z "${PACKAGE_USER_DIR:-}" ]; then
    EMACS_MAJOR_VERSION=$("$EMACS_BIN" --batch -Q \
        --eval '(princ emacs-major-version)')
    export PACKAGE_USER_DIR="$PROJECT_DIR/.cache/elpa/$EMACS_MAJOR_VERSION"
fi

BATCH=0
REPS=3
OUT_DIR=""
SCENARIOS=()

usage() {
    awk 'NR > 1 { if ($0 !~ /^#/) exit; sub(/^# ?/, ""); print }' "$0"
}

require_arg() {
    local option="$1"
    if [[ $# -lt 2 || -z "${2:-}" ]]; then
        echo "ERROR: $option requires an argument" >&2
        usage >&2
        exit 1
    fi
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --batch) BATCH=1; shift ;;
        -c|--count)
            require_arg "$1" "${2:-}"
            REPS="$2"
            shift 2
            ;;
        --out-dir)
            require_arg "$1" "${2:-}"
            OUT_DIR="$2"
            shift 2
            ;;
        --scenario)
            require_arg "$1" "${2:-}"
            SCENARIOS+=("$2")
            shift 2
            ;;
        --scenarios)
            require_arg "$1" "${2:-}"
            IFS=',' read -r -a SCENARIOS <<< "$2"
            shift 2
            ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; usage >&2; exit 1 ;;
    esac
done

if ! [[ "$REPS" =~ ^[0-9]+$ ]] || [[ "$REPS" -lt 1 ]]; then
    echo "ERROR: repetition count must be a positive integer: $REPS" >&2
    exit 1
fi
if [[ ${#SCENARIOS[@]} -eq 0 ]]; then
    SCENARIOS=(full)
fi

scenario_env() {
    case "$1" in
        smoke)
            cat <<'EOF'
PI_SD_BENCH_HISTORY_TURNS=6
PI_SD_BENCH_HISTORY_TEXT_BYTES=400
PI_SD_BENCH_TIMER_TEXT_DELTAS=12
PI_SD_BENCH_TEXT_BURST=12
PI_SD_BENCH_THINKING_DELTAS=6
PI_SD_BENCH_THINKING_BURST=6
PI_SD_BENCH_BACKLOG_DELTAS=20
PI_SD_BENCH_TOOLCALL_REPEATS=8
PI_SD_BENCH_BURST_PAUSE_MS=80
PI_SD_BENCH_SEED=20240817
PI_SD_BENCH_TIMEOUT_SECONDS=30
EOF
            ;;
        full)
            cat <<'EOF'
PI_SD_BENCH_HISTORY_TURNS=180
PI_SD_BENCH_HISTORY_TEXT_BYTES=1200
PI_SD_BENCH_TIMER_TEXT_DELTAS=700
PI_SD_BENCH_TEXT_BURST=20
PI_SD_BENCH_THINKING_DELTAS=80
PI_SD_BENCH_THINKING_BURST=20
PI_SD_BENCH_BACKLOG_DELTAS=300
PI_SD_BENCH_TOOLCALL_REPEATS=0
PI_SD_BENCH_BURST_PAUSE_MS=80
PI_SD_BENCH_SEED=20240817
PI_SD_BENCH_TIMEOUT_SECONDS=120
EOF
            ;;
        toolcall)
            cat <<'EOF'
PI_SD_BENCH_HISTORY_TURNS=180
PI_SD_BENCH_HISTORY_TEXT_BYTES=1200
PI_SD_BENCH_TIMER_TEXT_DELTAS=12
PI_SD_BENCH_TEXT_BURST=12
PI_SD_BENCH_THINKING_DELTAS=6
PI_SD_BENCH_THINKING_BURST=6
PI_SD_BENCH_BACKLOG_DELTAS=20
PI_SD_BENCH_TOOLCALL_REPEATS=160
PI_SD_BENCH_BURST_PAUSE_MS=80
PI_SD_BENCH_SEED=20240817
PI_SD_BENCH_TIMEOUT_SECONDS=120
EOF
            ;;
        *) echo "Unknown scenario: $1" >&2; exit 1 ;;
    esac
}

for scenario in "${SCENARIOS[@]}"; do
    scenario_env "$scenario" >/dev/null
done

if [[ -z "$OUT_DIR" ]]; then
    if [[ ${#SCENARIOS[@]} -eq 1 && "${SCENARIOS[0]}" == "smoke" ]]; then
        OUT_DIR="$PROJECT_DIR/tmp/stream-delta-bench/smoke"
    elif [[ "$BATCH" == "1" ]]; then
        OUT_DIR="$PROJECT_DIR/tmp/stream-delta-bench/batch"
    else
        OUT_DIR="$PROJECT_DIR/tmp/stream-delta-bench/gui"
    fi
fi
case "$OUT_DIR" in
    /*) ;;
    *) OUT_DIR="$PROJECT_DIR/$OUT_DIR" ;;
esac
while [[ "$OUT_DIR" != "/" && "$OUT_DIR" == */ ]]; do
    OUT_DIR="${OUT_DIR%/}"
done
if [[ -z "$OUT_DIR" || "$OUT_DIR" == "/" ]]; then
    echo "ERROR: refusing unsafe output directory: $OUT_DIR" >&2
    exit 1
fi

BENCH_MARKER="$OUT_DIR/.pilish-stream-delta-bench"
if [[ -L "$OUT_DIR" ]]; then
    echo "ERROR: refusing symlink output directory: $OUT_DIR" >&2
    exit 1
fi
if [[ -e "$OUT_DIR" && ! -d "$OUT_DIR" ]]; then
    echo "ERROR: refusing to replace non-directory output path: $OUT_DIR" >&2
    exit 1
fi
if [[ -d "$OUT_DIR" && ! -f "$BENCH_MARKER" ]] \
   && find "$OUT_DIR" -mindepth 1 -maxdepth 1 -print -quit | grep -q .; then
    echo "ERROR: refusing to remove non-empty output directory without benchmark marker: $OUT_DIR" >&2
    exit 1
fi

export PI_SD_BENCH_PROJECT_DIR="$PROJECT_DIR"
EMACS_INIT=(
    -Q -L "$PROJECT_DIR"
    --eval '(setq inhibit-startup-screen t)'
    --eval '(require (quote package))'
    --eval '(let ((dir (getenv "PACKAGE_USER_DIR"))) (when dir (setq package-user-dir (directory-file-name (expand-file-name dir)))))'
    --eval '(package-initialize)'
    --eval '(let ((project (getenv "PI_SD_BENCH_PROJECT_DIR"))) (unless project (error "PI_SD_BENCH_PROJECT_DIR is unset")) (setq load-path (cons (expand-file-name project) load-path)))'
    -l "$SCRIPT_DIR/pilish-stream-delta-bench.el"
)

printf '=== Pilish Stream-Delta Benchmarks ===\n'
printf 'Project: %s\n' "$PROJECT_DIR"
if [[ "$BATCH" == "1" ]]; then
    MODE="batch"
    printf 'Mode: batch (secondary lane), %s reps\n' "$REPS"
else
    MODE="gui-xvfb"
    printf 'Mode: GUI via xvfb (primary lane), %s reps\n' "$REPS"
    if ! command -v xvfb-run >/dev/null 2>&1; then
        echo "ERROR: xvfb-run not found. Install xvfb or use --batch." >&2
        exit 1
    fi
fi
printf 'Scenarios: %s\n\n' "${SCENARIOS[*]}"

rm -rf -- "$OUT_DIR"
mkdir -p -- "$OUT_DIR"
touch -- "$BENCH_MARKER"

for scenario in "${SCENARIOS[@]}"; do
    for ((iteration = 1; iteration <= REPS; iteration++)); do
        run_dir="$OUT_DIR/$scenario/iter-$(printf '%02d' "$iteration")"
        mkdir -p "$run_dir"
        env_file="$run_dir/env"
        scenario_env "$scenario" > "$env_file"
        set -a
        # shellcheck disable=SC1090
        source "$env_file"
        set +a
        export PI_SD_BENCH_SCENARIO="$scenario"
        export PI_SD_BENCH_ITERATION="$iteration"
        export PI_SD_BENCH_OUT_DIR="$run_dir"
        export PI_SD_BENCH_RUNNER_OUT_DIR="$OUT_DIR"
        export PI_SD_BENCH_DISPLAY=$([[ "$BATCH" == "1" ]] && echo 0 || echo 1)

        printf '[%s/%s] running\n' "$scenario" "$iteration"
        if [[ "$BATCH" == "1" ]]; then
            if ! "$EMACS_BIN" --batch "${EMACS_INIT[@]}" \
                -f pilish-sd-bench-run-batch \
                > "$run_dir/stdout.log" 2> "$run_dir/stderr.log"; then
                cat "$run_dir/stdout.log"
                cat "$run_dir/stderr.log" >&2
                exit 1
            fi
        else
            if ! xvfb-run -a env GDK_BACKEND=x11 PATH="$PATH" \
                "$EMACS_BIN" --geometry 120x40 "${EMACS_INIT[@]}" \
                --eval '(let ((standard-output (function external-debugging-output))) (kill-emacs (if (pilish-sd-bench-run) 0 1)))' \
                </dev/null > "$run_dir/stdout.log" 2> "$run_dir/stderr.log"; then
                cat "$run_dir/stdout.log"
                cat "$run_dir/stderr.log" >&2
                exit 1
            fi
        fi
    done
done

python3 - "$OUT_DIR" "$MODE" "$REPS" "${SCENARIOS[*]}" <<'PY'
from __future__ import annotations

import csv
import json
import statistics
import sys
from pathlib import Path
from typing import Any

out = Path(sys.argv[1])
mode = sys.argv[2]
reps = sys.argv[3]
scenario_arg = sys.argv[4]
rows: list[dict[str, Any]] = []


def mapping(value: Any) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


for result_path in sorted(out.glob("*/iter-*/result.json")):
    with result_path.open(encoding="utf-8") as handle:
        result = json.load(handle)
    checks = result.get("checks", [])
    failed_checks = [
        str(check.get("name")) for check in checks if check.get("ok") is not True
    ]
    derived_ok = result.get("settled") is True and not failed_checks
    if result.get("ok") is not derived_ok:
        failed_checks.append("result-ok-verdict-mismatch")
    filters = mapping(result.get("processFilters"))
    backlog = mapping(filters.get("backlog"))
    flushes = mapping(result.get("flushes"))
    displays = mapping(result.get("displayCalls"))
    toolcalls = mapping(result.get("toolcallPaints"))
    probe = mapping(result.get("probe"))
    gc = mapping(result.get("gc"))
    history = mapping(result.get("history"))
    md_ts = mapping(result.get("mdTs"))
    rows.append(
        {
            "scenario": result.get("scenario"),
            "iteration": result.get("iteration"),
            "ok": result.get("ok") is True and derived_ok and not failed_checks,
            "settled": result.get("settled") is True,
            "historyBytes": history.get("renderedBytes") or 0,
            "wallMs": result.get("wallMs") or 0,
            "filterCount": filters.get("count") or 0,
            "filterTotalMs": filters.get("totalMs") or 0,
            "filterMaxMs": filters.get("maxMs") or 0,
            "backlogFilterMs": backlog.get("wallMs") or 0,
            "timerFlushes": flushes.get("timerDriven") or 0,
            "synchronousFlushes": flushes.get("synchronous") or 0,
            "textDisplayCalls": displays.get("text") or 0,
            "thinkingDisplayCalls": displays.get("thinking") or 0,
            "displayCalls": displays.get("total") or 0,
            "deltaEvents": displays.get("deltaEvents") or 0,
            "displayRatio": displays.get("ratio") or 0,
            "toolcallPaints": toolcalls.get("count") or 0,
            "toolcallDeltaEvents": toolcalls.get("deltaEvents") or 0,
            "toolcallPaintRatio": toolcalls.get("ratio") or 0,
            "probeP95Ms": probe.get("p95Ms") or 0,
            "probeMaxMs": probe.get("maxMs") or 0,
            "gcs": gc.get("collections") or 0,
            "gcSeconds": gc.get("seconds") or 0,
            "mdTsDirtyBefore": md_ts.get("beforeCount"),
            "mdTsDirtyAfter": md_ts.get("afterCount"),
            "failedChecks": ";".join(failed_checks),
            "error": result.get("error") or "",
            "resultPath": str(result_path),
        }
    )

csv_path = out / "summary.csv"
if rows:
    with csv_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

summary: list[str] = [
    "# Pilish stream-delta benchmark summary",
    "",
    "Synthetic deterministic workload only; timing values are diagnostic.",
    "",
    f"- Mode: `{mode}`",
    f"- Repetitions per scenario: `{reps}`",
    f"- Scenarios: `{scenario_arg}`",
    "- Timing thresholds: `none` (correctness failures fail the run)",
    "",
    "| scenario | wall ms | history bytes | filter total/max ms | backlog ms | timer/sync flushes | displays/deltas | display ratio | toolcall paints/deltas | toolcall ratio | probe p95/max ms | GC | md-ts dirty before/after | successful runs |",
    "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|",
]

print("\nsummary")
print("scenario  wall-ms history-B filter-total/max backlog timer/sync displays/deltas ratio toolcall-paints/deltas toolcall-ratio probe-p95/max GC dirty ok")
failed = [row for row in rows if not row["ok"]]
for scenario in sorted({str(row["scenario"]) for row in rows}):
    all_rows = [row for row in rows if str(row["scenario"]) == scenario]
    good = [row for row in all_rows if row["ok"]]
    if not good:
        print(f"{scenario:<8} no successful runs")
        summary.append(
            f"| {scenario} | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a | 0/{len(all_rows)} |"
        )
        continue

    def median(key: str) -> float:
        return statistics.median(float(row[key]) for row in good)

    def maximum(key: str) -> float:
        return max(float(row[key]) for row in good)

    wall_median = median("wallMs")
    sample = min(good, key=lambda row: abs(float(row["wallMs"]) - wall_median))
    ok_count = f"{len(good)}/{len(all_rows)}"
    dirty = f"{sample['mdTsDirtyBefore']}/{sample['mdTsDirtyAfter']}"
    print(
        f"{scenario:<8} {wall_median:7.1f} {int(sample['historyBytes']):9d} "
        f"{median('filterTotalMs'):7.1f}/{maximum('filterMaxMs'):5.1f} "
        f"{maximum('backlogFilterMs'):7.1f} "
        f"{int(sample['timerFlushes'])}/{int(sample['synchronousFlushes'])} "
        f"{int(sample['displayCalls'])}/{int(sample['deltaEvents'])} "
        f"{float(sample['displayRatio']):.4f} "
        f"{int(sample['toolcallPaints'])}/{int(sample['toolcallDeltaEvents'])} "
        f"{float(sample['toolcallPaintRatio']):.4f} "
        f"{median('probeP95Ms'):.1f}/{maximum('probeMaxMs'):.1f} "
        f"{int(maximum('gcs'))} {dirty} {ok_count}"
    )
    summary.append(
        f"| {scenario} | {wall_median:.1f} | {int(sample['historyBytes'])} | "
        f"{median('filterTotalMs'):.1f}/{maximum('filterMaxMs'):.1f} | "
        f"{maximum('backlogFilterMs'):.1f} | "
        f"{int(sample['timerFlushes'])}/{int(sample['synchronousFlushes'])} | "
        f"{int(sample['displayCalls'])}/{int(sample['deltaEvents'])} | "
        f"{float(sample['displayRatio']):.4f} | "
        f"{int(sample['toolcallPaints'])}/{int(sample['toolcallDeltaEvents'])} | "
        f"{float(sample['toolcallPaintRatio']):.4f} | "
        f"{median('probeP95Ms'):.1f}/{maximum('probeMaxMs'):.1f} | "
        f"{int(maximum('gcs'))} | {dirty} | {ok_count} |"
    )

summary.extend(
    [
        "",
        "## Artifacts",
        "",
        f"- CSV: `{csv_path}`",
        "- Per-run reports: `SCENARIO/iter-NN/report.md`",
        "- Per-run JSON: `SCENARIO/iter-NN/result.json`",
        "- Per-run timing TSV: `SCENARIO/iter-NN/times.tsv`",
    ]
)
if failed:
    summary.extend(["", "## Correctness failures", ""])
    for row in failed:
        detail = row["error"] or f"failed checks: {row['failedChecks']}"
        summary.append(
            f"- {row['scenario']} iter {row['iteration']}: {detail} ({row['resultPath']})"
        )
summary_path = out / "summary.md"
summary_path.write_text("\n".join(summary) + "\n", encoding="utf-8")
print(f"\nWrote {csv_path}")
print(f"Wrote {summary_path}")

if not rows:
    print("ERROR: no benchmark result rows found", file=sys.stderr)
    raise SystemExit(1)
if failed:
    print("ERROR: one or more stream-delta correctness checks failed", file=sys.stderr)
    raise SystemExit(1)
PY
