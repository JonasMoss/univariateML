#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
bash dev/research/evaluations/2026-11-08-gev-allfree-data/benchmark.sh
bash dev/research/evaluations/2026-11-08-gev-allfree-data/held-benchmark.sh
python3 dev/research/evaluations/2026-11-08-gev-allfree-data/summarize.py > dev/research/evaluations/2026-11-08-gev-allfree-data/timing-summary.json
