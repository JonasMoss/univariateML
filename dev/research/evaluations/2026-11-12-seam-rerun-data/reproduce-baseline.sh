#!/usr/bin/env bash
# Read-only Git extraction; no source archive is retained in evaluation data.
set -euo pipefail
root=$(git rev-parse --show-toplevel)
cd "$root"
dest="$root/build/scratch/seam-rerun-baseline"
mkdir -p "$dest"
git archive 63bc2200ead3cab6c957543b2f1765d419bb9c89 | tar -x -C "$dest"
cd "$dest"
python3 - <<'PY'
from pathlib import Path
p=Path('dev/validation/seam-check.py')
p.write_text(p.read_text().replace("'-O1'","'-O2'"))
PY
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --output build/native.jsonl
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --no-long-double --output build/double.jsonl
