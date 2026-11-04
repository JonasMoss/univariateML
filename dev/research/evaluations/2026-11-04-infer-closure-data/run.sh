#!/usr/bin/env bash
set -euo pipefail
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
evidence_dir="$(cd -- "$(dirname -- "$0")" && pwd)"
python3 "$evidence_dir/symbolic.py"
python3 "$evidence_dir/check.py"
