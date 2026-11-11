#!/usr/bin/env bash
# Caller reserves one CPU slot. No nested slot reservations or background jobs.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export MAKEFLAGS=-j1
mkdir -p "$TMPDIR" build/scratch
D=dev/research/evaluations/2026-11-11-acc-fb-t-data
echo 'START final independent reference signs (workstation)'
python3 "$D/check-reference-signs.py" > build/scratch/reference-signs-final.log
cat build/scratch/reference-signs-final.log
echo 'START public R spots (workstation)'
Rscript "$D/capture.R" > "$D/after-r.jsonl"
echo 'START public WASM spots (workstation)'
node --v8-pool-size=1 "$D/capture.mjs" > "$D/after-wasm.jsonl"
python3 "$D/verify-spots.py" after-r after-wasm
echo 'START all-free SSTD golden R/WASM spots (workstation)'
Rscript "$D/capture-sstd-golden.R" > "$D/golden-sstd-after-r.json"
node --v8-pool-size=1 "$D/capture-sstd-golden.mjs" > "$D/golden-sstd-after-wasm.json"
echo 'START fresh independent 90-digit all-free SSTD root (workstation)'
python3 "$D/check-sstd-golden.py" > build/scratch/sstd-golden-final.log
cat build/scratch/sstd-golden-final.log
echo 'START final replay comparison and preservation checks (workstation)'
python3 "$D/compare.py" > build/scratch/comparison-final.log
python3 "$D/verify.py" > build/scratch/verification-final.log
echo 'PASS final independent and public-interface accuracy checks (workstation)'
