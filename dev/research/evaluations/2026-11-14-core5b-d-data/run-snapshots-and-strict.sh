#!/usr/bin/env bash
set -euo pipefail
export GIT_OPTIONAL_LOCKS=0
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-14-core5b-d-data
F=pois,binom,nbinom,geom,zeta,lgser,norm,exp,gamma,laplace
B=dev/research/evaluations/2026-11-14-fit-snapshot-data
for mode in native double routes-native routes-double; do
  args=()
  if [[ "$mode" == routes-* ]]; then args+=(--modifier-routes); else args+=(--family "$F"); fi
  if [[ "$mode" == *double ]]; then args+=(--no-long-double); fi
  echo "Starting snapshot $mode"
  /tmp/umvl-cpu/run python3 dev/validation/fit-snapshot.py --output "build/snapshots/$mode" "${args[@]}" --jobs 1 > "$D/$mode-run.log" 2>&1
  args=()
  if [[ "$mode" != routes-* ]]; then args+=(--family "$F"); fi
  /tmp/umvl-cpu/run python3 dev/validation/fit-snapshot.py --diff "$B/$mode.jsonl.gz" "build/snapshots/$mode" "${args[@]}" > "$D/$mode-diff.log" 2>&1 || [ "$?" = 1 ]
done
for judge in fit-judge fit-density fit-consistency fit-refine; do
  args=()
  if [[ "$judge" == fit-refine ]]; then args+=(--cache-only); fi
  /tmp/umvl-cpu/run python3 "dev/validation/$judge.py" build/snapshots/native,build/snapshots/routes-native build/snapshots/double,build/snapshots/routes-double "${args[@]}" --prune > "$D/$judge-prune.log" 2>&1
done
/tmp/umvl-cpu/run python3 dev/validation/strict-seam.py native double --prune > "$D/strict-seam-prune.log" 2>&1
/tmp/umvl-cpu/run python3 "$D/preserve-known.py" > "$D/known-preservation.log" 2>&1
/tmp/umvl-cpu/run just strict build/snapshots/native,build/snapshots/routes-native build/snapshots/double,build/snapshots/routes-double > "$D/strict.log" 2>&1
