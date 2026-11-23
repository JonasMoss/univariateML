#!/usr/bin/env bash
# Run from the repository root. Each snapshot uses one workstation slot.
set -euo pipefail
root=$PWD
scratch=$root/build/scratch/maxset-cont
baseline=$root/dev/research/evaluations/2026-11-15-fit-snapshot-data
comparison=$root/build/scratch/diff_baseline.py
evidence=$root/dev/research/evaluations/2026-11-23-maxset-cont-data
detail=$root/dev/research/evaluations/2026-11-22-maxset-core-data/snapshot_changes.py
export TMPDIR=$root/build/tmp JUST_TEMPDIR=$root/build/tmp XDG_RUNTIME_DIR=$root/build/tmp
export CCACHE_DIR=${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}
export CCACHE_BASEDIR=$root CCACHE_NOHASHDIR=1 GIT_OPTIONAL_LOCKS=0
mkdir -p "$TMPDIR" "$scratch"
run=/tmp/umvl-cpu/run

for mode in native double routes-native routes-double; do
    flags=()
    if [[ $mode == *double ]]; then flags+=(--no-long-double); fi
    if [[ $mode == routes-* ]]; then flags+=(--modifier-routes); fi
    "$run" python3 dev/validation/fit-snapshot.py --output "$scratch/$mode" "${flags[@]}"
    status=0
    if [[ -f $comparison ]]; then
        "$run" python3 "$comparison" "$scratch/$mode" "$baseline/$mode.jsonl.gz" \
            > "$evidence/diff-$mode.txt" || status=$?
    else
        "$run" python3 dev/validation/fit-snapshot.py --diff "$baseline/$mode.jsonl.gz" "$scratch/$mode" \
            > "$evidence/diff-$mode.txt" || status=$?
    fi
    if [[ $status -gt 1 ]]; then exit "$status"; fi
    "$run" python3 "$detail" "$scratch/$mode" "$baseline/$mode.jsonl.gz" \
        > "$evidence/fields-$mode.jsonl"
    cp "$scratch/$mode/provenance.json" "$evidence/$mode.provenance.json"
done

"$run" python3 dev/validation/fit-consistency.py \
    "$scratch/native,$scratch/routes-native" "$scratch/double,$scratch/routes-double" \
    > "$evidence/strict.txt" 2>&1
