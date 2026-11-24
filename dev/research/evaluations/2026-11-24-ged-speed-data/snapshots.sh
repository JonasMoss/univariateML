#!/bin/sh
# From the repository root, run through the slot lock. The fitter probes
# compile and run in parallel when the lock supplies more than one slot.
set -eu
base=dev/research/evaluations/2026-11-15-fit-snapshot-data
target=build/scratch/ged-speed/snapshots
status=0
for mode in native double routes-native routes-double; do
    set --
    case "$mode" in *double) set -- "$@" --no-long-double ;; esac
    case "$mode" in routes-*) set -- "$@" --modifier-routes ;; esac
    python3 dev/validation/fit-snapshot.py --jobs "${CMAKE_BUILD_PARALLEL_LEVEL:-1}" \
        --output "$target/$mode" "$@"
    if [ -f build/scratch/diff_baseline.py ]; then
        python3 build/scratch/diff_baseline.py "$target/$mode" "$base/$mode.jsonl.gz" || status=1
    else
        python3 dev/validation/fit-snapshot.py --diff "$base/$mode.jsonl.gz" "$target/$mode" || status=1
    fi
done
exit "$status"
