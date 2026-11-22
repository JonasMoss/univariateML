#!/bin/sh
# From repository root, run through /tmp/umvl-cpu/run.
set -eu
base=dev/research/evaluations/2026-11-15-fit-snapshot-data
target=build/scratch/pged-snapshots
mkdir -p "$target"
for mode in native double; do
    flag=
    if [ "$mode" = double ]; then flag=--no-long-double; fi
    python3 dev/validation/fit-snapshot.py --family ged,sged --output "$target/$mode" $flag
    python3 - "$base/$mode.jsonl.gz" "$target/$mode-baseline.jsonl.gz" <<'PY'
import gzip
import json
import sys
with gzip.open(sys.argv[1], 'rt') as source, gzip.open(sys.argv[2], 'wt') as target:
    for row in source:
        if json.loads(row)['family'] in ('ged', 'sged'):
            target.write(row)
PY
    if [ -f build/scratch/diff_baseline.py ]; then
        python3 build/scratch/diff_baseline.py "$target/$mode" "$target/$mode-baseline.jsonl.gz"
    else
        python3 dev/validation/fit-snapshot.py --family ged,sged \
            --diff "$target/$mode-baseline.jsonl.gz" "$target/$mode"
    fi
done
