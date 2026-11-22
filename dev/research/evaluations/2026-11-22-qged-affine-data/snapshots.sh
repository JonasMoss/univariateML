#!/bin/sh
set -eu
root=$PWD
base=$root/dev/research/evaluations/2026-11-15-fit-snapshot-data
target=$root/build/scratch/qged/snapshots
baseline=$root/build/scratch/qged/baseline
mkdir -p "$baseline"
git archive 7a57d011 univariateML-cpp/include bindings/common \
    dev/validation/fit-snapshot.py \
    dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz \
    dev/research/evaluations/2026-11-13-retire-16-data/route-inputs.json \
    dev/research/evaluations/2026-11-21-speed-e-data/snapshot-priority.json |
    tar -x -C "$baseline"
for mode in native double; do
    flag=
    if [ "$mode" = double ]; then flag=--no-long-double; fi
    echo "$mode branch-point fits"
    python3 "$baseline/dev/validation/fit-snapshot.py" --family ged,sged \
        --output "$target/branch-$mode" $flag
    echo "$mode current fits"
    python3 dev/validation/fit-snapshot.py --family ged,sged --output "$target/current-$mode" $flag
    echo "$mode branch point versus current"
    python3 dev/validation/fit-snapshot.py --family ged,sged \
        --diff "$target/branch-$mode" "$target/current-$mode"
    echo "$mode stored versus current"
    comparison=0
    python3 dev/validation/fit-snapshot.py --family ged,sged \
        --diff "$base/$mode.jsonl.gz" "$target/current-$mode" || comparison=$?
    # The preceding equality attributes all stored differences to the branch point.
    [ "$comparison" -le 1 ]
done
