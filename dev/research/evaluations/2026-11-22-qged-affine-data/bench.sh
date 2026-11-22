#!/bin/sh
# From the worktree root, run through the single-slot launcher.
set -eu
data=dev/research/evaluations/2026-11-22-qged-affine-data
work=build/scratch/qged
boost=${BOOST_INCLUDE:-$(Rscript -e 'cat(system.file("include", package="BH"))')}
mkdir -p "$work/timing-base"
git archive 955412ee univariateML-cpp/include | tar -x -C "$work/timing-base"
for variant in master fixed; do
    include=univariateML-cpp/include
    if [ "$variant" = master ]; then include="$work/timing-base/$include"; fi
    "${CXX:-g++}" -std=c++17 -O2 -I "$boost" -I "$include" "$data/bench.cpp" -o "$work/bench-$variant"

done
for repeat in 1 2 3 4 5; do
    master="$work/timing-master-$repeat.csv"
    fixed="$work/timing-fixed-$repeat.csv"
    : > "$master"
    : > "$fixed"
    row=0
    while [ "$row" -lt 96 ]; do
        if [ "$(((repeat+row)%2))" -eq 0 ]; then
            taskset -c "${QGED_BENCH_CPU:-0}" "$work/bench-master" "$row" >> "$master"
            taskset -c "${QGED_BENCH_CPU:-0}" "$work/bench-fixed" "$row" >> "$fixed"
        else
            taskset -c "${QGED_BENCH_CPU:-0}" "$work/bench-fixed" "$row" >> "$fixed"
            taskset -c "${QGED_BENCH_CPU:-0}" "$work/bench-master" "$row" >> "$master"
        fi
        row=$((row+1))
    done
done
python3 - "$work" "$data" <<'EOF_DATA'
import csv
from pathlib import Path
from statistics import median
import sys

work, data = map(Path, sys.argv[1:])
for variant in ('master', 'fixed'):
    runs = [list(csv.reader((work / f'timing-{variant}-{i}.csv').read_text().splitlines()))
            for i in range(1, 6)]
    with (data / f'timing-{variant}.csv').open('w', newline='') as f:
        out = csv.writer(f, lineterminator='\n')
        out.writerow(('shape', 'mean', 'sd', 'log', 'lower', 'median_us', *(f'run{i}_us' for i in range(1, 6))))
        for rows in zip(*runs, strict=True):
            assert all(row[:5] == rows[0][:5] for row in rows)
            times = [float(row[5]) for row in rows]
            out.writerow([*rows[0][:5], median(times), *times])
EOF_DATA
