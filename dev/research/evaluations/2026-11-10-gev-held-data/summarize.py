#!/usr/bin/env python3
"""Summarize the frozen seeded-study CSV without external dependencies."""
import csv
import json
from pathlib import Path
import statistics

root = Path(__file__).resolve().parent
rows = list(csv.DictReader((root / 'study.csv').open()))

def summary(group):
    times = sorted(float(r['milliseconds']) for r in group)
    certified = sum(r['status'] == '0' and r['certified'] == '1' for r in group)
    uncertified = sum(r['status'] == '0' and r['certified'] == '0' for r in group)
    stops = len(group) - certified - uncertified
    return dict(fits=len(group), certified=certified, uncertified=uncertified,
                stops=stops, certified_fraction=certified / len(group),
                uncertified_fraction=uncertified / len(group), stop_fraction=stops / len(group),
                median_ms=statistics.median(times), mean_ms=statistics.mean(times),
                p95_ms=times[max(0, (95 * len(times) + 99) // 100 - 1)], max_ms=max(times))

output = dict(seed=17020261110, datasets=len({r["sample_id"] for r in rows}), machine='workstation', maxit=100, tol=1e-10,
              replicates=2, shapes_gev=[-.9, -.5, 0, .5, 1, 2], n=[3, 10, 50, 100, 500],
              total=summary(rows), groups={}, by_shape={}, by_n={})
for family, hold in sorted({(r['family'], r['hold']) for r in rows}):
    label = family + '_' + hold
    group = [r for r in rows if (r['family'], r['hold']) == (family, hold)]
    output['groups'][label] = summary(group)
    output['by_shape'][label] = {
        shape: summary([r for r in group if r['shape'] == shape])
        for shape in sorted({r['shape'] for r in group}, key=float)}
    output['by_n'][label] = {
        n: summary([r for r in group if r['n'] == n])
        for n in sorted({r['n'] for r in group}, key=int)}
(root / 'summary.json').write_text(json.dumps(output, indent=2) + '\n')
print(json.dumps(output['groups'], indent=2))
