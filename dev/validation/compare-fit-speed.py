#!/usr/bin/env python3
"""Compare fit-speed-comparison stdout; retain every non-equivalent row."""
import argparse
import csv
import math
from pathlib import Path

parser = argparse.ArgumentParser(__doc__)
parser.add_argument('before', type=Path)
parser.add_argument('after', type=Path)
parser.add_argument('--differences', type=Path, required=True)
args = parser.parse_args()
before = [line.split() for line in args.before.read_text().splitlines()]
after = [line.split() for line in args.after.read_text().splitlines()]
if len(before) != len(after):
    raise SystemExit('row counts differ')
counts = dict(identical=0, equivalent=0, different=0)
rows = []
def close(a, b, tol):
    x, y = float(a), float(b)
    if x == y or (math.isnan(x) and math.isnan(y)):
        return True
    if not (math.isfinite(x) and math.isfinite(y)):
        return False
    return abs(x-y) <= tol*max(abs(x), abs(y))
for a, b in zip(before, after):
    if len(a) != len(b):
        raise SystemExit("column counts differ")
    if a[:3] != b[:3]:
        raise SystemExit('workloads differ')
    if a == b:
        counts['identical'] += 1
    elif a[3] == b[3] and all(close(x,y,1e-8) for x,y in zip(a[4:-1], b[4:-1])) and close(a[-1],b[-1],1e-12):
        counts['equivalent'] += 1
    else:
        counts['different'] += 1
        rows.append([' '.join(a[:3]), a[3], b[3], a[-1], b[-1], ' '.join(a[4:-1]), ' '.join(b[4:-1])])
with args.differences.open('w') as stream:
    writer=csv.writer(stream)
    writer.writerow(['case','old_status','new_status','old_logLik','new_logLik','old_estimates','new_estimates'])
    writer.writerows(rows)
print(counts)
raise SystemExit(bool(rows))
