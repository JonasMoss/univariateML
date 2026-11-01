#!/usr/bin/env python3
import csv
import math
import statistics
from collections import Counter
from pathlib import Path

p = Path(__file__).parent
rows = list(csv.DictReader((p / 'paired.csv').open()))
assert len(rows) == 2400, len(rows)
transitions = Counter((r['old_status'], r['new_status']) for r in rows
                      if r['old_status'] != r['new_status'])
losses = []
improvements = []
for r in rows:
    a, b = float(r['old_ll']), float(r['new_ll'])
    if r['old_status'] == '0' and r['new_status'] == '0':
        if a - b > 1e-12 * max(1, abs(a)):
            losses.append(r)
        if b > a:
            improvements.append(b-a)
out = [f'pairs={len(rows)} status_changes={dict(transitions)} losses={len(losses)} '
       f'improvements={len(improvements)} max_improvement={max(improvements, default=0):.17g}',
       'n mask old_mean_ms new_mean_ms ratio']
for n in [8, 32, 128, 500]:
    for mask in range(4):
        sub = [r for r in rows if int(r['n']) == n and int(r['mask']) == mask]
        a = statistics.mean(float(r['old_s']) for r in sub)*1000
        b = statistics.mean(float(r['new_s']) for r in sub)*1000
        out.append(f'{n} {mask} {a:.6f} {b:.6f} {b/a:.3f}')
golden = list(csv.DictReader((p / 'golden.csv').open()))
out.append('golden mask old_median_ms new_median_ms ratio')
for mask in range(4):
    sub = [r for r in golden if int(r['mask']) == mask]
    a = statistics.median(float(r['old_s']) for r in sub)*1000
    b = statistics.median(float(r['new_s']) for r in sub)*1000
    out.append(f'{mask} {a:.6f} {b:.6f} {b/a:.3f}')
text = '\n'.join(out)+'\n'
(p / 'summary.txt').write_text(text)
print(text, end='')
assert not losses
