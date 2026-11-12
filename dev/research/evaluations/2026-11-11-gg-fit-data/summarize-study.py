"""Summarize held-study.csv by held mask, status and certification."""
from collections import Counter, defaultdict
from pathlib import Path
import csv
import statistics

path=Path(__file__).with_name('held-study.csv')
with path.open(newline='') as stream:
    rows=list(csv.DictReader(stream))
groups=defaultdict(list)
for row in rows:
    groups[int(row['held_mask'])].append(row)
for mask, group in sorted(groups.items()):
    outcomes=Counter(('certified' if r['certified']=='1' else 'uncertified')
                     if r['status']=='Ok' else r['status'] for r in group)
    times=[float(r['milliseconds']) for r in group]
    print(f'mask {mask}: {dict(outcomes)}; median {statistics.median(times):.3f} ms; max {max(times):.3f} ms')
