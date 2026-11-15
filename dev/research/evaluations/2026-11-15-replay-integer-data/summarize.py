#!/usr/bin/env python3
"""Tables for the replay-integer report from records.jsonl(.gz) (cert-replay.py --records).

  python3 summarize.py [RECORDS]

Counts by family, comparison, level, verdict and mode for the mechanism-G comparisons (search, tail, tie, coef);
cases judged per family and mask; every comparison that is not 'holds'; grid sizes of the search comparisons.
"""
import collections, gzip, json, sys
from pathlib import Path

MINE = ('search', 'tail', 'tie', 'coef')
path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).with_name('records.jsonl')
opener = gzip.open if path.suffix == '.gz' else open
rows = [json.loads(line) for line in opener(path, 'rt')]
counts = collections.Counter(); cases = collections.Counter(); points = collections.defaultdict(list)
for r in rows:
    mine = [c for c in r['comparisons'] if c['name'] in MINE]
    if not mine: continue
    cases[(r['family'], r['mode'], r['mask'])] += 1
    for c in mine:
        counts[(r['family'], c['name'], c['rigor'], c['verdict'], r['mode'])] += 1
        s = c.get('search') or {}
        if 'points' in s: points[r['family']].append(s['points'])
print('family | comparison | level | verdict | native | double')
for key in sorted({k[:4] for k in counts}):
    print('%s | %s | %s | %s | %d | %d' % (*key, counts.get(key+('native',), 0), counts.get(key+('double',), 0)))
print('\ncases judged by family, mode (comparisons of this module only):')
for fam in sorted({k[0] for k in cases}):
    print('  %-7s native %3d double %3d' % (fam, sum(v for k, v in cases.items() if k[0] == fam and k[1] == 'native'),
                                           sum(v for k, v in cases.items() if k[0] == fam and k[1] == 'double')))
print('\ncases by family and mask (native):')
for fam in sorted({k[0] for k in cases}):
    print('  %-7s %s' % (fam, {k[2]: v for k, v in sorted(cases.items()) if k[0] == fam and k[1] == 'native'}))
print('\nmode totals:', {m: sum(1 for r in rows if r['mode'] == m and any(c['name'] in MINE for c in r['comparisons'])) for m in ('native', 'double')},
      'comparisons', {m: sum(1 for r in rows if r['mode'] == m for c in r['comparisons'] if c['name'] in MINE) for m in ('native', 'double')})
print('\ngrid sizes (points per grid search comparison):')
for fam, v in sorted(points.items()): print('  %-7s n=%d min %d median %d max %d' % (fam, len(v), min(v), sorted(v)[len(v)//2], max(v)))
print('\nnot holding:')
seen = collections.Counter()
for r in rows:
    for c in r['comparisons']:
        if c['name'] in MINE and c['verdict'] != 'holds':
            seen[(r['family'], r['mask'], c['name'], c['verdict'], c['note'][:90] if c['verdict'] != 'info' else '')] += 1
for k, v in sorted(seen.items()): print('  %s mask %d %s %s x%d %s' % (k[0], k[1], k[2], k[3], v, k[4]))
print('\nfails and open, per case:')
for r in rows:
    for c in r['comparisons']:
        if c['name'] in MINE and c['verdict'] in ('fails', 'open'):
            print('  %s %s %s/%s mask %d: %s %s | %s | %s' % (r['mode'], r['family'], r['source'], r['case'], r['mask'], c['name'], c['verdict'], c['hp'][:160], c['note'][:120]))
