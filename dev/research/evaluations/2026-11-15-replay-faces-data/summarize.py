#!/usr/bin/env python3
"""Tables for the replay-faces report from records.jsonl.gz (cert-replay.py --records).

  python3 summarize.py [RECORDS]

Prints verdict counts by family, comparison and mode; every comparison that is
not 'holds'; the closest cases to the boundary for the binom and gompertz
moment comparisons (relative gap read from the record); and every record
whose binary64 mirror disagrees with exact arithmetic.
"""
import collections, gzip, json, re, sys
from pathlib import Path

path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).with_name('records.jsonl.gz')
opener = gzip.open if path.suffix == '.gz' else open
rows = [json.loads(line) for line in opener(path, 'rt')]

counts = collections.Counter()
for r in rows:
    for c in r['comparisons']: counts[(r['family'], c['name'], c['rigor'], c['verdict'], r['mode'])] += 1
families = sorted({k[:4] for k in counts})
print('family | comparison | level | verdict | native | double')
for key in families:
    print('%s | %s | %s | %s | %d | %d' % (*key, counts.get(key+('native',), 0), counts.get(key+('double',), 0)))
print('\ncases judged: %s' % {m: sum(1 for r in rows if r['mode'] == m) for m in ('native', 'double')})

print('\nnot holding:')
for r in rows:
    for c in r['comparisons']:
        if c['verdict'] != 'holds':
            print('  %s %s %s/%s mask %d: %s %s | claim %s | hp %s | double %s | %s' % (
                r['mode'], r['family'], r['source'], r['case'], r['mask'], c['name'], c['verdict'], c['claim'], c['hp'], c['double'], c['note']))

def relative(c):
    m = re.search(r'relative gap (?:\|mean - var\|/max = )?([0-9.e+-]+)', (c['hp'] or '')+' '+(c['note'] or ''))
    return float(m.group(1)) if m else None

print('\nclosest to the boundary (moment comparisons, native records):')
for family in ('binom', 'gompertz', 'pig', 'foldnorm'):
    gaps = sorted((relative(c), r['source'], r['case'], r['mask'], c['verdict'], c['claim']) for r in rows
                  if r['family'] == family and r['mode'] == 'native' for c in r['comparisons']
                  if c['name'] in ('moment', 'location') and relative(c) is not None)
    for g in gaps[:4]: print('  %s %s/%s mask %d: relative gap %.3g, %s, %s' % (family, g[1], g[2], g[3], g[0], g[4], g[5]))

print('\nmirror disagreements with exact arithmetic:')
for r in rows:
    for c in r['comparisons']:
        text = '%s %s' % (c['double'] or '', c['note'] or '')
        if 'DISAGREES' in text or 'scaled multiplicity' in text:
            print('  %s %s %s/%s: %s: %s' % (r['mode'], r['family'], r['source'], r['case'], c['name'], text))
