#!/usr/bin/env python3
"""Counts per family and mode from records.jsonl(.gz): cases judged, comparisons by name, verdict and evidence level."""
import collections, gzip, json, sys
from pathlib import Path
path = Path(sys.argv[1] if len(sys.argv) > 1 else Path(__file__).with_name('records.jsonl.gz'))
opener = gzip.open if path.suffix == '.gz' else open
cases = collections.Counter(); comps = collections.Counter(); seconds = collections.Counter()
for line in opener(path, 'rt'):
    r = json.loads(line)
    cases[(r['family'], r['mode'])] += 1
    seconds[r['family']] += r['seconds']
    for c in r['comparisons']: comps[(r['family'], r['mode'], c['name'], c['verdict'], c['rigor'])] += 1
for family in sorted({k[0] for k in cases}):
    print('%s: %s' % (family, ', '.join('%s %d cases' % (m, cases[(family, m)]) for m in ('native', 'double'))))
    for (f, m, name, verdict, rigor), n in sorted(comps.items()):
        if f == family: print('    %-6s %-12s %-6s %-9s %d' % (m, name, verdict, rigor, n))
print('total cases %d, comparisons %d' % (sum(cases.values()), sum(comps.values())))
