#!/usr/bin/env python3
"""Per-family histogram of |reported - summed| / max(1, |reported|) by decade, from fit-density.py --records.

Usage: density-histogram.py RECORDS.jsonl OUT.json
Decade "1e-k" counts values in [1e-k, 1e-(k-1)); "0" counts exact agreement. Failing records are kept whole.
"""
import collections, json, math, sys
hist = collections.defaultdict(lambda: collections.defaultdict(collections.Counter)); fails = []
for text in open(sys.argv[1]):
    r = json.loads(text)
    if r['bounds'] is not None: key = 'candidate in bounds' if r['pass'] else 'candidate outside bounds'
    elif r['relative'] == 0: key = '0'
    elif not math.isfinite(r['relative']): key = 'non-finite'
    else: key = '1e%d' % math.floor(math.log10(r['relative']))
    hist[r['mode']][r['family']][key] += 1
    if not r['pass']: fails.append(r)
json.dump(dict(histogram={m: {f: dict(sorted(c.items())) for f, c in sorted(v.items())} for m, v in hist.items()},
               failures=fails), open(sys.argv[2], 'w'), indent=1)
total = collections.Counter()
for m in hist:
    for c in hist[m].values(): total.update({(m, k): n for k, n in c.items()})
for m in sorted(hist): print(m, {k: n for (mm, k), n in sorted(total.items(), key=lambda kv: kv[0][1]) if mm == m})
