#!/usr/bin/env python3
"""Markdown tables for the report from sweep.jsonl and whatif.jsonl (native mode; the double build is checked in summary.txt)."""
import json, collections, sys
e = 2.0**-52
s = [json.loads(l) for l in open('sweep.jsonl')]
w = [json.loads(l) for l in open('whatif.jsonl')]
def row(name, rs, fields):
    return '| %s | %d | %d | ' % (name, len(rs), sum(r['fails_claim'] for r in rs)) + ' | '.join(fields(rs)) + ' |'
print('### Terms by path (native snapshot, all cases of the path; offsets and terms relative, in units of eps = 2^-52 unless noted)\n')
print('| path | cases | claim fails | max offset | max half width | max conditioning (actual / a priori) | max data (a priori / actual) | max conversion (actual / a priori) | max W actual | max W a priori | max W a priori / 1e-10 |')
print('|---|---|---|---|---|---|---|---|---|---|---|')
g = collections.defaultdict(list)
for r in s:
    if r['mode'] == 'native' and r['match']: g[(r['path'], r['family'] if r['path'] == 'location_scale_held' else '')].append(r)
for (path, fam), rs in sorted(g.items()):
    f = lambda k: max((r.get(k) or 0) for r in rs)
    print('| %s %s | %d | %d | %.1f eps | %.1f eps | %.1f / %.1f eps | %.1f / %.1f eps | %.2f / %.2f eps | %.1f eps | %.1f eps | %.1e |' % (
        path, fam, len(rs), sum(r['fails_claim'] for r in rs), max(abs(r['offset']) for r in rs)/e, f('half')/e, f('cond_actual')/e, f('cond_bound')/e,
        f('data_actual')/e, f('data_bound')/e, f('conv_actual')/e, f('conv_bound')/e, f('W_actual')/e, f('W_bound')/e, f('W_bound')/1e-10))
print('\n### What-if variants (not in the C++)\n')
print('| variant | family | cases | max offset | max half | max W actual | max W a priori | max W a priori / 1e-10 |')
print('|---|---|---|---|---|---|---|---|')
gw = collections.defaultdict(list)
for r in w: gw[(r['variant'], r['family'])].append(r)
for (v, f_), rs in sorted(gw.items()):
    print('| %s | %s | %d | %.1f eps | %.1f eps | %.1f eps | %.1f eps | %.1e |' % (v, f_, len(rs), max(abs(r['offset']) for r in rs)/e, max(r['half'] for r in rs)/e,
          max(r['W_actual'] for r in rs)/e, max(r['W_bound'] for r in rs)/e, max(r['W_bound'] for r in rs)/1e-10))
