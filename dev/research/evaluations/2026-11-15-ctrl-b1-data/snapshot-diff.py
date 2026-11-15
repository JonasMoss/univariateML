"""Diff new snapshot dirs against the 2026-11-15 baselines, for the families of job 2a.
Usage (repository root): python3 snapshot-diff.py native|double build/snapshots/<mode>
Prints per family (a location is measured against max(|location|, scale)): cases, changed, kind changes (listed), max relative change per coefficient (ok->ok), max logLik change."""
import sys, collections, math
sys.path.insert(0, 'dev/validation')
from strict_common import load, number
mode, new_path = sys.argv[1], sys.argv[2]
FAM = {'cauchy','gamma','invgamma','lgamma','llogis','logis','naka'}
old = {k: v for k, v in load('dev/research/evaluations/2026-11-15-fit-snapshot-data/%s.jsonl.gz' % mode).items() if k[0] in FAM}
new = load(new_path)
print('==', mode, len(old), len(new), 'only old', len(set(old)-set(new)), 'only new', len(set(new)-set(old)))
stat = collections.defaultdict(lambda: collections.Counter()); mx = collections.defaultdict(float); mll = collections.defaultdict(float)
kinds = []; worst = {}
for k in sorted(set(old) & set(new)):
    a, b = old[k]['result'], new[k]['result']; f = k[0]
    stat[f]['cases'] += 1
    if a == b: continue
    stat[f]['changed'] += 1
    ka, kb = (a['kind'], a['outcome']['kind'], a.get('certified')), (b['kind'], b['outcome']['kind'], b.get('certified'))
    if ka != kb:
        kinds.append((k, ka, kb, b.get('message'))); stat[f]['kindchg'] += 1
    if a['kind'] == 'ok' == b['kind']:
        for c in a['estimates']:
            x, y = number(a['estimates'][c]), number(b['estimates'][c])
            den = max(abs(x), number(a['estimates']['scale'])) if c == 'location' else abs(x)
            d = abs(x-y)/max(den, 1e-300) if x != y else 0
            if d > mx.get((f, c), -1): mx[(f, c)] = d; worst[(f, c)] = k[2]
        l = abs(number(a['logLik'])-number(b['logLik']))/max(1, abs(number(a['logLik'])))
        mll[f] = max(mll[f], l)
for f in sorted(FAM): print(f, dict(stat[f]), 'max logLik rel', '%.2g' % mll[f])
for (f, c), v in sorted(mx.items()): print('  max rel change', f, c, '%.3g' % v, 'case', worst[(f, c)])
print('kind changes:', len(kinds))
for x in kinds: print('  ', x)
