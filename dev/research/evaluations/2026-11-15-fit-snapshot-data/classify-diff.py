"""Classify the differences between the 2026-11-14 and 2026-11-15 baselines.

Usage: python3 classify-diff.py native double routes-native routes-double
Per snapshot: outcome-kind changes by family, estimate changes of ok fits, and
log-likelihood changes above 1e-12 relative (largest first)."""
import sys, json, collections, math
sys.path.insert(0, str(__import__('pathlib').Path(__file__).resolve().parents[4]/'dev/validation'))
from strict_common import load, number
R = __import__('pathlib').Path(__file__).resolve().parents[4]
B = str(R/'dev/research/evaluations/2026-11-14-fit-snapshot-data')+'/'
S = str(R/'dev/research/evaluations/2026-11-15-fit-snapshot-data')+'/'
def rel(a, b):
    a, b = number(a), number(b)
    if a == b: return 0.0
    if not (math.isfinite(a) and math.isfinite(b)): return math.inf
    return abs(a-b)/max(1, abs(a))
for snap in sys.argv[1:]:
    old, new = load(B+snap+'.jsonl.gz'), load(S+snap+'.jsonl.gz')
    print('==', snap, len(old), len(new), 'only old', len(set(old)-set(new)), 'only new', len(set(new)-set(old)))
    combos = collections.Counter(); big = []; kinds = collections.Counter(); est = []
    for k in sorted(set(old) & set(new)):
        a, b = old[k]['result'], new[k]['result']
        fields = tuple(sorted(f for f in set(a) | set(b) if a.get(f) != b.get(f)))
        if not fields: continue
        combos[(k[0], fields)] += 1
        if a['kind'] != b['kind'] or (a.get('outcome') or {}).get('kind') != (b.get('outcome') or {}).get('kind'):
            kinds[(k[0], a['kind'], b['kind'], (a.get('outcome') or {}).get('kind'), (b.get('outcome') or {}).get('kind'))] += 1
        if 'estimates' in fields and a['kind'] == 'ok' and b['kind'] == 'ok':
            est.append((k, a['estimates'], b['estimates']))
        if 'logLik' in fields and a['kind'] == 'ok' == b['kind']:
            d = rel(a['logLik'], b['logLik'])
            if d > 1e-12: big.append((d, k, number(a['logLik']), number(b['logLik']), 'estimates' in fields))
    print('-- kind changes (family, kind old->new, outcome old->new): count')
    for x, c in sorted(kinds.items()): print('  ', x, c)
    print('-- ok->ok estimate changes:', len(est))
    for e in est[:10]: print('  ', e)
    print('-- logLik changes > 1e-12 relative (ok->ok):', len(big))
    for d, k, a, b, e in sorted(big, reverse=True)[:25]: print('   %.3g %s %r -> %r est_changed=%s' % (d, '|'.join(k), a, b, e))
    fam = collections.Counter(k[1] for d, k, a, b, e in big)
    print('   by family:', dict(fam.most_common()))
