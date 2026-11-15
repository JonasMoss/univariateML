#!/usr/bin/env python3
"""Negative controls for the mechanism-G replays (dev/validation/cert_replay_integer.py).

Takes stored snapshot records (the post-5b native baselines of master in 2026-11-15-fit-snapshot-data), changes a
returned integer, a returned continuous coefficient, the uniqueness flag or the data so that the claim becomes
false (or, for two controls, builds an input that has an exact tie), and replays it through cert-replay.py's
REPLAYS. Every control must end in the named verdict for the named comparison; the script exits nonzero otherwise.
A control with expected verdict 'fails' is a claim the replay must reject; one with 'open' is a flagged tie with no
exact tie, which the replay reports and does not judge.

  python3 negative-controls.py   (one slot: /tmp/umvl-cpu/run nice -n 10)
"""
import copy, gzip, importlib.util, json, sys
from fractions import Fraction
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
spec = importlib.util.spec_from_file_location('cert_replay', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
cr.iv.dps = 50; cr.mp.mp.dps = 50
SNAP = ROOT/'dev/research/evaluations/2026-11-15-fit-snapshot-data'

rows = cr.load('%s,%s' % (SNAP/'native.jsonl.gz', SNAP/'routes-native.jsonl.gz'))
archive = {}
for c in json.loads(gzip.decompress(cr.fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
inputs = {}

def data(r):
    family = r['family']
    if family not in inputs: inputs[family] = cr.rebuild(family, archive)
    return inputs[family][(r['source'], r['case'])][0]['data']

def num(r, name): return cr.number(r['result']['estimates'][name])

def find(family, test):
    for key, r in sorted(rows.items()):
        o = r['result'].get('outcome') or {}
        if key[0] == family and o.get('kind') == 'estimate' and o.get('certified') and test(r, data(r)): return r
    raise SystemExit('no %s case for a control' % family)

def with_estimates(record, **changes):
    record = copy.deepcopy(record)
    for k, v in changes.items(): record['result']['estimates'][k] = float(v).hex()
    return record

def with_unique(r, unique):
    r = copy.deepcopy(r); r['result']['outcome']['unique'] = unique; return r

def distinct(x): return len(set(x))

controls = []   # (label, record, data or None, comparison name, expected verdict)
def add(label, r, name, expected='fails', x=None): controls.append((label, r, x if x is not None else data(r), name if isinstance(name, tuple) else (name,), expected))

# ---- binom
bf = find('binom', lambda r, x: r['mask'] == 0 and distinct(x) > 1 and num(r, 'size') > max(x) and num(r, 'size') < 100)
k = num(bf, 'size')
add('binom free size: size + 1', with_estimates(bf, size=k+1, prob=sum(data(bf))/len(data(bf))/(k+1)), 'search')
add('binom free size: size - 1 (below the first nonpositive increment)', with_estimates(bf, size=k-1, prob=sum(data(bf))/len(data(bf))/(k-1)), 'search')
bf3 = find('binom', lambda r, x: r['mask'] == 0 and distinct(x) > 1 and num(r, 'size') >= max(x)+3 and num(r, 'size') < 100)
k3 = num(bf3, 'size')
add('binom free size: size - 3, tail (D(k-3+1) > 0)', with_estimates(bf3, size=k3-3, prob=sum(data(bf3))/len(data(bf3))/(k3-3)), 'tail')
add('binom free size: prob times 1 + 1e-6', with_estimates(bf, prob=num(bf, 'prob')*(1+1e-6)), 'coef')
bh = find('binom', lambda r, x: r['mask'] == 2 and num(r, 'size') > max(x) and sum(x) > 0)
add('binom prob held: size + 1', with_estimates(bh, size=num(bh, 'size')+1), 'search')
add('binom prob held: size - 1', with_estimates(bh, size=num(bh, 'size')-1), 'tail' if False else 'search')
x_tie = [1]*5   # all counts one, p = 1/2: equal maxima at sizes 1 and 2 (README)
r_tie = copy.deepcopy(bh); r_tie['held'] = {'prob': 0.5}
r_tie = with_estimates(r_tie, size=1, prob=0.5)
add('binom prob held: counts all one, p = 1/2, sizes 1 and 2 tie, claimed unique', r_tie, 'tie', x=x_tie)
add('binom data perturbed (the smallest count set to 0, maximum unchanged)', bf, 'search', x=[0]+sorted(data(bf))[1:])

# ---- hyper
def h_find(mask, extra=lambda r, x: True): return find('hyper', lambda r, x: r['mask'] == mask and extra(r, x) and distinct(x) > 1)
h6 = h_find(6); h3 = h_find(3); h5 = h_find(5, lambda r, x: sum(x) > 0); h2 = h_find(2); h1 = h_find(1); h4 = h_find(4)
add('hyper m free: m + 1', with_estimates(h6, m=num(h6, 'm')+1), 'search')
add('hyper k free: k + 1', with_estimates(h3, k=num(h3, 'k')+1), 'search')
add('hyper N free: N + 1', with_estimates(h5, N=num(h5, 'N')+1), 'search')
x5 = data(h5); u0 = int(num(h5, 'k')*(num(h5, 'm')+num(h5, 'k'))*len(x5))//int(sum(x5))+1
add('hyper N free: N beyond the cutoff k (m+k)/mean', with_estimates(h5, N=u0+5), 'tail')
add('hyper N held: m + 1', with_estimates(h2, m=num(h2, 'm')+1), 'search')
add('hyper N held: m and k both raised by 3', with_estimates(h2, m=num(h2, 'm')+3, k=num(h2, 'k')+3), 'search')
add('hyper m held: N + 1', with_estimates(h1, N=num(h1, 'N')+1), 'search')
add('hyper k held: N + 1', with_estimates(h4, N=num(h4, 'N')+1), ('search', 'tail'))
add('hyper m held: N below the binomial face (N = lo, free k = max x)', with_estimates(h1, N=int(num(h1, 'm'))+max(data(h1))-min(data(h1)), k=max(data(h1))), 'tail')
hc = find('hyper', lambda r, x: r['mask'] == 0 and distinct(x) == 1)
add('hyper all free, constant sample: N + 1', with_estimates(hc, N=num(hc, 'N')+1), 'search')
# exact tie of adjacent ratios for m free: search small (N, k, sample) for ratio(t*+1) = 1
def find_hyper_tie():
    import itertools
    for N in range(6, 14):
        for k in range(2, N):
            for xs in itertools.combinations_with_replacement(range(0, k+1), 3):
                counts = cr.cert_replay_integer.collections.Counter(xs); ci = cr.cert_replay_integer
                lo, hi = max(xs), N-k+min(xs)
                if lo >= hi: continue
                f = ci.hyper_m_factors(N, k, counts)
                signs = [ci.ratio_sign(f(t)) for t in range(lo+1, hi+1)]
                if 0 in signs and signs.index(0) > 0 and all(s > 0 for s in signs[:signs.index(0)]):
                    return N, k, list(xs), lo+1+signs.index(0)-1
    raise SystemExit('no exact tie found')
Nt, kt, xt, mt = find_hyper_tie()
r_ht = copy.deepcopy(h6); r_ht['held'] = {'N': Nt, 'k': kt}
r_ht = with_estimates(r_ht, N=Nt, k=kt, m=mt)
add('hyper m free: constructed exact tie (N=%d, k=%d, x=%s, m=%d and %d), claimed unique' % (Nt, kt, xt, mt, mt+1), r_ht, 'tie', x=xt)
add('hyper m free: a tolerance-flagged tie with no exact tie (unique = false on a strict maximum)', with_unique(h6, False), 'tie', expected='open')

# ---- nhyper
def n_find(mask, extra=lambda r, x: True): return find('nhyper', lambda r, x: r['mask'] == mask and extra(r, x) and distinct(x) > 1)
n6 = n_find(6); n3 = n_find(3); n5 = n_find(5); n1 = n_find(1); n2 = n_find(2); n4 = n_find(4); n0 = n_find(0)
add('nhyper m free: m + 1', with_estimates(n6, m=num(n6, 'm')+1), 'search')
add('nhyper r free: r + 1', with_estimates(n3, r=num(n3, 'r')+1), 'search')
add('nhyper N free (m, r held): N + 2', with_estimates(n5, N=num(n5, 'N')+2), 'search')
add('nhyper m held (N, r free): N + 2', with_estimates(n1, N=num(n1, 'N')+2), 'search')
add('nhyper N held: m + 1', with_estimates(n2, m=num(n2, 'm')+1), 'search')
add('nhyper r held (N, m free): N + 1', with_estimates(n4, N=num(n4, 'N')+1), ('search', 'tail'))
add('nhyper all free: m + 1', with_estimates(n0, m=num(n0, 'm')+1), ('search', 'tail'))
nc = find('nhyper', lambda r, x: r['mask'] == 0 and distinct(x) == 1)
add('nhyper all free, constant sample: N + 1, m, r unchanged', with_estimates(nc, N=num(nc, 'N')+1), 'search')

# ---- bbinom
b6 = find('bbinom', lambda r, x: r['mask'] == 6 and distinct(x) > 1 and num(r, 'size') < 30)
b4 = find('bbinom', lambda r, x: r['mask'] == 4 and distinct(x) > 1 and num(r, 'size') < 12)
b2 = find('bbinom', lambda r, x: r['mask'] == 2 and distinct(x) > 1 and num(r, 'size') < 12)
add('bbinom shapes held: size + 1', with_estimates(b6, size=num(b6, 'size')+1), 'search')
add('bbinom shapes held: size - 1', with_estimates(b6, size=num(b6, 'size')-1), 'search')
add('bbinom beta held: size + 2', with_estimates(b4, size=num(b4, 'size')+2), 'search')
add('bbinom alpha held: size + 1', with_estimates(b2, size=num(b2, 'size')+1), ('search', 'tail'))

# ---- cmb
c6 = find('cmb', lambda r, x: r['mask'] == 6 and distinct(x) > 1)
c4 = find('cmb', lambda r, x: r['mask'] == 4 and distinct(x) > 1 and num(r, 'size') < 12)
c2 = find('cmb', lambda r, x: r['mask'] == 2 and distinct(x) > 1)
c0 = find('cmb', lambda r, x: r['mask'] == 0 and distinct(x) > 1)
add('cmb prob and nu held: size + 2', with_estimates(c6, size=num(c6, 'size')+2), 'search')
add('cmb prob and nu held: size - 2', with_estimates(c6, size=max(max(data(c6)), num(c6, 'size')-2)), 'search')
add('cmb nu held: size + 3', with_estimates(c4, size=num(c4, 'size')+3), 'search')
add('cmb prob held: size + 3', with_estimates(c2, size=num(c2, 'size')+3), 'search')
add('cmb all free: size + 3', with_estimates(c0, size=num(c0, 'size')+3), 'search')

# ---- triang
t3 = find('triang', lambda r, x: r['mask'] == 3 and distinct(x) > 3)
t0 = find('triang', lambda r, x: r['mask'] == 0 and distinct(x) > 3)
def other_mode(r):
    xs = sorted(set(data(r))); c = num(r, 'c')
    return xs[0] if c != xs[0] else xs[-1]
add('triang both endpoints held: another observation as the mode', with_estimates(t3, c=other_mode(t3)), 'search')
add('triang all free: another observation as the mode', with_estimates(t0, c=other_mode(t0)), 'search')

failed = 0
for label, r, x, name, expected in controls:
    comps, why = cr.REPLAYS[r['family']](r, x, 4000)
    verdicts = {n: [c['verdict'] for c in comps if c['name'] == n] for n in name}
    ok = any(v == [expected] for v in verdicts.values())
    failed += not ok
    print('%-4s %-100s %s: %s (expected %s)%s' % ('ok' if ok else 'BAD', label[:100], '/'.join(name), verdicts if any(verdicts.values()) else 'not judged: %s' % why, expected,
                                                  '' if ok else '  hp=%s' % [c['hp'][:100] for c in comps if c['name'] in name]))
print('%d controls, %d as intended, %d not' % (len(controls), len(controls)-failed, failed))
sys.exit(1 if failed else 0)
