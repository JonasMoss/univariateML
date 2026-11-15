#!/usr/bin/env python3
"""Negative controls for the mechanism-D replays (dev/validation/cert_replay_faces.py).

Takes stored snapshot records (post-5b native baselines of master, ordinary
and route snapshots, validate worktree build/snapshots), changes the claimed outcome, its uniqueness flag or the
returned coefficients so that the claim becomes false, and replays them
through cert-replay.py's REPLAYS. Every control must end in 'fails' for the
named comparison; the script exits nonzero otherwise.

  python3 negative-controls.py   (one slot: /tmp/umvl-cpu/run)
"""
import copy, gzip, importlib.util, json, sys
from fractions import Fraction
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
spec = importlib.util.spec_from_file_location('cert_replay', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
cr.iv.dps = 50; cr.mp.mp.dps = 50
SNAP = Path('/home/jonas/Files/programming/univariateML.wt/validate/build/snapshots')

rows = cr.load('%s,%s' % (SNAP/'native', SNAP/'routes-native'))
archive = {}
for c in json.loads(gzip.decompress(cr.fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
inputs = {}

def data(r):
    family = r['family']
    if family not in inputs: inputs[family] = cr.rebuild(family, archive)
    return inputs[family][(r['source'], r['case'])][0]['data']

def find(family, test):
    for key, r in sorted(rows.items()):
        if key[0] == family and (r['result'].get('outcome') or {}).get('kind') and test(r, data(r)): return r
    raise SystemExit('no %s case for a control' % family)

def kind_is(*kinds, mask=0):
    return lambda r, x: r['mask'] == mask and r['result']['outcome']['kind'] in kinds

def claim(r, kind, estimates=None, model=None, limit=None, unique=None, reason=None, certified=True):
    r = copy.deepcopy(r); o = r['result']['outcome']
    o['kind'] = kind; o['certified'] = certified
    o['reason'] = reason or 'none'
    if unique is not None: o['unique'] = unique
    if estimates is not None: r['result']['estimates'] = {k: float(v).hex() for k, v in estimates.items()}
    if model is not None: r['result']['model'] = model
    if limit is not None: r['result']['limit_estimates'] = {k: float(v).hex() for k, v in limit.items()}
    return r

def scaled(r, name, factor):
    r = copy.deepcopy(r); v = cr.number(r['result']['estimates'][name])
    r['result']['estimates'][name] = float(v*factor).hex(); return r

def h0_positive(r, x):
    e = r['result']['estimates']; fx = [Fraction(v) for v in x]; n = len(fx)
    return (r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and cr.number(e['mu']) > 0 and
            1-n*sum(v**4 for v in fx)/(3*sum(v*v for v in fx)**2) > 0)

controls = []
b_fin = find('binom', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and len(set(x)) > 1)
b_lim = find('binom', kind_is('limit'))
b_const = find('binom', lambda r, x: r['mask'] == 0 and len(set(x)) == 1 and x[0] > 0)
controls += [('binom finite sample claimed Poisson limit', claim(b_fin, 'limit', {}, 'pois'), 'moment'),
             ('binom Poisson-limit sample claimed finite', claim(b_lim, 'estimate', {'size': 50, 'prob': 0.1}, ''), 'moment'),
             ('binom constant sample with size + 1', scaled(b_const, 'size', 1+1/cr.number(b_const['result']['estimates']['size'])), 'support')]
g_fin = find('gompertz', kind_is('estimate')); g_lim = find('gompertz', kind_is('limit'))
g_held = find('gompertz', kind_is('limit', mask=1))
controls += [('gompertz finite sample claimed exponential limit', claim(g_fin, 'limit', {}, 'exp'), 'moment'),
             ('gompertz exponential-limit sample claimed finite', claim(g_lim, 'estimate', {'a': 1, 'b': 1}, ''), 'moment'),
             ('gompertz a held: limit sample claimed finite', claim(g_held, 'estimate', {'a': 1, 'b': 1}, ''), 'moment')]
p_lim = find('pig', kind_is('limit')); p_fin = find('pig', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate' and sum(x) > 0)
controls += [('pig Poisson-limit sample claimed finite', claim(p_lim, 'estimate', {'mean': 1, 'shape': 1}, ''), 'moment'),
             ('pig mean held: finite sample claimed Poisson limit', claim(p_fin, 'limit', {}, 'pois'), 'moment')]
f_pos = find('foldnorm', h0_positive)
f_sig = find('foldnorm', lambda r, x: r['mask'] == 2 and cr.number(r['result']['estimates']['mu']) > 0)
controls += [('foldnorm mu > 0 with H(0) > 0 claimed mu = 0', claim(f_pos, 'estimate', {'mu': 0, 'sigma': 1}), 'quartic'),
             ('foldnorm sigma held: positive location claimed zero', claim(f_sig, 'estimate', {'mu': 0, 'sigma': f_sig['held']['sigma']}), 'location')]
# No baseline case has size 2 (archive and classes use size 10): a size-2 record with
# constructed counts, n0, n1, n2 = 2, 1, 3, so D = 4*2*3 - 1 = 23 > 0, alpha = 7/23, beta = 5/23.
bb = find('bbinom', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate')
bb2 = copy.deepcopy(bb); bb2['held'] = {'size': 2.0}; bb2_data = [0, 0, 1, 2, 2, 2]
exact2 = claim(bb2, 'estimate', {'size': 2, 'alpha': 7/23, 'beta': 5/23})
controls += [('bbinom size 2: finite sample claimed binomial face', (claim(bb2, 'limit', {}, 'binom', {'size': 2, 'prob': 0.5}), bb2_data), 'size2'),
             ('bbinom size 2: alpha x (1 + 1e-8)', (scaled(exact2, 'alpha', 1+1e-8), bb2_data), 'closed-form')]
c_fin = find('cauchy', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and len(set(x)) > 2)
c_none = find('cauchy', lambda r, x: r['mask'] == 0 and r['source'] == 'class' and r['case'] == 'ties')
controls += [('cauchy finite sample claimed unbounded', claim(c_fin, 'nonexistence', {}, reason='unbounded'), 'multiplicity'),
             ('cauchy 2k > n sample claimed unattained', claim(c_none, 'nonexistence', {}, reason='unattained'), 'multiplicity')]
h_fin = find('hcauchy', kind_is('estimate'))
s_none = find('slash', kind_is('nonexistence'))
controls += [('hcauchy finite sample claimed unattained', claim(h_fin, 'nonexistence', {}, reason='unattained'), 'multiplicity'),
             ('slash unbounded sample claimed finite', claim(s_none, 'estimate', {'mu': 0, 'sigma': 1}), 'multiplicity')]
sg = find('sgomp', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate')
sg_eta = cr.number(sg['result']['estimates']['eta'])
controls += [('sgomp b held: eta flipped between 0 and positive',
              claim(sg, 'estimate', {'b': sg['held']['b'], 'eta': 0.5 if sg_eta == 0 else 0.0}), 'face-score')]
dn2 = find('dnorm', lambda r, x: r['mask'] == 0 and (r['result'].get('model') or '').startswith('unavailable:two-point'))
dnf = find('dnorm', kind_is('estimate'))
dl = find('dlaplace', kind_is('estimate'))
controls += [('dnorm two-point sample claimed finite', claim(dn2, 'estimate', {'mean': 0, 'sd': 1}), 'support'),
             ('dnorm finite sample claimed point mass', claim(dnf, 'limit', {}, 'unavailable:point-mass', {'point': 0}), 'support'),
             ('dlaplace uniqueness flag flipped', claim(dl, 'estimate', unique=not dl['result']['outcome']['unique']), 'unique')]
zp = find('pois+zero=inflated', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and sum(x) > 0)
zp_pi = cr.number(zp['result']['estimates']['pi'])
controls += [('zip pi flipped between 0 and positive', claim(zp, 'estimate', {'lambda': 1, 'pi': 0.25 if zp_pi == 0 else 0.0}), 'zero-fraction')]
tn1 = find('norm +truncate=tnorm', lambda r, x: r['mask'] == 8 and r['result']['outcome']['kind'] == 'estimate')
tn2 = find('norm +truncate=tnorm', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'limit')
tnu = find('norm +truncate=tnorm', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'limit')
controls += [('tnorm one-sided: finite sample claimed exponential face', claim(tn1, 'limit', {}, 'unavailable:translated-exponential'), 'window'),
             ('tnorm two-sided: face sample claimed finite', claim(tn2, 'estimate', {'mean': 0, 'sd': 1}), 'window'),
             ('tnorm mean held: uniform-face sample claimed finite', claim(tnu, 'estimate', {'mean': 0, 'sd': 1}), 'window')]
# Uniqueness controls: the flag on a certified estimate is flipped against the README statement.
def flipped(family, test):
    r = find(family, test); return claim(r, 'estimate', unique=not r['result']['outcome']['unique'])
controls += [('binom all-zero counts claimed unique', flipped('binom', lambda r, x: r['mask'] == 0 and len(x) and max(x) == 0 and r['result']['outcome']['kind'] == 'estimate' and not r['result']['outcome']['unique']), 'unique'),
             ('cauchy balanced two-point claimed unique', flipped('cauchy', lambda r, x: r['mask'] == 0 and len(set(x)) == 2 and r['result']['outcome']['kind'] == 'estimate' and not r['result']['outcome']['unique']), 'unique'),
             ('zip all-zero claimed unique', flipped('pois+zero=inflated', lambda r, x: r['mask'] == 0 and len(x) and max(x) == 0 and r['result']['outcome']['kind'] == 'estimate' and not r['result']['outcome']['unique']), 'unique'),
             ('pig all-zero both free claimed unique (as master returns it)', find('pig', lambda r, x: r['mask'] == 0 and len(x) and max(x) == 0 and r['result']['outcome']['kind'] == 'estimate'), 'unique'),
             ('cmp all-zero both free claimed unique (as master returns it)', find('cmp', lambda r, x: r['mask'] == 0 and len(x) and max(x) == 0 and r['result']['outcome']['kind'] == 'estimate'), 'unique')]
# bbinom: constructed all-zero counts with free size (no such baseline case): a certified unique estimate must fail.
bb_free = copy.deepcopy(bb); bb_free['mask'] = 2; bb_free['held'] = {'alpha': 1.0}
controls += [('bbinom size free, all-zero counts claimed unique', (claim(bb_free, 'estimate', {'size': 0, 'alpha': 1, 'beta': 1}, unique=True), [0, 0, 0, 0]), 'unique'),
             ('bbinom size 0 held claimed unique', (claim(dict(bb, held={'size': 0.0}), 'estimate', {'size': 0, 'alpha': 1, 'beta': 1}, unique=True), [0, 1, 2, 3]), 'unique')]
# tnorm: constant sample with an estimated bound and held mean/sd (core: estimated_bounds_collapse), claimed finite.
tn_c = copy.deepcopy(tn1); tn_c['mask'] = 1 | 4; tn_c['held'] = {'mean': 0.3, 'a': -1.0}
controls += [('tnorm constant sample, held mean + a, b estimated, claimed finite', (claim(tn_c, 'estimate', {'mean': 0.3, 'sd': 1}), [-1.0, -1.0, -1.0]), 'support')]
# Support controls for the remaining discrete families: a finite sample claimed as a point mass.
for family, mask in (('dweibull', 0), ('dgamma', 0), ('cmp', 0), ('cmb', 0), ('lgser', 0), ('zeta', 0), ('yulesimon', 0)):
    r0 = find(family, lambda r, x, family=family: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and len(x) and max(x) >= 2 and len(set(x)) > 2)
    controls += [('%s finite sample claimed point mass' % family, claim(r0, 'limit', {}, 'unavailable:point-mass', {'point': 1}), 'support')]
zf = find('zeta +truncate=zipf', kind_is('limit'))
controls += [('zipf point mass claimed at the other end', claim(zf, 'limit', {}, 'unavailable:point-mass', {'point': 7}), 'support')]

sanity = cr.REPLAYS['bbinom'](exact2, bb2_data, 4000)[0]
print('sanity: exact size-2 closed form replays as', {k['name']: k['verdict'] for k in sanity})
bad = any(k['verdict'] != 'holds' for k in sanity)
for label, r, expect in controls:
    r, x = r if isinstance(r, tuple) else (r, None)
    comps, why = cr.REPLAYS[r['family']](r, data(r) if x is None else x, 4000)
    got = {k['name']: k['verdict'] for k in comps}
    ok = got.get(expect) == 'fails'
    bad += not ok
    print('%-4s %-58s %-20s %s/%s: %s  %s' % ('ok' if ok else 'BAD', label, r['family'], r['source'], r['case'], expect, got))
print('%d controls, %d did not fail as intended' % (len(controls), bad))
sys.exit(1 if bad else 0)
