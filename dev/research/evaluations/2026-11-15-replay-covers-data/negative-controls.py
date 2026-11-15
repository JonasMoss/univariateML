#!/usr/bin/env python3
"""Negative controls for the mechanism-E/F replays (dev/validation/cert_replay_covers.py).

Takes stored snapshot records (the 2026-11-15 baselines), changes the returned
coefficients or the data so that the replayed inequality becomes false, and
replays them through cert-replay.py's REPLAYS. Every control must end in
'fails' for the named comparison; the script exits nonzero otherwise.

  OPENBLAS_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 negative-controls.py

The replay cache is disabled for the controls (CERT_REPLAY_COVERS_CACHE=/dev/null/none).
"""
import copy, gzip, importlib.util, json, os, sys
from pathlib import Path

os.environ['CERT_REPLAY_COVERS_CACHE'] = '/tmp/cert-replay-covers-controls-cache.jsonl.gz'
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
spec = importlib.util.spec_from_file_location('cert_replay', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
cr.iv.dps = 50; cr.mp.mp.dps = 50
if Path('/tmp/cert-replay-covers-controls-cache.jsonl.gz').exists(): Path('/tmp/cert-replay-covers-controls-cache.jsonl.gz').unlink()
SNAP = ROOT/'dev/research/evaluations/2026-11-15-fit-snapshot-data'
rows = cr.load('%s,%s' % (SNAP/'native.jsonl.gz', SNAP/'routes-native.jsonl.gz'))
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

def kind_is(kind, mask=0, **extra):
    return lambda r, x: r['mask'] == mask and r['result']['outcome']['kind'] == kind and all(r['result']['estimates'].get(k) is None or True for k in extra)

def num(r, name): return cr.number(r['result']['estimates'][name])

def set_est(r, **values):
    r = copy.deepcopy(r)
    for k, v in values.items(): r['result']['estimates'][k] = float(v).hex()
    return r

def scaled(r, name, factor): return set_est(r, **{name: num(r, name)*factor})

def claim(r, kind, estimates=None, certified=True, unique=None):
    r = copy.deepcopy(r); o = r['result']['outcome']; o['kind'] = kind; o['certified'] = certified
    if unique is not None: o['unique'] = unique
    if estimates is not None: r['result']['estimates'] = {k: float(v).hex() for k, v in estimates.items()}
    return r

controls = []
# lomax transfers: pareto2 and invpareto
p_free = find('pareto2', lambda r, x: r['mask'] == 4 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5)
p_lim = find('pareto2', lambda r, x: r['mask'] == 4 and r['result']['outcome']['kind'] == 'limit' and len(set(x)) > 2)
p_held = find('pareto2', lambda r, x: r['mask'] == 5 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5)
p_zero = find('pareto2', lambda r, x: r['mask'] == 4 and r['source'] == 'class' and r['case'] == 'zeros')
controls += [('pareto2 all free: scale x (1 + 1e-4)', scaled(p_free, 'scale', 1+1e-4), 'root'),
             ('pareto2 all free: shape x (1 + 1e-4)', scaled(p_free, 'shape', 1+1e-4), 'coefficient'),
             ('pareto2 shape held: scale x (1 + 1e-6)', scaled(p_held, 'scale', 1+1e-6), 'root'),
             ('pareto2 zeros with both coefficients free claimed finite', claim(p_zero, 'estimate', {'shape': 1, 'scale': 1}), 'support')]
i_free = find('invpareto', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5)
i_held = find('invpareto', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5)
controls += [('invpareto all free: scale x (1 + 1e-4)', scaled(i_free, 'scale', 1+1e-4), 'root'),
             ('invpareto all free: shape x (1 + 1e-4)', scaled(i_free, 'shape', 1+1e-4), 'coefficient'),
             ('invpareto shape held: scale x (1 + 1e-6)', scaled(i_held, 'scale', 1+1e-6), 'root')]
# data that has a finite maximizer far above the exponential, claimed as the exponential limit
exp_data = [0.1, 0.3, 0.9, 2.5, 7.0, 19.0, 55.0, 150.0]
controls += [('pareto2 limit claimed on a heavy-tailed sample', (p_lim, exp_data), 'face')]
# cauchy, scale held
c_one = find('cauchy', lambda r, x: r['mask'] == 2 and r['result']['outcome']['kind'] == 'estimate' and r['result']['outcome']['unique'] and len(set(x)) > 3)
c_bi = find('cauchy', lambda r, x: r['mask'] == 2 and r['result']['outcome']['kind'] == 'estimate' and not r['result']['outcome']['unique'])
# a sample whose lower mode (4 points near -10) beats the upper one (2 points near 10): claim the upper root
two_modes = [-10.0, -10.2, -9.8, -9.9, 10.0, 10.3]
cc = cr.cert_replay_covers
def mode_root(x, scale, start):
    q = cc.CauchyLocation(x, scale)
    return cc.mp.findroot(lambda t: (lambda z: (cc.mp.mpf(z.a)+cc.mp.mpf(z.b))/2)(q.deriv(cc.mp.mpf(t), cc.mp.mpf(t))), cc.mp.mpf(start), tol=cc.mp.mpf(10)**-40)*cc.mp.mpf(scale)
upper = float(mode_root(two_modes, c_one['held']['scale'], 10.1/c_one['held']['scale']))
controls += [('cauchy scale held: location + 1e-8', set_est(c_one, location=num(c_one, 'location')+1e-8), 'bracket'),
             ('cauchy scale held: location + 0.5', set_est(c_one, location=num(c_one, 'location')+0.5), 'bracket'),
             ('cauchy scale held: the weaker (upper) local maximum claimed as the maximum', (set_est(c_one, location=upper), two_modes), 'cover')]
# gpd
g_int = find('gpd', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate' and num(r, 'xi') != -1.0 and len(x) >= 5)
g_uni = find('gpd', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate' and num(r, 'xi') == -1.0 and len(x) >= 5)
g_held = find('gpd', lambda r, x: r['mask'] == 3 and r['result']['outcome']['kind'] == 'estimate' and num(r, 'xi') != -1.0 and len(x) >= 5)
controls += [('gpd joint: xi + 1e-5', set_est(g_int, xi=num(g_int, 'xi')+1e-5), 'bracket'),
             ('gpd joint: sigma x (1 + 1e-5)', scaled(g_int, 'sigma', 1+1e-5), 'coefficient'),
             ('gpd joint: interior sample claimed the uniform member', set_est(g_int, xi=-1.0, sigma=max(float(v) for v in data(g_int))), 'cover'),
             ('gpd joint: uniform-member sample claimed interior (xi = 0.3)', set_est(g_uni, xi=0.3), 'bracket'),
             ('gpd scale held: xi + 1e-5', set_est(g_held, xi=num(g_held, 'xi')+1e-5), 'bracket')]
# foldnorm joint
f_pos = find('foldnorm', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and num(r, 'mu') > 0 and len(x) >= 5)
f_zero = find('foldnorm', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and num(r, 'mu') == 0 and len(x) >= 5)
controls += [('foldnorm joint: mu x (1 + 1e-6)', scaled(f_pos, 'mu', 1+1e-6), 'bracket'),
             ('foldnorm joint: interior sample claimed mu = 0', set_est(f_pos, mu=0.0), 'cover'),
             ('foldnorm joint: half-normal sample claimed mu = 3 sigma', set_est(f_zero, mu=3*num(f_zero, 'sigma')), 'cover')]

# fatigue: free scale and alpha held
fa_free = find('fatigue', lambda r, x: r['mask'] == 4 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5)
fa_held = find('fatigue', lambda r, x: r['mask'] == 5 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5 and len(set(x)) > 2)
fa_rec = copy.deepcopy(fa_held); fa_rec['held']['alpha'] = 4.0
fa_alpha = 3.0
fa_rec['held']['alpha'] = fa_alpha
fa_x = [1.0, 1.0, 1.0, 1.0, 1.4]; fa_t = [v-fa_rec['held']['mu'] for v in fa_x]
fa_p = cc.FatigueProfile(fa_t, fa_alpha)
dmid = lambda b: (lambda z: (cc.mp.mpf(z.a)+cc.mp.mpf(z.b))/2)(fa_p.deriv(cc.mp.mpf(b)))
grid = [cc.mp.mpf(10)**(k/cc.mp.mpf(20)) for k in range(-60, 61)]      # log grid 1e-3 .. 1e3; sign changes of l'
fa_vals = []
for u, v in zip(grid, grid[1:]):
    if dmid(u) > 0 > dmid(v):
        lo_b, hi_b = u, v
        for _ in range(200):
            mid_b = (lo_b+hi_b)/2
            if dmid(mid_b) > 0: lo_b = mid_b
            else: hi_b = mid_b
        z = (lo_b+hi_b)/2; fa_vals.append((cc.mp.mpf(fa_p.value(z).a), z))
fa_vals.sort()
print('fatigue alpha 3 sample: local maxima', [(cc.mp.nstr(v, 8), cc.mp.nstr(z, 8)) for v, z in fa_vals])
controls += [('fatigue free: beta x (1 + 1e-6)', scaled(fa_free, 'beta', 1+1e-6), 'bracket'),
             ('fatigue alpha held: the lower of two local maxima claimed', (set_est(fa_rec, beta=float(fa_vals[0][1])), fa_x), 'cover')]
# skellam: shift both rates along the mean line
sk = find('skellam', lambda r, x: r['mask'] == 0 and r['result']['outcome']['kind'] == 'estimate' and len(set(x)) > 2 and len(x) >= 5)
controls += [('skellam joint: both rates + 0.05', set_est(sk, mu1=num(sk, 'mu1')+0.05, mu2=num(sk, 'mu2')+0.05), 'cover')]

bad = 0
for control in controls:
    label, subject, name = control
    rec, x = subject if isinstance(subject, tuple) else (subject, data(subject))
    fam = rec['family']
    comps, why = cr.REPLAYS[fam](rec, x, 4000)
    got = {c['name']: c['verdict'] for c in comps}
    ok = got.get(name) == 'fails'
    bad += not ok
    print('%-4s %-78s %-9s %s: %s  %s' % ('ok' if ok else 'FAIL', label, fam, rec['source']+'/'+rec['case'], name, got))
print('%d controls, %d did not fail as intended' % (len(controls), bad))
sys.exit(1 if bad else 0)
