#!/usr/bin/env python3
"""Negative controls for the batch 3 of the mechanism-E/F replays (dev/validation/cert_replay_covers.py): paralogis, invparalogis, slash.

Takes stored snapshot records (the 2026-11-15 baselines), changes the returned
coefficients or the data so that the replayed inequality becomes false, and
replays them through cert-replay.py's REPLAYS. Every control must end in
'fails' for the named comparison; the script exits nonzero otherwise.

  OPENBLAS_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 negative-controls.py

Competing-mode controls (a genuine local maximizer that is not the global one) are built by
solving the stationarity equations at 50 digits from the starting point named in the dossier.
The replay cache is a throwaway file.
"""
import copy, gzip, importlib.util, json, os, sys
from pathlib import Path

CACHE = '/tmp/cert-replay-covers3-controls-cache.jsonl.gz'
os.environ['CERT_REPLAY_COVERS_CACHE'] = CACHE
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
spec = importlib.util.spec_from_file_location('cert_replay', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
cr.iv.dps = 50; cr.mp.mp.dps = 50
mp = cr.mp; cc = cr.cert_replay_covers
if Path(CACHE).exists(): Path(CACHE).unlink()
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
        if key[0] == family and (r['result'].get('outcome') or {}).get('kind') == 'estimate' and test(r, data(r)): return r
    raise SystemExit('no %s case for a control' % family)

def num(r, name): return cr.number(r['result']['estimates'][name])

def set_est(r, **values):
    r = copy.deepcopy(r)
    for k, v in values.items(): r['result']['estimates'][k] = float(v).hex()
    return r

def scaled(r, name, factor): return set_est(r, **{name: num(r, name)*factor})

def synthetic(family, mask, held, estimates, certified=True):
    """A record for a control that is not among the stored cases."""
    return dict(family=family, mask=mask, held=held, source='control', case='synthetic', n=0,
                result=dict(estimates={k: float(v).hex() for k, v in estimates.items()}, outcome=dict(kind='estimate', certified=certified, reason='none', unique=True),
                            logLik=float('nan').hex()))

controls = []
# paralogis, shape held (scale root)
p1 = find('paralogis', lambda r, x: r['mask'] == 1 and len(x) >= 5)
p2 = find('paralogis', lambda r, x: r['mask'] == 2 and len(x) >= 5)
p0 = find('paralogis', lambda r, x: r['mask'] == 0 and len(x) >= 5 and r['result']['outcome'].get('certified') and num(r, 'shape') < 50)
controls += [('paralogis shape held: scale x (1 + 1e-6)', scaled(p1, 'scale', 1+1e-6), 'scale-root'),
             ('paralogis scale held: shape x (1 + 1e-6)', scaled(p2, 'shape', 1+1e-6), 'bracket'),
             ('paralogis both free: shape x (1 + 1e-6)', scaled(p0, 'shape', 1+1e-6), 'bracket'),
             ('paralogis both free: scale x (1 + 1e-6)', scaled(p0, 'scale', 1+1e-6), 'scale-root'),
             ('paralogis both free: estimate claimed for a constant sample', (p0, [2.0, 2.0, 2.0, 2.0]), 'constant')]
# paralogis, scale held: the README example x = exp(-0.001), scale 1: local mode near 2.895 against the global mode near 9262
x1 = [float(mp.exp(mp.mpf('-0.001')))]
pl = cc.ParaLik([mp.log(mp.mpf(v)) for v in x1], 'mp')
local = mp.findroot(lambda a: pl.La(a, mp.mpf(0)), mp.mpf('2.895'), tol=mp.mpf(10)**-40)
controls += [('paralogis scale held: the local mode %s of x = exp(-0.001) claimed (README example)' % mp.nstr(local, 8),
              (synthetic('paralogis', 2, {'scale': 1.0}, {'shape': float(local), 'scale': 1.0}), x1), 'cover')]
# invparalogis
i1 = find('invparalogis', lambda r, x: r['mask'] == 1 and len(x) >= 5)
i2 = find('invparalogis', lambda r, x: r['mask'] == 2 and len(x) >= 5)
controls += [('invparalogis shape held: scale x (1 + 1e-6)', scaled(i1, 'scale', 1+1e-6), 'scale-root'),
             ('invparalogis scale held: shape x (1 + 1e-6)', scaled(i2, 'shape', 1+1e-6), 'bracket')]
# slash
s1 = find('slash', lambda r, x: r['mask'] == 1 and len(x) >= 5)
s2 = find('slash', lambda r, x: r['mask'] == 2 and len(x) >= 5)
s0 = find('slash', lambda r, x: r['mask'] == 0 and len(x) >= 5 and len(x) < 8)
controls += [('slash mu held: sigma x (1 + 1e-6)', scaled(s1, 'sigma', 1+1e-6), 'scale-bracket'),
             ('slash sigma held: mu + 1e-6 max(|mu|, sigma)', set_est(s2, mu=num(s2, 'mu')+1e-6*max(abs(num(s2, 'mu')), num(s2, 'sigma'))), 'bracket'),
             ('slash both free: mu + 1e-6 max(|mu|, sigma)', set_est(s0, mu=num(s0, 'mu')+1e-6*max(abs(num(s0, 'mu')), num(s0, 'sigma'))), 'bracket'),
             ('slash both free: sigma x (1 + 1e-6)', scaled(s0, 'sigma', 1+1e-6), 'accuracy')]
# slash, sigma held: x = (-100, 100, 100.5), sigma 1: the modes near -100 (one observation) and near 100 (two)
x3 = [-100.0, 100.0, 100.5]
hl = cc.SlashHeld(x3, 1.0)
lo_mode = mp.findroot(lambda m: (lambda z: (mp.mpf(z.a)+mp.mpf(z.b))/2)(hl.score_at(m)), mp.mpf(-100), tol=mp.mpf(10)**-40)
controls += [('slash sigma held: the local mode %s of (-100, 100, 100.5) claimed' % mp.nstr(lo_mode, 8),
              (synthetic('slash', 2, {'sigma': 1.0}, {'mu': float(lo_mode), 'sigma': 1.0}), x3), 'cover')]
# slash, both free: the dossier sample, ten zeros, one -1 and nine 10s; the dossier's two modes near (-1/625, 1/5) (L/B > 1.00012) and (9/2, 7/2) (L/B > 1.066); the global one is the second
x4 = [0.0]*10+[-1.0]+[10.0]*9
sp = cc.SlashLik(x4); mid = lambda z: (mp.mpf(z.a)+mp.mpf(z.b))/2
def stationary(m, v):
    R = sp.resid(m); s = cc.iv.mpf(mp.exp(v))
    return mid(sp.lm(R, s)), mid(sp.gsum(R, s))
m4, v4 = mp.findroot(stationary, (mp.mpf(-1)/625, mp.log(mp.mpf(1)/5)), tol=mp.mpf(10)**-40)
controls += [('slash both free: the lower mode (%s, %s) of 10 zeros, -1 and nine 10s claimed' % (mp.nstr(m4, 8), mp.nstr(mp.exp(v4), 8)),
              (synthetic('slash', 0, {}, {'mu': float(m4), 'sigma': float(mp.exp(v4))}), x4), 'cover')]

# paralogis both free: the profile cover machinery (value, derivative enclosure, cover) with a window and target at a wrong shape, 1.5 a_hat
def profile_cover_control():
    r = p0; x = [float(v) for v in data(r)]
    a_hat = mp.mpf(num(r, 'shape')); u_hat = mp.log(mp.mpf(num(r, 'scale')))
    p = cc.ParaProfile(x, u_hat)
    t0 = mp.log(1.5*a_hat); target = cc.lo_(p.value(t0))
    lo, hi = cc.para_cutoffs(p, target, False)
    verdict, info = cc.cover(p.value, p.dt, None, mp.log(lo), mp.log(hi), target, [(t0-mp.mpf('1e-6'), t0+mp.mpf('1e-6'))], budget=4000)
    return verdict, 'cover primitive on %s/%s, target = P(1.5 a_hat) = %s, counterexample at a = %s' % (r['source'], r['case'], mp.nstr(target, 12), mp.nstr(mp.exp(info['point']), 8) if verdict == 'fails' else '-')
controls.append(('paralogis both free: cover primitive with the target at 1.5 a_hat', profile_cover_control, 'cover'))

bad = 0
for label, subject, name in controls:
    if callable(subject):
        verdict, text = subject()
        ok = verdict == 'fails'; bad += not ok
        print('%-4s %-92s %-12s %s: %s  %s' % ('ok' if ok else 'FAIL', label, 'paralogis', 'direct', name, text), flush=True)
        continue
    rec, x = subject if isinstance(subject, tuple) else (subject, data(subject))
    fam = rec['family']
    comps, why = cr.REPLAYS[fam](rec, x, 4000)
    got = {c['name']: c['verdict'] for c in comps}
    ok = got.get(name) == 'fails'
    bad += not ok
    print('%-4s %-92s %-12s %s: %s  %s' % ('ok' if ok else 'FAIL', label, fam, rec['source']+'/'+rec['case'], name, got), flush=True)
print('%d controls, %d did not fail as intended' % (len(controls), bad))
sys.exit(1 if bad else 0)
