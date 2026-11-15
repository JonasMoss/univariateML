#!/usr/bin/env python3
"""Negative controls for the batch 2 of the mechanism-E/F replays (dev/validation/cert_replay_covers.py): gpd xi held, burr power held, dgamma, ht.

Takes stored snapshot records (the 2026-11-15 baselines), changes the returned
coefficients or the data so that the replayed inequality becomes false, and
replays them through cert-replay.py's REPLAYS. Every control must end in
'fails' for the named comparison; the script exits nonzero otherwise.

  OPENBLAS_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 negative-controls.py

The replay cache is disabled for the controls (CERT_REPLAY_COVERS_CACHE=/dev/null/none).
"""
import copy, gzip, importlib.util, json, os, sys
from pathlib import Path

os.environ['CERT_REPLAY_COVERS_CACHE'] = '/tmp/cert-replay-covers2-controls-cache.jsonl.gz'
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
spec = importlib.util.spec_from_file_location('cert_replay', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
cr.iv.dps = 50; cr.mp.mp.dps = 50
if Path('/tmp/cert-replay-covers2-controls-cache.jsonl.gz').exists(): Path('/tmp/cert-replay-covers2-controls-cache.jsonl.gz').unlink()
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
cc = cr.cert_replay_covers
# gpd, shape held
gx = find('gpd', lambda r, x: r['mask'] == 5 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5)
controls += [('gpd xi held: sigma x (1 + 1e-6)', scaled(gx, 'sigma', 1+1e-6), 'root'),
             ('gpd xi held: sigma x 1.5', scaled(gx, 'sigma', 1.5), 'root')]
# burr, shape2 held: Lomax on (x / max x)^c
bu = find('burr', lambda r, x: r['mask'] == 2 and r['result']['outcome']['kind'] == 'estimate' and len(set(x)) > 3)
bl = find('burr', lambda r, x: r['mask'] == 2 and r['result']['outcome']['kind'] == 'limit' and len(set(x)) > 3)
heavy = [0.1, 0.3, 0.9, 2.5, 7.0, 19.0, 55.0, 150.0]
controls += [('burr power held: scale x (1 + 1e-4)', scaled(bu, 'scale', 1+1e-4), 'root'),
             ('burr power held: shape1 x (1 + 1e-4)', scaled(bu, 'shape1', 1+1e-4), 'coefficient'),
             ('burr power held: Weibull limit claimed on a heavy-tailed sample', (bl, heavy), 'limit')]
# dgamma
d1 = find('dgamma', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5)
d2 = find('dgamma', lambda r, x: r['mask'] == 2 and r['result']['outcome']['kind'] == 'estimate' and len(x) >= 5 and num(r, 'shape') < 50)
controls += [('dgamma shape held: rate x (1 + 1e-6)', scaled(d1, 'rate', 1+1e-6), 'bracket'),
             ('dgamma rate held: shape x (1 + 1e-6)', scaled(d2, 'shape', 1+1e-6), 'bracket'),
             ('dgamma rate held: shape x 1.3 claimed the maximum', scaled(d2, 'shape', 1.3), 'cover')]
# ht: free scale, held scale, and a half-normal-like sample claimed finite
h1 = find('ht', lambda r, x: r['mask'] == 1 and r['result']['outcome']['kind'] == 'estimate' and len(x) == 12)
h5 = find('ht', lambda r, x: r['mask'] == 5 and r['result']['outcome']['kind'] == 'estimate' and len(x) == 12)
hnorm = [0.3, 0.7, 1.1, 0.2, 1.6, 0.9, 0.5, 1.2, 0.8, 0.4, 1.0, 0.6]
controls += [('ht endpoint and sigma held: nu x (1 + 1e-6)', scaled(h5, 'nu', 1+1e-6), 'bracket'),
             ('ht endpoint held, scale free: nu x (1 + 1e-6)', scaled(h1, 'nu', 1+1e-6), 'bracket'),
             ('ht endpoint held, scale free: nu x 1.5 claimed the maximum', scaled(h1, 'nu', 1.5), 'cover'),
             ('ht endpoint and sigma held: a half-normal sample claimed with nu = 2', (set_est(h5, nu=2.0), hnorm), 'cover')]

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
