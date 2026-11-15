#!/usr/bin/env python3
"""Negative controls for dev/validation/cert_replay_roots.py: perturbed coefficients must fail.

Takes stored native baseline records (master 7e3c9779), multiplies a returned coefficient by a factor large enough
that the claimed bracket (mechanism C, 'root'/'moment') or the represented-likelihood check (mechanism B, 'restore')
is false, and replays the record. Every control must end in 'fails' for the named comparison; exit 1 otherwise.

  /tmp/umvl-cpu/run python3 negative-controls.py [SNAPSHOT,ROUTE_SNAPSHOT]
"""
import copy, gzip, importlib.util, json, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
spec = importlib.util.spec_from_file_location('cert_replay', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
cr.iv.dps = 50; cr.mp.mp.dps = 50
base = ROOT/'dev/research/evaluations/2026-11-14-fit-snapshot-data'
paths = sys.argv[1].split(',') if len(sys.argv) > 1 else [base/'native.jsonl.gz', base/'routes-native.jsonl.gz']
rows = {}
for p in paths:
    with gzip.open(p, 'rt') as f:
        for line in f:
            r = json.loads(line); rows[(r['family'], r['source'], r['case'])] = r
archive = {}
for c in json.loads(gzip.decompress(cr.fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
inputs = {}

def scale(r, name, factor):
    r = copy.deepcopy(r); v = cr.number(r['result']['estimates'][name])
    r['result']['estimates'][name] = float(v*factor).hex(); return r

def first(family, mask, certified=True, minimum_n=8):   # certified is advisory: the control forces the certified claim
    for source, n_min in (('archive', minimum_n), ('golden', 1), ('class', 1), ('resample', 1)):
        for key, r in sorted(rows.items()):
            o = r['result'].get('outcome') or {}
            if key[0] == family and key[1] == source and r['mask'] == mask and o.get('kind') == 'estimate' and True and r['n'] >= n_min:
                return r
    raise SystemExit('no %s mask %d case' % (family, mask))

def inputs_data(family, r):
    if family not in inputs: inputs[family] = cr.rebuild(family, archive)
    return inputs[family][(r['source'], r['case'])][0]['data']

controls = [   # (label, record, expected comparison)
    ('C gumbel sigma x(1+1e-6), all free',            scale(first('gumbel', 0), 'sigma', 1+1e-6), 'root'),
    ('C gumbel sigma x(1+1e-6), mu held',             scale(first('gumbel', 1), 'sigma', 1+1e-6), 'root'),
    ('C weibull shape x(1+1e-6), all free',           scale(first('weibull', 0), 'shape', 1+1e-6), 'root'),
    ('C kumar a x(1+1e-6)',                           scale(first('kumar', 0), 'a', 1+1e-6), 'root'),
    ('C hcauchy sigma x(1+1e-6)',                     scale(first('hcauchy', 1, minimum_n=8), 'sigma', 1+1e-6), 'root'),
    ('C lgser theta x(1-1e-6)',                       scale(first('lgser', 0), 'theta', 1-1e-6), 'root'),
    ('C yulesimon rho x(1+1e-6)',                     scale(first('yulesimon', 0), 'rho', 1+1e-6), 'root'),
    ('C beta shape2 x(1+1e-6), shape1 held',          scale(first('beta', 1), 'shape2', 1+1e-6), 'root'),
    ('C beta shape1 x(1+1e-3), all free',             scale(first('beta', 0), 'shape1', 1+1e-3), 'root'),
    ('C gamma shape x(1+1e-3), all free',             scale(first('gamma', 0), 'shape', 1+1e-3), 'root'),
    ('C genbeta shape3 x(1+1e-6), a, b, scale held',  scale(first('genbeta', 11), 'shape3', 1+1e-6), 'root'),
    ('C gev mu + 1e-6 sigma, sigma and xi held',      None, 'root'),
    ('C pois+zero=truncated lambda x(1+1e-6)',        scale(first('pois+zero=truncated', 0), 'lambda', 1+1e-6), 'moment'),
    ('B lnorm meanlog x(1+1e-2), all free',           scale(first('lnorm', 0), 'meanlog', 1+1e-2), 'restore'),
    ('B logitnorm mu + 0.5, all free',                None, 'restore'),
    ('B invgamma beta x(1+1e-1), all free',           scale(first('invgamma', 0), 'beta', 1.1), 'restore'),
    ('B llogis scale x(1+1e-1), all free',            scale(first('llogis', 0), 'scale', 1.1), 'restore'),
    ('B invweibull scale x(1+1e-1), all free',        scale(first('invweibull', 0), 'scale', 1.1), 'restore'),
]
for i, (label, r, expect) in enumerate(controls):
    if r is None:
        if label.startswith('C gev'):
            # the snapshot has no certified gev record with held xi in (-1, 0): build one from a mask-6 record,
            # holding xi = -0.3 and sigma = 3 range, with mu* located in 60 digits; mu* + 1e-6 sigma must fail
            r = copy.deepcopy(first('gev', 6, certified=False)); x = [float(v) for v in inputs_data('gev', r)]
            cr.mp.mp.dps = 60; sg = 3*(max(x)-min(x)); xi = -0.3
            S = lambda m: sum((xi+1-(1+xi*(cr.mp.mpf(v)-m)/sg)**(-1/xi))/(1+xi*(cr.mp.mpf(v)-m)/sg) for v in x)
            lo = cr.mp.mpf(max(x))-sg/(-xi)*0.999; hi = cr.mp.mpf(min(x))+sg
            assert S(lo) > 0 > S(hi), (S(lo), S(hi))
            for _ in range(200):
                mid = (lo+hi)/2
                if S(mid) > 0: lo = mid
                else: hi = mid
            cr.mp.mp.dps = 50; mu_star = float(lo)
            r['held'] = {'sigma': float(sg), 'xi': xi}
            r['result']['estimates'] = dict(r['result']['estimates']); r['result']['estimates']['mu'] = float(lo+cr.mp.mpf(1e-6)*sg).hex()
            r['result']['estimates']['sigma'] = float(sg).hex(); r['result']['estimates']['xi'] = float(xi).hex()
        else:
            r = copy.deepcopy(first('logitnorm', 0)); v = cr.number(r['result']['estimates']['mu']); r['result']['estimates']['mu'] = float(v+0.5).hex()
        controls[i] = (label, r, expect)
bad = 0
for label, r, expect in controls:
    r['result']['outcome']['certified'] = True   # the claim under test is the certified one
    family = r['family']
    if family not in inputs: inputs[family] = cr.rebuild(family, archive)
    c, _ = inputs[family][(r['source'], r['case'])]
    comps, why = cr.REPLAYS[family](r, c['data'], 4000)
    got = {k['name']: k.get('judged_verdict', k['verdict']) for k in comps}
    ok = got.get(expect) == 'fails'
    bad += not ok
    print('%-4s %-52s %s/%s/%s: %s  %s' % ('ok' if ok else 'BAD', label, family, r['source'], r['case'], expect, got))
# positive control for gev (replay must hold at the located root, else the failing control above proves nothing)
g = next(r for l, r, e in controls if l.startswith('C gev')); g = copy.deepcopy(g); g['result']['estimates']['mu'] = float(mu_star).hex()
got = cr.REPLAYS['gev'](g, inputs_data('gev', g), 4000)[0][0]['verdict']
print('%-4s %-52s gev positive control at mu*: %s' % ('ok' if got == 'holds' else 'BAD', 'C gev mu*, sigma and xi held', got)); bad += got != 'holds'
sys.exit(1 if bad else 0)
