#!/usr/bin/env python3
"""Negative controls for dev/validation/cert-replay.py: perturbed claims must fail.

Takes stored snapshot records (native baseline of master 7e3c9779), changes
the returned coefficients or the outcome kind so that the certificate's claim
becomes false, and replays them. Every control is expected to end in 'fails'
for the named comparison; the script exits nonzero otherwise.

  python3 negative-controls.py [SNAPSHOT]   (one slot: /tmp/umvl-cpu/run)
"""
import copy, gzip, importlib.util, json, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
spec = importlib.util.spec_from_file_location('cert_replay', ROOT/'dev/validation/cert-replay.py')
cr = importlib.util.module_from_spec(spec); spec.loader.exec_module(cr)
cr.iv.dps = 50; cr.mp.mp.dps = 50

snapshot = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT/'dev/research/evaluations/2026-11-14-fit-snapshot-data/native.jsonl.gz'
rows = {}
with gzip.open(snapshot, 'rt') as f:
    for line in f:
        r = json.loads(line); rows[(r['family'], r['source'], r['case'])] = r
archive = {}
for c in json.loads(gzip.decompress(cr.fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
inputs = {f: cr.rebuild(f, archive) for f in ('nbinom', 'lomax')}

def scale(r, name, factor):
    r = copy.deepcopy(r); v = cr.number(r['result']['estimates'][name])
    r['result']['estimates'][name] = float(v*factor).hex(); return r

def as_kind(r, kind, estimates=None):
    r = copy.deepcopy(r); r['result']['outcome']['kind'] = kind; r['result']['kind'] = 'ok' if kind == 'estimate' else kind
    if estimates is not None: r['result']['estimates'] = {k: float(v).hex() for k, v in estimates.items()}
    return r

def first(family, mask, kind, source='archive'):
    for key, r in sorted(rows.items()):
        if key[0] == family and key[1] == source and r['mask'] == mask and (r['result'].get('outcome') or {}).get('kind') == kind:
            return r
    raise SystemExit('no %s %s mask %d case' % (family, kind, mask))

controls = []
nb_free = first('nbinom', 0, 'estimate'); nb_limit = first('nbinom', 0, 'limit'); nb_prob = first('nbinom', 2, 'estimate')
controls += [('nbinom size x(1+2e-6), all free', scale(nb_free, 'size', 1+2e-6), 'root'),
             ('nbinom size x(1-2e-6), prob held', scale(nb_prob, 'size', 1-2e-6), 'root'),
             ('nbinom Poisson-limit sample claimed finite', as_kind(nb_limit, 'estimate', {'size': 5.0, 'prob': 0.5}), 'dispersion'),
             ('nbinom finite sample claimed Poisson limit', as_kind(nb_free, 'limit', {}), 'dispersion')]
lx_free = first('lomax', 0, 'estimate'); lx_limit = first('lomax', 0, 'limit'); lx_kappa = first('lomax', 2, 'estimate')
controls += [('lomax lambda x(1+1e-5), all free', scale(lx_free, 'lambda', 1+1e-5), 'root'),
             ('lomax lambda x(1+1e-9), kappa held', scale(lx_kappa, 'lambda', 1+1e-9), 'root'),
             ('lomax finite-MLE sample claimed exponential limit', as_kind(lx_free, 'limit', {}), 'face'),
             ('lomax exponential sample claimed finite at lambda=1/mean, kappa=1e6',
              as_kind(lx_limit, 'estimate', {'lambda': 1.0, 'kappa': 1e6}), 'gain')]
bad = 0
for label, r, expect in controls:
    family = r['family']; c, _ = inputs[family][(r['source'], r['case'])]
    if family == 'lomax' and expect == 'gain':   # put lambda*kappa at the exponential rate
        x = [float(v) for v in c['data']]; mean = sum(x)/len(x)
        r['result']['estimates'] = {'lambda': float(1/(1e6*mean)).hex(), 'kappa': float(1e6).hex()}
    comps, why = cr.REPLAYS[family](r, c['data'], 4000)
    got = {k['name']: k['verdict'] for k in comps}
    ok = got.get(expect) == 'fails'
    bad += not ok
    print('%-4s %-64s %s/%s: %s  %s' % ('ok' if ok else 'BAD', label, r['source'], r['case'], expect, got))
sys.exit(1 if bad else 0)
