"""Synthetic controls for TASK-214.34, independent of the fitted snapshots."""

import importlib.util
from pathlib import Path
import sys

validation = Path(__file__).resolve().parents[3] / 'validation'
sys.path.insert(0, str(validation))
spec = importlib.util.spec_from_file_location('driver', validation / 'cert-replay.py')
driver = importlib.util.module_from_spec(spec)
spec.loader.exec_module(driver)

from mpmath import mp

mp.dps = 50
driver.iv.dps = 50


def record(family, held, mask, estimates=None, model=None):
    outcome = {'kind': 'limit' if model else 'estimate', 'certified': True}
    return {
        'family': family,
        'held': held,
        'mask': mask,
        'result': {
            'outcome': outcome,
            'estimates': {k: float(v).hex() for k, v in (estimates or {}).items()},
            'model': model,
            "logLik": "nan",
        },
    }


def check(r, data, name, verdict):
    comparisons, why = driver.REPLAYS[r['family']](r, data, 4000)
    actual = next((c for c in comparisons if c['name'] == name), None)
    assert actual and actual['verdict'] == verdict, (r, name, verdict, comparisons, why)
    print(r['family'], name, verdict, actual['hp'])


# For kappa fixed, solve the elementary Lomax score independently.
data = [1., 2., 8.]
for reflected in (False, True):
    kappa = 2
    y = [1/v for v in data] if reflected else data
    rate = mp.exp(mp.findroot(
        lambda t: 3-(kappa+1)*sum(mp.exp(t)*v/(1+mp.exp(t)*v) for v in y), 0))
    a, b = (kappa, 1) if reflected else (1, kappa)
    scale = rate if reflected else 1/rate
    held = {'shape1': a, 'shape2': b}
    for factor, verdict in ((1, 'holds'), (mp.mpf('1.000001'), 'fails')):
        check(record('betapr', held, 3, {**held, 'scale': scale*factor}),
              data, 'root', verdict)

# The all-free Lomax profile on the transformed sample has an interior gain.
y = [1, 1, 10]


def profile_score(rate):
    a = mp.fsum(mp.log1p(rate*v) for v in y)/3
    b = mp.fsum(rate*v/(1+rate*v) for v in y)/3
    return 1-b/a-b


rate = mp.findroot(profile_score, (mp.mpf('0.08'), mp.mpf('0.13')))
kappa = 3/mp.fsum(mp.log1p(rate*v) for v in y)
check(record('betapr', {'shape1': 1}, 1,
             {'shape1': 1, 'shape2': kappa, 'scale': 1/rate}), y, 'global', 'holds')
check(record('betapr', {'shape2': 1}, 2,
             {'shape1': kappa, 'shape2': 1, 'scale': rate}),
      [1., 1., 0.1], 'global', 'holds')
check(record('burr', {'shape2': 1}, 2,
             {'shape1': kappa, 'shape2': 1, 'scale': 1/rate}), y, 'global', 'holds')

check(record('sgomp', {}, 0, {'b': 2, 'eta': 0}), [0., 1., 2.], 'rate-cover', 'fails')
check(record('dgamma', {}, 0, {'shape': 1, 'rate': 1}), [0, 2, 7], 'joint-cover', 'fails')
check(record('trgamma', {}, 0, model='lnorm'), [1., 2., 4.], 'power-face-gap', 'fails')
check(record('invtrgamma', {}, 0, model='lnorm'), [1., 0.5, 0.25], 'power-face-gap', 'fails')
check(record('huber', {}, 0, model='norm'), [0.]*20+[100.], 'epsilon-face', 'fails')
