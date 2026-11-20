"""Independent 80-digit reference for selected large snapshot movements."""
import argparse
from pathlib import Path
import sys

import mpmath as mp

sys.path.insert(0, str(Path(__file__).resolve().parents[3] / 'validation'))
from strict_common import archive_data, load, number, snapshot_module  # noqa: E402


def weibull_family(xs, sign, shape):
    anchor = xs[0]
    w = [sign * mp.log(x / anchor) for x in xs]
    n = len(xs)

    def mean(k):
        q = [mp.exp(k*v) for v in w]
        return mp.fsum(v*p for v, p in zip(w, q))/mp.fsum(q)

    k = mp.findroot(lambda k: 1/k + mp.fsum(w)/n - mean(k), shape)
    scale = anchor * mp.exp(sign * mp.log(mp.fsum(mp.exp(k*v) for v in w)/n)/k)
    return {'shape': k, 'scale': scale}


def invgamma(xs, shape):
    y = [1/x for x in xs]
    ybar = mp.fsum(y)/len(y)
    gap = mp.log(ybar) - mp.fsum(map(mp.log, y))/len(y)
    k = mp.findroot(lambda k: mp.log(k) - mp.digamma(k) - gap, shape)
    return {'alpha': k, 'beta': k/ybar}


def llogis(xs, shape, scale):
    w = [mp.log(x/xs[0]) for x in xs]

    def score(k, t):
        u = [v-t for v in w]
        tail = [mp.tanh(k*v/2) for v in u]
        return len(w)/k-mp.fsum(v*s for v, s in zip(u, tail)), mp.fsum(tail)

    k, t = mp.findroot(score, (shape, mp.log(scale/xs[0])))
    return {'shape': k, 'scale': xs[0]*mp.exp(t)}


def run(baseline, candidate):
    mp.mp.dps = 80
    before, after = load(baseline), load(candidate)
    archive = archive_data()
    keys = [
        ('invgamma', 'class', 'tiny'),
        ('invweibull', 'class', 'tiny'),
        ('llogis', 'archive', '004533'),
        ('llogis', 'golden', 'golden'),
        ('weibull', 'class', 'huge'),
        ('weibull', 'class', 'tiny'),
    ]
    for key in keys:
        family, source, case = key
        a, b = (entry[key]['result']['estimates'] for entry in (before, after))
        if source == 'archive':
            data = archive[int(case)]
        elif source == 'golden':
            data = snapshot_module().read(family)[1]['data']
        else:
            data = after[key]['data']
        x = [mp.mpf(float(v)) for v in data]
        est = {name: mp.mpf(number(v)) for name, v in b.items()}
        if family == 'invgamma':
            truth = invgamma(x, est['alpha'])
        elif family == 'llogis':
            truth = llogis(x, est['shape'], est['scale'])
        else:
            truth = weibull_family(x, -1 if family == 'invweibull' else 1, est['shape'])
        print('/'.join(key))
        for name, target in truth.items():
            old = (mp.mpf(number(a[name]))-target)/target
            new = (est[name]-target)/target
            print(f"  {name} ref {mp.nstr(target, 50)}  "
                  f"relative error {mp.nstr(old, 6)} -> {mp.nstr(new, 6)}")


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('baseline')
    parser.add_argument('candidate')
    args = parser.parse_args()
    run(args.baseline, args.candidate)
