"""PGW likelihood in log-observation units; all input floats are exact dyadics."""
import math

import mpmath as mp


def logdata(sample):
    return [mp.log(mp.mpf(x)) for x in sample]


def likelihood(y, b, v, h):
    t = [mp.exp(v + b * yi) for yi in y]
    u = [mp.log1p(h * ti) / h if h else ti for ti in t]
    return sum(mp.log(b) + mp.log(ti) - mp.log1p(h * ti)
               + ui - mp.expm1(ui) for ti, ui in zip(t, u))


def conditional(y, h):
    center = sum(y) / len(y)
    span = max(y) - min(y)
    yy = [(yi - center) / span for yi in y]

    def scores(b, v):
        total = 0
        weighted = 0
        for yi in yy:
            t = mp.exp(v + b * yi)
            denominator = 1 + h * t
            u = mp.log1p(h * t) / h if h else t
            q = (1 - t * mp.expm1(u)) / denominator
            total += q
            weighted += yi * q
        return len(y) / b + weighted, total

    b, v = mp.findroot(scores, (2, -1), maxsteps=100)
    assert b > 0
    return b / span, v - b * center / span


def threshold(y, endpoint=None):
    d = [yi - (min(y) if endpoint is None else endpoint) for yi in y]
    high = 1 / max(d)
    score = lambda k: len(d) / k - sum(di * mp.expm1(k * di) for di in d)
    while score(high) > 0:
        high *= 2
    low = high / 2
    while score(low) < 0:
        low /= 2
    for _ in range(4 * mp.mp.dps):
        mid = (low + high) / 2
        if score(mid) > 0:
            low = mid
        else:
            high = mid
    k = (low + high) / 2
    return k, sum(mp.log(k) + k * di - mp.expm1(k * di) for di in d)


def dyadic(value):
    return mp.mpf(float(value))


def accuracy():
    for sample, c in [((1, 2, 4, 8), 1), ((1, 2, 3, 8), 2),
                      ((1, 1, 1 + 2**-52), 1)]:
        y = logdata(sample)
        h = 1 / mp.mpf(c)
        b, v = conditional(y, h)
        a = mp.exp((mp.log(c) - v) / b)
        ar, br = dyadic(a), dyadic(b)
        errors = [abs(ar / a - 1), abs(br / b - 1)]
        assert max(errors) < mp.mpf('1e-10')
        optimum = likelihood(y, b, v, h) - sum(y)
        public = likelihood(y, br, mp.log(c) - br * mp.log(ar), h) - sum(y)
        # Refit shape at the rounded scale, on a scale-free coordinate.
        objective = lambda ratio: likelihood(
            y, b * ratio, mp.log(c) - b * ratio * mp.log(ar), h)
        derivative = lambda ratio: mp.diff(objective, ratio)
        ratio = mp.findroot(derivative, (mp.mpf('.5'), mp.mpf('1.5')))
        print('rounding', sample, c, float(a).hex(), float(b).hex(),
              [mp.nstr(e, 8) for e in errors],
              'loss', mp.nstr(optimum - public, 12),
              'refit shape error', mp.nstr(abs(ratio - 1), 12))
        if len(set(sample)) == 2:
            assert abs(ratio - 1) > mp.mpf('1e-10')


def examples():
    candidates = [(1, 2, 4), (1, 2, 4, 8, 16), (1, 2, 3, 4, 5),
                  (1, 1, 2, 4, 8), (1, 2, 4, 4, 4, 8),
                  (1, 2, 4, 8, 8, 16), (1, 2, 3, 4, 6, 8, 10),
                  (1, 2, 3, 4, 5, 6, 8, 10, 15),
                  (1, 1, 2, 2, 4, 4, 8), (1, 4, 8, 16, 16, 32)]
    hits = 0
    for sample in candidates:
        y = logdata(sample)
        b, v = conditional(y, 0)
        g = likelihood(y, b, v, 0)
        _, t = threshold(y)
        us = [mp.exp(v + b * yi) for yi in y]
        deriv = sum(-u + u**2 * mp.expm1(u) / 2 for u in us)
        print('profile', sample, 'G', mp.nstr(g, 12), 'T', mp.nstr(t, 12),
              'G derivative', mp.nstr(deriv, 12))
        for h in map(mp.mpf, ('0.1', '0.5', '1', '2', '5')):
            try:
                b, v = conditional(y, h)
            except ValueError:
                continue
            value = likelihood(y, b, v, h)
            if value > max(g, t):
                hits += 1
                print('  witness', h, 'b,v', mp.nstr(b, 12), mp.nstr(v, 12),
                      'gap', mp.nstr(value - max(g, t), 12))
    assert hits


if __name__ == '__main__':
    mp.mp.dps = 85
    accuracy()
    examples()
