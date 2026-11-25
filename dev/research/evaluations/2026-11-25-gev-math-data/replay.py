"""Independent GEV likelihood profiles. Run with one CPU slot."""
import mpmath as mp
import numpy as np


def trial(gaps, shape, v):
    z = gaps / v
    r = z / (1 + shape * z)
    ell = np.log1p(shape * z) / shape if shape else z
    weights = np.exp(-ell)
    score = -1 + (1 + shape) * r.mean() - np.dot(weights, r) / weights.sum()
    value = -np.log(v) - np.log(weights.mean()) - (1 + shape) * ell.mean() - 1
    return score, value


def profile(data, shape):
    gaps = np.sort(data) - min(data)
    if shape == -1:
        return -1 - np.log(np.mean(gaps[-1] - gaps))
    lo, hi = max(0, -shape * gaps[-1]), gaps[-1] * 2
    while trial(gaps, shape, hi)[0] > 0:
        hi *= 2
    for _ in range(52):
        mid = (lo + hi) / 2
        if trial(gaps, shape, mid)[0] > 0:
            lo = mid
        else:
            hi = mid
    return trial(gaps, shape, hi)[1]


def high(data, shape):
    gaps = [x - min(data) for x in data]
    if shape == -1:
        sigma = mp.fsum(max(data) - x for x in data) / len(data)
        return -1 - mp.log(sigma), max(data) - sigma, sigma, mp.nan

    def fields(v):
        z = [x / v for x in gaps]
        radii = [x / (1 + shape * x) for x in z]
        ell = [mp.log1p(shape * x) / shape if shape else x for x in z]
        weights = [mp.exp(-x) for x in ell]
        score = -1 + (1 + shape) * mp.fsum(radii) / len(data)
        score -= mp.fdot(weights, radii) / mp.fsum(weights)
        return score, ell, weights, z

    lo, hi = max(0, -shape * max(gaps)), 2 * max(gaps)
    while fields(hi)[0] > 0:
        hi *= 2
    for _ in range(220):
        mid = (lo + hi) / 2
        if fields(mid)[0] > 0:
            lo = mid
        else:
            hi = mid
    _, ell, weights, z = fields(hi)
    log_mean = mp.log(mp.fsum(weights) / len(data))
    value = -mp.log(hi) - log_mean - (1 + shape) * mp.fsum(ell) / len(data) - 1
    derivatives = [(x / (1 + shape*x) - t) / shape if shape else -x*x/2
                   for x, t in zip(z, ell)]
    score = (-mp.fsum(ell) - (1 + shape) * mp.fsum(derivatives)) / len(data)
    score += mp.fdot(weights, derivatives) / mp.fsum(weights)
    sigma = hi * mp.exp(-shape * log_mean)
    mu = min(data) - hi * mp.expm1(-shape * log_mean) / (-shape) if shape else min(data) - hi * log_mean
    # v = sigma + xi*(min(data)-mu)
    return value, mu, sigma, score


def sample(t):
    return [t*0, t, t*0+1, 2-t]


def main():
    mp.mp.dps = 70
    grid = np.linspace(-1, 1, 401)
    for text in ('0.0001', '0.001', '0.01', '0.02', '0.04', '0.08',
                 '0.16', '0.32', '0.64', '1'):
        t = mp.mpf(text)
        data = sample(t)
        values = np.array([profile(list(map(float, data)), b) for b in grid])
        peaks = np.flatnonzero((values[1:-1] > values[:-2]) &
                               (values[1:-1] > values[2:])) + 1
        points = [mp.mpf(-1), *[mp.mpf(str(round(grid[i], 3))) for i in peaks],
                  mp.mpf(0), mp.mpf(1)]
        print(text, [(str(b), mp.nstr(high(data, b)[0], 15)) for b in points])
        for b in (-1, 0, 1):
            v, mu, sigma, _ = high(data, mp.mpf(b))
            assert abs(mu + sigma - max(data)) < mp.mpf('1e-60') if b == -1 else sigma > 0
            terms = []
            for x in data:
                z = (x - mu) / sigma
                if b == -1:
                    terms.append(-mp.log(sigma) - (1 - z))
                else:
                    ell = mp.log1p(b*z)/b if b else z
                    terms.append(-mp.log(sigma) - (1+b)*ell - mp.exp(-ell))
            assert abs(v - mp.fsum(terms)/len(data)) < mp.mpf('1e-60')
    root = mp.findroot(lambda t: high(sample(t), mp.mpf(1))[0] -
                       high(sample(t), mp.mpf(-1))[0], (mp.mpf('.001'), mp.mpf('.04')))
    print('Boundary-to-boundary equality candidate', mp.nstr(root, 50))
    for b in ('-1', '-.9', '-.5', '0', '.25', '.5', '.75', '1'):
        print(b, [mp.nstr(x, 25) for x in high(sample(root), mp.mpf(b))])


if __name__ == '__main__':
    main()
