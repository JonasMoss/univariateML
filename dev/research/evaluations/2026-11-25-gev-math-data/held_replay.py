"""Locate the competing branches in the two exact held-parameter constructions."""
import mpmath as mp
import numpy as np

from replay import sample
from witnesses import fixed_value


def fields(z, sigma, shape, axis=-1):
    with np.errstate(divide='ignore', invalid='ignore', over='ignore'):
        ell = np.log1p(shape*z)/shape if shape else z
        terms = -(1+shape)*ell-np.exp(-ell)
    terms = np.where(np.isfinite(terms), terms, -np.inf)
    return terms.mean(axis=axis)-np.log(sigma)


def fixed_scale(data, shape):
    if not shape:
        mu = -np.log(np.mean(np.exp(-data)))
        return fields(data-mu, 1, shape), mu
    shift = -np.expm1(-shape*np.log1p(shape))/shape
    lo, hi = min(data)+shift, max(data)+shift
    if shape < 0:
        lo = max(lo, max(data)+1/shape)
    else:
        hi = min(hi, min(data)+1/shape)
    for _ in range(45):
        mu = (lo+hi)/2
        t = 1+shape*(data-mu)
        with np.errstate(over='ignore'):
            score = np.sum((1+shape-t**(-1/shape))/t)
        if score > 0:
            lo = mu
        else:
            hi = mu
    mu = (lo+hi)/2
    return fields(data-mu, 1, shape), mu


def fixed_location(data, mu, shape):
    gaps = data-mu
    lower = max(0, max(-shape*gaps))
    scales = lower+np.geomspace(1e-7, 100, 257)
    values = fields(gaps/scales[:, None], scales, shape)
    k = np.argmax(values)
    return values[k], scales[k]


def explore(mask, t):
    grid = np.linspace(-.98, 1, 199)
    if mask == 'scale':
        data = np.array([0, t, t, t])
        face = -t/4
        fits = [fixed_scale(data, b) for b in grid]
    else:
        data = np.array(sample(t))
        mu = 3*t/4
        sigma = max(data)-mu
        face = -np.log(sigma)-1+(data.mean()-mu)/sigma
        fits = [fixed_location(data, mu, b) for b in grid]
    value, k = max((v, k) for k, (v, _) in enumerate(fits))
    return value-face, grid[k], fits[k][1]


def high_crossing(mask):
    lo, hi = (4, 100) if mask == 'scale' else (1e-4, 1)
    signs = (-1, 1) if mask == 'scale' else (1, -1)
    for _ in range(16):
        mid = (lo+hi)/2
        obj, b, nuisance = explore(mask, mid)
        if obj*signs[0] > 0:
            lo = mid
        else:
            hi = mid
    t = (lo+hi)/2
    _, b, nuisance = explore(mask, lo if signs[0] > 0 else hi)

    def expressions(b, x, t):
        if mask == 'scale':
            data, mu, sigma = [0,t,t,t], x, mp.mpf(1)
            face = -t/4
        else:
            data, mu, sigma = sample(t), 3*t/4, x
            reverse_sigma = max(data)-mu
            face = -mp.log(reverse_sigma)-1+(mp.fsum(data)/4-mu)/reverse_sigma
        return fixed_value(data, mu, sigma, b, mp)-face

    if b == 1:
        root = mp.findroot(
            lambda x, t: (mp.diff(lambda u: expressions(1, u, t), x),
                          expressions(1, x, t)),
            (nuisance, t), tol=mp.mpf('1e-60'))
        x, t = root
        b = mp.mpf(1)
        assert mp.diff(lambda y: expressions(y, x, t), b) > 0
    else:
        root = mp.findroot(
            lambda b, x, t: (mp.diff(lambda u: expressions(b, u, t), x),
                             mp.diff(lambda y: expressions(y, x, t), b),
                             expressions(b, x, t)),
            (b, nuisance, t), tol=mp.mpf('1e-60'))
        b, x, t = root
    assert abs(expressions(b, x, t)) < mp.mpf('1e-58')
    assert mp.diff(lambda u: expressions(b, u, t), x, 2) < 0
    print(mask, 'parameter, shape, nuisance',
          *(mp.nstr(v, 35) for v in (t,b,x)), flush=True)


if __name__ == '__main__':
    mp.mp.dps = 65
    for mask in ('scale', 'location'):
        high_crossing(mask)
