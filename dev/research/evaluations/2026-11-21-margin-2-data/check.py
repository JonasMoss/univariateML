"""Independent near-face checks on the exact binary64 inputs."""
from fractions import Fraction
import math
import random

import mpmath as mp


mp.mp.dps = 100


def gompertz(x, held=None):
    x = list(map(mp.mpf, x))
    n = len(x)
    total, squares = sum(x), sum(v*v for v in x)
    if held is None:
        score_at_zero = total - n * squares / (2 * total)
    else:
        score_at_zero = total - mp.mpf(held) * squares / 2

    def integral(b):
        return sum(mp.expm1(b * v) / b for v in x)

    def a(b):
        return n / integral(b) if held is None else mp.mpf(held)

    def profile(b):
        return n * mp.log(a(b)) + b * sum(x) - a(b) * integral(b)

    score = lambda b: mp.diff(profile, b)
    b = mp.findroot(score, (mp.mpf('1e-20'), mp.mpf('1e-15')), tol=mp.mpf('1e-80'))
    assert score(b * (1 - mp.mpf('5e-11'))) > 0 > score(b * (1 + mp.mpf('5e-11')))
    print('Gompertz', 'free' if held is None else f'a={held}')
    for name, value in [('score at zero', score_at_zero), ('a', a(b)),
                        ('b', b), ('logLik', profile(b))]:
        print(f'  {name} = {mp.nstr(value, 65)}  [{float(value).hex()}]')


def held_pad_check():
    """Exact fractions as oracle, including an emulated single-rounding FMA."""
    f = Fraction
    u = f(1, 2**53)
    tiny = f(1, 2**1074)

    def add(a, b):
        s = a + b
        c = s - a
        return s, (a - (s - c)) + (b - c)

    def product(a, b):
        p = a * b
        return p, float(f(a) * f(b) - f(p))

    rng = random.Random(21443)
    tested = decided = 0
    worst = f(0)
    for n in (1, 2, 3, 5, 12, 32, 128):
        for _ in range(160):
            x = [rng.random()] + [math.ldexp(rng.random(), -rng.randrange(1080))
                                  if rng.randrange(3) == 0 else rng.random()
                                  for _ in range(n-1)]
            exponent = math.frexp(max(x))[1]
            z = [f(v) * f(2)**(-exponent) for v in x]
            exact_s, exact_q = sum(z), sum(v*v for v in z)
            sh = sl = qh = ql = 0.
            for v in z:
                zv = float(v)
                sh, err = add(sh, zv)
                sl += err
                p, pe = product(zv, zv)
                qh, err = add(qh, p)
                ql += err
                ql += pe
            center = float(2 * exact_s / exact_q)
            for alpha in (math.nextafter(center, 0), center, math.nextafter(center, math.inf)):
                half = alpha / 2
                p, pe = product(half, qh)
                d, de = add(sh, -p)
                low_q = half * ql
                t = ((de - pe) + sl) - low_q
                value = d + t
                w = sum(map(abs, (de, pe, sl, low_q)))
                gs = n * float(u) / (1 - n * float(u))
                gq = 2 * n * float(u) / (1 - 2 * n * float(u))
                pad = (2 * (gs*gs*sh + gq*gq*p) + 16 * float(u) * w
                       + 16 * n * (1 + alpha) * float(tiny)) * (1 + 1e-6)
                exact = exact_s - f(alpha)/2 * exact_q
                worst = max(worst, abs(f(value) - exact) / f(pad))
                if abs(value) > pad:
                    assert (value > 0) == (exact > 0) and exact != 0
                    decided += 1
                tested += 1
    assert worst <= 1
    print(f'Held pad: {tested} comparisons, {decided} decisive, max error/pad = {float(worst):.6g}')


def slash():
    def gain(other, delta, sigma):
        t = delta**2 / (2 * sigma**2)
        core = 0 if t == 0 else mp.log(-mp.expm1(-t) / t)
        return 2 * core + sum(
            -2 * mp.log1p(-delta / d) + mp.log1p(-mp.exp(-(d-delta)**2/(2*sigma**2)))
            for d in other)

    for sigma in map(mp.mpf, ('.1', '.05', '.025')):
        for delta in (mp.mpf(0), sigma**2, sigma / 2, sigma):
            observed = gain((-1, 1), delta, sigma)
            assert observed < 0
            if 0 < abs(delta) <= sigma:
                assert observed < (8 - 2/(8*sigma**2))*delta**2
            print(f'Slash symmetric s={sigma}, delta={delta}: log L/B = {mp.nstr(observed, 60)}')
    checked = 0
    for sigma in map(mp.mpf, ('.01', '.05', '.2', '.5', '.7', '2')):
        for delta in map(mp.mpf, ('0', '.01', '.2', '.5', '.9', '.999')):
            d, v = delta**2, 4 * sigma**2
            if v <= 1 - d:
                upper = mp.mpf(0)
            else:
                w = v - (1 - d)
                excess = 4*d*(1-d) + 4*w + (9-4*d)*w**2 + 6*w**3 + w**4
                upper = -mp.log1p(excess / (4 * v**2))
            observed = gain((-1, 1), delta, sigma)
            assert observed < upper <= 0
            checked += 1
    print(f'Slash symmetric global bound: {checked} interior evaluations below the bound')
    other = (-1, mp.mpf(1.0001))
    observed = gain(other, mp.mpf('-0.00000427'), mp.mpf('.14'))
    assert 0 < observed < mp.mpf('4e-8')
    print(f'Slash asymmetric finite witness: log L/B = {mp.nstr(observed, 65)}')


if __name__ == '__main__':
    gompertz([415.8753190533696, 2688.353999767103, 240.47409880682352])
    gompertz([1, 2], 1.2)
    held_pad_check()
    slash()
