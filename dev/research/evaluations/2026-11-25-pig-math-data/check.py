"""Exact outcome fixtures and independent high-precision likelihood checks."""

from fractions import Fraction
from math import factorial, nextafter, inf

import mpmath as mp


def moments(sample):
    return len(sample), sum(sample), sum(k * (k - 1) for k in sample)


def edge(sample, held_mean=None):
    n, total, factorial_sum = moments(sample)
    if held_mean is None:
        return n * factorial_sum - total**2
    a, b = Fraction(held_mean).as_integer_ratio()
    return n * a**2 - 2 * total * a * b + factorial_sum * b**2


def mp_rational(x):
    a, b = Fraction(x).as_integer_ratio()
    return mp.mpf(a) / b


def polynomial(k, y):
    if k <= 1:
        return mp.mpf(1), mp.mpf(0)
    m = k - 1
    terms = [
        mp.mpf(factorial(m + j)) / (factorial(j) * factorial(m - j) * 2**j)
        * y**j
        for j in range(m + 1)
    ]
    return mp.fsum(terms), mp.fsum(j * term for j, term in enumerate(terms))


def shape_score(sample, mean, w):
    n, total, factorial_sum = moments(sample)
    if w == 0:
        return n * mean - 2 * total + factorial_sum / mean
    y = 2 * w / (mean * (1 - w**2))
    degree = mp.fsum(moment / value for value, moment in
                      (polynomial(k, y) for k in sample))
    return n * mean - 2 * total / (1 - w**2) + (
        (1 + w**2) * degree / (w * (1 - w**2))
    )


def bisect_decreasing(score, lo, hi):
    assert score(lo) > 0 > score(hi)
    for _ in range(440):
        mid = (lo + hi) / 2
        if score(mid) > 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def bessel_loglik(sample, mean, shape):
    a = 2 + shape / mean**2
    z = mp.sqrt(shape * a)
    return mp.fsum(
        mp.log(2 * shape / mp.pi) / 2 - mp.loggamma(k + 1)
        + shape / mean + (2 * k - 1) * mp.log(shape / a) / 4
        + mp.log(mp.besselk(mp.mpf(k) - mp.mpf('0.5'), z))
        for k in sample
    )


def poisson_loglik(sample, mean):
    return mp.fsum(-mean + k * mp.log(mean) - mp.loggamma(k + 1)
                   for k in sample)


def shape_fit(sample, mean):
    w = bisect_decreasing(
        lambda w: shape_score(sample, mean, w), mp.mpf(0), mp.mpf('0.999')
    )
    shape = mean**2 * (1 - w)**2 / (2 * w)
    gap = bessel_loglik(sample, mean, shape) - poisson_loglik(sample, mean)
    assert gap > 0
    return w, shape, gap


def held_shape_fit(sample, shape):
    n, total, _ = moments(sample)

    def score(mean):
        t = shape / mean
        z = mp.sqrt(2 * shape + t**2)
        q = t / z
        degree = mp.fsum(moment / value for value, moment in
                          (polynomial(k, 1 / z) for k in sample))
        return 2 * n * shape / (q * (1 + q)) - z * (total + degree)

    average = mp.mpf(total) / n
    mean = bisect_decreasing(lambda value: -score(value), average / 2, 2 * average)
    fitted = bessel_loglik(sample, mean, shape)
    levy_z = mp.sqrt(2 * shape)
    levy = mp.fsum(
        mp.log(2 * shape / mp.pi) / 2 - mp.loggamma(k + 1)
        + (2 * k - 1) * mp.log(shape / 2) / 4
        + mp.log(mp.besselk(mp.mpf(k) - mp.mpf('0.5'), levy_z))
        for k in sample
    )
    assert fitted > levy
    return mean, fitted - levy


def show(values):
    return ' '.join(mp.nstr(value, 16) for value in values)


def main():
    equality = (0,) * 5 + (1, 1, 2, 2)
    rounded = float(Fraction(2, 3))
    lower = nextafter(2.0, 0.0)
    upper = nextafter(2.0, inf)
    exact_cases = [
        ((1,), None, -1), ((0, 2), None, 0), ((0, 3), None, 3),
        ((0, 2), 1, 0), ((0, 2), 2, 2),
        ((1,), lower, -1), ((1,), 2, 0), ((1,), upper, 1),
        (equality, None, 0), (equality, Fraction(2, 3), 0),
        (equality, rounded, 1),
    ]
    for sample, held, expected_sign in exact_cases:
        value = edge(sample, held)
        assert (value > 0) - (value < 0) == (expected_sign > 0) - (expected_sign < 0)
        print('exact', sample, held, value)

    for digits in (150, 200):
        with mp.workdps(digits):
            print('decimal digits', digits)
            rows = {}
            for label, sample, mean in [
                ('free', (0, 3), Fraction(3, 2)),
                ('held', (0, 2), 2),
                ('binary upper neighbor', (1,), upper),
                ('rounded held mean', equality, rounded),
            ]:
                rows[label] = show(shape_fit(sample, mp_rational(mean)))
            for shape in (Fraction(1, 10**8), 1, 10**8):
                rows[f'held shape {shape}'] = show(
                    held_shape_fit((0, 3), mp_rational(shape))
                )
            if digits == 150:
                first = rows
            else:
                assert rows == first
            for label, row in rows.items():
                print(label, row)


if __name__ == '__main__':
    main()
