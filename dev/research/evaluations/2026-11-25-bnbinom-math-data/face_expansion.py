"""Check the Poisson corner expansion and uniform error on exact sample grids."""
import json
from fractions import Fraction as Q

import mpmath as mp
import sympy as sp


def symbolic_check():
    u, v, rate, mean, f2, f3 = sp.symbols("u v rate mean f2 f3")
    h = u + v
    first = rate**2 * h / 2 + f2 * h / 2 - mean * rate * h
    assert sp.expand(first - h * ((rate - mean)**2 + f2 - mean**2) / 2) == 0
    second = (
        -rate**2 * u * v / 2
        -rate**3 * ((u**2 + v**2) / 3 + u * v / 2)
        -(u**2 + v**2) * (f3 / 3 + f2 / 2) / 2
        -rate * u * v * f2 / 2
        +mean * rate**2 * h**2 / 2
    )
    balanced = ((mean**3 - f3) / 6 - mean**2 / 4) * (u**2 + v**2) - mean**2 * u * v / 2
    assert sp.expand(second.subs({rate: mean, f2: mean**2}) - balanced) == 0
    return str(second)


def mpq(x):
    return mp.mpf(x.numerator) / x.denominator


def expansion_bound(rate, counts):
    # Per observation coefficient of h^3 for h <= 1.
    return (
        rate**4 + rate**3 / 4 + rate**3 / 96 + mp.fsum(
            mp.fsum(
                mp.mpf(j)**3 / 3 + rate**3 * (1 + mp.mpf(j) / 4)**3 / 3
                + j * rate**2 / 4 + j*j * rate**2 / 32
                for j in range(x)
            ) for x in counts
        ) / len(counts)
    )


def main():
    mp.mp.dps = 90
    symbolic = symbolic_check()
    checked = 0
    maximum_error_ratio = mp.mpf(0)
    records = []
    for sample in [(0, 2), (0, 1, 2, 3, 4), (0, 3, 3), (0, 0, 1, 7)]:
        mean = Q(sum(sample), len(sample))
        f2 = Q(sum(x * (x - 1) for x in sample), len(sample))
        f3 = Q(sum(x * (x - 1) * (x - 2) for x in sample), len(sample))
        assert mean * (f3 + f2) >= f2**2
        equidispersed = f2 == mean**2
        if equidispersed:
            assert f3 >= mean**3 - mean**2
        records.append({"sample": sample, "mean": str(mean), "equidispersed": equidispersed})
        for rate_q in (mean - Q(1, 10), mean, mean + Q(1, 10)):
            rate = mpq(rate_q)
            for h in (mp.mpf("0.01"), mp.mpf("0.0001"), mp.mpf("1e-8")):
                for fraction in map(mpq, (Q(1, 100), Q(1, 5), Q(1, 2))):
                    u, v = fraction * h, (1 - fraction) * h
                    r, b, a = 1/u, 1/v, 1/(rate*u*v)
                    logp0 = mp.loggamma(a+r) + mp.loggamma(a+b) - mp.loggamma(a) - mp.loggamma(a+r+b)
                    ratio = mp.fsum(
                        logp0 + rate + mp.fsum(
                            mp.log1p(j*u) + mp.log1p(j*v) - mp.log1p(rate*h+j*rate*u*v)
                            for j in range(x)
                        ) for x in sample
                    ) / len(sample)
                    first = h * ((rate-mpq(mean))**2 + mpq(f2-mean**2)) / 2
                    second = (
                        -rate**2*u*v/2 - rate**3*((u*u+v*v)/3+u*v/2)
                        -(u*u+v*v)*mpq(f3/3+f2/2)/2 - rate*u*v*mpq(f2)/2
                        +mpq(mean)*rate**2*h*h/2
                    )
                    if equidispersed and rate_q == mean:
                        bound = -rate**2*((u*u+v*v)/12+u*v/2)
                        assert second <= bound + abs(bound)*mp.mpf("1e-80")
                        if h <= mp.mpf("0.0001"):
                            assert ratio < 0
                    error_ratio = abs(ratio-first-second)/(h**3*expansion_bound(rate, sample))
                    assert error_ratio < 1
                    maximum_error_ratio = max(maximum_error_ratio, error_ratio)
                    checked += 1
    print(json.dumps({"dps": mp.mp.dps, "grid_cases": checked, "samples": records,
                      "second_order_polynomial": symbolic,
                      "maximum_error_bound_fraction": mp.nstr(maximum_error_ratio, 12)}, indent=2))


if __name__ == "__main__":
    main()
