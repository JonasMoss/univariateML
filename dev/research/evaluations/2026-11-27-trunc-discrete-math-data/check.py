"""Replay algebraic witnesses and 80-digit boundary spot checks."""
import json
from fractions import Fraction as F
from math import comb
from pathlib import Path

import mpmath as mp
import sympy as sp

mp.mp.dps = 80
records = []


def normalize(weights):
    total = sum(weights)
    return [weight / total for weight in weights]


def distances(label, target, values, tolerance):
    errors = []
    for point, weights in values:
        error = max(abs(a - b) for a, b in zip(normalize(weights), target, strict=True))
        errors.append(error)
        records.append({"case": label, "grid": str(point), "sup_error": mp.nstr(error, 22)})
    assert errors[-1] < mp.mpf(tolerance), (label, errors)
    assert all(x > y for x, y in zip(errors, errors[1:])), (label, errors)


def hyper(population, draws, marked, cells):
    return normalize([F(comb(marked, k) * comb(population - marked, draws - k))
                      for k in cells])


def nhyper(population, marked, stop, cells):
    return normalize([F(comb(k - 1, stop - 1) * comb(population - k, marked - stop))
                      for k in cells])


def exact():
    assert hyper(4, 3, 2, [1, 2]) == hyper(4, 2, 3, [1, 2]) == [F(1, 2)] * 2
    for l in range(5):
        for draws in (None, l + 1, l + 3):
            for target in (F(1, 7), F(1), F(11, 3)):
                q = 1 if draws is None else draws - l
                p = (l + 1) * target.numerator
                d = q * target.denominator
                law = hyper(d + p + q + l - 1, l + q, l + p, [l, l + 1])
                assert law[1] / law[0] == target
    for population in (2, 3, 25):
        assert nhyper(population, 1, 1, [1, 2]) == [F(1, 2)] * 2
    for l in range(1, 6):
        for r in range(1, l + 1):
            for h in (F(1, 7), F(5, 8), F(1)):
                population = l + h.denominator
                marked = r + h.denominator - h.numerator
                law = nhyper(population, marked, r, [l, l + 1])
                assert law[1] / law[0] == h * F(l, l - r + 1)
    counts = [F(1), F(3, 5), F(2, 5)]
    for r, a, b in [(1, 1, 2), (2, 1, 1)]:
        for k in (1, 2):
            assert F((k+r)*(k+b), (k+1)*(k+a+b+r)) == counts[k] / counts[k-1]
    for a in (F(1, 2), F(1), F(3)):
        b = (a + 1) / 2
        assert (a + 1) / (2 * b) == 1
    q = (sp.sqrt(3) - 1) / 2
    prob = 1 - q
    assert sp.simplify(prob**2 / (1 - q**2) - 3 * prob**2 * q / (1 - q**3)) == 0
    assert 2*q < 1
    assert normalize([F(1), F(1, 2)]) == [F(2, 3), F(1, 3)]
    for z in (F(4), F(5), F(6)):
        v = 2 / (1 + 1/z)
        assert 0 < v < z/2 and v * (1 + 1/z)/2 == 1


def numerical():
    cells = range(2, 8)
    grid = (100, 1000, 10000, 100000)
    distances("bbinom growing-size NB", normalize([mp.rf(2, k) * mp.mpf("0.4")**k
                                                    / mp.factorial(k) for k in cells]),
              [(n, [mp.binomial(n, k) * mp.beta(k + 2, n - k + mp.mpf("1.5")*n)
                      / mp.beta(2, mp.mpf("1.5")*n) for k in cells]) for n in grid],
              "0.00003")
    distances("bnbinom growing-shape Poisson",
              normalize([mp.mpf("1.5")**k / mp.factorial(k) for k in cells]),
              [(n, [mp.rf(n, k)**2 / (mp.factorial(k) * mp.rf(mp.mpf(n*n)/mp.mpf("1.5"), k))
                      for k in cells]) for n in grid], "0.00004")
    distances("hyper growing-population Poisson",
              normalize([mp.mpf(2)**k / mp.factorial(k) for k in cells]),
              [(n, [mp.binomial(n, k) * mp.binomial(n*n//2 - n, n-k)
                      for k in cells]) for n in grid], "0.00003")
    distances("nhyper growing-population shifted NB",
              normalize([mp.binomial(k-1, 1)*mp.mpf("0.6")**k for k in cells]),
              [(n, [mp.binomial(k-1, 1)*mp.binomial(n-k, 2*n//5-2)
                      for k in cells]) for n in grid], "0.00003")
    tiny = [mp.mpf(10)**(-i) for i in (2, 4, 8, 16)]
    eta = mp.mpf("-0.45")
    distances("dnorm broad geometric",
              normalize([mp.exp(eta*k) for k in cells]),
              [(g, [mp.quad(lambda x: mp.exp(eta*x - g*x*x/2), [k,k+1])
                     for k in cells]) for g in tiny], "1e-15")
    distances("dweibull small-shape discretised Pareto",
              normalize([1/mp.mpf(k)-1/mp.mpf(k+1) for k in cells]),
              [(b, [mp.exp(-mp.expm1(b*mp.log(k))/b) -
                    mp.exp(-mp.expm1(b*mp.log(k+1))/b) for k in cells]) for b in tiny], "1e-15")
    dist_cells = range(-2, 4)
    distances("skellam large-product geometric",
              normalize([mp.exp(eta*k) for k in dist_cells]),
              [(z, [mp.exp(eta*k-z)*mp.besseli(abs(k), z)
                     for k in dist_cells]) for z in grid], "0.00002")
    rho = mp.mpf("0.7")
    tilt = rho*(2-rho)

    def pig_weight(k, z):
        poly = sum(mp.factorial(k-1+j) /
                   (mp.factorial(j)*mp.factorial(k-1-j)*(2*z)**j)
                   for j in range(k))
        return (z*tilt/2)**k * poly / mp.factorial(k)

    distances("pig rare tilted Sibuya",
              normalize([(-1)**(k+1)*mp.binomial(mp.mpf("0.5"), k)*tilt**k
                         for k in cells]),
              [(z, [pig_weight(k, z) for k in cells]) for z in tiny], "1e-15")
    distances("yulesimon zero-shape harmonic",
              normalize([mp.mpf(1)/k for k in cells]),
              [(r, [r*mp.beta(k, r+1) for k in cells]) for r in tiny], "1e-15")


if __name__ == "__main__":
    exact()
    numerical()
    destination = Path(__file__).with_name("checks.json")
    destination.write_text(json.dumps(records, indent=2) + "\n")
    print(f"Exact witness checks passed. {len(records)} numerical comparisons at {mp.mp.dps} digits.")
