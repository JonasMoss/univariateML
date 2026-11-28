"""Replay exact and high-precision witnesses on the estimated windows."""
from fractions import Fraction as Q
import csv
from pathlib import Path

import mpmath as mp
import sympy as sp

OUT = Path(__file__).resolve().parent


def exact_checks():
    assert [Q(2, 3), Q(1, 3)] == [1 / k / (1 + Q(1, 2)) for k in (Q(1), Q(2))]
    assert (Q(1) + Q(2)) / 2 > Q(4, 3)
    assert Q(1, 2) / (1 + Q(1, 2)) < 1
    for alpha, beta in [(1, 1), (3, 2)]:
        assert Q(alpha + 1, 2 * beta) == 1
    for size, alpha, beta in [(1, 1, 2), (2, 1, 1)]:
        assert [
            Q((k + size) * (k + beta), (k + 1) * (k + alpha + beta + size))
            for k in (1, 2)
        ] == [Q(3, 5), Q(2, 3)]
    rate = 1 / Q(1, 2)
    assert 2 / rate**2 == Q(1, 2)  # Half-normal edge and empirical second moments.
    c, r = sp.symbols("c r", positive=True)
    k, j = sp.symbols("k j", positive=True)
    score = (j-k)*(1/(1+c)-1/(r+c))
    assert sp.simplify(score-(j-k)*(r-1)/((1+c)*(r+c))) == 0
    # Conditional equality of uniform windows under two different clippings.
    for lo, hi, a, b in [(0, 1, -2, 3), (-2, 3, 0, 1), (-1, 4, 0, 1)]:
        assert min(hi, b) - max(lo, a) == 1


def numerical(precision):
    with mp.workdps(precision):
        for r in map(mp.mpf, ("0.125", "1", "2", "8")):
            for c in map(mp.mpf, ("0.125", "1", "8")):
                u = mp.exp(r)
                def q(x):
                    y = mp.exp(r*x)
                    return r * y * (1+c)*(u+c)/((u-1)*(y+c)**2)
                assert mp.almosteq(mp.quad(q, [0, 1]), 1)
                assert mp.almosteq(q(0)*q(1), (r/(2*mp.sinh(r/2)))**2)
                assert q(0)*q(1) < 1
        values = []
        for eps in map(mp.mpf, ("0.1", "0.01", "0.001")):
            z = mp.quad(lambda x: mp.exp(-x*x/2), [0, eps])
            free_location = eps * mp.exp(-eps*eps/2) / z
            held_location = eps / z
            assert free_location < 1 < held_location
            values.append((mp.nstr(free_location, 32), mp.nstr(held_location, 32)))
        return values


if __name__ == "__main__":
    exact_checks()
    low, high = numerical(70), numerical(100)
    assert low == high
    with (OUT / "checks.csv").open("w", newline="") as out:
        writer = csv.writer(out)
        writer.writerow(["epsilon", "width_times_density_free_location",
                         "width_times_density_held_location", "decimal_precisions"])
        writer.writerows((eps, *pair, "70 and 100")
                         for eps, pair in zip(("0.1", "0.01", "0.001"), high))
    print("PASS: exact witnesses and 70-/100-digit replays agree")
