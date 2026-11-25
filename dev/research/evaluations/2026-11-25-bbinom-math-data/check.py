"""Deterministic high-precision checks of the fixed-size existence proof."""

from fractions import Fraction
from itertools import product
from math import comb

import mpmath as mp


def compositions(total, parts):
    if parts == 1:
        yield (total,)
        return
    for first in range(total + 1):
        for rest in compositions(total - first, parts - 1):
            yield (first, *rest)


def log_products(shape, m):
    values = [mp.mpf(0)]
    for j in range(m):
        values.append(values[-1] + mp.log(shape + j))
    return values


def f(shape, length):
    return (mp.loggamma(shape + length) - mp.loggamma(shape)) / length


def check_scaling():
    count = 0
    for m, u, a in product(
        (2, 3, 4, 10, 100, 1000),
        (mp.mpf("0.000001"), mp.mpf("0.01"), mp.mpf("0.2"),
         mp.mpf("0.5"), mp.mpf("0.9"), mp.mpf("0.999999")),
        (mp.mpf(10) ** k for k in (-12, -3, 0, 3, 12)),
    ):
        assert f(a, 1 + (m - 1) * u) < mp.log(u) + f(a / u, m)
        count += 1
    return count


def check_table_grid():
    shapes = tuple(mp.mpf(10) ** k for k in (-8, -3, 0, 3, 8))
    totals = {"strict": 0, "equal": 0, "comparisons": 0}
    largest = {"strict": mp.ninf, "equal": mp.ninf}
    for m in range(2, 9):
        likelihoods = []
        for a, b in product(shapes, repeat=2):
            la = log_products(a, m)
            lb = log_products(b, m)
            den = log_products(a + b, m)[m]
            likelihoods.append([la[x] + lb[m - x] - den for x in range(m + 1)])
        for n in range(1, 9):
            for counts in compositions(n, m + 1):
                if not any(counts[1:-1]):
                    continue
                s = sum(x * c for x, c in enumerate(counts))
                q = sum(x * x * c for x, c in enumerate(counts))
                delta = m * n * q - (m - 1) * s * s - m * n * s
                if delta > 0:
                    continue
                group = "equal" if delta == 0 else "strict"
                totals[group] += 1
                u = mp.mpf(s) / (m * n)
                face = s * mp.log(u) + (m * n - s) * mp.log1p(-u)
                for reduced_log_pmf in likelihoods:
                    gap = mp.fdot(counts, reduced_log_pmf) - face
                    assert gap < 0, (m, counts, gap)
                    largest[group] = max(largest[group], gap / n)
                    totals["comparisons"] += 1
    return totals, {key: mp.nstr(value, 12) for key, value in largest.items()}


def check_equal_curvature():
    count = 0
    for m, denominator in product((2, 3, 4, 10, 30), (2, 3, 7)):
        for numerator in range(1, denominator):
            u = Fraction(numerator, denominator)
            v = 1 - u
            weights = [comb(m, x) * u**x * v**(m - x) for x in range(m + 1)]
            # A four-cell perturbation preserves total, mean and second moment.
            signed_change = (-1, 3, -3, 1) if m >= 3 else (0, 0, 0)
            for direction in (-1, 0, 1):
                perturbed = weights.copy()
                step = min(weights) / 4
                for cell, change in enumerate(signed_change):
                    perturbed[cell] += direction * step * change
                assert sum(perturbed) == 1
                assert sum(x * w for x, w in enumerate(perturbed)) == m * u
                assert sum(x * (x - 1) * w for x, w in enumerate(perturbed)) == m * (m - 1) * u * u
                third_x = sum(x * (x - 1) * (x - 2) * w
                              for x, w in enumerate(perturbed))
                third_y = sum(y * (y - 1) * (y - 2) * w
                              for y, w in enumerate(reversed(perturbed)))
                curvature = sum(j * j for j in range(m)) - m * (m - 1)
                curvature -= (third_x / u**2 + third_y / v**2) / 3
                assert curvature <= -Fraction(m * (m - 1), 6)
                u_mp = mp.mpf(u.numerator) / u.denominator
                w_mp = [mp.mpf(w.numerator) / w.denominator for w in perturbed]

                def reduced_loglik(s):
                    return mp.fsum(
                        w * (mp.fsum(mp.log(u_mp + j * s) for j in range(x))
                             + mp.fsum(mp.log(1 - u_mp + j * s) for j in range(m - x)))
                        for x, w in enumerate(w_mp)
                    ) - mp.fsum(mp.log1p(j * s) for j in range(m))

                exact = mp.mpf(curvature.numerator) / curvature.denominator
                assert abs(mp.diff(reduced_loglik, 0, 2) - exact) < mp.mpf("1e-65")
                count += 1
    return count


if __name__ == "__main__":
    mp.mp.dps = 80
    print(f"mpmath {mp.__version__}, {mp.mp.dps} decimal digits", flush=True)
    print(f"Strict scaling checks: {check_scaling()}", flush=True)
    print(f"Equality curvature checks: {check_equal_curvature()}", flush=True)
    totals, largest = check_table_grid()
    print(f"Sample and comparison counts: {totals}", flush=True)
    print(f"Largest log-likelihood difference per observation: {largest}", flush=True)
