#!/usr/bin/env python3
"""Seeded checks of the fully free hypergeometric Poisson envelope.

Checks the elementary pointwise inequality, not the search or rounding proof.
Run: python3 dev/research/evaluations/2026-11-01-hyper-free-data/check_envelope.py
"""
import math
import random

rng = random.Random(121)
worst = -math.inf
cases = 0
for _ in range(100000):
    n = rng.randint(2, 2000)
    m, k = rng.randint(1, n), rng.randint(1, n)
    lo, hi = max(0, m + k - n), min(m, k)
    x = rng.randint(lo, hi)
    if x == n:
        continue
    ll = (math.lgamma(m + 1) - math.lgamma(x + 1) - math.lgamma(m - x + 1)
          + math.lgamma(n - m + 1) - math.lgamma(k - x + 1)
          - math.lgamma(n - m - k + x + 1)
          - math.lgamma(n + 1) + math.lgamma(k + 1)
          + math.lgamma(n - k + 1))
    lam = m * k / n
    bound = (x * math.log(lam) - math.lgamma(x + 1)
             - lam * (1 - x / m) * (1 - x / k)
             - x * math.log1p(-x / n))
    worst = max(worst, ll - bound)
    cases += 1
    assert ll <= bound + 2e-10, (n, m, k, x, ll, bound)
print(f"seed=121 cases={cases} largest_residual={worst:.12g} tolerance=2e-10")
