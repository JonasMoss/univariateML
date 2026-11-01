#!/usr/bin/env python3
"""Numerically check the exact-arithmetic scale bracket; not a proof of rounding."""
import csv
import math
import random
import sys

rng = random.Random(11620261101)
out = csv.writer(sys.stdout)
out.writerow(["sample", "n", "ties", "scale", "lower_score", "upper_score", "root"])
for case in range(600):
    n = (8, 32, 128)[case % 3]
    k = (0, 1, n // 4, (n - 1) // 2)[(case // 3) % 4]
    scale = (1e-100, 1e-5, 1.0, 1e5, 1e100)[(case // 12) % 5]
    residuals = [0.0] * k + [scale * math.exp(rng.uniform(-8, 8)) for _ in range(n-k)]
    d, D = min(residuals[k:]), max(residuals)
    T = 4 * n / (n - 2 * k)
    lo, hi = d / math.sqrt(2 * T), math.sqrt(2) * D

    def score(s):
        total = 0.0
        for v in residuals:
            t = (v / s) ** 2 / 2
            total += 1 if t > 700 else (-1 if t == 0 else 1 - 2*t/math.expm1(t))
        return total

    a, b = score(lo), score(hi)
    assert a > 0 and b < 0, (case, a, b)
    left, right = lo, hi
    for _ in range(180):
        mid = math.sqrt(left) * math.sqrt(right)
        if mid == left or mid == right:
            break
        if score(mid) > 0:
            left = mid
        else:
            right = mid
    root = math.sqrt(left) * math.sqrt(right)
    assert lo < root < hi
    out.writerow([case, n, k, scale, a, b, root])
