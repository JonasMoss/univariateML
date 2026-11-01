#!/usr/bin/env python3
"""Check the finite-truncation inequality at sampled parameters, not its proof."""
import math
import random

random.seed(1142026)

def logsumexp(xs):
    m = max(xs)
    return m + math.log(sum(math.exp(x - m) for x in xs))

worst = -math.inf
superlevel_checked = 0
for _ in range(10000):
    n = random.randint(5, 300)
    kmax = random.randint(2, n)
    x = [random.randint(0, kmax) for _ in range(20)]
    nu = random.uniform(-3, 3)
    a = random.uniform(-4, 4)
    base = [j * a - nu * math.lgamma(j + 1) for j in range(n + 1)]
    cn = [0.0]
    for j in range(n):
        cn.append(cn[-1] + math.log1p(-j / n))
    actual = sum(base[j] + nu * cn[j] for j in x) - len(x) * logsumexp(
        [base[j] + nu * cn[j] for j in range(n + 1)])
    truncated = sum(base[j] for j in x) - len(x) * logsumexp(base[:kmax + 1])
    correction = nu * sum(cn[j] for j in x)
    if nu > 0:
        correction -= len(x) * nu * cn[kmax]
    worst = max(worst, actual - truncated - correction)
    assert actual <= truncated + correction + 1e-9
    u, v = min(x), max(x)
    if v - u >= 2 and v < n:
        bound = actual - 1.0
        w = (v - u - 1) / (v - u)
        delta = (w * math.lgamma(u + 1) + (1 - w) * math.lgamma(v + 1)
                 - math.lgamma(u + 2))
        if nu > 0:
            assert nu <= -bound * (w / x.count(u) + (1 - w) / x.count(v)) / delta + 1e-9
        else:
            assert nu >= bound / (x.count(v) * math.log(n)) - 1e-9
        superlevel_checked += 1
print('seed=1142026 cases=10000 max_residual=%.12g superlevel_checked=%d' %
      (worst, superlevel_checked))
