#!/usr/bin/env python3
"""Check held-beta likelihood bound and its sharp asymptotic path (mpmath)."""
import math
import random
import mpmath as mp

mp.mp.dps = 60
rng = random.Random(113)
worst = -math.inf
for _ in range(3000):
    n = rng.randrange(1, 11)
    xs = [rng.randrange(0, 9) for _ in range(n)]
    if max(xs) == 0:
        xs[0] = 1
    N = rng.randrange(2 * max(xs), 2001)
    a = mp.exp(rng.uniform(-7, 5))
    b = mp.exp(rng.uniform(-4, 4))
    h = mp.digamma(b + 1 + N) - mp.digamma(b + 1)
    m = sum(x > 0 for x in xs)
    ll = sum(mp.loggamma(N + 1) - mp.loggamma(x + 1) - mp.loggamma(N - x + 1)
             + mp.loggamma(a + x) + mp.loggamma(b + N - x)
             - mp.loggamma(a + b + N) - mp.loggamma(a) - mp.loggamma(b)
             + mp.loggamma(a + b) for x in xs)
    low = sum(xs) * mp.log(2) + m * mp.log(m / (mp.e * n * h))
    high = sum(mp.log(mp.power(2, x + 1) - 1) for x in xs if x) - n * h
    upper = max(low, high)
    residual = float(ll - upper)
    worst = max(worst, residual)
    assert residual <= 1e-35, (xs, N, a, b, residual)
print(f'3000 random cases passed; max log likelihood minus bound = {worst:.8g}')
xs = [0, 0, 1, 2, 4]
n, m, t, b = len(xs), 3, mp.mpf('1.7'), mp.mpf('3')
target = mp.exp(-n*t) * t**m / math.prod(x for x in xs if x)
for exponent in (5, 10, 20, 40):
    # Stable rising factorial products avoid subtracting enormous log-gammas.
    N = mp.power(10, exponent)
    a = t / mp.log(N)
    p0 = mp.exp(mp.loggamma(b + N) - mp.loggamma(a + b + N)
                + mp.loggamma(a + b) - mp.loggamma(b))
    ll = n * mp.log(p0)
    for x in xs:
        for j in range(x):
            ll += mp.log((N-j)*(a+j)/((j+1)*(b+N-1-j)))
    ratio = mp.exp(ll) * mp.log(N)**m / target
    print(f'N=1e{exponent}: scaled likelihood / limit = {mp.nstr(ratio, 10)}')
