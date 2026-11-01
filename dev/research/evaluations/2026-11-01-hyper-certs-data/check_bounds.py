#!/usr/bin/env python3
"""Seeded checks of pointwise tail inequalities; not a proof or fitter test."""
import math
import random

rng = random.Random(121)
def lc(n, k):
    return sum(math.log1p(-j/n) for j in range(k))
def choose(n, k):
    return math.lgamma(n+1)-math.lgamma(k+1)-math.lgamma(n-k+1)

worst_h = worst_n = -math.inf
cases = 0
for _ in range(10000):
    N = rng.randint(10, 1000)
    m, k = rng.randint(1, N-1), rng.randint(1, min(N-1, 30))
    lo, hi = max(0, k-N+m), min(k, m)
    x = rng.randint(lo, hi)
    p = m/N
    h = choose(m,x)+choose(N-m,k-x)-choose(N,k)
    b = choose(k,x)+x*math.log(p)+(k-x)*math.log1p(-p)
    worst_h = max(worst_h, h-b+lc(N,k))
    r = rng.randint(1, min(m, 8))
    x = rng.randint(r, min(N-m+r, r+30))
    n = choose(x-1,r-1)+choose(N-x,m-r)-choose(N,m)
    nb = choose(x-1,r-1)+r*math.log(p)+(x-r)*math.log1p(-p)
    worst_n = max(worst_n, n-nb+lc(N,x))
    cases += 1
assert worst_h < 1e-9 and worst_n < 1e-9, (worst_h, worst_n)
print(f'seed=121 cases={cases} hyper_max_residual={worst_h:.12g} nhyper_max_residual={worst_n:.12g}')
