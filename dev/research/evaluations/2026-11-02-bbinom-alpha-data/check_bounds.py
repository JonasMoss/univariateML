"""Seeded numerical checks of the exact-arithmetic bounds in the dossier."""
import math
import random

random.seed(113)
worst_small = -math.inf
worst_large = -math.inf
for _ in range(5000):
    h = random.randint(2, 12)
    M = random.randint(1, 20)
    x = [random.randint(0, M) for _ in range(h)] + [M]
    h = len(x)
    N = random.randint(M + 1, 250)
    a = math.exp(random.uniform(-3, 3))
    c = math.exp(random.uniform(-4, 2))
    b = c * N * math.exp(random.uniform(-3, 3))
    S = sum(x)
    coeff = sum(sum(math.log((a + j) / (j + 1)) for j in range(v)) for v in x)
    ll = sum(math.lgamma(N + 1) - math.lgamma(v + 1) - math.lgamma(N - v + 1)
             + math.lgamma(a + v) - math.lgamma(a)
             + math.lgamma(b + N - v) - math.lgamma(b)
             - math.lgamma(a + b + N) + math.lgamma(a + b) for v in x)
    mu = S / h
    face = coeff + S * math.log(mu / (a + mu)) - h * a * math.log1p(mu / a)
    small = coeff + S * math.log(N / (N - M)) + h * a * math.log((a + c*N)/(a+(1+c)*N))
    large = face + sum(v*(v+1)/2 for v in x)/(N-M) + h*a*a/2*(1/(c*N) + 1/(c*N)**2)
    if b <= c*N:
        worst_small = max(worst_small, ll-small)
        assert ll <= small + 1e-8
    else:
        worst_large = max(worst_large, ll-large)
        assert ll <= large + 1e-8
print('5000 cases; max log likelihood minus upper bound:', worst_small, worst_large)
