#!/usr/bin/env python3
"""betapr (shape1 p, shape2 q, scale b all free) on the near-constant class, at 200 digits.

Reduction: for fixed b the score in b is  sum_i [N x_i/(x_i+b) - p] = 0 with N = p+q, i.e. the mean of
x_i/(x_i+b) equals rho = p/N.  So (p, q, b) <-> (N, b) with rho = rho(b).  For each b the profile over
N is a one-dimensional problem; the profile in b (equivalently rho) is then scanned from the gamma face
(rho -> 0, b -> infinity) to the inverse-gamma face (rho -> 1, b -> 0).
"""
import mpmath as mp
from common import *
mp.mp.dps = 200
x, held = sample('betapr'); n = 3
L = [mp.log(v) for v in x]
d = delta(x)
print('data', [float(t).hex() for t in x], 'delta =', mp.nstr(d, 12), ' held', held)

def rho_of(b): return mp.fsum(v/(v+b) for v in x)/n
def b_of(rho):      # solve mean x/(x+b) = rho
    b = x[0]*(1-rho)/rho
    return mp.findroot(lambda t: rho_of(t)-rho, b, tol=mp.mpf(10)**-150, maxsteps=100)
def ll(p, q, b):
    N = p+q
    return mp.fsum((p-1)*(l-mp.log(b)) - mp.log(b) - N*mp.log1p(v/b) for l, v in zip(L, x)) + n*(mp.loggamma(N)-mp.loggamma(p)-mp.loggamma(q))

def best_N(rho, b):
    f = lambda lN: ll(rho*mp.exp(lN), (1-rho)*mp.exp(lN), b)
    grid = [mp.mpf(k)/4*mp.log(10) for k in range(4*20, 4*75)]
    g0 = max(grid, key=f)
    lo, hi = g0-mp.log(10)/4, g0+mp.log(10)/4
    for _ in range(300):
        m1, m2 = lo+(hi-lo)*(1-mp.mpf(0.618033988749895)), lo+(hi-lo)*mp.mpf(0.618033988749895)
        if f(m1) < f(m2): lo = m1
        else: hi = m2
    lN = (lo+hi)/2
    # polish with Newton on the derivative
    try:
        lN = mp.findroot(lambda t: mp.diff(f, t), lN, tol=mp.mpf(10)**-60, maxsteps=50)
    except Exception:
        pass
    return mp.exp(lN), f(lN)

# gamma / inverse-gamma limits (exact maxima of the limit models on the same data)
def gamma_ll(y):
    m = mp.fsum(y)/n; ml = mp.fsum(mp.log(v) for v in y)/n
    c = mp.log(m)-ml
    k = mp.findroot(lambda t: mp.log(t)-mp.psi(0, t)-c, 1/(2*c), tol=mp.mpf(10)**-120)
    th = m/k
    return k, th, mp.fsum(-mp.loggamma(k)-k*mp.log(th)+(k-1)*mp.log(v)-v/th for v in y)
kg, thg, Lg = gamma_ll(x)
ki, thi, Li = gamma_ll([1/v for v in x])
Li = gamma_ll([1/v for v in x])[2] - 2*mp.fsum(L)   # f_x(x) = f_y(1/x)/x^2
scale_i = 1/thi                       # inverse-gamma scale (rate of 1/x is 1/thi)
print('gamma  limit: shape %s scale %s  loglik %s' % (mp.nstr(kg, 12), mp.nstr(thg, 12), mp.nstr(Lg, 30)))
print('invgam limit: shape %s  loglik %s' % (mp.nstr(ki, 12), mp.nstr(Li, 30)))
print('inverse-gamma scale', mp.nstr(scale_i, 12), ' representable:', representable(ki), representable(scale_i), representable(kg), representable(thg))
print('Li - Lg =', mp.nstr(Li-Lg, 15), ' delta/3 =', mp.nstr(d/3, 15), ' ratio =', mp.nstr((Li-Lg)/(d/3), 12))

rows = []
for rho in [mp.mpf(10)**(-k) for k in (1, 3, 6, 10, 14, 18, 22)] + [mp.mpf(1)/2] + [1-mp.mpf(10)**(-k) for k in (22, 18, 14, 10, 6, 3, 1)]:
    b = b_of(rho)
    N, v = best_N(rho, b)
    rows.append((rho, b, N, v))
    print('rho = %-14s b = %-12s N = %-12s  loglik %s   minus gamma-limit %s   minus invgamma-limit %s'
          % (mp.nstr(rho, 6) if rho < .5 else '1-'+mp.nstr(1-rho, 6) if rho > .5 else '0.5', mp.nstr(b, 6), mp.nstr(N, 6), mp.nstr(v, 25), mp.nstr(v-Lg, 8), mp.nstr(v-Li, 8)))

print()
print('Lambda(rho) = Lg + rho*(Li-Lg)?  relative deviation of (Lambda-Lg)/(Li-Lg) from rho:')
for rho, b, N, v in rows:
    print('  rho %s: (Lambda-Lg)/(Li-Lg) - rho = %s' % (mp.nstr(rho, 8), mp.nstr((v-Lg)/(Li-Lg)-rho, 6)))
print('p, q at each row (p = rho N, q = (1-rho) N):')
for rho, b, N, v in rows:
    print('  rho %s: p = %s q = %s' % (mp.nstr(rho, 8), mp.nstr(rho*N, 6), mp.nstr((1-rho)*N, 6)))
