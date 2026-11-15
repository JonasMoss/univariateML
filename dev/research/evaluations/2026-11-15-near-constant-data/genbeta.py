#!/usr/bin/env python3
"""genbeta, scale = 1 held, (shape1 p, shape2 q, shape3 a) free, on the near-constant class, 200 digits.

log f = log a - log x - lB(p,q) + p a log x + (q-1) log(1 - x^a)
For fixed a, with u = x^a this is a beta(p+1, q)-type exponential family in (p, q): the log-likelihood is
concave in (p, q) with scores  psi(p+q) - psi(p) + mean(a log x) = 0  and  psi(p+q) - psi(q) + mean log(1-u) = 0.
The profile in a is scanned from small a (log-gamma face a -> 0) to large a.
"""
import sys
import mpmath as mp
from common import *
mp.mp.dps = 200
x, held = sample('genbeta'); n = 3
Lx = [mp.log(v) for v in x]
d = delta(x); ell = mp.log(x[2]/x[0])
print('data', [float(t).hex() for t in x], 'held', held, 'delta =', mp.nstr(d, 12))

def pq_hat(a, start=None):
    u = [mp.exp(a*l) for l in Lx]
    T1 = mp.fsum(a*l for l in Lx)/n
    T2 = mp.fsum(mp.log1p(-v) for v in u)/n
    m = mp.fsum(u)/n; var = mp.fsum((v-m)**2 for v in u)/n
    if start is None:
        N0 = m*(1-m)/var; start = (m*N0, (1-m)*N0)
    F = lambda lp, lq: (mp.psi(0, mp.exp(lp)+mp.exp(lq)) - mp.psi(0, mp.exp(lp)) + T1,
                        mp.psi(0, mp.exp(lp)+mp.exp(lq)) - mp.psi(0, mp.exp(lq)) + T2)
    sol = mp.findroot(F, (mp.log(start[0]), mp.log(start[1])), tol=mp.mpf(10)**-120, maxsteps=200, solver='mdnewton')
    return mp.exp(sol[0]), mp.exp(sol[1])

def ll(p, q, a):
    return mp.fsum(mp.log(a) - l + mp.loggamma(p+q) - mp.loggamma(p) - mp.loggamma(q) + p*a*l + (q-1)*mp.log1p(-mp.exp(a*l)) for l in Lx)

if __name__ == '__main__':
    rows = []
    base = None
    for a in [mp.mpf(s) for s in ['0.001', '0.01', '0.1', '0.3', '0.5', '1', '1.5', '2', '3', '5', '10', '20', '50']]:
        try:
            p, q = pq_hat(a)
            v = ll(p, q, a)
        except Exception as e:
            print('a =', a, 'failed', str(e)[:100]); continue
        rows.append((a, p, q, v))
        if base is None: base = v
        print('a = %-6s p = %-12s q = %-12s loglik = %s  (loglik - loglik(first))/delta = %s' % (mp.nstr(a, 5), mp.nstr(p, 6), mp.nstr(q, 6), mp.nstr(v, 22), mp.nstr((v-base)/d, 8)))
        sys.stdout.flush()
