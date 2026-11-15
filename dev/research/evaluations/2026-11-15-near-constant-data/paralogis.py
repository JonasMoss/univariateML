#!/usr/bin/env python3
"""paralogis and invparalogis on the near-constant class: the maximizer, at 100 digits.

Both coefficients free (held {} in the record).  Derivation: see the report, section paralogis.
Writes the numbers; usage: python3 paralogis.py > paralogis.txt
"""
import mpmath as mp
from common import *
mp.mp.dps = 120

def logf(fam, x, a, th):
    if fam == 'paralogis':
        return sum(2*mp.log(a) + a*mp.log(v/th) - mp.log(v) - (a+1)*mp.log1p((v/th)**a) for v in x)
    return sum(2*mp.log(a) + a*mp.log(th/v) - mp.log(v) - (a+1)*mp.log1p((th/v)**a) for v in x)

def make(fam, x):
    x0 = x[0]; sg = 1 if fam == 'paralogis' else -1
    # theta = x0*exp(sg*(-z/a)): z = a*log(x0/theta) (paralogis) or a*log(theta/x0) (invparalogis)
    def ll(la, z):
        a = mp.exp(la)
        th = x0*mp.exp(-sg*z/a)
        return logf(fam, x, a, th), a, th
    return ll

def best_z(ll, la, z0):
    f = lambda z: ll(la, z)[0]
    # concave in theta direction? solve score by secant from z0
    z = mp.findroot(lambda t: mp.diff(f, t), (z0, z0+1), solver='secant', tol=mp.mpf(10)**-60, maxsteps=200)
    return z

out = {}
for fam in ('paralogis', 'invparalogis'):
    x, held = sample(fam)
    d = delta(x)
    ll = make(fam, x)
    # coarse scan in log10 alpha with profile over z (grid in z, then polish)
    def prof(la, z0=None):
        f = lambda z: ll(la, z)[0]
        zs = [mp.mpf(k)/2 for k in range(-300, 41)]
        zz = max(zs, key=f)
        z = best_z(ll, la, zz)
        return f(z), z
    scan = []
    grid = [mp.mpf(e) for e in [-1, 0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 15, 15.5, 16, 16.25, 16.5, 17, 17.5, 18]]
    print(fam, 'delta =', mp.nstr(d, 12), ' 1/delta =', mp.nstr(1/d, 12))
    for e in grid:
        v, z = prof(e*mp.log(10))
        scan.append((e, v))
        print('  log10 alpha %6s  profile loglik %s  z=%s' % (mp.nstr(e, 5), mp.nstr(v, 14), mp.nstr(z, 8)))
    # refine
    ebest = max(scan, key=lambda t: t[1])[0]
    la0 = ebest*mp.log(10)
    g = lambda la: prof(la)[0]
    la = mp.findroot(lambda t: mp.diff(g, t), la0, tol=mp.mpf(10)**-30, maxsteps=100)
    v, z = prof(la)
    _, a, th = ll(la, z)
    h = mp.diff(g, la, 2)
    # curvature in log-alpha of the profile; and Hessian-based curvature in (log alpha, z)
    res = dict(alpha=a, theta=th, loglik=v, d2_profile_log_alpha=h, z=z, alpha_times_delta=a*d,
               theta_over_x0=th/x[0]-1, alpha_rep=representable(a), theta_rep=representable(th),
               delta=d, x=[float(t).hex() for t in x])
    print(fam, {k: (mp.nstr(v_, 15) if isinstance(v_, mp.mpf) else v_) for k, v_ in res.items()})
    out[fam] = res

# What survives rounding of the scale to binary64?  Re-optimise alpha for each double theta near the maximizer.
print()
for fam in ('paralogis', 'invparalogis'):
    x, held = sample(fam)
    r = out[fam]; a_hat, th_hat, L = r['alpha'], r['theta'], r['loglik']
    import struct
    base = float(th_hat)
    def ulp_step(v, k):
        import math
        for _ in range(abs(k)):
            v = math.nextafter(v, math.inf if k > 0 else -math.inf)
        return v
    rows = []
    for k in range(-4, 5):
        th = mp.mpf(ulp_step(base, k))
        f = lambda la: logf(fam, x, mp.exp(la), th)
        gridv = [mp.log(10)*(mp.mpf(14)+mp.mpf(j)/20) for j in range(0, 80)]
        lo0 = max(gridv, key=f); lo, hi = lo0-mp.log(10)/20, lo0+mp.log(10)/20
        for _ in range(150):                      # golden-section search on the profile in log alpha
            m1, m2 = lo+(hi-lo)*0.382, lo+(hi-lo)*0.618
            if f(m1) < f(m2): lo = m1
            else: hi = m2
        la = (lo+hi)/2
        rows.append((k, th, mp.exp(la), f(la)))
        print(fam, 'theta = nearest double %+d ulp: best alpha %s, loglik %s, loss %s; loglik at (alpha_hat, theta) %s'
              % (k, mp.nstr(mp.exp(la), 10), mp.nstr(f(la), 12), mp.nstr(L-f(la), 6), mp.nstr(f(mp.log(a_hat)), 12)))
    print(fam, 'theta_hat - double(theta_hat), in ulps:', mp.nstr((th_hat-mp.mpf(base))/(mp.mpf(2)**-53 if th_hat < 1 else mp.mpf(2)**-52)*(1), 6))
