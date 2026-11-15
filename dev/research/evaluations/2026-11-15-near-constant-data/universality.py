#!/usr/bin/env python3
"""Large-shape asymptotics of paralogis / invparalogis near-constant fits, and their dependence on c.

Derivation (report): with alpha delta = s and y = alpha e^{w}, the log-likelihood is
  paralogis    : 3 log(s/delta) - 3 log x0 + 3 log 3 - 3 log(2+e^s) + s - 3,         g'(s)=0: 3/s - 3e^s/(2+e^s) + 1 = 0
  invparalogis : 3 log(s/delta) - 3 log x0 + 3 log 3 - 3 log(1+2e^s) + 2s - 3,       g'(s)=0: 3/s - 6e^s/(1+2e^s) + 2 = 0
(the invparalogis sample is, after x -> 1/x, one low value and two high ones).
This script solves g'(s)=0, evaluates the prediction against the exact maximum (record sample) and repeats
the exact maximisation for other c (c, c, nextafter(c)).  Hexfloat inputs, 100 digits."""
import math, sys
import mpmath as mp
from common import *
mp.mp.dps = 100

S = {}
S['paralogis'] = mp.findroot(lambda s: 3/s - 3*mp.exp(s)/(2+mp.exp(s)) + 1, 2)
S['invparalogis'] = mp.findroot(lambda s: 3/s - 6*mp.exp(s)/(1+2*mp.exp(s)) + 2, 3)
def pred(fam, x):
    d = x[2]/x[0]-1; s = S[fam]
    if fam == 'paralogis': g = 3*mp.log(3) - 3*mp.log(2+mp.exp(s)) + s - 3
    else: g = 3*mp.log(3) - 3*mp.log(1+2*mp.exp(s)) + 2*s - 3
    return s/d, 3*mp.log(s/d) - 3*mp.log(x[0]) + g
def curv(fam):
    s = S[fam]
    g = (lambda t: 3*mp.log(t) - 3*mp.log(2+mp.exp(t)) + t) if fam == 'paralogis' else (lambda t: 3*mp.log(t) - 3*mp.log(1+2*mp.exp(t)) + 2*t)
    return mp.diff(g, s, 2)*s*s          # d^2/d(log alpha)^2 of the profile at the maximum

def logf(fam, x, a, th):
    if fam == 'paralogis':
        return sum(2*mp.log(a) + a*mp.log(v/th) - mp.log(v) - (a+1)*mp.log1p((v/th)**a) for v in x)
    return sum(2*mp.log(a) + a*mp.log(th/v) - mp.log(v) - (a+1)*mp.log1p((th/v)**a) for v in x)

def exact(fam, x):
    sg = 1 if fam == 'paralogis' else -1
    d = x[2]/x[0]-1
    a0, _ = pred(fam, x)
    # z = alpha*log(x0/theta) (paralogis) or alpha*log(theta/x0): the analytic start is z0 = log(y/alpha)*...: use a coarse z scan
    def f(la, z):
        a = mp.exp(la); return logf(fam, x, a, x[0]*mp.exp(-sg*z/a))
    z0 = max((mp.mpf(k)/2 for k in range(-300, 41)), key=lambda z: f(mp.log(a0), z))
    sol = mp.findroot(lambda la, z: (mp.diff(f, (la, z), (1, 0)), mp.diff(f, (la, z), (0, 1))), (mp.log(a0), z0), tol=mp.mpf(10)**-40, maxsteps=100)
    la, z = sol
    return mp.exp(la), f(la, z), z

for fam in ('paralogis', 'invparalogis'):
    print(fam, ': s* =', mp.nstr(S[fam], 15), ' curvature of profile in log alpha =', mp.nstr(curv(fam), 12))
    cases = [('record', sample(fam)[0])]
    for c in (1.0, 1.5, 3.7, 0.2):
        cases.append(('c=%g' % c, [mp.mpf(c), mp.mpf(c), mp.mpf(math.nextafter(c, 99.0))]))
    for lab, x in cases:
        a_pred, L_pred = pred(fam, x)
        a, L, z = exact(fam, x)
        print('  %-8s delta = %s  alpha_pred = %s  alpha_exact = %s  (ratio %s)  loglik_pred = %s  loglik_exact = %s' %
              (lab, mp.nstr(x[2]/x[0]-1, 6), mp.nstr(a_pred, 8), mp.nstr(a, 8), mp.nstr(a/a_pred, 10), mp.nstr(L_pred, 12), mp.nstr(L, 12)))
        sys.stdout.flush()
