#!/usr/bin/env python3
"""genbeta, scale = 1 held: the log-gamma face (a -> 0, p -> infinity, p a -> lambda) versus the profile in a.

Face model: W = -log X ~ Gamma(shape q, rate lambda); maximised by the gamma MLE on w_i = -log x_i.
Compares loglik_face with the profile loglik at a = 1e-2, 1e-4, 1e-6 and checks the coefficient of a^2.
Also repeats the sign test for other values of c (same construction [c, c, nextafter(c)])."""
import math, sys
import mpmath as mp
import genbeta as G
from common import *
mp.mp.dps = 200

def face(x):
    w = [-mp.log(v) for v in x]; n = len(w)
    m = mp.fsum(w)/n; ml = mp.fsum(mp.log(v) for v in w)/n; c = mp.log(m)-ml
    q = mp.findroot(lambda t: mp.log(t)-mp.psi(0, t)-c, 1/(2*c), tol=mp.mpf(10)**-120)
    lam = q/m
    return q, lam, mp.fsum(q*mp.log(lam)-mp.loggamma(q)+(q-1)*mp.log(v)-lam*v + v for v in w)

def run(x, label, a_list):
    G.x = x; G.Lx = [mp.log(v) for v in x]
    d = x[2]/x[0]-1
    q, lam, Lf = face(x)
    print(label, 'x0 =', mp.nstr(x[0], 12), 'delta =', mp.nstr(d, 8), ' face: q =', mp.nstr(q, 8), 'lambda =', mp.nstr(lam, 8), 'loglik_face =', mp.nstr(Lf, 25))
    out = []
    for a in a_list:
        a = mp.mpf(a); p, qq = G.pq_hat(a); v = G.ll(p, qq, a)
        print('   a = %-8s  (loglik - face)/delta = %-14s  /a^2 = %-12s  p a = %s' % (mp.nstr(a, 4), mp.nstr((v-Lf)/d, 8), mp.nstr((v-Lf)/d/a**2, 8), mp.nstr(p*a, 8)))
        out.append(((v-Lf)/d))
    return out

x, _ = sample('genbeta')
run(x, 'record sample', ['1e-6', '1e-4', '1e-2', '0.1', '1', '5', '20', '100', '1000'])
for c in (0.3, 0.5, 0.7, 0.95, 0.999):
    c0 = float(c); c1 = math.nextafter(c0, 2.0)
    run([mp.mpf(c0), mp.mpf(c0), mp.mpf(c1)], 'c=%g' % c, ['1e-4', '1e-2', '1', '10'])
