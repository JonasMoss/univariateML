#!/usr/bin/env python3
"""betapr: Li - Lg (inverse-gamma face minus gamma face, exact 100-digit maxima of the two limit models) against delta/3,
for several c (sample c, c, nextafter(c))."""
import math
import mpmath as mp
from common import *
mp.mp.dps = 100
def gamma_ll(y):
    n = len(y); m = mp.fsum(y)/n; ml = mp.fsum(mp.log(v) for v in y)/n; c = mp.log(m)-ml
    k = mp.findroot(lambda t: mp.log(t)-mp.psi(0, t)-c, 1/(2*c), tol=mp.mpf(10)**-80)
    th = m/k
    return k, mp.fsum(-mp.loggamma(k)-k*mp.log(th)+(k-1)*mp.log(v)-v/th for v in y)
for c in [0.001, 0.2, 0.612107714198942, 1.0, 1.5, 3.7, 1000.0]:
    x = [mp.mpf(c), mp.mpf(c), mp.mpf(math.nextafter(c, 1e9))]
    d = x[2]/x[0]-1
    kg, Lg = gamma_ll(x); ki, Li = gamma_ll([1/v for v in x]); Li -= 2*mp.fsum(mp.log(v) for v in x)
    print('c = %-12g delta = %s  Li - Lg = %s  (Li-Lg)/(delta/3) = %s   gamma shape %s  invgamma shape %s' % (c, mp.nstr(d, 6), mp.nstr(Li-Lg, 8), mp.nstr((Li-Lg)/(d/3), 10), mp.nstr(kg, 6), mp.nstr(ki, 6)))
