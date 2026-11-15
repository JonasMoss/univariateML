#!/usr/bin/env python3
"""genbeta, large-power regime: h(s) = 3 log s + 3 [p psi(p) - p - lgamma(p)],  log p - psi(p) = log((2+e^s)/3) - s/3.
Here s = a * log(x3/x0).  The limit h(0+) = (3/2) log(9/(4 pi)) - 3/2 is the flat value of the Gaussian regime."""
import mpmath as mp
mp.mp.dps = 60
def h(s):
    s = mp.mpf(s)
    c = mp.log((2+mp.exp(s))/3) - s/3
    p = mp.findroot(lambda t: mp.log(t)-mp.psi(0, t)-c, 1/(2*c)+mp.mpf(1)/6, tol=mp.mpf(10)**-45)
    return 3*mp.log(s) + 3*(p*mp.psi(0, p) - p - mp.loggamma(p)), p
h0 = mp.mpf(3)/2*mp.log(9/(4*mp.pi)) - mp.mpf(3)/2
print('h(0+) =', mp.nstr(h0, 20))
for s in ['1e-6', '1e-4', '1e-3', '1e-2', '0.1', '0.5', '1', '2', '3', '5', '10', '20']:
    v, p = h(s)
    print('s = %-6s p = %-14s h(s) = %-22s h(s)-h(0+) = %-14s  /s = %s' % (s, mp.nstr(p, 8), mp.nstr(v, 16), mp.nstr(v-h0, 8), mp.nstr((v-h0)/mp.mpf(s), 8)))
