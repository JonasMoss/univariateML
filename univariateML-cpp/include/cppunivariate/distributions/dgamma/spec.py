"""Independent positive-series CDF oracle, with an explicit remainder check.

F_a(t) = exp(-t) sum_{j>=0} t^(a+j)/Gamma(a+j+1).
The geometric bound checks every golden endpoint before using 64 terms.
This representation has ordinary symbolic first and second derivatives.
"""
import json
from pathlib import Path
import mpmath as mp
import sympy as sp


def log_density(symbols, x):
    golden = json.loads(Path(__file__).with_name('golden.json').read_text())
    a0 = golden['estimates']['shape']
    r0 = golden['estimates']['rate']
    with mp.workdps(40):
        for k in set(golden['data']):
            t = mp.mpf(k + 1)*r0
            ratio = t/(a0+65)
            assert ratio < 1
            remainder = mp.exp(-t)*t**(a0+64)/mp.gamma(a0+65)/(1-ratio)
            mass = mp.gammainc(a0, k*r0, t)/mp.gamma(a0)
            assert remainder/mass < mp.mpf('1e-20')
    a, r = symbols['shape'], symbols['rate']
    def cdf(t):
        return sp.exp(-t)*sum(t**(a+j)/sp.gamma(a+j+1) for j in range(64))
    return sp.log(cdf(r*(x+1))-sp.Piecewise((0, sp.Eq(x, 0)), (cdf(r*x), True)))
