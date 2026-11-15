"""Check the derived forward-error bounds on every constructed sample (binom_boundary.py, gompertz_boundary.py).

binom:    |ratio_hat / ratio - 1| <= gamma_{n+5}  (+ n xbar^2 u^2 / V),  ratio = mean / var_pop, V = sum (x - xbar)^2
gompertz: |(lhs_hat / rhs_hat) / (n Q / (2 S^2)) - 1| <= gamma_{3n+8}   (lhs_hat = sq/n, rhs_hat = 2 m^2 of the standardized data)
Reports the worst observed error in units of the bound.  Exact quantities in rationals.
"""
import math, os, sys
from fractions import Fraction
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import binom_boundary as bb, gompertz_boundary as gb
u = 2.0**-53
gam = lambda k: k*u/(1-k*u)

def keep(module, old):
    src = open(module.__file__).read().replace(old, "x=x")
    ns = {'__name__': 'keep'}; exec(compile(src, 'keep', 'exec'), ns); return ns

# binom
rows = keep(bb, "x=x if n <= 255 else None")['run']()
worst = 0.0; count = 0
for r in rows:
    x = r['x']; n = len(x); S = sum(x); Q = sum(v*v for v in x)
    ratio = Fraction(S, n)/Fraction(n*Q-S*S, n*n)
    for fma in (False, True):
        _, rh = bb.mirror(x, fma)
        err = abs(Fraction(rh)/ratio-1)
        bound = gam(n+5)+Fraction(n*S*S, n*n)*Fraction(u)**2/Fraction(n*Q-S*S, n)
        worst = max(worst, float(err/Fraction(bound))); count += 1
print('binom: %d (sample, variant) pairs; worst |error| / bound = %.3f' % (count, worst))
# gompertz
def parts(x, extended, fma):
    n = len(x)
    if extended:
        t = gb.LD(0)
        for v in x: t += gb.LD(v)
        unit = float(t/gb.LD(n))
    else:
        t = 0.0
        for v in x: t += v
        unit = t/n
    z = [v/unit for v in x]; sx = sq = 0.0
    for v in z:
        sx += v; sq = math.fma(v, v, sq) if fma else sq+v*v
    m = sx/n
    return sq/n, 2.0*m*m
rows = keep(gb, "x=x if n <= 5 else None")['run']()
worst = 0.0; count = 0
for r in rows:
    x = r['x']; n = len(x); d, S = gb.exact_delta(x)
    Q = sum(Fraction(v)**2 for v in x); rho = n*Q/(2*S*S)
    for e in (True, False):
        for f in (False, True):
            lhs, rhs = parts(x, e, f)
            err = abs(Fraction(lhs)/Fraction(rhs)/rho-1)
            worst = max(worst, float(err/Fraction(gam(3*n+8)))); count += 1
print('gompertz: %d (sample, variant) pairs; worst |error| / bound = %.4f' % (count, worst))
