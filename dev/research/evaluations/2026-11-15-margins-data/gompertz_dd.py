"""Option (a) for gompertz: error-free double-double evaluation of  T = n Q - 2 S^2  with a rigorous pad.

x scaled by an exact power of two (no rounding unless a term underflows); S and Q accumulated as unevaluated pairs
(Ogita, Rump, Oishi 2005: Sum2/Dot2 before the final rounding).  Bounds (u = 2^-53, gamma_k = k u/(1 - k u)):
  |S_hat - S| <= gamma_{n-1}^2 sum|x|            (Sum2 pair)
  |Q_hat - Q| <= gamma_n^2 sum x^2               (Dot2 pair; the products are exact through fma)
T_hat = n Q_hat - 2 S_hat^2 is formed in double-double; its own rounding is bounded by 8 u^2 (n Q_hat + 2 S_hat^2) (hypothesis: a
few double-double operations, each with relative error <= 2 u^2 in Joldes-Muller-Popescu).  The decision is
  limit  if T_hat >  +pad,   finite if T_hat < -pad,   undecided otherwise,
  pad = 2 n gamma_n^2 Q_hat + 4 gamma_{n-1}^2 S_hat^2 ... (see report), here: pad = (4 n gamma_n^2 + 16 u^2) * n Q_hat.
Run on the samples of gompertz_boundary.py (same seed): wrong decisions and undecided counts.
"""
import math, sys, json, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from fractions import Fraction
import gompertz_boundary as gb
u = 2.0**-53

def two_sum(a, b):
    s = a+b; bb = s-a; return s, (a-(s-bb))+(b-bb)

def two_prod(a, b):
    p = a*b; return p, math.fma(a, b, -p)

def dd_add(a, b):                       # (hi, lo) + (hi, lo)
    s, e = two_sum(a[0], b[0]); e += a[1]+b[1]; return two_sum(s, e)

def dd_mul(a, b):
    p, e = two_prod(a[0], b[0]); e += a[0]*b[1]+a[1]*b[0]; return two_sum(p, e)

def evaluate(x):
    n = len(x); big = max(x); e = math.frexp(big)[1]
    z = [math.ldexp(v, -e) for v in x]
    s = (0.0, 0.0); q = (0.0, 0.0)
    for v in z:
        s = dd_add(s, (v, 0.0))
        p, pe = two_prod(v, v); q = dd_add(q, (p, pe))
    nq = dd_mul((float(n), 0.0), q)
    s2 = dd_mul(s, s); s2 = (2*s2[0], 2*s2[1])
    t = dd_add(nq, (-s2[0], -s2[1]))
    g = n*u/(1-n*u)
    pad = (4*n*g*g+16*u*u)*(nq[0]+s2[0])
    T = t[0]+t[1]
    return (1 if T > pad else -1 if T < -pad else 0), T, pad

if __name__ == '__main__':
    # same samples as gompertz_boundary.run(), keeping every x
    src = open(gb.__file__).read().replace("x=x if n <= 5 else None", "x=x")
    ns = {'__name__': 'gb_keepx'}; exec(compile(src, 'gb_keepx', 'exec'), ns)
    sample = ns['run']()
    from collections import defaultdict
    by = defaultdict(lambda: [0, 0, 0, 0.0, 0.0])
    for r in sample:
        x = r['x']; d, S = gb.exact_delta(x); truth = d >= 0
        side, T, pad = evaluate(x)
        b = by[r['n']]; b[0] += 1
        if side == 0: b[2] += 1
        elif (side > 0) != truth: b[1] += 1
        z = math.ldexp(1.0, -math.frexp(max(x))[1]); Sz = float(S)*z
        b[3] = max(b[3], pad/(2*Sz*Sz)); b[4] = max(b[4], abs(float(d/(2*S*S))))
    print('double-double decision with pad: per n -> samples, wrong, undecided, max pad/(2 S^2) (relative band), max |gap| of the samples')
    for n in sorted(by): print('  n=%5d  samples %4d  wrong %d  undecided %4d  band %.2e   max|gap| %.2e' % (n, *by[n]))
