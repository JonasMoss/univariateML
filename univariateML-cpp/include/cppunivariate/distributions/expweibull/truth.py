"""CDF antiderivative and analytic inverse, independently differentiated by the harness."""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement

boundary_parameter_sets = [
    {'shape': .01, 'exponent': .02},
    {'shape': 100, 'exponent': 100},
    {'shape': 1e-3, 'exponent': 1e3},
    {'shape': 1e3, 'exponent': 1e-3},
    {'shape': 1e3, 'exponent': 1e200},
]

def build(t, declarative):
    a, b, c = t['scale'], t['shape'], t['exponent']
    def tails(x):
        h = (x/a)**b
        # Preserve both tails when the Weibull survival is below mp epsilon.
        if h > max(200, mp.log(c)+200):
            return pair(mp.log(c)-h, False)
        return pair(c*(mp.log1p(-mp.exp(-h)) if h>1 else mp.log(-mp.expm1(-h))), True)
    def logdensity(x):
        if x == 0:
            return mp.inf if b*c < 1 else -mp.inf if b*c > 1 else -mp.log(a)
        h = (x/a)**b
        if h < mp.mpf('1e-35'):
            # Relative correction O(h^2) < 1e-70, below the harness
            # verification tolerance and binary64 table precision.
            return mp.log(b*c)-mp.log(x)+b*c*mp.log(x/a)-(c+1)*h/2
        if h > 1:
            return mp.log(b*c)-mp.log(x)+mp.log(h)-h+(c-1)*mp.log1p(-mp.exp(-h))
        return declarative(x)
    ref = Reference(tails, 0, log_density=logdensity)
    def quantile(lp, lower):
        if not lower and lp < -180:
            return a*(-lp+mp.log(c))**(1/b)
        lf = lp if lower else complement(lp)
        if lf == -mp.inf: return mp.mpf(0)
        if lf == 0: return mp.inf
        if lf/c < -180: return a*mp.exp(lf/(b*c))
        return a*(-mp.log(-mp.expm1(lf/c)))**(1/b)
    ref.quantile = quantile
    return ref
