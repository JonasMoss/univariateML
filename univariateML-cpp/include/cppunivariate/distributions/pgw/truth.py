"""Independent mpmath cumulative hazard and closed-form inverse.

The harness integrates the declarative density and differentiates the CDF.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement

boundary_parameter_sets = [
    {'exponent': 1.}, {'shape': 1., 'exponent': 1.},
    {'exponent': 1.000000000001}, {'exponent': .999999999999},
    {'shape': .01, 'exponent': .02},
    {'shape': 100., 'exponent': 100.},
    {'shape': 1000., 'exponent': .001},
    {'shape': .001, 'exponent': 1000.},
    # Power-Gompertz path: a=s*c**(1/b), s=1,b=2.
    {'scale': 100., 'shape': 2., 'exponent': 10000.},
    # Threshold-conditional Weibull: a=2,b*c=2.
    {'scale': 2., 'shape': 10000., 'exponent': .0002},
]

def build(t, declarative):
    a,b,c = t['scale'],t['shape'],t['exponent']
    def tails(x):
        v=c*mp.log1p((x/a)**b)
        if v>mp.mpf('1e5'):
            # exp(v) has an enormous integer exponent. Its log upper tail
            # rounds to -Inf in the table; the lower log tail rounds to zero.
            return (mp.mpf(0), -mp.inf)
        return pair(-mp.expm1(v),False)
    def logdensity(x):
        if x==0:
            return mp.inf if b<1 else -mp.inf if b>1 else mp.log(c/a)
        z=b*mp.log(x/a)
        v=c*mp.log1p(mp.exp(z))
        if v>mp.mpf('1e5'): return -mp.inf
        return mp.log(b*c/x)+z-mp.log1p(mp.exp(z))+v-mp.expm1(v)
    ref=Reference(tails,0,log_density=logdensity)
    def quantile(lp,lower):
        ls=complement(lp) if lower else lp
        if ls==0: return mp.mpf(0)
        if ls==-mp.inf: return mp.inf
        return a*mp.expm1(mp.log1p(-ls)/c)**(1/b)
    ref.quantile=quantile
    # Integrate the declarative density in cumulative-hazard coordinates.
    # dx/dH = x*(1+t)/(b*c*t*(1+H)); this resolves narrow threshold
    # transitions and extremely diffuse log-scale laws without a fixed x grid.
    def transformed(h):
        if h==0: return mp.mpf(1)
        power=mp.expm1(mp.log1p(h)/c)
        x=a*power**(1/b)
        jac=x*(1+power)/(b*c*power*(1+h))
        return mp.exp(declarative(x))*jac
    ref.normalization_integrand=transformed
    ref.normalization_domain=[0,mp.mpf('.01'),mp.mpf('.1'),1,4,16,64,256]
    return ref
