"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,xi=t['mean'],t['sd'],t['xi']
    a=t['nu']
    m1=mp.gamma(2/a)/mp.sqrt(mp.gamma(1/a)*mp.gamma(3/a))
    b=mp.sqrt(mp.gamma(1/a)/mp.gamma(3/a))
    base=lambda z:pair(gamma(1/a,(abs(z)/b)**a)[1]-mp.log(2),z<0)
    shift=m1*(xi-1/xi)
    scale=mp.sqrt((1-m1*m1)*(xi*xi+1/(xi*xi))+2*m1*m1-1)
    def tails(x):
        y=scale*(x-m)/s+shift
        if y<0: return pair(mp.log(2/(1+xi*xi))+base(y*xi)[0])
        return pair(mp.log(2*xi*xi/(1+xi*xi))+base(y/xi)[1],False)
    def density(x):
        y=scale*(x-m)/s+shift
        z=y*xi if y<0 else y/xi
        return (mp.log(a)-mp.log(b)-mp.loggamma(1/a)-mp.log(xi+1/xi)
                +mp.log(scale/s)-(abs(z)/b)**a)
    out=Reference(tails,breaks=[m-s*shift/scale],log_density=density)
    center=m-s*shift/scale
    width=s*b/scale
    out.chart=lambda y:(center+width*(xi if y>=0 else 1/xi)*mp.sinh(y),
                        mp.log(width*(xi if y>=0 else 1/xi))+mp.log(mp.cosh(y)))
    out.unchart=lambda x:mp.asinh((x-center)/(width*(xi if x>=center else 1/xi)))
    # Two half-gamma integrals have stretch factors xi and 1/xi;
    # the outer affine Jacobian cancels scale/s.
    C=mp.exp(mp.log(a)-mp.log(b)-mp.loggamma(1/a)-mp.log(xi+1/xi))
    out.normalization_identity=C*(b/a)*mp.gamma(1/a)*(xi+1/xi)
    return out


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'nu': 100000000.0, 'xi': 100000000.0}, {'nu': 100000000.0, 'xi': 1e-08}]

# Exact and general compensated-moment stresses.
extreme_parameter_sets.extend([{'nu': 1.0, 'xi': 1e-08}, {'nu': 1.0, 'xi': 100000000.0}, {'nu': 3.8792797496374307, 'xi': 100000000.0}])
