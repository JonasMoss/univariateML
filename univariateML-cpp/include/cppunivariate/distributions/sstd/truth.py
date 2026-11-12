"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd=None):
    m,s,xi=t['mean'],t['sd'],t['xi']
    a=t['nu']
    m1=mp.gamma((a-1)/2)/mp.gamma(a/2)*mp.sqrt((a-2)/mp.pi)
    base=lambda z:student(a,z*mp.sqrt(a/(a-2)))
    shift=m1*(xi-1/xi)
    scale=mp.sqrt((1-m1*m1)*(xi*xi+1/(xi*xi))+2*m1*m1-1)
    def tails(x):
        y=scale*(x-m)/s+shift
        if y<0: return pair(mp.log(2/(1+xi*xi))+base(y*xi)[0])
        return pair(mp.log(2*xi*xi/(1+xi*xi))+base(y/xi)[1],False)
    out=Reference(tails,breaks=[m-s*shift/scale])
    # Two stretched half-Student integrals, followed by an affine map.
    C=mp.exp(mp.loggamma((a+1)/2)-mp.loggamma(a/2)-mp.log(a*mp.pi)/2)
    out.normalization_identity=C*mp.sqrt(a)*mp.beta(mp.mpf('.5'),a/2)
    out.log_density=logd
    return out


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'nu': 2.00000001, 'sd': 14142.135659086289}, {'nu': 100000000.0, 'xi': 100000000.0}, {'nu': 100000000.0, 'xi': 1e-08}]



# The exact nu=5 skew moment retains all measured thin-side rows as active.

# Exact and general compensated-moment stresses.
extreme_parameter_sets.extend([{'nu': 3.093978716511268, 'xi': 100000000.0}])
