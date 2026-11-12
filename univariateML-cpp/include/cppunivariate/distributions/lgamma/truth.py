"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    shapelog = t['shapelog']
    ratelog = t['ratelog']
    def density(x):
        if x==1:
            return mp.inf if shapelog<1 else -mp.inf if shapelog>1 else mp.log(ratelog)
        lx=mp.log(x)
        return shapelog*mp.log(ratelog)-mp.loggamma(shapelog)+(shapelog-1)*mp.log(lx)-(ratelog+1)*lx
    out=Reference(lambda x:gamma(shapelog,ratelog*mp.log(x)),1,mp.inf,log_density=density)
    # y=ratelog*log(x) gives the complete gamma integral.
    out.normalization_identity=mp.exp(-mp.loggamma(shapelog))*mp.gamma(shapelog)
    return out

