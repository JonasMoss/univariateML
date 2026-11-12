"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd=None):
    mu,nu,sigma=t['mu'],t['nu'],t['sigma']
    def tails(x):
        z=((x-mu)/sigma)**2
        return beta(mp.mpf('.5'),nu/2,z/(nu+z),nu/(nu+z))
    out=Reference(tails,mu)
    # Integral of 2 C (1+z^2/nu)^(-(nu+1)/2), z>=0:
    # C sqrt(nu) B(1/2,nu/2), by z=sqrt(nu)*sqrt(u/(1-u)).
    C=mp.exp(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2)
    out.normalization_identity=C*mp.sqrt(nu)*mp.beta(mp.mpf('.5'),nu/2)
    out.log_density=logd
    return out
