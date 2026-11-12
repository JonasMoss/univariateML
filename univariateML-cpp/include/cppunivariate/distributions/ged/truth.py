"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,a=t['mean'],t['sd'],t['nu']
    scale=s*mp.sqrt(mp.gamma(1/a)/mp.gamma(3/a))
    def tails(x):
        small=gamma(1/a,(abs(x-m)/scale)**a)[1]-mp.log(2)
        return pair(small,x<m)
    # Evaluate the same README log density in mp arithmetic: symbolic
    # substitution would raise exact rationals to 200-millionth powers.
    def density(x):
        return mp.log(a)-mp.log(2*scale)-mp.loggamma(1/a)-(abs(x-m)/scale)**a
    out=Reference(tails,breaks=[m],log_density=density)
    out.chart=lambda y:(m+scale*mp.sinh(y),mp.log(scale)+mp.log(mp.cosh(y)))
    out.unchart=lambda x:mp.asinh((x-m)/scale)
    # Exact gamma integral after u=(|x-m|/scale)^a.
    C=mp.exp(mp.log(a)-mp.log(2*scale)-mp.loggamma(1/a))
    out.normalization_identity=C*(2*scale/a)*mp.gamma(1/a)
    return out
