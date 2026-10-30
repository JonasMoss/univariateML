"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,a=t['mu'],t['sigma'],t['a']
    def primitive(z):
        return z*mp.erfc(-z/mp.sqrt(2))/2+mp.exp(-z*z/2)/mp.sqrt(2*mp.pi)
    def tails(x):
        # A normal plus a symmetric uniform is symmetric about m. In
        # particular H(b)-H(-b)=b, so both center tails equal exactly 1/2.
        # Preserve this identity rather than passing integration roundoff
        # into the generic relative root stop near a zero median.
        if x==m:return (-mp.log(2),-mp.log(2))
        d=abs(x-m)
        z=(d-a)/s
        if z>mp.mpf('1e30'):
            # Integrated Mills expansion. Relative omitted correction O(z^-2)
            # is <1e-60; the opposite endpoint is exponentially smaller.
            return pair(-z*z/2-2*mp.log(z)+mp.log(s/(2*a))-mp.log(2*mp.pi)/2,x<m)
        small=s/(2*a)*(primitive((a-d)/s)-primitive((-a-d)/s))
        return pair(mp.log(small),x<m)
    def ld(x):
        d=abs(x-m); z=(d-a)/s
        if z>mp.mpf('1e30'):return -z*z/2-mp.log(z)-mp.log(2*a)-mp.log(2*mp.pi)/2
        return mp.log(mp.erfc((d-a)/(s*mp.sqrt(2)))-mp.erfc((d+a)/(s*mp.sqrt(2))))-mp.log(4*a)
    return Reference(tails,breaks=[m-a,m,m+a],log_density=ld)

