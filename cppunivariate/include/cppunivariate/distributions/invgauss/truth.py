"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,a=t['mean'],t['shape']
    def tails(x):
        if x>mp.mpf('1e30')*max(1,m,a,1/a,m*m):
            # Laplace endpoint expansion; omitted relative correction O(1/x).
            ld=mp.log(a)/2-mp.log(2*mp.pi)/2-3*mp.log(x)/2-a*(x-m)**2/(2*m*m*x)
            return pair(ld-mp.log(a/(2*m*m)),False)
        c=mp.sqrt(a/x)
        p=normal(c*(x/m-1))
        other=2*a/m+normal(-c*(x/m+1))[0]
        if p[0]<-mp.log(2): return pair(mp.log(mp.exp(p[0])+mp.exp(other)))
        return pair(p[1]+mp.log1p(-mp.exp(other-p[1])),False)
    return Reference(tails,0)

