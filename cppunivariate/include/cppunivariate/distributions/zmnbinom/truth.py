"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import negative_binomial, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

# size is continuous in this negative-binomial density. Its README role
# says trials, but unlike a binomial size it is not an integer design.
regular_parameters = ['size']

def build(t, logd):
    r,p=t['size'],t['prob']
    base=lambda x:negative_binomial(r,p,x)
    ratio=lambda k:(1-p)*max(1,(k+r)/(k+1))
    weight=1-t['p0']
    survival0=base(0)[1]
    def tails(x):
        small=base(x)[1]+mp.log(weight)-survival0
        return pair(small,False)
    out=Reference(tails,0,mp.inf,True)
    out.normalization=geometric_sum(lambda k:mp.exp(logd(mp.mpf(k))),0,ratio)
    return out

