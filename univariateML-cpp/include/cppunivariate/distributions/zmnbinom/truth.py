"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import negative_binomial, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

# size is continuous in this negative-binomial density. Its README role
# says trials, but unlike a binomial size it is not an integer design.
regular_parameters = ['size']

def build(t, logd):
    if t['prob'] == 1: return atomic({0: t['p0'], 1: 1-t['p0']})
    if t['p0'] == 1: return atomic({0: 1})
    # r -> 0: the positive NB law becomes the logarithmic-series law,
    # (1-p)^k / (-k log(p)); follows by Gamma(r+k)/Gamma(r) ~ r Gamma(k).
    if t['size'] == 0:
        from quality.truth_reference import logarithmic_tail
        p=t['prob']; q=1-p
        weight=1-t['p0']
        ld=lambda x: (mp.log(1-weight) if x == 0 else
            mp.log(weight)+x*mp.log(q)-mp.log(x)-mp.log(-mp.log(p)))
        out=Reference(lambda x:pair(mp.log(weight)+logarithmic_tail(q,int(x)+1),False),
                      0,mp.inf,True,log_density=ld)
        out.normalization=mp.mpf(1)  # sum q^k/k = -log(1-q)
        return out
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

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'size': 100000000.0, 'prob': 0.9999999900000001}]
