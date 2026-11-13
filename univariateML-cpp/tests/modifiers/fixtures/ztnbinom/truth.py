"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import conditioned_positive_tails
from quality.truth_reference import atomic
from quality.truth_reference import negative_binomial, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

# size is continuous in this negative-binomial density. Its README role
# says trials, but unlike a binomial size it is not an integer design.
regular_parameters = ['size']

def build(t, logd):
    if t['prob'] == 1: return atomic({1: 1}, 1)
    # r -> 0: the positive NB law becomes the logarithmic-series law,
    # (1-p)^k / (-k log(p)); follows by Gamma(r+k)/Gamma(r) ~ r Gamma(k).
    if t['size'] == 0:
        from quality.truth_reference import logarithmic_tail
        p=t['prob']; q=1-p
        weight=mp.mpf(1)
        ld=lambda x: (mp.log(1-weight) if x == 0 else
            mp.log(weight)+x*mp.log(q)-mp.log(x)-mp.log(-mp.log(p)))
        out=Reference(lambda x:pair(mp.log(weight)+logarithmic_tail(q,int(x)+1),False),
                      1,mp.inf,True,log_density=ld)
        out.normalization=mp.mpf(1)  # sum q^k/k = -log(1-q)
        return out
    r,p=t['size'],t['prob']
    base=lambda x:negative_binomial(r,p,x)
    ratio=lambda k:(1-p)*max(1,(k+r)/(k+1))
    weight=mp.mpf(1)
    zero=base(0)
    def tails(x):
        return conditioned_positive_tails(base(x),zero)
    out=Reference(tails,1,mp.inf,True)
    out.normalization=mp.mpf(1)  # Exact generating-series identity; pmf differences checked separately.
    return out


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'size': 100000000.0, 'prob': 0.9999999900000001}]
