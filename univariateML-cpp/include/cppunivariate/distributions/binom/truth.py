"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference

def build(t, logd):
    # At probability endpoints the conditional law concentrates on an extreme.
    lo, hi = 0, t['size']
    if lo > hi: raise ValueError('empty conditional support')
    if t['prob'] in (0, 1) or hi == lo:
        return atomic({lo if t['prob'] == 0 else hi: 1}, lo, hi)
    n,p=t['size'],t['prob']
    # Positive polynomial weights preserve exact dyadic probabilities. In
    # particular Binomial(9, 1/2) has P(X>4)=1/2 exactly. Exponentiating
    # log-PMFs perturbed that equality and moved the upper median to 5.
    weights=[mp.binomial(int(n),k)*p**k*(1-p)**(int(n)-k)
             for k in range(int(n)+1)]
    def tails(x):
        k=int(x)+1
        return mp.log(mp.fsum(weights[:k])), mp.log(mp.fsum(weights[k:]))
    out=Reference(tails,0,n,True)
    out.normalization=mp.fsum(weights)
    return out


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'size': 100000000, 'prob': 1e-08}]
