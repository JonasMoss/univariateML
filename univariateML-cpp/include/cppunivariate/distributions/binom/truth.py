"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference

def build(t, logd):
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

