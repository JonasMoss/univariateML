"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

held_parameters = ['a','b']

def build(t):
    a,b,c=t['a'],t['b'],t['c']
    def tails(x):
        if c == a:
            z=(x-a)/(b-a)
            return pair(mp.log(z)+mp.log(2-z)) if z < mp.mpf('.5') else pair(2*mp.log((b-x)/(b-a)),False)
        if c == b:
            z=(b-x)/(b-a)
            return pair(mp.log(z)+mp.log(2-z),False) if z < mp.mpf('.5') else pair(2*mp.log((x-a)/(b-a)))
        return pair(mp.log((x-a)**2/((b-a)*(c-a)))) if x<c else pair(mp.log((b-x)**2/((b-a)*(b-c))),False)
    return Reference(tails,a,b,breaks=[c])
