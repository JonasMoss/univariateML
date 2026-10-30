"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s=t['mu'],t['sigma']
    def tails(x):
        if x<mp.mpf('1e-20')*s/(1+m/s):
            # Relative Taylor remainder bounded by O((x/s)^2+(m*x/s^2)^2)
            # <1e-40. This avoids subtracting equal normal tail values.
            return pair(mp.log(2)+mp.log(x)-mp.log(s)-mp.log(2*mp.pi)/2-(m/s)**2/2)
        upper=mp.log(mp.exp(normal((x-m)/s)[1])+mp.exp(normal((x+m)/s)[1]))
        if upper < -mp.log(2):return pair(upper,False)
        a,b=normal((x-m)/s)[0],normal((-x-m)/s)[0]
        return pair(a+mp.log(-mp.expm1(b-a)))
    return Reference(tails,0)
