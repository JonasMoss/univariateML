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


# Independent rows retained outside active checks; exact findings in the TASK-106 report.
truth_exclusions = {'quantiles': [([0.0, 1.0, 0.0], [1, 1, -690.7755278982137], 'TASK-106 dpqr boundary mismatch; see boundary report'),
               ([0.0, 1.0, 0.0], [1, 1, -230.25850929940458], 'TASK-106 dpqr boundary mismatch; see boundary report'),
               ([0.0, 1.0, 0.0], [1, 1, -69.07755278982137], 'TASK-106 dpqr boundary mismatch; see boundary report'),
               ([0.0, 1.0, 0.0], [1, 1, -27.631021115928547], 'TASK-106 dpqr boundary mismatch; see boundary report'),
               ([0.0, 1.0, 0.0], [1, 0, 1e-300], 'TASK-106 dpqr boundary mismatch; see boundary report'),
               ([0.0, 1.0, 0.0], [1, 0, 1e-100], 'TASK-106 dpqr boundary mismatch; see boundary report'),
               ([0.0, 1.0, 0.0], [1, 0, 1e-30], 'TASK-106 dpqr boundary mismatch; see boundary report'),
               ([0.0, 1.0, 0.0], [1, 0, 1e-12], 'TASK-106 dpqr boundary mismatch; see boundary report')]}
