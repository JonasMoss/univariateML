"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,xi=t['mean'],t['sd'],t['xi']
    a=t['nu']
    m1=mp.gamma(2/a)/mp.sqrt(mp.gamma(1/a)*mp.gamma(3/a))
    b=mp.sqrt(mp.gamma(1/a)/mp.gamma(3/a))
    base=lambda z:pair(gamma(1/a,(abs(z)/b)**a)[1]-mp.log(2),z<0)
    shift=m1*(xi-1/xi)
    scale=mp.sqrt((1-m1*m1)*(xi*xi+1/(xi*xi))+2*m1*m1-1)
    def tails(x):
        y=scale*(x-m)/s+shift
        if y<0: return pair(mp.log(2/(1+xi*xi))+base(y*xi)[0])
        return pair(mp.log(2*xi*xi/(1+xi*xi))+base(y/xi)[1],False)
    return Reference(tails,breaks=[m-s*shift/scale])


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'nu': 100000000.0, 'xi': 100000000.0}, {'nu': 100000000.0, 'xi': 1e-08}]

# Exact and general compensated-moment stresses.
extreme_parameter_sets.extend([{'nu': 1.0, 'xi': 1e-08}, {'nu': 1.0, 'xi': 100000000.0}, {'nu': 3.8792797496374307, 'xi': 100000000.0}])

# The compensated moment's practical error bound cannot certify these natural
# tails. Keep the independent numbers; public p returns a named-region NaN.
truth_exclusions = {'values': [
    ([0.0, 1.0, 3.8792797496374307, 100000000.0], [x], reason)
    for x, reason in [
        (-1.5461574810135936, 'TASK-189 open: natural lower-tail precision gate returns NaN; reference 4.111206646631721e-108; propagated log-tail margin 3.1645e-10 exceeds the 1e-10 contract.'),
        (-1.5461574810135932, 'TASK-189 open: natural lower-tail precision gate returns NaN; reference 1.5600541083276566e-34; propagated log-tail margin 9.1662e-11 exceeds the reserved 1e-11 coordinate budget; sharper moment enclosure remains open.')
    ]
]}
