#!/usr/bin/env python3
"""Independent 80-digit scalar shape roots on frozen binary64 golden inputs.
This checks coefficients and derivatives, not global optimality. No goldens written.
Run: python3 dev/research/evaluations/2026-11-08-std-free-data/oracle.py
"""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps = 80
root = Path(__file__).resolve().parents[4]
records = []
for family, location, scale, shape in [('lst','mu','sigma','df'),('std','mean','sd','nu')]:
    g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions'/family/'golden.json').read_text())
    m,s=mp.mpf(g['estimates'][location]),mp.mpf(g['estimates'][scale])
    q=[((mp.mpf(v)-m)/s)**2 for v in g['data']]
    def ll(v):
        r=v-2 if family=='std' else v
        return len(q)*(mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(mp.pi*r)/2-mp.log(s))-(v+1)/2*sum(mp.log1p(z/r) for z in q)
    v=mp.findroot(lambda v:mp.diff(ll,v), (mp.mpf(g['estimates'][shape])*.99,mp.mpf(g['estimates'][shape])*1.01))
    assert mp.diff(ll,v,2)<0 and abs(mp.diff(ll,v))<mp.mpf('1e-65')
    records.append(dict(family=family,shape=str(v),loglik=str(ll(v)),score=str(mp.diff(ll,v)),curvature=str(mp.diff(ll,v,2)),golden_relative_difference=str(abs(v/mp.mpf(g['estimates'][shape])-1))))
print(json.dumps(records,indent=2))

# Exact rational proof of the subunit nonuniqueness comparison. The centered
# scale root a lies in (1979,1981); raising the likelihood ratio to power four
# cancels fractional powers and its common gamma normalizer.
from fractions import Fraction
from math import prod
B,C=10000,9801
P=lambda a:5*a*a+2*(B+C)*a-B*C
assert P(1979)<0<P(1981)
xs=[-100,-99,0,99,100]
U=[1+2*(2*x-199)**2 for x in xs]
ratio_lower=Fraction(8**10*prod((1979+x*x)**3 for x in xs),1981**5*prod(u**3 for u in U))
assert ratio_lower>5
print('Exact subunit comparison: likelihood_ratio^4 >',ratio_lower,'> 5',file=__import__('sys').stderr)

# Exact sufficient comparison for std with sd held, both mean and shape free.
# At mean zero, unimodality and variance one imply f(b)<=3/(2*b^3).
# The mean=b, sd=1, nu=3 witness has likelihood 4/(pi^2*(1+4*b^2)^2).
# pi<4 and the following integer inequality prove strict improvement.
b=100
assert b**6>9*(1+4*b*b)**2
print('Exact std nonuniqueness comparison:',b**6,'>',9*(1+4*b*b)**2,file=__import__('sys').stderr)
