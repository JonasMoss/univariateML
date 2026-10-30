"""Exact rational certificates for the README half-mass slash examples.

The normal-density constant cancels in L/B. Taylor's remainder gives
1-exp(-t) >= h(t) for positive t; the asserted h values are positive.
Run with Python 3 (standard library only, without -O).
"""
from fractions import Fraction as F
from math import factorial
# For t>0, Taylor with Lagrange remainder gives 1-exp(-t) >= h(t).
def h(t): return sum(((-1)**(j+1)*t**j/F(factorial(j))) for j in range(1,13))
assert all(h(t)>0 for t in (F(8,9),F(25,18),F(81,98),F(121,98)))
# Half-mass, zero reciprocal sum: x=(0*10,-1,9*9), m=4,s=3.
r9=(F(3,16)*F(3,25)*h(F(8,9))*h(F(25,18)))**10 * 2**10*9**18
assert r9>F(145605,100000)
# Two finite modes: x=(0*10,-1,10*9).
# Positive m=9/2,s=7/2; all nonzero observations have distance 11/2.
r10pos=(F(14,81)*F(14,121)*h(F(81,98))*h(F(121,98)))**10 * 2**10*10**18
assert r10pos>F(106613,100000)
# Negative m=-1/625,s=1/5. Bound all ten nonzero exponential
# remainders by 1/250000, and use Bernoulli for the ten zero factors.
t=F(624,625)**2/F(2,25)
assert sum(t**j/F(factorial(j)) for j in range(41))>250000
r10neg=F(625,624)**2*F(6250,6251)**18 * F(6249,6250)*F(24999,25000)
assert r10neg>F(100012,100000)
print('Exact rational lower bounds on L/B:',float(r9),float(r10pos),float(r10neg))
