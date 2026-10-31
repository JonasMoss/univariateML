#!/usr/bin/env python3
"""Assertions for the numerical examples in the research report."""
import mpmath as mp
from fractions import Fraction
mp.mp.dps = 40
assert (Fraction(1,256)/Fraction(9,4096)) == Fraction(16,9)
c=1/mp.sqrt(2*mp.pi)
x=[0]*10+[-1]+[9]*9
B=c**20*mp.mpf(2)**-10*mp.fprod(mp.mpf(abs(v))**-2 for v in x if v)
def density(v,mu,s):
    t=(mp.mpf(v)-mu)**2/(2*s*s)
    return c/(2*s) if not t else c/s*(-mp.expm1(-t))/(2*t)
ratio=mp.fprod(density(v,4,3) for v in x)/B
assert ratio > mp.mpf('1.456')
a=mp.mpf('0.001')
p=mp.betainc(a,a,0,mp.exp(-1000),regularized=True)
assert abs(p-mp.exp(-1)/2) < mp.mpf('0.001')
assert 2/(1+mp.mpf('0.001')) > mp.mpf('1.99')
print('PASS: exact beta-binomial ratio, slash witness, mixed-face probability, uniform-junction ratio')
