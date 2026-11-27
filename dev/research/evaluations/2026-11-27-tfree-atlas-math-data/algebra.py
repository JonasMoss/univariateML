"""Exact rational checks of the small-sample comparisons."""
from fractions import Fraction as F
from math import prod

from sympy import Poly, symbols

m, z, b = symbols('m z b', real=True)
d1, d2 = (m-b)**2+z, (m+b)**2+z
assert Poly(d1*d2 - ((m*m+z-b*b)**2+4*b*b*z), m, z, b).is_zero

x = (-100, -99, 0, 99, 100)
score_poly = lambda a: 5*a*a+39602*a-98010000
assert score_poly(1979) < 0 < score_poly(1981)
ratio4_lower = F(8**10*prod((1979+r*r)**3 for r in x),
                 1981**5*prod((1+2*(2*r-199)**2)**3 for r in x))
assert ratio4_lower > 5

assert 100**6 > 9*(1+4*100**2)**2
reciprocals = (F(1), F(8, 9), F(4, 5))
gain = sum(reciprocals)**2/2-sum(r*r for r in reciprocals)
assert gain > 0

T = m*m+z
product_at_critical = T*((m-1)**2+z)*((m+1)**2+z)
assert Poly(product_at_critical - z - (T*(T-1)**2+(4*T-1)*z), m, z).is_zero

assert 2**40 > 2**6*4**10*2**4  # Cauchy witness at epsilon=2^-20
print('Exact identities and inequalities passed.')
