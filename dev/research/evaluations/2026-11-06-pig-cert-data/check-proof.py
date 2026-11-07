"""Symbolic identities used in the hand proof; positivity is proved in prose."""
import sympy as s
e,k,z,w,r=s.symbols('e k z w r',positive=True)
c=k*(k-1)
b=e*(k*k-e*e)/(k*k+e*e)
at_cross=-(c-e*e-e-b)+(c/e+e-b/e+s.diff(b,e))*b
expected=2*e*k*(e+k)**2*(e**3+e**2*k-e*k**2+k**3)/(e**2+k**2)**3
assert s.factor(at_cross-expected)==0
curvature=-4*w+2*r*(3+w*w)-2*r**3*(1+w*w)**2/(w*w*(1+r*r))
factored=2*(r-w)**2*(r*w*w-r-2*w)/(w*w*(1+r*r))
assert s.factor(curvature-factored)==0
print('PASS: barrier derivative and concavity factorization identities')
