"""Exact polynomial certificate; run through /tmp/umvl-cpu/run (SymPy)."""
import hashlib
import json
from functools import reduce
from math import comb, lcm
from pathlib import Path
import sympy as s

A, u, v, w = s.symbols('A u v w')
# Coefficients of exp(A*z+u*z^2+v*z^3+w*z^4), excluding coefficient zero.
B = [A, A*A/2+u, A**3/6+A*u+v,
     A**4/24+A*A*u/2+u*u/2+A*v+w]
P = s.prod(B)
# Construct t*d(ell)/dt times its positive denominator P along increasing
# dispersion. Q=-N below; A+2u+3v+4w=2.
N = 5*(u+2*v+3*w)*P+(A-2)*s.diff(P,A)
N += u*s.diff(P,u)+v*s.diff(P,v)+w*s.diff(P,w)
Q = s.Poly(s.expand(-N.subs(A,2-2*u-3*v-4*w)),u,v,w)
t = s.symbols('t0:4')
S = sum(t)
degree = Q.total_degree()
H = s.Poly(s.expand(sum(c*t[1]**i*(s.Rational(2,3)*t[2])**j*(t[3]/2)**k
                       *S**(degree-i-j-k) for (i,j,k),c in Q.terms())),*t)
den = reduce(lcm,(int(c.q) for _,c in H.terms()),1)
coefficients = {m:int(c*den) for m,c in H.terms()}
negative_counts = []
for elevation in range(5):
    negative_counts.append(sum(c<0 for c in coefficients.values()))
    if elevation == 4:
        break
    new = {}
    for monomial, value in coefficients.items():
        for j in range(4):
            exponent = list(monomial)
            exponent[j] += 1
            key = tuple(exponent)
            new[key] = new.get(key,0)+value
    coefficients = {m:c for m,c in new.items() if c}
assert degree == 11
assert negative_counts == [21,11,4,1,0]
assert all(c>0 for c in coefficients.values())
# Strictness near the Poisson vertex: a nonzero v or w gives a positive
# linear term. On the pure-u ray, the leading term is positive cubic.
assert Q.coeff_monomial(v) == s.Rational(40,9)
assert Q.coeff_monomial(w) == s.Rational(112,9)
assert Q.coeff_monomial(u) == Q.coeff_monomial(u*u) == 0
assert Q.coeff_monomial(u**3) == s.Rational(40,9)
encoded = json.dumps(sorted(coefficients.items()),separators=(',',':')).encode()
result = dict(arithmetic='exact rational/integer', degree=degree+4,
              negative_coefficients_by_elevation=negative_counts,
              positive_coefficients=len(coefficients),
              zero_coefficients=comb(degree+7,3)-len(coefficients),
              denominator=den,minimum_positive_integer=min(coefficients.values()),
              integer_coefficient_sha256=hashlib.sha256(encoded).hexdigest())
Path(__file__).with_name('compound-poisson.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
