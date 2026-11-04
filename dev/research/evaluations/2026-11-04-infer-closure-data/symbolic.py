"""Fourth-order BB log-pmf normalization and effective-coordinate drift.

This checks algebra, not the uniform Taylor bounds used in the proof.
"""
import json
from pathlib import Path
import sympy as s
from sympy.functions.combinatorial.numbers import stirling

x, j, mu, u, k, z, d = s.symbols('x j mu u k z d')
N = s.symbols('N', integer=True, positive=True)

def trunc(poly, degree=4):
    return s.Add(*(coef*u**power[0]*k**power[1]
                   for power, coef in s.Poly(s.expand(poly), u, k).terms()
                   if sum(power)<=degree))

def expect(poly):
    return s.expand(sum(coef*sum(stirling(power[0], r, kind=2)*mu**r
                     for r in range(power[0]+1))
                     for power, coef in s.Poly(s.expand(poly), x).terms()))

# Expand zero probability by log(1+B-A)-log(1+B), then sum Faulhaber powers.
logzero = 0
for l in range(1, 6):
    for r in range(1, l+1):
        p = l-r
        power_sum = s.summation(j**p, (j, 0, N-1)).subs(N, 1/k)
        logzero += (-1)**(l+1)*s.binomial(l,r)*(-mu*k)**r*(mu*u*k)**p*power_sum/l
logzero = trunc(logzero)+mu
inc = 0
v = mu*(u-k)-mu*(j+1)*u*k
for l in range(1, 5):
    inc += (-(j*k)**l+(-1)**(l+1)*(j*u)**l-(-1)**(l+1)*v**l)/l
inc = trunc(inc)
logratio = trunc(logzero+s.summation(inc, (j, 0, x-1)))
normalizer = 1
for power in range(1, 5):
    normalizer += trunc(logratio**power)/s.factorial(power)
assert s.simplify(expect(trunc(normalizer)-1)) == 0

# Weight k as epsilon, d as epsilon^2, and cancel the quadratic C2 term.
effective = s.expand(logratio.subs(u, k+(mu+1)*k**2+d).subs({k:k*z, d:d*z**2}))
leading = s.expand(effective).coeff(z,2)
c2 = x*(x-1)-2*mu*x+mu**2
c3 = x*(x-1)*(x-2)-3*mu*x*(x-1)+3*mu**2*x-mu**3
assert s.expand(leading-d*c2/2+k*k*c3/3) == 0
assert s.simplify(expect(effective.coeff(z,3))) == 0
drift = s.simplify(expect(effective.coeff(z,4)))
assert s.expand(drift+mu**2*d*d/4+mu**3*k**4/3) == 0
result = {"order": 4, "normalization_through_order_4": "0",
          "effective_leading_log_ratio": str(leading),
          "effective_order_3_expectation": "0", "effective_order_4_expectation": str(drift),
          "log_ratio_polynomial": str(s.collect(logratio,[u,k]))}
path = Path(__file__).with_name('symbolic.json')
path.write_text(json.dumps(result, indent=2)+'\n')
print('Fourth-order algebra passed:', path)
