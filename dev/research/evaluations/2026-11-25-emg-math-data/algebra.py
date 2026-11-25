"""Exact moment and scaled-profile calculations for TASK-346."""
import sympy as s

z, u = s.symbols("z u")
m = [1, 0, 1, 0, 3, 0, s.Symbol("m6")]
h = [s.hermite_prob(i, z) for i in range(7)]
poly = s.Poly(2*h[6]/9-h[3]**2/18, z)
assert s.expand(sum(c*m[i[0]] for i, c in poly.terms())) == (m[6]-17)/6

weight = u**2*(u-1)*(2-u)
b = s.integrate(u*weight, (u, 1, 2))/s.integrate(weight, (u, 1, 2))
q = s.integrate(-6*u**2*(u-1)*(u-b)*(u-2), u)
assert q.subs(u, 1) == q.subs(u, 2) > 0
print("b, q, plateau:", b, q, q.subs(u, 1))

sites = range(-3, 4)
vandermonde = s.Matrix([[x**i for x in sites] for i in range(7)])
base = vandermonde.inv()*s.Matrix([1, 0, 1, 0, 3, 0, 15])
assert min(base) > 0
print("rational base weights:", list(base))
