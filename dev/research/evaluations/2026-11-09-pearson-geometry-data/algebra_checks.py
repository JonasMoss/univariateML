# Symbolic checks for the Pearson geometry notes.
import sympy as sp
b1, b2, g, k, c0, c1, c2, x = sp.symbols('beta1 beta2 gamma1 k c0 c1 c2 x', real=True)

# 1. Pearson's kappa and the type V / type III curves near the normal (beta1, beta2) = (0, 3).
kappa = b1*(b2 + 3)**2/(4*(4*b2 - 3*b1)*(2*b2 - 3*b1 - 6))
# type III: denominator factor 2 beta2 - 3 beta1 - 6 = 0  ->  beta2 = 3 + (3/2) beta1
typeIII = sp.solve(sp.Eq(2*b2 - 3*b1 - 6, 0), b2)[0]
# type V: kappa = 1; insert beta2 = 3 + k beta1 and take the leading order in beta1
eq = sp.numer(sp.together(kappa - 1)).subs(b2, 3 + k*b1)
lead = sp.Poly(sp.expand(eq), b1).all_coeffs()[-2]       # coefficient of beta1^1
kV = sp.solve(lead, k)
print("type III: beta2 =", typeIII, "  type V slope k (beta2 = 3 + k beta1):", kV)
assert typeIII == 3 + sp.Rational(3, 2)*b1 and kV == [sp.Rational(15, 8)]
print("in signed skewness gamma1 (beta1 = gamma1^2) both curves are 3 + k gamma1^2: tangent at the normal")

# 2. In the plane of quadratics [c0:c1:c2], the double-root conic c1^2 = 4 c0 c2 has tangent
#    line c2 = 0 (a root at infinity: type III) at [1:0:0] (double root at infinity: normal).
D = c1**2 - 4*c0*c2
grad = [sp.diff(D, v).subs({c0: 1, c1: 0, c2: 0}) for v in (c0, c1, c2)]
print("gradient of the discriminant at [1:0:0]:", grad, "-> tangent line c2 = 0")
assert grad == [0, 0, -4]

# 3. Lomax(alpha, lam) on (0, inf): -f'/f = (alpha+1)/(x+lam) = (alpha+1) x / (x (x+lam)),
#    keeping the endpoint 0 as a root of Q: [b0:b1:c0:c1:c2] = [0 : alpha+1 : 0 : lam : 1].
a, lam, r = sp.symbols('alpha lam r', positive=True)
f = a/lam*(1 + x/lam)**(-a - 1)
assert sp.simplify(-sp.diff(f, x)/f - (a + 1)*x/(x*(x + lam))) == 0
pt = sp.Matrix([0, a + 1, 0, lam, 1])/lam
lim = pt.subs(a, r*lam).applyfunc(lambda e: sp.limit(e, lam, sp.oo))
print("Lomax point / lam with alpha = r lam, lam -> oo:", list(lim), "= exponential with rate r, threshold 0")
assert list(lim) == [0, r, 0, 1, 0]
print("all algebra checks passed")
