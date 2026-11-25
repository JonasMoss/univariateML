"""Exact coefficient certificates for both size-three vertex types."""
import sympy as sp

a, b, z = sp.symbols("a b z", positive=True)
t = a+b
A = [sum(1/(a+j) for j in range(k)) for k in range(4)]
B = [sum(1/(b+j) for j in range(3-k)) for k in range(4)]
C = sum(1/(t+j) for j in range(3))
D = sum(1/(t+j)**2 for j in range(3))
common_denominator = (a+1)*(b+1)*(b+2)*t**3*(t+1)**3*(t+2)**2


def vertex(cells):
    i, j, k = cells
    u, v = A[i]-A[j], A[k]-A[j]
    r, s = B[i]-B[j], B[k]-B[j]
    determinant = u*s-v*r
    left = sp.factor(((C-A[j])*s-(C-B[j])*v)/determinant)
    right = sp.factor(((C-B[j])*u-(C-A[j])*r)/determinant)
    return left, sp.factor(1-left-right), right


def curvature_numerator(cells, weights, denominator):
    U = sum(-w*sp.diff(A[k], a) for w,k in zip(weights,cells))
    V = sum(-w*sp.diff(B[k], b) for w,k in zip(weights,cells))
    return sp.Poly(sp.cancel(denominator*(U*V-D*(U+V))), a, b)


def coefficient_check(poly, count, minimum, constant):
    assert len(poly.coeffs()) == count
    assert min(poly.coeffs()) == minimum > 0
    assert poly.coeff_monomial(1) == constant > 0
    print(count, "positive coefficients, minimum", minimum,
          "constant", constant, flush=True)


if __name__ == "__main__":
    cells = (0,1,2)
    weights = vertex(cells)
    F = 3*a*a*b+4*a*a+5*a*b+6*a-3*b**3-3*b*b+2*b+2
    assert sp.cancel(weights[1] + a*F/(b*t*(t+1)*(t+2))) == 0
    assert sp.expand(F.subs(a,0)) == -3*b**3-3*b*b+2*b+2
    assert sp.expand(F.subs(b,a+sp.Rational(1,2))) == 3*a*a+sp.Rational(21,4)*a+sp.Rational(15,8)
    N = curvature_numerator(cells, weights, b**3*common_denominator)
    transformed = sp.Poly(N.as_expr().subs(b,a+sp.Rational(1,2)+z),a,z)
    integer_primitive = transformed.clear_denoms()[1].primitive()[1]
    coefficient_check(integer_primitive,45,2304,6855)

    cells = (0,1,3)
    weights = vertex(cells)
    G = 3*a**3+3*a*a-3*a*b*b-5*a*b-2*a-4*b*b-6*b-2
    h = 3*a*b+2*a+4*b+2
    assert sp.cancel(weights[0]+b*(b+2)*G/(t*(t+1)*(t+2)*h)) == 0
    outside = sp.Poly(G.subs(a,b+1+z),b,z)
    assert min(outside.coeffs()) > 0 and outside.coeff_monomial(1) == 2
    N = curvature_numerator(cells, weights, (a+2)*h*h*common_denominator)
    polynomial_in_a = sp.Poly(N.as_expr(),a)
    degree = polynomial_in_a.degree()
    assert degree == 8
    transformed = sp.Poly(sum(c*((b+1)*z)**i*(1+z)**(degree-i)
                              for (i,),c in polynomial_in_a.terms()),b,z)
    coefficient_check(transformed,93,135,192)
