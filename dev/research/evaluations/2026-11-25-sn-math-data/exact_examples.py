"""Rational enclosures for the exact examples in the report."""
from fractions import Fraction as F
from math import factorial, isqrt


def point(x):
    return F(x), F(x)


def add(a, b):
    return a[0] + b[0], a[1] + b[1]


def neg(a):
    return -a[1], -a[0]


def mul(a, b):
    products = [x*y for x in a for y in b]
    return min(products), max(products)


def div(a, b):
    assert b[0] > 0
    return mul(a, (1/b[1], 1/b[0]))


def sqrt(a):
    scale = 10**32
    lows = [F(isqrt(x.numerator*scale**2//x.denominator), scale) for x in a]
    return lows[0], lows[1]+F(1, scale)


def power(a, k):
    assert a[0] >= 0
    return a[0]**k, a[1]**k


def atan(x):
    lo = sum((-1)**j*x**(2*j+1)/(2*j+1) for j in range(40))
    return lo, lo+x**81/81


PI = add(mul(point(16), atan(F(1, 5))), neg(mul(point(4), atan(F(1, 239)))))
M = sqrt(div(point(2), PI))


def two_phi(x):
    if x < 0:
        return add(point(2), neg(two_phi(-x)))
    assert x < 2
    terms = [x**(2*j+1)/(2**j*factorial(j)*(2*j+1)) for j in range(32)]
    lo = sum((-1)**j*t for j, t in enumerate(terms))
    next_term = x**65/(2**32*factorial(32)*65)
    return add(point(1), mul(M, (lo, lo+next_term)))


def exp(x):
    if x < 0:
        return div(point(1), exp(-x))
    assert x < 1
    partial = sum(x**j/factorial(j) for j in range(25))
    return partial, partial + 2*x**25/factorial(25)


def log_at(x):
    assert x > 1
    z = (x-1)/(x+1)
    lo = 2*sum(z**(2*j+1)/(2*j+1) for j in range(30))
    return lo, lo+2*z**61/(61*(1-z*z))


def sn_example():
    x = (-1, 0, 0, 0, 0, 0, 1)
    n = len(x)
    eta, h, alpha = F('1.748'), F('-0.357'), F('0.5')
    gain = mul(point((2*eta**2/n)**n), exp(n-2*eta**2-n*h*h))
    for v in x:
        gain = mul(gain, power(two_phi(alpha*(eta*v-h)), 2))
    assert F('1.0009') < gain[0] <= gain[1] < F('1.0010')
    # Squared likelihood ratio face/normal = (4*2/9)^n.
    assert F(8, 9)**n < 1
    print('sn squared witness/normal ratio:', *(float(v) for v in gain))


def snorm_examples():
    # All free, sample (-2, -1 repeated 10, +1 repeated 10, +2).
    side_count, side_sq, side_abs = 11, 14, 12
    assert 3*side_count*side_sq-4*side_abs**2 == -114
    assert 8*side_sq == 112 < 10+90+16 == 116
    # Held mean zero and sd 151/200, reproduce the prior exact example.
    sd = point(F(151, 200))
    h2 = add(point(1), neg(power(M, 2)))
    r = div(sqrt(add(point(1), mul(h2, point(F(35, 6)**2)))), sd)
    e = mul(M, point(F(35, 6)))
    normalizer_ratio = mul(mul(point(F(12, 37)), sd), r)
    log_ratio = log_at(normalizer_ratio[0])[0], log_at(normalizer_ratio[1])[1]
    loss = mul(point(F(1, 2)), add(
        mul(point(36), power(add(r, neg(e)), 2)),
        div(add(power(e, 2), power(add(r, e), 2)), point(36)),
    ))
    gain = add(add(mul(point(3), log_ratio), div(point(1), power(sd, 2))), neg(loss))
    endpoint_sq = div(mul(power(M, 2), power(sd, 2)), h2)
    assert F('0.07685') < gain[0] <= gain[1] < F('0.07686')
    assert endpoint_sq[1] < F('0.999')
    print('snorm held log-likelihood gain:', *(float(v) for v in gain))
    print('snorm free curvature and endpoint inequalities: exact')


if __name__ == '__main__':
    sn_example()
    snorm_examples()
