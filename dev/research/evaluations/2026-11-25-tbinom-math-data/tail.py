"""Certified tail orientation. Algebraic finite windows, analytic lower truncation."""
from fractions import Fraction
from math import factorial, lcm
from mpmath import iv
from sympy import Poly, Rational, cancel, diff, expand, prod, resultant, symbols
from check import bracket, enclosing, finite, interval

s, z = symbols("s z")


def bounded_tail(lower, upper, counts):
    n = sum(counts)
    S = sum(k*c for k, c in enumerate(counts, lower))
    b = {k: prod(1-j*s for j in range(1, k))*Rational(1, factorial(k))
         for k in range(lower, upper+1)}
    Z = sum(b[k]*z**(k-lower) for k in b)
    H = sum((n*k-S)*b[k]*z**(k-lower) for k in b)
    D = prod(1-j*s for j in range(1, upper))
    R = expand(Z*sum(c*cancel(D*diff(b[k], s)/b[k])
                     for k, c in enumerate(counts, lower) if c)
               -n*D*diff(Z, s))
    roots = bracket([factorial(upper)//factorial(k) for k in b], counts)
    a, d = map(Rational, roots)
    factors = Poly(H, s, z).factor_list()[1]
    matching = []
    for p, _ in factors:
        at_zero = Poly(p.as_expr().subs(s, 0), z)
        if at_zero.degree() > 0 and at_zero.count_roots(a, d) == 1:
            matching.append(p.as_expr())
    assert len(matching) == 1
    branch = matching[0]
    if Poly(R, z, domain='QQ(s)').rem(Poly(branch, z, domain='QQ(s)')).is_zero:
        return 2*upper, 0
    critical = Poly(resultant(branch, R, z), s)
    assert not critical.is_zero
    # Factor the zero at s=0 and isolate positive roots of the remaining
    # polynomial. Use half of a strictly positive rational lower endpoint.
    order = min(critical.monoms())[0]
    critical = Poly.from_dict({(j[0]-order,): c for j, c in critical.terms()}, s)
    cap = Rational(1, 2*upper)
    for (left, right), _ in critical.intervals(eps=Rational(1, 10**8)):
        if right > 0:
            if left <= 0:
                # Always works even if a positive root is very near zero.
                coefficients = critical.all_coeffs()
                bound = abs(coefficients[-1])/(2*sum(map(abs, coefficients[:-1])))
                cap = min(cap, bound)
            else:
                cap = min(cap, left/2)
            break
    end = int(1/cap) + 1
    normalized = [Rational(w.subs(s, Rational(1, end))) for w in b.values()]
    denom = lcm(*(int(w.q) for w in normalized))
    integer_weights = [int(w*denom) for w in normalized]
    r_at_end = Poly(R.subs(s, Rational(1, end)), z)
    bits = 100
    while True:
        left, right = map(Rational, bracket(integer_weights, counts, bits))
        if r_at_end.count_roots(left, right) == 0:
            sign = 1 if r_at_end.eval(left) > 0 else -1
            return end, sign
        bits *= 2


def observed_weights(t, rate, upper):
    weight = iv.mpf(1)
    slope = iv.mpf(0)
    for k in range(upper+1):
        yield k, weight, slope
        factor = (1-k*t)*rate/(k+1)
        slope = slope*factor - weight*k*rate/(k+1)
        weight *= factor


def normalizer(t, rate, lower, degree=24):
    """Enclose Z, Z_s and z*Z_z for an unbounded upper window."""
    u = t*rate
    r = u.b
    assert r < 1
    value = sum((-u)**j/(j+1) for j in range(degree+1))
    derivative = sum((-1)**j*j*u**(j-1)/(j+1) for j in range(1, degree+1))
    value_error = r**(degree+1)/(1-r)
    deriv_error = r**degree/(1-r)
    value += iv.mpf([-value_error.b, value_error.b])
    derivative += iv.mpf([-deriv_error.b, deriv_error.b])
    exponential = iv.exp(rate*value)
    Z = exponential
    ds = exponential*rate**2*derivative
    dz = exponential*rate/(1+u)
    for k, w, ws in observed_weights(t, rate, lower-1):
        Z -= w
        ds -= ws
        dz -= k*w
    return Z, ds, dz


def lower_tail(lower, counts):
    n = sum(counts)
    S = sum(k*c for k, c in enumerate(counts, lower))
    mean = interval(Fraction(S, n))
    # Rational endpoints bracket the face score. This search runs on the
    # real samples and can use wider degree/precision for very narrow gaps.
    zero = iv.mpf(0)
    lo, hi = Fraction(0), Fraction(S, n)+1
    for _ in range(55):
        mid = (lo+hi)/2
        Z, _, moment = normalizer(zero, interval(mid), lower)
        score = moment-mean*Z
        if score.a > 0:
            hi = mid
        elif score.b < 0:
            lo = mid
        else:
            raise ArithmeticError("face score enclosure contains zero")
    rate = enclosing((lo, hi))
    Z, _, _ = normalizer(zero, rate, lower)
    face = sum(c * (k*iv.log(rate)-iv.log(factorial(k))-iv.log(Z))
               for k, c in enumerate(counts, lower) if c)
    # Widen the rate box symmetrically to avoid demanding a cutoff large
    # enough to keep the root within its narrow face-only bracket.
    mid = (lo+hi)/2
    for bits in range(2, 56):
        radius = mid/2**bits
        box = enclosing((mid-radius, mid+radius))
        face_Z, face_ds, _ = normalizer(zero, box, lower)
        empirical_at_zero = -sum(c*k*(k-1)//2 for k,c in enumerate(counts, lower))
        first = empirical_at_zero - n*face_ds/face_Z
        if first.a <= 0 <= first.b:
            continue
        for power in range(3, 160):
            end = 2**power
            if end <= max(lower+len(counts), (mid+radius)*2):
                continue
            t = enclosing((Fraction(0), Fraction(1, end)))
            Zlo, _, mlo = normalizer(t, interval(mid-radius), lower)
            Zhi, _, mhi = normalizer(t, interval(mid+radius), lower)
            if not ((mlo-mean*Zlo).b < 0 < (mhi-mean*Zhi).a):
                continue
            Z, ds, _ = normalizer(t, box, lower)
            if Z.a <= 0:
                continue
            derivative = -n*ds/Z - sum(c*j/(1-j*t) for k,c in enumerate(counts, lower)
                                       for j in range(1,k) if c)
            if derivative.a > 0 or derivative.b < 0:
                return end, 1 if derivative.a > 0 else -1, face, rate
    raise ArithmeticError("tail sign was not separated at working precision")


def rational_face_terms(lower, upper, counts, rate):
    raw = [Fraction(rate**k, factorial(k)) for k in range(lower, upper+1)]
    q = [w/sum(raw) for w in raw]
    p = [Fraction(c,sum(counts)) for c in counts]
    p += [Fraction(0)] * (len(q)-len(p))
    k = list(range(lower, upper+1))
    a = [-Fraction(x*(x-1),2) for x in k]
    b = [-Fraction(x*(x-1)*(2*x-1),12) for x in k]
    def expectation(weights, values):
        return sum(w*v for w,v in zip(weights, values))
    mean = expectation(q,k)
    assert mean == expectation(p,k)
    qa = expectation(q,a)
    variance = expectation(q, [(x-mean)**2 for x in k])
    covariance = expectation(q, [(x-mean)*(y-qa) for x,y in zip(k,a)])
    h = [y-qa-covariance*(x-mean)/variance for x,y in zip(k,a)]
    return (expectation(p,a)-qa,
            expectation(p,b)-expectation(q,b)-expectation(q,[v*v for v in h])/2)


if __name__ == "__main__":
    examples = [(0, 4, [2358,0,2927,4714,1], 1),
                (0, 4, [48,48,24,8,2], -1),
                (0, 4, [50,41,33,3,3], -1),
                (0, 4, [104,0,0,608,112], 1),
                (0, 4, [242187,60672,121344,1488980,406274], 1),
                (2, 3, [2,2], 0)]
    for lower, upper, counts, expected in examples:
        end, sign = bounded_tail(lower, upper, counts)
        print("bounded", lower, upper, counts, end, sign, flush=True)
        assert sign == expected
    for counts, rate, expected in (
        ([48,48,24,8,2], 1, (0, -1)),
        ([50,41,33,3,3], 1, (0, -1)),
        ([104,0,0,608,112], 4, (0, 1)),
        ([242187,60672,121344,1488980,406274], 4, (0, 0)),
    ):
        terms = rational_face_terms(0,4,counts,rate)
        assert (terms[0], (terms[1] > 0) - (terms[1] < 0)) == expected
        print("exact coefficients", counts, *terms)
    for counts in ([42,102,142,113,71,26,3,1], [1,0,0,1]):
        end, sign, face, rate = lower_tail(1, counts)
        print("unbounded", counts, end, sign, face, rate)
        if sign == -1:
            vals = [finite(k,1,float('inf'),counts)[0]
                    for k in range(len(counts), end)]
            assert all(v.b < face.a for v in vals)
