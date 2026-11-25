"""Rational sign and curvature cover for the two TASK-342 crossings."""
from fractions import Fraction as F
from functools import lru_cache
from math import isqrt

TAU, JOIN, END = F(17, 16), F(3), F(64)
DL, DR = F(35, 4), F(37, 4)


def atan_bounds(q, pairs):
    z = F(1, q)
    lower = sum(((-1)**j*z**(2*j+1)/(2*j+1) for j in range(2*pairs)), F(0))
    return lower, lower+z**(4*pairs+1)/(4*pairs+1)


pa, pb = atan_bounds(5, 5)
qa, qb = atan_bounds(239, 2)
PI_LO, PI_HI = 16*pa-4*qb, 16*pb-4*qa


def log_series(r):
    z = (r-1)/(r+1)
    step = z*z
    power = z
    lower = F(0)
    for j in range(16):
        lower += power/(2*j+1)
        power *= step
    return 2*lower, 2*(lower+power/(33*(1-step)))


LOG2 = log_series(F(2))


def div_round(x, bits):
    return F((x.numerator << bits)//x.denominator, 1 << bits)


@lru_cache(maxsize=32768)
def log_bounds(x):
    assert x > 0
    e = x.numerator.bit_length()-x.denominator.bit_length()
    unit = F(2)**e
    if x < unit:
        e -= 1
        unit /= 2
    a, b = log_series(x/unit)
    ea, eb = sorted((e*LOG2[0], e*LOG2[1]))
    return div_round(a+ea, 48), -div_round(-b-eb, 48)


@lru_cache(maxsize=8192)
def normalizer_derivatives(v):
    # b'(v)+b'(v+1)=1/v. Shift by 64, then use the tanh bounds.
    shift = 64
    d1 = sum(((-1)**j/(v+j) for j in range(shift)), F(0))
    d2 = -sum(((-1)**j/(v+j)**2 for j in range(shift)), F(0))
    z = v+shift
    d1 += 1/(2*z)+1/(4*z*z)
    d2 -= 1/(2*z*z)+1/(2*z**3)
    return (d1-1/(8*z**4), d1), (d2, d2+1/(2*z**5))


@lru_cache(maxsize=32768)
def scale_bounds(nu, d, eps):
    n, w = 200+eps, 99+eps
    target = n/(nu+1)
    y = d*d
    b = target*(1+y)-w-y
    c = (target-w-1)*y
    disc = b*b-4*target*c
    r = F(isqrt((disc.numerator << 100)//disc.denominator), 1 << 50)
    return (r-b)/(2*target), (r+F(1, 1 << 50)-b)/(2*target)


def penalty_bounds(u0, u1, d0, d1, eps):
    w = 99+eps
    return (w*log_bounds(1+1/u1)[0]+log_bounds(1+d0*d0/u1)[0],
            w*log_bounds(1+1/u0)[1]+log_bounds(1+d1*d1/u0)[1])


def tube(a, b, d0, d1, eps):
    return scale_bounds(a, d0, eps)[0], scale_bounds(b, d1, eps)[1]


def score_bounds(a, b, d0, d1, eps):
    u0, u1 = tube(a, b, d0, d1, eps)
    p0, p1 = penalty_bounds(u0, u1, d0, d1, eps)
    n = 200+eps
    return (n*normalizer_derivatives(b)[0][0]-p1/2,
            n*normalizer_derivatives(a)[0][1]-p0/2)


def curvature_bounds(a, b, d0, d1, eps):
    u0, u1 = tube(a, b, d0, d1, eps)
    n, w = 200+eps, 99+eps
    y0, y1 = d0*d0, d1*d1
    s0 = w/(u1+1)**2+y0/(u1+y1)**2
    s1 = w/(u0+1)**2+y1/(u0+y0)**2
    return (n*normalizer_derivatives(a)[1][0]+n*n/(2*(b+1)**3*u1*s1),
            n*normalizer_derivatives(b)[1][1]+n*n/(2*(a+1)**3*u0*s0))


def cover(eps):
    boxes = [(TAU, JOIN, DL, DR, 1, 0), (JOIN, END, DL, DR, -1, 0),
             (JOIN, JOIN, DL, DR, 0, 0)]
    leaves = 0
    while boxes:
        a, b, d0, d1, sign, depth = boxes.pop()
        low, high = score_bounds(a, b, d0, d1, eps)
        closed = low > 0 or (sign and high < 0)
        if not closed and sign:
            lo, hi = curvature_bounds(a, b, d0, d1, eps)
            closed = lo > 0 if sign == 1 else hi < 0
        if closed:
            leaves += 1
            continue
        assert depth < 32, (a, b, d0, d1, sign)
        if (b-a)/(a+1) > (d1-d0)/d0:
            mid = (a+b)/2
            boxes.extend([(a, mid, d0, d1, sign, depth+1),
                          (mid, b, d0, d1, sign, depth+1)])
        else:
            mid = (d0+d1)/2
            boxes.extend([(a, b, d0, mid, sign, depth+1),
                          (a, b, mid, d1, sign, depth+1)])
    # Positive inverse-shape score from the outlier alone for nu >= 64.
    v_hi = (99+eps+DR*DR)/(200+eps)
    assert DL**4 > 3*(200+eps)*(v_hi+DR*DR/END)**2
    print("cover leaves", eps, leaves, flush=True)


def b_bounds(nu):
    lo = log_bounds(2*nu/PI_HI)[0]/2-1/(4*nu)
    hi = log_bounds(2*nu/PI_LO)[1]/2-1/(4*nu)+1/(24*nu**3)
    if nu == 1:
        return log_bounds(2/PI_HI)[0], log_bounds(2/PI_LO)[1]
    return lo, hi


def value_bounds(a, b, d, eps):
    u0, u1 = tube(a, b, d, d, eps)
    p0, p1 = penalty_bounds(u0, u1, d, d, eps)
    n = 200+eps
    return (n*(b_bounds(a)[0]-log_bounds(u1)[1]/2)-(b+1)*p1/2,
            n*(b_bounds(b)[1]-log_bounds(u0)[0]/2)-(a+1)*p0/2)


def low_bounds(d, eps):
    if eps:
        return value_bounds(F(1), F(1), d, eps)
    return (200*b_bounds(F(1))[0]-2*log_bounds(d)[1],
            200*b_bounds(F(1))[1]-2*log_bounds(d)[0])


def winner_bracket(d, eps):
    a, b = JOIN, END
    while b-a > F(1, 10000):
        x, y = (2*a+b)/3, (a+2*b)/3
        sx, sy = score_bounds(x, x, d, d, eps), score_bounds(y, y, d, d, eps)
        if sy[0] > 0:
            a = y
        elif sx[1] < 0:
            b = x
        elif sx[0] > 0 and sy[1] < 0:
            a, b = x, y
        elif sx[0] > 0:
            a = x
        elif sy[1] < 0:
            b = y
        else:
            raise AssertionError((x, y, sx, sy))
    return a, b


def crossing_bracket(eps):
    for d, sign in ((DL, 1), (DR, -1)):
        a, b = winner_bracket(d, eps)
        vl, vh = value_bounds(a, b, d, eps)
        ll, lh = low_bounds(d, eps)
        lo, hi = vl-lh, vh-ll
        assert lo > 0 if sign == 1 else hi < 0
        # Print outward decimal-friendly dyadic bounds.
        print("crossing endpoint", eps, d, "height gap in",
              div_round(lo, 12), -div_round(-hi, 12), flush=True)


if __name__ == "__main__":
    for epsilon in (F(0), F(1, 1024)):
        cover(epsilon)
        crossing_bracket(epsilon)
