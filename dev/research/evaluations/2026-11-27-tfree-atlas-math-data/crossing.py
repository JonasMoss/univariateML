"""Outward-rounded bounds for the exact real Student shape crossing."""
from decimal import Decimal as D, getcontext
from functools import lru_cache
import json

getcontext().prec = 60


class Interval:
    def __init__(self, lo, hi=None):
        self.lo = D(lo)
        self.hi = self.lo if hi is None else D(hi)

    def __add__(self, other):
        other = as_interval(other)
        return Interval((self.lo + other.lo).next_minus(),
                        (self.hi + other.hi).next_plus())

    __radd__ = __add__

    def __neg__(self):
        return Interval(self.hi.copy_negate(), self.lo.copy_negate())

    def __sub__(self, other):
        return self + (-as_interval(other))

    def __rsub__(self, other):
        return as_interval(other) + (-self)

    def __mul__(self, other):
        other = as_interval(other)
        products = [x*y for x in (self.lo, self.hi) for y in (other.lo, other.hi)]
        return Interval(min(products).next_minus(), max(products).next_plus())

    __rmul__ = __mul__

    def __truediv__(self, other):
        other = as_interval(other)
        assert other.lo > 0 or other.hi < 0
        return self * Interval((1/other.hi).next_minus(), (1/other.lo).next_plus())

    def log(self):
        assert self.lo > 0
        return Interval(self.lo.ln().next_minus(), self.hi.ln().next_plus())


def as_interval(value):
    return value if isinstance(value, Interval) else Interval(value)


LEFT = tuple(map(D, ['.047824', '.828300', '.047379', '.162880', '.337914', '326.079611']))
RIGHT = tuple(map(D, ['.036672', '.772301', '.047294', '.144816', '.322601', '335.645682']))
DELTA = tuple(b-a for a, b in zip(LEFT, RIGHT))
COUNTS = (300000, 4000000, 4000000, 4800000, 3100000, 1)
TOTAL = sum(COUNTS)
LOW = D('.05')
SEPARATOR = D(30)
HIGH = D(4000000)
EARLY = D('5.5')
LATE = D(158)


def average(values):
    return sum(w*x for w, x in zip(COUNTS, values))/TOTAL


def radii(t):
    return tuple(a+t*d for a, d in zip(LEFT, DELTA))


def squared(rs):
    return tuple(r*r for r in rs)


@lru_cache(maxsize=None)
def constants(v):
    """A(v)-A(infinity) and the digamma difference D(v)."""
    r = v
    log_sum, digamma_sum = Interval(0), Interval(0)
    while r < 80:
        denominator = Interval(r)*(r+2)
        log_sum += (1 + Interval(1)/denominator).log()/2
        digamma_sum += Interval(2)/(Interval(r)*(r+1))
        r += 2
    u = Interval(1)/r
    a = -u/4
    d = u + u*u/2
    a = Interval(a.lo, (a+u*u*u/24).hi)
    d = Interval((d-u*u*u*u/4).lo, d.hi)
    return a-log_sum, d+digamma_sum


@lru_cache(maxsize=65536)
def scale_at(v, q):
    """Bracket the unique a^2 using two independently checked score signs."""
    v_float = float(v)
    q_float = tuple(map(float, q))
    lo, hi = 0.0, v_float*sum(w*x for w, x in zip(COUNTS, q_float))/TOTAL
    for _ in range(52):
        mid = (lo+hi)/2
        score = sum(w*x/(mid+x) for w, x in zip(COUNTS, q_float))
        if (v_float+1)*score > TOTAL:
            lo = mid
        else:
            hi = mid
    mid = D.from_float((lo+hi)/2)
    lo, hi = mid*D('.9999999999'), mid*D('1.0000000001')

    def score(z):
        return (v+1)*average([Interval(x)/(Interval(z)+x) for x in q])

    assert score(lo).lo > 1 and score(hi).hi < 1, (v, q)
    return Interval(lo, hi)


def scale_box(vlo, vhi, q):
    # a^2 increases in shape and in each squared residual.
    lo = scale_at(vlo, tuple(x.lo for x in q)).lo
    hi = scale_at(vhi, tuple(x.hi for x in q)).hi
    return Interval(lo, hi)


def value(v, q):
    z = scale_box(v, v, q)
    return constants(v)[0]-(z/v).log()/2-(v+1)*average([(1+x/z).log() for x in q])/2


def shape_derivative(lo, hi, q):
    z = scale_box(lo, hi, q)
    d = Interval(constants(hi)[1].lo, constants(lo)[1].hi)
    return (d-average([(1+x/z).log() for x in q]))/2


def path_derivative(v, rs):
    q = squared(rs)
    if v is None:
        return -average([r*d for r, d in zip(rs, DELTA)])/average(q)
    z = scale_box(v, v, q)
    return -(v+1)*average([r*d/(z+x) for r, d, x in zip(rs, DELTA, q)])


def path_gap(a, b, lo, hi):
    mid = (lo+hi)/2
    q = squared(radii(Interval(mid)))
    va = value(a, q)
    vb = -Interval('.5')-average(q).log()/2 if b is None else value(b, q)
    derivative = path_derivative(a, radii(Interval(lo, hi)))-path_derivative(
        b, radii(Interval(lo, hi)))
    return va-vb+Interval(lo-mid, hi-mid)*derivative


def cover(lo, hi, check, margin, depth=24):
    """Cover the closed interval. Exhausted subdivisions fail the proof."""
    pending = [(lo, hi, depth)]
    count, smallest = 0, D('Infinity')
    while pending:
        left, right, remaining = pending.pop()
        bound = check(left, right)
        if bound > margin:
            count += 1
            smallest = min(smallest, bound)
            continue
        assert remaining > 0, (left, right, bound, margin)
        mid = (left+right)/2
        pending.extend(((left, mid, remaining-1), (mid, right, remaining-1)))
    return [count, str(smallest)]


def endpoint_gap(t, winner, lo, hi):
    q = squared(radii(Interval(t)))
    witness = value(winner, q).lo

    def check(a, b):
        mid = (a+b)/2
        bound = value(mid, q)+Interval(a-mid, b-mid)*shape_derivative(a, b, q)
        return (Interval(witness)-bound).lo

    return cover(lo, hi, check, D('.000001'))


def low_shape(lo, hi):
    def check(a, b):
        return shape_derivative(a, b, squared(radii(Interval(lo, hi)))).lo
    return cover(LOW, D(2), check, D(0))


def tail_bounds():
    rs = radii(Interval(0, 1))
    q = squared(rs)
    low = Interval(LOW/2).log()-average([r.log() for r in rs])+1
    # At s=.3 the scale score is positive throughout the high tail.
    s0_sq = Interval('.09')
    max_q = max(x.hi for x in q)
    assert (average(q)/(s0_sq+Interval(max_q)/HIGH)).lo > 1
    normal_error = average([x*x for x in q])*(1+Interval(1)/HIGH)/(
        4*s0_sq*s0_sq*HIGH)
    # The independent cover below shows EARLY beats normal by .014.
    assert normal_error.hi < D('.014')
    # Enclose the EARLY profile from below using a fixed feasible s=.36.
    early_z = Interval('.1296')*EARLY
    early_lower = (constants(EARLY)[0]-(early_z/EARLY).log()/2
                   -(EARLY+1)*average([(1+x/early_z).log() for x in q])/2)
    assert early_lower.lo > low.hi
    return [str(low.hi), str(early_lower.lo), str(normal_error.hi)]


def main():
    report = {'tails': tail_bounds()}
    for a, b, margin in [(LATE, SEPARATOR, '.000005'), (EARLY, D(2), '.006'),
                          (EARLY, None, '.014')]:
        report[f'path gap {a} vs {b}'] = cover(
            D(0), D(1), lambda lo, hi: path_gap(a, b, lo, hi).lo, D(margin))
    low = [low_shape(D(j)/32, D(j+1)/32) for j in range(32)]
    report['low-shape derivative'] = [sum(entry[0] for entry in low),
                                      str(min(D(entry[1]) for entry in low))]
    report['left endpoint excludes right'] = endpoint_gap(D(0), EARLY, SEPARATOR, HIGH)
    report['right endpoint excludes left'] = endpoint_gap(D(1), LATE, D(2), SEPARATOR)
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
