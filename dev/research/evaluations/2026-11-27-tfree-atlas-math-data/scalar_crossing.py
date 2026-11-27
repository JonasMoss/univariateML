"""Outward-rounded crossing for std with mean zero and sd one."""
from decimal import Decimal as D
import json

from crossing import Interval, average, constants, cover, squared

LEFT = tuple(map(D, ['.08387377', '1.62238777', '.08624025', '.37818028',
                     '.73883338', '734.657008']))
DIRECTION = tuple(b-a for a, b in zip(
    LEFT, map(D, ['.10722859', '1.76965608', '.09903796', '.38411865',
                  '.71640914', '700.304874'])))
RIGHT_T = D('.125')
LOW, HIGH = D('2.00001'), D(10000000)
EARLY, SEPARATOR, LATE = D('8.25'), D(30), D(135)
RIGHT_WITNESS = D(161)


def radii(t):
    return tuple(a+t*d for a, d in zip(LEFT, DIRECTION))


def value(v, q):
    w = v-2
    return (constants(v)[0]-(Interval(w)/v).log()/2
            -(v+1)*average([(1+x/w).log() for x in q])/2)


def shape_derivative(lo, hi, q):
    v = Interval(lo, hi)
    w = v-2
    digamma = Interval(constants(hi)[1].lo, constants(lo)[1].hi)
    return (digamma-Interval(1)/w-average([(1+x/w).log() for x in q])
            +(v+1)/w*average([x/(w+x) for x in q]))/2


def path_derivative(v, rs):
    if v is None:
        return -average([r*d for r, d in zip(rs, DIRECTION)])
    return -(v+1)*average([r*d/(v-2+r*r) for r, d in zip(rs, DIRECTION)])


def path_gap(a, b, lo, hi):
    mid = (lo+hi)/2
    q = squared(radii(Interval(mid)))
    va = value(a, q)
    vb = -average(q)/2 if b is None else value(b, q)
    rs = radii(Interval(lo, hi))
    return va-vb+Interval(lo-mid, hi-mid)*(path_derivative(a, rs)-path_derivative(b, rs))


def endpoint_gap(t, winner, lo, hi):
    q = squared(radii(Interval(t)))
    witness = value(winner, q).lo

    def check(a, b):
        derivative = shape_derivative(a, b, q)
        if derivative.lo >= 0:
            return (Interval(witness)-value(b, q)).lo
        if derivative.hi <= 0:
            return (Interval(witness)-value(a, q)).lo
        mid = (a+b)/2
        bound = value(mid, q)+Interval(a-mid, b-mid)*derivative
        return (Interval(witness)-bound).lo

    return cover(lo, hi, check, D('.000001'), depth=34)


def tail_bounds():
    q = squared(radii(Interval(0, RIGHT_T)))
    min_q = min(x.lo for x in q)
    assert min_q > LOW-2
    low = Interval(LOW-2).log()+Interval(3).log()/2-3*Interval(min_q).log()/2
    near = value(EARLY, q)
    assert near.lo > low.hi
    s0_sq = 1-Interval(2)/HIGH
    normal_error = (average([x*x for x in q])*(1+Interval(1)/HIGH)/(
        4*s0_sq*s0_sq*HIGH)-s0_sq.log()/2)
    assert normal_error.hi < D('.014')
    return [str(low.hi), str(near.lo), str(normal_error.hi)]


def main():
    report = {'tails': tail_bounds()}
    for a, b, margin in [(LATE, SEPARATOR, '.000005'), (EARLY, None, '.014')]:
        report[f'path gap {a} vs {b}'] = cover(
            D(0), RIGHT_T, lambda lo, hi: path_gap(a, b, lo, hi).lo, D(margin))
    report['left endpoint excludes right'] = endpoint_gap(D(0), EARLY, SEPARATOR, HIGH)
    report['right endpoint excludes left'] = endpoint_gap(RIGHT_T, RIGHT_WITNESS, LOW, SEPARATOR)
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
