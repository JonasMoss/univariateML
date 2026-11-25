"""Deterministic box counts for the held-moment half-face contact examples."""
import json
from functools import lru_cache

import mpmath as mp

from check import M, geometry, likelihood

PAD = M("1e-65")


class Interval:
    def __init__(self, low, high=None):
        self.low, self.high = M(low), M(low if high is None else high)

    def __add__(self, other):
        return Interval(self.low+other.low, self.high+other.high)

    def __sub__(self, other):
        return Interval(self.low-other.high, self.high-other.low)

    def __mul__(self, other):
        values = [a*b for a in (self.low, self.high) for b in (other.low, other.high)]
        return Interval(min(values), max(values))

    def __truediv__(self, other):
        assert other.low > 0
        return self*Interval(1/other.high, 1/other.low)

    def exp(self):
        return Interval(mp.exp(self.low), mp.exp(self.high))

    def sqrt(self):
        assert self.low > 0
        return Interval(mp.sqrt(self.low), mp.sqrt(self.high))


@lru_cache(None)
def log_gamma(left, right):
    lower, upper = mp.loggamma(left), mp.loggamma(right)
    # Tangents are lower bounds and the secant is an upper bound.
    return Interval(min(lower, lower+mp.digamma(left)*(right-left),
                        upper, upper-mp.digamma(right)*(right-left)),
                    max(lower, upper))


@lru_cache(None)
def shape_bounds(family, lo, hi):
    if family == "student":
        x = Interval(lo/2, hi/2)
        peak = (Interval(mp.log(2/mp.sqrt(mp.pi)))+
                log_gamma(x.low+M(".5"), x.high+M(".5"))-
                log_gamma(x.low, x.high))
        return peak.exp()/Interval(lo-1, hi-1), Interval(1/(hi-2), 1/(lo-2)), peak
    t = Interval(1/hi, 1/lo)
    g = [log_gamma(1+j*t.low, 1+j*t.high) for j in (1, 2, 3)]
    return ((g[1]-g[0]).exp()/Interval(2), (g[2]-g[0]).exp()/Interval(3),
            Interval(0)-g[0])


def upper(family, box, mu, sd, sample, use_contact):
    lo, hi, wl, wh = box
    k, v, peak = shape_bounds(family, lo, hi)
    z = Interval(1-2*wh, 1-2*wl)
    # W² = v(1-z²)+4(v-k²)z². Bound each term separately.
    z2 = z*z
    var = v*(Interval(1)-z2)+Interval(4)*(v-k*k)*z2
    if var.low <= 0:
        return mp.inf
    width = Interval(2*sd)/var.sqrt()
    c = Interval(mu)-k*z*width
    if use_contact:
        # c decreases with q at any z>=0 because k²/v increases.
        # At fixed q, c increases with w. The two corners are exact extrema.
        c = Interval(geometry(family, hi, wl, mu, sd)[0]-PAD,
                     geometry(family, lo, wh, mu, sd)[0]+PAD)
    result = len(sample)*(peak.high-mp.log(width.low))
    for x in sample:
        narrow = x < c.low
        residual = max(c.low-x, x-c.high, M(0))
        if residual == 0:
            continue
        if narrow and wh == 0:
            return -mp.inf
        r = residual/(width.high*(wh if narrow else 1-wl))
        if family == "student":
            result -= (lo+1)/2*mp.log1p(r*r)
        else:
            result -= r**(lo if r >= 1 else hi)
    return result+PAD


def comparison(family, sample, mu, sd, start, witness, use_contact):
    stack = [(start, 0)]
    visited = 0
    minimum = mp.inf
    most_depth = 0
    while stack:
        box, depth = stack.pop()
        visited += 1
        assert visited <= 128*200 and depth <= 60
        most_depth = max(depth, most_depth)
        bound = upper(family, box, mu, sd, sample, use_contact)
        if bound < witness:
            minimum = min(minimum, witness-bound)
            continue
        lo, hi, wl, wh = box
        if (hi-lo)/(start[1]-start[0]) >= (wh-wl)/(start[3]-start[2]):
            q = (lo+hi)/2
            stack.extend([((lo,q,wl,wh), depth+1), ((q,hi,wl,wh), depth+1)])
        else:
            w = (wl+wh)/2
            stack.extend([((lo,hi,wl,w), depth+1), ((lo,hi,w,wh), depth+1)])
    return dict(contact_enclosure=use_contact, boxes=visited, depth=most_depth,
                least_margin=mp.nstr(minimum, 12))


def examples():
    rows = []
    for family, start, sample, mu, sd, witness_at in (
        ("student", (M(3), M("3.0001"), M(0), M(".0000001")), [0,1], 2/mp.pi+M(".0001"), mp.sqrt(1-4/mp.pi**2), (M(3), M(".5"))),
        ("ged", (M(1), M("1.0001"), M(0), M(".0000001")), [0,3], M("1.0001"), M(1), (M(2), M(".5"))),
    ):
        witness = likelihood(family, *witness_at, mu, sd, sample)-PAD
        rows.append(dict(family=family, sample=sample,
                         witness=mp.nstr(witness, 16),
                         comparisons=[comparison(family,sample,mu,sd,start,witness,method)
                                      for method in (False, True)]))
    return rows


if __name__ == "__main__":
    print(json.dumps({"digits": mp.mp.dps, "exclusions": examples()}, indent=2))
