"""Exact counterexample and numerical slash profiles (workstation, one slot)."""

from collections import Counter
from fractions import Fraction as Q
from math import factorial, prod
from random import Random

import mpmath as mp


def family(e):
    return (-type(e)(1), *([-1/e]*29), *([30/(1+29*e)]*30))


def grouped(ds):
    return Counter(ds)


def mpq(x):
    return mp.mpf(x.numerator)/x.denominator


def lower_a(t):
    # Integrate the odd Taylor lower bound for exp(-tu). For large t use
    # exp(t) >= sum_{j=0}^{40} 20**j/j!.
    if t >= 20:
        return (1 - 1/sum(Q(20)**j/factorial(j) for j in range(41)))/t
    u = min(t, Q(4))
    return sum((-u)**j/factorial(j+1) for j in range(40))*u/t if t else Q(1)


def lower_ratio(ds, m, s):
    lam = 1/(2*s*s)
    factors = [lower_a(lam*m*m)]*len(ds)
    factors += [lam*d*d*lower_a(lam*(d-m)**2) for d in ds]
    assert min(factors) > 0
    return prod(factors)


def exact_checks():
    e = Q(1, 10000)
    source = family(e)
    winner = source*2
    collapse = tuple(sign*abs(d) for d in source for sign in (-1, 1))
    for ds in (winner, collapse):
        assert sum(1/d for d in ds) == 0 and sum(d > 0 for d in ds) == 60
    assert Counter(map(abs, winner)) == Counter(map(abs, collapse))
    b = source[-1]
    assert lower_ratio(source, b/5, 3*b/10) > 2
    assert lower_ratio((Q(-1), *([Q(9)]*9)), Q(4), Q(3)) > Q(1456, 1000)

    # Close strict win on exactly representable binary64 observations.
    p, q = 367487, 100000000
    close = family(Q(p, q))
    integral = [d*p*(q+29*p) for d in close]
    assert all(d.denominator == 1 and Q(float(d)) == d for d in integral)
    assert sum(1/d for d in integral) == 0
    bound = lower_ratio(close, Q("5.6694643"), Q("8.8880781"))
    assert bound > 1 + Q(4, 10**7)
    return winner, collapse


class Profile:
    def __init__(self, ds):
        self.k = len(ds)
        self.ds = [(mpq(Q(d)) if isinstance(d, (int, Q)) else d, count) for d, count in grouped(ds).items()]

    def scores(self, m, s):
        residuals = [(-m, self.k)] + [(d-m, j) for d, j in self.ds]
        location, scale = [], []
        for r, count in residuals:
            t = (r/s)**2/2
            h = t/mp.expm1(t) if t else 1
            location.append(2*count*(1-h)/r if r else 0)
            scale.append(count*(1-2*h))
        return mp.fsum(location), mp.fsum(scale)

    def value(self, m, s):
        def log_a(t):
            return mp.log(-mp.expm1(-t)/t) if t else mp.mpf(0)

        lam = 1/(2*s*s)
        return self.k*log_a(lam*m*m) + mp.fsum(
            count*(mp.log(lam*d*d)+log_a(lam*(d-m)**2))
            for d, count in self.ds
        )

    def scale(self, m):
        distances = [abs(m)] + [abs(d-m) for d, _ in self.ds if d != m]
        lo = mp.log(min(distances)/4)
        hi = mp.log(max(distances)*2)
        assert self.scores(m, mp.exp(lo))[1] > 0 > self.scores(m, mp.exp(hi))[1]
        for _ in range(54):
            mid = (lo+hi)/2
            if self.scores(m, mp.exp(mid))[1] > 0:
                lo = mid
            else:
                hi = mid
        return mp.exp((lo+hi)/2)

    def grid(self, subdivisions=16):
        knots = sorted([mp.mpf(0), *(d for d, _ in self.ds)])
        grid = {a+(b-a)*j/subdivisions
                for a, b in zip(knots, knots[1:])
                for j in range(subdivisions+1)}
        a = min(abs(d) for d, _ in self.ds)
        grid.update(sign*a*mp.power(2, -j) for sign in (-1, 1) for j in range(1, 21))
        rows = []
        for m in sorted(grid-{mp.mpf(0)}):
            s = self.scale(m)
            rows.append((m, s, self.value(m, s), self.scores(m, s)[0]))
        return rows

    def root(self, m, s):
        m, log_s = mp.findroot(lambda x, y: self.scores(x, mp.exp(y)),
                               (m, mp.log(s)), maxsteps=50)
        return m, mp.exp(log_s)


def transition():
    # Locate the high-valued branch by continuation. The grid probes below
    # also scan the negative side and later e, where this branch can disappear.
    m, s = mp.mpf(6), mp.mpf(9)
    last_win = None
    for j in range(101):
        e = mp.mpf(1)/10000+j*mp.mpf(1)/10000
        p = Profile(family(e))
        m, s = p.root(m, s)
        if p.value(m, s) < 0:
            break
        last_win = e
    assert last_win is not None and last_win < e

    def equations(e, m, log_s):
        p = Profile(family(e))
        s = mp.exp(log_s)
        return *p.scores(m, s), p.value(m, s)

    e, m, log_s = mp.findroot(equations, ((last_win+e)/2, m, mp.log(s)))
    s = mp.exp(log_s)
    assert max(map(abs, equations(e, m, log_s))) < mp.mpf("1e-75")
    profile = Profile(family(e))
    hessian = mp.matrix([
        [mp.diff(lambda x: profile.scores(x, s)[i], m),
         mp.diff(lambda z: profile.scores(m, mp.exp(z))[i], log_s)]
        for i in range(2)
    ])
    assert hessian[0, 0] < 0 and mp.det(hessian) > 0
    print("transition e,m,s:", *(mp.nstr(x, 32) for x in (e, m, s)))
    close = Profile(family(Q(367487, 100000000)))
    print("binary64 sample normalized witness log gap:",
          mp.nstr(close.value(mp.mpf("5.6694643"), mp.mpf("8.8880781")), 16))
    for sign in (-1, 1):
        shifted = Profile(family(e+sign*mp.mpf("1e-12")))
        x, y = shifted.root(m, s)
        gap = shifted.value(x, y)
        assert sign*gap < 0
        print("shift", sign, "gap", mp.nstr(gap, 16))
    return e


def probe(samples):
    wins = {}
    for name, ds, expect in samples:
        p = Profile(ds)
        rows = p.grid()
        best = max(row[2] for row in rows)
        for left, right in zip(rows, rows[1:]):
            if left[0]*right[0] < 0 or not left[3] > 0 > right[3]:
                continue
            start = max((left, right), key=lambda row: row[2])
            m, s = p.root(*start[:2])
            assert max(map(abs, p.scores(m, s))) < mp.mpf("1e-35")
            if left[0] <= m <= right[0]:
                best = max(best, p.value(m, s))
        win = best > mp.mpf("1e-25")
        if expect is not None:
            assert win == expect, (name, best)
        wins.setdefault(name, [0, 0])[int(win)] += 1
    return wins


def random_samples():
    rng = Random(326)
    for _ in range(12):
        weights = [Q(rng.randint(1, 30)) for _ in range(rng.randint(2, 5))]
        ds = [1/w for w in weights]
        yield "paired", tuple(ds+[-x for x in ds]), False
        weights = [Q(rng.randint(1, 30)) for _ in range(rng.randint(2, 8))]
        other = [Q(rng.randint(1, 30)) for _ in range(rng.randint(1, 6))]
        ds = tuple([1/w for w in weights] + [-sum(other)/(w*sum(weights)) for w in other])
        yield "mixed", ds, None
        weights = [Q(rng.randint(90, 110), 100) for _ in range(rng.randint(9, 14))]
        ds = (-1, *(sum(weights)/w for w in weights))
        yield "one against a cluster", ds, True


def main():
    winner, collapse = exact_checks()
    print("Exact counterexample and binary64 witness checks passed.")
    mp.mp.dps = 80
    e = transition()
    with mp.workdps(45):
        samples = [
            ("paired original", (-1, 1), False),
            ("strict win original", (-1, *([9]*9)), True),
            ("same magnitudes collapse", collapse, False),
            ("same magnitudes win", winner, True),
            ("equality branch", family(e), False),
        ]
        samples += [("transition family sweep", family(x), mpq(x) < e)
                    for x in [Q(1, 10000), Q(1, 2000), Q(1, 1000),
                              *[Q(j, 1000) for j in range(2, 11)],
                              *[Q(j, 100) for j in range(2, 101, 4)], Q(1)]]
        samples += list(random_samples())
        for _, ds, _ in samples:
            if all(isinstance(d, (int, Q)) for d in ds):
                assert sum(1/Q(d) for d in ds) == 0
        for key, result in probe(samples).items():
            print(key, "no positive gap / positive gap:", *result)


if __name__ == "__main__":
    main()
