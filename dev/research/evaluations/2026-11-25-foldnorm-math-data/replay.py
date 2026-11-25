"""Run from the worktree with /tmp/umvl-cpu/run python3 <this file>."""
import json
import math
from fractions import Fraction
from pathlib import Path

import mpmath as mp


class Profile:
    def __init__(self, blocks):
        self.blocks = blocks
        self.n = sum(count for _, count in blocks)
        self.m2 = self.mean(lambda x: x**2)
        self.scale = mp.sqrt(self.m2)
        self.z = [(x / self.scale, count) for x, count in blocks]
        self.m = self.average(lambda x: x)
        self.m4 = self.average(lambda x: x**4)
        self.m6 = self.average(lambda x: x**6)
        self.right = self.m / (1 - self.m**2)

    def mean(self, fn):
        return mp.fsum(count * fn(x) for x, count in self.blocks) / self.n

    def average(self, fn):
        return mp.fsum(count * fn(x) for x, count in self.z) / self.n

    def variance(self, a):
        return 2 / (1 + mp.sqrt(1 + 4 * a**2))

    def location(self, a):
        return a * self.variance(a)

    def location_prime(self, a):
        return self.variance(a) / mp.sqrt(1 + 4 * a**2)

    def slope_component(self, a):
        return self.average(lambda x: x * mp.tanh(a * x))

    def curvature_component(self, a):
        return self.average(lambda x: (x / mp.cosh(a * x))**2)

    def score(self, a):
        return self.slope_component(a) - self.location(a)

    def delta(self, a):
        v = self.variance(a)
        return self.average(lambda x: mp.log(mp.cosh(a * x))) - a * self.location(a) - mp.log(v) / 2

    def moment_upper(self, b):
        return 3 * (1 - b)**2 + 6 * self.m6 * b / (5 * self.m4)

    def roots(self):
        # Strict analytic score signs through left, and from right onward.
        left = mp.mpf('0.01')
        assert left < self.right
        b = self.location(left)**2
        if self.m4 < 3:
            assert 3 * (1 - b)**2 - self.m4 > mp.mpf('1e-30')
            initial = 1
        else:
            assert self.m4 >= max(mp.mpf(3), self.moment_upper(b))
            initial = -1
        assert mp.sign(self.score(left)) == initial
        assert self.score(self.right) < 0
        pending = [(left, self.right)]
        brackets = []
        cells = 0
        guard = mp.mpf('1e-30')
        while pending:
            cells += 1
            assert cells <= 10000
            lo, hi = pending.pop()
            sl, sh = self.slope_component(lo), self.slope_component(hi)
            ml, mh = self.location(lo), self.location(hi)
            if sl - mh > guard or sh - ml < -guard:
                continue
            cl, ch = self.curvature_component(lo), self.curvature_component(hi)
            if cl - self.location_prime(hi) < -guard or ch - self.location_prime(lo) > guard:
                dl, dh = sl - ml, sh - mh
                assert min(abs(dl), abs(dh)) > guard
                if dl * dh < 0:
                    brackets.append((lo, hi, int(mp.sign(dh))))
                continue
            mid = (lo + hi) / 2
            pending.extend([(mid, hi), (lo, mid)])
        roots = []
        for lo, hi, direction in sorted(brackets):
            root = mp.findroot(self.score, (lo, hi))
            assert lo < root < hi and abs(self.score(root)) < mp.mpf('1e-65')
            roots.append((root, direction))
        return roots, cells


def report(name, profile):
    roots, cells = profile.roots()
    print(name, 'n', profile.n, 'cells', cells)
    print('normalized m4, m6, squared mean:',
          *map(fmt, (profile.m4, profile.m6, profile.m**2)))
    print('moment bound at squared mean:',
          fmt(profile.moment_upper(profile.m**2)))
    print('ordinary normal witness margin:', fmt(-mp.log(4 * (1 - profile.m**2)) / 2))
    for root, direction in roots:
        print('a, derivative crossing, delta, mu, sigma:',
              fmt(root / profile.scale), direction, fmt(profile.delta(root)),
              fmt(profile.scale * profile.location(root)),
              fmt(profile.scale * mp.sqrt(profile.variance(root))))
    return roots


def fmt(x):
    return mp.nstr(x, 26)


def two_level(c):
    return Profile([(mp.mpf(1), 99), (c, 1)])


def exact_score_sign(sample, a):
    lower = upper = Fraction(0)
    for x in sample:
        y = 2 * a * x
        term = total = Fraction(1)
        for k in range(1, 41):
            term *= y / k
            total += term
        ratio = y / 42
        assert 0 <= ratio < 1
        ceiling = total + term * y / 41 / (1 - ratio)
        lower += x * (total - 1) / (total + 1)
        upper += x * (ceiling - 1) / (ceiling + 1)
    m2 = sum(x*x for x in sample) / len(sample)

    def polynomial(score_component):
        s = score_component / len(sample)
        return a * (s*s - m2) + s

    if polynomial(lower) > 0:
        return 1
    assert polynomial(upper) < 0
    return -1


def main():
    mp.mp.dps = 85
    golden_path = Path('univariateML-cpp/include/cppunivariate/distributions/foldnorm/golden.json')
    golden = json.loads(golden_path.read_text())
    profile = Profile([(mp.mpf(x), 1) for x in golden['data']])
    roots = report('golden (binary64 observations)', profile)
    assert [direction for _, direction in roots] == [-1]
    a = roots[0][0]
    mu = profile.scale * profile.location(a)
    sigma = profile.scale * mp.sqrt(profile.variance(a))
    half_loglik = profile.n * (mp.log(2 / (mp.pi * profile.m2)) - 1) / 2
    loglik = half_loglik + profile.n * profile.delta(a)
    print('golden logLik:', fmt(loglik))
    for value, reference in [(mu, golden['estimates']['mu']), (sigma, golden['estimates']['sigma']),
                             (loglik, golden['logLik'])]:
        assert abs(value - reference) / abs(value) < mp.mpf('1e-10')
    sample = '''.6986839225419381 .2562548690820494 .3007670968247965
        .1186039408776371 .19075617333842024 .2781001825837044
        .4979965214041169 .2063506498314821 .31027919417683314
        .30928300405819176 .2729747407467472 .2835705294774539
        1 .3820824073712553'''
    roots = report('prior two-maxima sample (exact decimal observations)',
                   Profile([(mp.mpf(x), 1) for x in sample.split()]))
    assert [direction for _, direction in roots] == [-1, 1, -1]
    sign_points = [Fraction(1), Fraction(3, 2), Fraction(5, 2), Fraction(4)]
    signs = [exact_score_sign(list(map(Fraction, sample.split())), a) for a in sign_points]
    assert signs == [1, -1, 1, -1]
    print('exact rational score signs at 1, 3/2, 5/2, 4:', signs)
    equality = Profile([(mp.mpf(0), 2), (mp.mpf(1), 1)])
    assert abs(equality.m4 - 3) < mp.mpf('1e-80')
    assert equality.moment_upper(equality.m**2) < equality.m4
    print('quartic equality: {0, 0, 1}, endpoint bound:',
          fmt(equality.moment_upper(equality.m**2)))
    positive = Profile([(mp.mpf(1) / 64, 9), (mp.mpf(1), 1)])
    assert positive.m4 > max(3, positive.moment_upper(positive.m**2))
    print('positive observations: nine 1/64, one 1, m4 and endpoint bound:',
          fmt(positive.m4), fmt(positive.moment_upper(positive.m**2)))

    def equations(physical_a, c):
        p = two_level(c)
        a = physical_a * p.scale
        return p.score(a), p.delta(a)

    a_star, c_star = mp.findroot(equations, (mp.mpf('2.61'), mp.mpf('7.20')))
    assert max(map(abs, equations(a_star, c_star))) < mp.mpf('1e-70')
    print('tie candidate a, c, epsilon:', fmt(a_star), mp.nstr(c_star, 74), mp.nstr(1 / c_star, 74))
    nearest = float(c_star)
    below, above = ((nearest, math.nextafter(nearest, math.inf))
                    if mp.mpf(nearest) < c_star else (math.nextafter(nearest, -math.inf), nearest))
    for c, expected in [(below, 1), (above, -1)]:
        p = two_level(mp.mpf(c))
        roots = report('tie neighbor ' + c.hex(), p)
        assert [direction for _, direction in roots] == [1, -1]
        assert mp.sign(p.delta(roots[-1][0])) == expected


if __name__ == '__main__':
    main()
