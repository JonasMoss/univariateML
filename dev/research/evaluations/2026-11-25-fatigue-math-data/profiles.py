"""Check fatigue profile identities and bracket bounds at 100 decimal digits."""
import itertools

import mpmath as mp
import sympy as sp

mp.mp.dps = 100


def bisect(score, lo, hi):
    assert score(lo) > 0 > score(hi)
    for _ in range(240):
        mid = (lo + hi) / 2
        if score(mid) > 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


class Profile:
    def __init__(self, data, alpha=None):
        self.z = list(map(mp.log, data))
        self.n = len(data)
        self.a = mp.fsum(data) / self.n
        self.h = self.n / mp.fsum(1 / t for t in data)
        self.alpha = alpha
        self.center = mp.log(self.a * self.h) / 2

    def q(self, u):
        return 4 * mp.fsum(mp.sinh((u - z) / 2)**2 for z in self.z) / self.n

    def value(self, u):
        jac = mp.fsum(mp.log(mp.cosh((u - z) / 2)) for z in self.z)
        q = self.q(u)
        if self.alpha is None:
            return jac - self.n * mp.log(q) / 2
        return jac - self.n * q / (2*self.alpha**2)

    def score(self, u):
        jac = mp.fsum(mp.tanh((u - z) / 2) for z in self.z) / 2
        qp = 2 * mp.fsum(mp.sinh(u - z) for z in self.z) / self.n
        return jac - self.n * qp / (2 * (self.q(u) if self.alpha is None else self.alpha**2))

    def curvature_upper(self, lo, hi):
        distances = [max(lo - z, z - hi, 0) for z in self.z]
        jac = mp.fsum(1 / mp.cosh(d / 2)**2 for d in distances) / 4
        qmin = self.q(min(hi, max(lo, self.center)))
        qmax = max(self.q(lo), self.q(hi))
        if self.alpha is not None:
            return jac - self.n * (qmin + 2) / (2 * self.alpha**2)
        dmin = 4 * self.q(mp.log(self.h)) - 2 * qmax
        denom = qmax if dmin >= 0 else qmin
        return jac - self.n * dmin / (2 * denom**2)

    def check_bound(self, lo, hi, root):
        assert self.score(lo) > 0 > self.score(hi)
        center = (lo + hi) / 2
        radius = (hi - lo) / 2
        upper_curvature = self.curvature_upper(lo, hi)
        height = self.value(center)
        slope = abs(self.score(center))
        radius_of_max = min(radius, -slope / upper_curvature) if upper_curvature < 0 else radius
        upper = height + slope * radius_of_max + upper_curvature * radius_of_max**2 / 2
        value_slack = mp.mpf("1e-65")
        root_value = self.value(root)
        assert height - value_slack <= root_value <= upper + value_slack, (
            height, root_value, upper, lo, hi
        )
        for j in range(17):
            u = lo + (hi - lo) * j / 16
            actual = mp.diff(self.value, u, 2)
            slack = mp.mpf("1e-65") * max(1, abs(upper_curvature))
            assert actual <= upper_curvature + slack, (actual, upper_curvature, lo, hi, u)
            assert self.value(u) <= upper + value_slack
        return upper - height


def free_shape():
    scales = [mp.mpf(10)**p for p in (-12, 0, 12)]
    spreads = list(map(mp.mpf, ("1e-24", "1e-12", ".01", "1", "1e6", "1e24")))
    weights = (1, 3)
    for scale, spread, left_count, right_count in itertools.product(scales, spreads, weights, weights):
        data = [scale] * left_count + [scale * (1 + spread)] * right_count
        profile = Profile(data)
        lo, hi = mp.log(profile.h), mp.log(profile.a)
        root = bisect(profile.score, lo, hi)
        assert 0 < mp.exp(root) - profile.h < (profile.a - profile.h) / 2
        for shrink in (1, 16, 1024):
            profile.check_bound(root - (root - lo) / shrink, root + (hi - root) / shrink, root)
    print("72 free-shape cases, three brackets each, 17 points per bracket")
    profile = Profile(list(map(mp.mpf, (1, 2, 9))))
    lo, hi = mp.log(profile.h / 100), mp.log(100 * profile.a)
    profile.check_bound(lo, hi, bisect(profile.score, lo, hi))
    print("Broad free-shape bracket exercising the negative determinant bound")


def held_shape():
    for displacement in ("0", "1e-16", ".1"):
        data = [mp.mpf(1), 1 + mp.mpf(displacement)]
        profile = Profile(data, mp.mpf(3))
        brackets = [(-4, -1), (1, 4)]
        roots = [bisect(profile.score, mp.mpf(lo), mp.mpf(hi)) for lo, hi in brackets]
        gap = abs(profile.value(roots[0]) - profile.value(roots[1]))
        print("Held alpha=3, displacement =", displacement, "absolute root gap =", mp.nstr(gap, 12))
        for radius in map(mp.mpf, (".1", "1e-5", "1e-10")):
            for root in roots:
                profile.check_bound(root - radius, root + radius, root)
        assert gap < mp.mpf("1e-90")  # Any two-point sample is reciprocal about its geometric mean.
        profile.check_bound(mp.mpf(-4), mp.mpf(4), roots[1])

    data = [mp.mpf(1), mp.mpf(1), 1 + mp.mpf("1e-12")]
    profile = Profile(data, mp.mpf(3))
    roots = [bisect(profile.score, mp.mpf(lo), mp.mpf(hi)) for lo, hi in brackets]
    width = mp.mpf("1e-23")
    lower = [profile.value(r - width/3) for r in roots]
    corrections = [profile.check_bound(r - width, r + width/3, r) for r in roots]
    best = lower.index(max(lower))
    other = 1 - best
    assert lower[best] > lower[other] + corrections[other]
    print("Held alpha=3, sample (1,1,1+10^-12), root gap =",
          mp.nstr(abs(profile.value(roots[0]) - profile.value(roots[1])), 12))
    print("Taylor corrections =", *(mp.nstr(c, 8) for c in corrections))


def exact_held_cases():
    b = sp.Symbol("b")

    def numerator(data, alpha_squared):
        a = sum(data) / len(data)
        inv_h = sum(1 / t for t in data) / len(data)
        s = a - inv_h*b*b + alpha_squared*b * (
            2*b*sum(1 / (t + b) for t in data) / len(data) - 1
        )
        return sp.Poly(sp.cancel(s).as_numer_denom()[0], b), a / inv_h

    pair = tuple(map(sp.Rational, ("1/2", "2")))
    critical, _ = numerator(pair, sp.Rational(45, 8))
    assert sp.rem(critical, (b - 1)**3, b).is_zero
    assert not sp.rem(critical, (b - 1)**4, b).is_zero
    asymmetric, ah = numerator((sp.Integer(1), sp.Integer(1), 1 + sp.Rational(1, 10**12)), 9)
    assert asymmetric.eval(0) > 0 > asymmetric.eval(sp.Rational(1, 2))
    assert asymmetric.eval(2) > 0 and asymmetric.LC() < 0
    mirror = sp.Poly(sum(c * ah**k * b**(asymmetric.degree() - k)
                         for (k,), c in asymmetric.terms()), b)
    assert sp.gcd(asymmetric, mirror).degree() == 0
    print("Exact held-shape cases: triple root for (1/2,2), two unequal peaks for (1,1,1+10^-12)")


if __name__ == "__main__":
    exact_held_cases()
    free_shape()
    held_shape()
