"""Interval replay of the shifted Gompertz rate profile (sgomp/mle.hpp)."""

from functools import lru_cache

from mpmath import iv, mp


class Profile:
    def __init__(self, data):
        unit = max(data)
        self.x = [iv.mpf(v) / iv.mpf(unit) for v in data]
        self.n = len(data)
        self.total = sum(self.x)
        minimum = min(data)
        self.gap = sum((iv.mpf(v) - iv.mpf(minimum)) / unit for v in data)

    def terms(self, rate):
        return [(x, iv.exp(-rate * x), -iv.expm1(-rate * x)) for x in self.x]

    @lru_cache(maxsize=8192)
    def shape(self, rate):
        terms = self.terms(iv.mpf(rate))

        total_decay = sum(v for _, v, _ in terms)
        if total_decay.a <= 0:
            raise ValueError('conditional shape bracket has unresolved positive decay sum')
        normalizer = iv.mpf(total_decay.a)

        def score(scaled_eta):
            eta = iv.mpf(scaled_eta) / normalizer
            return sum(a / (1 + eta * a) - v for _, v, a in terms)

        if score(0).b <= 0:
            return iv.mpf(0)
        # The score at eta = (n+1)/sum(exp(-rate*x)) is strictly negative.
        # Using the lower endpoint of the denominator widens this bracket.
        lo, hi = mp.mpf(0), mp.mpf(self.n + 1)
        for _ in range(100):
            mid = (lo + hi) / 2
            value = score(mid)
            if value.a > 0:
                lo = mid
            elif value.b < 0:
                hi = mid
            else:
                break
            if hi - lo < mp.mpf('1e-23') * max(1, lo):
                break
        return iv.mpf([lo, hi]) / normalizer

    def likelihood(self, rate, eta):
        return (self.n * iv.log(rate) - rate * self.total
                + sum(iv.log1p(eta * a) - eta * v for _, v, a in self.terms(rate)))

    def value(self, rate):
        return self.likelihood(iv.mpf(rate), self.shape(rate))

    def score(self, rate, eta):
        return (self.n / rate - self.total
                + sum(eta * x * v * (1 + 1 / (1 + eta * a))
                      for x, v, a in self.terms(rate)))

    def shape_range(self, lo, hi):
        return iv.mpf([self.shape(lo).a, self.shape(hi).b])

    def derivative(self, lo, hi):
        return self.score(iv.mpf([lo, hi]), self.shape_range(lo, hi))

    def curvature(self, lo, hi):
        rate = iv.mpf([lo, hi])
        eta = self.shape_range(lo, hi)
        terms = self.terms(rate)
        cross = sum(x * v * (1 + 1 / (1 + eta * a)**2) for x, v, a in terms)
        denominator = sum((a / (1 + eta * a))**2 for _, _, a in terms)
        if denominator.a <= 0:
            return None
        return (
            -self.n / rate**2
            - sum(eta * x**2 * v * (1 + (1 + eta) / (1 + eta * a)**2)
                  for x, v, a in terms)
            + cross**2 / denominator
        )

    def root_window(self, lo, hi):
        """Narrow a window already known to contain all maximizers."""
        curvature = self.curvature(lo, hi)
        if curvature is None or curvature.b >= 0:
            return lo, hi
        for _ in range(40):
            mid = (lo+hi)/2
            if not lo < mid < hi or hi-lo <= mid*mp.mpf('1e-15'):
                break
            slope = self.score(iv.mpf(mid), self.shape(mid))
            if slope.a > 0:
                lo = mid
            elif slope.b < 0:
                hi = mid
            else:
                break
        return lo, hi

    def upper(self, lo, hi):
        curvature = self.curvature(lo, hi)
        if curvature is None:
            return None
        mid = (lo + hi) / 2
        radius = max(iv.mpf(mid) - lo, iv.mpf(hi) - mid)
        slope = self.score(iv.mpf(mid), self.shape(mid))
        return (self.value(mid) + abs(slope) * radius
                + max(iv.mpf(0), curvature.b) * radius**2 / 2)
