"""Numerically replay the exact-arithmetic profile box inequalities."""
import argparse
from collections import Counter
from functools import lru_cache
import mpmath as mp
from zero_curvature import cutoff, criterion


def check(counts, raw_score):
    total = sum(counts.values())
    sample = [(k, mp.mpf(n) / total) for k, n in sorted(counts.items())]
    empirical_mean = sum(k*w for k, w in sample)
    zero_fraction = sample[0][1] if sample[0][0] == 0 else 0

    def log_mass(a, r, k):
        return mp.log(mp.gammainc(a, r*k, r*(k + 1)))

    @lru_cache(maxsize=None)
    def score_moment(a, r):
        value = mp.mpf(0)
        for k, w in sample:
            if k or raw_score:
                value += w*(mp.diff(lambda s: log_mass(s, r, k), a) - mp.log(r))
            else:
                # The scaled zero-cell integral a*J_0 has an atom at
                # t=0 plus a continuous exponential piece with shape a+1.
                continuous = mp.exp(log_mass(a + 1, r, 0) - a*mp.log(r))
                fraction = continuous/(mp.exp(-r) + continuous)
                value += w*fraction*(mp.diff(lambda s: log_mass(s, r, 0), a + 1)
                                     - mp.log(r))
        return value

    def mean_y(a, r):
        return sum(w*mp.exp(log_mass(a + 1, r, k) - log_mass(a, r, k))/r
                   for k, w in sample)

    def scaled_log_kernel_mass(a, r, k):
        return log_mass(a, r, k) - a*mp.log(r*(k + 1)) + r*k

    @lru_cache(maxsize=None)
    def rate_bracket(a):
        lo, hi = a/(empirical_mean + 1), a/empirical_mean
        for _ in range(32):
            mid = (lo + hi)/2
            if a/mid > mean_y(a, mid):
                lo = mid
            else:
                hi = mid
        return lo, hi

    def score_box(a, b, r, s):
        if raw_score:
            return (mp.log(r) - mp.digamma(b) + score_moment(a, s),
                    mp.log(s) - mp.digamma(a) + score_moment(b, r))
        q = 1 - zero_fraction
        return (mp.log(r) - mp.digamma(b + 1) + q/b + score_moment(a, s),
                mp.log(s) - mp.digamma(a + 1) + q/a + score_moment(b, r))

    def covariance_box(a, b, r, s):
        tt = ty = yy = mp.mpf(0)
        for k, w in sample:
            def ratio(shape):
                return mp.exp(log_mass(shape + 1, r, k) - log_mass(shape, r, k))/r

            vt = mp.diff(lambda shape: log_mass(shape, r, k), a, 2)
            ct = mp.diff(ratio, a)
            vy = mp.exp(log_mass(a + 2, r, k) - log_mass(a, r, k))/r**2 - ratio(a)**2
            rho = mp.exp(scaled_log_kernel_mass(a, r, k)
                         - scaled_log_kernel_mass(b, s, k))
            tt += w*rho*vt
            ty += w*rho*ct
            yy += w*rho*vy
        diagonal_t = mp.polygamma(1, b) - tt
        diagonal_y = a/s**2 - yy
        cross_max = max(abs(1/r - ty), abs(1/s - ty))
        return diagonal_t, diagonal_y, cross_max

    a, b = 1/mp.e, cutoff(sample)
    while (a*mp.log(a) + 1 - zero_fraction*mp.exp(a/empirical_mean)
           - a*(mp.log(empirical_mean + 1) + max(0, mp.digamma(1+a))) <= 0):
        a /= 2
    assert a < b
    pending = [(a, b, 0)]
    stats = Counter()
    curvature_boxes = []
    while pending:
        a, b, depth = pending.pop()
        stats['visited'] += 1
        stats['depth'] = max(stats['depth'], depth)
        assert depth < 40, counts
        r, s = rate_bracket(a)[0], rate_bracket(b)[1]
        lower, upper = score_box(a, b, r, s)
        if lower > 0 or upper < 0:
            stats['score'] += 1
            continue
        tt, yy, ty = covariance_box(a, b, r, s)
        if tt > 0 and yy > 0 and ty**2 < tt*yy:
            stats['curvature'] += 1
            curvature_boxes.append(1 - ty**2/(tt*yy))
            continue
        mid = mp.sqrt(a*b)
        pending.extend([(mid, b, depth + 1), (a, mid, depth + 1)])
    assert stats['score'] > 0 and stats['curvature'] > 0
    print(mp.mp.dps, counts, dict(stats), 'smallest determinant margin',
          mp.nstr(min(curvature_boxes), 10), flush=True)
    return stats


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--raw-score', action='store_true',
                        help='bound each conditional log moment separately')
    args = parser.parse_args()
    for counts in ({0: 1, 2: 1}, {0: 998, 10: 1, 100: 1}):
        with mp.workdps(50):
            assert criterion(counts)[1] > 1
            first = check(counts, args.raw_score)
        with mp.workdps(80):
            assert check(counts, args.raw_score) == first


if __name__ == '__main__':
    main()
