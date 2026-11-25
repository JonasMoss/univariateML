"""Exact geometric decisions and high-precision discrete Weibull replays."""

from collections import Counter
from fractions import Fraction
from math import nextafter

from mpmath import mp


def geometric_sign(histogram, q):
    numerator, denominator = q.numerator, q.denominator
    product = Fraction(1)
    for k, count in histogram.items():
        if k == 0:
            continue
        product *= Fraction(k + 1) ** (numerator * count * (k + 1))
        product /= Fraction(k) ** (denominator * count * k)
    return (product > 1) - (product < 1)


def scores(histogram, t, beta):
    alpha_score = beta_score = mp.mpf(0)
    for k, count in histogram.items():
        if k == 0:
            alpha_score += count * t / mp.expm1(t)
            continue
        log_k = mp.log(k)
        log_gap = mp.log1p(mp.mpf(1) / k)
        lower = t * mp.exp(beta * log_k)
        width = lower * mp.expm1(beta * log_gap)
        ratio = width / mp.expm1(width)
        alpha_score += count * (ratio - lower)
        beta_score += count * (
            (ratio - lower) * log_k
            + ratio * log_gap / (-mp.expm1(-beta * log_gap))
        )
    return alpha_score, beta_score


def bisect_decreasing(score, low, high):
    assert score(low) > 0 > score(high)
    for _ in range(105):
        mid = (low + high) / 2
        value = score(mid)
        if value == 0:
            return mid
        if value > 0:
            low = mid
        else:
            high = mid
    return (low + high) / 2


def joint_fit(histogram, beta_low, beta_high):
    center = mp.log(max(histogram) + mp.mpf("0.5"))

    def inner(beta):
        def score(centered_alpha):
            t = mp.exp(centered_alpha - beta * center)
            return scores(histogram, t, beta)[0]

        alpha = bisect_decreasing(score, mp.mpf(-16), mp.mpf(16))
        return mp.exp(alpha - beta * center)

    def profile_score(beta):
        return scores(histogram, inner(beta), beta)[1]

    beta = bisect_decreasing(profile_score, mp.mpf(beta_low), mp.mpf(beta_high))
    t = inner(beta)
    assert max(map(abs, scores(histogram, t, beta))) < mp.mpf("1e-24")
    return t, beta


def log_likelihood(histogram, t, beta):
    value = mp.mpf(0)
    for k, count in histogram.items():
        lower = t * mp.power(k, beta)
        width = t * mp.power(k + 1, beta) - lower
        value += count * (-lower + mp.log(-mp.expm1(-width)))
    return value


def check_geometric():
    for zeros, expected in [(51, 1), (52, 0), (53, -1)]:
        histogram = {0: zeros, 1: 12, 2: 4, 3: 1}
        sample_sum = sum(k * count for k, count in histogram.items())
        q = Fraction(sample_sum, sample_sum + sum(histogram.values()))
        assert geometric_sign(histogram, q) == expected
        _, beta = joint_fit(histogram, mp.mpf("0.8"), mp.mpf("1.2"))
        if expected == 0:
            assert abs(beta - 1) < mp.mpf("1e-28")
        else:
            assert (beta - 1) * expected > 0
        print("geometric", zeros, q, expected, mp.nstr(beta, 24))

    # The exact product exponents for binary64 neighbors are too large to
    # materialize. At q=1/4 the product equality and the positive slope
    # of its log establish the two neighbor signs.
    held_histogram = {1: 12, 2: 4, 3: 1}
    assert geometric_sign(held_histogram, Fraction(1, 4)) == 0
    for q, expected in [(nextafter(0.25, 0), -1), (0.25, 0), (nextafter(0.25, 1), 1)]:
        t = -mp.log(mp.mpf(q))
        g = scores(held_histogram, t, mp.mpf(1))[1]
        if expected == 0:
            assert abs(g) < mp.mpf("1e-70")
        else:
            assert g * expected > 0
        print("held geometric", q.hex(), expected, mp.nstr(g, 24))


def check_faces():
    q = mp.mpf(1) / 3
    saturated = 2 * mp.log(1 - q) + mp.log(q)
    for k, shapes in [(0, (2, 4, 6)), (2, (2, 8, 32, 128))]:
        for beta in shapes:
            t = -mp.log(q) / mp.power(k + 1, beta)
            gap = saturated - log_likelihood({k: 2, k + 1: 1}, t, beta)
            assert gap > 0
            if beta == 128:
                assert gap < mp.mpf("1e-20")
    for beta in (4, 16, 64):
        t = mp.power(mp.mpf("2.5"), -beta)
        point = log_likelihood({2: 3}, t, beta)
        assert point < 0
        if beta == 64:
            assert point > mp.mpf("-1e-5")
    t = -mp.log(q)
    assert log_likelihood({0: 3}, t, 1) == log_likelihood({0: 3}, t, 128)
    # At q=1/3 the held Bernoulli weights differ from the empirical weights.
    held_limit = mp.log(1 - q) + 2 * mp.log(q)
    assert held_limit > log_likelihood({0: 1, 1: 2}, t, 4)
    assert held_limit < mp.log(mp.mpf(1) / 3) + 2 * mp.log(mp.mpf(2) / 3)
    print("face paths and held flat likelihood pass")


def check_rounding():
    for k in (5, 6, 7, 8, 9, 10, 12, 15):
        histogram = Counter((k, k + 2))
        t, beta = joint_fit(histogram, 1, 40)
        q = mp.exp(-t)
        rounded_q = float(q)
        if rounded_q == 1:
            print("rounded endpoint", k, mp.nstr(t, 18), mp.nstr(beta, 18))
            continue
        rounded_t = -mp.log(mp.mpf(rounded_q))
        rounded_beta = bisect_decreasing(
            lambda b: scores(histogram, rounded_t, b)[1], mp.mpf(1), mp.mpf(40)
        )
        error = abs(rounded_beta / beta - 1)
        print("rounding", k, mp.nstr(t, 18), rounded_q.hex(), mp.nstr(beta, 18),
              mp.nstr(rounded_beta, 18), mp.nstr(error, 9))
        if k == 8:
            assert t < mp.mpf("1e-6")
            assert error > mp.mpf("1e-10")


if __name__ == "__main__":
    mp.dps = 90
    check_geometric()
    check_faces()
    check_rounding()
