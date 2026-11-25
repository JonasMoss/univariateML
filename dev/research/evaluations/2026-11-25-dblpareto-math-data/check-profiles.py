"""Check exact log-lattice fixtures and replay held-rate and face crossings."""
from fractions import Fraction
from itertools import combinations_with_replacement
from math import hypot, inf, log, log1p, nextafter, sqrt, ulp

import mpmath as mp
import sympy as sp


def distances(levels, counts, k):
    n = sum(counts)
    return tuple(
        Fraction(sum(w * max(0, sign * (k - z)) for z, w in zip(levels, counts)), n)
        for sign in (1, -1)
    )


def sign(value):
    return (value > 0) - (value < 0)


def compare(first, second):
    l, r = first
    v, w = second
    a, b, d = l + r - v - w, l * r, v * w
    if a < 0:
        return -compare(second, first)
    if a == 0:
        return sign(b - d)
    if b >= d:
        return 1
    f = 4 * d - a * a - 4 * b
    return 1 if f < 0 else sign(16 * a * a * b - f * f)


def exact_winners(levels, counts):
    values = [distances(levels, counts, k) for k in levels]
    best = values[0]
    for value in values[1:]:
        if compare(value, best) < 0:
            best = value
    winners = [k for k, value in zip(levels, values) if compare(value, best) == 0]
    radicals = [l + r + 2 * sp.sqrt(l * r) for l, r in values]
    return winners, radicals


def one_held_loss(t, u, h):
    s = h * t
    inverse_q = (s + mp.sqrt(s * (s + 4))) / 2
    return mp.log1p(inverse_q) + inverse_q / (1 + inverse_q) + h * u


def bisect_crossing(gap, low, high):
    assert gap(low) < 0 < gap(high)
    for _ in range(350):
        midpoint = (low + high) / 2
        if gap(midpoint) > 0:
            high = midpoint
        else:
            low = midpoint
    return (low + high) / 2


def neighbors(value):
    rounded = float(value)
    if mp.mpf(rounded) < value:
        return rounded, nextafter(rounded, inf)
    return nextafter(rounded, -inf), rounded


def log_distances(sample, k):
    return tuple(
        mp.fsum(max(0, sign * mp.log(mp.mpf(k) / v)) for v in sample) / len(sample)
        for sign in (1, -1)
    )


def replay(dps):
    with mp.workdps(dps):
        log_two = mp.log(2)

        def profile(k, levels, counts, h):
            u, t = (mp.mpf(v.numerator) / v.denominator * log_two
                    for v in distances(levels, counts, k))
            return one_held_loss(t, u, h)

        levels, counts = [0, 1], [1, 1]

        def face_gap(h):
            return profile(1, levels, counts, h) - profile(0, levels, counts, h)

        crossing = bisect_crossing(face_gap, mp.mpf(1), mp.mpf(10))
        held_neighbors = neighbors(crossing)
        gaps = [face_gap(mp.mpf(h)) for h in held_neighbors]
        assert gaps[0] < 0 < gaps[1]
        for h in held_neighbors:
            distance = log(2) / 2
            inverse = distance / 2 + hypot(distance / 2, sqrt(distance) / sqrt(h))
            finite_loss = log(log1p(inverse * h) + distance / inverse)
            face_loss = log(h) + log(distance)
            margin = 128 * ulp(1.0) * (1 + abs(finite_loss) + abs(face_loss))
            assert abs(finite_loss - face_loss) < margin

        logs, weights = [0, 1, 2], [1, 100, 1]

        def finite_gap(h):
            return profile(1, logs, weights, h) - profile(0, logs, weights, h)

        tie = bisect_crossing(finite_gap, 1 / log_two, mp.mpf(2000))
        margin = profile(2, logs, weights, tie) - profile(0, logs, weights, tie)
        finite_neighbors = neighbors(tie)
        finite_gaps = [finite_gap(mp.mpf(h)) for h in finite_neighbors]
        assert margin > 0 and finite_gaps[0] < 0 < finite_gaps[1]
        print('finite crossing', mp.nstr(tie, 24), 'face loss margin',
              mp.nstr(margin, 18), 'neighbors', [h.hex() for h in finite_neighbors],
              'low-kink minus high-kink loglik', [mp.nstr(g, 18) for g in finite_gaps])

        face_near = []
        for lower in [nextafter(0.5, -inf), 0.5, nextafter(0.5, inf)]:
            sample = [lower, 1, 1, 1, 1, 16]
            values = [(sum(mp.sqrt(v) for v in log_distances(sample, k))) ** 2
                      for k in sorted(set(sample))]
            assert values[2] > max(values[:2])
            face_near.append((lower.hex(), mp.nstr(mp.log(values[0] / values[1]), 15)))
            if lower != 0.5:
                assert (values[0] > values[1]) == (lower < 0.5)

        # A strictly beaten finite knot within the fitter's fixed margin.
        assert 0 < abs(gaps[0]) < mp.mpf('1e-14')
        print('dps', dps, 'held crossing', mp.nstr(crossing, 28),
              'neighbors', [h.hex() for h in held_neighbors],
              'finite-minus-face loglik', [mp.nstr(g, 18) for g in gaps])
        print('all-free neighbors, finite-minus-Pareto loglik', face_near)
        return held_neighbors, finite_neighbors


def main():
    fixtures = [
        ([-1, 0, 4], [1, 4, 1], [-1, 0]),
        ([0, 1, 2], [1, 2, 1], [0, 1, 2]),
        ([-5, -1, 2, 6], [9, 27, 24, 4], [-1, 2]),
        ([-5, -2, -1, 0, 1, 2, 5], [4, 11, 6, 6, 6, 11, 4],
         list(range(-2, 3))),
        ([-20, *range(-6, 7), 20], [50, 97, *([14] * 11), 97, 50],
         list(range(-6, 7))),
        ([0, 1], [1, 1], [0, 1]),
        ([0, 1], [2, 1], [0]),
        ([-4, -2, 1, 4], [1, 1, 2, 1], [-4, 4]),
    ]
    for levels, counts, expected in fixtures:
        winners, scores = exact_winners(levels, counts)
        assert winners == expected, (levels, counts, winners, scores)
        print('exact exponents, counts, squared-height costs, winners:',
              levels, counts, scores, winners)

    # Compare the exact comparator and the three-level predicates against
    # direct radical costs on a small independent grid.
    profiles = three_levels = 0
    with mp.workdps(80):
        for levels in combinations_with_replacement(range(5), 5):
            knots = sorted(set(levels))
            if len(knots) < 2:
                continue
            counts = [levels.count(k) for k in knots]
            winners, _ = exact_winners(knots, counts)
            approx = [sum(mp.sqrt(mp.mpf(v.numerator) / v.denominator)
                          for v in distances(knots, counts, k)) for k in knots]
            numerical = [k for k, value in zip(knots, approx)
                         if abs(value - min(approx)) < mp.mpf('1e-75')]
            assert winners == numerical
            profiles += 1
            if len(knots) == 3:
                a, b = knots[1] - knots[0], knots[2] - knots[1]
                p, r = Fraction(counts[0], 5), Fraction(counts[2], 5)
                finite_wins = (p < Fraction(1, 2) and r < Fraction(1, 2)
                               and 4 * p * r * b <= (1 - 2 * p)**2 * a
                               and 4 * p * r * a <= (1 - 2 * r)**2 * b)
                assert (knots[1] in winners) == finite_wins
                three_levels += 1
    print('lattice grid profile count', profiles, 'including three-level cases', three_levels)
    assert replay(90) == replay(130)


if __name__ == '__main__':
    main()
