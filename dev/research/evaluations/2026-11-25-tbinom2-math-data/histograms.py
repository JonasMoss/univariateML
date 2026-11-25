"""Exhaustive small histograms with exact brackets and interval log scores."""
from functools import lru_cache
from itertools import combinations
from math import comb

import mpmath as mp

mp.iv.dps = 55


@lru_cache(None)
def profile(lower, size, mean_numerator, mean_denominator):
    weights = [comb(size, k) for k in range(lower, size + 1)]
    coefficients = [
        (k * mean_denominator - mean_numerator) * weight
        for k, weight in enumerate(weights, lower)
    ]

    def sign(a, b):
        value = 0
        power = 1
        for coefficient in reversed(coefficients):
            value = a * value + power * coefficient
            power *= b
        return value

    right = 1
    while sign(right, 1) < 0:
        right *= 2
    left, denominator = 0, 1
    for _ in range(145):
        middle = left + right
        left *= 2
        right *= 2
        denominator *= 2
        if sign(middle, denominator) < 0:
            left = middle
        else:
            right = middle
    odds = mp.iv.mpf([left, right]) / denominator
    partition = mp.iv.mpf(0)
    for weight in reversed(weights):
        partition = odds * partition + weight
    intercept = (mp.iv.mpf(mean_numerator - lower * mean_denominator)
                 / mean_denominator * mp.iv.log(odds) - mp.iv.log(partition))
    return [mp.iv.log(weight) + intercept for weight in weights]


def histograms(width, n):
    for cuts in combinations(range(n + width - 1), width - 1):
        yield tuple(b - a - 1 for a, b in zip((-1, *cuts), (*cuts, n + width - 1)))


def main():
    from fractions import Fraction

    tested = unresolved = reversals = 0
    for lower in range(1, 7):
        for maximum in range(lower + 1, lower + 6):
            width = maximum - lower + 1
            for n in range(2, 9):
                for counts in histograms(width, n):
                    if counts[-1] == 0 or counts[-1] == n:
                        continue
                    mean = Fraction(sum(k * h for k, h in enumerate(counts, lower)), n)
                    previous = None
                    declined = False
                    for size in range(maximum, maximum + 13):
                        table = profile(lower, size, mean.numerator, mean.denominator)
                        value = sum(h * a for h, a in zip(counts, table) if h)
                        if previous is not None:
                            step = value - previous
                            if step.b < 0:
                                declined = True
                            elif step.a > 0:
                                if declined:
                                    print("reversal", lower, counts, size - 1, size, step)
                                    reversals += 1
                            else:
                                unresolved += 1
                                print("unresolved adjacent comparison", lower, counts, size)
                        previous = value
                    tested += 1
        profile.cache_clear()
    print(f"{tested} histograms, {12 * tested} steps, "
          f"{reversals} reversals, {unresolved} unresolved signs")


if __name__ == "__main__":
    main()
