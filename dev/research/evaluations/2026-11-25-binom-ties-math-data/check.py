"""Exact enumeration and independent checks for TASK-332."""
import csv
from collections import Counter
from fractions import Fraction
from itertools import combinations_with_replacement
from math import comb, gcd, prod
from pathlib import Path
import struct


def held_ratio(xs, size, p):
    advance = size + 1
    return (1 - p) ** len(xs) * prod(
        Fraction(advance, advance - x) for x in xs
    )


def held_tie(xs, size, p):
    advance = size + 1
    complement = 1 - p
    if advance % complement.denominator:
        return False
    target = advance // complement.denominator * complement.numerator
    remainders = [advance - x for x in xs]
    for _ in xs:
        factor = target
        for j, remainder in enumerate(remainders):
            common = gcd(factor, remainder)
            factor //= common
            remainders[j] //= common
            if factor == 1:
                break
        if factor != 1:
            return False
    return all(remainder == 1 for remainder in remainders)


def dyadic_from_bits(p):
    bits = int.from_bytes(struct.pack(">d", p), "big")
    exponent = (bits >> 52) & 0x7ff
    fraction = bits & ((1 << 52) - 1)
    if exponent:
        return Fraction((1 << 52) | fraction, 1 << (1075 - exponent))
    return Fraction(fraction, 1 << 1074)


def likelihood(xs, size, p):
    successes = sum(xs)
    return prod(comb(size, x) for x in xs) * p ** successes * (
        1 - p
    ) ** (len(xs) * size - successes)


def free_ratio(xs, size):
    n = len(xs)
    total = sum(xs)
    advance = size + 1
    failures = n * size - total
    next_failures = failures + n
    left = (size ** (n * size)) * next_failures ** next_failures
    right = (advance ** (n * size)) * n ** n * (
        failures ** failures
    ) * prod(advance - x for x in xs)
    return Fraction(left, right)


def free_best(xs):
    low = high = max(xs)
    while free_ratio(xs, high) > 1:
        low = high + 1
        high *= 2
    while low < high:
        middle = (low + high) // 2
        if free_ratio(xs, middle) > 1:
            low = middle + 1
        else:
            high = middle
    return low


def held_grid(samples):
    probabilities = [Fraction(1, 2), Fraction(1, 3), Fraction(1, 4), Fraction(2, 3)]
    rows = []
    summary = Counter()
    for xs in samples:
        for p in probabilities:
            low = max(xs)
            ceiling = low / p
            high = max(low, -(-ceiling.numerator // ceiling.denominator))
            values = [likelihood(xs, size, p) for size in range(low, high + 1)]
            best = max(values)
            winners = [size for size, value in enumerate(values, low) if value == best]
            ratios = [held_ratio(xs, size, p) for size in range(low, high + 1)]
            assert all(a > b for a, b in zip(ratios, ratios[1:])) if low else (
                len(set(ratios)) == 1
            )
            assert ratios[-1] < 1
            for size, ratio in enumerate(ratios, low):
                assert held_tie(xs, size, p) == (ratio == 1)
                if size < high:
                    assert ratio == values[size - low + 1] / values[size - low]
            assert len(winners) in (1, 2)
            if len(winners) == 2:
                first, second = winners
                assert second == first + 1 and held_tie(xs, first, p)
                missed = len(set(xs)) > 1
                rows.append((len(xs), " ".join(map(str, xs)), str(p), first, second, missed))
                summary[p, missed] += 1
    return rows, summary


def free_grid(samples):
    counts = Counter()
    ties = []
    largest_size = 0
    for xs in samples:
        n = len(xs)
        total = sum(xs)
        if not total:
            counts["zero"] += 1
            continue
        if n * total + total * total <= n * sum(x * x for x in xs):
            counts["face"] += 1
            continue
        counts["finite"] += 1
        size = free_best(xs)
        largest_size = max(largest_size, size)
        candidate = likelihood(xs, size, Fraction(total, n * size))
        right = likelihood(xs, size + 1, Fraction(total, n * (size + 1)))
        assert candidate >= right and right / candidate == free_ratio(xs, size)
        if size > max(xs):
            left = likelihood(xs, size - 1, Fraction(total, n * (size - 1)))
            assert candidate > left and candidate / left == free_ratio(xs, size - 1)
        if candidate == right:
            ties.append((xs, size))
    return counts, largest_size, ties


def fixtures():
    cases = [
        ((0, 6), 7, 0.5, (7, 8)),
        ((0, 6), 7, float.fromhex("0x1.fffffffffffffp-2"), (8,)),
        ((0, 6), 7, float.fromhex("0x1.0000000000001p-1"), (7,)),
        ((0, 0, 7), 7, 0.5, (7, 8)),
        ((0, 0), 0, 0.5, (0,)),
    ]
    for xs, size, p, winners in cases:
        exact = dyadic_from_bits(p)
        assert exact == Fraction.from_float(p)
        assert held_tie(xs, size, exact) == (len(winners) == 2)
        assert held_ratio(xs, winners[0], exact) <= 1
        if winners[0] > max(xs):
            assert held_ratio(xs, winners[0] - 1, exact) > 1
        if len(winners) == 2:
            assert held_ratio(xs, winners[-1], exact) < 1
    for value in (2.0 ** -1074, 2.0 ** -1022, 1 / 3, 2 / 3, 0.75):
        assert dyadic_from_bits(value) == Fraction.from_float(value)
    xs = (0, 1, 6)
    p = Fraction(1, 3)
    assert held_tie(xs, 8, p) and not held_tie(xs, 8, dyadic_from_bits(float(p)))
    print("Binary64 and boundary fixtures passed.")


def main():
    samples = [
        xs for n in range(1, 5) for xs in combinations_with_replacement(range(11), n)
    ]
    rows, summary = held_grid(samples)
    counts, largest_size, free_ties = free_grid(samples)
    fixtures()
    assert len(samples) == 1364
    path = Path(__file__).with_name("held-ties.csv")
    with path.open("w", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(["n", "sample", "p", "size", "next_size", "constant_branch_misses"])
        writer.writerows(rows)
    print(f"{len(samples)} multisets, {4 * len(samples)} held fits, {len(rows)} ties")
    for p in sorted({p for p, _ in summary}):
        print(f"p={p}: {summary[p, False]} constant, {summary[p, True]} nonconstant")
    print(f"Both free: {dict(counts)}, largest finite size {largest_size}, ties {free_ties}")


if __name__ == "__main__":
    main()
