"""Search for both-free exact ties over all sample lengths at small sizes."""
from fractions import Fraction
from itertools import product
from math import gcd, prod

from sympy import factorint
from sympy.solvers.simplex import InfeasibleLPError, linprog


def valuation(value, prime):
    exponent = 0
    while value % prime == 0:
        value //= prime
        exponent += 1
    return exponent


def possible_next_failures(size, factors):
    breaks = {Fraction(1), Fraction(size + 1)}
    for exponent in factors.values():
        breaks.update(
            Fraction(size * exponent, k) for k in range(1, size * exponent + 1)
            if size * exponent < k * (size + 1)
        )
    edges = sorted(breaks)
    probes = edges[1:-1] + [(a + b) / 2 for a, b in zip(edges, edges[1:])]
    return {
        prod(q ** (-(-(size * e * r.denominator) // r.numerator))
             for q, e in factors.items()): r
        for r in probes
    }


def possible_means(size):
    factors = factorint(size)
    next_factors = factorint(size + 1)
    denominators = []
    for q, e in factors.items():
        max_count_valuation = 0
        power = q
        while power <= size + 1:
            max_count_valuation += 1
            power *= q
        denominators.append(
            [1] + [q ** j for j in range(max(1, size * e - max_count_valuation), size * e + 1)]
        )
    for v in possible_next_failures(size, next_factors):
        for parts in product(*denominators):
            b = prod(parts)
            if not b < v < b * (size + 1):
                continue
            u = v - b
            if gcd(v, b) != 1:
                continue
            if any(valuation(v, q) != -(-(size * e * b) // v)
                   for q, e in next_factors.items()):
                continue
            remaining = u
            for q in factors:
                remaining //= q ** valuation(u, q)
            if remaining == 1:
                yield u, v, b


def witness(size, u, v, b):
    r = Fraction(v, b)
    moments = [Fraction(1), r]
    matrix = [[1] * (size + 1), list(range(1, size + 2))]
    for q in range(2, size + 2):
        if factorint(q) != {q: 1}:
            continue
        column = [valuation(k, q) for k in range(1, size + 2)]
        target = (
            size * (valuation(size, q) - valuation(size + 1, q))
            + r * valuation(v, q) - (r - 1) * valuation(u, q) - valuation(b, q)
        )
        if not 0 <= target <= max(column):
            return None
        matrix.append(column)
        moments.append(target)
    try:
        _, weights = linprog(
            [0] * (size + 1), [[0] * (size + 1)], [0], matrix, moments
        )
    except InfeasibleLPError:
        return None
    assert all(weight >= 0 for weight in weights)
    for row, target in zip(matrix, moments):
        assert sum(c * Fraction(w) for c, w in zip(row, weights)) == target
    return weights


def main():
    checked = 0
    ties = []
    for size in range(1, 101):
        for u, v, b in possible_means(size):
            checked += 1
            weights = witness(size, u, v, b)
            if weights is not None:
                ties.append((size, u, v, b, weights))
    print(f"Sizes 1 through 100: {checked} candidate means after integer filters.")
    print(f"Exact tie witnesses found: {ties}")


if __name__ == "__main__":
    main()
