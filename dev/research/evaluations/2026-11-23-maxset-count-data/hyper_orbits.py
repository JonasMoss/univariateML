"""Exact finite-grid witnesses for the unresolved nonconstant urn comparison."""
from fractions import Fraction
from math import comb, prod


def choose(n, k):
    return comb(n, k) if 0 <= k <= n else 0


def likelihood(N, m, k, sample):
    return prod(Fraction(choose(m, x) * choose(N - m, k - x), comb(N, k))
                for x in sample)


for N in (3, 5):
    sample = (0, 1)
    scores = {(m, N, k): likelihood(N, m, k, sample)
              for m in range(N + 1) for k in range(N + 1)}
    best = max(scores.values())
    winners = sorted(counts for counts, score in scores.items() if score == best)
    assert len({(min(m, k), max(m, k)) for m, _, k in winners}) == 2
    print('N', N, 'sample', sample, 'likelihood', best, 'maximizers', winners)
