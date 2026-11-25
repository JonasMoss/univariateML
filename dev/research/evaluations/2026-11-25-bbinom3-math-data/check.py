"""Check rational vertices and stress stationary curvature at 80 decimal digits."""
from fractions import Fraction as Q
from itertools import combinations, product
import random

import mpmath as mp


def prefix(m, r, power):
    result = [0]
    for j in range(m):
        result.append(result[-1] + 1/(r+j)**power)
    return result


def solve_vertex(cells, A, B, C):
    i, j, k = cells
    x, y = A[i]-A[j], A[k]-A[j]
    u, v = B[i]-B[j], B[k]-B[j]
    det = x*v-y*u
    left = ((C-A[j])*v-(C-B[j])*y)/det
    right = (x*(C-B[j])-u*(C-A[j]))/det
    return left, 1-left-right, right


def scans(m, a, b, triples):
    A, A2 = [prefix(m, a, p) for p in (1, 2)]
    B, B2 = [prefix(m, b, p)[::-1] for p in (1, 2)]
    C, D = [prefix(m, a+b, p)[-1] for p in (1, 2)]
    for cells in triples:
        weights = solve_vertex(cells, A, B, C)
        if min(weights) < 0:
            continue
        U = sum(w*A2[k] for w, k in zip(weights, cells))
        W = sum(w*B2[k] for w, k in zip(weights, cells))
        yield cells, weights, U*W/(D*(U+W))-1


def exact_checks():
    small = [Q(1, 1000), Q(1, 100), Q(1, 16), Q(1, 10), Q(1, 6), Q(1, 4), Q(3, 10)]
    regions = [0, 0, 0]
    shapes = (Q(1, 10**10), Q(1, 10**6), Q(1, 3), Q(1), Q(10), Q(100))
    for m in (4, 5, 6, 8, 12):
        H = sum(Q(1, j) for j in range(1, m))
        triples = list(combinations(range(m+1), 3))
        pairs = list(product(small, repeat=2))
        for b in shapes:
            pairs.append((b/(m*m*(2*b+1)**3*H), b))
        for p in (Q(1, 2), Q(1, 10), Q(1, 100)):
            t = 100*m*m/p**4
            pairs.append((p*t, (1-p)*t))
        for a, b in pairs:
            t = a+b
            p = min(a,b)/t
            flags = (t <= Q(1, 3), min(a,b)*H <= 2*max(a,b)/(m*m*(t+1)**3),
                     1/t <= p**4/(100*m*m))
            for _, _, gap in scans(m, a, b, triples):
                assert gap > 0
                for j, flag in enumerate(flags):
                    regions[j] += int(flag)
    assert all(regions)
    print("Exact feasible vertices in the three proved regions:", regions, flush=True)


def high_precision_checks():
    mp.mp.dps = 80
    rng = random.Random(275)
    shapes = [mp.mpf(10)**j for j in (-14, -8, -4, -2, -1, 0, 1, 2, 4, 8, 14)]
    best = None
    tested = feasible = 0
    for m in (4, 5, 6, 8, 12, 20, 50, 100, 300, 1000):
        all_triples = list(combinations(range(m+1), 3)) if m <= 12 else None
        pairs = list(product(shapes, repeat=2))
        for _ in range(24):
            pairs.append(tuple(mp.power(10, mp.mpf(rng.randrange(-560, 561))/40)
                               for _ in range(2)))
        for a, b in pairs:
            triples = all_triples
            if triples is None:
                triples = set()
                for _ in range(100):
                    i, j, k = sorted(rng.sample(range(m+1), 3))
                    triples.update([(i, j, k), (0, j, k), (i, j, m), (0, j, m)])
                triples.update([(0,1,2), (m-2,m-1,m), (0,1,m), (0,m-1,m)])
                triples = sorted(triples)
            tested += len(triples)
            for cells, weights, gap in scans(m, a, b, triples):
                # A zero from a rounded feasibility solve needs exact replay.
                if min(weights) <= mp.mpf("1e-55"):
                    continue
                feasible += 1
                if gap <= 0:
                    raise AssertionError((m, cells, a, b, weights, gap))
                if best is None or gap < best[0]:
                    best = (gap, m, cells, a, b)
    assert feasible and best is not None
    print("80-digit triples checked:", tested, "strictly feasible:", feasible, flush=True)
    gap, m, cells, a, b = best
    print("Smallest sampled ratio minus one:", mp.nstr(gap, 18),
          "at", m, cells, mp.nstr(a, 18), mp.nstr(b, 18), flush=True)
    # All grid powers of 10 and the printed extreme coordinates are rational.
    if a in shapes and b in shapes:
        exact_a, exact_b = [Q(10)**j for x in (a,b) for j in
                            (-14,-8,-4,-2,-1,0,1,2,4,8,14) if mp.mpf(10)**j == x]
        (_, weights, exact_gap), = scans(m, exact_a, exact_b, [cells])
        assert min(weights) > 0 and exact_gap > 0
        print("Extreme grid vertex verified with exact rational arithmetic.", flush=True)


if __name__ == "__main__":
    exact_checks()
    high_precision_checks()
