"""Independently replay polynomial, Bernstein and histogram signs exactly."""
from fractions import Fraction as Q
from itertools import combinations, product
from math import comb

from bernstein import bernstein, split, vertex
from check import prefix, scans, solve_vertex


def evaluate(poly, x, y):
    return sum(int(c)*x**i*y**j for (i, j), c in poly.terms())


def basis(d, x):
    return [comb(d, k)*x**k*(1-x)**(d-k) for k in range(d+1)]


def evaluate_grid(grid, x, y):
    bx, by = basis(len(grid)-1, x), basis(len(grid[0])-1, y)
    return sum(grid[i][j]*u*v for i, u in enumerate(bx) for j, v in enumerate(by))


def sign(x):
    return int(x > 0)-int(x < 0)


def polynomial_checks():
    tests = 0
    for m in (4, 5):
        for cells in combinations(range(m+1), 3):
            N, weights = vertex(m, cells)
            grid = bernstein(N)
            low, high = split(grid, 0)
            low_y, high_y = split(grid, 1)
            scale = 2**(len(grid)-1)
            for a, b in product((Q(1, 11), Q(2, 3), Q(1), Q(11)), repeat=2):
                A, A2 = [prefix(m, a, power) for power in (1, 2)]
                B, B2 = [prefix(m, b, power)[::-1] for power in (1, 2)]
                C, D = [prefix(m, a+b, power)[-1] for power in (1, 2)]
                w = solve_vertex(cells, A, B, C)
                U = sum(f*A2[k] for f, k in zip(w, cells))
                W = sum(f*B2[k] for f, k in zip(w, cells))
                p, y = a/(a+b), 1/(1+a+b)
                assert sign(evaluate(N, a, b)) == sign(U*W-D*(U+W))
                assert [sign(evaluate(f, a, b)) for f in weights] == list(map(sign, w))
                grid_value = evaluate_grid(grid, p, y)
                assert sign(grid_value) == sign(evaluate(N, a, b))
                assert scale*grid_value == (evaluate_grid(low, 2*p, y) if p <= Q(1, 2)
                                            else evaluate_grid(high, 2*p-1, y))
                assert scale*grid_value == (evaluate_grid(low_y, p, 2*y) if y <= Q(1, 2)
                                            else evaluate_grid(high_y, p, 2*y-1))
                tests += 1
    print("Exact polynomial and basis cross-checks:", tests, flush=True)


def endpoint_checks():
    tests = 0
    for m in (4, 5, 9, 20, 50):
        for x in range(1, m):
            threshold = 4
            for counts in ((threshold, 1, threshold+7), (threshold+5, 1, threshold)):
                for p, s in product((Q(1,1000), Q(1,3), Q(2,3), Q(999,1000)),
                                    (Q(0), Q(1,9), Q(1), Q(3))):
                    denom = sum(j/(1+j*s) for j in range(1, m))
                    score = sum(
                        f*(sum(j/(p+j*s) for j in range(1, k))
                           +sum(j/(1-p+j*s) for j in range(1, m-k))-denom)
                        for k,f in zip((0,x,m), counts)
                    )
                    assert score > 0
                    tests += 1
    print("Exact endpoint-count score checks:", tests, flush=True)


def large_shape_checks():
    tested = 0
    for m in (4, 5, 8, 12):
        triples = list(combinations(range(m+1), 3))
        minimum = Q(256*m*m)
        for a, b in product((minimum, 2*minimum, 10*minimum, 10**8*minimum),
                            repeat=2):
            for _, _, gap in scans(m, a, b, triples):
                assert gap > 0
                tested += 1
    assert tested > 0
    print("Exact feasible vertices in the large-shape region:", tested, flush=True)


if __name__ == "__main__":
    polynomial_checks()
    endpoint_checks()
    large_shape_checks()
