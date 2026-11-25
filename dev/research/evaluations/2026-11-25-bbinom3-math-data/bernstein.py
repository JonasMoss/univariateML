"""Exact polynomial exploration of the size-four stationary vertices."""
from itertools import combinations

import sympy as sp

a, b, z = sp.symbols("a b z")
t = a + b


def vertex(m, cells):
    """Return numerator of curvature and numerator of each weight."""
    def prefixes(shape):
        denominator = sp.prod(shape + j for j in range(m))
        terms = [sp.div(denominator, shape + j, shape)[0] for j in range(m)]
        sums = [sum(terms[:k]) for k in range(m + 1)]
        return denominator, sums

    pa, ha = prefixes(a)
    pb, hb = prefixes(b)
    pt, ht = prefixes(z)
    pt, ht = pt.subs(z, t), ht[m].subs(z, t)
    A = [ha[k] for k in cells]
    B = [hb[m-k] for k in cells]
    det = sp.Poly((A[0]-A[1])*(B[2]-B[1]) -
                  (A[2]-A[1])*(B[0]-B[1]), a,b).as_expr()
    c, d = ht*pa-A[1]*pt, ht*pb-B[1]*pt
    weights = [sp.Poly(c*(B[2]-B[1])-d*(A[2]-A[1]),a,b),
               None,
               sp.Poly(d*(A[0]-A[1])-c*(B[0]-B[1]),a,b)]
    weights[1] = sp.Poly(det*pt,a,b)-weights[0]-weights[2]
    A2 = [sp.Poly(Ai*sp.diff(pa,a)-sp.diff(Ai,a)*pa,a,b) for Ai in A]
    B2 = [sp.Poly(Bi*sp.diff(pb,b)-sp.diff(Bi,b)*pb,a,b) for Bi in B]
    D2 = sp.Poly(ht*sp.diff(pt,a)-sp.diff(ht,a)*pt,a,b)
    U = sum(w*h for w,h in zip(weights,A2))
    W = sum(w*h for w,h in zip(weights,B2))
    N = U*W*sp.Poly(pt,a,b) - D2*sp.Poly(det,a,b)*(
        U*sp.Poly(pb**2,a,b)+W*sp.Poly(pa**2,a,b))
    # Strip common factors whose positivity follows from the coefficients.
    content, factors = N.factor_list()
    if content < 0:
        factors.append((sp.Poly(-1,a,b),1))
    undecided = sp.Poly(1,a,b)
    for factor, exponent in factors:
        if min(factor.coeffs()) <= 0 and exponent % 2:
            undecided *= factor
        elif min(factor.coeffs()) <= 0:
            # Keep sign-indefinite squares so strict positivity is checked.
            undecided *= factor**exponent
    return undecided, [w.primitive()[1] for w in weights]


def bernstein(poly):
    """Standard Bernstein coefficients on (p, y), times a common integer."""
    from math import comb, lcm

    d = int(poly.total_degree())
    binomials = [comb(d,k) for k in range(d+1)]
    scale = lcm(*binomials)
    raw = [[0]*(d+1) for _ in range(d+1)]
    for (i,j), coefficient in poly.terms():
        for k in range(d-i-j+1):
            raw[i+k][d-i-j] += int(coefficient)*comb(d-i-j,k)
    return [[v*(scale//binomials[i])*(scale//binomials[j])
             for j,v in enumerate(row)] for i,row in enumerate(raw)]


def split(grid, axis):
    if axis == 0:
        grid = list(zip(*grid))
    first, second = [], []
    for row in grid:
        d = len(row)-1
        left, right = [], []
        for i in range(d+1):
            left.append(row[0] << (d-i))
            right.append(row[-1] << (d-i))
            row = [x+y for x,y in zip(row,row[1:])]
        first.append(left)
        second.append(right[::-1])
    if axis == 0:
        first, second = list(zip(*first)), list(zip(*second))
    return first, second


def positive(grid, outer):
    """Check strict positivity on all faces except global boundary faces."""
    if any(min(row) < 0 for row in grid):
        return False
    if not any(max(row) > 0 for row in grid):
        return False
    for i, j in ((0,0),(0,-1),(-1,0),(-1,-1)):
        if not outer[i == -1] and not outer[2+(j == -1)] and grid[i][j] <= 0:
            return False
    for i in (0,-1):
        if not outer[i == -1] and max(grid[i]) <= 0:
            return False
        if not outer[2+(i == -1)] and max(row[i] for row in grid) <= 0:
            return False
    return True


def cover(polys, depth_limit=24):
    stack = [(tuple(map(bernstein,polys)), 0, (True,)*4, "")]
    tested = [0,0,0]
    first = None
    while stack:
        grids, depth, outer, path = stack.pop()
        if any(positive(grid,outer) for grid in grids[1:]):
            tested[0] += 1
        elif positive(grids[0],outer):
            tested[1] += 1
        elif depth == depth_limit:
            tested[2] += 1
            first = first or path
        else:
            axis = depth % 2
            halves = tuple(zip(*(split(grid,axis) for grid in grids)))
            for side in (0,1):
                flags = list(outer)
                flags[2*axis+1-side] = False
                stack.append((halves[side], depth+1, flags, path+str(side)))
    return tested, first


def check_size(m):
    totals = [0,0,0]
    supports = difficult = 0
    for cells in combinations(range(m+1),3):
        if cells > tuple(m-k for k in cells[::-1]):
            continue
        supports += 1
        N, weights = vertex(m,cells)
        tested, first = cover([N] + [-w for w in weights])
        totals = [x+y for x,y in zip(totals,tested)]
        if first:
            difficult += 1
            print(m, cells, tested, first, flush=True)
    print(m, "supports", supports, "leaves", totals, "unresolved supports", difficult, flush=True)
    return not difficult


if __name__ == "__main__":
    import sys

    sizes = list(map(int,sys.argv[1:])) or [4]
    results = [check_size(m) for m in sizes]
    assert all(results), "some supports were not certified"
