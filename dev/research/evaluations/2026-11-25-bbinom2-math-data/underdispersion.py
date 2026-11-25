"""Exact checks of the increasing affine direction for nonpositive Delta."""
from fractions import Fraction as Q
from itertools import combinations_with_replacement

from check import delta


if __name__ == "__main__":
    shape_grid = (Q(1,10**6), Q(1,10), Q(1), Q(10), Q(10**6))
    tables = comparisons = equal = 0
    for m in range(2,7):
        for n in range(1,7):
            for sample in combinations_with_replacement(range(m+1),n):
                if not any(0 < x < m for x in sample):
                    continue
                counts = tuple(sample.count(x) for x in range(m+1))
                excess = delta(counts)
                if excess > 0:
                    continue
                tables += 1
                equal += excess == 0
                u = Q(sum(sample), m*n)
                for a in shape_grid:
                    A = sum(sum(1/(a+j) for j in range(x)) for x in sample)
                    for b in shape_grid:
                        B = sum(sum(1/(b+j) for j in range(m-x)) for x in sample)
                        C = n*sum(1/(a+b+j) for j in range(m))
                        assert u*A + (1-u)*B > C
                        comparisons += 1
    print(tables, "nonpositive-Delta tables including", equal, "at equality;",
          comparisons, "exact comparisons", flush=True)
