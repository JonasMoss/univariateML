"""Structured score scans and 100-digit refinement of the two-peak example."""
from fractions import Fraction
from itertools import product

import mpmath as mp
import numpy as np


def scan():
    gaps = [0.125, 0.5, 1, 2, 3, 5, 8, 12, 20, 40, 64, 96, 128, 192]
    for k in (2, 3):
        weights = {}
        for n in range(k, 13):
            for counts in product(range(1, n), repeat=k-1):
                last = n - sum(counts)
                if last > 0:
                    weights.setdefault(tuple(Fraction(c, n) for c in (*counts, last)), n)
        # Free weights on a denominator-20 simplex, plus near-face weights.
        for n in (20, 100):
            for counts in product(range(1, n), repeat=k-1):
                last = n - sum(counts)
                if last > 0 and (n == 20 or min(*counts, last) == 1):
                    weights.setdefault(tuple(Fraction(c, n) for c in (*counts, last)), n)
        w = np.array(list(weights), dtype=float)
        n = np.array(list(weights.values()))
        found = np.zeros(len(w), dtype=int)
        cases = 0
        smallest = None
        for steps in product(gaps, repeat=k-1):
            xlog = np.cumsum([0, *steps])
            xlog -= xlog[-1]
            # z=log(rate*max(x)); positive tail cutoff is descriptive.
            z = np.arange(-16, -xlog[0]+12.0001, 0.125)
            s = np.exp(z[:, None] + xlog)
            a = np.log1p(s)
            b = s/(1+s)
            remainder = a-b
            small = b < 1e-3
            remainder[small] = sum(b[small]**j/j for j in range(2, 8))
            score = (w @ remainder.T)/(w @ a.T) - w @ b.T
            crossings = np.sum((score[:, :-1] > 0) & (score[:, 1:] < 0), axis=1)
            found = np.maximum(found, crossings)
            cases += len(w)
            multi = np.flatnonzero(crossings >= 2)
            if len(multi):
                i = min(multi, key=lambda j: n[j])
                candidate = (n[i], steps, tuple(weights)[i])
                if smallest is None or candidate[0] < smallest[0]:
                    smallest = candidate
        print(f"{k} points: {cases} grids, {len(w)} weights, "
              f"at most {max(found)} detected maxima, smallest detected {smallest}",
              flush=True)


def refine():
    mp.mp.dps = 100

    def values(z, t):
        a = mp.exp(-t)
        rate = mp.exp(z)
        v = [rate, 100*rate, a*rate]
        p = [x/(1+x) for x in v]
        avg = lambda vals: (10*vals[0] + 10*vals[1] + vals[2])/21
        A = avg([mp.log1p(x) for x in v])
        B = avg(p)
        C = avg([x/(1+x)**2 for x in v])
        gap = z + mp.log((1010+a)/21) - mp.log(A) - A
        return gap, 1-B/A-B, B**2/A**2-C/A-C

    def bisect(f, left, right):
        left, right = mp.mpf(left), mp.mpf(right)
        fl, fr = f(left), f(right)
        assert fl*fr < 0
        for _ in range(300):
            mid = (left+right)/2
            fm = f(mid)
            if fm == 0:
                return mid
            if fl*fm > 0:
                left, fl = mid, fm
            else:
                right = mid
        return (left+right)/2

    def peaks(t):
        left = bisect(lambda z: values(z, t)[1], -3, 1)
        right = bisect(lambda z: values(z, t)[1], t-6, t+6)
        return left, right

    def difference(t):
        left, right = peaks(t)
        return values(right, t)[0]-values(left, t)[0]

    # The exact proof uses a much broader parameter bracket.
    t = mp.findroot(difference, (80, 120), tol=mp.mpf("1e-88"))
    left, right = peaks(t)
    assert abs(difference(t)) < mp.mpf("1e-80")
    for z in (left, right):
        gap, score, curvature = values(z, t)
        assert abs(score) < mp.mpf("1e-80") and curvature < 0 and gap > 0
        print("z, gap, curvature:", *(mp.nstr(x, 25) for x in (z, gap, curvature)))
    print("t, a:", mp.nstr(t, 25), mp.nstr(mp.exp(-t), 25))
    print("nearby right-minus-left gaps:",
          *(mp.nstr(difference(t+dt), 15) for dt in (-1, 1)))
    assert difference(t-1) < 0 < difference(t+1)


if __name__ == "__main__":
    scan()
    refine()
