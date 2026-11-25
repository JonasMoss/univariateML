"""Screen fixed-mean histogram polytopes for size-step reversals."""
from fractions import Fraction
from itertools import combinations

import numpy as np

from histograms import profile


def mean_vertices(atoms, mean):
    """Extreme histograms for a fixed total mass and mean."""
    vertices = []
    for i, j in combinations(range(len(atoms)), 2):
        if atoms[i] <= mean <= atoms[j]:
            vector = np.zeros(len(atoms))
            vector[i] = (atoms[j] - mean) / (atoms[j] - atoms[i])
            vector[j] = 1 - vector[i]
            vertices.append(vector)
    return np.array(vertices)


def joint_signs(vertices, first, second):
    """Intersect the two-score hull with a translated positive quadrant."""
    x, y = first - 1e-11, second - 1e-11
    interior = np.flatnonzero((x > 0) & (y > 0))
    if len(interior):
        return vertices[interior[0]]
    left, right = np.flatnonzero(x <= 0), np.flatnonzero(x > 0)
    if not len(left) or not len(right):
        return None
    fraction = -x[left, None] / (x[right] - x[left, None])
    crossings = (1-fraction)*y[left, None] + fraction*y[right]
    i, j = np.unravel_index(np.argmax(crossings), crossings.shape)
    if crossings[i, j] > 0:
        weight = fraction[i, j]
        return (1-weight)*vertices[left[i]] + weight*vertices[right[j]]
    return None


def profiles(lower, maximum, mean, sizes):
    y = np.arange(sizes[-1] - lower + 1)
    factorial = np.r_[0., np.cumsum(np.log(lower + y[1:]))]
    rows = []
    for size in sizes:
        h = size - lower
        k = y[:h + 1]
        coeff = np.r_[0., np.cumsum(np.log1p(-k[:-1] / h))]
        # Divide out the weight at L and scale odds by H.
        base = coeff - factorial[:h + 1]
        lo, hi = -50., 50.
        for _ in range(55):
            eta = (lo + hi) / 2
            logs = base + eta * k
            peak = max(logs)
            w = np.exp(logs - peak)
            if np.dot(k, w) < mean * sum(w):
                lo = eta
            else:
                hi = eta
        rows.append(coeff[:maximum-lower+1] + mean * eta - peak - np.log(sum(w)))
    return np.array(rows)


def main():
    fractions = (1e-5, .0001, .001, .003, .01, .025, .05, .1, .2, .35, .5,
                 .65, .8, .9, .95, .975, .99, .997, .999, .9999, .99999)
    offsets = np.array([0, 1, 2, 3, 4, 5, 8, 12, 20, 35, 60, 100])
    settings = pairs = verified = 0
    for lower in (1, 2, 3, 5, 10, 30, 100, 1000, 1000000):
        for width in (2, 3, 4, 5, 6, 8, 12, 16, 24, 32):
            maximum = lower + width
            atoms = np.arange(width+1)
            steps = maximum + offsets
            sizes = np.unique(np.r_[steps, steps+1])
            for f in fractions:
                mean = width * f
                vertices = mean_vertices(atoms, mean)
                table = profiles(lower, maximum, mean, sizes)
                diffs = table[np.searchsorted(sizes,steps+1)]-table[np.searchsorted(sizes,steps)]
                signs = diffs @ vertices.T
                for i, j in combinations(range(len(steps)), 2):
                    pairs += 1
                    candidate = joint_signs(vertices, -signs[i], signs[j])
                    if candidate is None:
                        continue
                    counts = tuple(int(round(h * 10**18)) for h in candidate)
                    mean_exact = Fraction(sum((lower+k)*c for k,c in enumerate(counts)), sum(counts))
                    def diff(size):
                        a, b = (profile(lower, int(j), mean_exact.numerator, mean_exact.denominator)
                                for j in (size,size+1))
                        return sum(c*(y-x) for c,x,y in zip(counts,a,b) if c)
                    first, second = diff(steps[i]), diff(steps[j])
                    if first.b < 0 < second.a:
                        print("reversal", lower, counts, steps[i], steps[j], first, second, flush=True)
                        verified += 1
                settings += 1
    print(settings, "settings,", pairs, "pairs,", verified, "verified reversals")


if __name__ == "__main__":
    main()
