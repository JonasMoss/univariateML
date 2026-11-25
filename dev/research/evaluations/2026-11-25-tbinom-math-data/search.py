"""Binary64 screening at fixed sample means. Verify any hits with check.py."""
import itertools
import math

import numpy as np

from check import finite


FRACTIONS = (.002, .01, .025, .05, .075, .1, .15, .25, .35, .5, .65, .8, .95)
OFFSETS = (0, 1, 2, 3, 5, 8, 12, 20, 30, 45, 70, 105)


def bisect_rate(moment, mean, lo, hi):
    for _ in range(52):
        mid = (lo + hi) / 2
        if moment(mid) < mean:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def weights(logweights):
    scale = max(logweights)
    relative = np.exp(logweights - scale)
    return relative, scale


def scores(lower, upper, maximum, mean, sizes):
    result = []
    for size in sizes:
        k = np.arange(lower, min(size, upper) + 1)
        c = np.r_[0, np.cumsum(np.log1p(-np.arange(size) / size))][k]
        base = c - np.array([math.lgamma(j+1) for j in k])

        def moment(eta):
            w, _ = weights(base + eta*k)
            return np.dot(k, w)/sum(w)

        eta = bisect_rate(moment, mean, -40, 40)
        w, scale = weights(base + eta*k)
        # Drop -log(k!), which cancels in all comparisons.
        result.append(c[:maximum-lower+1] + mean*eta - scale - np.log(sum(w)))
    return np.array(result)


def mean_vertices(atoms, mean):
    vertices = []
    for i, j in itertools.combinations(range(len(atoms)), 2):
        if atoms[i] <= mean <= atoms[j]:
            vector = np.zeros(len(atoms))
            vector[i] = (atoms[j]-mean)/(atoms[j]-atoms[i])
            vector[j] = 1-vector[i]
            vertices.append(vector)
    return np.array(vertices)


def joint_signs(vertices, x, y):
    """Test a translated positive quadrant against a two-score convex hull."""
    tol = 1e-11
    x = x - tol
    y = y - tol
    interior = np.flatnonzero((x > 0) & (y > 0))
    if len(interior):
        return vertices[interior[0]]
    left = np.flatnonzero(x < 0)
    right = np.flatnonzero(x > 0)
    if not len(left) or not len(right):
        return None
    fraction = -x[left,None] / (x[right] - x[left,None])
    crossings = (1-fraction)*y[left,None] + fraction*y[right]
    i, j = np.unravel_index(np.argmax(crossings), crossings.shape)
    if crossings[i,j] > 0:
        p = fraction[i,j]
        return (1-p)*vertices[left[i]]+p*vertices[right[j]]
    return None


def scan_lower_windows():
    cases = 0
    hits = 0
    for lower in (1, 2, 3, 5, 10):
        for maximum in range(lower+2, lower+25):
            atoms = np.arange(lower, maximum+1)
            steps = maximum + np.array(OFFSETS)
            sizes = np.unique(np.r_[steps, steps+1])
            for f in FRACTIONS:
                mean = lower + (maximum-lower)*f
                vertices = mean_vertices(atoms, mean)
                v = scores(lower, 10**9, maximum, mean, sizes)
                diffs = v[np.searchsorted(sizes,steps+1)]-v[np.searchsorted(sizes,steps)]
                scores_at_vertices = diffs @ vertices.T
                for i, j in itertools.combinations(range(len(steps)), 2):
                    candidate = joint_signs(vertices, -scores_at_vertices[i], scores_at_vertices[j])
                    if candidate is not None:
                        counts = np.rint(candidate*10**14).astype(int).tolist()
                        before, after = int(steps[i]), int(steps[j])
                        def step_at(size):
                            upper = float("inf")
                            return (finite(size+1, lower, upper, counts)[0]
                                    - finite(size, lower, upper, counts)[0])
                        first, second = step_at(before), step_at(after)
                        accepted = first.b < 0 < second.a
                        print("integer reversal candidate", lower, maximum,
                              mean, before, after, counts, "verified", accepted,
                              first, second, flush=True)
                        hits += int(accepted)
                        if hits >= 4:
                            return
                cases += 1
    print("lower-only screens", cases, "reversal candidates", hits, flush=True)


if __name__ == "__main__":
    scan_lower_windows()
