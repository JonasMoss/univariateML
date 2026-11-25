"""Search for stationary saddles by enumerating frequency vertices."""
import itertools
from functools import lru_cache

import mpmath as mp
import numpy as np
from numpy.polynomial.legendre import leggauss

NODES, WEIGHTS = leggauss(96)
CELLS = np.r_[np.arange(31), 40, 60, 100, 200, 500]
TRIPLES = np.array(list(itertools.combinations(range(len(CELLS)), 3)))


@lru_cache(maxsize=None)
def gamma_derivatives(shape):
    with mp.workdps(30):
        return float(mp.digamma(float(shape))), float(mp.polygamma(1, float(shape)))


def moments(shape, rate):
    # Positive cells use y. The zero cell uses t=-log(y), with
    # the integration endpoint chosen for the decay e^(-shape*t).
    y = CELLS[:, None] + (1 + NODES) / 2
    log_y = np.log(y)
    log_weight = (shape - 1) * log_y - rate * y
    cutoff = max(0, np.log(rate / shape)) + 45 / shape
    log_y[0] = -cutoff * (1 + NODES) / 2
    y[0] = np.exp(log_y[0])
    log_weight[0] = shape * log_y[0] - rate * y[0]
    weight = WEIGHTS * np.exp(log_weight - log_weight.max(axis=1)[:, None])
    weight /= weight.sum(axis=1)[:, None]
    mean_log = np.sum(weight * log_y, axis=1)
    mean_y = np.sum(weight * y, axis=1)
    dl = log_y - mean_log[:, None]
    dy = y - mean_y[:, None]
    psi, trigamma = gamma_derivatives(shape)
    return (
        mean_log - psi + np.log(rate),
        (mean_y * rate / shape - 1),
        np.sum(weight * dl**2, axis=1) - trigamma,
        np.sum(weight * dl * dy, axis=1) * rate / shape - 1 / shape,
        np.sum(weight * dy**2, axis=1) * (rate / shape)**2 - 1 / shape,
    )


def stationary_vertices(shape, mean):
    scores_a, scores_r, *cov = moments(shape, shape / mean)
    a, b, c = scores_a[TRIPLES].T
    d, e, f = scores_r[TRIPLES].T
    weights = np.array([b*f-c*e, c*d-a*f, a*e-b*d]).T
    applicable = np.all(weights > 0, axis=1) | np.all(weights < 0, axis=1)
    weights = weights[applicable]
    weights /= weights.sum(axis=1)[:, None]
    triples = TRIPLES[applicable]
    aa, ab, bb = [np.sum(weights * item[triples], axis=1) for item in cov]
    valid = bb < 0
    curvature = np.full(len(bb), -np.inf)
    curvature[valid] = (aa[valid] - ab[valid]**2 / bb[valid]) * shape**2
    return curvature, triples, weights


def main():
    best = -np.inf
    count = positive = 0
    for shape in np.geomspace(.01, 1e4, 85):
        for mean in np.geomspace(.025, 350, 100):
            curvature, triples, weights = stationary_vertices(shape, mean)
            count += len(curvature)
            positive += np.count_nonzero(curvature > 1e-6)
            if len(curvature) and max(curvature) > best:
                index = np.argmax(curvature)
                best = curvature[index]
                print(shape, mean, best, CELLS[triples[index]], weights[index], flush=True)
    assert count > 0 and np.isfinite(best)
    print(f'{count=} {positive=} {best=}')


if __name__ == '__main__':
    main()
