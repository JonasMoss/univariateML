"""Search stationary pairs and triples with quadrature in log(y/mean)."""
from collections import Counter
from functools import lru_cache
from itertools import combinations
from pathlib import Path
import argparse
import json

import mpmath as mp
import numpy as np
from numpy.polynomial.legendre import leggauss

CELLS = np.array([0, 1, 2, 3, 4, 6, 9, 14, 21, 32, 48, 73, 110, 250, 1000, 10000])
PAIRS = np.array([(i, j) for i, j in combinations(range(len(CELLS)), 2)
                  if CELLS[j] > CELLS[i] + 1])
TRIPLES = np.array(list(combinations(range(len(CELLS)), 3)))
NODES, WEIGHTS = leggauss(96)
DROP = 90.


@lru_cache(maxsize=None)
def special(shape):
    with mp.workdps(40):
        a = mp.mpf(shape)
        return float(mp.log(a) - mp.digamma(a)), float(mp.polygamma(1, a) - 1/a)


def deviance(z):
    small = np.abs(z) < 1.e-3
    series = z*z*(.5 + z*(1/6 + z*(1/24 + z*(1/120 + z/720))))
    return np.where(small, series, np.expm1(z) - z)


def moments(a, mean, cells=CELLS):
    # Use the log-density relative to its maximum on each bin.
    right = np.log((cells + 1) / mean)
    left = np.full(len(cells), -np.inf)
    positive = cells > 0
    left[positive] = np.log(cells[positive] / mean)
    mode = np.clip(0., left, right)
    mode_energy = deviance(mode)
    left = np.maximum(left, np.minimum(mode, -1 - mode_energy - DROP/a))
    endpoints = []
    # Limit each tail to a drop of DROP log units. This is exploratory;
    # mp re-evaluates selected cases from the incomplete-gamma integrals.
    for outside in (left, right):
        if np.all(a*(deviance(outside) - mode_energy) <= DROP):
            endpoints.append(outside)
            continue
        far, near = outside.copy(), mode.copy()
        for _ in range(38):
            mid = (far + near) / 2
            too_far = a*(deviance(mid) - mode_energy) > DROP
            far = np.where(too_far, mid, far)
            near = np.where(too_far, near, mid)
        endpoints.append(far)

    lower, upper = endpoints
    z = lower[:, None] + (upper - lower)[:, None]*(NODES + 1)/2
    result = quadrature_stats(a, z, WEIGHTS, mode_energy)
    if cells[0] == 0 and lower[0] < -30 < upper[0]:
        # A single panel can miss the rapid transition near the right
        # endpoint when the lower log tail is of length DROP / a.
        ends = np.array([lower[0], -30, upper[0]])
        widths = np.diff(ends)
        z0 = ends[:-1, None] + widths[:, None]*(NODES + 1)/2
        w0 = widths[:, None]*WEIGHTS
        result[:, 0] = quadrature_stats(
            a, z0.reshape(1, -1), w0.reshape(1, -1), mode_energy[:1])[:, 0]
    return result


def quadrature_stats(a, z, weights, mode_energy):
    d = deviance(z)
    x = np.expm1(z)
    weight = weights * np.exp(-a*(d - mode_energy[:, None]))
    weight /= weight.sum(axis=1)[:, None]
    avg_d = (weight*d).sum(axis=1)
    avg_x = (weight*x).sum(axis=1)
    delta, diff = special(a)
    dd, dx = d - avg_d[:, None], x - avg_x[:, None]
    # Scores in (log(y/m) - y/m, y/m), with shapes standardized
    # so that the full covariance is the identity.
    return np.array([
        (delta - avg_d)/np.sqrt(diff),
        avg_x*np.sqrt(a),
        (weight*dd**2).sum(axis=1)/diff - 1,
        -(weight*dd*dx).sum(axis=1)*np.sqrt(a/diff),
        (weight*dx**2).sum(axis=1)*a - 1,
    ])


def cross(m, indices):
    i, j = indices.T
    return m[0, i]*m[1, j] - m[1, i]*m[0, j]


def relative_curvature(w, hess):
    aa, ar, rr = np.einsum('ij,kij->ki', w, hess)
    # This is profile curvature divided by the full centered shape variance.
    return np.where(rr < 0, aa - ar**2 / np.minimum(rr, -1.e-300), np.nan)


def grid():
    summary = Counter()
    best = {}
    cases = {}
    marks = np.unique(np.r_[np.geomspace(.005, 20000, 65),
                            (CELLS[:, None] + [0.01, .2, .5, .8, .99]).ravel()])
    for a in np.geomspace(.0001, 1.e4, 33):
        previous = None
        for mean in marks:
            m = moments(a, mean)
            w = np.stack([cross(m, TRIPLES[:, [1, 2]]),
                          -cross(m, TRIPLES[:, [0, 2]]),
                          cross(m, TRIPLES[:, [0, 1]])], axis=1)
            # Very nearly collinear scores cannot resolve feasibility.
            scale = np.linalg.norm(m[:2, TRIPLES], axis=0).max(axis=1)**2
            mask = np.all(w > 0, axis=1) | np.all(w < 0, axis=1)
            mask &= np.abs(w.sum(axis=1)) > 1.e-13*scale
            w = w[mask]
            w /= w.sum(axis=1)[:, None]
            curv = relative_curvature(w, m[2:, TRIPLES[mask]])
            summary['triples'] += len(curv)
            summary['triple_above_1e-6'] += np.count_nonzero(curv > 1.e-6)
            for zero in (False, True):
                sub = (TRIPLES[mask, 0] == 0) == zero
                if np.any(sub & np.isfinite(curv)):
                    index = np.argmax(np.where(sub & np.isfinite(curv), curv, -np.inf))
                    key = 'triple_zero' if zero else 'triple_positive'
                    value = curv[index]
                    if value > best.get(key, -np.inf):
                        best[key] = value
                        cases[key] = [float(a), float(mean), CELLS[TRIPLES[mask][index]].tolist()]
                resolved = (sub & np.isfinite(curv) & (w.min(axis=1) > 1.e-5)
                            & (np.linalg.norm(m[:2, TRIPLES[mask]], axis=0).min(axis=1) > 1.e-6))
                if np.any(resolved):
                    index = np.argmax(np.where(resolved, curv, -np.inf))
                    key = 'triple_zero_resolved' if zero else 'triple_positive_resolved'
                    value = curv[index]
                    if value > best.get(key, -np.inf):
                        best[key] = value
                        cases[key] = [float(a), float(mean), CELLS[TRIPLES[mask][index]].tolist()]

            determinant = cross(m, PAIRS)
            if previous is not None:
                previous_mean, previous_det, previous_rate_score = previous
                spans = (determinant*previous_det < 0)
                # A pair is only feasible if its rate scores bracket zero.
                # Keep intervals where the endpoint score ranges allow that.
                spans &= np.minimum(m[1, PAIRS[:, 0]], previous_rate_score[PAIRS[:, 0]]) < 0
                spans &= np.maximum(m[1, PAIRS[:, 1]], previous_rate_score[PAIRS[:, 1]]) > 0
                for index in np.flatnonzero(spans):
                    cells = CELLS[PAIRS[index]]
                    lo, hi = previous_mean, mean
                    sign = np.sign(previous_det[index])
                    for _ in range(30):
                        mid = (lo + hi)/2
                        mm = moments(a, mid, cells)
                        det = mm[0, 0]*mm[1, 1] - mm[0, 1]*mm[1, 0]
                        if np.sign(det) == sign:
                            lo = mid
                        else:
                            hi = mid
                    if mm[1, 0] < 0 < mm[1, 1]:
                        summary['pairs'] += 1
                        w = mm[1, ::-1] * [1, -1] / (mm[1, 1] - mm[1, 0])
                        curvature = relative_curvature(w[None, :], mm[2:, None, :])[0]
                        summary['pair_above_1e-6'] += curvature > 1.e-6
                        if (np.min(w) > .001 and np.min(np.abs(mm[1])) > 1.e-6
                                and np.isfinite(curvature)):
                            key = 'pair_zero' if cells[0] == 0 else 'pair_positive'
                            if curvature > best.get(key, -np.inf):
                                best[key] = curvature
                                cases[key] = [float(a), float(lo), float(hi), cells.tolist()]
            previous = mean, determinant, m[1]
    assert summary['pairs'] and summary['triples']
    print({key: int(value) for key, value in summary.items()}, flush=True)
    print({key: float(value) for key, value in best.items()}, flush=True)
    return cases


def mp_cells(a, m, cells, with_hessian=True):
    scores, hess, _ = mp_intervals(a, m, [(k, k + 1) for k in cells], with_hessian)
    return scores, hess


def mp_intervals(a, m, intervals, with_hessian=True, with_mass=False):
    rate = a/m
    psi = mp.digamma(a)
    trigamma = mp.polygamma(1, a)
    scores, hess, probs = [], [], []
    for lower, upper in intervals:
        def z(s):
            return mp.gammainc(s, rate*lower, rate*upper)

        mass = z(a)
        if with_mass:
            probs.append(mp.exp(mp.log(mass) - mp.loggamma(a)))
        mu = z(a + 1)/(rate*mass)
        au = mp.diff(lambda s: mp.log(z(s)), a)
        # In the fixed basis (log(y) - y/m, y/m), at the given (a,m).
        sx = mu/m - 1
        sa = au - psi - sx
        scores.append((sa, sx))
        if not with_hessian:
            continue
        vy = (z(a + 2)/mass - (z(a + 1)/mass)**2)/a**2
        cy = mp.diff(lambda s: z(s + 1)/z(s), a)/a
        vt = mp.diff(lambda s: mp.log(z(s)), a, 2)
        hess.append((vt - 2*cy + vy - (trigamma - 1/a), cy - vy, vy - 1/a))
    return scores, hess, probs


def replay_saturation(name, case):
    """Keep the exponentially small curvature direction out of subtractions."""
    a, m = [mp.mpf(str(v)) for v in case[:2]]
    cells = case[-1]
    assert cells == list(range(cells[0], cells[-1] + 1)) and len(cells) == 3
    complement = [(cells[-1] + 1, mp.inf)]
    if cells[0]:
        complement.insert(0, (0, cells[0]))
    intervals = [(k, k + 1) for k in cells] + complement
    scores, hess, probs = mp_intervals(a, m, intervals, with_mass=True)
    bulk, minor = sorted(range(3), key=lambda i: probs[i], reverse=True)[:2]
    rest = [i for i in range(len(intervals)) if i != bulk]
    probs[bulk] = 1 - sum(probs[i] for i in rest)

    slope = -scores[minor][0]/scores[minor][1]
    scores = [(x + slope*y, y) for x, y in scores]
    scores[minor] = (mp.mpf(0), scores[minor][1])
    scores[bulk] = tuple(-sum(probs[i]*scores[i][j] for i in rest)/probs[bulk]
                         for j in range(2))

    # Express the frequency displacement from model probabilities using
    # only the omitted masses and barycentric coordinates of their scores.
    columns = mp.matrix([[1, *column] for column in scores[:3]]).T
    delta = mp.matrix([0, 0, 0])
    for i in range(3, len(intervals)):
        delta += probs[i]*mp.lu_solve(columns, mp.matrix([1, *scores[i]]))
    w = [probs[i] + delta[i] for i in range(3)]
    assert all(wi > 0 for wi in w)

    hess = [(aa + 2*slope*ar + slope**2*rr, ar + slope*rr, rr)
            for aa, ar, rr in hess]
    squares = [(x*x, x*y, y*y) for x, y in scores]
    hess[bulk] = tuple(-sum(probs[i]*(hess[i][j] + squares[i][j]) for i in rest)
                       / probs[bulk] - squares[bulk][j] for j in range(3))
    aa, ar, rr = [
        sum(delta[i]*hess[i][j] for i in range(3))
        - sum(probs[i]*hess[i][j] for i in range(3, len(intervals)))
        - sum(p*ss[j] for p, ss in zip(probs, squares))
        for j in range(3)
    ]
    assert rr < 0
    value = (aa - ar**2/rr)/(mp.polygamma(1, a) - 1/a)
    print(mp.mp.dps, name, 'weights', [mp.nstr(wi, 10) for wi in w],
          'relative curvature', mp.nstr(value, 16), flush=True)
    return value


def replay(name, case):
    a = mp.mpf(str(case[0]))
    cells = case[-1]
    if len(cells) == 2:
        if cells[1] == cells[0] + 1:
            print(mp.mp.dps, name, case, 'adjacent pair has no exact stationary point')
            return

        def determinant(mean):
            s, _ = mp_cells(a, mean, cells, with_hessian=False)
            return s[0][0]*s[1][1] - s[0][1]*s[1][0]

        lo, hi = map(lambda v: mp.mpf(str(v)), case[1:3])
        # Enlarge the numerical bracket to account for floating-point error.
        pad = (hi - lo)*100 + (lo + hi)*mp.mpf('1e-7') if hi - lo < lo/100 else 0
        lo, hi = lo - pad, hi + pad
        initial = determinant(lo)
        if not (lo > 0 and initial*determinant(hi) < 0):
            print(mp.mp.dps, name, case, 'unresolved determinant bracket')
            return
        for _ in range(2*mp.mp.dps):
            mid = (lo + hi)/2
            if initial*determinant(mid) > 0:
                lo = mid
            else:
                hi = mid
        m = (lo + hi)/2
    else:
        m = mp.mpf(str(case[1]))
    s, h = mp_cells(a, m, cells)
    if len(cells) == 2:
        w0 = s[1][1]/(s[1][1] - s[0][1])
        w = [w0, 1 - w0]
    else:
        w = mp.lu_solve(mp.matrix([[1, *column] for column in s]).T,
                        mp.matrix([1, 0, 0]))
    aa, ar, rr = [sum(wi*row[i] for wi, row in zip(w, h)) for i in range(3)]
    if not all(wi > 0 for wi in w) or not rr < 0:
        print(mp.mp.dps, name, case, 'no usable sign', 'weights',
              [mp.nstr(wi, 8) for wi in w], 'rate curvature', mp.nstr(rr, 8), flush=True)
        return
    value = (aa - ar**2/rr)/(mp.polygamma(1, a) - 1/a)
    print(mp.mp.dps, name, case, 'm', mp.nstr(m, 14), 'weights',
          [mp.nstr(wi, 8) for wi in w], 'relative curvature', mp.nstr(value, 16), flush=True)
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('cases', type=Path)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--replay', action='store_true')
    mode.add_argument('--saturation', action='store_true')
    args = parser.parse_args()
    if args.saturation:
        cases = json.loads(args.cases.read_text())
        for digits in (80, 120):
            with mp.workdps(digits):
                for name in ('triple_zero', 'triple_positive'):
                    replay_saturation(name, cases[name])
    elif not args.replay:
        args.cases.write_text(json.dumps(grid(), indent=2) + '\n')
    else:
        cases = json.loads(args.cases.read_text())
        for digits in (80, 120):
            with mp.workdps(digits):
                for name, case in (
                    ('separated zero pair', [1, 10, 99, [0, 100]]),
                    ('separated positive pair', [1, 2, 10, [1, 100]]),
                ):
                    assert replay(name, case) < 0
                # Two suspect signs from the preliminary single-panel scan.
                replay('single-panel zero triple',
                       [.0031622776601683794, .0063405537968470324, [0, 9, 14]])
                replay('single-panel zero pair',
                       [.0031622776601683794, .007519448953766669,
                        .007519448955349889, [0, 6]])
                for name, case in cases.items():
                    replay(name, case)


if __name__ == '__main__':
    main()
