"""Finite-sample affine-invariant intervals, with monotone outer inversion."""
import csv
from pathlib import Path

import numpy as np


REPLICATES = 12000
REFERENCE = 7999
TAIL_RANK = 100
PIVOT_TAIL = .0125


def orders(rng, n, size):
    # Consecutive uniform spacings are Dirichlet(1,...,1). Aggregate
    # spacings to generate just the six required order statistics.
    indices = np.array([1, n//4, n//2, 3*n//4, n-1, n])
    gaps = np.diff(np.r_[0, indices, n+1])
    sums = rng.gamma(gaps, size=(size, gaps.size)).cumsum(axis=1)
    return sums[:, :-1] / sums[:, -1:]


def quantile(family, shape, u):
    if family == 'triang':
        return np.where(u <= shape, np.sqrt(shape*u),
                        1-np.sqrt((1-shape)*(1-u)))
    s = -np.log1p(-u) if family == 'gpd' else -np.log(-np.log(u))
    return s if shape == 0 else np.expm1(shape*s)/shape


def invariant(family, x):
    if family == 'gpd':
        return x[:, 3] / x[:, 1]
    ratio = (x[:, 3]-x[:, 2]) / (x[:, 2]-x[:, 1])
    return -ratio if family == 'triang' else ratio


def calibration(family, grid, u):
    bounds = []
    for shape in grid:
        ratios = invariant(family, quantile(family, shape, u))
        low, high = TAIL_RANK-1, REFERENCE-TAIL_RANK
        order = np.partition(ratios, (low, high))
        bounds.append((order[low], order[high]))
    bounds = np.array(bounds)
    assert np.all(np.diff(bounds, axis=0) >= -1e-12)
    # Triangular ratios can be flat on a branch. Enlarge the brackets
    # before searching so floating-point fluctuations preserve ordering.
    lower = np.minimum.accumulate(bounds[::-1, 0])[::-1] - 1e-12
    upper = np.maximum.accumulate(bounds[:, 1]) + 1e-12
    return lower, upper


def shape_interval(r, grid, critical, domain):
    q_low, q_high = critical
    # Strict comparisons retain ties. The grid only restricts the hull
    # using monotonicity and can never remove an off-grid accepted value.
    below = np.searchsorted(q_high, r, side='left') - 1
    above = np.searchsorted(q_low, r, side='right')
    lo = np.where(below >= 0, grid[np.maximum(below, 0)], domain[0])
    hi = np.where(above < len(grid), grid[np.minimum(above, len(grid)-1)], domain[1])
    return lo, hi


def gev_gap_quantile(n, xi, p):
    """Quantile of (endpoint - max)/(max - second largest)."""
    log_ratio = np.log1p(n*(1-p)/((n-1)*p))
    return np.divide(1., np.expm1(-xi*log_ratio),
                     out=np.full_like(xi, np.inf), where=xi < 0)


def triangular_box(x, lo, hi, n):
    e = PIVOT_TAIL/2
    v_low, v_high = -np.expm1(np.log1p(-e)/n), -np.expm1(np.log(e)/n)
    u_low, u_high = e**(1/n), (1-e)**(1/n)

    def q(p, u):
        return np.where(u <= p, np.sqrt(p*u), 1-np.sqrt((1-p)*(1-u)))

    lower_q = q(lo, v_low), q(hi, v_high)
    upper_q = q(lo, u_low), q(hi, u_high)
    left, right = x[:, 0], x[:, -1]
    span = right-left
    width_lo = span/(upper_q[1]-lower_q[0])
    width_hi = np.divide(span, upper_q[0]-lower_q[1],
                         out=np.full_like(span, np.inf),
                         where=upper_q[0] > lower_q[1])
    a = left-width_hi*lower_q[1], left-width_lo*lower_q[0]
    b = right+width_lo*(1-upper_q[1]), right+width_hi*(1-upper_q[0])
    d_lo, d_hi = lo-lower_q[1], hi-lower_q[0]
    c_lo = left+np.where(d_lo < 0, width_hi, width_lo)*d_lo
    c_hi = left+np.where(d_hi > 0, width_hi, width_lo)*d_hi
    return a, b, (c_lo, c_hi)


def endpoint_box(family, x, lo, hi, n):
    maximum = x[:, -1]
    neg_lo, neg_hi = np.minimum(lo, 0), np.minimum(hi, 0)
    if family == 'gpd':
        # Negative shape: max / endpoint = 1 - (1-U_max)^(-xi).
        u_lo, u_hi = PIVOT_TAIL**(1/n), (1-PIVOT_TAIL)**(1/n)
        d_lo = -np.expm1(-neg_hi*np.log1p(-u_lo))
        d_hi = -np.expm1(-neg_lo*np.log1p(-u_hi))
        lower = np.divide(maximum, d_hi, out=np.full_like(maximum, np.inf), where=d_hi > 0)
        upper = np.divide(maximum, d_lo, out=np.full_like(maximum, np.inf), where=d_lo > 0)
        return lower, upper
    gap = maximum-x[:, -2]
    with np.errstate(divide='ignore'):
        return (maximum+gap*gev_gap_quantile(n, neg_lo, PIVOT_TAIL),
                maximum+gap*gev_gap_quantile(n, neg_hi, 1-PIVOT_TAIL))


def contains(bounds, value):
    lower, upper = bounds
    assert np.all(lower <= upper)
    return (lower <= value) & (value <= upper)


def write_cell(rows, family, n, shape, target, hits, bounds):
    coverage = hits.mean()
    low, high = bounds
    finite = np.isfinite(low) & np.isfinite(high)
    med_width = np.median(high[finite]-low[finite]) if finite.any() else float('nan')
    finite_fraction = f'{finite.mean():.6f}' if target != 'joint' else ''
    width = f'{med_width:.6g}' if target != 'joint' else ''
    assert coverage >= .95 - 5*np.sqrt(.95*.05/REPLICATES)
    rows.append((family, n, f'{shape:.10g}', target, int(hits.sum()),
                 REPLICATES, f'{coverage:.6f}',
                 f'{np.sqrt(coverage*(1-coverage)/REPLICATES):.6f}',
                 finite_fraction, width))


def run():
    rng = np.random.default_rng(28320261125)
    rows = []
    for n in [32, 128, 512]:
        for family in ['gpd', 'gev', 'triang']:
            domain = (0., 1.) if family == 'triang' else (-1., 1. if family == 'gev' else np.inf)
            grid = (np.linspace(0, 1, 201) if family == 'triang'
                    else np.linspace(-1, 1 if family == 'gev' else 4, 401))
            u_reference = orders(rng, n, REFERENCE)
            critical = calibration(family, grid, u_reference)
            shapes = [0., .35, 1.] if family == 'triang' else [
                -1., -.75, -.5, -1/(2-1/np.log(n)), -1/(2+1/np.log(n))]
            for shape in shapes:
                u = orders(rng, n, REPLICATES)
                x = quantile(family, shape, u)
                ratio = invariant(family, x)
                lo, hi = shape_interval(ratio, grid, critical, domain)
                shape_hit = contains((lo, hi), shape)
                # Check monotone grid projection contains all acceptances
                # of the off-grid full-null rank test, up to roundoff.
                direct = calibration(family, [shape], u_reference)
                rank_accept = (direct[0][0] <= ratio) & (ratio <= direct[1][0])
                assert np.all(~rank_accept | shape_hit)
                if family == 'triang':
                    bounds = triangular_box(x, lo, hi, n)
                    hits = [contains(b, v) for b, v in zip(bounds, [0, 1, shape])]
                    for target, hit, b in zip(['a', 'b', 'mode'], hits, bounds):
                        write_cell(rows, family, n, shape, target, hit, b)
                else:
                    box = endpoint_box(family, x, lo, hi, n)
                    # An empty negative-shape projection has no finite
                    # endpoint to report. Allow an extended +infinity.
                    hits = [contains(box, -1/shape)]
                    write_cell(rows, family, n, shape, 'endpoint', hits[0], box)
                write_cell(rows, family, n, shape, 'shape' if family != 'triang' else 'mode_fraction',
                           shape_hit, (lo, hi))
                joint = shape_hit & np.logical_and.reduce(hits)
                write_cell(rows, family, n, shape, 'joint', joint, (lo, hi))
    output = Path(__file__).with_name('coverage.csv')
    with output.open('w', newline='') as stream:
        writer = csv.writer(stream, lineterminator='\n')
        writer.writerow(['family', 'n', 'xi_or_mode_fraction', 'target', 'covered',
                         'replicates', 'coverage', 'mcse', 'finite_fraction',
                         'median_finite_width'])
        writer.writerows(rows)
    print(f'{len(rows)} coverage rows passed')


if __name__ == '__main__':
    run()
