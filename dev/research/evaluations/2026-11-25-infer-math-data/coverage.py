"""Fixed-seed coverage and boundary checks. NumPy, no fitting libraries."""
import csv
from pathlib import Path

import numpy as np

RNG = np.random.default_rng(265)
REPS = 12000
Z = 1.959963984540054
ROWS = []


def record(cell, method, accepted):
    accepted = np.asarray(accepted)
    p = accepted.mean()
    se = np.sqrt(p * (1 - p) / accepted.size)
    ROWS.append((cell, method, accepted.size, int(accepted.sum()), p, se))


def uniform_checks(n):
    sample = np.sort(RNG.random((REPS, n)), axis=1)
    lo, hi = sample[:, 0], sample[:, -1]
    cutoff = .05 ** (-1 / (n - 1)) - 1
    record(f'unif n={n}', 'unknown opposite endpoint exact one-sided', lo <= cutoff * (hi - lo))
    record(f'unif n={n}', 'ideal parametric and empirical percentile', hi >= 1)
    record(f'unif n={n}', 'known opposite endpoint exact one-sided', hi >= .05 ** (1/n))
    m = int(np.sqrt(n))
    # For known lower endpoint 0, the upper basic interval uses the 95%
    # conditional quantile of m*(max_n-max_m). For empirical replacement,
    # P(max_m <= X_(j))=(j/n)^m. For subsampling the CDF is C(j,m)/C(n,m).
    j = int(np.floor(n * .05 ** (1/m))) + 1
    record(f'unif n={n}', f'empirical m={m} basic', n * (1 - hi) <= m * (hi - sample[:, j-1]))
    cdf = 1.
    j = n
    while j >= m and cdf >= .05:
        cdf *= (j-m)/j
        j -= 1
    record(f'unif n={n}', f'subsample m={m} basic', n * (1 - hi) <= m * (hi - sample[:, j]))
    record(f'unif n={n}', 'ideal parametric basic n', n * (1 - hi) <= n * hi * (1 - .05 ** (1/n)))


def exp_pivots(n):
    # These are the log-distance experiment for both Pareto and power.
    logs = RNG.exponential(size=(REPS, n))
    d = logs.min(axis=1)
    s = (logs - d[:, None]).sum(axis=1)
    survival = np.exp(-(n-1) * np.log1p(n*d/s))
    record(f'Pareto/power n={n}', 'free shape exact endpoint two-sided', (.025 <= survival) & (survival <= .975))
    record(f'Pareto/power n={n}', 'free shape exact endpoint one-sided', survival >= .05)


def gpd_checks(n, alpha):
    # Negative shape held; true upper endpoint=1, lower endpoint=0.
    accepted = {'exact maximum pivot': [], 'critical normal': [], 'ordinary chi-square LR': []}
    for size in [200] * (REPS // 200):
        d = RNG.random((size, n)) ** (1 / alpha)
        max_x = 1 - d.min(axis=1)
        if alpha == 1:
            fit = max_x
        else:
            # Score sign equals sign((alpha-1)*sum(x/(e-x))-n).
            # It strictly decreases for e>max(x). For e=alpha*max(x)
            # it is nonpositive, so this bracket certifies the root.
            x = 1 - d
            lo, hi = max_x.copy(), alpha * max_x
            for _ in range(50):
                mid = (lo + hi)/2
                positive = (alpha - 1) * (x / (mid[:, None] - x)).sum(axis=1) > n
                lo = np.where(positive, mid, lo)
                hi = np.where(positive, hi, mid)
            assert np.max(hi - lo) < 1e-13
            fit = (lo + hi)/2
        endpoint_cdf = np.exp(n * np.log1p(-d.min(axis=1)**alpha))
        accepted['exact maximum pivot'].extend((.025 <= endpoint_cdf) & (endpoint_cdf <= .975))
        if alpha == 2:
            accepted['critical normal'].extend(np.abs(np.sqrt(n*np.log(n))*(fit-1)) <= Z*fit)
        llr = -alpha*n*np.log(fit)
        if alpha > 1:
            llr += (alpha - 1)*np.log1p((fit[:, None]-1)/d).sum(axis=1)
        accepted['ordinary chi-square LR'].extend(2*llr <= Z**2)
    for method, trials in accepted.items():
        if trials:
            record(f'GPD alpha={alpha} n={n}', method, trials)


def gev_spacing(n, alpha):
    first = RNG.exponential(1/n, size=REPS)
    second_gap = RNG.exponential(1/(n-1), size=REPS)
    distance = first**(1/alpha)
    spacing = (first+second_gap)**(1/alpha)-distance
    k = np.expm1(alpha*np.log1p(spacing/distance))
    survival = (n-1)*k/(n+(n-1)*k)
    record(f"GEV alpha={alpha} n={n}", "exact scale-free spacing", (.025 <= survival) & (survival <= .975))


def triangular_lr(n, count):
    # Fixed endpoints [0,1], true mode 0. On each observation interval
    # the log likelihood is convex in the mode, so knots suffice.
    x = np.sort(-np.expm1(.5 * np.log1p(-RNG.random((count, n)))), axis=1)
    k = np.arange(1, n + 1)
    left = np.cumsum(np.log(x) - np.log1p(-x), axis=1)
    knot = left - k*np.log(x) - (n-k)*np.log1p(-x)
    return 2*np.maximum.reduce([np.zeros(count), left[:, -1], knot.max(axis=1)])


def triangular_checks(n):
    draws = triangular_lr(n, REPS)
    training = triangular_lr(n, 23999)
    cutoff = np.sort(training)[int(.95*(len(training)+1))-1]
    record(f'triang mode 0 n={n}', 'independently calibrated null LR', draws <= cutoff)
    record(f'triang mode 0 n={n}', 'regular chi-square LR', draws <= Z**2)
    record(f'triang mode 0 n={n}', 'half chi-square LR', draws <= 1.6448536269514722**2)


def gaussian_checks():
    # Exactly Gaussian limit experiments, with the fitted nuisance
    # already projected out and the efficient variance set to one.
    g = RNG.normal(size=REPS)
    fit = np.maximum(g, 0)
    record('Gaussian half-line at zero', 'cone LR', fit <= 1.6448536269514722)
    record('Gaussian half-line at zero', 'regular chi-square LR', fit <= Z)
    record('Gaussian half-line at zero', 'ideal parametric percentile', fit <= Z)
    # The original skew-normal shape is a signed cube root in this chart.
    # Its naive delta/Wald interval accepts zero iff 3*abs(g) <= Z.
    record('signed cubic Gaussian at zero', 'invert interval in cubic chart', np.abs(g) <= Z)
    record('signed cubic Gaussian at zero', 'delta Wald in original chart', 3*np.abs(g) <= Z)


for n in (40, 400):
    uniform_checks(n)
    exp_pivots(n)
for alpha in (1., 4/3, 2.):
    for n in (80, 640):
        gpd_checks(n, alpha)
        gev_spacing(n, alpha)
for n in (40, 320):
    triangular_checks(n)
gaussian_checks()
assert all(reps == REPS for _, _, reps, *_ in ROWS)
for cell, method, reps, hits, p, se in ROWS:
    if 'exact' in method or method in ('cone LR', 'invert interval in cubic chart'):
        assert abs(p - .95) < .012, (cell, method, p)
    if method == 'ideal parametric and empirical percentile':
        assert hits == 0
with Path(__file__).with_name('coverage.csv').open('w', newline='') as output:
    writer = csv.writer(output)
    writer.writerow(('cell', 'method', 'replications', 'accepted', 'coverage', 'mcse'))
    writer.writerows(ROWS)
print(f'{len(ROWS)} coverage cells passed')
