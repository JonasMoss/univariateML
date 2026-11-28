"""Recompute illustrative conditional likelihood ratios at 60 and 90 digits."""
import csv
import pathlib

from mpmath import mp

DATA = pathlib.Path(__file__).resolve().parent
GRID = (16, 64, 256, 1024)


def likelihood(kernel, points, breaks):
    return mp.fprod(kernel(point) for point in points) / mp.quad(kernel, breaks)**len(points)


def near_wall(breaks, wall, n):
    nearby = [wall + mp.mpf(k)/n for k in (-64, -16, -4, 0, 4, 16, 64)]
    return sorted(set(breaks) | {y for y in nearby if breaks[0] < y < breaks[-1]})


def checks(n):
    inv_n = mp.one / n
    half = mp.mpf("0.5")
    beta_points = (mp.one, mp.mpf(2))
    beta_window = (mp.one, mp.mpf(3))
    beta_kernel = lambda x: (1 + inv_n*x)**(-n-1)
    exponential = lambda x: mp.exp(-x)
    yield "beta-prime-to-exponential", (
        likelihood(beta_kernel, beta_points, beta_window) /
        likelihood(exponential, beta_points, beta_window)), mp.one

    sample = (mp.mpf(-1), mp.zero)
    retained = (mp.mpf(-2), mp.zero, mp.one)
    wall_limit = mp.exp(-1) / (1-mp.exp(-2))**2
    for movable in (False, True):
        wall = mp.sqrt(inv_n) if movable else mp.zero
        kernel = lambda y: mp.exp(y-mp.exp(n*(y-wall)))
        label = "gamma-moving-wall" if movable else "gamma-held-wall"
        trace = mp.one if movable else mp.exp(-1)
        yield label, likelihood(kernel, sample, near_wall(retained, wall, n))/wall_limit, trace

    q = mp.mpf(3)
    kernel = lambda y: mp.exp(half*y-(half/n+q)*mp.log1p(mp.exp(n*y)))
    upper_power = (mp.exp(-half)*half**2 /
                   (1-mp.exp(-2*half))**2)
    yield "gb2-hard-wall-contact", likelihood(kernel, sample, near_wall(retained, mp.zero, n))/upper_power, 2**(-q)

    left, right = mp.mpf(2), mp.mpf(3)
    kernel = lambda y: mp.exp(left*y-(left+right)/n*mp.log1p(mp.exp(n*y)))
    broken = lambda y: mp.exp(left*y if y < 0 else -right*y)
    yield "gb2-two-piece-power", (
        likelihood(kernel, sample, near_wall(retained, mp.zero, n))/likelihood(broken, sample, retained)), mp.one

    d, k, rate = inv_n, mp.mpf(2), mp.mpf(3)
    v = rate/d
    # Exact lower-cut incomplete-gamma normalizer in the centered log kernel.
    log_z = v - mp.log(d) - k*mp.log(v) + mp.log(mp.gammainc(k, v, mp.inf))
    sample = (mp.zero, half)
    height = mp.exp(mp.fsum(k*d*y-v*mp.expm1(d*y) for y in sample)-2*log_z)
    yield "gamma-one-sided-negative-power", height/(rate**2*mp.exp(-rate*half)), mp.one

    # The held superunit upper shape has full trace along a moving upper wall.
    q = mp.mpf(2)
    wall = mp.sqrt(inv_n)
    cutoff = lambda y: (mp.exp(y)*(-mp.expm1(n*(y-wall)))**(q-1)
                        if y < wall else mp.zero)
    yield "genbeta-moving-wall", (
        likelihood(cutoff, (mp.mpf(-1), mp.zero), near_wall((mp.mpf(-2), mp.zero, wall, mp.one), wall, n)) /
        wall_limit), mp.one

    z_window = (mp.mpf(-2), mp.mpf(-1), mp.zero)
    sample = (mp.mpf(-1), -half)
    rho, q = half, mp.mpf(2)
    kernel = lambda z: mp.exp(rho*z)*(-mp.expm1(inv_n*z)/inv_n)**(q-1)
    distance = lambda z: mp.exp(rho*z)*(-z)**(q-1)
    yield "genbeta-log-distance", likelihood(kernel, sample, z_window)/likelihood(
        distance, sample, z_window), mp.one


    # Fixed p=2, q=d=1, sample (1,2) with lower cut 1 and m-h=S.
    large_excess = n+1
    density = lambda v: 2*v/(1+v)**3
    survivor = mp.mpf(3)/4
    yield "shifted-gb2-critical-contact", (
        n**2*density(mp.one)*density(mp.mpf(large_excess))/survivor**2), mp.mpf(8)/9


def evaluate(precision):
    with mp.workdps(precision):
        return {(name, n): (actual, expected)
                for n in GRID for name, actual, expected in checks(n)}


low = evaluate(60)
high = evaluate(90)
with mp.workdps(90):
    differences = {key: abs(low[key][0]-high[key][0]) for key in high}
    worst = max(differences, key=differences.get)
    assert differences[worst] < mp.mpf("1e-54"), (worst, differences[worst])
    for name in {key[0] for key in high}:
        errors = [abs(high[name, n][0]-high[name, n][1]) for n in GRID]
        assert errors[-1] < errors[0], (name, errors)
        assert errors[-1] < mp.mpf("0.08"), (name, errors)
    with (DATA / "limit-checks.csv").open("w", newline="") as target:
        writer = csv.writer(target, lineterminator="\n")
        writer.writerow(("example", "grid_n", "likelihood_ratio", "limiting_ratio",
                         "absolute_error", "cross_precision_difference"))
        for (name, n), (actual, expected) in sorted(high.items()):
            writer.writerow((name, n, mp.nstr(actual, 18), mp.nstr(expected, 18),
                             mp.nstr(abs(actual-expected), 8),
                             mp.nstr(abs(actual-low[name, n][0]), 8)))
    print(f"{len(high)} likelihood ratios agree at 60 and 90 digits within 1e-54.")
