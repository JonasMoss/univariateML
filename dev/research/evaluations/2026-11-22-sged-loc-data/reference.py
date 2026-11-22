"""Independent held-SD SGED reference, using exact binary64 inputs.

Solve the monotone score by bisection in physical coordinates at 560 decimal
digits. Stop only when the mode enclosure gives 80 significant digits in
both the nearest-knot displacement and public mean.
"""
import gzip
import json
from pathlib import Path
import sys

import mpmath as mp


CASES = (
    ("original", (0., 1e100), 1., 1.01, 100.),
    ("unit-gap", (0., 1.), 1., 1.5, 3.),
    ("wide-gap", (0., 1e300), 1., 1.01, 100.),
    ("negative-point", (-1e100, 0.), 1., 1.01, .01),
    ("opposite-skew", (-2., 1.), 2., 2.5, .7),
    ("unequal-counts", (-3., -3., 2.), 2., 2.5, 1.5),
    ("three-knots-near-endpoint", (0., 1e100, 2e100), 1., 1.01, 100.),
    ("three-knots-interior", (-3., -1., 2.), 2., 2.5, 1.5),
    ("symmetric-stationary-knot", (-2., 0., 2.), 2., 2., 1.),
)


def solve(name, data, sd, nu, xi):
    x = tuple(map(mp.mpf, data))
    sigma, p, skew = map(mp.mpf, (sd, nu, xi))
    shape = 1 / p
    a, b, c = (mp.gamma(k * shape) for k in (1, 2, 3))
    radius = mp.sqrt(a / c)
    moment = radius * b / a
    variance = (1 - moment**2) * (skew**2 + skew**-2) + 2 * moment**2 - 1
    scale = mp.sqrt(variance)
    shift = sigma * moment * (skew - 1 / skew) / scale

    def score(mode):
        return mp.fsum(mp.sign(mode - v) * abs(mode - v)**(p - 1)
                       * skew**(mp.sign(mode - v) * p) for v in x)

    lo, hi = min(x), max(x)
    for _ in range(4096):
        mid = (lo + hi) / 2
        s = score(mid)
        if s == 0:
            lo = hi = mid
            break
        if s < 0:
            lo = mid
        else:
            hi = mid
        nearest = min(x, key=lambda v: abs(v - mid))
        width = hi - lo
        if width <= mp.mpf('1e-80') * min(abs(mid - nearest), abs(mid + shift)):
            assert score(lo) <= 0 <= score(hi)
            break
    else:
        raise RuntimeError(f"mode enclosure failed for {name}")

    mode = (lo + hi) / 2
    nearest = min(x, key=lambda v: abs(v - mode))
    cost = mp.fsum((abs(v - mode) * scale / (sigma * radius)
                    * skew**mp.sign(mode - v))**p for v in x)
    ll = len(x) * mp.log(scale / ((skew + 1 / skew) * sigma * radius
                                 * mp.gamma(1 + shape))) - cost
    return dict(name=name, data=data, sd=sd, nu=nu, xi=xi,
                anchor=mp.nstr(nearest, 72), offset=mp.nstr(mode - nearest, 72),
                mean=mp.nstr(mode + shift, 72), logLik=mp.nstr(ll, 72),
                bracket_width=mp.nstr(hi - lo, 6))


def archive_cases(indices):
    archive = (Path(__file__).resolve().parents[1]
               / "2026-11-07-fit-audit-data" / "cases.json.gz")
    with gzip.open(archive, "rt") as stream:
        for row in json.load(stream):
            if row["family"] != "sged" or row["index"] not in indices:
                continue
            held = row["held"]
            yield (f"{row['index']:06d} {row['sample']}", row["data"],
                   held["sd"], held["nu"], held["xi"])


if __name__ == "__main__":
    cases = list(archive_cases({int(i) for i in sys.argv[1:]})) if sys.argv[1:] else CASES
    with mp.workdps(560):
        print(json.dumps([solve(*case) for case in cases], indent=2))
