"""Reproduce the numerical examples and exact rational checks for TASK-475."""
import json
from fractions import Fraction as Q

import mpmath as mp

mp.mp.dps = 80


def grouped(k, r, cell):
    if r == 0:
        return ((cell + 1)**k - cell**k) / k
    return mp.gammainc(k, r * cell, r * (cell + 1)) / r**k


def gamma_odds(k, r, cell):
    return grouped(k, r, cell + 1) / grouped(k, r, cell)


def record(value):
    return mp.nstr(value, 28)


def checks():
    output = {"decimal_precision": mp.mp.dps}
    # Tight [2, 3] with two occurrences of 2 and one of 3: target odds 1/2.
    roots = []
    for k in (1, 2, 8, 32):
        lo, hi = mp.mpf(0), mp.mpf(64)
        assert gamma_odds(k, lo, 2) > mp.mpf("0.5") > gamma_odds(k, hi, 2)
        for _ in range(230):
            mid = (lo + hi) / 2
            if gamma_odds(k, mid, 2) > mp.mpf("0.5"):
                lo = mid
            else:
                hi = mid
        r = (lo + hi) / 2
        odds = gamma_odds(k, r, 2)
        assert abs(odds - mp.mpf("0.5")) < mp.mpf("1e-65")
        roots.append({"shape": k, "rate": record(r), "likelihood": record(odds / (1 + odds)**3)})
    output["dgamma_two_cells"] = roots

    # Same divergent GPD path, with an infinite or a finite upper cut.
    paths = []
    for j in (4, 8, 16, 24):
        s = mp.mpf(10)**(-j)
        h = s**mp.mpf("0.25")
        at_one = (h / s)**2 * mp.exp(-(h + 1) * mp.log1p(1 / s))
        retained = -mp.expm1(-h * mp.log1p(1 / s))
        paths.append(
            {"j": j, "ray_height": record(at_one), "bounded_height": record(at_one / retained**2),
             "bounded_mass": record(retained),
             "bounded_cdf_at_half": record(-mp.expm1(-h * mp.log1p(mp.mpf("0.5") / s)) / retained)}
        )
    for quantity in ("ray_height", "bounded_height", "bounded_cdf_at_half"):
        assert all(mp.mpf(a[quantity]) < mp.mpf(b[quantity]) for a, b in zip(paths, paths[1:]))
    output["gpd_escape"] = paths

    exact = []
    for j in (1, 2, 4, 8):
        eps = Q(1, 10**j)
        # Held triangular parent [0,1], mode 1, window [1-eps,2].
        mass = 1 - (1 - eps)**2
        height = 2 / mass
        assert height * eps == 2 / (2 - eps)
        # Held uniform GPD [0,1] under the same window.
        assert 1 / eps == 10**j
        exact.append({"j": j, "triangular_height": str(height), "uniform_gpd_height": str(1 / eps)})
    output["constant_endpoint_contacts"] = exact
    return output


if __name__ == "__main__":
    from pathlib import Path

    path = Path(__file__).with_name("checks.json")
    path.write_text(json.dumps(checks(), indent=2) + "\n")
    print("80-digit grouped-odds and GPD grids, and rational contact identities passed")
