"""Replay the held-scale sign certificate and archived coefficient checks."""
import gzip
import json
import math
from pathlib import Path

import mpmath as mp


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
EVALUATIONS = HERE.parent
DISTRIBUTIONS = ROOT / "univariateML-cpp/include/cppunivariate/distributions"
mp.mp.dps = 80
mp.iv.dps = 50


def average(values):
    values = list(values)
    return mp.fsum(values) / len(values)


def psi_bounds(k):
    """Digamma recurrence and signed first omitted Stirling term."""
    h = k + 64
    return (mp.iv.log(h) - 1 / (2 * h) - 1 / (12 * h**2)
            + mp.iv.mpf([0, 1]) / (120 * h**4)
            - sum(1 / (k + j) for j in range(64)))


def held_scale_signs():
    signs = []
    for token, expected in [("0.001", 1), ("0.01", -1), ("1", 1), ("20", -1)]:
        q = mp.mpf(token)
        target = -1001 * q / 2
        k = mp.exp(mp.findroot(lambda u: mp.digamma(mp.exp(u)) - target,
                               mp.log(1 / max(1, -target))))
        # Choose rational brackets independently of the interval verification.
        denom = 10**9
        center = int(mp.floor(k * denom))
        lo, hi = [mp.iv.mpf(j) / denom for j in (center - 2, center + 3)]
        q = mp.iv.mpf(token)
        target = -1001 * q / 2
        assert psi_bounds(lo).b < target.a and target.b < psi_bounds(hi).a
        shape = mp.iv.mpf([lo.a, hi.b])
        score = (shape * psi_bounds(1 + shape)
                 + q * (mp.iv.exp(-q) + 1000 * mp.iv.exp(-1000 * q)) / 2)
        assert (score.a > 0 if expected > 0 else score.b < 0)
        signs.append({"q": token, "shape_bracket_numerators": [center - 2, center + 3],
                      "shape_bracket_denominator": denom,
                      "score": str(score)})
    return signs


def standardized(logs):
    mean = average(logs)
    sd = mp.sqrt(average((y - mean)**2 for y in logs))
    return [(y - mean) / sd for y in logs]


def free_profile(z, t):
    weights = [mp.exp(t * (v - max(z))) for v in z]
    total = mp.fsum(weights)
    K = t * max(z) + mp.log(total / len(z))
    K1 = mp.fsum(v * w for v, w in zip(z, weights)) / total
    # Monotone shape equation, Newton in log shape.
    u = mp.log(1 / K)
    for _ in range(50):
        k = mp.exp(u)
        step = (u - mp.digamma(k) - K) / (1 - k * mp.polygamma(1, k))
        u -= step
        if abs(step) < 128 * mp.eps:
            break
    k = mp.exp(u)
    assert abs(u - mp.digamma(k) - K) < mp.sqrt(mp.eps) * K
    value = mp.log(t) + k * (u - 1 - K) - mp.loggamma(k)
    return value, 1 / t - k * K1


def path(r):
    return standardized([-mp.mpf(3), mp.mpf(2)] + [-r, r] * 5)


def finite_power_crossing():
    # Numerical localization of a branch crossing. The report's compact
    # maximum construction independently proves an exact global contact.
    with mp.workdps(45):
        def equations(r, t):
            z = path(r)
            value, score = free_profile(z, t)
            return value + 1 + mp.log(max(z)), score

        for r in map(mp.mpf, ("0.8", "1", "1.2", "1.4", "1.6", "1.8", "2")):
            z = path(r)
            t0, s0 = None, None
            for j in range(45):
                t = mp.exp(mp.mpf(j - 22) / 7)
                _, score = free_profile(z, t)
                if s0 is not None and s0 > 0 > score:
                    try:
                        r1, t1 = mp.findroot(equations, (r, (t + t0) / 2),
                                             tol=mp.mpf("1e-38"), maxsteps=30)
                    except (ValueError, ZeroDivisionError, TypeError, AssertionError):
                        break
                    if 0 < r1 < 2 and 0 < t1 < 20:
                        z1 = path(r1)
                        face = -1 - mp.log(max(z1))
                        normal = -(1 + mp.log(2 * mp.pi)) / 2
                        assert face > normal
                        return {"r": str(r1), "standardized_power": str(t1),
                                "power_minus_normal": str(face - normal)}
                    break
                t0, s0 = t, score
    raise AssertionError("branch crossing failed to localize")


def root_error(sample, inverse, coefficient, holds, requested):
    sign = -1 if inverse else 1
    y = [sign * mp.log(mp.mpf(x)) for x in sample]
    public = list(map(mp.mpf, coefficient))
    base = [mp.log(public[0]), mp.log(public[1]), sign * mp.log(public[2])]
    for i, held in requested.items():
        base[i] = (sign if i == 2 else 1) * mp.log(held)
    free = [j for j in range(3) if not holds & (1 << j)]
    if not free:
        return mp.mpf(0)

    def scores(*args):
        point = base[:]
        for i, v in zip(free, args):
            point[i] = v
        logk, logp, logs = point
        k, p = mp.exp(logk), mp.exp(logp)
        z = [p * (v - logs) for v in y]
        ex = [mp.exp(v) for v in z]
        score = [k * (average(z) - mp.digamma(k)),
                 1 + k * average(z) - average(v * w for v, w in zip(z, ex)),
                 p * (average(ex) - k)]
        return [score[i] for i in free]

    with mp.workdps(65):
        values = mp.findroot(scores, tuple(base[i] for i in free), tol=mp.mpf("1e-57"))
        assert max(map(abs, scores(*values))) < mp.mpf("1e-53")
        return max(abs(mp.expm1(base[i] - v if i != 2 else sign * (base[i] - v)))
                   for i, v in zip(free, values))


def archive_checks():
    results = {}
    families = ("trgamma", "invtrgamma")
    parameters = ("shape1", "shape2", "scale")
    for mode in ("native", "double"):
        name = "results.json" if mode == "native" else "results-no-long-double.json"
        archive = json.loads((EVALUATIONS / "2026-11-07-trgamma-seam-data" / name).read_text())
        errors = {}
        held_errors = {}
        goldens = {}
        for family in families:
            rows = {r["case"]: r for r in archive["checks"]}
            golden = json.loads((DISTRIBUTIONS / family / "golden.json").read_text())
            goldens[family] = golden
            for mask in range(8):
                case = f"{family} held {mask}" if mask else f"{family} golden"
                row = rows[case]
                assert row["status"] == 0
                requested = {i: golden["estimates"][name]
                             for i, name in enumerate(parameters) if mask & (1 << i)}
                held_errors[case] = max((abs(row["coef"][i] / value - 1)
                                         for i, value in requested.items()), default=0)
                error = root_error(golden["data"], family == "invtrgamma",
                                   row["coef"], mask, requested)
                assert error < mp.mpf("1e-10"), (case, error)
                errors[case] = float(error)
        snapshots = EVALUATIONS / "2026-11-15-fit-snapshot-data" / f"{mode}.jsonl.gz"
        snapshot_errors = {}
        with gzip.open(snapshots, "rt") as archive_file:
            for line in archive_file:
                row = json.loads(line)
                family = row["family"]
                if family in families and row["source"] == "golden":
                    assert row["result"]["kind"] == "ok" and row["mask"] == 0
                    public = row["result"]["estimates"]
                    coefficient = [float.fromhex(public[name]) for name in parameters]
                    error = root_error(goldens[family]["data"], family == "invtrgamma",
                                       coefficient, 0, {})
                    assert error < mp.mpf("1e-10"), (family, mode, error)
                    snapshot_errors[family] = float(error)
        assert set(snapshot_errors) == set(families)
        results[mode] = {"nov_07_free_coefficient_errors": errors,
                         "nov_07_max_held_coefficient_error": max(held_errors.values()),
                         "nov_15_golden_errors": snapshot_errors}
    return results


def tiny_held_power():
    # Source-level replay of the two-held-shapes branch, with correctly
    # rounded transcendentals and an exactly accumulated two-element sum.
    p = 2.0**-40
    ymax = float(mp.log(2))
    arg = -p * ymax
    term = float(mp.exp(arg))
    mean = float((1 + mp.mpf(term)) / 2)
    centered = float(mp.log(mean))
    log_scale = ymax + centered / p
    represented = float(mp.exp(log_scale))
    exact = ((1 + mp.power(2, p)) / 2)**(1 / mp.mpf(p))
    relative = abs(represented / exact - 1)
    assert relative > mp.mpf("1e-8")

    # Replay the represented-density acceptance inequality. Accumulation
    # differences of a few ulps are immaterial to the gap in this guard.
    eps = math.ulp(1.0)
    K = p * (ymax - ymax / 2) + centered
    profile_ll = 2 * (math.log(p) - 1 - K) - ymax
    public_log = math.log(represented)
    base = math.log(p) - 1
    z = [p * (v - public_log) for v in (0.0, ymax)]
    penalty = [float(mp.expm1(v) - v) for v in z]
    terms = [base - v - d for v, d in zip((0.0, ymax), penalty)]
    uncertainty = 0.0
    for v, r in zip((0.0, ymax), z):
        dz = 8 * eps * (1 + p * (abs(v) + abs(public_log)))
        uncertainty += abs(math.expm1(r)) * dz + 0.5 * math.exp(r + dz) * dz**2
    pad = (64 * eps * (2 + 2 * abs(base) + ymax + sum(penalty))
           + uncertainty + 64 * eps)
    guard = abs(math.fsum(terms) - profile_ll) + pad
    threshold = 1e-10 * (1 + abs(profile_ll))
    assert guard < threshold / 100
    return {"represented_scale": represented, "exact_scale": str(exact),
            "relative_error": float(relative), "likelihood_guard": guard,
            "guard_threshold": threshold}


def main():
    results = {"held_scale_signs": held_scale_signs(),
               "crossing_branch": finite_power_crossing(),
               "archived_relative_errors": archive_checks(),
               "tiny_held_power": tiny_held_power()}
    (HERE / "results.json").write_text(json.dumps(results, indent=2) + "\n")
    print("Held-scale interval signs, crossing replay, archived coefficients and rounding replay passed.")


if __name__ == "__main__":
    main()
