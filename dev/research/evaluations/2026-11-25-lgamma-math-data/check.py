#!/usr/bin/env python3
"""Check archived public fits and replay the lossy part of the current formula."""
import gzip
import json
import math
from pathlib import Path

import mpmath as mp


EVALUATIONS = Path(__file__).resolve().parents[1]
AUDIT = EVALUATIONS / "2026-11-07-fit-audit-data"
SNAPSHOTS = EVALUATIONS / "2026-11-15-fit-snapshot-data"
TOL = 1e-10


def shape_at_gap(s):
    def score(t):
        return t - mp.digamma(mp.exp(t)) - s

    lo, hi = mp.log(mp.mpf("0.25") / s), mp.log(2 / s)
    assert score(lo) > 0 > score(hi)
    return mp.exp(mp.findroot(
        score, (lo, hi), solver="anderson", tol=mp.eps * 100,
        maxsteps=200, verify=False,
    ))


def stats(xs):
    y = [mp.log(mp.mpf(x)) for x in xs]
    m, b = mp.fsum(y) / len(y), mp.fsum(map(mp.log, y)) / len(y)
    return m, b, mp.log(m) - b


def loglik(n, m, b, k, rate):
    return n * (k * mp.log(rate) - mp.loggamma(k) + (k - 1) * b - (rate + 1) * m)


def reference(xs, held, dps):
    with mp.workdps(dps):
        m, b, s = stats(xs)
        k = held.get("shapelog")
        rate = held.get("ratelog")
        if k is None and rate is None:
            k = shape_at_gap(s)
            assert abs((mp.log(k) - mp.digamma(k)) / s - 1) < mp.mpf("1e-90")
        elif k is None:
            target = mp.log(rate) + b
            k = mp.exp(mp.findroot(
                lambda t: mp.digamma(mp.exp(t)) - target,
                mp.log(1 + mp.exp(target)), tol=mp.eps * 100,
            ))
        if rate is None:
            rate = k / m
        return k, rate, loglik(len(xs), m, b, k, rate)


def error(a, b, floor=0):
    return abs(a - b) / max(floor, abs(b))


def check_public(label, xs, held, returned, reported, certified):
    k, rate, peak = reference(xs, held, 360)
    low = reference(xs, held, 180)
    assert max(error(a, b, 1) for a, b in zip(low, (k, rate, peak))) < mp.mpf("1e-90")
    m, b, _ = stats(xs)
    actual = loglik(len(xs), m, b, *map(mp.mpf, returned))
    ke, re = [error(mp.mpf(a), b) for a, b in zip(returned, (k, rate))]
    ll_error = error(mp.mpf(reported), actual, 1)
    assert max(ke, re, ll_error) < TOL
    assert all(returned[i] == held[name] for i, name in enumerate(
        ("shapelog", "ratelog")) if name in held)
    print(label, "certified", certified, "shape/rate/likelihood errors",
          *(mp.nstr(x, 6) for x in (ke, re, ll_error)),
          "peak minus returned", mp.nstr(peak - actual, 9))


def archived():
    ids = {"004428", "004431", "004435", "004444", "004460", "004476"}
    with gzip.open(AUDIT / "cases.json.gz", "rt") as f:
        cases = {f"{row['index']:06d}": row for row in json.load(f)
                 if f"{row['index']:06d}" in ids}
    for mode in ("native", "double"):
        with gzip.open(SNAPSHOTS / f"{mode}.jsonl.gz", "rt") as f:
            found = {}
            for line in f:
                row = json.loads(line)
                if row["case"] in ids and row["family"] == "lgamma":
                    found[row["case"]] = row
        assert set(found) == ids
        for key, row in sorted(found.items()):
            case, result = cases[key], row["result"]
            assert result["kind"] == "ok"
            coefs = [float.fromhex(result["estimates"][name]) for name in ("shapelog", "ratelog")]
            check_public(f"{mode}/{key}", case["data"], case["held"], coefs,
                         float.fromhex(result["logLik"]), result["certified"])

    # Source-native historical public restoration record from TASK-101.7.5.
    import csv
    path = EVALUATIONS / "2026-11-10-audit-transform-data/restoration-native.csv"
    with path.open() as f:
        row, = [row for row in csv.DictReader(f) if row["family"] == "lgamma"]
    xs = [2 * (1 + j * 2**-52) for j in (0, 1, 2, 4, 7)]
    check_public("restoration/log-asymmetric", xs, {},
                 (float(row["power"]), float(row["scale"])),
                 float(row["logLik"]), bool(int(row["certified"])))


def rounded_gap(anchor, copies):
    # One outlier and identical anchors. The compensated sum of delta is
    # exactly d. Pair division/addition rounded to binary64 is used for M.
    x = math.nextafter(1.0, math.inf)
    primitive_ratio = x / anchor
    log_ratio = (math.log(primitive_ratio) if primitive_ratio >= 2**-1022
                 else math.log(x) - math.log(anchor))
    d = log_ratio / math.log(anchor)
    n = copies + 1
    md = d / n
    mean_ratio = float(1 + mp.mpf(d) / n)
    assert abs(md) < .1
    u = (d - md) / mean_ratio
    assert abs(u) >= .1
    regular = mp.mpf(-md / mean_ratio)
    regular_phi = regular - mp.log1p(regular)
    outlier_phi = u - math.log(1 + u) if u > -1 else math.inf
    # Remedy using an independent log ratio for the far-outlier penalty.
    log_z = math.log(math.log(x)) - math.log(math.log(anchor)) - math.log(mean_ratio)
    repaired = (mp.mpf(u - log_z) + copies * regular_phi) / n
    return d, u, (outlier_phi + copies * regular_phi) / n, repaired


def outlier_grid():
    wrong = infinite = 0
    repair_error = mp.mpf(0)
    for anchor in (4.0, 16.0, 2.0**16, 2.0**128, 2.0**512, 2.0**1023):
        for copies in (10, 15, 31, 127):
            xs = [math.nextafter(1.0, math.inf)] + [anchor] * copies
            _, _, s = stats(xs)
            d, u, observed, repaired = rounded_gap(anchor, copies)
            repair_error = max(repair_error, error(repaired, s))
            if mp.isinf(observed):
                infinite += 1
                if anchor == 16 and copies == 10:
                    print(anchor.hex(), copies, "d/u", d.hex(), u.hex(), "nonfinite gap")
                continue
            k, candidate = shape_at_gap(s), shape_at_gap(observed)
            relative = error(candidate, k)
            wrong += relative > TOL
            if anchor == 4 and copies == 10:
                m, b, _ = stats(xs)
                actual = loglik(len(xs), m, b, candidate, candidate/m)
                max_ll = loglik(len(xs), m, b, k, k/m)
                ideal_used = actual + len(xs) * candidate * (s - observed)
                print("4 x 10 example", "gap exact/replayed", mp.nstr(s, 18), mp.nstr(observed, 18),
                      "shape exact/replayed", mp.nstr(k, 18), mp.nstr(candidate, 18),
                      "rate exact/replayed", mp.nstr(k/m, 18), mp.nstr(candidate/m, 18),
                      "relative shape/rate error", mp.nstr(relative, 10),
                      "true peak loss", mp.nstr(max_ll-actual, 10),
                      "source ideal minus returned", mp.nstr(ideal_used-actual, 10))
                assert relative > mp.mpf(".001") and ideal_used < actual
    assert (wrong, infinite) == (4, 20)
    assert repair_error < mp.mpf("1e-13")
    print("largest repaired gap relative error", mp.nstr(repair_error, 8))
    print("outlier grid:", wrong, "inaccurate finite roots and", infinite, "nonfinite gaps")


def representations():
    _, rate, _ = reference([2.0], {"shapelog": 1.0}, 360)
    nearest = float(rate)
    floor = error(mp.mpf(nearest), rate)
    assert floor > mp.mpf("1e-20")
    print("held shape 1, x=2, nearest rate", nearest.hex(), "relative floor", mp.nstr(floor, 16))
    tiny = math.ulp(0.0)
    _, rate, _ = reference([4.0], {"shapelog": tiny}, 360)
    assert float(rate) == tiny and error(mp.mpf(tiny), rate) > .38
    print("held shape min-subnormal, x=4, relative floor", mp.nstr(error(mp.mpf(tiny), rate), 16))
    _, _, ll = reference([4.0], {"shapelog": tiny, "ratelog": 1.0}, 360)
    logz = math.log(math.log(4.0)) - math.log(tiny)
    assert logz > math.log(float.fromhex("0x1.fffffffffffffp+1023")) and mp.isfinite(ll)
    print("fully held min-subnormal/1, x=4, exact logLik", mp.nstr(ll, 18),
          "and unscaled expm1 argument", logz)


if __name__ == "__main__":
    mp.mp.dps = 360
    archived()
    outlier_grid()
    representations()
