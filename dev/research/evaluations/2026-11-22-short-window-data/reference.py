"""Extend the mod-gaps log-gamma oracle to all five discrete window routes."""
import csv
from pathlib import Path
import sys

import mpmath as mp

mp.mp.dps = 100

CASES = [
    ("nbinom-left", "nbinom", (100000000, .5), 2, 12),
    ("nbinom-zero", "nbinom", (100000000, .5), 0, 5),
    ("geom-remote", "geom", (.3,), 10**9, 10**9 + 10),
    ("geom-interior-guard", "geom", (1e-5,), 100000, 100010),
    ("geom-at-cap", "geom", (.3,), 10**9, 10**9 + 255),
    ("pois-tiny", "pois", (1000,), 2, 12),
    ("binom-tiny", "binom", (3000, .5), 2, 12),
    ("zeta-finite", "zeta", (-30,), 1, 12),
    ("nbinom-too-wide", "nbinom", (2, .3), 10**9, 10**9 + 256),
]


def reference(family, params, lo, hi):
    params = tuple(map(mp.mpf, params))
    def log_pmf(k):
        if family == "nbinom":
            size, prob = params
            return (mp.loggamma(size + k) - mp.loggamma(size) - mp.loggamma(k + 1)
                    + size * mp.log(prob) + k * mp.log1p(-prob))
        if family == "geom":
            prob, = params
            return mp.log(prob) + k * mp.log1p(-prob)
        if family == "pois":
            rate, = params
            return -rate + k * mp.log(rate) - mp.loggamma(k + 1)
        if family == "binom":
            size, prob = params
            return (mp.log(mp.binomial(size, k)) + k * mp.log(prob)
                    + (size - k) * mp.log1p(-prob))
        return -params[0] * mp.log(k)  # Finite zeta also admits improper parents.

    logs = [log_pmf(k) for k in range(lo, hi + 1)]
    weights = [mp.exp(v) for v in logs]
    mass = mp.fsum(weights)

    def log_probability(part, rest):
        return mp.log(part / mass) if part < rest else mp.log1p(-rest / mass)

    lower = [log_probability(mp.fsum(weights[:i+1]), mp.fsum(weights[i+1:]))
             for i in range(len(weights))]
    upper = [log_probability(mp.fsum(weights[i+1:]), mp.fsum(weights[:i+1]))
             for i in range(len(weights))]
    density = [log_probability(w, mp.fsum(weights[:i] + weights[i+1:]))
               for i, w in enumerate(weights)]
    return mp.log(mass), density, lower, upper


def records():
    for name, family, params, lo, hi in CASES:
        log_mass, density, lower, upper = reference(family, params, lo, hi)
        prefix = (name, family, ":".join(map(str, params)), lo, hi)
        if name.endswith("too-wide"):
            yield (*prefix, "refuse", 1, 1, lo, float(log_mass))
            continue
        for log in (0, 1):
            for op, tail, values in (("d", 1, density), ("p", 1, lower), ("p", 0, upper)):
                for i in (range(len(values)) if len(values) < 256 else (0, 1, 127, 254, 255)):
                    v = values[i]
                    expected = "-Inf" if log and not mp.isfinite(v) else float(v if log else mp.exp(v))
                    yield (*prefix, op, tail, log, lo + i, expected)
            targets = ([-1000, -138.2, -27.6, -16.1, -.69, -.105, -1e-10] if log
                       else [1e-60, 1e-12, 1e-7, .5, .9, .9999999999])
            for tail, values in ((1, lower), (0, upper)):
                for p in targets:
                    target = mp.mpf(p) if log else mp.log(mp.mpf(p))
                    q = next(k for k, value in enumerate(values, lo)
                             if (value >= target if tail else value <= target))
                    yield (*prefix, "q", tail, log, p, q)


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[4]
    fixture = root / "bindings/r/tests/testthat/fixtures/short-window.tsv"
    with fixture.open("w") as stream:
        writer = csv.writer(stream, delimiter="\t", lineterminator="\n")
        writer.writerow("case family parameters lo hi op lower log input expected".split())
        writer.writerows(records())
    for name, family, params, lo, hi in CASES:
        mass, *_ = reference(family, params, lo, hi)
        print(name, mp.nstr(mass, 25))
