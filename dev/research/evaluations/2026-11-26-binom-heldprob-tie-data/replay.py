"""Run from the worktree root at 2d89fd4a or with the proposed fix applied."""
import importlib.util
import itertools
import json
import math
from fractions import Fraction
from pathlib import Path
import sys

import mpmath as mp

validation = Path("dev/validation").resolve()
sys.path.insert(0, str(validation))
spec = importlib.util.spec_from_file_location("snapshot", validation / "fit-snapshot.py")
snapshot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(snapshot)
mp.mp.dps = 100


def mass(data, size, prob):
    return math.prod(math.comb(size, x) * prob**x * (1 - prob)**(size - x) for x in data)


def digits(value):
    return mp.nstr(mp.log(mp.mpf(value.numerator) / value.denominator), 85)


work = Path("build/scratch/binom-tie-probe").resolve()
work.mkdir(parents=True, exist_ok=True)
probe = snapshot.Probe(snapshot.build("binom", work, "--double" in sys.argv, snapshot.boost_include()))


def fit(data, held):
    mask = int("size" in held) + 2 * int("prob" in held)
    case = dict(data=data, mask=mask, held=held)
    return probe.ask(snapshot.line(case, ["size", "prob"]), 30)


fixtures = []
for data, p, first in [
    ((0, 6), .5, 7), ((0, 0, 7), .5, 7), ((0, 7), .25, 15),
    ((1, 1, 1, 1), .5, 1),
    ((0, 6), math.nextafter(.5, 0.), 7), ((0, 6), math.nextafter(.5, 1.), 7),
    ((0, 1, 6), 1 / 3, 8),
]:
    q = Fraction(p)
    sizes = range(max(data), max(first + 2, math.ceil(max(data) / q) + 1))
    values = {size: mass(data, size, q) for size in sizes}
    peak = max(values.values())
    fixtures.append(dict(data=data, prob=p.hex(), winners=[k for k in values if values[k] == peak],
                         logLik={k: digits(values[k]) for k in (first, first + 1)},
                         increment=digits(values[first + 1] / values[first]),
                         fit=fit(data, {"prob": p})))

grid = dict.fromkeys(("all_free_finite", "all_free_poisson", "all_free_tie",
                      "held_size", "held_pair", "held_prob_endpoint"), 0)
findings = []
for length in (2, 3):
    for data in itertools.combinations_with_replacement(range(5, 9), length - 1):
        for minimum in (0, 1, 2):
            x = (minimum,) + data
            n, total, size = len(x), sum(x), max(x)
            for holds in ({"size": size}, {"size": size, "prob": .5}):
                row = fit(x, holds)
                key = "held_pair" if "prob" in holds else "held_size"
                grid[key] += 1
                if row.get("kind") != "ok" or row.get("unique") is not True or (
                        float.fromhex(row["estimates"]["prob"]) != holds.get("prob", float(Fraction(total, n*size)))
                        and abs(float.fromhex(row["estimates"]["prob"]) - total/(n*size)) > 2**-52):
                    findings.append(dict(data=x, held=holds, fit=row))
            row = fit(x, {})
            if n*total + total**2 <= n*sum(v*v for v in x):
                grid["all_free_poisson"] += 1
                if row.get("kind") != "limit":
                    findings.append(dict(data=x, held={}, fit=row))
                continue
            grid["all_free_finite"] += 1
            previous = mass(x, size, Fraction(total, n*size))
            while True:
                rival = mass(x, size + 1, Fraction(total, n*(size + 1)))
                if rival <= previous:
                    break
                size, previous = size + 1, rival
            grid["all_free_tie"] += rival == previous
            if row.get("kind") != "ok" or float.fromhex(row["estimates"]["size"]) != size or rival == previous:
                findings.append(dict(data=x, expected=size, tie=rival == previous, fit=row))

endpoints = []
for x, p in [((0, 6), 0.), ((0, 6), 1.), ((0, 0), 0.), ((6, 6), 1.), ((0, 0), .5)]:
    grid["held_prob_endpoint"] += 1
    endpoints.append(dict(data=x, prob=p, fit=fit(x, {"prob": p})))
probe.close()
print(json.dumps(dict(precision=mp.mp.dps, fixtures=fixtures, grid=grid, findings=findings,
                      endpoints=endpoints), indent=2))
