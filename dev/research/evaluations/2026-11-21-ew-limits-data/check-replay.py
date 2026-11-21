"""Check both face choices and the steep offset case with high precision."""
import argparse
import csv
import json
import sys
from collections import Counter, defaultdict
from pathlib import Path

import mpmath as mp

HERE = Path(__file__).resolve().parent
LIMITS = HERE.with_name("2026-11-15-expweibull-limits-data")
ARCHIVE = HERE.with_name("2026-11-12-ew-fit-data")
ROOT = HERE.parents[3]
sys.path.insert(0, str(ROOT / "dev/validation"))
sys.path.insert(0, str(LIMITS))
from core import faces, h
from strict_common import load_one, number, snapshot_module

mp.mp.dps = 70


def snapshot_check(path):
    rows = {key[2]: case["result"] for key, case in load_one(path).items()}
    expected = json.loads((LIMITS / "verdicts.json").read_text())
    outcomes = Counter()
    for name, record in expected.items():
        verdict = record["verdict"]
        kind, model = {
            "power face": ("limit", "power"),
            "inverse Weibull face": ("limit", "invweibull"),
            "finite maximum": ("ok", ""),
        }[verdict]
        result = rows[name]
        assert (result["kind"], result["model"]) == (kind, model), (name, result)
        assert not result["certified"]
        outcomes[verdict] += 1
        print(f"{name:14} {kind:5} {model:10} P={record['P']:.10g} I={record['I']:.10g}")
    print(dict(outcomes))
    return rows


def offset_reference(result):
    fs = snapshot_module()
    x = list(map(mp.mpf, fs.classes(*fs.read("expweibull"))["offset"]))
    anchor = mp.log(x[0])
    unit = mp.log(x[-1]) - anchor
    y = [(mp.log(v) - anchor)/unit for v in x]
    reported = {name: mp.mpf(number(text)) for name, text in result["estimates"].items()}
    coords = ((mp.log(reported["scale"]) - anchor)/unit,
              mp.log(reported["shape"]*unit), mp.log(reported["exponent"]))

    def value(mu, log_b, log_c):
        b, c = mp.exp(log_b), mp.exp(log_c)
        z = [b*(v-mu) for v in y]
        t = [mp.exp(v) for v in z]
        return (len(x)*(log_b+log_c-mp.log(unit)) - mp.fsum(map(mp.log, x)) +
                mp.fsum(v-w+(c-1)*h(w) for v, w in zip(z, t)))

    def score(*p):
        return tuple(mp.diff(value, p, tuple(int(i == j) for i in range(3)))
                     for j in range(3))

    root = mp.findroot(score, coords)
    mu, log_b, log_c = root
    ref = {"scale": mp.exp(anchor+unit*mu),
           "shape": mp.exp(log_b)/unit, "exponent": mp.exp(log_c)}
    error = max(abs(reported[name]/v-1) for name, v in ref.items())
    represented_ll = value(*coords)
    print("offset: reference coefficients", {k: mp.nstr(v, 22) for k, v in ref.items()})
    print("offset: max relative coefficient error", mp.nstr(error, 10))
    print("offset: optimum logLik", mp.nstr(value(*root), 20))
    print("offset: represented logLik", mp.nstr(represented_ll, 20))
    assert float(ref["scale"]) == float(reported["scale"])
    assert error < mp.mpf("1e-10")
    assert abs(represented_ll - number(result["logLik"])) < mp.mpf("1e-10")


def archive_check(mode):
    samples = defaultdict(list)
    with (ARCHIVE / "archive-samples.csv").open() as stream:
        for row in csv.DictReader(stream):
            samples[row["sample"]].append(mp.mpf(float(row["x"])))
    print("archive flagged free fits and independent face values")
    with (HERE / f"archive-{mode}.csv").open() as stream:
        results = {(row["sample"], row["mask"]): row for row in csv.DictReader(stream)}
    for (name, mask), row in results.items():
        if mask != "0" or row["kind"] != "limit":
            continue
        p, inverse, *_ = faces([mp.log(v) for v in samples[name]])
        print(name, "P", mp.nstr(p, 13), "I", mp.nstr(inverse, 13))
        assert row["model"] == ("power" if p > inverse else "invweibull")
        if row["model"] == "power":
            assert abs(float(row["logLik"]) - p) <= mp.mpf("1e-10")*max(1, abs(p))

    print("held shape 1.5: feasible witness gap, profiled stationary log exponent and likelihood gap")
    for name, x in samples.items():
        if name != "near_constant" and not name.startswith("ties_"):
            continue
        held = results[name, "2"]
        assert (held["status"], held["kind"], held["reason"]) == ("9", "failure", "evaluation")
        t = [1424*(v/min(x))**mp.mpf("1.5") for v in x]
        log_c = 1424 - mp.log(mp.fsum(mp.exp(1424-v) for v in t)/len(t))
        value = mp.fsum(mp.log(v)+log_c-v+(mp.exp(log_c)-1)*h(v) for v in t)/len(t)
        witness_gap = value - (mp.log(713)-1)
        assert witness_gap > 0

        b = mp.mpf("1.5")
        n = len(x)
        relative = [(v/x[n//2])**b for v in x]

        def evaluate(log_tau):
            t = [mp.exp(log_tau)*q for q in relative]
            sum_h = mp.fsum(map(h, t))
            c = -n/sum_h
            score = n - mp.fsum(t) + (c-1)*mp.fsum(v/mp.expm1(v) for v in t)
            mean_ll = mp.log(c) - 1 + (mp.fsum(mp.log(v)-v for v in t)-sum_h)/n
            return score, mp.log(c), mean_ll

        lo, hi = mp.mpf(0), mp.mpf(30)
        assert evaluate(lo)[0] > 0 > evaluate(hi)[0]
        for _ in range(200):
            mid = (lo+hi)/2
            if evaluate(mid)[0] > 0:
                lo = mid
            else:
                hi = mid
        _, log_c, mean_ll = evaluate((lo+hi)/2)
        print(name, "witness gap", mp.nstr(witness_gap, 14), "log_c", mp.nstr(log_c, 14),
              "stationary comparison gap", mp.nstr(mean_ll-(mp.log(713)-1), 14))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("native", "double"))
    parser.add_argument("snapshot", type=Path)
    args = parser.parse_args()
    offset_reference(snapshot_check(args.snapshot)["offset"])
    archive_check(args.mode)
