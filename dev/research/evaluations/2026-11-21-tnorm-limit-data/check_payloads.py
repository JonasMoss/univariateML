"""Compare exponential limits in the two route snapshots to 60-digit MLEs."""
import gzip
import json
from pathlib import Path
import sys

import mpmath as mp

mp.mp.dps = 60
ROOT = Path(__file__).resolve().parents[4]
ARCHIVE = ROOT / "dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz"


def optimum(x, model, coefficients):
    n = len(x)
    if model.endswith(":truncated-exponential"):
        a, b = coefficients["a"], coefficients["b"]
        w = b - a
        mean = mp.fsum(v - a for v in x) / (w * n)
        if mean == mp.mpf("0.5"):
            return mp.mpf(0), -n * mp.log(w)
        d = min(mean, 1 - mean)
        lo, hi = mp.mpf(0), 1 / d
        for _ in range(240):
            t = (lo + hi) / 2
            if 1 / t - mp.exp(-t) / (-mp.expm1(-t)) > d:
                lo = t
            else:
                hi = t
        t = (lo + hi) / 2
        rate = t / w if mean > mp.mpf("0.5") else -t / w
        return rate, n * (mp.log(abs(rate)) - t * d - mp.log(-mp.expm1(-t)))
    endpoint = coefficients["mu"]
    d = mp.fsum(abs(v - endpoint) for v in x) / n
    return 1 / d, n * (-mp.log(d) - 1)


def check(snapshot, cases):
    groups = {}
    for line in Path(snapshot).read_text().splitlines():
        if line.startswith("#"):
            continue
        row = json.loads(line)
        r = row["result"]
        if r["kind"] != "limit" or not r["model"].endswith("-exponential"):
            continue
        values = {k: mp.mpf(float.fromhex(v)) for k, v in r["limit_estimates"].items()}
        case = int(row["case"])
        rate, ll = optimum(cases[case], r["model"], values)
        ll_hat = mp.mpf(float.fromhex(r["limit_loglik"]))
        rate_scale = max(mp.mpf(1), abs(rate)) if ":truncated-" in r["model"] else abs(rate)
        rate_err = abs(values["rate"] - rate) / rate_scale
        ll_err = abs(ll_hat - ll) / max(mp.mpf(1), abs(ll))
        counts, max_rate, max_ll = groups.get(r["model"], (0, (0, 0), (0, 0)))
        groups[r["model"]] = counts + 1, max(max_rate, (rate_err, case)), max(max_ll, (ll_err, case))
        if case in (7657, 7660, 7779):
            print(f"  case {case}: {'%.17g' % values['rate']}, {'%.17g' % ll_hat}")
        assert rate_err <= mp.mpf("1e-10") and ll_err <= mp.mpf("1e-10"), (case, rate_err, ll_err)
    for model, (count, max_rate, max_ll) in sorted(groups.items()):
        print(model, count, f"max scaled rate error {mp.nstr(max_rate[0], 5)} (case {max_rate[1]})",
              f"max scaled loglik error {mp.nstr(max_ll[0], 5)} (case {max_ll[1]})")


with gzip.open(ARCHIVE, "rt") as f:
    archive = {row["index"]: [mp.mpf(v) for v in row["data"]] for row in json.load(f)
               if row["family"] == "tnorm"}
for path in sys.argv[1:]:
    print(path)
    check(path, archive)
