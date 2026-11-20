"""Check the two GED held-scale location regressions against a 50-digit root."""

import argparse
import gzip
import json
from pathlib import Path

import mpmath as mp


CASES = {"002463", "002466"}
ARCHIVE = Path("dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz")


def reference(sample, shape):
    lower, upper = min(sample), max(sample)
    for _ in range(200):
        middle = (lower+upper)/2
        if middle == lower or middle == upper:
            break
        score = mp.fsum(mp.sign(x-middle)*abs(x-middle)**(shape-1) for x in sample)
        if score > 0:
            lower = middle
        else:
            upper = middle
    return (lower+upper)/2


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("snapshot", type=Path, help="path to the GED family JSONL snapshot")
    args = parser.parse_args()
    with gzip.open(ARCHIVE, "rt") as stream:
        cases = {f'{row["index"]:06d}': row for row in json.load(stream)}
    with args.snapshot.open() as stream:
        rows = [json.loads(line) for line in stream if line.startswith("{")]
    mp.mp.dps = 50
    passed = set()
    for row in rows:
        ident = row["case"]
        if row["source"] != "archive" or ident not in CASES:
            continue
        case, fit = cases[ident], row["result"]
        if fit["kind"] != "ok":
            print(ident, fit["kind"], fit["message"])
            continue
        sample = [mp.mpf(x) for x in case["data"]]
        center = reference(sample, mp.mpf(case["held"]["nu"]))
        reported = mp.mpf(float.fromhex(fit["estimates"]["mean"]))
        error = abs(reported-center)/max(abs(center), mp.mpf(case["held"]["sd"]))
        print(ident, case["sample"], mp.nstr(center, 25), "scaled error", mp.nstr(error, 8))
        if error <= mp.mpf("1e-10"):
            passed.add(ident)
    return 0 if passed == CASES else 1


if __name__ == "__main__":
    raise SystemExit(main())
