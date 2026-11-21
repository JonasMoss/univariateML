#!/usr/bin/env python3
"""Compare one fresh tlambda snapshot with its stored canonical mode."""
import gzip
import json
import math
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[4]
ARCHIVE = ROOT / "dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz"


def main(baseline, snapshot):
    def key(row):
        return (row["source"], row["case"], row["mask"], row["input_sha256"])

    with gzip.open(baseline, "rt") as source:
        old = {key(row): row for row in map(json.loads, source)
               if row["family"] == "tlambda"}
    new = {key(row): row for row in map(json.loads, snapshot.read_text().splitlines()[1:])}
    assert old.keys() == new.keys()
    with gzip.open(ARCHIVE, "rt") as source:
        archive = {f"{row['index']:06d}": row["data"] for row in json.load(source)
                   if row["family"] == "tlambda"}
    count = paired = 0
    max_move = 0.0
    for k in sorted(old):
        a, b = old[k]["result"], new[k]["result"]
        if a["kind"] == b["kind"] == "ok":
            paired += 1
            for name, value in a["estimates"].items():
                u, v = float.fromhex(value), float.fromhex(b["estimates"][name])
                assert math.isfinite(u) and math.isfinite(v)
                if u != v:
                    max_move = max(max_move, abs(v - u) / abs(u) if u else math.inf)
        if a != b:
            count += 1
            print(*k[:3], a.get("outcome"), "->", b.get("outcome"))
            print("  data:", new[k].get("data", archive.get(k[1])))
            print("  changed fields:", ", ".join(sorted(
                field for field in a.keys() | b.keys() if a.get(field) != b.get(field))))
    print(f"{len(old)} compared, {count} changed, {paired} estimate pairs,"
          f" maximum relative coefficient move {max_move:g}")


if __name__ == "__main__":
    main(*(Path(arg) for arg in sys.argv[1:]))
