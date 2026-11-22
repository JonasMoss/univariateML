"""Quantify changes to selected families in canonical fit snapshots."""
import gzip
import json
import math
from pathlib import Path
import sys


def key(row):
    return (row["family"], row["case"], json.dumps(row["held"], sort_keys=True),
            row["mask"], row["input_sha256"])


def summarize(baseline, snapshot):
    new = {}
    for path in snapshot.glob("*.jsonl"):
        for line in path.read_text().splitlines()[1:]:
            row = json.loads(line)
            new[key(row)] = row
    selected = {k[0] for k in new}
    with gzip.open(baseline, "rt") as handle:
        rows = map(json.loads, handle)
        old = {key(row): row for row in rows if row["family"] in selected}
    if old.keys() != new.keys():
        raise ValueError(f"mismatched keys: {old.keys() - new.keys()}, {new.keys() - old.keys()}")

    for family in sorted(selected):
        keys = [k for k in old if k[0] == family]
        differing = [k for k in keys if old[k]["result"] != new[k]["result"]]
        moves = []
        for k in differing:
            a, b = old[k]["result"], new[k]["result"]
            changes = sorted(field for field in a.keys() | b.keys() if a.get(field) != b.get(field))
            print(f"{family} {k[1]} {a['kind']} -> {b['kind']} {', '.join(changes)}")
            if a["kind"] == b["kind"] == "ok":
                for param, value in a["estimates"].items():
                    prior = float.fromhex(value)
                    current = float.fromhex(b["estimates"][param])
                    if current != prior:
                        moves.append((abs((current - prior) / prior) if prior else math.inf, k[1], param))
        largest = max(moves, default=(0.0, "", ""))
        print(f"{family}: {len(differing)}/{len(keys)} changed, "
              f"max relative estimate move {largest[0]:.12g} {largest[1]} {largest[2]}".rstrip())


if __name__ == "__main__":
    summarize(Path(sys.argv[1]), Path(sys.argv[2]))
