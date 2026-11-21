"""Summarize canonical snapshots against the frozen 2026-11-15 baseline."""
import argparse
from collections import Counter
import gzip
import json
import math
from pathlib import Path


def key(row):
    return row["family"], row["source"], row["case"], row["mask"], row["input_sha256"]


def read(lines):
    return dict((key(row), row["result"]) for row in
                (json.loads(line) for line in lines if not line.startswith("#")))


def summarize(snapshot, baseline):
    with gzip.open(baseline, "rt") as stream:
        old = read(stream)
    new = {}
    for path in sorted(snapshot.glob("*.jsonl")):
        with path.open() as stream:
            new.update(read(stream))
    for family in sorted({k[0] for k in new}):
        before = {k for k in old if k[0] == family}
        after = {k for k in new if k[0] == family}
        common = sorted(before & after)
        changes = [k for k in common if old[k] != new[k]]
        fields = Counter()
        moves = []
        kinds = []
        for k in changes:
            a, b = old[k], new[k]
            fields.update(f for f in a.keys() | b.keys() if a.get(f) != b.get(f))
            if a["kind"] != b["kind"]:
                kinds.append((k[1:4], a["kind"], b["kind"]))
            if a["kind"] == b["kind"] == "ok":
                for param in a["estimates"].keys() & b["estimates"].keys():
                    x = float.fromhex(a["estimates"][param])
                    y = float.fromhex(b["estimates"][param])
                    if math.isfinite(x) and math.isfinite(y):
                        moves.append((abs(y-x)/abs(x) if x else (math.inf if y else 0.0), k[1:4], param))
        print(family, len(common), "compared,", len(changes), "changed,", len(kinds), "kind moves")
        print("  fields:", dict(sorted(fields.items())))
        print("  biggest coefficient move:", max(moves, default=(0, "", "")))
        for move in kinds:
            print("  kind:", *move)
        for label, keys in (("missing", before-after), ("additional", after-before)):
            for k in sorted(keys):
                print(" ", label, k[1:])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("snapshot", type=Path)
    parser.add_argument("baseline", type=Path)
    args = parser.parse_args()
    summarize(args.snapshot, args.baseline)
