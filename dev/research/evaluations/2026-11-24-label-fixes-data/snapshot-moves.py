#!/usr/bin/env python3
"""Record every baseline difference for the four label-fixes snapshots."""

import argparse
from collections import Counter
import gzip
import json
from pathlib import Path


MODES = ("native", "double", "routes-native", "routes-double")


def index(lines):
    records = {}
    for line in lines:
        if line.startswith("#"):
            continue
        row = json.loads(line)
        key = (row["family"], row["source"], row["case"],
               json.dumps(row["held"], sort_keys=True), row.get("mask"),
               row.get("input_sha256"))
        if key in records:
            raise ValueError(f"duplicate snapshot input: {key}")
        records[key] = row["result"]
    return records


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline", type=Path)
    parser.add_argument("fresh", type=Path)
    args = parser.parse_args()
    modes = {}
    for mode in MODES:
        with gzip.open(args.baseline / (mode + ".jsonl.gz"), "rt") as stream:
            before = index(stream)
        with gzip.open(args.fresh / mode / "snapshot.jsonl.gz", "rt") as stream:
            after = index(stream)
        changes = []
        counts = Counter()
        for key in sorted(before.keys() & after.keys(), key=repr):
            old, new = before[key], after[key]
            fields = sorted(field for field in old.keys() | new.keys()
                            if (field in old) != (field in new) or old.get(field) != new.get(field))
            if fields:
                counts[key[0]] += 1
                changes.append({
                    "family": key[0], "source": key[1], "case": key[2],
                    "held": json.loads(key[3]), "mask": key[4], "input_sha256": key[5],
                    "before": {field: old[field] for field in fields if field in old},
                    "after": {field: new[field] for field in fields if field in new},
                })
        modes[mode] = {
            "baseline_cases": len(before),
            "fresh_cases": len(after),
            "changed_by_family": dict(sorted(counts.items())),
            "only_in_baseline": sorted(before.keys() - after.keys(), key=repr),
            "only_in_fresh": sorted(after.keys() - before.keys(), key=repr),
            "changes": changes,
        }
    print(json.dumps(modes, indent=2))


if __name__ == "__main__":
    main()
