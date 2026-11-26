#!/usr/bin/env python3
"""Classify every dgamma change against the batch-19 fit snapshots."""
import argparse
from collections import Counter
import gzip
import json
from pathlib import Path


def key(rec):
    return rec["source"], rec["case"], rec["mask"], rec["input_sha256"]


def difference(a, b):
    return abs(b - a) / abs(a) if a else abs(b - a)


def compare(baseline, fresh):
    with gzip.open(baseline, "rt") as stream:
        rows = (json.loads(line) for line in stream)
        old = {key(row): row for row in rows if row["family"] == "dgamma"}
    snapshots = sorted(fresh.glob("*.jsonl"))
    new = {}
    for snapshot in snapshots:
        for line in snapshot.read_text().splitlines()[1:]:
            row = json.loads(line)
            if row["family"] == "dgamma":
                new[key(row)] = row
    assert old.keys() == new.keys(), (old.keys() - new.keys(), new.keys() - old.keys())
    moves = []
    for identity, row in new.items():
        before, after = old[identity]["result"], row["result"]
        if before == after:
            continue
        entry = {k: row[k] for k in ("source", "case", "held", "input_sha256")}
        entry["changed_fields"] = sorted(k for k in before.keys() | after.keys()
                                         if before.get(k) != after.get(k))
        if after["kind"] == "ok" and not row["held"]:
            if after["unique"] is True:
                entry["route"] = "Certified unique via the curvature proof and coefficient brackets"
            else:
                assert after["certified"] is False and after["unique"] is None
                entry["route"] = "Checked multistart fit with inconclusive curvature certificate"
        else:
            entry["route"] = "Other outcome change"
        entry["before"] = dict(kind=before["kind"], certified=before["certified"], unique=before["unique"])
        entry["after"] = dict(kind=after["kind"], certified=after["certified"], unique=after["unique"])
        if before["kind"] == after["kind"] == "ok":
            entry["relative_coefficient_change"] = {
                name: difference(float.fromhex(value), float.fromhex(after["estimates"][name]))
                for name, value in before["estimates"].items()
            }
            entry["logLik_change"] = float.fromhex(after["logLik"]) - float.fromhex(before["logLik"])
        moves.append(entry)
    return dict(rows=len(old), changes=len(moves), by_route=dict(Counter(m["route"] for m in moves)), moves=moves)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline", type=Path)
    parser.add_argument("fresh", type=Path)
    args = parser.parse_args()
    modes = ("native", "double", "routes-native", "routes-double")
    print(json.dumps({mode: compare(args.baseline / (mode + ".jsonl.gz"),
                                    args.fresh / mode) for mode in modes}, indent=2))


if __name__ == "__main__":
    main()
