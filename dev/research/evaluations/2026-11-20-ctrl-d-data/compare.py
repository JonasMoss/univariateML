"""Summarize paired fit snapshots, including all changes in rendered kind."""

import argparse
from collections import Counter
import gzip
import json
import math
from pathlib import Path


def key(row):
    return (
        row["family"], row["source"], row["case"],
        json.dumps(row["held"], sort_keys=True), row.get("mask"), row.get("input_sha256"),
    )


def read(lines):
    rows = {}
    for line in lines:
        if not line.startswith("{"):
            continue
        row = json.loads(line)
        if "result" in row:
            rows[key(row)] = row["result"]
    return rows


def describe(result):
    outcome = result.get("outcome") or {}
    return f'{result["kind"]}/{outcome.get("kind")}/{outcome.get("reason")}'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("snapshot", type=Path)
    parser.add_argument("baseline", type=Path)
    args = parser.parse_args()
    new = {}
    for path in sorted(args.snapshot.glob("*.jsonl")):
        with path.open() as stream:
            new.update(read(stream))
    families = {case[0] for case in new}
    with gzip.open(args.baseline, "rt") as stream:
        old = {case: fit for case, fit in read(stream).items() if case[0] in families}
    paired = new.keys() & old.keys()
    compared = Counter(case[0] for case in paired)
    changed = Counter(case[0] for case in paired if new[case] != old[case])
    moves = {}
    triang_move = (0, "")
    print("Family        Compared Changed   Max relative coefficient move (case, coefficient)")
    for case in sorted(paired):
        before, after = old[case], new[case]
        if before["kind"] != "ok" or after["kind"] != "ok":
            continue
        for name in before["estimates"].keys() & after["estimates"].keys():
            x = float.fromhex(before["estimates"][name])
            y = float.fromhex(after["estimates"][name])
            if x == y:
                continue
            scale = abs(x)
            if name in {"mu", "mean"}:
                for unit in ("sd", "sigma", "scale"):
                    if unit in before["estimates"]:
                        scale = max(scale, float.fromhex(before["estimates"][unit]))
                        break
            distance = abs(y-x)
            error = (distance/scale if math.isfinite(distance)
                     else abs(y/scale-x/scale)) if scale else math.inf
            label = f"{case[1]} {case[2]}, {name}"
            if error > moves.get(case[0], (0,))[0]:
                moves[case[0]] = (error, label)
            if case[0] == "triang" and name in {"a", "b"}:
                a, b = (float.fromhex(before["estimates"][field]) for field in ("a", "b"))
                width = b-a
                if width > 0 and distance/width > triang_move[0]:
                    triang_move = (distance/width, label)
    for family in sorted(compared):
        error, where = moves.get(family, (0, ""))
        print(f"{family:14} {compared[family]:7} {changed[family]:7}   {error:.6g} {where}")
    if "triang" in families:
        print(f"Triang endpoints in support-width units: {triang_move[0]:.6g} {triang_move[1]}")
    print("\nOutcome transitions")
    for case in sorted(paired):
        before, after = old[case], new[case]
        if describe(before) != describe(after):
            print(*case[:3], case[3], describe(before), "->", describe(after),
                  repr(after["message"]))
    print("\nUnpaired cases")
    for title, cases in (("new", new.keys()-old.keys()), ("baseline", old.keys()-new.keys())):
        for case in sorted(cases):
            print(title, *case)
    print("\nNamed strict entries")
    for case in sorted(paired):
        family, source, label = case[:3]
        if ((family, source, label) in {("snorm", "class", "huge"), ("sstd", "class", "two")}
                or family == "snorm" and source == "archive" and label in {"006576", "006587", "006590"}):
            print(*case[:3], describe(old[case]), "->", describe(new[case]),
                  repr(new[case]["message"]))


if __name__ == "__main__":
    main()
