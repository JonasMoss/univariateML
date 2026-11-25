"""Compare four snapshot modes and write the cardinality changes by route."""
import argparse
import csv
import gzip
import json
from collections import Counter
from pathlib import Path

MODES = ("native", "double", "routes-native", "routes-double")


def key(row):
    return (
        row["family"], row["case"], json.dumps(row["held"], sort_keys=True),
        row.get("mask"), row["input_sha256"],
    )


def index(lines):
    rows = {}
    for line in lines:
        if line.startswith("#") or not line.strip():
            continue
        row = json.loads(line)
        identity = key(row)
        if identity in rows:
            raise ValueError(f"duplicate snapshot key {identity}")
        rows[identity] = row
    return rows


def remove_uniqueness(result):
    return {
        **{k: v for k, v in result.items() if k not in ("outcome", "unique")},
        "outcome": {k: v for k, v in result["outcome"].items() if k != "unique"},
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("snapshot_dir", type=Path)
    parser.add_argument("baseline_dir", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--mode", choices=MODES, action="append",
                        help="check only the selected mode, repeatable")
    args = parser.parse_args()
    moves = Counter()
    unexpected = 0
    for mode in args.mode or MODES:
        with gzip.open(args.baseline_dir / f"{mode}.jsonl.gz", "rt") as source:
            baseline = index(source)
        fresh = index(
            line
            for file in sorted((args.snapshot_dir / mode).glob("*.jsonl"))
            for line in file.read_text().splitlines()
        )
        only_new = fresh.keys() - baseline.keys()
        only_old = baseline.keys() - fresh.keys()
        print(f"{mode} {len(fresh)} cases, {len(only_new)} added, {len(only_old)} missing")
        unexpected += len(only_new) + len(only_old)
        for identity in sorted(baseline.keys() & fresh.keys()):
            before, after = baseline[identity], fresh[identity]
            a, b = before["result"], after["result"]
            if a == b:
                continue
            if (
                a.get("unique") is True and b.get("unique") is None
                and a["outcome"].get("unique") is True and b.get("outcome", {}).get("unique") is None
                and "outcome" in b and remove_uniqueness(a) == remove_uniqueness(b)
            ):
                holds = ",".join(sorted(after["held"])) or "(none)"
                source = after.get("source", "")
                moves[mode, after["family"], holds, source, b["core_status"], b["certified"]] += 1
            else:
                fields = [name for name in a.keys() | b.keys() if a.get(name) != b.get(name)]
                print(f"UNEXPECTED {mode} {identity[:4]} {', '.join(sorted(fields))}")
                unexpected += 1
        print(f"{mode} {sum(n for group, n in moves.items() if group[0] == mode)} TRUE to NA")
    with args.output.open("w", newline="") as output:
        writer = csv.writer(output, lineterminator="\n")
        writer.writerow(("mode", "family", "held", "source", "core_status", "certified", "count"))
        for group, count in sorted(moves.items()):
            writer.writerow((*group, count))
    return int(bool(unexpected))


if __name__ == "__main__":
    raise SystemExit(main())
