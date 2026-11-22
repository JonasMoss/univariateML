"""Compare the five family snapshots to the frozen and current-master baselines."""
import gzip
import json
from pathlib import Path
import subprocess
import sys

FAMILIES = {"bbinom", "bhatt", "cauchy", "emg", "st"}
SNAPSHOTS = Path("build/snapshots/render-fu")
FROZEN = Path("dev/research/evaluations/2026-11-15-fit-snapshot-data")
DIFF = Path("build/scratch/diff_baseline.py")


def key(row):
    return row["family"], row["source"], row["case"], row.get("mask"), row["input_sha256"]


def read(directory):
    rows = {}
    for family in sorted(FAMILIES):
        path = directory / f"{family}.jsonl"
        for line in path.read_text().splitlines()[1:]:
            row = json.loads(line)
            rows[key(row)] = row
    return rows


def compare(mode):
    before = read(SNAPSHOTS / f"before-{mode}")
    after = read(SNAPSHOTS / f"after-{mode}")
    frozen = {}
    frozen_subset = SNAPSHOTS / f"frozen-{mode}.jsonl.gz"
    with gzip.open(FROZEN / f"{mode}.jsonl.gz", "rt") as file, gzip.open(frozen_subset, "wt") as out:
        for line in file:
            row = json.loads(line)
            if row["family"] in FAMILIES:
                frozen[key(row)] = row
                out.write(line)
    print(f"{mode}, against stored baselines", flush=True)
    subprocess.run([sys.executable, DIFF, SNAPSHOTS / f"after-{mode}", frozen_subset], check=False)
    print(f"{mode}, against cc7bfa80 ({len(before)} cases)", flush=True)
    assert before.keys() == after.keys()
    for case, old_row in before.items():
        new = after[case]["result"]
        old = old_row["result"]
        if old != new:
            fields = sorted(f for f in set(old) | set(new) if old.get(f) != new.get(f))
            print(case[:4], old_row["held"], old["outcome"], "->", new["outcome"], fields)
            print(json.dumps({f: [old.get(f), new.get(f)] for f in fields}, sort_keys=True))
    print("Inherited differences on frozen cases:",
          sum(before[k]["result"] != frozen[k]["result"] for k in before.keys() & frozen.keys()))


if __name__ == "__main__":
    for precision in ("native", "double"):
        compare(precision)
