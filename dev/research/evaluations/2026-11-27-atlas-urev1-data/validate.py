"""Check the reviewed atlas join."""
import csv
from collections import Counter
from pathlib import Path
import sys

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def read(path):
    with path.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def key(row):
    return row["family"], row["held"], row["question"]


def validate():
    with (ATLAS / "proofs.csv").open(newline="", encoding="utf-8") as stream:
        fields = next(csv.reader(stream))
    regimes = {
        (row["family"], "" if row["held"] == "all free" else row["held"])
        for row in read(ATLAS / "atlas.csv")
    }
    proposals = {
        (*key(row), row["origin"])
        for job in ("generic", "skew2", "locscale")
        for row in read(DATA.parent / f"2026-11-27-uniq-{job}-math-data/proofs.csv")
    }
    seen, anchors = set(), {}
    grades = Counter()
    for row in read(DATA / "reviewed.csv"):
        assert list(row) == fields and None not in row.values(), row
        row_key = key(row)
        assert row_key[:2] in regimes and row_key[2] == "uniqueness", row_key
        assert row_key not in seen, row_key
        assert (*row_key, row["origin"]) in proposals, row
        seen.add(row_key)
        file, fragment = row["evidence"].split("#", 1)
        source = ROOT / file
        assert source.is_file(), source
        if source not in anchors:
            anchors[source] = proof_anchors(source)
        assert fragment in anchors[source], row["evidence"]
        grades[row["direction"], row["status"]] += 1
    print(f"{len(seen)} unique atlas keys with valid origins and evidence anchors")
    for (direction, status), count in sorted(grades.items()):
        print(f"{direction}, {status}: {count}")


if __name__ == "__main__":
    validate()
