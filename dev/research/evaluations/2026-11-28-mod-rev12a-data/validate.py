"""Validate the reviewed estimated-window join input."""

import csv
import sys
from collections import Counter
from pathlib import Path

DATA = Path(__file__).resolve().parent
REPO = DATA.parents[3]
sys.path.insert(0, str(REPO / "dev/tools"))
from existence_atlas import proof_anchors

FAMILIES = {"ged", "sged", "sn", "snorm", "sstd", "st", "lst"}
MODIFIERS = {
    "truncate:lower-estimated",
    "truncate:upper-estimated",
    "truncate:both-estimated",
}
KEY = ("family", "held", "modifier", "question")
COLUMNS = (*KEY, "claim", "direction", "status", "evidence", "frontier", "origin")


def read(path):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def key(row):
    return tuple(row[field] for field in KEY)


def main():
    columns, rows = read(DATA / "reviewed.csv")
    assert tuple(columns) == COLUMNS
    _, original = read(DATA.parent / "2026-11-28-trunc-est3-math-data/proofs.csv")
    _, atlas = read(REPO / "dev/research/existence-atlas/atlas.csv")
    parents = {(row["family"], row["held"]) for row in atlas}
    expected = {key(row) for row in original if row["family"] in FAMILIES}
    seen = set()
    anchors = {}
    for row in rows:
        item = key(row)
        assert item not in seen, item
        seen.add(item)
        assert item[:2] in parents, item
        assert row["modifier"] in MODIFIERS, item
        assert row["origin"] == "TASK-469", item
        assert row["status"] == "written", item
        assert row["direction"] in {"iff", "exact", "one-sided"}, item
        for evidence in row["evidence"].split("; "):
            filename, separator, anchor = evidence.partition("#")
            path = REPO / filename
            assert separator and path.is_file(), (item, evidence)
            if path not in anchors:
                anchors[path] = proof_anchors(path)
            assert anchor in anchors[path], (item, evidence)

    assert seen == expected and len(rows) == 621, (seen - expected, expected - seen)
    print(f"Validated {len(rows)} unique reviewed keys, parent regimes and evidence anchors.")
    print(dict(Counter(row["direction"] for row in rows)))


if __name__ == "__main__":
    main()
