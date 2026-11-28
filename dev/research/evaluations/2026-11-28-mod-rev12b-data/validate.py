"""Validate the TASK-473 submission against the parent and modifier atlases."""

import csv
from functools import lru_cache
from pathlib import Path
import sys

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
sys.path.insert(0, str(ROOT / "dev/tools"))

from existence_atlas import proof_anchors

FAMILIES = {
    "huber", "dblpareto", "ht", "bhatt", "emg", "tlambda",
    "logitnorm", "lgamma", "slash",
}
FIELDS = [
    "family", "held", "modifier", "question", "claim", "direction",
    "status", "evidence", "frontier", "origin",
]


def read_csv(path):
    with path.open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def key(row):
    return tuple(row[field] for field in FIELDS[:4])


def main():
    fields, reviewed = read_csv(DATA / "reviewed.csv")
    assert fields == FIELDS, fields
    submitted = [key(row) for row in reviewed]
    assert len(submitted) == len(set(submitted)), "duplicate keys"

    _, proposals = read_csv(DATA.with_name("2026-11-28-trunc-est3-math-data") / "proofs.csv")
    proposed = {key(row) for row in proposals if row["family"] in FAMILIES}
    assert set(submitted) == proposed, (proposed.difference(submitted), set(submitted) - proposed)

    _, parent_rows = read_csv(ATLAS / "atlas.csv")
    parents = {(row["family"], row["held"]) for row in parent_rows}
    _, modifier_rows = read_csv(ATLAS / "modifier-atlas.csv")
    eligible = {
        (row["family"], row["held"], row["modifier"]) for row in modifier_rows
    }

    anchors = lru_cache(maxsize=None)(proof_anchors)
    for row in reviewed:
        k = key(row)
        assert k[:2] in parents, k
        assert k[:3] in eligible, k
        assert row["origin"] == "TASK-469", k
        assert row["claim"] and row["status"] == "written", k
        assert row["direction"] in {"iff", "exact", "one-sided"}, k
        assert row["evidence"], k
        for citation in row["evidence"].split("; "):
            name, _, anchor = citation.partition("#")
            path = ROOT / name
            assert path.is_file() and anchor and anchor in anchors(path), (k, citation)

    print(f"{len(reviewed)} rows validated for schema, coverage, parent, modifier, anchor, and key uniqueness")


if __name__ == "__main__":
    main()
