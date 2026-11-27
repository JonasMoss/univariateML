#!/usr/bin/env python3
"""Validate reviewed keys, source cards and all evidence anchors."""

from collections import Counter
import csv
from functools import lru_cache
from pathlib import Path
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def read(path):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def key(row):
    return row["family"], row["held"] or "all free", row["question"]


def main():
    atlas_dir = ROOT / "dev/research/existence-atlas"
    fields, _ = read(atlas_dir / "proofs.csv")
    atlas_fields, atlas = read(atlas_dir / "atlas.csv")
    masks = {(row["family"], row["held"]) for row in atlas}
    proposed = {}
    for job, card in (("crit-open-math", "TASK-424"), ("checkable-math", "TASK-425")):
        _, candidates = read(HERE.parent / f"2026-11-27-{job}-data/proofs.csv")
        for candidate in candidates:
            candidate_key = key(candidate)
            assert candidate_key not in proposed, candidate_key
            proposed[candidate_key] = card

    anchors = lru_cache(maxsize=None)(proof_anchors)
    reviewed_fields, rows = read(HERE / "reviewed.csv")
    assert reviewed_fields == fields, reviewed_fields
    assert rows, "Empty review"
    seen = set()
    for row in rows:
        assert None not in row and all(value is not None for value in row.values()), row
        row_key = key(row)
        assert row_key[:2] in masks and f"{row_key[2]}_claim" in atlas_fields, row_key
        assert row_key not in seen, f"Duplicate key {row_key}"
        assert proposed[row_key] == row["origin"], row_key
        for reference in row["evidence"].split("; "):
            file, sep, fragment = reference.partition("#")
            source = ROOT / file
            assert sep and source.is_file() and fragment in anchors(source), (row_key, reference)
        seen.add(row_key)

    print(f"{len(seen)} unique atlas keys and all evidence anchors verified")
    for (status, direction), count in sorted(Counter(
        (row["status"], row["direction"]) for row in rows
    ).items()):
        print(f"{status}/{direction} {count}")


if __name__ == "__main__":
    main()
