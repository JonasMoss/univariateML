"""Validate the TASK-454 join file against the proposals and both atlas axes."""

import csv
import sys
from collections import Counter
from functools import cache
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
SOURCE = HERE.with_name("2026-11-27-trunc-ls-math-data") / "proofs.csv"
KEY_FIELDS = ("family", "held", "modifier", "question")

sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors

anchors = cache(proof_anchors)


def read(path):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream, strict=True)
        return reader.fieldnames, list(reader)


def key(row):
    return tuple(row[field] for field in KEY_FIELDS)


def main():
    fields, proposed = read(SOURCE)
    reviewed_fields, reviewed = read(HERE / "reviewed.csv")
    assert fields == reviewed_fields
    proposed_keys = {key(row) for row in proposed}
    assert len(proposed_keys) == len(proposed) == 91

    _, base = read(ATLAS / "atlas.csv")
    _, modified = read(ATLAS / "modifier-atlas.csv")
    parent_keys = {(row["family"], row["held"]) for row in base}
    modifier_keys = {(row["family"], row["held"], row["modifier"]) for row in modified}
    for family, held, modifier, _ in proposed_keys:
        parent = family, held or "all free"
        assert parent in parent_keys, parent
        assert modifier == "truncate:known" and (*parent, modifier) in modifier_keys, parent

    seen = set()
    for row in reviewed:
        k = key(row)
        assert None not in row and None not in row.values(), k
        assert k in proposed_keys and k not in seen, k
        seen.add(k)
        assert row["origin"] == "TASK-449" and row["status"] == "written", k
        for reference in row["evidence"].split("; "):
            file, separator, anchor = reference.partition("#")
            path = (ROOT / file).resolve()
            assert separator and path.is_relative_to(ROOT) and path.is_file(), (k, reference)
            assert anchor in anchors(path), (k, reference)

    print(f"{len(seen)} validated join rows of {len(proposed_keys)} proposed")
    print(dict(sorted(Counter(row["direction"] for row in reviewed).items())))
    for missing in sorted(proposed_keys - seen):
        print("Omitted", missing)


if __name__ == "__main__":
    main()
