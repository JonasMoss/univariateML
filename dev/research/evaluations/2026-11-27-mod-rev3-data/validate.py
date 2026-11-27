"""Validate the TASK-447 join file against both saved atlas axes."""

import csv
import sys
from collections import Counter
from functools import cache
from pathlib import Path

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
SOURCE = DATA.with_name("2026-11-27-trunc-expfam-math-data") / "proofs.csv"
KEY = ("family", "held", "modifier", "question")
ATOM = {"zero:modified": "p0", "zero:inflated": "pi"}

sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors  # noqa: E402

anchors = cache(proof_anchors)


def read(path):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream, strict=True)
        return reader.fieldnames, list(reader)


def key(row):
    return tuple(row[name] for name in KEY)


def main():
    fields, proposed = read(SOURCE)
    incoming_fields, reviewed = read(DATA / "reviewed.csv")
    assert fields == incoming_fields and tuple(fields[:4]) == KEY
    scoped = [row for row in proposed
              if set(row["held"].split("+")) & set(ATOM.values())]
    proposed_keys = {key(row) for row in scoped}
    assert len(proposed_keys) == len(scoped) == 42
    _, base_rows = read(ATLAS / "atlas.csv")
    parents = {(row["family"], row["held"]) for row in base_rows}
    _, modifier_rows = read(ATLAS / "modifier-atlas.csv")
    modifiers = {(row["family"], row["held"], row["modifier"])
                 for row in modifier_rows}
    for family, held, modifier, question in proposed_keys:
        mask = held.split("+")
        assert mask == sorted(set(mask)) and ATOM[modifier] in mask
        parent = "+".join(name for name in mask if name != ATOM[modifier])
        assert (family, parent or "all free") in parents, (family, held)
        assert (family, held, modifier) in modifiers, (family, held, modifier)

    seen = set()
    for row in reviewed:
        k = key(row)
        assert None not in row and None not in row.values(), k
        assert k in proposed_keys and k not in seen, k
        seen.add(k)
        assert row["origin"] == "TASK-439" and row["status"] == "written", k
        for reference in row["evidence"].split("; "):
            file, separator, anchor = reference.partition("#")
            path = ROOT / file
            assert separator and path.is_file() and anchor in anchors(path), (k, reference)

    print(f"{len(seen)} validated join rows of {len(proposed_keys)} in scope")
    for label, field in [("modifier", "modifier"), ("grade", "direction")]:
        for value, count in sorted(Counter(row[field] for row in reviewed).items()):
            print(f"{label} {value}: {count}")
    for missing in sorted(proposed_keys - seen):
        print("Omitted:", " / ".join(missing))


if __name__ == "__main__":
    main()
