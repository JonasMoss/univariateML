"""Check the reviewed submission against its source and the saved atlases."""

from collections import Counter
import csv
from functools import cache
from pathlib import Path
import sys


DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
PROPOSAL = DATA.with_name("2026-11-27-trunc-est-math-data") / "proofs.csv"
KEY = "family", "held", "modifier", "question"
FIELDS = (*KEY, "claim", "direction", "status", "evidence", "frontier", "origin")

sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def read(path):
    with path.open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream, strict=True)
        fields = tuple(reader.fieldnames)
        rows = list(reader)
        assert all(None not in row and None not in row.values() for row in rows), path
        return fields, rows


def key(row):
    return tuple(row[field] for field in KEY)


@cache
def anchors(path):
    assert path.is_file() and path.is_relative_to(ROOT), path
    return proof_anchors(path)


def main():
    fields, proposed = read(PROPOSAL)
    assert fields == FIELDS
    fields, reviewed = read(DATA / "reviewed.csv")
    assert fields == FIELDS
    _, base = read(ATLAS / "atlas.csv")
    _, modifiers = read(ATLAS / "modifier-atlas.csv")
    _, known = read(ATLAS / "modifier-proofs.csv")
    parent_keys = {(r["family"], r["held"]) for r in base}
    modifier_keys = {tuple(r[field] for field in KEY[:3]) for r in modifiers}
    known_keys = {key(r) for r in known if r["modifier"] == "truncate:known"}
    proposed_keys = {key(r) for r in proposed}
    reviewed_keys = {key(r) for r in reviewed}
    assert len(proposed_keys) == len(proposed) == 270
    assert len(reviewed_keys) == len(reviewed)
    assert reviewed_keys <= proposed_keys
    for family, held, modifier, question in proposed_keys:
        assert (family, held or "all free") in parent_keys
        assert (family, held or "all free", modifier) in modifier_keys
        assert (family, held, "truncate:known", question) in known_keys

    for row in reviewed:
        assert row["origin"] == "TASK-448"
        assert row["status"] == "written"
        assert row["direction"] in {"iff", "exact", "one-sided"}
        for ref in row["evidence"].split("; "):
            file, separator, anchor = ref.partition("#")
            assert separator and anchor in anchors((ROOT / file).resolve()), ref

    before = {key(r): r for r in proposed}
    counts = Counter(
        "accepted" if r == (before[key(r)] | {"origin": "TASK-448"}) else "regraded"
        for r in reviewed
    )
    counts["rejected"] = len(proposed) - len(reviewed)
    print(f"{len(reviewed)}/{len(proposed)} rows validated, {len(known_keys)} known-window keys")
    print(", ".join(f"{counts[label]} {label}" for label in ("accepted", "regraded", "rejected")))
    print(dict(sorted(Counter(r["direction"] for r in reviewed).items())))


if __name__ == "__main__":
    main()
