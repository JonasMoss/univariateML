"""Validate the TASK-474 join file without writing to the atlas."""

import csv
import sys
from pathlib import Path

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
PROPOSAL = DATA.parent / "2026-11-28-trunc-est3-math-data/proofs.csv"
FIELDS = (
    "family", "held", "modifier", "question", "claim", "direction",
    "status", "evidence", "frontier", "origin",
)
FAMILIES = {
    "frechet", "pareto2", "weibull", "lomax", "llogis",
    "gompertz", "power", "kumar", "invexp",
}

sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def read_rows(path):
    with path.open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream)
        return tuple(reader.fieldnames), list(reader)


def parent(row):
    return row["family"], row["held"] or "all free"


def regime(row):
    return (*parent(row), row["modifier"])


def key(row):
    return (*regime(row), row["question"])


def main():
    fields, rows = read_rows(DATA / "reviewed.csv")
    assert fields == FIELDS, fields
    proposed = {key(row) for row in read_rows(PROPOSAL)[1]
                if row["family"] in FAMILIES}
    parents = {parent(row) for row in read_rows(ATLAS / "atlas.csv")[1]}
    eligible = {regime(row) for row in read_rows(ATLAS / "modifier-atlas.csv")[1]}
    seen = set()
    anchors = {}
    for row in rows:
        identity = key(row)
        assert identity in proposed, ("not proposed", identity)
        assert identity not in seen, ("duplicate", identity)
        seen.add(identity)
        assert parent(row) in parents, ("missing parent", identity)
        assert regime(row) in eligible, ("ineligible", identity)
        assert row["origin"] == "TASK-469", identity
        assert row["status"] == "written", identity
        assert row["direction"] in {"iff", "exact", "one-sided"}, identity
        for ref in row["evidence"].split("; "):
            filename, separator, anchor = ref.partition("#")
            path = (ROOT / filename).resolve()
            assert separator and anchor and path.is_relative_to(ROOT) and path.is_file(), ref
            if path not in anchors:
                anchors[path] = proof_anchors(path)
            assert anchor in anchors[path], ref
    assert seen == proposed, ("missing reviewed rows", sorted(proposed - seen))
    print(f"{len(seen)} reviewed rows with eligible unique keys and valid evidence anchors")


if __name__ == "__main__":
    main()
