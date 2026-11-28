"""Validate the reviewed TASK-475 rows and replay exact endpoint identities."""

from collections import Counter
import csv
from fractions import Fraction
from functools import cache
from pathlib import Path
import sys

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
PROPOSED = DATA.with_name("2026-11-28-trunc-est4-math-data") / "proofs.csv"
FAMILIES = {"gpd", "nsbeta", "pareto4", "triang"}
KEY = ("family", "held", "modifier", "question")
FIELDS = (*KEY, "claim", "direction", "status", "evidence", "frontier", "origin")

sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def read(path):
    with path.open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream, strict=True)
        assert reader.fieldnames
        rows = list(reader)
        assert all(None not in row and None not in row.values() for row in rows), path
        return tuple(reader.fieldnames), rows


def key(row):
    return tuple(row[field] for field in KEY)


def regime(row):
    return row["family"], row["held"] or "all free", row["modifier"]


@cache
def anchors(path):
    assert path.is_file() and path.is_relative_to(ROOT), path
    return proof_anchors(path)


def replay():
    for denominator in (10, 100, 10000, 100000000):
        epsilon = Fraction(1, denominator)
        # At x=1 on [1-epsilon,2], the held triangle with mode 1
        # and held uniform GPD with xi=-1 retain only their upper slivers.
        triangle_mass = 1 - (1 - epsilon)**2
        assert 2 * epsilon / triangle_mass == 2 / (2 - epsilon)
        assert 1 / epsilon == denominator
        # A reflected triangle with mode 0 still has zero density at x=1.
        assert 2 * (1 - 1) / epsilon**2 == 0
        # Sample zero and fixed upper cut one: padding below zero is free.
        assert min(Fraction(1), 1) - max(-epsilon, 0) == 1

    # The triangular mode example uses D=2+c*(1-c) on 0<c<1.
    # Its positive gap factors as 4*(2*D-1)*(D-2)/(9*(D-1)**2).
    assert Fraction(2) / Fraction(3, 2)**2 == Fraction(8, 9)
    for mode in (Fraction(1, 10), Fraction(1, 2), Fraction(9, 10)):
        d = (mode + 1) * (2 - mode)
        gap = Fraction(8, 9) - 4 * d / (9 * (d - 1)**2)
        assert gap == 4 * (2 * d - 1) * (d - 2) / (9 * (d - 1)**2) > 0

    # Pareto IV with fixed scale 1, both shapes 1, at x=0.
    # A tight upper cut at threshold has no mass; outward expansion
    # retains epsilon/(1+epsilon), with numerator density 1.
    epsilon = Fraction(1, 100)
    assert 1 / (1 - 1 / (1 + epsilon)) == 101

    # Pareto IV p=1/2, h=1, s=1, contact threshold d=0.
    # Two admissible upper limits retain different masses and laws.
    survival = lambda root_excess: 1 / (1 + root_excess)
    assert (1 - survival(Fraction(1))) / (1 - survival(Fraction(2))) == Fraction(3, 4)

    # Positive-width but null GPD slice and a zero-density alternative.
    # xi=-1/2 and scale=1/2 gives support [0,1] and density 2*(1-x).
    assert 2 * (1 - 1) == 0
    assert 1 - (1 - epsilon**2) == epsilon**2


def main():
    fields, before = read(PROPOSED)
    assert fields == FIELDS
    proposed = {key(row): row for row in before if row["family"] in FAMILIES}
    assert len(proposed) == sum(row["family"] in FAMILIES for row in before) == 591
    fields, reviewed = read(DATA / "reviewed.csv")
    assert fields == FIELDS
    _, base = read(ATLAS / "atlas.csv")
    _, modifiers = read(ATLAS / "modifier-atlas.csv")
    _, known = read(ATLAS / "modifier-proofs.csv")
    parent_keys = {(row["family"], row["held"]) for row in base}
    modifier_keys = {regime(row) for row in modifiers}
    known_questions = {
        (row["family"], row["held"] or "all free", row["question"])
        for row in known if row["modifier"] == "truncate:known" and row["status"] == "written"
    }

    assert len({key(row) for row in reviewed}) == len(reviewed)
    assert {key(row) for row in reviewed} <= proposed.keys()
    tallies = Counter()
    for row in reviewed:
        assert regime(row)[:2] in parent_keys, row
        assert regime(row) in modifier_keys, row
        if row["question"] in {"criterion", "failure", "uniqueness"}:
            assert (*regime(row)[:2], row["question"]) in known_questions, row
        assert row["origin"] == "TASK-475"
        assert row["status"] == "written" and row["direction"] in {"iff", "exact", "one-sided"}
        for reference in row["evidence"].split("; "):
            filename, separator, anchor = reference.partition("#")
            assert separator and anchor in anchors((ROOT / filename).resolve()), reference
        original = proposed[key(row)] | {"origin": "TASK-475"}
        tallies["accepted" if row == original else "regraded"] += 1
    tallies["rejected"] = len(proposed) - len(reviewed)
    replay()
    print(f"{len(reviewed)}/{len(proposed)} rows validated, exact endpoint checks passed")
    print(dict(tallies))
    print(dict(Counter(row["direction"] for row in reviewed)))


if __name__ == "__main__":
    main()
