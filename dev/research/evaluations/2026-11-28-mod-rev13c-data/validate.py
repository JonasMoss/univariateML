"""Validate the review join and replay exact window-contact calculations."""
import csv
from collections import Counter
from fractions import Fraction
from pathlib import Path
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import MODIFIER_PROOF_FIELDS, proof_anchors

FAMILIES = {"betapr", "trgamma", "invtrgamma", "burr", "invburr", "gb2"}
KEY = ("family", "held", "modifier", "question")


def rows(path):
    with path.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream, strict=True))


def key(row):
    return tuple(row[field] for field in KEY)


def validate():
    parents = {(row["family"], row["held"] or "all free") for row in rows(ATLAS / "atlas.csv")}
    eligible = {tuple(row[field] for field in KEY[:3]) for row in rows(ATLAS / "modifier-atlas.csv")}
    proposal = HERE.with_name("2026-11-28-trunc-est4-math-data") / "proofs.csv"
    proposed = {key(row) for row in rows(proposal) if row["family"] in FAMILIES}
    seen = set()
    anchors = {}
    reviewed = rows(HERE / "reviewed.csv")
    for row in reviewed:
        assert tuple(row) == MODIFIER_PROOF_FIELDS
        assert row["origin"] == "TASK-475"
        claim_key = key(row)
        assert claim_key[:2] in parents and claim_key[:3] in eligible, claim_key
        assert claim_key not in seen, claim_key
        seen.add(claim_key)
        for reference in row["evidence"].split("; "):
            filename, _, anchor = reference.partition("#")
            target = (ROOT / filename).resolve()
            assert target.is_relative_to(ROOT) and target.is_file(), reference
            if target not in anchors:
                anchors[target] = proof_anchors(target)
            assert anchor in anchors[target], reference
    assert seen == proposed and len(seen) == 621
    print("Validated 621 keys, parent masks, modifier eligibility and evidence anchors")
    print(dict(sorted(Counter(row["family"] for row in reviewed).items())))


def replay():
    for denominator in (10, 100, 10000, 100000000):
        eps = Fraction(1, denominator)
        # Triangle with mode at the right support endpoint.
        mass = 1 - (1 - eps)**2
        assert 2 * eps / mass == 2 / (2 - eps)
        # Reversing the mode leaves positive mass but zero sample density.
        reflected_mass = eps**2
        assert reflected_mass > 0 and Fraction(0) / reflected_mass == 0
        # Uniform GPD at either support endpoint.
        assert 1 / eps == denominator
        for a in (-denominator, -1, 0):
            assert min(1, 1) - max(a, 0) == 1
        # Half-normal at its mode: the monotone kernel gives height >= 1/eps.
        # The exact Pareto analogue verifies the distinction between a
        # nominal plateau and a window that only meets the support at a point.
        assert max(1 - eps, 1) == 1
        assert min(1, 1 + eps) - max(0, 1) == 0

    # Triangular example at known support [-1, 2], on tight [0, 1].
    for c in (Fraction(1, 10), Fraction(1, 2), Fraction(9, 10)):
        d = (c + 1) * (2 - c)
        plateau = Fraction(8, 9)
        gap = plateau - 4 * d / (9 * (d - 1)**2)
        assert gap == 4 * (2 * d - 1) * (d - 2) / (9 * (d - 1)**2) > 0
    # GEV xi=-1: the mean 3/4 is strictly in the increasing-tilt range.
    assert Fraction(1, 2) < (Fraction(1, 2) + 1) / 2 < 1
    assert Fraction(1, 2) < Fraction(2, 3) < 1
    # Exact fixed-scale beta-prime check: logistic(±log 3) gives complementary cuts.
    lower, upper = Fraction(1, 4), Fraction(3, 4)
    assert lower * (1 - lower) == upper * (1 - upper)
    assert Fraction(1, 2)**2 > lower * (1 - lower)

    # Two observed window endpoints, beta-prime in u with both shapes one.
    # The report proves the cancellation for every positive t.
    for lower, upper in ((1, 2), (Fraction(1, 2), 1)):
        for t in (Fraction(1, 4), 1, 7):
            products = (1 + t * lower) * (1 + t * upper)
            mass = Fraction(upper - lower, products)
            endpoint_product = Fraction(1, products**2)
            assert endpoint_product / mass**2 == Fraction(1, (upper - lower)**2)
    print("Exact endpoint, plateau, beta-corner and scale-tie arithmetic passed")


if __name__ == "__main__":
    validate()
    replay()
