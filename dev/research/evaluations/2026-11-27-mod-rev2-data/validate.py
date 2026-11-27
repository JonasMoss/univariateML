"""Validate the TASK-443 join file and exact checks for two boundary examples."""
import csv
from collections import Counter
from fractions import Fraction
from pathlib import Path
import re

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
FIELDS = (
    "family", "held", "modifier", "question", "claim", "direction", "status",
    "evidence", "frontier", "origin",
)
KEY = FIELDS[:4]
MODIFIERS = {"zero:truncated", "zero:modified", "zero:inflated"}


def rows(path):
    with path.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def proof_key(row):
    return tuple(row[field] for field in KEY)


def heading_anchors(path):
    headings = re.findall(r"^#{1,6} (.+)$", path.read_text(), re.MULTILINE)
    return {re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")
            for heading in headings}


def exponential_bounds(x):
    term = total = Fraction(1)
    for k in range(1, 20):
        term *= x / k
        total += term
    next_term = term * x / 20
    return total, total + next_term / (1 - x / 21)


def main():
    atlas = rows(ROOT / "dev/research/existence-atlas/atlas.csv")
    parents = {(row["family"], "" if row["held"] == "all free" else row["held"])
               for row in atlas}
    source = rows(DATA.parent / "2026-11-27-zero-mod-math-data/proofs.csv")
    original = {proof_key(row): row for row in source}
    reviewed = rows(DATA / "reviewed.csv")
    assert len(original) == len(source) == 249
    assert all(key[:2] in parents and key[2] in MODIFIERS for key in original)
    dossiers = ROOT / "univariateML-cpp/include/cppunivariate/distributions"
    for family in {key[0] for key in original}:
        dossier = (dossiers / family / "README.md").read_text().split("---", 2)[1]
        assert re.search(r'^support: \{type: integer, lower: "?0"?,', dossier, re.M)
        assert not re.search(r"^- (truncation|modification)$", dossier, re.M)
    assert len({proof_key(row) for row in reviewed}) == len(reviewed)
    anchors = {}
    counts = Counter()
    for row in reviewed:
        assert tuple(row) == FIELDS and None not in row.values()
        key = proof_key(row)
        assert key in original and key[:2] in parents, key
        assert row["modifier"] in MODIFIERS and row["origin"] == "TASK-440", key
        filename, separator, anchor = row["evidence"].partition("#")
        path = ROOT / filename
        assert separator and path.is_file(), row["evidence"]
        if path not in anchors:
            anchors[path] = heading_anchors(path)
        assert anchor in anchors[path], row["evidence"]
        counts["accepted" if row == original[key] else "regraded"] += 1
    counts["rejected"] = len(source) - len(reviewed)
    print(f"{len(source)} proposals, {len(reviewed)} join rows, {dict(counts)}")

    # One positive root of exp(mu) = 1 + 2 mu by strict convexity.
    # Bound the remaining Taylor terms by a geometric series.
    lo, hi = Fraction(12564312, 10**7), Fraction(12564313, 10**7)
    assert exponential_bounds(lo)[1] < 1 + 2 * lo
    assert exponential_bounds(hi)[0] > 1 + 2 * hi
    print(f"Exact PIG tie root bracket: ({float(lo)}, {float(hi)})")

    # The symmetric normal has zero linear tilt. At quadratic tilt zero,
    # the score is -gap. For any tilt, Var(X^2) <= 1/4 on [-1,1].
    # The Taylor lower bound certifies an interior witness beating uniform.
    gap = Fraction(1, 3) - Fraction(57735, 100000)**2
    tilt = -4 * gap
    assert gap > 0 and tilt < 0
    assert -tilt * gap - tilt**2 / 8 == 2 * gap**2 > 0
    # On [-1/100, 0], the tilted density is at least 99/100 times
    # uniform, hence its variance is at least that fraction of 4/45.
    curvature = Fraction(99, 100) * Fraction(4, 45)
    assert 0 < gap < curvature / 100  # The score changes sign in this bracket.
    assert gap**2 / (2 * curvature) < Fraction(1, 10**8)
    print(f"Exact normal inward score at the uniform edge: {gap}")


if __name__ == "__main__":
    main()
