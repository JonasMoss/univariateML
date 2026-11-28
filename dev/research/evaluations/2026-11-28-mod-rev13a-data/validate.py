"""Validate the review join keys and replay exact sample reductions."""
import csv
from fractions import Fraction as Q
from functools import cache
from pathlib import Path
import sys

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
SOURCE = DATA.with_name("2026-11-28-trunc-est4-math-data")
FAMILIES = {"dgamma", "fpareto", "genbeta", "gev"}
FIELDS = ("family", "held", "modifier", "question", "claim", "direction",
          "status", "evidence", "frontier", "origin")
sys.dont_write_bytecode = True
sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def read(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))


def key(row):
    family, held, modifier, question = (row[field] for field in FIELDS[:4])
    return family, held or "all free", modifier, question


@cache
def anchors(path):
    assert path.is_file(), path
    return proof_anchors(path)


def validate():
    proposed = [row for row in read(SOURCE / "proofs.csv") if row["family"] in FAMILIES]
    reviewed = read(DATA / "reviewed.csv")
    expected = {key(row) for row in proposed}
    parents = {(r["family"], r["held"]) for r in read(ATLAS / "atlas.csv")}
    eligible = {key(r | {"question": ""})[:3] for r in read(ATLAS / "modifier-atlas.csv")}
    assert len(proposed) == len(expected) == 604
    seen = set()
    for row in reviewed:
        k = key(row)
        assert tuple(row) == FIELDS and row["origin"] == "TASK-475", k
        assert k in expected and k not in seen, k
        assert k[:2] in parents and k[:3] in eligible, k
        seen.add(k)
        for reference in row["evidence"].split("; "):
            file, marker, anchor = reference.partition("#")
            assert marker and anchor in anchors(ROOT / file), (k, reference)
    return len(seen)


def exact_checks():
    # Null endpoint contacts despite a positive nominal width.
    for epsilon in (Q(1, 10), Q(1, 100), Q(1, 10000)):
        triangular_mass = 1 - (1 - epsilon)**2
        assert epsilon * 2 / triangular_mass == 2 / (2 - epsilon)
        # Uniform GPD(0, 1, -1) and generalized beta(1, 1, 1, 1).
        assert epsilon * (1 / epsilon) == 1
        # Genbeta with q=2 has mass epsilon**2 and zero endpoint density.
        assert 1 - (1 - epsilon**2) == epsilon**2
        assert 2 * (1 - Q(1)) / epsilon**2 == 0
        # Shifted Lomax at its closed threshold: CDF 1 - (1 + x)**(-2).
        assert epsilon * 2 / (1 - (1 + epsilon)**-2) == (
            2 * (1 + epsilon)**2 / (2 + epsilon)
        )
    # Held uniform GPD: every outward lower cut at threshold zero ties.
    for lower in (Q(-4), Q(-1), Q(0)):
        assert min(1, 1) - max(lower, 0) == 1

    # The Feller–Pareto sample (1, 2) on [1, infinity).
    for threshold in (Q(3, 4), Q(1)):
        scale = threshold - Q(1, 2)
        s = scale + 1 - threshold
        assert 4 * s / (1 + s)**3 == Q(16, 27)
    # Coefficients of 16(1+s)^3 - 108s and (2s-1)^2(4s+16).
    assert (16, 48 - 108, 48, 16) == (16, 4 - 64, 64 - 16, 16)

    # Normalize with y=log(x) on [0, log 2]. Only ratios to log 2
    # enter the endpoint mean comparisons for the exact examples.
    assert Q(1, 3) < Q(1, 2) < Q(2, 3) < 1
    # GEV reverse exponential fixtures on [0,1].
    assert Q(1, 2) < (Q(1, 2) + 1) / 2 < 1
    assert Q(1, 2) < Q(2, 3) < 1

    # Integer bounds and the two-cell Bernoulli upper bound for (2,2,3).
    assert Q(1) < Q(3, 2) <= Q(2)
    assert Q(3) <= Q(7, 2) < Q(4)
    odds = Q(1, 2)
    assert odds / (1 + odds)**3 == Q(4, 27)
    assert ((4**2 - 3**2) / Q(2)) / ((3**2 - 2**2) / Q(2)) == Q(7, 5)


if __name__ == "__main__":
    print(f"{validate()} join rows with valid parent masks, unique keys and proof anchors")
    exact_checks()
    print("Exact contact and sample comparisons passed")
