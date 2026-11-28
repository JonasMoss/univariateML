"""Check review coverage, atlas keys, evidence anchors and exact examples."""
import csv
import importlib.util
import os
import sys
from collections import Counter
from fractions import Fraction as Q
from functools import lru_cache
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
EVALUATIONS = HERE.parent
ATLAS = EVALUATIONS.parent / "existence-atlas"
FAMILIES = set(
    "gengamma pareto3 pgw fatigue expweibull naka foldnorm invpareto "
    "invparalogis paralogis sgomp".split()
)
FIELDS = (
    "family held modifier question claim direction status evidence frontier origin".split()
)
KEY = FIELDS[:4]


def read(path):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def key(row):
    return tuple(row[field] for field in KEY)


def exact_checks():
    # Inverse Pareto, x=(1,2,2), S=1, tight [1,2].
    z1, z2 = Q(1, 2), Q(2, 3)
    assert (z1 * z2**2)**2 > (z1 * z2)**3 > ((1 - z1) * (1 - z2)**2)**2

    for power in (1, 2, 4, 8):
        eps = Q(1, 10**power)
        # Held triangular with mode at the upper support end, and held uniform GPD.
        left = 1 - eps
        triangular_mass = 1 - left**2
        uniform_mass = 1 - left
        assert (2 / triangular_mass) * eps == 2 / (2 - eps)
        assert 1 / uniform_mass == 10**power
        # Pareto III at held d=1, s=p=1, x=(1), upper-estimated.
        mass = 1 - 1 / (1 + eps)
        assert 1 / mass == 1 + 1 / eps

    # Uniform GPD at x=0 with the fixed upper cut at 1.
    for lower in (Q(-3), Q(-1), Q(0)):
        assert 1 / (1 - max(lower, Q(0))) == 1

    # Pareto III at p=1, lower-estimated and B=infinity.
    # t=scale-threshold > 0 with threshold <= 0.  For x=(0,1,1),
    # L=t/(1+t)^4.  The next identity bounds it at every t via a square.
    for t in (Q(1, 10), Q(1, 3), Q(1, 2), Q(1), Q(4)):
        assert 27 * (1 + t)**4 - 256 * t == (3 * t - 1)**2 * (3 * t * t + 14 * t + 27)
    for threshold, scale in ((Q(0), Q(1, 3)), (Q(-1, 6), Q(1, 6))):
        t = scale - threshold
        assert t == Q(1, 3)
        assert t / (1 + t)**4 == Q(27, 256)


def main():
    spec = importlib.util.spec_from_file_location(
        "existence_atlas", ROOT / "dev/tools/existence_atlas.py"
    )
    atlas_tool = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(atlas_tool)
    anchors = lru_cache(maxsize=None)(atlas_tool.proof_anchors)
    header, proposed = read(EVALUATIONS / "2026-11-28-trunc-est4-math-data/proofs.csv")
    fields, reviewed = read(HERE / "reviewed.csv")
    assert fields == header == FIELDS
    expected = {
        key({**row, "held": row["held"] or "all free"})
        for row in proposed if row["family"] in FAMILIES
    }
    assert len(expected) == 690
    parent_keys = {(r["family"], r["held"]) for r in read(ATLAS / "atlas.csv")[1]}
    eligible = {
        (r["family"], r["held"], r["modifier"])
        for r in read(ATLAS / "modifier-atlas.csv")[1]
    }
    keys = set()
    for row in reviewed:
        joined = key(row)
        assert joined not in keys, joined
        keys.add(joined)
        assert joined[:2] in parent_keys, joined
        assert joined[:3] in eligible, joined
        assert row["origin"] == "TASK-475" and row["status"] == "written", joined
        assert row["claim"] and row["evidence"], joined
        for reference in row["evidence"].split("; "):
            file, anchor = reference.split("#", 1)
            path = ROOT / file
            assert path.is_file() and anchor in anchors(path), reference
    assert keys == expected
    exact_checks()
    print(f"{len(keys)} eligible unique rows, all evidence anchors and exact replays passed")
    print(dict(sorted(Counter(row["direction"] for row in reviewed).items())))


if __name__ == "__main__":
    os.chdir(ROOT)
    main()
