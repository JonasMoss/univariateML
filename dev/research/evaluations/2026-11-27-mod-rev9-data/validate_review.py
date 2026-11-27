"""Validate the reviewed joins and check the triangular example exactly."""
import csv
from collections import Counter
from fractions import Fraction as Q
from pathlib import Path
import runpy

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
TOOLS = runpy.run_path(str(ROOT / "dev/tools/existence_atlas.py"))
FIELDS = (
    "family", "held", "modifier", "question", "claim", "direction",
    "status", "evidence", "frontier", "origin",
)


def records(path):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream, strict=True)
        return tuple(reader.fieldnames), list(reader)


def mask(row):
    return row["family"], row["held"] or "all free"


def validate_rows():
    parents = {mask(row) for row in records(ATLAS / "atlas.csv")[1]}
    modifier_rows = records(ATLAS / "modifier-atlas.csv")[1]
    eligible = {(*mask(row), row["modifier"]) for row in modifier_rows}
    path = HERE / "reviewed.csv"
    fields, rows = records(path)
    source = HERE.parent / "2026-11-27-trunc-misc-math-data/proofs.csv"
    proposals = records(source)[1]
    assert fields == FIELDS
    _, errors = TOOLS["read_modifier_proofs"](path, modifier_rows)
    assert not errors, errors
    seen = set()
    anchors = {}
    for row in rows:
        regime = (*mask(row), row["modifier"])
        key = (*regime, row["question"])
        assert mask(row) in parents and regime in eligible, key
        assert key not in seen and row["origin"] == "TASK-462", key
        if row["direction"] == "open":
            assert row["status"] == "unproved" and row["frontier"], key
        seen.add(key)
        for ref in row["evidence"].split("; "):
            file, separator, anchor = ref.partition("#")
            target = (ROOT / file).resolve()
            assert separator and target.is_relative_to(ROOT) and target.is_file(), ref
            if target not in anchors:
                anchors[target] = TOOLS["proof_anchors"](target)
            assert anchor in anchors[target], ref
    proposed_keys = {(*mask(row), row["modifier"], row["question"]) for row in proposals}
    proposed_open = {
        (*mask(row), row["modifier"], row["question"])
        for row in proposals if row["direction"] == "open"
    }
    reviewed_open = {
        (*mask(row), row["modifier"], row["question"])
        for row in rows if row["direction"] == "open"
    }
    assert seen == proposed_keys and reviewed_open == proposed_open
    print(f"Validated {len(rows)} distinct rows and all evidence anchors")
    print(dict(Counter(row["direction"] for row in rows)))


def triangular_example():
    # Polynomials in the mode c, with coefficients in increasing order.
    def add(left, right):
        result = left.copy()
        for degree, coefficient in right.items():
            result[degree] = result.get(degree, 0) + coefficient
        return {degree: value for degree, value in result.items() if value}

    def multiply(left, right):
        result = {}
        for i, a in left.items():
            for j, b in right.items():
                result[i + j] = result.get(i + j, 0) + a * b
        return {degree: value for degree, value in result.items() if value}

    # When c <= 0 or c >= 1 the single affine branch has endpoint
    # heights (2, 1) or (1, 2) after dropping its common divisor.
    for first, last in ((2, 1), (1, 2)):
        integral = Q(first + last, 2)
        assert first * last / integral**2 == Q(8, 9)

    one = {0: Q(1)}
    left = {0: Q(1), 1: Q(1)}       # c + 1
    right = {0: Q(2), 1: Q(-1)}     # 2 - c
    d = multiply(left, right)
    # For 0 < c < 1, multiply each trapezoid integral by D.
    rising = multiply(multiply({1: Q(1, 2)}, add(one, left)), right)
    falling = multiply(multiply({0: Q(1, 2), 1: Q(-1, 2)}, add(one, right)), left)
    integral = add(rising, falling)
    assert integral == multiply({0: Q(3, 2)}, add(d, {0: Q(-1)}))
    # The endpoint product is 1/D, so J = D / integral**2.
    gap = add(multiply({0: Q(8, 9)}, multiply(integral, integral)),
              multiply({0: Q(-1)}, d))
    assert gap == multiply(add(multiply({0: Q(2)}, d), {0: Q(-1)}),
                           add(d, {0: Q(-2)}))
    assert add(d, {0: Q(-2)}) == multiply({1: Q(1)}, {0: Q(1), 1: Q(-1)})
    # Therefore the gap is strictly positive throughout 0 < c < 1.
    print("Exact triangular maximum 8/9 on [-1,0] union [1,2]")


if __name__ == "__main__":
    validate_rows()
    triangular_example()
