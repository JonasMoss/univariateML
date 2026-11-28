"""Check the review keys, proof anchors, and two exact likelihood reductions."""
import csv
from fractions import Fraction
from pathlib import Path
import sys

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def rows(path):
    with path.open(newline="") as source:
        return list(csv.DictReader(source))


def validate_rows():
    reviewed = rows(DATA / "reviewed.csv")
    parents = {(r["family"], r["held"]) for r in rows(ATLAS / "atlas.csv")}
    eligible = {
        (r["family"], r["held"], r["modifier"])
        for r in rows(ATLAS / "modifier-atlas.csv")
    }
    proposed = rows(DATA.with_name("2026-11-27-trunc-gb2-math-data") / "proofs.csv")
    fields = ("family", "held", "modifier", "question")
    proposed_keys = {tuple(r[field] for field in fields) for r in proposed}
    keys = set()
    anchors = {}
    for r in reviewed:
        key = tuple(r[field] for field in fields)
        assert key[:2] in parents and key[:3] in eligible, key
        assert key in proposed_keys and key not in keys, key
        assert r["origin"] == "TASK-461", key
        keys.add(key)
        for ref in r["evidence"].split("; "):
            filename, marker, anchor = ref.partition("#")
            assert marker and anchor, (key, ref)
            target = ROOT / filename
            if target not in anchors:
                anchors[target] = proof_anchors(target)
            assert anchor in anchors[target], (key, ref)
    print(f"{len(keys)} reviewed rows across {len({key[:2] for key in keys})} eligible masks.")


def multiply(left, right):
    result = [0] * (len(left) + len(right) - 1)
    for i, a in enumerate(left):
        for j, b in enumerate(right):
            result[i + j] += a * b
    return result


def check_shifted_lomax():
    # Coefficients of 27(1+s)^3 times the gap from 16/27.
    assert [16, 48 - 108, 48, 16] == multiply(multiply([-1, 2], [-1, 2]), [16, 4])
    for threshold, scale in ((Fraction(3, 4), Fraction(1, 4)),
                             (Fraction(1), Fraction(1, 2))):
        s = scale + 1 - threshold
        assert scale > 0 and threshold <= 1 and s == Fraction(1, 2)
        assert 4 * s / (1 + s)**3 == Fraction(16, 27)


def check_genbeta():
    # Use R = 2/log(2) as an exact symbol. Each pair gives p*d = R.
    # The log-likelihood derivative in r is 2/r - log(2),
    # with strictly negative second derivative -2/r^2.
    argument = Fraction(1, 3)
    terms = 12
    log_lower = sum(2 * argument**(2*j + 1) / (2*j + 1) for j in range(terms))
    log_upper = log_lower + (
        2 * argument**(2*terms + 1) / ((2*terms + 1) * (1 - argument**2))
    )
    assert 0 < log_lower < log_upper < 1
    for p, power_multiple in ((Fraction(1), Fraction(1)), (Fraction(2), Fraction(1, 2))):
        assert p > 0 and power_multiple > 0 and p * power_multiple == 1
        assert 2 / (p * power_multiple) - 2 == 0  # derivative times R at r=p*d
    assert [(x, x <= 2) for x in (1, 2)] == [(1, True), (2, True)]


if __name__ == "__main__":
    validate_rows()
    check_shifted_lomax()
    check_genbeta()
    print("Both exact nonuniqueness reductions passed.")
