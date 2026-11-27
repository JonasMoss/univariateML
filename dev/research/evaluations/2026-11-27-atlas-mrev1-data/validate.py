"""Validate the proposed atlas join and reproduce the skew-t branch bounds."""
import csv
from collections import Counter
from fractions import Fraction
from pathlib import Path
import sys

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def validate_rows():
    with (ATLAS / "atlas.csv").open() as stream:
        regimes = {
            (row["family"], "" if row["held"] == "all free" else row["held"])
            for row in csv.DictReader(stream)
        }
    with (ATLAS / "proofs.csv").open() as stream:
        header = next(csv.reader(stream))
    seen = set()
    anchors = {}
    counts = Counter()
    with (DATA / "reviewed.csv").open() as stream:
        rows = csv.DictReader(stream)
        assert rows.fieldnames == header
        for row in rows:
            family, held, question = (row[name] for name in header[:3])
            key = family, held, question
            assert (family, held) in regimes, key
            assert question in {"criterion", "failure", "uniqueness"}, key
            assert key not in seen, key
            seen.add(key)
            filename, anchor = row["evidence"].split("#", 1)
            if filename not in anchors:
                anchors[filename] = proof_anchors(ROOT / filename)
            assert anchor in anchors[filename], row["evidence"]
            assert row["origin"] in {"TASK-401", "TASK-402", "TASK-403", "TASK-406"}
            counts[row["direction"], row["status"]] += 1
    print(f"{len(seen)} unique atlas keys with valid evidence anchors")
    for (direction, status), count in sorted(counts.items()):
        print(f"{direction}, {status}: {count}")


def check_scale_crossing():
    """Check rational constants in the corrected branch comparison."""
    p = 100
    slant = 2**20
    remote = 2**50000
    low = slant * 2**10
    high = slant * 2**100
    early = slant // 8
    one_percent = Fraction(1, 100)
    # Bound the five factors apart from the skew gain at residual one.
    assert p * Fraction(1, early**2) < one_percent
    assert p * Fraction(early, low)**2 < one_percent
    assert p * Fraction(1, slant**2) < one_percent
    assert Fraction(early, remote)**2 < one_percent
    assert Fraction(1, slant**2) < one_percent
    assert (Fraction(1, 64) + Fraction(1, slant**2)) / 4 < Fraction(1, 250)
    early_lower = early * 2**p * (1 - Fraction(p, 250)) * (1 - one_percent)**5
    assert early_lower > Fraction(high, 16)
    assert (2**50 * slant) * Fraction(9, 8)**p < Fraction(high, 16)
    assert (1 + Fraction(1, 2**50))**p < Fraction(5, 4)
    print("Corrected skew-t scale objective: exact branch separation bounds passed")


if __name__ == "__main__":
    validate_rows()
    check_scale_crossing()
