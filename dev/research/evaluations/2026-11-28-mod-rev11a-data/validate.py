"""Validate TASK-470 join keys and evidence, and replay exact count witnesses."""

import csv
from fractions import Fraction as Q
from functools import cache
from pathlib import Path
import re

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
PROPOSALS = DATA.with_name("2026-11-28-trunc-est2-math-data") / "proofs.csv"
FAMILIES = frozenset("bbinom bnbinom cmb cmp hyper lgser nhyper pig yulesimon".split())
FIELDS = "family held modifier question claim direction status evidence frontier origin".split()


def read(path):
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def mask(row):
    held = row["held"]
    return row["family"], "" if held == "all free" else held


def key(row):
    return (*mask(row), row["modifier"], row["question"])


@cache
def anchors(path):
    content = (ROOT / path).read_text()
    titles = re.findall(r"^#{1,6} (.+)$", content, re.M)
    return {re.sub(r"[^a-z0-9_-]", "", title.lower().replace(" ", "-"))
            for title in titles}


def validate():
    parent_masks = {mask(row) for row in read(ATLAS / "atlas.csv")[1]}
    eligible = {key({**row, "question": ""})[:3]
                for row in read(ATLAS / "modifier-atlas.csv")[1]}
    known = {key(row) for row in read(ATLAS / "modifier-proofs.csv")[1]
             if row["modifier"] == "truncate:known"}
    proposals = [row for row in read(PROPOSALS)[1] if row["family"] in FAMILIES]
    assert len(proposals) == len({key(row) for row in proposals}) == 507
    for row in proposals:
        question = row["question"]
        if question != "case:bound-cardinality":
            assert (*mask(row), "truncate:known", question) in known, key(row)
        for ref in row["evidence"].split("; "):
            file, fragment = ref.split("#", 1)
            assert fragment in anchors(file), ref

    fields, reviewed = read(DATA / "reviewed.csv")
    assert fields == FIELDS
    seen = set()
    for row in reviewed:
        full_key = key(row)
        assert mask(row) in parent_masks and full_key[:3] in eligible, full_key
        assert full_key not in seen, full_key
        seen.add(full_key)
        assert row["origin"] == "TASK-468" and row["status"] == "written", full_key
        for ref in row["evidence"].split("; "):
            file, fragment = ref.split("#", 1)
            assert fragment in anchors(file), ref
    assert seen == {key(row) for row in proposals}
    print(f"PASS: {len(seen)} unique proposed keys, eligible parent masks and valid evidence anchors")


def witnesses():
    for alpha, beta in [(1, 1), (3, 2)]:
        assert Q(alpha + 1, 2 * beta) == 1
    empirical = [Q(10, 20), Q(6, 20), Q(4, 20)]
    for size, alpha, beta in [(1, 1, 2), (2, 1, 1)]:
        odds = [Q((k + size) * (k + beta), (k + 1) * (k + alpha + beta + size))
                for k in (1, 2)]
        assert odds == [b / a for a, b in zip(empirical, empirical[1:])]
    for z in (4, 5):
        v = 2 / (1 + Q(1, z))
        assert 0 < 2 * v < z and v * (1 + Q(1, z)) / 2 == 1
    # The conditional mass of one at r=b=t and a=1 is (t/(t+1))**2.
    for t in (1, 2, 20):
        ratio = Q(t * t, 2 * t + 1)
        assert ratio / (1 + ratio) == Q(t, t + 1)**2 < 1
    # This older cross-size CMB tie changes when the lower cut tightens.
    # At p in (0,1), design 2 gives a singleton and design 3 retains cell 3.
    for prob in (Q(1, 3), Q(1, 2), Q(2, 3)):
        weight_2 = 3 * prob**2 * (1 - prob)
        assert weight_2 / (weight_2 + prob**3) < 1
    print("PASS: exact count witness arithmetic")


if __name__ == "__main__":
    validate()
    witnesses()
