"""Validate the proposed atlas rows and relative report links."""

import csv
from pathlib import Path
import re


DATA = Path(__file__).resolve().parent
REPORT = DATA.parent / "2026-11-27-uniq-generic-math-report.md"
ROOT = DATA.parents[3]
ATLAS = DATA.parent.parent / "existence-atlas"


def read_csv(path):
    with path.open(newline="") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def headings(text):
    return {
        re.sub(r"[^\w -]", "", name.lower()).replace(" ", "-")
        for name in re.findall(r"^#{1,6} (.+)$", text, re.M)
    }


def main():
    text = REPORT.read_text()
    assert all(ord(char) >= 32 or char in "\t\n" for char in text)
    assert b"\r" not in REPORT.read_bytes()
    assert text.count(r"\[") == text.count(r"\]")
    assert text.count(r"\(") == text.count(r"\)")
    schema, inventory = read_csv(ATLAS / "proofs.csv")
    proposed_schema, proposed = read_csv(DATA / "proofs.csv")
    assert proposed_schema == schema
    keys = {(row["family"], row["held"], row["question"]) for row in inventory}
    expected = {
        ("std", "nu"): "exact",
        ("std", "nu+sd"): "one-sided",
        ("sstd", ""): "one-sided",
        ("gengamma", ""): "one-sided",
    }
    seen = set()
    for row in proposed:
        regime = row["family"], row["held"]
        assert regime not in seen and row["direction"] == expected[regime]
        seen.add(regime)
        assert (*regime, row["question"]) in keys and row["question"] == "uniqueness"
        assert row["status"] == "written" and row["origin"] == "TASK-412"
        assert row["claim"] and bool(row["frontier"]) == (row["direction"] == "one-sided")
        target, fragment = row["evidence"].split("#")
        assert (ROOT / target).resolve() == REPORT and fragment in headings(text)
    assert seen == set(expected)

    for link in re.findall(r"\]\(([^)\s]+)\)", text):
        if "://" in link:
            continue
        target, _, fragment = link.partition("#")
        path = (REPORT.parent / target).resolve() if target else REPORT
        assert path.is_file(), link
        if fragment and path.suffix == ".md":
            assert fragment in headings(path.read_text()), link
    print("Report links, math delimiters and four atlas rows validated.")


if __name__ == "__main__":
    main()
