"""Check the report, its regime anchors and the proposed atlas rows."""

import csv
import re
from pathlib import Path


DATA = Path(__file__).resolve().parent
REPORT = DATA.with_name(DATA.name.removesuffix("-data") + "-report.md")
ATLAS = DATA.parent.parent / "existence-atlas"
ROOT = DATA.parents[3]


def read_csv(path):
    with path.open(newline="") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def slug(heading):
    return re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")


def headings(path):
    return {slug(h) for h in re.findall(r"^#{1,6} (.+)$", path.read_text(), re.M)}


def main():
    report = REPORT.read_text()
    assert all(ord(char) >= 32 or char in "\t\n" for char in report)
    assert report.count(r"\[") == report.count(r"\]")
    assert report.count(r"\(") == report.count(r"\)")
    sections = re.findall(r"^### (sstd|sged|ged) held (.+)$", report, re.M)
    assert len(sections) == 32
    expected = {(family, "" if held == "all free" else held) for family, held in sections}
    assert len(expected) == len(sections)

    schema, _ = read_csv(ATLAS / "proofs.csv")
    _, inventory = read_csv(ATLAS / "atlas.csv")
    proposed_schema, proposed = read_csv(DATA / "proofs.csv")
    assert schema == proposed_schema
    regimes = {(row["family"], "" if row["held"] == "all free" else row["held"]) for row in inventory}
    seen = set()
    for row in proposed:
        regime = row["family"], row["held"]
        assert regime in expected and regime not in seen
        seen.add(regime)
        assert regime in regimes and row["question"] == "uniqueness"
        assert row["status"] == "written" and row["origin"] == "TASK-414"
        assert row["direction"] in {"one-sided", "exact"}
        assert row["claim"] and bool(row["frontier"]) == (row["direction"] == "one-sided")
        target, fragment = row["evidence"].split("#")
        assert (ROOT / target).resolve() == REPORT
        assert fragment == slug(f"{regime[0]} held {regime[1] or 'all free'}")
        assert fragment in headings(REPORT)
    assert seen == expected

    for link in re.findall(r"\]\(([^)\s]+)\)", report):
        if "://" in link:
            continue
        target, _, fragment = link.partition("#")
        path = (REPORT.parent / target).resolve() if target else REPORT
        assert path.exists(), link
        if fragment and path.suffix == ".md":
            assert fragment in headings(path), link
    print(f"Validated {len(sections)} report sections, relative links and {len(proposed)} atlas proposals.")


if __name__ == "__main__":
    main()
