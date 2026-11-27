"""Verify the held-mask submission schema, coverage and proof anchors."""

import csv
from pathlib import Path
import re

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
REPORT = DATA.with_name(DATA.name.removesuffix("-data") + "-report.md")

MASKS = {
    "emg": ("lambda", "lambda+mu", "mu", "mu+sigma", "sigma"),
    "ht": ("mu", "mu+sigma", "nu+sigma", "sigma"),
    "huber": ("mu", "mu+sigma", "sigma"),
    "fatigue": ("alpha", "alpha+beta", "beta"),
    "gpd": ("mu+sigma", "sigma", "xi"),
    "fpareto": ("min", "min+shape1", "min+shape3"),
}
DIRECTIONS = {"criterion": "iff", "failure": "exact", "uniqueness": "exact"}
EXACT_UNIQUENESS = {
    ("ht", "nu+sigma"),
    ("huber", "sigma"),
    ("gpd", "mu+sigma"),
    ("gpd", "sigma"),
    ("gpd", "xi"),
}


def read_csv(path):
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def anchors(path):
    output = set()
    for line in path.read_text().splitlines():
        output.update(re.findall(r"""\b(?:id|name)=["']([^"']+)["']""", line))
        if match := re.match(r"^#{1,6}\s+(.+?)\s*$", line):
            output.add(re.sub(r"[^\w -]", "", match[1].lower()).replace(" ", "-"))
        if path.suffix == ".tex":
            output.update(re.findall(r"\\label\{([^}]+)\}", line))
    return output


def main():
    fields, rows = read_csv(DATA / "proofs.csv")
    upstream_fields, _ = read_csv(ATLAS / "proofs.csv")
    assert fields == upstream_fields
    _, regimes = read_csv(ATLAS / "atlas.csv")
    atlas = {
        (row["family"], "" if row["held"] == "all free" else row["held"]): row
        for row in regimes
    }
    expected = {
        (family, held, question)
        for family, masks in MASKS.items()
        for held in masks
        for question in DIRECTIONS
    }
    expected.add(("huber", "", "uniqueness"))
    report_anchors = anchors(REPORT)
    report_text = REPORT.read_text()
    report_sections = {
        " ".join(title.lower().split()).replace("+", "").replace(" ", "-"): body
        for title, body in re.findall(
            r"^### ([^\n]+)\n(.*?)(?=^#{1,3} |\Z)", report_text, re.M | re.S
        )
    }
    actual = set()
    for row in rows:
        family, held, question = (row[key] for key in ("family", "held", "question"))
        key = family, held, question
        assert key not in actual, key
        actual.add(key)
        assert atlas[family, held][question + "_status"] == "unproved", key
        assert row["status"] == "written" and row["claim"], key
        path, fragment = row["evidence"].split("#")
        direction = DIRECTIONS[question]
        if question == "uniqueness" and (family, held) not in EXACT_UNIQUENESS:
            direction = "one-sided"
            assert row["frontier"], key
            assert "Uniqueness open" in report_sections[fragment], key
        assert row["direction"] == direction, key
        assert row["origin"] == "TASK-407", key
        assert ROOT / path == REPORT and fragment in report_anchors, row["evidence"]
    assert actual == expected, (actual - expected, expected - actual)

    for target in re.findall(r"\]\(([^)\s]+)\)", report_text):
        if target.startswith("https://"):
            continue
        file, _, fragment = target.partition("#")
        path = REPORT.parent / file if file else REPORT
        assert path.is_file(), target
        if fragment.startswith("L") and fragment[1:].isdigit():
            line = int(fragment[1:])
            assert 1 <= line <= len(path.read_text().splitlines()), target
        elif fragment:
            assert fragment in anchors(path), target
    print(f"{len(rows)} atlas rows and all local report links verified")
    print(f"{len(EXACT_UNIQUENESS)} exact and "
          f"{sum(key[2] == "uniqueness" for key in expected) - len(EXACT_UNIQUENESS)} "
          "one-sided uniqueness rows verified")


if __name__ == "__main__":
    main()
