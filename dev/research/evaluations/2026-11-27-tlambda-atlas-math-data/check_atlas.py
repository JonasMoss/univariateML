"""Check the delivered atlas rows against the canonical masks and report."""
import csv
from pathlib import Path
import re

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
MASKS = {"lambda", "lambda+mu", "lambda+sigma", "mu", "sigma"}
QUESTIONS = {"criterion", "failure", "uniqueness"}


def read_csv(path):
    with path.open(newline="") as source:
        reader = csv.DictReader(source)
        return reader.fieldnames, list(reader)


def main():
    schema, _ = read_csv(ATLAS / "proofs.csv")
    _, atlas = read_csv(ATLAS / "atlas.csv")
    fields, rows = read_csv(DATA / "proofs.csv")
    assert fields == schema
    assert MASKS <= {row["held"] for row in atlas if row["family"] == "tlambda"}
    expected = {(held, question) for held in MASKS for question in QUESTIONS}
    assert len(rows) == len(expected)
    assert {(row["held"], row["question"]) for row in rows} == expected
    for row in rows:
        assert row["family"] == "tlambda" and row["status"] == "written"
        assert row["claim"] and row["origin"] == "TASK-406"
        path, anchor = row["evidence"].split("#")
        heading = f"### tlambda held {row['held']}"
        report = (ROOT / path).read_text()
        assert heading in report.splitlines()
        assert anchor == re.sub(r"[^a-z0-9 -]", "", heading[4:]).replace(" ", "-")
        direction = {"criterion": "iff", "failure": "exact"}.get(
            row["question"], "exact" if row["held"] == "lambda+mu" else "one-sided"
        )
        assert row["direction"] == direction
    print(f"{len(rows)} atlas rows validated against canonical masks and report anchors")


if __name__ == "__main__":
    main()
