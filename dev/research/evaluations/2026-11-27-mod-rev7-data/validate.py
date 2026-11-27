"""Check the TASK-460 join file against the proposals and atlas."""
import csv
import re
from pathlib import Path

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
PROPOSALS = DATA.with_name("2026-11-27-trunc-skew-math-data") / "proofs.csv"
FIELDS = "family held modifier question claim direction status evidence frontier origin".split()


def read(path):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def regime(row):
    return row["family"], "" if row["held"] == "all free" else row["held"]


def key(row):
    return *regime(row), row["modifier"], row["question"]


def anchors(path):
    headings = re.findall(r"^#{1,6} +(.+?)\s*$", path.read_text(), re.MULTILINE)
    return {re.sub(r"[^\w -]", "", title.lower()).replace(" ", "-") for title in headings}


def main():
    fields, reviewed = read(DATA / "reviewed.csv")
    assert fields == FIELDS
    proposed = {key(row) for row in read(PROPOSALS)[1]}
    parents = {regime(row) for row in read(ATLAS / "atlas.csv")[1]}
    eligible = {(*regime(row), row["modifier"])
                for row in read(ATLAS / "modifier-atlas.csv")[1]}
    seen = set()
    for row in reviewed:
        row_key = key(row)
        assert row_key not in seen and row_key in proposed, row_key
        assert regime(row) in parents and row_key[:3] in eligible, row_key
        assert row["modifier"] == "truncate:known" and row["origin"] == "TASK-458", row_key
        assert row["direction"] in {"iff", "exact", "one-sided"}, row_key
        assert row["status"] == "written" and row["claim"], row_key
        for evidence in row["evidence"].split("; "):
            file, separator, anchor = evidence.partition("#")
            path = ROOT / file
            assert separator and path.is_file() and anchor in anchors(path), evidence
        seen.add(row_key)
    print(f"Validated {len(seen)} reviewed keys against {len(proposed)} proposals.")


if __name__ == "__main__":
    main()
