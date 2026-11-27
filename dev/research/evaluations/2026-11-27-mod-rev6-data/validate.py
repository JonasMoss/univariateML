"""Check review schema, eligible parent regimes, provenance and evidence."""
import csv
import re
from pathlib import Path

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
FIELDS = ("family held modifier question claim direction status evidence "
          "frontier origin").split()


def load(path):
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def regime(row):
    held = row["held"]
    return row["family"], "" if held == "all free" else held


def anchors(path):
    markdown = path.read_text(encoding="utf-8")
    headings = re.findall(r"^#{1,6} +(.+?)\s*$", markdown, re.MULTILINE)
    return {re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")
            for heading in headings}


def main():
    fields, reviewed = load(DATA / "reviewed.csv")
    assert fields == FIELDS
    parents = {regime(row) for row in load(ATLAS / "atlas.csv")[1]}
    eligible = {(*regime(row), row["modifier"])
                for row in load(ATLAS / "modifier-atlas.csv")[1]}
    proposal = DATA.with_name("2026-11-27-trunc-shape-math-data") / "proofs.csv"
    renamed = {"path:critical-contact": "case:critical-contact",
               "path:gumbel": "case:gumbel"}
    proposed_keys = {(*regime(row), row["modifier"],
                      renamed.get(row["question"], row["question"]))
                     for row in load(proposal)[1]}
    seen = set()
    for row in reviewed:
        parent = regime(row)
        key = (*parent, row["modifier"], row["question"])
        assert parent in parents and key[:3] in eligible, key
        assert row["modifier"] == "truncate:known", key
        assert key in proposed_keys and key not in seen, key
        seen.add(key)
        assert row["origin"] == "TASK-450" and row["status"] == "written", key
        assert row["direction"] in {"iff", "exact", "one-sided"}, key
        for evidence in row["evidence"].split("; "):
            file, delim, anchor = evidence.partition("#")
            path = ROOT / file
            assert delim and path.is_file() and anchor in anchors(path), evidence
    print(f"Validated {len(seen)} reviewed keys against {len(proposed_keys)} proposals.")


if __name__ == "__main__":
    main()
