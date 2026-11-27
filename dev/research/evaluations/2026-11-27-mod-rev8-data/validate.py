"""Validate the reviewed TASK-457 modifier rows against the local atlas."""

import csv
import re
from pathlib import Path


DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
FIELDS = "family held modifier question claim direction status evidence frontier origin".split()


def read(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))


def key(row):
    held = row["held"]
    return row["family"], "" if held == "all free" else held


def anchor(heading):
    return re.sub(r"[^a-z0-9 -]", "", heading.lower()).replace(" ", "-")


def validate():
    parent = {key(row) for row in read(ATLAS / "atlas.csv")}
    eligible = {(*key(row), row["modifier"])
                for row in read(ATLAS / "modifier-atlas.csv")}
    proposals = {(*key(row), row["modifier"], row["question"])
                 for row in read(DATA.with_name("2026-11-27-trunc-discrete-math-data") /
                                 "proofs.csv")}
    with (DATA / "reviewed.csv").open(newline="") as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == FIELDS
        rows = list(reader)
    seen = set()
    anchors = {}
    for row in rows:
        regime = (*key(row), row["modifier"])
        full_key = (*regime, row["question"])
        assert key(row) in parent and regime in eligible, full_key
        assert full_key in proposals and full_key not in seen, full_key
        assert row["origin"] == "TASK-457" and row["status"] == "written", full_key
        seen.add(full_key)
        for ref in row["evidence"].split("; "):
            relative, heading = ref.split("#")
            if relative not in anchors:
                content = (ROOT / relative).read_text()
                anchors[relative] = {
                    anchor(title) for title in re.findall(r"^#{1,6} (.+)$", content, re.M)
                }
            assert heading in anchors[relative], ref
    print(f"{len(rows)} rows, {len({key(row) for row in rows})} eligible masks, "
          f"{len(proposals - seen)} unjoined proposal keys.")


if __name__ == "__main__":
    validate()
