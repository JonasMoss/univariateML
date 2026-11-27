"""Validate the coordinator's TASK-439 import file against the parent atlas."""
import csv
import re
from collections import Counter
from pathlib import Path

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
PROPOSED = DATA.with_name("2026-11-27-trunc-expfam-math-data") / "proofs.csv"
KEY = ("family", "held", "modifier", "question")
ZERO_ELIGIBLE = {"bbinom", "binom", "bnbinom", "cmb", "cmp", "dgamma",
                 "dweibull", "geom", "nbinom", "pig", "pois"}


def read(path):
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def key(row):
    return tuple(row[field] for field in KEY)


def parent_key(row):
    # The proof inputs use an empty mask, while the atlas displays "all free".
    return row["family"], row["held"] or "all free"


def anchors(path):
    headings = re.findall(r"^#{1,6} (.+)$", path.read_text(), re.MULTILINE)
    return {re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")
            for heading in headings}


def main():
    fields, proposed = read(PROPOSED)
    incoming_fields, reviewed = read(DATA / "reviewed.csv")
    assert incoming_fields == fields
    _, regimes = read(ROOT / "dev/research/existence-atlas/atlas.csv")
    parents = {parent_key(row) for row in regimes}
    proposed_keys = {key(row) for row in proposed}
    assert len(proposed_keys) == len(proposed) == 207
    seen = set()
    evidence = {}
    for row in reviewed:
        k = key(row)
        assert k in proposed_keys and k not in seen, k
        seen.add(k)
        assert parent_key(row) in parents, k
        if row["modifier"].startswith("zero:"):
            assert row["family"] in ZERO_ELIGIBLE, k
        else:
            assert row["modifier"].startswith("truncate:"), k
        assert row["origin"] == "TASK-439", k
        assert row["status"] == "written", k
        file, separator, fragment = row["evidence"].partition("#")
        path = ROOT / file
        assert path.is_file() and separator and fragment, k
        if path not in evidence:
            evidence[path] = anchors(path)
        assert fragment in evidence[path], (k, row["evidence"])
    counts = Counter(row["modifier"] for row in reviewed)
    print(f"{len(reviewed)} valid unique rows of {len(proposed)} proposed")
    for modifier, count in sorted(counts.items()):
        print(f"{modifier}: {count}")


if __name__ == "__main__":
    main()
