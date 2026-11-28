"""Validate the TASK-471 review and its proposed join keys."""
from collections import Counter
import csv
from functools import cache
from pathlib import Path
import re
import sys

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
REPORT = DATA.with_name("2026-11-28-mod-rev11b-report.md")
PROPOSAL = DATA.with_name("2026-11-28-trunc-est2-math-data") / "proofs.csv"
FAMILIES = set("std dnorm skellam laplace logis dweibull hcauchy hnorm "
               "dlaplace dunif cauchy gumbel unif".split())
KEY = ("family", "held", "modifier", "question")
FIELDS = (*KEY, "claim", "direction", "status", "evidence", "frontier", "origin")

sys.path.insert(0, str(ROOT / "dev/tools"))
from existence_atlas import proof_anchors


def read(path, fields=None):
    with path.open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream, strict=True)
        if fields:
            assert tuple(reader.fieldnames) == fields, path
        rows = list(reader)
    assert all(None not in row and None not in row.values() for row in rows), path
    return rows


def key(row):
    return tuple(row[field] for field in KEY)


@cache
def anchors(path):
    assert path.is_file() and path.is_relative_to(ROOT), path
    return proof_anchors(path)


def main():
    proposals = [r for r in read(PROPOSAL, FIELDS) if r["family"] in FAMILIES]
    submission = read(DATA / "reviewed.csv", FIELDS)
    source = {key(r): r for r in proposals}
    reviewed = {key(r): r for r in submission}
    assert len(source) == len(proposals) == 552
    assert len(reviewed) == len(submission) and reviewed.keys() <= source.keys()

    parents = {(r["family"], r["held"]) for r in read(ATLAS / "atlas.csv")}
    modifiers = {(r["family"], r["held"], r["modifier"])
                 for r in read(ATLAS / "modifier-atlas.csv")}
    fixed = {(r["family"], r["held"]) for r in read(ATLAS / "modifier-proofs.csv")
             if r["modifier"] == "truncate:known"}
    for family, held, modifier, _ in source:
        assert (family, held or "all free") in parents
        assert (family, held or "all free", modifier) in modifiers
        assert (family, held) in fixed

    decisions = {}
    for item, before in source.items():
        after = reviewed.get(item)
        if after is None:
            decisions[item] = "rejected"
            continue
        assert after["origin"] == "TASK-468" and after["status"] == "written"
        assert after["direction"] in {"iff", "exact", "one-sided"}
        for ref in after["evidence"].split("; "):
            file, sep, anchor = ref.partition("#")
            assert sep and anchor in anchors((ROOT / file).resolve()), ref
        decisions[item] = ("accepted" if after == (before | {"origin": "TASK-468"})
                           else "regraded")

    report_rows = {}
    pattern = r"^\| (\w+) \| ([\w+ ]+) \| (\w+) \| ([\w:-]+) \| (accepted|regraded|rejected) \| (.+) \|$"
    for family, held, mode, question, verdict, reason in re.findall(
            pattern, REPORT.read_text(encoding="utf-8"), re.MULTILINE):
        item = (family, "" if held == "all free" else held, f"truncate:{mode}-estimated", question)
        assert item not in report_rows and reason.endswith("."), item
        report_rows[item] = verdict
    assert report_rows == decisions
    print(f"{len(reviewed)}/{len(source)} rows validated")
    print(dict(Counter(decisions.values())))
    print(dict(Counter(r["direction"] for r in submission)))


if __name__ == "__main__":
    main()
