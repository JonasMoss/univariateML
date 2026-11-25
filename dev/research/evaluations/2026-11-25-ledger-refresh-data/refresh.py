"""Rebuild and check the dated ledger without editing the inventory."""
import csv
import re
import sys
from collections import Counter
from pathlib import Path

DATA = Path(__file__).resolve().parent
EVAL = DATA.parent
BASE = EVAL / "2026-11-24-claims-inventory-data" / "ledger.csv"
OUTPUT = DATA / "ledger-2026-11-25.csv"
FIELDS = ("global_backing", "uniqueness_basis")
GLOBAL_PROOF = {"closed-form", "proof-exact-comparison", "proof-practical-margin"}
BASES = {"universal-proof", "per-input-certificate", "typed-set"}
MATCH = ("family", "held", "status", "outcome", "path_class")


def read(path, delimiter=","):
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream, delimiter=delimiter)
        return reader.fieldnames, list(reader)


def matches(rule, row):
    if not all(rule[key] == "*" or row[key].replace(",", "+") in rule[key].split(",") for key in MATCH):
        return False
    if rule["report"] == "thresh-math" and row["outcome"] == "failure":
        return row["reason"] == "Unsupported"
    return True


def show_counts(label, estimates):
    print(label, len({r["family"] for r in estimates}), len(estimates))


def summarize(label, rows):
    print(label, "all rows", len(rows), "families", len({r["family"] for r in rows}))
    print("global backing", dict(sorted(Counter(r["global_backing"] for r in rows).items())))
    estimates = [r for r in rows if r["outcome"] == "estimate"]
    proven = [r for r in estimates if r["global_backing"] in GLOBAL_PROOF]
    show_counts("global + singleton or known set", [r for r in proven if r["uniqueness_basis"] in BASES])
    show_counts("global + singleton", [r for r in proven if r["uniqueness_basis"] in BASES - {"typed-set"}])
    for basis in sorted(BASES):
        show_counts(basis, [r for r in estimates if r["uniqueness_basis"] == basis])
        show_counts("  with global backing", [r for r in proven if r["uniqueness_basis"] == basis])
    default = [r for r in estimates if "default" in r["unique"] and r["uniqueness_basis"] == "not-addressed"]
    show_counts("default true requiring evidence", default)
    print("default true requiring evidence (distinct return alternatives)",
          len({(r["outcome_source"], r["path_class"]) for r in default}))


def check_report():
    path = EVAL / "2026-11-25-ledger-refresh-report.md"
    report = path.read_text(encoding="utf-8")
    assert re.findall(r"^## (.+)$", report, re.M)[-1] == "Open"
    today = sorted(EVAL.glob("2026-11-25-*math-report.md"))
    today.append(EVAL / "2026-11-25-sn-ties-report.md")
    assert all(source.name in report for source in today)
    for ref in re.findall(r"\]\(([^\s)]+)\)", report):
        target, _, fragment = ref.partition("#")
        destination = path.parent / target
        assert destination.exists(), ref
        if fragment and destination.suffix == ".md":
            headings = re.findall(r"^#{1,6} (.+)$", destination.read_text(), re.M)
            anchors = {re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")
                       for heading in headings}
            assert fragment in anchors, ref
    print(f"Report checked against {len(today)} source reports and all local links")


def main():
    fields, original = read(BASE)
    _, rules = read(DATA / "evidence-rules.csv", "|")
    sources = []
    for rule in rules:
        assert None not in rule and all(value is not None for value in rule.values()), rule
        path = EVAL / f"2026-11-25-{rule['report']}-report.md"
        headings = re.findall(r"^#{1,6} (.+)$", path.read_text(), re.M)
        assert sum(title == rule["section"] for title in headings) == 1, rule
        sources.append(f"{path.name} § {rule['section']} — {rule['scope']}")

    rows = []
    hits = Counter()
    for old in original:
        row = dict(old, evidence_source="")
        assignments = {}
        evidence = []
        for index, (rule, source) in enumerate(zip(rules, sources), 2):
            if matches(rule, old):
                hits[index] += 1
                for field in FIELDS:
                    if rule[field]:
                        assert assignments.setdefault(field, rule[field]) == rule[field], (index, old)
                evidence.append(source)
        row.update(assignments)
        row["evidence_source"] = " || ".join(evidence)
        rows.append(row)

    assert len(rows) == 1514 == len(original)
    assert all(all(old[k] == new[k] for k in fields if k not in FIELDS)
               for old, new in zip(original, rows))
    assert all(new["evidence_source"] for old, new in zip(original, rows)
               if any(old[k] != new[k] for k in FIELDS))
    if "--diagnose" in sys.argv:
        print("Rules with no matching rows:", sorted(set(range(2, len(rules) + 2)) - hits.keys()))
    else:
        assert len(hits) == len(rules), "One or more evidence rules did not match an inventory row"

    output_fields = fields + ["evidence_source"]
    if "--check" in sys.argv:
        actual_fields, actual = read(OUTPUT)
        assert actual_fields == output_fields and actual == rows
    else:
        with OUTPUT.open("w", newline="", encoding="utf-8") as stream:
            writer = csv.DictWriter(stream, fieldnames=output_fields, lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)

    summarize("Before", original)
    summarize("After", rows)
    changed = [row for old, row in zip(original, rows) if any(old[k] != row[k] for k in FIELDS)]
    print("Rows with a backing or basis change:", len(changed))
    print("Rows with evidence:", sum(bool(r["evidence_source"]) for r in rows))
    print("Sources on rows:", len({piece.split(" § ")[0] for r in rows
                                    for piece in r["evidence_source"].split(" || ") if piece}))
    if "--report" in sys.argv:
        check_report()


if __name__ == "__main__":
    main()
