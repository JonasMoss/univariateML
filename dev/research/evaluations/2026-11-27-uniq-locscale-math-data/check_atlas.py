"""Validate the TASK-415 report and its separate proposed proof inventory."""
import argparse
import csv
from pathlib import Path
import re


DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
REPORT = DATA.parent / "2026-11-27-uniq-locscale-math-report.md"
ATLAS = DATA.parent.parent / "existence-atlas"
EXACT = {("std", "nu+sd"), ("lst", "df+sigma"), ("cauchy", "scale")}
FRONTIERS = {
    "std": "Resolve remaining small-level or one-magnitude profiles and simplify the exceptional height-equality locus.",
    "lst": "Resolve remaining small-level profiles or critical face comparisons and classify the exceptional stationary-height equalities.",
    "st": "Resolve possible equal-gradient winners at smaller level counts and isolate every winner on exceptional samples.",
    "sn": "Settle smaller-level charts and count all winning reflected orbits.",
    "slash": "Order finite location branches at smaller level counts and resolve exact finite-height equalities.",
    "ht": "Give a pointwise equality or dominance rule for stationary shapes on heterogeneous samples.",
    "bhatt": "Resolve remaining pointwise equality of two stationary heights, including the normal seam.",
    "emg": "Order the stationary heights outside the singleton classes, with an exact equality predicate.",
    "huber": "Classify ties between different stationary cutoffs by exact height comparison.",
    "foldnorm": "Characterize exceptional equal heights of the stationary roots and the included half-normal.",
    "tlambda": "Resolve remaining small-level critical and support-contact comparisons and classify exceptional nonuniform components.",
}


def read_csv(path):
    with path.open(newline="") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def anchor(name):
    return re.sub(r"[^\w -]", "", name.lower()).replace(" ", "-")


def headings(text):
    return {anchor(name) for name in re.findall(r"^#{1,6} (.+)$", text, re.M)}


def regime(label):
    family, held = label.split(" ", 1)
    return family, held if held != "all free" else ""


def plain(text):
    """Translate the table's simple math markup to atlas text."""
    return (text.replace(r"\(", "").replace(r"\)", "")
            .replace(r"\ge", " ≥ ").replace(r"\le", " ≤ ")
            .replace(r"\lambda", "lambda").replace(r"\sigma", "sigma")
            .replace(r"\omega", "omega").replace(r"\nu", "nu"))


def make_claim(result):
    result = re.sub(r"at m ≥ (\d+)(?: (noncontacts|free levels))?",
                    lambda match: (f"with at least {match[1]} distinct "
                                   + ("noncontact levels" if match[2] == "noncontacts"
                                      else match[2] or "observation levels")), result)
    if result.startswith("Generic winning cutoff"):
        return "The winning cutoff is unique for almost every attaining sample" + result.removeprefix("Generic winning cutoff") + "."
    if result.startswith("Generic "):
        return "Unique for almost every attaining sample " + result.removeprefix("Generic ") + "."
    return result + "."


def proposal_rows(text):
    table = text.split("## Summary\n", 1)[1].split("\n## ", 1)[0]
    table_lines = [line for line in table.splitlines() if line.startswith("| ")]
    entries = [line[2:-2].split(" | ") for line in table_lines[2:]]
    for label, _, result in entries:
        family, held = regime(label)
        exact = (family, held) in EXACT
        if exact:
            claim = ("All real roots of the cleared location score at the least polynomial product are exactly the maximizers. "
                     "Unique for almost every sample with at least three levels. At two balanced levels a half-gap exceeding the "
                     "kernel width gives exactly two reflected points; all other samples with at most two levels have one winner.")
        else:
            claim = make_claim(plain(result))
        section = f"{family} held {held or 'all free'}"
        yield dict(
            family=family, held=held, question="uniqueness", claim=claim,
            direction="exact" if exact else "one-sided", status="written",
            evidence=f"{REPORT.relative_to(ROOT)}#{anchor(section)}",
            frontier="" if exact else FRONTIERS[family], origin="TASK-415",
        )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write-proposals", action="store_true")
    args = parser.parse_args()

    text = REPORT.read_text()
    assert all(ord(char) >= 32 or char in "\t\n" for char in text)
    assert b"\r" not in REPORT.read_bytes()
    assert text.count(r"\[") == text.count(r"\]")
    assert text.count(r"\(") == text.count(r"\)")
    schema, _ = read_csv(ATLAS / "proofs.csv")
    _, atlas = read_csv(ATLAS / "atlas.csv")
    atlas_regimes = {(row["family"], row["held"] if row["held"] != "all free" else "") for row in atlas}
    sections = [regime(f"{family} {held}") for family, held in
                re.findall(r"^### ([a-z]+) held (.+)$", text, re.M)]
    assert len(sections) == len(set(sections)) == 47
    expected = list(proposal_rows(text))
    proposal_path = DATA / "proofs.csv"
    if args.write_proposals:
        with proposal_path.open("w", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=schema, lineterminator="\n")
            writer.writeheader()
            writer.writerows(expected)

    proposed_schema, proposed = read_csv(proposal_path)
    assert proposed_schema == schema and proposed == expected
    assert [(row["family"], row["held"]) for row in proposed] == sections
    assert set(sections) <= atlas_regimes
    for row in proposed:
        key = row["family"], row["held"]
        assert row["question"] == "uniqueness" and row["origin"] == "TASK-415"
        assert (row["direction"] == "exact") == (key in EXACT)
        assert bool(row["frontier"]) == (key not in EXACT)
        target, fragment = row["evidence"].split("#")
        assert ROOT / target == REPORT and fragment in headings(text)

    broken = []
    for link in re.findall(r"\]\(([^)\s]+)\)", text):
        if "://" in link:
            continue
        target, _, fragment = link.partition("#")
        path = (REPORT.parent / target).resolve() if target else REPORT
        if not path.is_file():
            broken.append(link)
        elif fragment and path.suffix == ".md" and fragment not in headings(path.read_text()):
            broken.append(link)
    assert not broken, broken
    print("47 regime sections, summary rows, proposed atlas rows and report links: pass.")


if __name__ == "__main__":
    main()
