"""Reproduce and verify the modifier proof inventory and hold-mask index."""
import argparse
import csv
import io
from pathlib import Path
import re

DATA = Path(__file__).resolve().parent
EVALUATIONS = DATA.parent
REPORT = EVALUATIONS / (DATA.name.removesuffix("-data") + "-report.md")
ATLAS = EVALUATIONS.parent / "existence-atlas"
MODIFIERS = ("truncated", "modified", "inflated")
INDEX_MARKER = "\n## Individual hold masks\n"

# Each note gives the specialization of the preceding family proof.
MASK_NOTES = {
    "bbinom": {
        "alpha": "Keep fixed alpha in each slice and in the growing-size truncated NB face.",
        "alpha+size": "Use the fixed-design beta score and its endpoint laws at beta zero and infinity.",
        "beta": "Only bounded-design alpha escapes compete. The growing-size tail vanishes.",
        "beta+size": "Compare the fixed-design alpha score with the rare-alpha law and upper atom.",
        "size": "Compare both fixed-design scores with the rare-alpha, binomial and endpoint faces.",
    },
    "bnbinom": {
        "alpha": "Use both numerator scores. The swap is permitted. The bounded rare-shape faces remain.",
        "alpha+beta": "Vary size alone and keep the bounded rare-size face at its zero endpoint.",
        "alpha+beta+size": "Evaluate the supplied parameters.",
        "alpha+size": "Vary beta alone and keep the bounded rare-beta face at its zero endpoint.",
        "beta": "Apply the two-coordinate score under the beta hold. Retain allowed rare-size and scaled NB faces.",
        "beta+size": "Vary alpha alone. Its zero endpoint loses positive mass, and its infinite endpoint conditions to one.",
        "size": "Apply the two-coordinate score under the size hold. Retain allowed rare-beta and scaled NB faces.",
    },
    "cmb": {
        "nu": "Compare the unique fixed-design probability solutions or endpoint decisions across supported sizes.",
        "nu+prob": "Compare one evaluation per supported size, subject to compatibility with the held endpoint probability.",
        "nu+prob+size": "Evaluate the supplied design and canonical coefficients.",
        "nu+size": "Use the scalar probability chart or its endpoint decisions at the given size.",
        "prob": "Compute each fixed-odds dispersion optimum or extreme-statistic limit, then compare design heights.",
        "prob+size": "Use the scalar dispersion moment or the extreme-statistic limit at the given odds and size.",
        "size": "Use the positive-support hull, including the size-two rank drop and the size-one flat case.",
    },
    "cmp": {
        "lambda": "Use the scalar dispersion moment with the geometric KKT condition when the held rate is below one.",
        "lambda+nu": "Evaluate the supplied law if the positive event is available.",
        "nu": "Use the scalar conditional mean with the rate domain determined by the held dispersion.",
    },
    "dgamma": {
        "rate": "Compare the right shape score at zero shape and use its strictly decreasing score.",
        "rate+shape": "Evaluate the supplied gamma cell integrals.",
        "shape": "Check the all-ones escape first. For other data bracket the rate, with strict concavity at shape at least one.",
    },
    "dweibull": {
        "shape1": "Hold t fixed and use strict conditional concavity in shape2. All ones selects the upper-shape limit.",
        "shape1+shape2": "Evaluate the supplied conditional survival differences.",
        "shape2": "Hold beta fixed and use the strict rate score. All ones selects the upper-rate limit.",
    },
    "pig": {
        "mean": "Compare the two held-mean boundary values with the scalar Bessel score. All ones has the exact two-endpoint test.",
        "mean+shape": "Evaluate the supplied Bessel law. Shape zero has no positive component.",
        "shape": "At positive held shape, all ones selects the lower-mean limit. A strict inward comparison removes the upper-mean Lévy limit.",
    },
}

CRITERIA = {
    "bbinom": "Some finite-design critical value reaches the best applicable fixed- or growing-design escape.",
    "bnbinom": "Some conditional critical value reaches the greatest reachable recurrence-face height.",
    "cmb": "At least one attaining design candidate has the greatest height among the fixed-design candidates, including excluded endpoints.",
    "cmp": "The held or joint moment test gives a finite solution including an admissible geometric KKT point.",
    "dgamma": "The conditional gamma score and shape-zero, atom or adjacent comparisons decide attainment as specified under the holds.",
    "dweibull": "Under a single hold the scalar score gives attainment exactly off all ones. With both free compare the scalar profile against the applicable saturated or Pareto face.",
    "pig": "The conditional Bessel score is compared with the reachable Sibuya-type, Poisson and atom faces, using strict Lévy exclusion.",
}

FAILURES = {
    "bbinom": "Every positive-likelihood escape is among the rare-alpha, truncated binomial, atom, truncated NB, logarithmic and truncated Poisson laws under its path conditions.",
    "bnbinom": "The bounded rare-shape recurrence faces, truncated NB, logarithmic, truncated Poisson and the one atom exhaust positive-likelihood escapes under their path conditions.",
    "cmb": "The conditional fixed-design hull or scalar extreme statistic gives the escaped law. Keep every excluded face tied at the best supported design height.",
    "cmp": "On nonattainment the winning positive-likelihood escapes are the permitted point or adjacent laws, with only the one atom when one coefficient is held.",
    "dgamma": "The shape-zero conditional gamma, atoms and adjacent splits exhaust positive-likelihood escapes under their path conditions.",
    "dweibull": "The discretised Pareto, atoms and adjacent splits exhaust positive-likelihood escapes under their path conditions.",
    "pig": "The reachable Sibuya-type, truncated Poisson and one atom exhaust winning escapes after strict Lévy exclusion.",
}

FRONTIERS = {
    "bbinom": "Conditional integer-tail bounds and cross-size equality where size is free.",
    "bnbinom": "Canonical root coverage and exact comparison with the best reachable face.",
    "cmb": "Efficient bounds inside the design cap, and held nonzero inflation root coverage.",
    "cmp": "Held nonzero inflation root coverage and tie comparison.",
    "dgamma": "Joint conditional profile and shape-zero comparison, and rate-root counting below shape one.",
    "dweibull": "Joint profile uniqueness and global Pareto comparison.",
    "pig": "Global Bessel profile and equality with Poisson or Sibuya faces.",
}

INFLATION = {
    "bbinom": "Use (beta)_N/(alpha+beta)_N for the zero comparison and retain bounded- and growing-design parent faces.",
    "bnbinom": "Compare the gamma-ratio zero mass and retain held-compatible NB/Poisson parent faces and label swaps.",
    "cmb": "Compare the natural-chart branch optima across the supported designs. The size-three interior-inflation counterexample requires the comparison.",
    "cmp": "For free inflation the branch-compatible moment or KKT point is the unique finite optimizing point when it exists.",
    "dgamma": "With both free and free inflation, attain exactly for positive range at least two. A single parent hold ensures attainment with positives.",
    "dweibull": "With both free and free inflation, attain exactly for positive range at least two. One held coefficient gives the stated scalar criterion.",
    "pig": "Compare to an admissible parent Poisson face. The Poisson–Lévy face is strictly dominated for every compatible inflation hold.",
}

UNIQUENESS = {
    "bbinom": "At a fixed size with algebraic holds the semialgebraic critical components determine all finite maximizers. Size one is shape flat and (1,2) at size two has a maximizing shape ray.",
    "bnbinom": "The positive recurrence identifies the unordered numerator shapes. The exact off-diagonal sample refutes labeled uniqueness.",
    "cmb": "The conditional fixed-design maximizer is unique on a minimal chart; the size-two mixed fit has a ridge when both canonical coordinates are free. The exact two-design tie precludes general uniqueness.",
    "cmp": "Each attained truncated fit on a nontrivial free chart is unique by positive covariance.",
    "dgamma": "An attained fit has a unique free shape at held rate, or a unique free rate at held shape at least one.",
    "dweibull": "An attained fit under one parent hold is unique by strict scalar concavity.",
    "pig": "Fixed Bessel argument has a unique inner optimum if a count exceeds one. Held mean admits an exact two-face tie on all ones.",
}


def slug(heading):
    return re.sub(r"[^a-z0-9 -]", "", heading.lower()).replace(" ", "-")


def title(family, held, modifier):
    return f"{family} {held or 'all-free'} {modifier}"


def index():
    parts = [INDEX_MARKER, "\nEach entry applies the indicated family proof under the named holds.\n"]
    for family, masks in MASK_NOTES.items():
        for held, note in masks.items():
            for modifier in MODIFIERS:
                reference = slug(title(family, "", modifier))
                link = f"[{family} {modifier} proof](#{reference})"
                if modifier == "modified":
                    description = "Apply the hurdle factor to the held [conditional fit](#" + slug(
                        title(family, held, "truncated")) + ")."
                elif modifier == "inflated":
                    description = ("Preserve the same parent holds in both branches and in the "
                                   "parent boundary fit. Evaluate directly when all are held.")
                else:
                    description = note
                parts.append(f"\n### {title(family, held, modifier)}\n\n{description} {link}\n")
    return "".join(parts)


def inventory():
    with (ATLAS / "atlas.csv").open() as source:
        expected = {(r["family"], "" if r["held"] == "all free" else r["held"])
                    for r in csv.DictReader(source) if r["family"] in MASK_NOTES}
    assert expected == {(family, held)
                        for family, masks in MASK_NOTES.items() for held in ("", *masks)}

    with (ATLAS / "proofs.csv").open() as source:
        columns = next(csv.reader(source))
    columns.insert(columns.index("held") + 1, "modifier")
    output = io.StringIO()
    writer = csv.writer(output, lineterminator="\n")
    writer.writerow(columns)

    def add(family, held, modifier, question, claim, direction):
        path = REPORT.relative_to(EVALUATIONS.parents[2]).as_posix()
        anchor = slug(title(family, held, modifier))
        writer.writerow((family, held, "zero:" + modifier, question, claim, direction,
                         "written", path + "#" + anchor, FRONTIERS[family], "TASK-440"))

    for family, masks in MASK_NOTES.items():
        for held in ("", *masks):
            add(family, held, "truncated", "criterion", CRITERIA[family] + (
                " Apply the designated hold specialization." if held else ""), "iff")
            add(family, held, "truncated", "failure", FAILURES[family], "exact")
            add(family, held, "modified", "criterion",
                "On compatible data with positives, attainment is exactly that of the truncated fit. "
                "On all zeros the positive coordinates are flat wherever the positive component is defined or unused.", "iff")
            add(family, held, "modified", "failure",
                "With positives the escaped maximizing laws and ties are exactly those of the truncated fit "
                "with its selected total zero mass.", "exact")
            add(family, held, "inflated", "criterion",
                "A compatible modified optimum remains optimal. Otherwise attainment holds iff a finite branch "
                "critical candidate reaches the greatest escaped height. " + INFLATION[family], "iff")
            add(family, held, "inflated", "failure",
                "Use reachable parent face masses with the chosen inflation objective. A path to parent zero mass one "
                "loses positive likelihood. Keep every reachable co-winning face.", "exact")
        add(family, "", "truncated", "uniqueness", UNIQUENESS[family], "one-sided")
        if family in {"bbinom", "bnbinom"}:
            for held in ("", *masks):
                add(family, held, "truncated", "case:all-ones",
                    "For an all-ones sample the exact attained set or winning escape is given by "
                    "monotonicity of the conditional recurrence and the hold-specific endpoint.",
                    "exact")
    return output.getvalue()


def verify(report, records):
    anchors = [slug(match) for match in re.findall(r"^#+ (.+)$", report, re.MULTILINE)]
    assert len(anchors) == len(set(anchors)), "duplicate report anchors"
    for row in csv.DictReader(io.StringIO(records)):
        filename, anchor = row["evidence"].split("#")
        assert (EVALUATIONS.parents[2] / filename) == REPORT
        assert anchor in anchors, anchor
    for target in re.findall(r"\]\(([^ )]+)\)", report):
        filename, separator, anchor = target.partition("#")
        if filename:
            destination = REPORT.parent / filename
            assert destination.is_file() or destination == DATA / "proofs.csv", filename
            if separator and destination.suffix == ".md":
                destination_titles = re.findall(r"^#+ (.+)$", destination.read_text(), re.MULTILINE)
                assert anchor in {slug(item) for item in destination_titles}, target
        elif separator:
            assert anchor in anchors, anchor
    print(f"{len(list(csv.DictReader(io.StringIO(records))))} proof rows, "
          f"{sum(len(masks) + 1 for masks in MASK_NOTES.values())} parent hold masks")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    report = REPORT.read_text()
    records = inventory()
    output = DATA / "proofs.csv"
    if args.write:
        report = report.split(INDEX_MARKER, 1)[0].rstrip() + "\n"
        report = report.replace(" hold masks truncated", " all-free truncated")
        report = report.replace(" hold masks modified", " all-free modified")
        report = report.replace(" hold masks inflated", " all-free inflated")
        report += index()
        verify(report, records)
        REPORT.write_text(report)
        output.write_text(records)
    else:
        assert report.endswith(index())
        assert output.read_text() == records
        verify(report, records)


if __name__ == "__main__":
    main()
