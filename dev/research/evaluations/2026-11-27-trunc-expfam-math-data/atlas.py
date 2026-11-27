"""Generate the modifier proof inventory and validate its report anchors."""
import csv
from pathlib import Path
import re

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
REPORT = DATA.with_name(DATA.name.removesuffix("-data") + "-report.md")
SOURCE = REPORT.relative_to(ROOT).as_posix()

# Columns after the key are the exact criterion, failure list, and
# cardinality rule for admissible observations and the declared domain.
KNOWN = [
    ("pois", "", "Interior count mean or an admitted compatible endpoint atom.",
     "Excluded lower atom at zero rate or upper atom at infinite rate.",
     "Unique on a nonsingleton; every compatible singleton parameter maximizes."),
    ("geom", "", "Mean strictly between bottom and the zero-probability mean, or an admitted lower atom.",
     "Excluded lower atom or discrete uniform at zero probability.",
     "Unique on a nonsingleton; every compatible singleton parameter maximizes."),
    ("binom", "size", "Interior count mean or an admitted compatible endpoint atom.",
     "Excluded lower or upper atom when the parent cannot supply positive event mass at the endpoint.",
     "Unique on a nonsingleton; every compatible singleton parameter maximizes."),
    ("binom", "", "Constants attain; otherwise a finite matched size must meet or beat the Poisson face.",
     "Strict victory of the mean-matched same-window Poisson.",
     "All winning sizes and their matched probabilities; ties can include every feasible size."),
    ("binom", "prob", "At interior held prob a positive finite prefix wins except on a nonsingleton bounded window with every observation at its top; endpoint holds use compatibility.",
     "Upper atom at infinite size for constant data at a bounded window's top with interior held prob.",
     "All winning feasible integer sizes, including the compatible singleton and atom sets.",
     "binom free truncate:known"),
    ("nbinom", "size", "For positive size the mean is between the bottom and the zero-probability mean, or gives an admitted lower atom; size zero uses the stated extensions.",
     "Excluded lower atom or the normalized rising-factorial window at zero probability.",
     "Unique at nondegenerate scalar fits; compatible singleton parameters can be flat."),
    ("zeta", "", "Interior log mean for finite N>1; N=1 always attains.",
     "Either endpoint atom at infinite signed exponent.",
     "Unique except N=1, when every exponent maximizes."),
    ("norm", "", "Nonconstant data and, if a matched linear edge exists, empirical variance below its variance.",
     "Collapse or the signed truncated, translated or reflected exponential.",
     "Unique finite fit."),
    ("norm", "mean", "Mean squared distance to held mean is above the squared projection distance and below the uniform mean squared distance on bounded windows.",
     "Projection collapse or a finite-window uniform.",
     "Unique finite fit."),
    ("norm", "sd", "Sample mean is interior to the window.",
     "Unbounded likelihood at either observed finite endpoint.",
     "Unique finite fit."),
    ("exp", "", "Mean distance from the lower cut is positive and below half the width if bounded above.",
     "Lower collapse or excluded uniform.",
     "Unique finite fit."),
    ("gamma", "shape", "Mean interior to the negative rate score range.",
     "Lower collapse where compatible or the finite-window power law.",
     "Unique finite fit."),
    ("gamma", "scale", "Log mean interior to the positive shape score range.",
     "Shape-zero inverse-power exponential carrier when the lower end is cut, or upper collapse.",
     "Unique finite fit."),
    ("gamma", "", "Nonconstant data and no admissible finite edge or corner passes its inward score test.",
     "Collapse, shape-zero inverse-power exponential, rate-zero power, or log-uniform corner.",
     "Unique finite fit."),
    ("beta", "", "Nonconstant data and no admissible edge or corner passes inward log-moment tests.",
     "Collapse, either zero-shape edge, or the carrier corner.",
     "Unique finite fit."),
    ("beta", "shape1", "Mean log complement strictly between the attainable scalar mean limits.",
     "Shape-two-zero edge on a cut window or collapse.", "Unique finite fit.",
     "beta free truncate:known"),
    ("beta", "shape2", "Mean log X strictly between the attainable scalar mean limits.",
     "Shape-one-zero edge on a cut window or collapse.", "Unique finite fit.",
     "beta free truncate:known"),
    ("lnorm", "", "Apply the free normal criterion to the logged observations and limits.",
     "Collapse or transformed exponential edge (power law including log-uniform).",
     "Unique finite fit."),
    ("lnorm", "meanlog", "Apply the held-mean normal criterion in log coordinates.",
     "Projection collapse in log coordinates or log-uniform face.", "Unique finite fit.",
     "lnorm free truncate:known"),
    ("lnorm", "sdlog", "Empirical log mean is interior to the log window.",
     "Endpoint location escape with unbounded likelihood.", "Unique finite fit.",
     "lnorm free truncate:known"),
    ("invgauss", "", "Nonconstant data and no admissible edge or corner passes its inward score test.",
     "Collapse, truncated Levy, exponentially tilted inverse power, or inverse-power corner.",
     "Unique finite fit."),
    ("invgauss", "mean", "Average of X/mean^2+1/X is strictly between the scalar mean limits.",
     "Projected-mean collapse or the inverse-power corner on a window cut at both ends.",
     "Unique finite fit.", "invgauss free truncate:known"),
    ("invgauss", "shape", "Mean interior to the scalar range bounded above by the truncated Levy mean.",
     "Lower collapse or truncated Levy face.", "Unique finite fit.",
     "invgauss free truncate:known"),
    ("weibull", "shape", "Apply the exponential criterion to X^shape.",
     "Lower collapse or the finite-window power-law face.", "Unique finite fit."),
    ("pareto", "b", "Apply the exponential criterion to log(X/b).",
     "Lower collapse or log-uniform face.", "Unique finite fit."),
    ("pareto", "a", "A positive-width window at the sample minimum gives attainment with shape held.",
     "Collapse by approaching a constant sample at the fixed upper cut.",
     "All parent endpoints inducing the optimal effective lower cut.",
     "pareto b truncate:known"),
    ("pareto", "", "Profile the effective lower endpoint to the minimum and apply the scalar exponential criterion there.",
     "Lower collapse, including windows shrinking to the fixed upper cut, or log-uniform escape.",
     "The shape is unique at an estimate, with all parent endpoints inducing the optimal effective cut.",
     "pareto b truncate:known"),
    ("rayleigh", "", "Apply the exponential criterion to X squared on positive-density data.",
     "Lower collapse or the finite-window density proportional to X.", "Unique finite fit."),
    ("invgamma", "alpha", "Apply the held-shape gamma criterion to 1/X.",
     "Upper collapse or the power law proportional to X^(-alpha-1).", "Unique finite fit."),
    ("invweibull", "shape", "Apply the exponential criterion to X^(-shape).",
     "Upper collapse or the power law proportional to X^(-shape-1).", "Unique finite fit."),
]

TRUNCATED = [
    ("pois", "", "All compatible positive samples attain.",
     "No excluded maximizing face.", "Unique, including the zero-rate atom."),
    ("geom", "", "All compatible positive samples attain at prob=1/mean.",
     "No excluded maximizing face.", "Unique, including the atom at one."),
    ("binom", "", "Constants attain; otherwise compare the empirical factorial ratio to the matched Poisson rate.",
     "Strict positive-Poisson escape exactly on the larger empirical ratio side for nonconstants.",
     "All winning integer sizes and matched probabilities; the all-ones union is flat."),
    ("binom", "size", "Mean interior to [1,size] or an admitted endpoint.",
     "No excluded maximizing face for compatible data.", "Unique except singleton support.",
     "binom free zero:truncated"),
    ("binom", "prob", "Compatible data attain, with finite prefix at interior held prob.",
     "No excluded maximizing face.", "Every winning size, including endpoint sets.",
     "binom free zero:truncated"),
    ("nbinom", "", "Ones attain; otherwise some finite stationary or size-zero candidate must meet or beat the Poisson face.",
     "Strict victory of the positive-Poisson face; size zero is admitted.",
     "The all-ones ray or all highest stationary and size-zero pairs."),
    ("nbinom", "size", "Compatible positives attain at the mean-matched probability or at prob one for ones.",
     "No excluded maximizing face.", "Unique, including size zero held.",
     "nbinom free zero:truncated"),
    ("nbinom", "prob", "For interior held prob compare size zero and the finitely many positive score roots; prob one allows only ones.",
     "No excluded maximizing face.", "The winning finite sizes, or all sizes at prob one with ones.",
     "nbinom free zero:truncated"),
]

INFLATED = [
    ("pois", "", "Compatible positives attain; all zeros attain at inflation one.",
     "No excluded maximizing face in the free slice.", "Unique on positive samples; all positive rates at all zeros."),
    ("pois", "pi", "All positive samples with compatible held inflation attain by the rate root comparison; all zeros require inflation one.",
     "At all zeros with inflation below one the zero-rate atom escapes.",
     "All winning positive rate roots; an exact two-rate tie occurs.",
     "pois free zero:inflated"),
    ("geom", "", "Compatible positives attain by the fixed-carrier rule; all zeros attain.",
     "No excluded maximizing face.", "Unique for positives; all-zero union at prob one or inflation one."),
    ("geom", "pi", "A strict scalar root for positive data with held inflation below one; otherwise the compatible zero case.",
     "No excluded maximizing face.", "Unique for positives; at all zeros intersect the atom union with the hold.",
     "geom free zero:inflated"),
    ("binom", "size", "Compatible positive samples attain by the fixed-size rule.",
     "No excluded maximizing face.", "Unique for positives at size at least two with inflation free; at size one recover the product relation."),
    ("binom", "pi+size", "Compare score-polynomial roots and compatible atoms, or use the fixed-size Bernoulli slice.",
     "No excluded maximizing face.", "All winning candidates, including compatible flat zero laws.",
     "binom size zero:inflated"),
    ("binom", "", "With positives compare each finite size to the ZIP face; all zeros attain.",
     "Strict ZIP-face victory can force escape at infinite size.", "All winning pairs and weights; the size-one Bernoulli slice can be flat."),
    ("binom", "pi", "Compare each fixed-size profile with the same-inflation ZIP face; positivity and zero-only holds as stated.",
     "Strict winning ZIP face at positive height.", "All winning finite size/probability pairs.",
     "binom free zero:inflated"),
    ("nbinom", "size", "Compatible positives attain by the fixed-size rule; zero-only samples use the atom union.",
     "No excluded maximizing face.", "Unique positive-sample pair at positive held size with inflation free."),
    ("nbinom", "pi+size", "Compare interior score roots for compatible positives and apply the atom rule at all zeros.",
     "No excluded maximizing face.", "All winning roots, or the compatible all-zero maximizing slice.",
     "nbinom size zero:inflated"),
    ("nbinom", "", "For positives compare the ordinary NB candidate and admissible positive-profile stationary points to the ZIP face.",
     "Strict victory of a Poisson face, with or without inflation.",
     "Every winning candidate; the all-zero union is sliced by the holds."),
    ("nbinom", "pi", "Compare simultaneous parent score roots with the same-inflation Poisson face.",
     "Strict victory of that Poisson face for compatible positive data.",
     "Every winning candidate; all zeros use the compatible atom union.",
     "nbinom free zero:inflated"),
]

FIELDS = ["family", "held", "modifier", "question", "claim", "direction",
          "status", "evidence", "frontier", "origin"]


def anchor(heading):
    return re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")


def markdown_anchors(path):
    return {anchor(line.lstrip("# ")) for line in path.read_text().splitlines()
            if line.startswith("#")}


def check_report_links():
    for target in re.findall(r"\]\(([^ )]+)\)", REPORT.read_text()):
        if "://" in target:
            continue
        path, _, fragment = target.partition("#")
        destination = REPORT.parent / path
        assert destination.is_file(), target
        if fragment and destination.suffix == ".md":
            assert fragment in markdown_anchors(destination), target


def main():
    headings = markdown_anchors(REPORT)
    with (ROOT / "dev/research/existence-atlas/proofs.csv").open() as source:
        original = next(csv.reader(source))
    assert FIELDS == original[:2] + ["modifier"] + original[2:]
    rows = []

    def add(modifier, records):
        for family, held, criterion, failure, uniqueness, *heading in records:
            section = anchor(heading[0] if heading else f"{family} {held or 'free'} {modifier}")
            assert section in headings, section
            frontier = ""
            if family == "nbinom" and not held and modifier == "zero:truncated":
                frontier = "Effective isolation at multiple roots or exactly tied heights remains open."
            if family in {"binom", "nbinom"} and held in {"", "pi"} and modifier == "zero:inflated":
                frontier = "Effective global candidate and face separation in general mixed samples remains open."
            for question, claim, direction in zip(
                    ("criterion", "failure", "uniqueness"), (criterion, failure, uniqueness),
                    ("iff", "exact", "exact")):
                rows.append([family, held, modifier, question, claim, direction, "written",
                             SOURCE + "#" + section, frontier, "TASK-439"])

    add("truncate:known", KNOWN)
    add("zero:truncated", TRUNCATED)
    add("zero:inflated", INFLATED)
    for family, held, *_ in TRUNCATED:
        for mask in (held, "+".join(sorted(filter(None, (held, "p0"))))):
            add("zero:modified", [(family, mask,
                "With positives use the corresponding positive-truncation criterion and a compatible Bernoulli factor; all zeros attain at compatible weights.",
                "Any excluded positive-truncation limit transports with the fitted or held Bernoulli zero weight.",
                "The positive-count maximizer set times the singleton fitted or held zero weight; all-zero data leave the admissible parent slice flat.",
                f"{family} free zero:modified")])

    for design in ("lower", "upper", "both"):
        add(f"truncate:{design}-estimated", [("generic", "",
            "Tighten each free bound to its sample extreme and apply the fixed-window criterion if the tight event has positive mass.",
            "Tight-window failures transport; admissible continuous windows shrinking to a common observation have unbounded likelihood.",
            "Include all maximizing tight-window parent coefficients and precisely those looser windows that remove no probability from the tight-window likelihood comparison.",
            "all applicable truncate:estimated")])

    rows.sort(key=lambda row: tuple(row[:4]))
    assert len({tuple(row[:4]) for row in rows}) == len(rows)
    with (DATA / "proofs.csv").open("w", newline="") as out:
        writer = csv.writer(out, lineterminator="\n")
        writer.writerow(FIELDS)
        writer.writerows(rows)
    check_report_links()
    print(f"Validated {len(rows)} proof rows, evidence anchors and report links.")


if __name__ == "__main__":
    main()
