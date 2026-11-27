"""Write the scoped TASK-457 proof proposals and verify mask/anchor coverage."""
import csv
import re
from pathlib import Path

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
REPORT = DATA.with_name("2026-11-27-trunc-discrete-math-report.md")
ATLAS = ROOT / "dev/research/existence-atlas"
FIELDS = "family held modifier question claim direction status evidence frontier origin".split()
MODIFIER = "truncate:known"
rows = []


def add(family, held, question, claim, direction="exact", frontier=""):
    anchor = re.sub(r"[^a-z0-9 -]", "", f"{family} {held or 'all free'} {MODIFIER}".lower())
    evidence = REPORT.relative_to(ROOT).as_posix() + "#" + anchor.replace(" ", "-")
    rows.append(dict(zip(FIELDS, [family, held, MODIFIER, question, claim, direction,
                                 "written", evidence, frontier, "trunc-discrete-math"],
                         strict=True)))


def paired(family, held, criterion, failure, frontier=""):
    add(family, held, "criterion", criterion, "iff", frontier)
    add(family, held, "failure", failure, frontier=frontier)


fixed = {
    "bnbinom": "alpha+beta+size",
    "cmb": "nu+prob+size",
    "cmp": "lambda+nu",
    "dlaplace": "location+scale",
    "dnorm": "mean+sd",
    "dunif": "max+min",
    "dweibull": "shape1+shape2",
    "lgser": "theta",
    "pig": "mean+shape",
    "skellam": "mu1+mu2",
    "yulesimon": "rho",
}
for family, held in fixed.items():
    paired(family, held, "Attains at the held law exactly when it is compatible.",
           "No compatible likelihood when retained mass is zero or an observed cell is unsupported.")
    add(family, held, "uniqueness", "The compatible held coefficient tuple is a singleton.")

for held in ("", "alpha", "beta"):
    paired("bbinom", held,
           "Some compatible fixed-size critical value or singleton is at least the best excluded height.",
           "Compare fixed-size zero-shape and binomial faces and compatible growing-size NB/power, logarithmic, Poisson or atom limits; include drift at height zero.",
           "Uniform integer-tail comparison and finite/limit equality beyond the short-window tests.")
for held in ("size", "alpha+size", "beta+size"):
    paired("bbinom", held,
           "A compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights.",
           "Allowed zero-shape recurrences, endpoint mixtures, binomial or endpoint atoms at fixed size.")
    add("bbinom", held, "uniqueness",
        "The finite maximizer set is decided by the rational critical-set comparison at algebraic holds.")
for held in ("", "size"):
    add("bbinom", held, "case:two-cell-tie",
        "On [1,2], sample (1,2) is fitted at size two by every pair beta=(alpha+1)/2.", "one-sided")

bnbinom_masks = ("", "alpha", "alpha+beta", "alpha+size", "beta", "beta+size", "size")
for held in bnbinom_masks:
    paired("bnbinom", held,
           "A compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates.",
           "Normalize reachable rare-shape, NB/power, logarithmic, Poisson and lower-atom ratio limits; on rays discard drifting paths.",
           "Beyond finite rational charts, isolate all competitive ray score roots and decide exact face comparisons.")
    add("bnbinom", held, "case:finite-rational",
        "Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision.")
for held in ("", "alpha"):
    add("bnbinom", held, "case:label-tie",
        "On [1,3], histogram (10,6,4) is attained at (size,alpha,beta)=(1,1,2) and (2,1,1).", "one-sided")

for held in ("", "nu", "nu+prob", "nu+size", "prob", "prob+size", "size"):
    paired("cmb", held,
           "The greatest compatible height among permitted sizes is attained on at least one winning slice.",
           "At each size compare the allowed canonical faces, including extreme-statistic sets and included true endpoint atoms.")
    add("cmb", held, "uniqueness",
        "Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root.")
add("cmb", "nu+prob", "case:cross-size-tie",
    "On [1,infinity), sample (2) with nu=1 and probability (3-sqrt(3))/2 ties exactly at sizes two and three.", "one-sided")

for held, criterion, failure in (
    ("", "On three or more cells apply the constant/adjacent dispatch, then the mean-matched geometric inward test and rate inclusion rule; mixed pairs attain.",
     "Point and adjacent laws and the excluded finite geometric chart with rate at least one."),
    ("lambda", "Use the empirical factorial statistic against its attainable interval and inclusion bits, with separate flat or atomic branches.",
     "Minimal factorial-statistic set, or the excluded zero-dispersion tilt at held rate at least one on a finite window."),
    ("nu", "Use the conditional mean interval and included zero atom. At zero dispersion on a finite window the upper mean is excluded.",
     "Endpoint atom or uniform at the excluded upper geometric rate on a finite window."),
):
    paired("cmp", held, criterion, failure)
    add("cmp", held, "uniqueness", "Each identified moment root is unique; the stated flat, atom and two-cell fibers give every tied coefficient.")

for held, criterion in (
    ("", "The largest compatible center-specific height occurs at a finite rate in at least one center slice."),
    ("location", "An identified rate root exists exactly when the mean distance is strictly between its endpoint expectations."),
    ("scale", "Every compatible sample attains by the center reduction and direct comparison."),
):
    paired("dlaplace", held, criterion + " Singletons are flat.",
           "A free rate can escape to the nearest atom or finite uniform; held rate always attains on compatible data.")
    add("dlaplace", held, "uniqueness", "The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set.")

for held, criterion, failure, frontier in (
    ("", "Short-window and collapse dispatch, then compare finite stationary values with the best geometric tilt for range at least two.",
     "Point and adjacent limits, geometric tilts including uniform, or drift.",
     "General proper-window root isolation, broad-face equality, and singleton cardinality."),
    ("mean", "Compare finite scalar candidates with the uniform and projected sharp-face heights, using the exact two-cell rule on a pair.",
     "Uniform, point, equal-half adjacent sharp law, or drift.",
     "General proper-window scalar root isolation and cardinality."),
    ("sd", "At least two cells give attainment exactly unless all observations equal a finite window end; a singleton always attains.",
     "Favored finite endpoint atom or drift toward an infinite end.",
     "Cardinality on larger proper windows."),
):
    paired("dnorm", held, criterion, failure, frontier)
add("dnorm", "", "case:two-cell-tie", "Every fixed positive scale gives one location fitting a mixed pair by its odds; balanced adjacent cells fit at their common edge for all scales.")
add("dnorm", "mean", "case:two-cell-cardinality", "At the shared edge all scales tie; otherwise the nearer cell's probability increases strictly with precision.")
add("dnorm", "sd", "case:two-cell-cardinality", "Every compatible mixed pair has a unique location at the held positive scale.")

for held in ("", "max", "min"):
    paired("dunif", held, "Every compatible sample attains at the shortest retained interval subject to held ends.",
           "No fit on incompatible data; widening is dominated and clipping plateaus are included.")
    add("dunif", held, "uniqueness", "Every parent endpoint tuple mapping to the optimal effective ends is a maximizer.")

for held, criterion, failure, frontier in (
    ("", "Use the saturated constant/adjacent dispatch and the odds rule on two cells; compare finite critical values against the escape envelope for wider samples.",
     "Power and log increments, discretised Pareto, singleton and adjacent laws, or drift.",
     "Certify all proper-window profile maxima and equality against power and Pareto faces."),
    ("shape1", "Compare finite scalar values against the two held-probability endpoint heights.",
     "Small-shape log increments, zero mass on fixed positives on rays, lower atom or zero-one law at large shape.",
     "Root enclosures and cardinality on larger proper windows."),
    ("shape2", "Except for a singleton, a sample with some count above the lower end attains iff the lower-rate score is strictly positive.",
     "Lower atom or finite-window increment law; the broad ray has zero height.",
     ""),
):
    paired("dweibull", held, criterion, failure, frontier)
add("dweibull", "shape2", "uniqueness", "The identified rate root is unique by strictly negative curvature.")
add("dweibull", "", "case:two-cell-tie", "On [1,2], sample (1,2) fits at one finite probability for every sufficiently large finite shape2.", "one-sided")
add("dweibull", "shape1", "case:two-cell-comparison", "Two-cell odds are monotone on [0,1]; at positive lower cut their finite critical values and endpoint limits give the comparison.")

for family, bounded, unbounded, failure in (
    ("hyper", ("N", "N+k", "N+m"), ("", "k", "m", "k+m"),
     "The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift."),
    ("nhyper", ("N", "N+m", "N+r"), ("", "m", "m+r", "r"),
     "The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses."),
):
    for held in bounded:
        paired(family, held, "Every compatible sample attains by bounded rational enumeration.",
               "No compatible candidate if the integer enumeration yields no positive-weight sample.")
        add(family, held, "uniqueness", "Compare all compatible exact rational heights and retain every tied integer design.")
    for held in unbounded:
        paired(family, held, "A finite compatible design reaches at least the hold-specific population-tail envelope.",
               failure, "General unbounded integer-tail equality and cardinality beyond short-window cases.")
        add(family, held, "case:two-cell-comparison",
            "Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches.")
add("hyper", "k+m", "case:population-only", "With both counts held, only the eventual lower endpoint is an escaping atom.")
for held in ("m", "m+r"):
    add("nhyper", held, "case:ray-attainment", "Every compatible sample on a right ray attains.")

paired("lgser", "", "For a nonsingleton window the empirical mean is strictly above the lower end and below its harmonic-weight mean, infinite on a ray.",
       "Lower atom, finite harmonic law, or drift on a ray.")
add("lgser", "", "uniqueness", "The identified conditional mean root is unique by positive variance.")

for held in ("", "mean", "shape"):
    paired("pig", held,
           "A finite critical value or included atom meets the hold-specific escape envelope.",
           "Reachable restricted Poisson, Sibuya or tilted Sibuya, Lévy and endpoint atoms, or drift.",
           "Root isolation and equality with excluded laws on rays; finite windows admit a rational decision.")
    add("pig", held, "case:finite-rational",
        "Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds.")
add("pig", "", "case:two-cell-tie", "On [1,2], sample (1,2) attains on a continuum of rate and mixing pairs.", "one-sided")
add("pig", "shape", "case:positive-shape-ray", "On a ray at held positive shape, attainment holds exactly when some count exceeds the lower cell.")

for held in ("", "mu1", "mu2"):
    paired("skellam", held,
           "A finite critical value or included axis candidate meets the hold-specific excluded height.",
           "Geometric tilts where summable, endpoint atoms including rare-axis conditional atoms, or drift.",
           "General score root count and exact face equality for larger windows.")
    add("skellam", held, "case:two-cell-comparison",
        "Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits.")
paired("yulesimon", "", "Use the ray score or on finite windows compare rational critical values to the two endpoint heights.",
       "Lower atom or finite harmonic law; at zero shape on rays all fixed masses vanish.")
add("yulesimon", "", "uniqueness", "The ray root is unique, and the finite rational comparison decides the entire tied root set.")


def main():
    families = {row["family"] for row in rows}
    with (ATLAS / "modifier-atlas.csv").open(newline="") as stream:
        masks = {(row["family"], "" if row["held"] == "all free" else row["held"])
                 for row in csv.DictReader(stream)
                 if row["family"] in families and row["modifier"] == MODIFIER}
    report_text = REPORT.read_text()
    for relative in re.findall(r"\]\(([.\w/-]+)(?:#[\w:-]+)?\)", report_text):
        if relative != f"{DATA.name}/proofs.csv":
            assert (REPORT.parent / relative).is_file(), relative
    headings = {re.sub(r"[^a-z0-9 -]", "", heading.lower()).replace(" ", "-")
                for heading in re.findall(r"^### (.*)$", report_text, re.M)}
    assert not any(line.endswith((" ", "\t")) for line in report_text.splitlines())
    assert len(families) == 14 and len(masks) == len(headings) == 68
    assert {(r["family"], r["held"]) for r in rows if r["question"] == "criterion"} == masks
    assert {(r["family"], r["held"]) for r in rows if r["question"] == "failure"} == masks
    assert len({(r["family"], r["held"], r["question"]) for r in rows}) == len(rows)
    for row in rows:
        assert row["evidence"].split("#")[1] in headings
    ordered = sorted(rows, key=lambda r: (r["family"], r["held"], r["question"]))
    with (DATA / "proofs.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(ordered)
    print(f"{len(rows)} proof rows, {len(masks)} masks, {len(families)} families.")


if __name__ == "__main__":
    main()
