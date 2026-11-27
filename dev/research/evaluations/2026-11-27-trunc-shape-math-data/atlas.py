"""Generate the proposal and verify its keys against the local atlas."""
import csv
import re
from pathlib import Path


DATA = Path(__file__).resolve().parent
REPORT = DATA.with_name("2026-11-27-trunc-shape-math-report.md")
RESEARCH = DATA.parent.parent
ROOT = RESEARCH.parent.parent
ATLAS = RESEARCH / "existence-atlas"
MODIFIER = "truncate:known"
ORIGIN = "trunc-shape-math"
COLUMNS = ["family", "held", "modifier", "question", "claim", "direction",
           "status", "evidence", "frontier", "origin"]


def read_csv(path):
    with path.open(newline="") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def generate():
    reviewed_fields, reviewed = read_csv(ATLAS / "modifier-proofs.csv")
    assert reviewed_fields == COLUMNS
    _, inventory = read_csv(ATLAS / "modifier-atlas.csv")
    target = {"weibull", "lomax", "llogis", "gompertz", "frechet",
              "invexp", "power", "kumar", "pareto2", "gamma"}
    covered = {(row["family"], row["held"]) for row in reviewed
               if row["modifier"] == MODIFIER and row["status"] == "written"}
    pending = {(row["family"], "" if row["held"] == "all free" else row["held"])
               for row in inventory if row["family"] in target and row["modifier"] == MODIFIER}
    headings = set(re.findall(r"^### (.+)$", REPORT.read_text(), re.M))
    frontier = "General pointwise finite cardinality and effective equality comparison remain open."
    rows = []

    def add(family, held, question, claim, direction, restriction=""):
        heading = f"{family} {held or 'free'} {MODIFIER}"
        assert heading in headings, heading
        anchor = re.sub(r"[^a-z0-9 -]", "", heading).replace(" ", "-")
        evidence = f"{REPORT.relative_to(ROOT)}#{anchor}"
        rows.append((family, held, MODIFIER, question, claim, direction,
                     "written", evidence, restriction, ORIGIN))

    def result(family, held, criterion, failure, uniqueness=None, cardinality_open=False):
        add(family, held, "criterion", criterion, "iff")
        add(family, held, "failure", failure, "exact")
        if uniqueness:
            add(family, held, "uniqueness", uniqueness,
                "one-sided" if cardinality_open else "exact",
                frontier if cardinality_open else "")

    def comparison(family, held, face, other=""):
        result(family, held,
               "Outside the specified divergence branches, a feasible stationary or join candidate meets or exceeds the best excluded height.",
               f"An excluded winning limit is among {face}. {other}".strip())
        add(family, held, "uniqueness",
            "The attained maximizing set consists of the candidates at the highest likelihood, including any specified coefficient plateaus.",
            "exact", frontier)

    comparison("weibull", "", "the signed power tilts, including log-uniform where integrable",
               "Constants diverge to an atom.")
    comparison("weibull", "scale", "log-uniform where integrable",
               "Constant data at the projected scale diverge to an atom.")
    comparison("llogis", "", "the signed power tilts, including log-uniform where integrable",
               "Constants diverge to an atom.")
    comparison("llogis", "scale", "log-uniform where integrable",
               "Constant data at the projected scale diverge to an atom.")
    comparison("llogis", "shape", "the integrable powers of exponents shape-1 and -shape-1")
    comparison("gompertz", "", "signed exponential tilts, including uniform",
               "Constants diverge to an atom.")
    comparison("gompertz", "a", "the rate-a truncated exponential",
               "Positive lower-cut constants diverge. All-zero data select the rate-a face on a finite window and attain throughout the domain on the full positive half-line.")
    result("gompertz", "b", "The mean of exp(b X) lies strictly between the lower endpoint and the zero-rate mean on the transformed window.",
           "Lower collapse or an excluded positive exponential tilt of slope b.", "Unique if attained.")
    result("invexp", "", "The reciprocal mean lies strictly between the lower reciprocal cut and the zero-rate mean.",
           "Upper collapse or the excluded power x^-2.", "Unique if attained.")
    result("power", "", "The sample maximum exceeds the lower cut and the log mean is strictly between the zero-shape mean and log of the sample maximum.",
           "Shrinking window or infinite-shape collapse, or excluded log-uniform.",
           "Shape is unique. Alpha is the sample maximum below the cut and any alpha at or above the cut when the maximum equals it.")
    result("power", "alpha", "On the retained interval the log mean lies strictly between the zero-shape mean and the upper log endpoint.",
           "Excluded log-uniform or upper collapse.", "Unique if attained.")
    result("power", "beta", "The sample maximum strictly exceeds the lower cut.",
           "An interval shrinking to the lower cut has unbounded likelihood.",
           "Alpha is the sample maximum below the cut and any alpha at or above the cut when the maximum equals it.")
    comparison("lomax", "", "truncated exponential, power and translated reciprocal limits",
               "All-lower-cut data diverge to a lower atom.")
    comparison("lomax", "kappa", "the integrable truncated power or uniform")
    result("lomax", "lambda", "The mean of log(1+y/s) is strictly between zero and the zero-shape mean.",
           "Lower collapse or the excluded translated reciprocal law.", "Unique if attained.")
    comparison("pareto2", "min", "the shifted Lomax exponential, power and translated reciprocal limits",
               "Threshold-contact spikes and all-lower-cut collapse follow the stated branches.")
    comparison("pareto2", "min+shape", "the truncated power, uniform or exact critical contact envelope",
               "A negative small-scale exponent gives an unbounded atom path.")
    result("pareto2", "min+scale", "The mean of the log transformed excess lies strictly between zero and the zero-shape mean.",
           "Lower collapse or the excluded translated reciprocal law.", "Unique if attained.")
    result("pareto2", "", "No compatible sample with all three coefficients free attains.",
           "Small-scale lower collapse or shrinking-width upper collapse gives unbounded likelihood.",
           "No attained branch.")
    comparison("pareto2", "shape", "the finite-window uniform or critical contact envelope",
               "A negative small-scale exponent or upper width collapse gives unbounded likelihood.")
    comparison("pareto2", "scale", "the translated reciprocal or shifted exponential limits",
               "Constants diverge by lower collapse or upper width collapse.")
    comparison("pareto2", "scale+shape", "the finite-window uniform",
               "Upper width collapse gives unbounded likelihood.")
    comparison("kumar", "", "the powers in negative log x or the reciprocal kernel 1/(exp(A y)-1)",
               "Constants diverge to an atom.")
    result("kumar", "a", "The mean of log(1-x^a) lies strictly between its zero-shape mean and upper endpoint.",
           "Excluded reciprocal carrier or lower x collapse.", "Unique if attained.")
    comparison("kumar", "b", "the power in negative log x when integrable",
               "A constant sample at the included upper x cut diverges to an atom.")
    comparison("frechet", "mu", "the signed powers in the positive excess",
               "Constants diverge to an atom.")
    comparison("frechet", "mu+sigma", "log-uniform in the excess where integrable",
               "Constants at the projected excess scale diverge to an atom.")
    result("frechet", "lambda+mu", "The inverse-power excess mean is strictly between its lower transformed cut and zero-rate mean.",
           "An excluded inverse-power carrier or upper x collapse.", "Unique if attained.")
    comparison("frechet", "lambda", "the shifted power, nonnegative exponential tilt or critical contact envelope",
               "Negative contact exponent or upper width collapse gives an unbounded likelihood.")
    comparison("frechet", "lambda+sigma", "the finite-window uniform",
               "Upper width collapse gives unbounded likelihood.")
    comparison("frechet", "sigma", "a nonincreasing exponential tilt on the physical window",
               "Constants diverge to an atom.")
    add("frechet", "", "criterion", "Attainment requires at most half of the observations at the minimum, with strict minority when the minimum equals the lower cut.",
        "one-sided", "Complete boundary comparison remains open at exact half above the lower cut and minimum minority.")
    add("frechet", "", "failure", "A strict minimum majority gives an unbounded likelihood with a point-mass limit.",
        "one-sided", "Complete boundary comparison remains open at exact half above the lower cut and minimum minority.")
    add("frechet", "", "case:half-at-lower-cut",
        "At exact half at the lower cut the strict supremum is the product of inverse squared positive gaps.",
        "exact")
    add("frechet", "", "path:gumbel", "Increasing shape with fixed limiting center and scaled width yields the same-window Gumbel maximum law.",
        "one-sided", "Global comparison to the truncated Gumbel face is open.")
    add("frechet", "", "path:critical-contact", "At exact half and shape one a contact path yields the explicit finite positive atom likelihood.",
        "one-sided", "The path height alone does not settle attainment.")

    for family, held in sorted(pending - covered):
        if (family, held, MODIFIER, "criterion") not in {row[:4] for row in rows}:
            result(family, held, "Evaluate the specified coefficients on a positive-mass compatible window.",
                   "An invalid window or incompatible sample has no positive-likelihood fit.",
                   "At most one specified coefficient vector.")

    keys = [row[:4] for row in rows]
    assert len(keys) == len(set(keys))
    assert {(family, held) for family, held, *_ in rows} == pending - covered
    path = DATA / "proofs.csv"
    with path.open("w", newline="") as handle:
        writer = csv.writer(handle, lineterminator="\n")
        writer.writerow(COLUMNS)
        writer.writerows(sorted(rows, key=lambda row: row[:4]))
    print(f"Validated {len(rows)} rows, {len(pending - covered)} masks and their report anchors.")


if __name__ == "__main__":
    generate()
