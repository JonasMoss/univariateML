"""Regenerate TASK-449 modifier proof proposals."""
import csv
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPORT = HERE.with_name("2026-11-27-trunc-ls-math-report.md")
ROOT = HERE.parents[3]
FIELDS = "family held modifier question claim direction status evidence frontier origin".split()
rows = []


def add(family, held, question, claim, direction="exact", frontier=""):
    label = held or "all free"
    anchor = f"{family} {label} truncate:known".replace(" ", "-")
    anchor = anchor.replace("+", "").replace(":", "")
    rows.append((family, held, "truncate:known", question, claim, direction,
                 "written", f"{REPORT.relative_to(ROOT)}#{anchor}", frontier,
                 "trunc-ls-math"))


def results(family, held, criterion, failure, uniqueness=None, frontier=""):
    add(family, held, "criterion", criterion, "iff", frontier)
    add(family, held, "failure", failure)
    if uniqueness:
        add(family, held, "uniqueness", uniqueness)


results("laplace", "", "Nonconstant, and on bounded windows a finite candidate reaches the uniform threshold.",
        "Constant-data collapse or bounded uniform escape.",
        frontier="Simplify the bounded witness test and classify maximizing location/rate sets.")
add("laplace", "", "case:nonunique", "On the right ray, (a,a+1) has all locations <=a at rate 2.", "one-sided")
results("laplace", "mu", "Mean absolute residual lies strictly between the minimum and zero-rate model mean.",
        "Collapse at the projection at the lower equality, otherwise uniform at or above the finite upper threshold.",
        "Unique on attainment.")
results("laplace", "sigma", "All compatible samples.",
        "No failure.", "Compare cuts, sample locations and integrable exterior plateaus. All winning locations maximize.")
results("logis", "", "Nonconstant with a finite score candidate at or above the optimized exponential face.",
        "Constant-data collapse, or optimized signed exponential on the window, including uniform at slope zero.",
        frontier="Global root isolation and uniqueness.")
results("logis", "location", "Exclude constants at the projection. On rays the rest attain; on bounded windows a rate root must reach uniform.",
        "Collapse at the projection or bounded uniform escape.", frontier="Count rate roots and compare their values.")
results("logis", "scale", "A positive rational-score root reaches the applicable exponential face threshold, or the score is identically zero.",
        "Exponential escape of an integrable slope of the held-rate magnitude.",
        "Compare the polynomial roots; every location maximizes if the score vanishes identically.")
add("logis", "scale", "case:nonunique", "The bounded-window sample (a,b) has every location maximizing.", "one-sided")

for family, location, scale, power in (
    ("cauchy", "location", "scale", "inverse-square"),
    ("hcauchy", "mu", "sigma", "inverse-square"),
    ("std", "mean+nu", "nu+sd", "power of exponent nu+1"),
):
    free = "nu" if family == "std" else ""
    tie = "k > nu(n-k) at a feasible center" if family == "std" else "a feasible strict majority"
    results(family, free, "No divergent tie and a finite candidate reaches the applicable power-family threshold.",
            f"Collapse at {tie}; critical collapse, effective uniform, or exterior {power} escape.",
            frontier="Reduce score and boundary comparisons to simple data inequalities and settle cardinality.")
    results(family, location, "No divergent tie at the held center and a scale root reaches the applicable boundary threshold.",
            f"Divergent or critical collapse at the held center, effective uniform, or exterior {power} escape.",
            frontier="Classify the rate roots and ties against the held boundary values.")
    if family == "hcauchy":
        results(family, scale, "Exclude constants at a finite upper cut. Otherwise a finite candidate reaches the remote-location threshold.",
                "Shrinking-support collapse at the upper cut or bounded uniform escape.",
                frontier="Compare possible distinct finite branch maxima.")
        add(family, scale, "case:left-ray-unique", "On a left ray with m<b the unique location is m.", "one-sided")
    else:
        results(family, scale, "Every sample on rays; on bounded windows a location root must reach uniform.",
                "Bounded uniform escape.", frontier="Classify finite cardinality beyond the exact nonunique example.")
        add(family, scale, "case:nonunique", "On [-100,100], data (-4,4) and effective scale 1 give reflected global maxima (Student nu=3).", "one-sided")

results("gumbel", "", "Nonconstant with a finite score candidate reaching the optimized exponential face.",
        "Constant-data collapse or optimized exponential escape, including uniform at slope zero.",
        frontier="Global root isolation and uniqueness of the inverse-scale profile.")
results("gumbel", "mu", "Exclude constants at the projection; on bounded windows a finite rate root must also reach uniform.",
        "Collapse at the projection or bounded uniform escape.", frontier="Count rate roots and compare their values.")
results("gumbel", "sigma", "In the exponential transform the sample mean is strictly between its lower cut and its zero-rate mean.",
        "Upper-x-end collapse or the decreasing exponential with slope -1/sigma.",
        "Unique by the scalar exponential score.")

results("hnorm", "", "Nonconstant with a finite branch winner reaching the other branch supremum.",
        "Atomic collapse, or an excluded uniform or negative-slope exponential from the branch comparison.",
        "At most two finite fits; compare the two branch winners and count coincidences once.",
        "Determine whether distinct equal-valued branch winners occur.")
results("hnorm", "mu", "The empirical squared residual lies strictly between the minimum and zero-tilt mean on the effective window.",
        "Collapse at the lower effective cut or bounded effective uniform.",
        "Unique by strict monotonicity of the squared-residual mean.")
results("hnorm", "sigma", "All samples except constants at a finite window cut.",
        "Collapse at the lower cut by remote location or at the upper cut by shrinking support.",
        "Compare the two finite branch winners and count coincidences once.",
        "Determine whether distinct equal-valued branch winners occur.")

for held, center in (("", "a sample level"), ("mean", "the held center"),
                     ("sd", "a sample level"), ("mean+sd", "the held center")):
    add("std", held, "case:divergent-tie", f"A tie of multiplicity k at {center} with k > 2(n-k) gives divergence.", "one-sided",
        "Free-df joint face classification remains open.")

results("unif", "", "Exactly nonconstant compatible samples.",
        "Constant samples collapse to the common observation.",
        "Unique iff neither sample extremum is on its window cut.")
results("unif", "max", "Minimum is strictly below the fixed effective upper endpoint.",
        "Collapse to the fixed effective upper endpoint.",
        "Unique iff the sample minimum is above the lower window cut.")
results("unif", "min", "Maximum is strictly above the fixed effective lower endpoint.",
        "Collapse to the fixed effective lower endpoint.",
        "Unique iff the sample maximum is below the upper window cut.")

for family, held in (("laplace", "mu+sigma"), ("logis", "location+scale"),
                     ("cauchy", "location+scale"), ("gumbel", "mu+sigma"),
                     ("hnorm", "mu+sigma"), ("hcauchy", "mu+sigma"),
                     ("std", "mean+nu+sd"), ("unif", "max+min")):
    results(family, held, "Every compatible sample with positive retained mass.",
            "No failure on compatible input.", "The feasible parameter set is a singleton.")

with (HERE / "proofs.csv").open("w", newline="") as out:
    writer = csv.writer(out, lineterminator="\n")
    writer.writerow(FIELDS)
    writer.writerows(sorted(rows))
print(f"{len(rows)} written proposals")
