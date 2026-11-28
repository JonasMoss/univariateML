"""Render the mask register and export the proved atlas rows."""
import csv
import pathlib
import sys

DATA = pathlib.Path(__file__).resolve().parent
REPORT = "dev/research/evaluations/2026-11-27-trunc-gb2-math-report.md"
ATLAS = DATA.parents[1] / "existence-atlas"
MOD = "truncate:known"
families = ("betapr burr gb2 genbeta gengamma trgamma invtrgamma invburr "
            "invparalogis paralogis invpareto fpareto").split()
entries = {}
rows = []


def record(family, mask, body, claims=(), frontier=""):
    assert (family, mask) not in entries, (family, mask)
    entries[family, mask] = body, frontier
    anchor = f"{family}-{mask.replace('+', '')}-truncateknown".replace("all free", "free").lower()
    for question, direction, claim in claims:
        rows.append((family, mask, MOD, question, claim, direction, "written",
                     f"{REPORT}#{anchor}", frontier, "trunc-gb2-math"))


def exact(f, m, body, criterion, failure, unique=None, frontier=""):
    failure_direction = "one-sided" if criterion == "No finite positive MLE exists." or criterion == "There is no finite positive MLE." else "exact"
    claims = [("criterion", "iff", criterion), ("failure", failure_direction, failure)]
    if unique:
        claims.append(("uniqueness", "exact", unique))
    record(f, m, body, claims, frontier)


def fixed(f, m):
    exact(f, m, "**Proved.** Evaluate the supplied coefficients with the window "
          "likelihood formula. At closed parent contact use the inside density.",
          "A finite positive conditional evaluation exists iff the retained mass "
          "is positive and every observation is compatible with positive finite density.",
          "Zero retained mass, incompatible data, or zero or infinite density under "
          "the stated inside endpoint convention.",
          "At most one supplied coefficient vector.")


beta_ref = "[two-shape tests](#likelihood-formulas-and-comparison-rules)"
root_frontier = "Bound the number of winning roots or exhibit a sample with multiple winners."
bp_ref = "[fixed-power beta-prime escapes](#fixed-power-beta-prime-escapes)"
power_ref = "[varying-power proofs](#varying-the-power)"
tg_ref = "[all-free transformed gamma](#transformed-gamma-with-all-coefficients-free)"
end_ref = "[moving-support proofs](#moving-support-and-endpoint-conventions)"


def beta(f, m, geometry, free):
    if free == "both":
        rule = "Use both beta-shape scores and the edge and corner inward tests."
        criterion = "The data are nonconstant and no eligible beta edge or corner passes the inward score tests."
        failure = "Constant collapse or a winning eligible zero beta shape edge or corner."
    else:
        stat = "log z" if free == "p" else "log(1-z)"
        rule = f"Use the scalar positive tilt on ${stat}$."
        criterion = f"The empirical mean of {stat} is strictly between the zero-tilt mean and the upper extremum."
        failure = "An eligible zero-tilt beta edge or upper-statistic collapse at its favored cut."
    body = f"**Proved.** {geometry} {rule} Apply the {beta_ref}."
    exact(f, m, body, criterion, failure, "Unique when attained.")


def beta_prime(f, m, geometry, holds, collapse):
    exact(f, m, f"**Proved.** {geometry} In the transformed coordinate use {bp_ref}. "
          f"{holds} {collapse} Outside divergence compare the scale and free-shape scores "
          "to the eligible face heights, with equality allowed.",
          "Outside the stated constant-divergence cases, a finite score root meets every eligible beta-prime boundary height.",
          f"{collapse} Other nonattainment is a winning eligible beta-prime zero-shape, gamma or power limit.",
          frontier=root_frontier)


def power_comparison(f, m, body, collapse, face):
    exact(f, m, "**Proved.** " + body,
          "Outside the stated divergence branches, a feasible score root or specified "
          "finite join meets all eligible excluded heights.",
          collapse + " Other nonattainment is a winning " + face + ".",
          frontier=root_frontier)


bp_geom = "Use $z=x^d/(S^d+x^d)$ at the held scale and power."
beta("betapr", "scale", bp_geom + " Here $d=1$.", "both")
beta("betapr", "scale+shape1", bp_geom + " Here $d=1$.", "q")
beta("betapr", "scale+shape2", bp_geom + " Here $d=1$.", "p")
fixed("betapr", "scale+shape1+shape2")
for mask, holds, collapse in [
    ("all free", "Neither beta shape is held.", "Every constant sample collapses at its common value."),
    ("shape1", "Hold the lower beta shape p.", "Constants at a positive lower cut collapse."),
    ("shape2", "Hold the upper beta shape q.", "Constants at a finite upper cut collapse."),
    ("shape1+shape2", "Hold both beta shapes.", "There is no divergence branch."),
]:
    beta_prime("betapr", mask, "Use $d=1,p=shape1,q=shape2$.",
               holds, collapse)

for family in ("burr", "invburr"):
    reflected = family == "invburr"
    direction = ("Use reciprocal observations $1/x$ and scale $1/S$ before applying Burr. "
                 "All lower and upper cuts below refer to that transformed window."
                 if reflected else "Use the original positive window.")
    fixed(family, "scale+shape1+shape2")
    beta(family, "scale+shape2", f"{direction} {bp_geom} Set $p=1$.", "q")
    beta_prime(family, "shape2", direction, "Hold $d=shape2,p=1$.",
               "Constants at the transformed positive lower cut collapse.")
    beta_prime(family, "shape1+shape2", direction, "Hold $p=1,q=shape1,d=shape2$.",
               "There is no divergence branch.")
    for mask, collapse in [
        ("shape1", "Every constant sample collapses at its common value."),
        ("scale+shape1", "Constants at the projected held log scale collapse."),
    ]:
        power_comparison(family, mask, f"{direction} Apply the fixed-q Burr comparison "
                         f"in the {power_ref}. {collapse} The only regular excluded height is "
                         "log-uniform on a two-cut log window. Either unbounded log side gives zero.",
                         collapse, "log-uniform")

for family in ("paralogis", "invparalogis"):
    direction = ("First reciprocate observations and scale. " if family.startswith("inv") else "")
    fixed(family, "scale+shape")
    beta_prime(family, "shape", direction + "Set $d=q=shape,p=1$.",
               "The two beta shapes are fixed.", "There is no divergence branch.")
    for mask, collapse in [
        ("all free", "All constants collapse by free modal alignment."),
        ("scale", "Constants at the projected held log scale collapse, except at the held "
                  "log scale when the retained log interval extends to its left."),
    ]:
        power_comparison(family, mask, direction + f"Apply the tied $q=d$ proof in {power_ref}. "
                         + collapse + " The sole regular excluded height is log-uniform on a "
                         "bounded log window.", collapse, "log-uniform")

ip = "Reciprocate $x$ and the scale. The resulting Lomax has $p=d=1,q=shape$."
fixed("invpareto", "scale+shape")
beta("invpareto", "scale", ip + " " + bp_geom, "q")
beta_prime("invpareto", "shape", ip, "Hold both beta shapes.", "There is no divergence branch.")
beta_prime("invpareto", "all free", ip, "Free the upper beta shape in the reciprocal coordinate.",
           "Constants at the transformed positive lower cut collapse.")

gb = "Use $q=shape1,d=shape2,p=shape3$."
beta("gb2", "scale+shape2", gb + " " + bp_geom, "both")
beta("gb2", "scale+shape1+shape2", gb + " " + bp_geom, "p")
beta("gb2", "scale+shape2+shape3", gb + " " + bp_geom, "q")

for family in ("trgamma", "invtrgamma"):
    coord = ("Use $u=x^{-d}$, $y=-\\log x$, and reciprocal scale in the forward kernel. "
             if family.startswith("inv") else "Use $u=x^d$, $y=\\log x$ in the forward kernel. ")
    fixed(family, "scale+shape1+shape2")
    for mask, body, criterion, failure in [
        ("shape2", "Use the two-coordinate gamma test.", "Distinct data and no eligible edge or corner passes the gamma inward tests.",
         "Constant collapse or an excluded shape-zero, rate-zero, or corner gamma edge."),
        ("scale+shape2", "Use the scalar tilt on log u with fixed rate.",
         "The mean log u is strictly between its zero-shape limit mean and the upper log cut.",
         "Excluded zero-shape gamma edge or upper collapse."),
        ("shape1+shape2", "Use the scalar positive tilt on -u.",
         "The mean u is strictly above the lower cut and strictly below its zero-rate limit.",
         "Lower collapse or excluded zero-rate power."),
    ]:
        exact(family, mask, f"**Proved.** {coord}{body} See {beta_ref}.",
              criterion, failure, "Unique when attained.")
    for mask, body, collapse, face in [
        ("all free", f"Use the full list in {tg_ref}.", "Constants collapse at their common value.",
         "zero-shape gamma, power, lognormal or hard-wall power"),
        ("shape1", f"Use the fixed-gamma-shape comparison in {power_ref}.",
         "Constants collapse at their common value.", "integrable power on the retained window"),
        ("scale", f"Use the held-scale escape list in {power_ref}, including the exact wall trace.",
         "Constants diverge at or above the held log scale, or at the finite upper cut, in the forward coordinate.",
         "zero-shape, power or hard-wall power"),
        ("scale+shape1", f"Use the held-shape held-scale comparison in {power_ref}.",
         "Constants at the projected held log scale collapse.", "log-uniform"),
    ]:
        power_comparison(family, mask, coord + body + " " + collapse, collapse, face)

normal_ref = "[normal truncation tests](2026-11-27-trunc-expfam-math-report.md#norm-free-truncateknown)"
power_comparison("gengamma", "all free", f"Apply {tg_ref} in both signs and include the "
                 f"attained normal seam via the {normal_ref} in log coordinates. Constants collapse.",
                 "Constants collapse at their common value.",
                 "signed zero-shape gamma, power or hard-wall power")
power_comparison("gengamma", "Q", f"For nonzero Q use the held-shape signed comparison in "
                 f"{tg_ref}. At Q=0 apply the {normal_ref}. Constants collapse at their common value.",
                 "Constants collapse at their common value.", "integrable power")
power_comparison("gengamma", "Q+mu", f"For nonzero Q use the fixed template about the "
                 f"held log-mode in {tg_ref}. At Q=0 use the held-normal-mean test in the "
                 f"{normal_ref}. Constants at the projection of mu collapse.",
                 "Constants at the projected log-mode collapse.", "log-uniform")
exact("gengamma", "Q+sigma", r"**Proved.** For nonzero Q use $u=x^{Q/\sigma}$ "
      "and the scalar positive tilt on $-u$ with fixed shape $Q^{-2}$. At $Q=0$ "
      f"use the held-normal-SD location test in the {normal_ref}.",
      "For nonzero Q the mean u lies strictly above the lower cut and below the zero-rate mean. "
      "For Q=0 the mean log observation lies strictly between its cuts.",
      "For nonzero Q the excluded zero-rate power or lower-u collapse. For Q=0 endpoint collapse.",
      "Unique when attained.")
power_comparison("gengamma", "mu+sigma", f"Use the two signed log-uniform wall limits "
                 f"in {tg_ref} and include $Q=0$ as a feasible candidate.",
                 "Constants at the relevant lower signed cut collapse if that cut is at or above the corresponding signed wall.",
                 "log-uniform on the retained part below a signed wall")
held_ref = "[held Prentice comparisons](#prentice-generalized-gamma-with-one-location-or-scale-hold)"
power_comparison("gengamma", "mu", f"Use the signed fixed-mode analysis in {held_ref} "
                 "and the held-normal-mean seam. The candidate bound includes log-uniform "
                 "and the permitted power at the wall.",
                 "Constants at the projected log-mode collapse.", "signed hard-wall power or gated log-uniform")
power_comparison("gengamma", "sigma", f"Use the signed fixed-scale analysis in {held_ref} "
                 "and the held-normal-SD seam. The excluded list has finite-power "
                 "signed tilts and arbitrary log-uniform cutoffs subject to integrability.",
                 "Constants at finite log cuts collapse.", "signed power or gated log-uniform")

gen_geometry = "Use $z=(x/S)^d, p=shape1,q=shape2,d=shape3$."
fixed("genbeta", "scale+shape1+shape2+shape3")
for mask in ("all free", "shape1", "shape1+shape3", "shape3"):
    exact("genbeta", mask, f"**Proved.** Apply {end_ref}. Free scale and q allow a "
          "subunit upper shape and an endpoint spike at the sample maximum. All samples "
          "at the lower cut instead allow width collapse.", "There is no finite positive MLE.",
          "Unbounded likelihood by a subunit upper-endpoint spike or retained-width collapse.",
          "No finite maximizing vector.")
exact("genbeta", "scale", "**Proved.** Use the [full held-scale comparison](#generalized-beta-with-scale-held-and-all-shapes-free) for distinct interior observations. Constants or observations at the parent endpoint have unbounded likelihood.",
      "Strictly interior distinct data attain iff a finite score root meets every eligible excluded height.",
      "Constant collapse, singular parent contact, or a winning zero-shape, log-distance or hard-wall power limit.",
      frontier=root_frontier)

power_comparison("gb2", "shape1+shape3",
                 f"{gb} The fixed-shape large- and small-power arguments in {power_ref} "
                 "extend to any fixed p,q>0. The mode is $m+\\log(p/q)/d$, the "
                 "small-power log slope is bounded by $d\\max(p,q)$, and the large-power "
                 "slope bounds away from the mode hold with limiting magnitudes $pd,qd$. "
                 "Consequently the only regular excluded height is log-uniform on a "
                 "bounded log window.",
                 "Every constant sample collapses by free modal alignment.", "log-uniform")

fixed("fpareto", "min+scale+shape1+shape2+shape3")
for mask in (
    "all free", "scale", "scale+shape1", "scale+shape1+shape2",
    "scale+shape1+shape3", "scale+shape2", "scale+shape3",
    "shape1", "shape1+shape2", "shape1+shape3", "shape2", "shape3",
):
    body = f"**Proved.** Apply {end_ref} to the free threshold. "

    exact("fpareto", mask, body + "At least one factor of $dp$ is free, giving "
          "a subunit product. This gives a lower-endpoint spike if the sample "
          "minimum is below the upper cut and a width collapse at upper-cut equality.",
          "No finite positive MLE exists.",
          "Unbounded likelihood by a lower-endpoint spike or upper-cut width collapse.",
          "No finite maximizing vector.")

gb_full = "[varying-power GB2 and Burr boundaries](#complete-varying-power-gb2-and-burr-boundaries)"
upgrades = [("gb2", mask) for mask in ("all free", "shape1", "shape3")]
upgrades += [(family, mask) for family in ("burr", "invburr") for mask in ("all free", "scale")]
for family, mask in upgrades:
    direction = "First reciprocate observations and scale. " if family == "invburr" else ""
    if mask == "scale":
        collapse = "Constants at the lower log cut or at or below the held log scale collapse."
        face = "nonpositive power, zero-shape Burr, or lower-wall power with its contact penalty"
    else:
        collapse = "Every constant sample collapses by free modal alignment."
        face = "eligible zero-shape beta-prime, gamma, power, two-piece power, lognormal or hard-wall power"
    holds = ("Use p=1 and respect the held scale when specified." if family != "gb2" else
             "Keep the indicated beta shape fixed and discard every incompatible boundary.")
    power_comparison(family, mask, f"{direction}Apply the {gb_full}. {holds} {collapse}", collapse, face)

gen_ref = "[held-scale generalized beta comparisons](#generalized-beta-with-scale-and-one-beta-shape-held)"
for mask, body, criterion, failure in [
    ("scale+shape1", f"Use the held lower shape proof in {gen_ref}.",
     "For distinct interior data a finite root meets both eligible excluded heights.",
     "Constant collapse, upper parent singularity, or an eligible zero-upper-shape or log-distance-power boundary."),
    ("scale+shape2", f"Use the held upper shape proof in {gen_ref} and the unit-shape and endpoint rules in {end_ref}.",
     "At unit upper shape use the product rule. Otherwise, away from collapse and parent contact, a finite root meets every eligible excluded height.",
     "Upper parent contact at nonunit shape, collapse at an eligible upper cut, or an excluded zero-lower-shape, power or log-distance-power limit."),
    ("shape1+shape2+shape3", f"Use the free endpoint with all shapes held proof in {end_ref}.",
     "At unit upper shape use the endpoint rule. Above one and away from width collapse a finite score root meets the eligible pure-power limit.",
     "Subunit endpoint spike, retained-width collapse or a winning finite-window power."),
]:
    exact("genbeta", mask, f"**Proved.** {body}", criterion, failure, frontier=root_frontier)
for mask, free in [("scale+shape3", "both shapes"), ("scale+shape1+shape3", "upper shape"), ("scale+shape2+shape3", "lower shape")]:
    exact("genbeta", mask, f"**Proved.** {gen_geometry} Use the beta moment tests with {free} free for strictly interior observations. At closed parent contact use {end_ref}. If the upper shape is held at one the product test applies also at contact.",
          "The beta moment test on interior data, or the unit-upper-shape product test at compatible contact.",
          "The eligible beta edges or collapse for interior data, and a zero or singular density at nonunit parent contact.",
          "Unique when a finite positive fit exists.")

free_q_masks = ("shape1+shape2", "shape2", "shape2+shape3")
for mask in free_q_masks:
    exact("genbeta", mask, "**Proved.** Use the [held superunit upper shape comparison](#generalized-beta-with-free-endpoint-and-held-superunit-upper-shape) for q above one and distinct observations. For all other cases consult the width, subunit, and unit rules there and in the moving-support proof.",
          "At q=1 use the product and cutoff rule. At q>1 distinct data attain iff a finite score root meets every eligible excluded height.",
          "A subunit endpoint spike, collapse, or a winning eligible zero-shape, power, log-distance-power or hard-wall power boundary.",
          frontier=root_frontier)
exact("fpareto", "scale+shape1+shape2+shape3", "**Proved.** Apply the [threshold-only comparison](#shifted-gb2-with-only-the-threshold-free), retaining the feasible closed contact when the low-end exponent is zero.",
      "Outside the stated spike and width-collapse cases a feasible threshold score root, join or closed contact meets the excluded uniform height, if eligible.",
      "A lower-endpoint spike, upper-cut width collapse, or a winning bounded-window uniform.", frontier=root_frontier)
translated = {"min": "all free", "min+shape1": "shape1", "min+shape3": "shape3",
              "min+shape1+shape3": "shape1+shape3", "min+scale+shape2": "scale+shape2"}
for mask, gb_mask in translated.items():
    anchor = f"gb2-{gb_mask.replace('+', '')}-truncateknown".replace("all free", "free").lower()
    exact("fpareto", mask, f"**Proved.** For strictly positive excesses use [the GB2 comparison](#{anchor}) on the translated window. At compatible threshold contact at least one factor of dp can vary, permitting a subunit product and an infinite endpoint density.",
          "Positive excesses satisfy the corresponding GB2 criterion. Threshold contact has no finite positive fit.",
          "A GB2 failure on positive excesses or a singular density at threshold contact.",
          "Unique when attained." if gb_mask == "scale+shape2" else None,
          frontier="" if gb_mask == "scale+shape2" else root_frontier)

exact("fpareto", "scale+shape2+shape3", "**Proved.** Apply the [shifted GB2 comparison with scale held](#shifted-gb2-with-scale-and-both-low-end-factors-held). Use the feasible fixed-cut join and the feasible support contact in addition to the interior scores.",
      "With low-end product at least one and distinct data, a feasible score root, cut join or closed contact meets every eligible excluded height.",
      "Subunit endpoint spike, constant collapse, or a winning eligible zero-tail-shape or decreasing exponential, including uniform at zero rate.",
      frontier=root_frontier)

exact("fpareto", "shape1+shape2+shape3", "**Proved.** Use the [fixed-shape shifted GB2 comparison](#shifted-gb2-with-the-shapes-held), including the small-scale contact exponent and its exact critical envelope. The shifted Lomax specialization gives the explicit nonunique sample (1,2) on [1,infinity) at q=2 and p=d=1.",
      "With dp at least one and distinct data, the contact exponent must be nonnegative and a feasible stationary or join candidate must reach the best excluded height.",
      "Subunit spike, constant or small-scale collapse, a winning critical collapse envelope, or an eligible uniform or shifted power.",
      frontier=root_frontier)

rows.append(("fpareto", "shape1+shape2+shape3", MOD, "case:exact-nonunique-sample",
             "At unit shape2 and shape3 and shape1 two, the sample (1,2) on [1,infinity) has a continuum of maximizing threshold-scale pairs.",
             "exact", "written", f"{REPORT}#fpareto-shape1shape2shape3-truncateknown",
             root_frontier, "trunc-gb2-math"))

exact("fpareto", "shape2+shape3", "**Proved.** Use the [free tail-shape comparison](#shifted-gb2-with-tail-shape-scale-and-threshold-free). A sufficiently small positive tail shape makes the small-scale contact exponent negative even when the fixed lower-end product is at least one.",
      "No finite positive MLE exists.",
      "A subunit endpoint spike or a small-scale atom path gives unbounded likelihood.",
      "No finite maximizing vector.")

rows.append(("genbeta", "scale+shape2", MOD, "case:exact-nonunique-sample",
             "With scale two and shape2 one, sample (1,2) on (0,2] has all positive shape1 and shape3 pairs with product 2/log(2) as its entire maximizing set.",
             "exact", "written", f"{REPORT}#genbeta-scaleshape2-truncateknown",
             "Determine cardinality for nonunit held upper shape.", "trunc-gb2-math"))

with (ATLAS / "modifier-atlas.csv").open() as source:
    masks = [(r["family"], r["held"]) for r in csv.DictReader(source)
             if r["modifier"] == MOD and r["family"] in families]
assert len(masks) == len(set(masks))
assert set(masks) == set(entries), (set(masks) - set(entries), set(entries) - set(masks))
assert len(rows) == len({row[:4] for row in rows})

if sys.argv[1:] == ["--csv"]:
    with (DATA / "proofs.csv").open("w", newline="") as target:
        writer = csv.writer(target, lineterminator="\n")
        writer.writerow(("family", "held", "modifier", "question", "claim",
                         "direction", "status", "evidence", "frontier", "origin"))
        writer.writerows(sorted(rows))
elif not sys.argv[1:]:
    print("## Family and held-mask results\n")
    for family in families:
        for mask in (mask for name, mask in masks if name == family):
            print(f"### {family} {'free' if mask == 'all free' else mask} truncate:known\n")
            body, frontier = entries[family, mask]
            print(body + "\n")
            if frontier:
                print(f"**Open.** {frontier}\n")
    print("## Summary\n")
    print("| Family | Masks | Full criterion for finite positive fits | Full cardinality claim |")
    print("| --- | ---: | ---: | ---: |")
    complete = {row[:2] for row in rows if row[3] == "criterion"}
    uniqueness = {row[:2] for row in rows if row[3] == "uniqueness"}
    for family in families:
        total = sum(name == family for name, _ in masks)
        full = sum(name == family for name, _ in complete)
        unique_count = sum(name == family for name, _ in uniqueness)
        print(f"| {family} | {total} | {full} | {unique_count} |")
    print(f"| Total | {len(masks)} | {len(complete)} | {len(uniqueness)} |")
else:
    raise SystemExit("Use no arguments for the register or --csv to export proofs.")
