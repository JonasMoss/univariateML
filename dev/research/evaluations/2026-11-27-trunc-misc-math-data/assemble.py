"""Assemble mask decisions, validate their atlas keys, and check evidence anchors."""
import argparse
import csv
from pathlib import Path
import re

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
REPORT = HERE.parent / (HERE.name.removesuffix("-data") + "-report.md")
FAMILIES = {
    "pareto3", "pareto4", "expweibull", "pgw", "fatigue", "naka", "dgamma",
    "sgomp", "gev", "gpd", "foldnorm", "nsbeta", "triang",
}
QUESTIONS = ("criterion", "failure", "uniqueness")
HELD_ALL = {
    "pareto3": "min+scale+shape", "pareto4": "min+scale+shape1+shape2",
    "expweibull": "exponent+scale+shape", "pgw": "exponent+scale+shape",
    "fatigue": "alpha+beta+mu", "naka": "scale+shape", "dgamma": "rate+shape",
    "sgomp": "b+eta", "gev": "mu+sigma+xi", "gpd": "mu+sigma+xi",
    "foldnorm": "mu+sigma", "nsbeta": "max+min+shape1+shape2", "triang": "a+b+c",
}


def read_csv(path):
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle))


def scalar(criterion, failure, unique=""):
    return criterion, failure, unique


FIXED = scalar(
    "The compatible positive finite conditional likelihood is attained at the held model.",
    "There is no parameter escape on the single feasible coefficient vector.",
    "The feasible coefficient vector is unique.",
)
MOMENT = "Every attained fit on the stated domain is unique by strict concavity."
SHAPE_ONLY = scalar(
    "For positive data outside projected-scale constant divergence, a scalar shape-score root must reach the small-shape threshold.",
    "For positive data the only competitive ends are projected-scale collapse and, on a bounded window away from zero, log-uniform.",
)
SCALE_ONLY = scalar(
    "For positive data outside the lower-cut constant collapse case, a scale-score root must reach the larger explicit endpoint height.",
    "For positive data, small-scale lower-cut constant collapse or the bounded broad-scale power face.",
)
DIRECT_SPIKE = (
    "If a free endpoint's corresponding power is below one or its shape can be chosen below one, "
    "contact yields divergence on positive-compatible data. Shrinking at the opposite cut can also diverge."
)
PARETO_FULL = scalar(
    "Positive-compatible data admit no finite MLE.",
    "At any sample minimum choose a fixed shape below one and approach contact, "
    "or shrink retained width on an upper-cut constant.",
    "There is no attained positive finite fit.",
)
PARETO_SCALE = scalar(
    "With a held threshold and all shapes held, compare scalar scale roots with the explicit two ends (P3), on the positive finite likelihood domain.",
    "Broad bounded power, narrow integrable tail power, or a contact atom with a critical finite trace or divergence.",
)
PARETO_THRESHOLD = scalar(
    "With scale and all shapes held, apply P4 to scalar threshold roots and included contact and cut joins on the positive finite likelihood domain.",
    "Spike for shape below one, upper-cut constant shrinking, or remote uniform on bounded windows.",
)
DECISIONS = {
    ("pareto3", "min+shape"): PARETO_SCALE,
    ("pareto4", "min+shape1+shape2"): PARETO_SCALE,
    ("pareto3", "scale+shape"): PARETO_THRESHOLD,
    ("pareto4", "scale+shape1+shape2"): PARETO_THRESHOLD,
    ("pareto4", "min+scale+shape2"): scalar(
        "On positive finite carriers, the empirical transformed mean lies strictly inside the positive-rate range (P2).",
        "Lower constant collapse or zero-rate uniform in the log-survival coordinate.", MOMENT,
    ),
    ("expweibull", "scale+shape"): scalar(
        "On positive observations use E1's strictly increasing transformed mean range.",
        "On positive observations, upper-cut collapse or the integrable exponent-zero derivative carrier.", MOMENT,
    ),
    ("expweibull", "exponent+scale"): SHAPE_ONLY,
    ("pgw", "exponent+scale"): SHAPE_ONLY,
    ("expweibull", "exponent+shape"): SCALE_ONLY,
    ("pgw", "exponent+shape"): SCALE_ONLY,
    ("pgw", "scale+shape"): scalar(
        "On positive data, outside collapse, compare exponent-score roots with the small-exponent boundary height.",
        "On positive data, lower-cut constant collapse or a finite-upper window log-coordinate carrier.",
    ),
    ("fatigue", "beta+mu"): scalar(
        "The empirical squared normal coordinate has mean strictly inside the inverse-shape tilt range.",
        "Projected-center collapse or the integrable uniform-in-normal-coordinate face.", MOMENT,
    ),
    ("fatigue", "alpha+mu"): scalar(
        "Attains for positive-compatible data except a constant at an accessible finite concentration cut.",
        "Small-scale lower-cut or large-scale upper-cut constant collapse.",
    ),
    ("fatigue", "alpha+beta"): scalar(
        "Outside the upper-cut constant, compare threshold-score roots with the remote decreasing exponential height or zero.",
        "Upper-cut constant shrinking or a remote integrable decreasing exponential.",
    ),
    ("naka", ""): scalar(
        "Nonconstant data attain iff no integrable edge or corner of the gamma quadrant passes its inward scores.",
        "Collapse or one of the integrable gamma zero-shape, zero-rate and log-uniform faces.", MOMENT,
    ),
    ("naka", "scale"): scalar(
        "The mean of log squared observation minus squared observation divided by held scale is strictly inside the positive-tilt range.",
        "Projected-center collapse or zero-tilt log-uniform on a window cut away from both ends.", MOMENT,
    ),
    ("naka", "shape"): scalar(
        "The empirical squared observation mean is strictly inside the decreasing-rate range.",
        "Lower-cut collapse or finite-window power.", MOMENT,
    ),
    ("dgamma", ""): scalar(
        "Singleton windows are flat. On multiple cells constants do not attain. Adjacent occupied cells attain iff they exhaust the window, "
        "and nonadjacent occupied cells attain iff a finite model reaches the grouped-face threshold.",
        "A first or other occupied cell atom, an adjacent empirical Bernoulli, or an integrable grouped zero-shape, zero-rate or corner face.",
    ),
    ("dgamma", "rate"): scalar(
        "On multiple cells, compare scalar shape roots with the explicit grouped-kernel or atom heights. A singleton is flat.",
        "Small-shape first atom or grouped inverse-power exponential, or large-shape last atom.",
    ),
    ("dgamma", "shape"): scalar(
        "On multiple cells compare scalar rate roots with the explicit grouped-power or atom heights. A singleton is flat.",
        "Small-rate grouped power or large-rate first-cell atom.",
    ),
    ("sgomp", ""): scalar(
        "Nonconstant data attain on a right ray, or iff a finite value reaches the optimized onset-kernel height on a bounded window.",
        "Constant collapse or a bounded-window onset kernel proportional to (1+tx)exp(tx) for t nonnegative.",
    ),
    ("sgomp", "b"): scalar(
        "Attains except on a constant at the finite upper cut.", "Constant-at-upper-cut collapse.",
    ),
    ("sgomp", "eta"): scalar(
        "Outside a lower-cut constant, compare finite rate roots to the broad-scale threshold.",
        "Lower-cut constant collapse or uniform on a bounded window.",
    ),
    ("gpd", ""): scalar(
        "Every compatible sample on the fitted shape branch admits a divergent path.",
        "Positive-shape scale collapse with threshold at the sample minimum, or uniform shrinking on an upper-cut constant.",
        "There is no attained positive finite fit.",
    ),
    ("gpd", "mu+xi"): scalar(
        "Use G1's two-end comparison, decreasing exponential moment test or uniform width minimization, according to held shape.",
        "Threshold collapse, shrinking at a lower cut, integrable tail power, critical contact trace or bounded uniform, according to shape.",
    ),
    ("gpd", "mu+sigma"): scalar(
        "Compare finite shape roots and included joins to G2's remote height after testing shrinking support and threshold divergence.",
        "Shrinking support, divergent threshold atom, bounded log-uniform or the attained all-threshold right-ray plateau.",
    ),
    ("foldnorm", ""): scalar(
        "Nonconstant data attain iff a finite fit reaches the optimized bounded-window cosh height or zero on a right ray.",
        "Constant collapse or the bounded-window cosh family, including uniform.",
    ),
    ("foldnorm", "mu"): scalar(
        "Outside constants at the projected held location, compare finite scale roots with the bounded uniform height or zero.",
        "Projected-location collapse or bounded uniform.",
    ),
    ("foldnorm", "sigma"): scalar(
        "Attains except for constants at a finite upper cut.",
        "Concentration at an occupied upper cut under remote location.",
    ),
}

TRIANGLE = (
    "Eliminate coefficients cellwise from the cleared conditional-height equation. "
    "Attainment is equivalent to a finite included rightmost endpoint of the resulting height intervals and points."
)
OPEN = {
    "pareto3": "Bound joint pole, shape and scale escapes, including ties and the upper cut.",
    "pareto4": "Bound joint pole, two-shape and scale escapes, including ties and a simultaneous Weibull path.",
    "expweibull": "Exhaust joint shape/scale escapes and zero-observation strata. Bound limiting exponents when the limiting retained mass is zero.",
    "pgw": "Exhaust joint shape/scale escapes and zero-observation strata, in particular conditioning when the window lies below the threshold face.",
    "fatigue": "Control vanishing excess with simultaneous shape/scale changes and remote thresholds at varying rates.",
    "naka": "The stated positive-carrier moment tests settle this mask.",
    "dgamma": "Determine cardinality of joint stationary winners when occupied cells are nonadjacent.",
    "sgomp": "Show that the winning roots or parameter components have the asserted cardinality, or give an exact tie.",
    "gev": "Bound moving support, scale and shape escapes, including the critical tie and upper-only increasing exponential.",
    "gpd": "Compare joint threshold and shape escapes, especially separated lower cuts and negative-shape upper support.",
    "foldnorm": "Control the number and heights of conditional stationary roots, including ties with mu=0.",
    "nsbeta": "Exhaust joint remote endpoint and free shape paths, including exponential tilts when an endpoint and its power grow together.",
    "triang": "Find simple face-specific inequalities without algebraic elimination.",
}


def mask_decision(family, hold):
    if HELD_ALL[family] == hold:
        return FIXED
    if family == "triang":
        return TRIANGLE, "", "The rightmost included height fiber gives the exact maximizing set."
    if (family, hold) in DECISIONS:
        return DECISIONS[family, hold]
    parts = set(hold.split("+"))
    if family in {"pareto3", "pareto4"}:
        shape = "shape" if family == "pareto3" else "shape2"
        if "min" not in parts and shape not in parts:
            return PARETO_FULL
    if family == "nsbeta":
        if {"min", "max"} <= parts:
            return scalar(
                "On positive finite carriers transfer the ordinary beta inward-score or scalar moment test.",
                "On positive finite carriers, integrable zero-shape beta edges or corner, or collapse.",
                MOMENT,
            )
        if ("min" not in parts and "shape1" not in parts) or (
            "max" not in parts and "shape2" not in parts
        ):
            return scalar(
                "On the positive-compatible domain the free associated shape allows an arbitrarily high likelihood at a moving support contact.",
                "An endpoint power spike, or a shrinking retained interval for a constant pinned at the opposite cut.",
                "There is no attained positive finite fit.",
            )
        if {"shape1", "shape2"} <= parts:
            return scalar(
                "For shapes at least one, use the endpoint comparison with explicit remote powers and uniform. Free-side held shapes below one yield a spike on compatible data.",
                "On the positive-carrier domain there is a moving-endpoint spike, shrinking constant, integrable power or bounded uniform.",
            )
    return "", "", ""


def one_sided(family, hold):
    parts = set(hold.split("+"))
    claims = []
    if family == "pareto3" and "min" not in parts and "shape" in parts:
        claims.append(("case:threshold-spike", DIRECT_SPIKE))
    if family == "pareto4" and "min" not in parts and "shape2" in parts:
        claims.append(("case:threshold-spike", DIRECT_SPIKE))
    if family == "fatigue" and hold in {"", "alpha", "mu"}:
        claims.append(("case:constant-collapse", "On positive-compatible constants a free scale or threshold enables concentration."))
    if family in {"expweibull", "pgw"} and not hold:
        claims.append(("case:constant-collapse", "Every positive constant can be matched by scale and has a divergent free-shape path."))
    if family == "dgamma" and not hold:
        claims.append(("case:adjacent-cells", "Two occupied adjacent cells give a nonattained empirical Bernoulli on windows with other cells, and infinitely many fits on exactly two cells."))
    if family == "gev" and hold in {"", "xi"}:
        claims.append(("case:upper-only-exponential", "An increasing exponential is an accessible face on upper-only and bounded windows."))
        claims.append(("case:reverse-exponential", "In the held xi=-1 submodel the truncated reverse-exponential mean test and endpoint plateau give the exact winning set."))
    if family == "gev" and hold in {"", "mu"}:
        claims.append(("case:minimum-tie-collapse", "The xi=1 small-scale path diverges at a strict majority minimum, when mu can equal that minimum and the window has positive width."))
    if family == "gpd" and hold == "mu":
        claims.append(("case:threshold-contact", "Threshold ties allow divergent positive-shape scale collapse when shape and scale are free."))
    if family == "gpd" and hold == "xi":
        claims.append(("case:flat-uniform", "For xi=-1 width minimization gives every maximizer and all constant data diverge with both endpoints free."))
    if family == "foldnorm" and hold == "mu":
        claims.append(("case:half-normal", "At mu=0 the inverse-variance tilt has at most one maximum."))
    if family == "nsbeta" and not {"min", "max"} <= parts:
        claims.append(("case:endpoint-spike", DIRECT_SPIKE))
    if family == "triang":
        claims.append(("case:width-bound", "The density everywhere is bounded by twice the reciprocal retained width. Nonconstant data cannot diverge."))
        if hold == "a+b":
            claims.append(("case:nonunique", "At support [-1,2], window [0,1] and sample (0,1), modes in [-1,0] or [1,2] all maximize."))
    return claims


REFERENCES = {
    "pareto3": "Pareto III and IV", "pareto4": "Pareto III and IV",
    "expweibull": "Exponentiated Weibull and PGW", "pgw": "Exponentiated Weibull and PGW",
    "fatigue": "Fatigue", "naka": "Nakagami", "dgamma": "Discretized gamma",
    "sgomp": "Shifted Gompertz", "gev": "Generalized extreme value",
    "gpd": "Generalized Pareto", "foldnorm": "Folded normal",
    "nsbeta": "Nonstandard beta", "triang": "Triangular",
}
QUESTION_LABELS = {"criterion": "Attainment", "failure": "Failure", "uniqueness": "Cardinality"}


def slug(text):
    return re.sub(r"[^a-z0-9 -]", "", text.lower()).replace(" ", "-")


def artifacts():
    mask_atlas = read_csv(ATLAS / "modifier-atlas.csv")
    keys = [
        (row["family"], row["held"] if row["held"] != "all free" else "")
        for row in mask_atlas
        if row["family"] in FAMILIES and row["modifier"] == "truncate:known"
    ]
    assert len(keys) == len(set(keys))
    assert {family for family, _ in keys} == FAMILIES
    fieldnames = list(read_csv(ATLAS / "modifier-proofs.csv")[0])
    rows = []
    markdown = ["## Mask decisions", ""]
    for family, hold in keys:
        title = f"{family} {hold or 'free'} truncate:known"
        reference = REFERENCES[family]
        markdown += [f"### {title}", "", f"[Derivation](#{slug(reference)})", ""]
        evidence = f"{REPORT.relative_to(ROOT)}#{slug(title)}"
        decisions = mask_decision(family, hold)
        for question, claim in zip(QUESTIONS, decisions):
            frontier = ""
            if claim:
                direction = "iff" if question == "criterion" else "exact"
                status = "written"
            else:
                direction, status = "open", "unproved"
                if question == "uniqueness" and any(decisions):
                    frontier = "Isolate and rank the winning conditional score roots or determine the maximizing component."
                else:
                    frontier = OPEN[family]
                if family == "triang":
                    frontier = OPEN[family]
            display = claim or frontier
            grade = "Proved" if claim else "Open"
            markdown.append(f"- **{QUESTION_LABELS[question]} · {grade}.** {display}")
            rows.append((family, hold, "truncate:known", question, claim,
                         direction, status, evidence, frontier, "trunc-misc-math"))
        for question, claim in one_sided(family, hold):
            markdown.append(f"- **Proved subcase.** {claim}")
            rows.append((family, hold, "truncate:known", question, claim,
                         "one-sided", "written", evidence, "", "trunc-misc-math"))
        markdown.append("")
    return fieldnames, rows, "\n".join(markdown)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--render-mask-section", action="store_true")
    args = parser.parse_args()
    fields, rows, markdown = artifacts()
    if args.render_mask_section:
        print(markdown)
        return
    anchors = {slug(heading) for heading in re.findall(r"^#{2,3} (.+)$", REPORT.read_text(), re.M)}
    keys = set()
    for row in rows:
        record = dict(zip(fields, row, strict=True))
        key = row[:4]
        assert key not in keys
        keys.add(key)
        assert record["evidence"].split("#", 1)[1] in anchors, record
        assert record["evidence"].split("#", 1)[0] == str(REPORT.relative_to(ROOT))
        assert bool(record["claim"]) == (record["status"] == "written")
        assert bool(record["frontier"]) == (record["status"] == "unproved")
    assert {slug(reference) for reference in REFERENCES.values()} <= anchors
    assert markdown.strip() in REPORT.read_text()
    with (HERE / "proofs.csv").open("w", newline="") as handle:
        writer = csv.writer(handle, lineterminator="\n")
        writer.writerow(fields)
        writer.writerows(rows)
    print(f"{len({row[:2] for row in rows})} masks, {len(rows)} decisions, "
          f"{sum(row[6] == 'written' for row in rows)} written")


if __name__ == "__main__":
    main()
