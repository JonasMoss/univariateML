"""Regenerate the proposed proof rows and the regime sections of the report."""

import csv
import re
from collections import Counter, defaultdict
from pathlib import Path


DATA = Path(__file__).resolve().parent
EVALUATIONS = DATA.parent
ROOT = EVALUATIONS.parents[2]
ATLAS = EVALUATIONS.parent / "existence-atlas"
REPORT = EVALUATIONS / "2026-11-28-trunc-est3-math-report.md"
FAMILIES = """
ged sged sn snorm sstd st lst slash huber ht tlambda bhatt emg logitnorm
dblpareto lgamma weibull lomax llogis gompertz frechet invexp power kumar pareto2
""".split()
WINDOWS = {
    "lower": (
        "Use $[m,b]$ with the observed minimum included and $h\\le b$.",
        "At a constant $m=h=b$, only the left side can be opened.",
        "On a positive-mass fit the lower cut is unique at $m$, or ranges over all cuts at or below $m$ if the fitted support starts there.",
    ),
    "upper": (
        "Use $[a,h]$ with the observed maximum included and $a\\le m$.",
        "At a constant $a=m=h$, only the right side can be opened.",
        "On a positive-mass fit the upper cut is unique at $h$, or ranges over all cuts at or above $h$ if the fitted support ends there.",
    ),
    "both": (
        "Use $[m,h]$ with both extremes included. For nonconstant data, the sample mean of each strictly monotone transformed statistic is strictly between its effective cuts.",
        "Every constant gives a nominal singleton, with either side available to open.",
        "On a positive-mass fit a cut can vary outward from its sample extreme exactly when the fitted support ends on that side at the extreme.",
    ),
}
MOVING_SUPPORT = {"frechet", "ht", "pareto2", "power", "tlambda"}
PARTIAL = {
    "ged": "Joint scale and shape limits, including compact-uniform contact at large shape.",
    "sged": "Coupled shape, skew and scale motion, including moving uniform edges and skew joins.",
    "sn": "Joint diverging skew and shrinking or remote Gaussian scale, including a moving half-normal join.",
    "snorm": "Joint skew and scale motions at the moving one-sided normal join.",
    "sstd": "Joint df, scale and skew limits near df two and along the surviving and compressed half-kernels.",
    "tlambda": "Coupled support-edge motion near shapes zero and one and the allowed large negative shape end.",
    "bhatt": "Joint Gaussian scale, uniform width and location growth or contact, including remote cut slopes.",
    "emg": "Coupled rate, Gaussian scale and location motion in the retained translated-exponential and Gaussian transition.",
}
WINDOW_CASE = (
    "If the nominal tight window is a singleton at compatible data, every inward "
    "positive-density expansion gives a shrinking-window atom and an unbounded "
    "likelihood. If no such expansion exists there is no positive fit."
)


def read_csv(path):
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def normal_mask(held):
    return "" if held in {"", "all free"} else held


def clause(text):
    return text[0].lower() + text[1:]


def new_claim(question, claim, direction="exact", frontier=""):
    return dict(question=question, claim=claim, direction=direction, frontier=frontier)


def specialized(family, held, regime, claims):
    """Apply boundary geometry and add results that need more than transfer."""
    output = dict(claims)
    extra = []
    lower_tight = regime in {"lower", "both"}

    if "case:endpoint-pair" in output:
        old = output["case:endpoint-pair"]
        old["claim"] = (
            "For distinct endpoint data (m,h) on the tight window [m,h], "
            "the excluded physical uniform gives the strict supremum."
            + (" A held nu must be at least one." if family in {"ged", "sged"} else "")
        )
    if family in {"lst", "slash"} and "case:nonunique" in output:
        hold = "df=3 and sigma=1/sqrt(3)" if family == "lst" else "sigma=1"
        output["case:nonunique"]["claim"] = (
            f"With {hold}, take one copy each of -100 and 100 and 50 copies "
            "each of -4 and 4 on tight [-100,100]. The trial center 4 beats "
            "the centered model and the remote uniform; symmetry forces at "
            "least two finite maximizers."
        )
        extra.append("[Exact reflected maxima proof](#exact-reflected-maxima-for-lst-and-slash)")

    if family == "gompertz" and held == "a":
        if regime == "lower":
            output["uniqueness"]["claim"] = (
                "At sample (0), fixed upper cut infinity and held a>0, every b>0 "
                "and every estimated lower cut at or below zero maximize."
            )
        else:
            output.pop("uniqueness", None)
        extra.append(
            "At all-zero data, finite positive retained width selects the rate-$a$ "
            "face. The unbounded upper window gives the entire $b>0$ plateau. "
            "An upper-estimated cut at zero cannot retain positive mass with "
            "a fixed lower cut below zero."
        )

    if family == "ht":
        if lower_tight:
            output.pop("case:left-ray-unique", None)
        if held == "nu+sigma":
            extra.append(
                "For fixed shape and scale, a free endpoint is profiled over "
                "$c\\le m$ if the lower cut is $m$. If the fixed lower cut is "
                "$-\\infty$ and $m<h$, the maximizing threshold is $c=m$. "
                "For any fitted $c=m$ an estimated lower cut is flat outwards."
            )
        if held in {"mu", "mu+sigma", "mu+nu"}:
            extra.append(
                "Use the effective lower cut $\\max(a_*,\\mu)$. In particular an "
                "upper-estimated physical left ray can have bounded effective "
                "width and must use the bounded-window tests. Where df can "
                "tend to zero, apply the reviewed effective-ray normalizer."
            )

    if family == "tlambda":
        extra.append(
            "At positive shape check the fitted endpoint density before evaluating "
            "the overlap. At an admissible shape-one uniform use the shortest "
            "overlap rule, with the nominal mass plateaus restored."
        )

    if family == "dblpareto":
        if held == "left_rate+right_rate":
            if regime == "upper":
                output["case:nonunique"]["claim"] = (
                    "With both rates one, data (1,1) and fixed lower cut 1/e, "
                    "every kink scale at least one maximizes. Reflect the log window "
                    "and the slope argument for the lower-estimated case."
                )
            elif regime == "both":
                output.pop("case:nonunique", None)
        extra.append(
            "Use the log images of the tight cuts. In moving-kink comparisons the "
            "estimated cuts are sample knots, so evaluate each of those only once. "
            "A winning finite cut still represents its entire exterior kink plateau."
        )

    if family == "frechet":
        if not held:
            for question in ("criterion", "failure"):
                output[question]["frontier"] = (
                    "Minimum-minority escape comparison remains open. For upper "
                    "estimation, the general exact-half case with a<m is also open."
                )
            if lower_tight:
                output["criterion"]["claim"] = "Attainment requires strictly fewer than half the observations at m."
                output["failure"]["claim"] = (
                    "A minimum majority gives divergent lower contact; exact half "
                    "gives the strict atom envelope equal to the product of inverse squared gaps."
                )
            else:
                output["criterion"]["claim"] = (
                    "Attainment requires at most half the data at the minimum, strictly "
                    "less than half when the fixed lower cut a=m. For a=-infinity, equal frequencies at exactly "
                    "two distinct values attain on the full shape-one fiber."
                )
                output["case:half-at-lower-cut"]["claim"] += " Here this requires fixed a=m."
                output["case:upper-ray-pair"] = new_claim(
                    "case:upper-ray-pair",
                    "For a=-infinity and two distinct values with equal counts, "
                    "shape one with any location below m and its profiled scale is "
                    "the entire maximizing set. The upper bound h is unique.",
                )
            extra.append(
                "For the Gumbel witness take $\\lambda\\to\\infty$, "
                "$\\mu+\\sigma\\to z$ and $\\sigma/\\lambda\\to t>0$. The CDF and "
                "sample densities converge to those of "
                "$G(x)=\\exp[-e^{-(x-z)/t}]$ before dividing by the tight retained mass."
            )
        elif held == "lambda":
            output["criterion"] = new_claim(
                "criterion",
                "Outside the divergence branches, attainment is equivalent to "
                "a positive finite candidate reaching the best eligible escape height.",
                "iff",
                "General cardinality and an effective root and equality comparison remain open.",
            )
            output["failure"] = new_claim(
                "failure",
                "For finite lower cut compare shifted power, integrable nonnegative "
                "exponential tilt and critical contact. On a physical upper ray compare "
                "the reflected exponential and the contact height. Negative contact exponent "
                "and the applicable constant branches diverge.",
            )
            extra.append(
                "Let $s=\\lambda$, $j$ count minima, $E=s(n-j)-j$, "
                "$d=m-\\mu$, $q=(\\sigma/d)^s$. Negative $E$ gives divergence. "
                "At $E=0$ optimize $q^ne^{-jq}/D(q)^n$ times "
                "$s^n\\prod_{x_i>m}(x_i-m)^{-1-s}$, where $D(q)=1-e^{-q}$ at "
                "lower contact and $D(q)=1$ below it. On a physical upper ray use "
                "the exact scale profile in [Fréchet upper ray](#fréchet-upper-ray)."
            )
            if regime == "upper":
                output["case:upper-ray-pair"] = new_claim(
                    "case:upper-ray-pair",
                    "With a=-infinity and two distinct values of equal frequency, "
                    "shape above one selects the reflected exponential, shape one "
                    "attains on the entire profiled fiber, and shape below one diverges.",
                )
        if held in {"sigma", "lambda+sigma"} and regime == "upper":
            extra.append(
                "On the physical upper-only ray, distinct data attain because all "
                "excluded profile heights vanish. Constants at the upper end diverge. "
                "[Upper-ray proof](#fréchet-rays-with-scale-held)"
            )

    if family == "power":
        if held in {"", "beta"}:
            output["uniqueness"]["claim"] = (
                "Shape, when free, is unique. With free alpha a lower-estimated fit "
                "uses alpha=h if b>h and any alpha>=h if b=h."
                if regime == "lower" else
                "Shape, when free, is unique. The whole endpoint locus has "
                "alpha>=h, upper cut>=h and min(alpha, upper cut)=h."
            )
        if held in {"alpha", "alpha+beta"}:
            extra.append(
                "For held $\\alpha$, first require $h\\le\\alpha$. At $h=\\alpha$ "
                "the upper-estimated bound has its outward plateau."
            )
        extra.append(
            "Use $L=\\max(0,a_*)$ and "
            + ("$u=h$ after profiling the free endpoint." if held in {"", "beta"} else "$u=\\min(b_*,\\alpha)$.")
        )

    if family == "pareto2":
        if held == "shape":
            output["uniqueness"].update(
                claim="Whenever attained with the lower cut at m, any winning pole "
                "p has the full threshold-scale fiber p<c<=m, scale=c-p. "
                "With an estimated lower cut the joint fiber also requires max(c,cut)=m. "
                "The finite-width witness establishes nonuniqueness in this design.",
                frontier="General winning-pole cardinality on bounded windows remains open.",
            )
        if held in {"shape", "min+shape", "scale+shape"} and regime == "lower":
            output["case:lower-tight-ray"] = new_claim(
                "case:lower-tight-ray",
                "For infinite fixed upper cut, the retained pole likelihood has a strictly "
                "decreasing log-scale score on distinct data. Use its unique root after "
                "applying the open or included lower scale constraint for the holds, "
                "or its excluded-end limit. Restore the entire pole fiber when applicable.",
            )
        extra.append(
            "Check threshold contact at $L_x=\\max(a_*,c)$. A closed observation "
            "there has positive parent density. An estimated lower cut ties outward "
            "exactly at $c=m$. The fixed-pole fibers and lower-ray score are in "
            "[Power and moving endpoints](#power-and-moving-endpoints)."
        )

    if family == "weibull" and held == "shape" and regime == "upper":
        extra.append("On $a\\le0$ the transformed lower cut is zero, so lower equality is impossible for admissible data.")
    if family in {"invexp", "frechet"} and (family == "invexp" or held == "lambda+mu"):
        extra.append(
            "The decreasing coordinate sends the estimated upper cut to the "
            "lower transformed endpoint, and the estimated lower cut to the upper endpoint."
        )
    if family == "kumar":
        extra.append(
            "The effective upper cut is clipped at 1. For the coordinate "
            "$y=-\\log x$, a retained end at 1 is $y=0$, and a retained end at "
            "zero is $y=\\infty$. The order is reversed."
        )
    if family == "logitnorm":
        extra.append("The logit cuts use $-\\infty$ at zero and $+\\infty$ at one. Both are finite when both cuts are estimated.")
    if family == "lgamma":
        extra.append("The transformed lower cut is $\\log(\\max(1,a_*))$. A lower-estimated cut makes it strictly positive; an upper-estimated cut makes the transformed upper cut finite.")

    return output, extra


def main():
    fields, prior = read_csv(ATLAS / "modifier-proofs.csv")
    _, atlas = read_csv(ATLAS / "modifier-atlas.csv")
    estimated = {
        (r["family"], normal_mask(r["held"]), r["modifier"]): r for r in atlas
        if r["family"] in FAMILIES and r["modifier"].endswith("-estimated")
    }
    inventory = defaultdict(list)
    for row in prior:
        if row["family"] in FAMILIES and row["modifier"] == "truncate:known" and row["status"] == "written":
            inventory[row["family"], row["held"]].append(row)
    assert len(inventory) == 149
    assert {family for family, _ in inventory} == set(FAMILIES)
    assert all(row["implemented"] == "false" and row["implementation_status"] == "not-implemented" for row in estimated.values())

    parts = ["\n## Regime summary\n"]
    parts += [
        "“Iff” counts masks with a proved complete attainment criterion in each "
        "of their three designs. Other masks have scoped witnesses or necessary "
        "conditions. Every regime has the exact bound profile and singleton test.\n",
        "| Family | Reviewed masks | Iff masks | Estimated regimes |",
        "| --- | ---: | ---: | ---: |",
    ]
    rows = []
    tallies = defaultdict(Counter)
    sections = []
    for family in FAMILIES:
        masks = sorted((held, items) for (base, held), items in inventory.items() if base == family)
        for held, items in masks:
            proofs = {row["question"]: row for row in items}
            known = items[0]["evidence"].split(";")[0]
            path, _, anchor = known.partition("#")
            source_link = Path(path).name + "#" + anchor
            for regime, (window, collapse, bounds) in WINDOWS.items():
                modifier = f"truncate:{regime}-estimated"
                assert (family, normal_mask(held), modifier) in estimated
                claims, notes = specialized(family, normal_mask(held), regime,
                                            {key: dict(row) for key, row in proofs.items()})
                label = held or "free"
                heading = f"{family} {label} {modifier}"
                anchor = re.sub(r"[^a-z0-9_ -]", "", heading.lower()).replace(" ", "-")
                evidence = f"{REPORT.relative_to(ROOT)}#{anchor}"
                text = [
                    f"### {heading}",
                    "",
                    f"{window} {collapse} Use the [singleton test](#joint-profiling-and-boundary-conventions) there.",
                    "",
                ]
                claims["case:collapsed-window"] = new_claim("case:collapsed-window", WINDOW_CASE)
                if family not in MOVING_SUPPORT and family != "gompertz":
                    bounds = {
                        "lower": "The estimated lower cut is unique at $m$ whenever a fit attains.",
                        "upper": "The estimated upper cut is unique at $h$ whenever a fit attains.",
                        "both": "Both estimated cuts are unique at the observed extremes whenever a fit attains.",
                    }[regime]
                if "uniqueness" not in claims:
                    claims["case:bound-set"] = new_claim("case:bound-set", bounds.replace("$", ""))
                for question in ("criterion", "failure", "uniqueness"):
                    if question not in claims:
                        continue
                    claim = claims[question]
                    sentence = claim["claim"]
                    if question == "uniqueness":
                        if family == "power" and normal_mask(held) in {"", "beta"}:
                            pass
                        elif family in MOVING_SUPPORT or family == "gompertz":
                            sentence += " " + bounds.replace("$", "")
                        else:
                            sentence += " The estimated cuts are uniquely tight at attained fits."
                        claim["claim"] = sentence
                    prefix = {"criterion": "Attainment", "failure": "Failure", "uniqueness": "Cardinality"}[question]
                    scope = {
                        "criterion": " On compatible positive width, ",
                        "failure": " On compatible positive width, ",
                        "uniqueness": " ",
                    }[question]
                    text += [f"**Proved · {prefix}.**{scope}{clause(sentence) if scope.endswith(', ') else sentence}", ""]
                    if question in {"criterion", "failure"}:
                        claim["claim"] = "On compatible positive tight width, " + clause(sentence) + " Apply the separate singleton-width and support tests first."
                for question, claim in claims.items():
                    if question.startswith("case:") and question not in {"case:collapsed-window", "case:bound-set"}:
                        text += ["**Proved case.** " + claim["claim"], ""]
                    rows.append(dict(
                        family=family, held=held, modifier=modifier, question=question,
                        claim=claim["claim"], direction=claim["direction"], status="written",
                        evidence=evidence, frontier=claim["frontier"], origin="trunc-est3-math",
                    ))
                if notes:
                    text += [" ".join(notes), ""]
                if "uniqueness" not in claims:
                    text += [bounds + " General parent cardinality remains open beyond the stated cases.", ""]
                if "criterion" not in claims:
                    text += ["**Open.** " + PARTIAL[family] + " The stated cases leave the general attainment and failure classification open.", ""]
                text += [f"[Known-window proof]({source_link})", ""]
                sections.extend(text)
                tallies[family]["masks"] += 1
                tallies[family]["iff"] += claims.get("criterion", {}).get("direction") == "iff"

    for family in FAMILIES:
        regimes = tallies[family]["masks"]
        iff = tallies[family]["iff"]
        assert regimes % 3 == iff % 3 == 0
        parts.append(f"| {family} | {regimes // 3} | {iff // 3} | {regimes} |")
    parts += ["", "## Results by family, hold and estimated cut", "", *sections]
    key_fields = ("family", "held", "modifier", "question")
    keys = {tuple(row[key] for key in key_fields) for row in rows}
    assert len(keys) == len(rows)
    assert len({key[:3] for key in keys}) == 447
    assert all(row["status"] == "written" and row["frontier"] != "TODO" for row in rows)
    keys_and_anchors = {(row["family"], row["held"], row["modifier"], row["evidence"]) for row in rows}
    assert len({anchor for *_, anchor in keys_and_anchors}) == len(keys_and_anchors) == 447

    with (DATA / "proofs.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
    report = REPORT.read_text()
    start = "<!-- regime results -->"
    end = "<!-- end regime results -->"
    before, _, tail = report.partition(start)
    _, _, after = tail.partition(end)
    assert before and after
    REPORT.write_text(before + start + "\n" + "\n".join(parts) + "\n" + end + after)
    print(f"{len(rows)} written rows, 447 regimes, 149 masks, {sum(t['iff'] for t in tallies.values())} iff regimes")


if __name__ == "__main__":
    main()
