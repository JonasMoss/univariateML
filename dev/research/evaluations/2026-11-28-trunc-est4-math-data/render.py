"""Render the mask-by-design report and its reviewed-scope proof proposals."""

import csv
import re
from collections import Counter, defaultdict
from pathlib import Path

from families import LOWER_ONLY, MOVING, POSITIVE, geometry

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
REPORT = HERE.with_name("2026-11-28-trunc-est4-math-report.md")
ATLAS = ROOT / "dev/research/existence-atlas"
SOURCES = {"TASK-461", "TASK-462"}
DESIGNS = {
    "lower-estimated": ("m", "B"),
    "upper-estimated": ("A", "M"),
    "both-estimated": ("m", "M"),
}
QUESTIONS = ("criterion", "failure", "uniqueness")
FAMILIES = set(POSITIVE) | set(LOWER_ONLY) | set(MOVING) | {"dgamma"}
PROLOGUE = ("report-base.md", "counts.md", "submodels.md")
EVALUATIONS = "dev/research/evaluations/"


def anchor(heading):
    return re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")


def read_rows(path):
    with path.open(newline="") as source:
        reader = csv.DictReader(source)
        return reader.fieldnames, list(reader)


def source_links(rows):
    evidence = set()
    for row in rows.values():
        if row["evidence"]:
            evidence.update(row["evidence"].split("; "))
    return " · ".join(
        f"[{('Independent review' if 'mod-rev' in ref else 'Fixed-window derivation')}]"
        f"({ref.removeprefix(EVALUATIONS)})"
        for ref in sorted(evidence)
    )


def count_geometry(design, held):
    intervals = {
        "lower-estimated": (
            r"$L=m,\ U=\lfloor B\rfloor$, with infinite $B$ leaving $U$ infinite.",
            r"Adjacent occupied cells exhaust the window iff $U=M=m+1$.",
            r"A constant is always in the first cell, but the window is a singleton only if $U=m$.",
        ),
        "upper-estimated": (
            r"$L=\max(0,\lceil A\rceil),\ U=M$, including $L=0$ when $A=-\infty$.",
            r"Adjacent occupied cells exhaust the window iff $L=m=M-1$.",
            r"A constant is always in the last cell, but the window is a singleton only if $L=M$.",
        ),
        "both-estimated": (
            r"$L=m,\ U=M$. The latent gamma window is $[m,M+1]$.",
            r"Two occupied adjacent cells always exhaust this window.",
            r"A constant always gives a genuine retained cell, with conditional mass one.",
        ),
    }
    interval, adjacent, constant = intervals[design]
    special = {
        "": adjacent,
        "shape": r"The rate-infinity atom is at $L$, so only a constant there gets its positive limiting height.",
        "rate": r"The infinite-shape atom is eligible only at finite $U$. When $L=0$ the zero-shape atom replaces the grouped inverse-power exponential.",
        "rate+shape": r"The sole parent vector has positive probability at every retained integer, so no coefficient comparison is needed.",
    }[held]
    return f"Use {interval} {constant} {special} The removed parent mass is strictly positive exactly when a change of a nominal cut adds an integer cell at or above zero."


def dgamma_claims(design, held):
    constant = design == "both-estimated"
    if held == "rate+shape":
        return {
            "criterion": ("iff", "Every compatible sample and consistent cut has a positive finite fit at the supplied shape and rate."),
            "failure": ("exact", "There is no coefficient escape or null compatible count window."),
            "uniqueness": ("exact", "The held vector is unique, and the estimated nominal real cuts form the rounding intervals in the grouped-gamma test."),
        }
    if not held:
        occupancy = (
            "Two adjacent occupied cells always attain by matching the empirical odds."
            if constant else "Two adjacent occupied cells attain iff they exhaust the retained set."
        )
        failures = (
            "Only the eligible grouped shape-zero, rate-zero and corner faces can prevent attainment on separated-cell data."
            if constant else "A constant on a multisite window has an excluded occupied-cell atom; two adjacent occupied cells with any extra cell have an excluded empirical Bernoulli. Separated-cell failure is a winning eligible grouped shape-zero, rate-zero or corner face."
        )
        return {
            "criterion": ("iff", f"Singletons attain. Constants on multisite windows do not attain. {occupancy} For separated cells a finite fit must meet all eligible grouped faces."),
            "failure": ("exact", failures),
        }
    ends = (
        "grouped rate-zero power on finite windows and first-cell atom at infinite rate"
        if held == "shape" else
        "grouped shape-zero inverse-power exponential for L>0 or zero-cell atom for L=0, and last-cell atom only on finite windows"
    )
    return {
        "criterion": ("iff", "A singleton attains with every compatible coefficient. Otherwise a finite scalar root must meet both endpoint heights, or use the strict empirical odds test on two occupied cells exhausting the window."),
        "failure": ("exact", f"The competitive endpoints are {ends}. At an ineligible endpoint the height is zero."),
    }


def scoped_case(family, question, design):
    if question == "case:exact-nonunique-sample":
        if family == "fpareto":
            if design != "lower-estimated":
                return None
            return r"Sample $(1,2)$ with fixed $B=\infty$, holding shape1=2 and shape2=shape3=1, has tight window $[1,\infty)$. The likelihood $4s/(1+s)^3$ is maximized at $s=1/2$, where $s=\mathrm{scale}+1-\mathrm{min}$. The full family of tight winners includes every $\mathrm{min}\in(1/2,1]$ with $\mathrm{scale}=\mathrm{min}-1/2$. Exactly at $\mathrm{min}=1$ lower-cut loosenings tie."
        if family == "genbeta":
            return r"Hold scale=2 and shape2=1. For $(1,2,2)$ choose fixed $A=1$ or $B=2$ when required. On $[1,2]$ the empirical log mean $2\log(2)/3$ lies strictly between the uniform-in-log mean $\log(2)/2$ and $\log 2$. The unique positive product maximizes the likelihood, giving all positive shape1 and shape3 pairs with that product."
    if question == "case:nonunique" and family == "triang":
        return "The triangular example with held a=-1 and b=2 has two disjoint intervals of winning modes on the displayed tight design. Use the exact triangular calculation above."
    if question == "case:reverse-exponential":
        return "At xi=-1 with location and scale free, apply the reverse-exponential mean test and the endpoint fiber in the explicit moving-endpoint submodels."
    if question == "case:flat-uniform":
        return "At xi=-1 the joint optimum for a nonconstant sample is attained by the full support and cut set in the explicit uniform GPD submodel above. On a constant the supremum is infinite."
    if question == "case:upper-only-exponential":
        return "The remote increasing exponential face in the reviewed shape range is integrable when the displayed tight upper cut is finite, including on a left ray. An infinite upper cut rules it out as a positive-height face."
    if question == "case:adjacent-cells":
        return "For free shape and rate, a sample using both adjacent cells of a two-cell retained set has a continuum of parent maximizers matching the empirical odds. Extra retained cells turn this height into an excluded Bernoulli."
    return None


def build():
    fields, source = read_rows(ATLAS / "modifier-proofs.csv")
    _, inventory = read_rows(ATLAS / "modifier-atlas.csv")
    availability = {(r["family"], r["held"], r["modifier"]): r for r in inventory}
    masks = defaultdict(dict)
    for row in source:
        if row["origin"] in SOURCES and row["modifier"] == "truncate:known" and row["family"] in FAMILIES:
            masks[row["family"], row["held"]][row["question"]] = row
    masks = {key: value for key, value in masks.items() if any(r["status"] == "written" for r in value.values())}
    assert {f for f, _ in masks} == FAMILIES
    assert len(masks) == 184
    intro, counts, submodels = [(HERE / p).read_text() for p in PROLOGUE]
    intro, label = intro.split("## Regime results", 1)
    parts = [intro, counts, "\n", submodels, "\n## Regime results", label]
    proposed = []
    frontiers = defaultdict(set)
    totals = Counter()

    for (family, held), by_question in sorted(masks.items()):
        name = held if held else "free"
        for design, (lower, upper) in DESIGNS.items():
            modifier = f"truncate:{design}"
            heading = f"{family} {name} {modifier}"
            lookup = (family, held or "all free", modifier)
            assert availability[lookup]["implementation_status"] == "not-implemented", lookup
            assert availability[lookup[:2] + ("truncate:known",)]["implementation_status"] == "not-implemented", lookup
            totals[family, "regimes"] += 1
            evidence = f"{REPORT.relative_to(ROOT)}#{anchor(heading)}"
            parts.append(f"\n### {heading}\n\nTight nominal window $[{lower},{upper}]$.\n\n")
            parts.append(
                (count_geometry(design, held) if family == "dgamma"
                 else geometry(family, design, lower, upper)) + "\n\n"
            )
            cut_claim = "Each estimated real cut has a nontrivial rounding plateau at every attained fit." if family == "dgamma" else (
                "Every attaining fit has the unique nominal estimated cut or cuts at the sample extremes."
                if family in POSITIVE else
                "For each winning parent vector a nominal estimated cut is strict on that side unless the occupied extreme is its corresponding support endpoint, where all outward loosenings tie."
            )

            def add(question, direction, claim, frontier=""):
                proposed.append(dict(family=family, held=held, modifier=modifier, question=question,
                                     claim=claim, direction=direction, status="written",
                                     evidence=evidence, frontier=frontier, origin="trunc-est4-math"))
                totals[family, question if question in QUESTIONS else "cases"] += 1

            claims = dgamma_claims(design, held) if family == "dgamma" else {}
            for question in QUESTIONS:
                old = by_question.get(question)
                if question in claims:
                    direction, text = claims[question]
                elif family == "dgamma" or old is None or old["status"] != "written":
                    frontier = (old or {}).get("frontier", "").strip() or {
                        "criterion": "Complete the positive-width attainment test outside the proved subcases.",
                        "failure": "Bound and classify the remaining competitive paths at this hold.",
                        "uniqueness": "Isolate and rank the winning parent roots or determine the maximizing components.",
                    }[question]
                    frontiers[family, held].add(frontier)
                    parts.append(f"**{question.capitalize()} · Open.** {frontier}\n\n")
                    continue
                else:
                    direction, text = old["direction"], old["claim"]
                if family == "dgamma":
                    scope = "Apply the grouped-gamma tests with these cell ends."
                elif question == "criterion":
                    scope = "Apply this criterion on the specified positive-mass tight-window branch, after the null-slice and singular tests."
                elif question == "failure":
                    scope = ("The stated parent paths are witnesses on the tight branch." if direction == "one-sided"
                             else "This gives the competing tight-window paths on the stated branch.")
                else:
                    scope = cut_claim
                parts.append(f"**{question.capitalize()} · Proved.** {text} {scope}\n\n")
                add(question, direction, f"{text} {scope}", (old or {}).get("frontier", ""))

            if family != "dgamma":
                null_claim = (
                    "At a null tight window on a constant sample, a permitted positive-mass expansion with positive finite density gives unbounded likelihood and a weak point mass; without such a slice apply the singular and zero-density rules."
                )
                if design == "both-estimated":
                    null_claim += " Every constant has a null tight window."
                parts.append(f"**Null and endpoint cases · Proved.** {null_claim}\n\n")
                add("case:null-tight-slice", "exact", null_claim)
            parts.append(f"**Estimated cuts · Proved.** {cut_claim}\n\n")
            add("case:bound-cardinality", "exact", cut_claim)

            if family in {"expweibull", "pgw"}:
                fixed_power = "shape" in held and (family == "pgw" or "exponent" in held)
                zero_claim = (
                    "With a zero observation and valid positive retention, the inside endpoint density is singular for held endpoint power below one, and zero for held endpoint power above one."
                    if fixed_power else
                    "With a zero observation and valid positive retention, selecting a subunit endpoint power gives an infinite extended likelihood and no positive finite maximum."
                )
                if held == "exponent+shape":
                    zero_claim += " At unit endpoint power use the scale-score comparison against bounded uniform, with zero broad height on an unbounded ray and small-scale divergence only at an all-zero sample."
                if family == "pgw" and held == "scale+shape":
                    zero_claim += " At held shape one, on a positive-width window use the exponent-score comparison against the bounded (S+x)^(-1) kernel, with zero broad height on a ray and divergence at exponent infinity for all zeros."
                parts.append(f"**Zero observations · Proved.** {zero_claim} See the zero-observation proof above.\n\n")
                add("case:zero-observation", "exact", zero_claim)

            if family == "dgamma" and held != "rate+shape":
                if design == "both-estimated":
                    sentence = "A constant sample has likelihood one at every compatible value of the free parent coefficients."
                    parts.append(f"**Singleton case · Proved.** {sentence}\n\n")
                    add("case:singleton", "exact", sentence)
                if held:
                    sentence = "On a retained set of exactly two cells, with both occupied, the odds test gives at most one fitted parent coefficient."
                    parts.append(f"**Two-cell uniqueness · Proved.** {sentence}\n\n")
                    add("case:two-cell-uniqueness", "exact", sentence)

            for question, old in sorted(by_question.items()):
                if not question.startswith("case:") or old["status"] != "written":
                    continue
                claim = scoped_case(family, question, design)
                if claim is None and question in {"case:exact-nonunique-sample", "case:nonunique"}:
                    continue
                if claim is None:
                    claim = "On the stated positive-width tight branch, " + old["claim"][0].lower() + old["claim"][1:]
                parts.append(f"**{question.removeprefix('case:').replace('-', ' ').capitalize()} · Proved.** {claim}\n\n")
                add(question, old["direction"], claim, old["frontier"])

            parts.append(source_links(by_question) + "\n")
    parts.append("\n## Summary\n\n")
    parts.append("| Family | Held masks | Regimes | Attainment rows | Failure rows | Cardinality rows | Scoped cases |\n"
                 "| --- | ---: | ---: | ---: | ---: | ---: | ---: |\n")
    for family in sorted(FAMILIES):
        values = [totals[family, "regimes"] // 3, totals[family, "regimes"]]
        values.extend(totals[family, key] for key in (*QUESTIONS, "cases"))
        parts.append(f"| {family} | " + " | ".join(map(str, values)) + " |\n")
    parts.append(f"\nThe table covers {len(masks)} held masks and {3 * len(masks)} regimes. "
                 f"The companion CSV contains {len(proposed)} proved rows including scoped cases. "
                 "An attainment or failure entry can retain the stated hold and sample restrictions. "
                 "The bound-set rows cover every regime with an attained positive finite fit. "
                 "[Proof rows](2026-11-28-trunc-est4-math-data/proofs.csv)\n")
    parts.append((HERE / "closing.md").read_text())
    parts.append("\n| Family | Held | Remaining work on the displayed positive-width windows |\n"
                 "| --- | --- | --- |\n")
    for (family, held), conditions in sorted(frontiers.items()):
        parts.append(f"| {family} | {held or 'all free'} | {' '.join(sorted(conditions))} |\n")
    parts.append("\nScope restrictions and reviews also apply where an attainment comparison is available "
                 "but the set of all divergent laws, equal-height branches or exact finite winner counts is unresolved.\n")
    return fields, parts, proposed


def main():
    fields, parts, rows = build()
    with (HERE / "proofs.csv").open("w", newline="") as out:
        writer = csv.DictWriter(out, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    REPORT.write_text("".join(parts))
    print(f"Verified atlas availability for 552 regimes and rendered {len(rows)} proof rows.")


if __name__ == "__main__":
    main()
