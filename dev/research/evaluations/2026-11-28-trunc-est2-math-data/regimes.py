"""Render regime statements and proof rows from the reviewed known-window atlas."""
import argparse
from collections import Counter, defaultdict
import csv
from pathlib import Path
import re

DATA = Path(__file__).resolve().parent
ROOT = DATA.parents[3]
ATLAS = ROOT / "dev/research/existence-atlas"
REPORT = DATA.with_name("2026-11-28-trunc-est2-math-report.md")
ORIGIN = "trunc-est2-math"
CONTINUOUS = "laplace logis cauchy gumbel hnorm hcauchy std unif".split()
DISCRETE = "bbinom bnbinom cmb cmp dlaplace dnorm dunif dweibull hyper lgser nhyper pig skellam yulesimon".split()
FAMILIES = DISCRETE + CONTINUOUS
WINDOWS = {
    "lower": "[m,b]",
    "upper": "[a,M]",
    "both": "[m,M]",
}
STUDENT_OPEN = ("A joint compactification as df approaches two or infinity with varying location "
                "or effective scale, followed by finite-to-face comparisons and cardinality.")

def read(path):
    with path.open(newline="") as source:
        return list(csv.DictReader(source))


def tight_notes(family, held, mode):
    if family in ("unif", "dunif"):
        lower = "m" if mode != "upper" or "min" not in held.split("+") else r"\max(l,a)"
        upper = "M" if mode != "lower" or "max" not in held.split("+") else r"\min(u,b)"
        return f"The target effective ends are ${lower}$ and ${upper}$ by the uniform endpoint formulas."
    if family in ("lgser", "yulesimon", "bnbinom", "dweibull", "cmp", "pig"):
        lo = 1 if family in ("lgser", "yulesimon") else 0
        if mode == "upper":
            return f"The ordinary effective lower cell is $\\max({lo},\\lceil a\\rceil)$, and the upper cell is $M$. Check included atoms separately."
        return f"The ordinary effective lower cell is $m\\ge {lo}$. At $m={lo}$ all lower padding is flat. Check included atoms separately."
    if family in ("bbinom", "cmb", "hyper", "nhyper", "skellam"):
        return ("Intersect the displayed lattice window with each candidate design's support. "
                "An included endpoint atom or shorter carrier can change the inclusion test.")
    if family in ("dnorm", "dlaplace"):
        left = "m" if mode != "upper" else r"\lceil a\rceil"
        right = "M" if mode != "lower" else r"\lfloor b\rfloor"
        if family == "dnorm":
            return f"The integrated normal carrier runs from ${left}$ to ${right}+1$ when those cuts are finite."
        return f"Use the center and distance comparisons on integer cells from ${left}$ to ${right}$."
    if family in ("hnorm", "hcauchy"):
        if mode == "upper":
            return "For compatible location $\\mu\\le m$, use effective interval $[\\max(a,\\mu),M]$."
        end = "b" if mode == "lower" else "M"
        return f"For compatible location $\\mu\\le m$, the effective interval is $[m,{end}]$."
    if mode == "both":
        return "On positive width both cuts have observed densities. Use the bounded-window faces."
    if mode == "lower":
        return "The minimum is at the lower cut. If $b=\\infty$, use the right-ray faces."
    return "The maximum is at the upper cut. If $a=-\\infty$, use the left-ray faces."


def customize(family, held, mode, rows):
    """Specialize reviewed claims where the new window yields a sharper result."""
    def put(question, claim, direction="exact", frontier=""):
        rows[question] = dict(claim=claim, direction=direction, frontier=frontier)

    if family in ("cauchy", "std") and held in ("scale", "nu+sd"):
        rows.pop("case:nonunique", None)  # The known window has neither cut at an extremum.
    if family == "cmb":
        rows.pop("case:cross-size-tie", None)
    if mode != "lower":
        rows.pop("case:ray-attainment", None)
        rows.pop("case:positive-shape-ray", None)
    if family == "hcauchy" and mode != "upper":
        rows.pop("case:left-ray-unique", None)
    if family == "cmb" and rows["failure"]["direction"] == "one-sided":
        put("failure", "At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately.")
    if family in ("unif", "dunif"):
        if family == "unif":
            put("criterion", "On compatible data, the two optimal effective ends must have strictly positive separation.", "iff")
            put("failure", "If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.")
        else:
            put("criterion", "Every compatible sample attains at the optimal effective ends, including coincident ends.", "iff")
            put("failure", "Incompatibility gives no fit. Coincident optimal ends give an attained atom.")
        put("uniqueness", "The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).")
    if family == "hnorm" and mode != "upper":
        if held == "":
            put("criterion", "Require nonconstant data and a strictly positive inward second-moment score at the optimized nonpositive exponential edge.", "iff")
            put("failure", "Constant-data collapse, or the optimized decreasing exponential or uniform when its inward second-moment score is nonpositive.")
            put("uniqueness", "The identified concave wedge gives one coefficient vector whenever it attains. The lower cut has a plateau exactly when the fitted location equals the minimum.")
        if held == "sigma":
            put("criterion", "Exactly the compatible nonconstant samples attain.", "iff")
            put("failure", "A positive-width constant at the lower cut diverges along the remote-location tilt. Coincident cuts follow the compatible shrinking-window rule.")
            put("uniqueness", "One location from the constrained monotone mean score. The lower cut has a plateau exactly when the fitted location equals the minimum.")
    if family in ("hnorm", "hcauchy") and mode == "upper":
        if "mu" in held.split("+"):
            put("case:null-effective-window", "A constant at the held location with fixed lower cut at or below that location has a null tight event. Increasing the estimated upper cut above the location and then shrinking it back gives divergent likelihood and the weak point mass.")
        else:
            put("case:null-effective-window", "A constant at the estimated upper cut has divergent likelihood whenever the fixed lower cut permits the data. Take the parent location at the constant and let the estimated upper cut approach it from above.")
    if "uniqueness" not in rows:
        if family in DISCRETE:
            claim = "If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau."
        elif family in ("hnorm", "hcauchy"):
            claim = "For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients."
        else:
            claim = "Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density."
        put("case:bound-cardinality", claim)
    if family == "std" and "criterion" not in rows:
        rows["case:divergent-tie"]["frontier"] = STUDENT_OPEN
        if held != "mean+sd":
            put("case:balanced-endpoints", "On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.", "one-sided", STUDENT_OPEN)
    if family == "laplace" and held in ("", "sigma"):
        claim = ("On a bounded window with only the two endpoints occupied, unequal counts attain "
                 "at the mean-matched exponential on the majority-side plateau; equal counts escape to uniform.")
        if held == "sigma":
            claim = ("On a bounded window with only the two endpoints occupied, the majority-side "
                     "exterior plateau attains, and both plateaus attain at equal counts.")
        put("case:endpoint-histogram", claim, "one-sided")
    if family == "logis" and held in ("", "scale"):
        claim = ("On a bounded window with only the two endpoints occupied, the optimized signed "
                 "exponential wins without attainment, including uniform at balance.")
        if held == "scale":
            claim = ("On a bounded window with only the two endpoints occupied, every location "
                     "attains at equal counts; otherwise the fixed-rate signed exponential wins without attainment.")
        put("case:endpoint-histogram", claim, "one-sided")


def wrapper(family, question, claim):
    if family in ("unif", "dunif"):
        return claim
    if question == "criterion":
        if family in DISCRETE:
            return "On the displayed window, a compatible retained singleton attains. Otherwise " + claim[0].lower() + claim[1:]
        return "On a compatible positive-width tight window, " + claim[0].lower() + claim[1:]
    if question == "failure":
        if family in CONTINUOUS:
            return claim + " Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma."
        return claim + " A compatible retained singleton attains."
    if question == "uniqueness":
        if family in DISCRETE:
            return claim + " Nominal real estimated cuts are nonunique at every attained fit."
        return claim + " A free cut ties exactly on the zero-removed-mass plateau."
    return claim


def generate():
    reviewed = defaultdict(dict)
    for row in read(ATLAS / "modifier-proofs.csv"):
        if row["family"] in FAMILIES and row["modifier"] == "truncate:known":
            reviewed[row["family"], row["held"]][row["question"]] = row
    assert len(reviewed) == 104
    eligible = {(r["family"], r["held"], r["modifier"]): r for r in read(ATLAS / "modifier-atlas.csv")}
    output, sections = [], []
    summary = Counter()
    for family in FAMILIES:
        for fam, held in sorted(reviewed):
            if fam != family:
                continue
            source = reviewed[family, held]
            display = held or "all free"
            for mode, window in WINDOWS.items():
                modifier = f"truncate:{mode}-estimated"
                route = eligible[family, display, modifier]
                assert route["implementation_status"] == "not-implemented"
                title = f"{family} {display} {modifier}"
                anchor = title.lower().replace(" ", "-").replace("+", "").replace(":", "")
                rows = {q: dict(row) for q, row in source.items()}
                customize(family, held, mode, rows)
                sections.extend((f"### {title}", "", f"**Proved window reduction.** Use ${window}$. " + tight_notes(family, held, mode), ""))
                primary = ("criterion", "failure", "uniqueness")
                for question in sorted(rows, key=lambda q: (primary.index(q) if q in primary else len(primary), q)):
                    row = rows[question]
                    claim = wrapper(family, question, row["claim"])
                    if row.get("evidence"):
                        links = []
                        for item in row["evidence"].split("; "):
                            path, fragment = item.split("#")
                            assert (ROOT / path).is_file()
                            links.append(f"[Fixed-window evidence]({Path(path).name}#{fragment})")
                        reference = " " + " · ".join(links)
                    else:
                        reference = ""
                    label = question.removeprefix("case:").replace("-", " ").capitalize()
                    sections.extend((f"**{label} ({row['direction']}).** {claim}{reference}", ""))
                    output.append(dict(
                        family=family, held=held, modifier=modifier, question=question, claim=claim,
                        direction=row["direction"], status="written",
                        evidence=f"{REPORT.relative_to(ROOT)}#{anchor}", frontier=row["frontier"], origin=ORIGIN))
                if "criterion" not in rows:
                    sections.extend((f"**Open.** {STUDENT_OPEN}", ""))
                elif "uniqueness" not in rows:
                    sections.extend(("**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.", ""))
                summary[family, "masks"] += 1 if mode == "lower" else 0
                for question in ("criterion", "failure", "uniqueness"):
                    if question in rows:
                        summary[family, question, rows[question]["direction"]] += 1
    sections.extend(("## Summary", "",
                     "Counts below are for estimated-bound regimes. The failure and uniqueness columns give complete and one-sided main claims. Scoped case rows are additional.", "",
                     "| Family | Held masks | Regimes | Attainment iff | Failure complete / one-sided | Cardinality complete / one-sided |",
                     "| --- | ---: | ---: | ---: | ---: | ---: |"))
    for family in FAMILIES:
        masks = summary[family, "masks"]
        counts = [summary[family, question, direction] for question, direction in
                  (("criterion", "iff"), ("failure", "exact"), ("failure", "one-sided"),
                   ("uniqueness", "exact"), ("uniqueness", "one-sided"))]
        c, f, fp, u, up = counts
        sections.append(f"| {family} | {masks} | {3*masks} | {c} | {f} / {fp} | {u} / {up} |")
    sections.append("")
    assert len({(r["family"], r["held"], r["modifier"], r["question"]) for r in output}) == len(output)
    return output, "\n".join(sections)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    rows, section = generate()
    report = REPORT.read_text()
    assert all(ch >= " " or ch in "\n\t" for ch in report)
    assert all(line.count("$") % 2 == 0 for line in report.splitlines())
    start = "<!-- Regimes generated below -->\n"
    end = "<!-- End generated regimes -->"
    before, rest = report.split(start)
    old, after = rest.split(end)
    path = DATA / "proofs.csv"
    if args.write:
        REPORT.write_text(before + start + section + "\n" + end + after)
        with path.open("w", newline="") as out:
            writer = csv.DictWriter(out, fieldnames=rows[0], lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
    else:
        assert old == section + "\n", "regime statements differ from generated text"
        assert read(path) == rows, "proof rows differ from generated rows"
    anchors = {re.sub(r"[^a-z0-9_-]", "", title.lower().replace(" ", "-"))
               for title in re.findall(r"^### (.*)$", section, re.MULTILINE)}
    assert all(row["evidence"].split("#")[1] in anchors for row in rows)
    assert len({row["evidence"] for row in rows}) == 312
    print(f"PASS: {len(rows)} proof rows, 312 regimes, 104 held masks, no implemented routes in scope")


if __name__ == "__main__":
    main()
