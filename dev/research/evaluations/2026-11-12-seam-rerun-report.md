# logLik seam rerun — TASK-213

Job dated 2026-11-12, completed on the workstation. Baseline:
`63bc2200ead3cab6c957543b2f1765d419bb9c89`, branch `codex/seam-rerun`.

**The native returned-point seam is clear.** Double-only retains the excluded
bnbinom row 579, owned by TASK-212. EMG row 1311 remains guarded-open in double-only
mode after separate 600-second before/after attempts. Six distinct archive rows
had owned likelihood arithmetic repaired across Burr, generalized beta, binomial,
and negative hypergeometric. Separate Burr coefficient and negative-hypergeometric
search failures are documented below; an accurate candidate value does not certify
that candidate as an MLE.

The required validation is finished: native C++ coverage passed after the
family-only binomial retest, full double-only C++ passed 135/135 shards, R dev/full
passed, and the site check passed. No commits, board edits, dependency installations,
fixture rewrites, or protected probability-code changes were made.

## Census and independent attribution

The [checker](../../validation/seam-check.py) now compiles at `-O2`, replacing
its former `-O1`. Both native and
`BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS` modes were run with GCC 15.2.0.
Each census refitted all 11,185 [frozen archive rows](2026-11-07-fit-audit-data/cases.json.gz)
and 100 current goldens: **11,285 rows per mode**. Gengamma adds the hundredth
fitted family; it has a golden but no row in the older archive. Existing adapter
controls and holds were retained.

The comparison is reported logLik against the compensated sum of public C++ log
masses/densities at the returned coefficients, using absolute tolerance
`1e-10 * max(1, abs(value))`. Support comparisons use exact binary64 rational
arithmetic. This audits returned values and support, not every search decision,
coefficient, global certificate, or reason for a stopped fit. It concerns the C++
seam; R's separate stats-routing job is outside this census.

Every baseline disagreement was independently evaluated at exact binary64 inputs.
The [decimal sums](2026-11-12-seam-rerun-data/high-precision.json), computed at
150 and 250 digits, agree to better than `1e-120` relative. All nine mode/row
disagreements were fitter-side; none required a density-side or one-sided cusp
classification. After repair, the same independent values apply because data and
coefficient bit patterns are unchanged. The
[returned-point verification](2026-11-12-seam-rerun-data/returned-point-verification.json)
checks that identity explicitly and records the corrected errors.

Counts below include longer audit-guard retries. A guard is an open audit entry,
not a fitter status or a nonexistence claim.

| Outcome | Native before | Native after | Double-only before | Double-only after |
|---|---:|---:|---:|---:|
| Seam passes | 8,756 | 8,759 | 8,751 | 8,756 |
| Disagreements | 3 | 0 | 6 | 1 |
| Terminal fitter stops | 2,526 | 2,526 | 2,527 | 2,527 |
| Guarded-open fits | 0 | 0 | 1 | 1 |
| Returned fits outside exact support | 0 | 0 | 0 | 0 |

No density-side violations remain among these rows, and no cusp allowance was
needed. The density defects retained by the first report are not
inferred from a new refusal: its ten density-error rows all return seam passes
in both modes. The affected families are the four listed in the comparison below.

## Arithmetic repairs and invariance

Errors are normalized by `max(1, abs(independent value))` at the public returned point.
Native binomial row 457 already met the value tolerance; the correction improves
it too. Generalized-beta row 2568 was a violation only in native mode.

| Family | Archive rows | Violating modes | Worst error before | Worst error after | Change |
|---|---|---|---:|---:|---|
| burr | 636, 639 | both | 1.98e-7 | 1.23e-17 | Final held-shape values use the existing close-ratio likelihood sum at public coefficients. |
| genbeta | 2568 | native | 1.45e-10 | 2.47e-16 | Preserve the powered beta-logit coordinate through a compensated, centered beta-kernel evaluation. |
| binom | 457 | double-only | 1.31e-10 | 2.31e-15 | Use the stable public PMF sum when the discrepancy exceeds summation roundoff; retain already accurate ordinary values. |
| nhyper | 5175, 5230 | double-only | 3.44e-10 | 7.74e-17 | Re-evaluate the selected grid candidate through the stable hypergeometric PMF identity. |

The changes do not add or alter a search, start, or control; coefficients and
terminal statuses are preserved in the complete before/after archive replay.
The negative-hypergeometric correction does not change values used by recursive
face-search comparisons. No probability evaluator or truth fixture was edited.
New C++ and R regressions cover all four families and independent fixed points.
They do not freeze the known inaccurate Burr or capped negative-hypergeometric
coefficients as reference MLEs.

The complete before/after replay, followed by the late binomial-only replay in
both modes, found **zero coefficient or terminal-status drift and zero stop-message
drift**. All 100 golden coefficient vectors are bitwise unchanged in each mode.
The only golden refit likelihood payload change is genbeta, by `-2.05e-12` native
and `-2.50e-12` double-only. Golden files themselves are unchanged.
See [analysis.json](2026-11-12-seam-rerun-data/analysis.json) for every changed
likelihood payload and per-family counts.

## Every status change from the first report

The main comparison uses the **final census** in the
[first report](2026-11-11-seam-check-report.md). That report did not contain a
complete double-only census, so the double-only comparison below is against its
native final snapshot, not evidence of a new same-mode regression. The controlled
before/after comparison within this job covers both modes.

These grouped lists enumerate every changed row: 21 native transitions and 25
double-only transitions. All numbers are archive indices unless marked golden.

| Family and rows | First final outcome → current outcome | Modes |
|---|---|---|
| bbinom 75 | density disagreement → pass | both |
| burr 636, 639 | fitter disagreement → pass | both |
| genbeta 2568 | fitter disagreement → pass | both |
| lgamma 4435, 4438 | density disagreement → pass | both |
| lnorm 4601, 4604, 4611 | density disagreement → pass | both |
| pareto4 5665, 5666, 5668, 5669 | density disagreement → pass | both |
| cmb 837, 915 | guarded-open → terminal stop | both |
| dgamma 997 | guarded-open → terminal accuracy stop | both |
| fpareto 1599, 1600, 10717 | guarded-open → terminal limit stop | both |
| gengamma golden | absent → pass | both |
| bhatt 428 | stop → returned fit, seam pass | native |
| bhatt 364 | pass → convergence stop | double-only |
| dgamma 1025 | pass → coefficient-accuracy stop | double-only |
| emg 1311 | stop → guarded-open after 600 s | double-only |
| bnbinom 579 | pass → fitter disagreement | double-only |
| st 7040 | stop → returned fit, seam pass | double-only |

Thus all 13 retained disagreements from the first final census close on their
current returned points. Its ten density-side rows were already closed by the
merged master changes; this job closes its three remaining native fitter-value
rows. The newly observed double-only binomial and negative-hypergeometric value
errors are also closed. No formerly returned native fit is lost.

The complete row-level tables include holds, dataset labels, and stop reasons:
[native vs first final](2026-11-12-seam-rerun-data/status-changes-native-vs-first-final.csv),
[double-only vs first final](2026-11-12-seam-rerun-data/status-changes-double-vs-first-final.csv).
For the first report's initial, pre-repair census, the corresponding complete
comparisons are also retained:
[native vs first initial](2026-11-12-seam-rerun-data/status-changes-native-vs-first-before.csv),
[double-only vs first initial](2026-11-12-seam-rerun-data/status-changes-double-vs-first-before.csv).

## Remaining open entries and mathematical scope

### bnbinom 579: excluded fitter-value error, TASK-212

Only double-only mode violates the seam. Data are
`[1000000, 1000001, 1000003, 1000008, 1000011]`; holds are
`size=1.0672234787479615`, `beta=5.36473383510874`, and the returned
`alpha=0.0825043451682516`. Reported logLik is `-86.55337674075822`, versus
independent `-86.5533767289753`: relative error `1.36e-10`.
The public PMF agrees with the independent sum. No bnbinom source was edited.
Exact inputs and 250-digit values are in the
[verification record](2026-11-12-seam-rerun-data/returned-point-verification.json).
Missing: the owning job's stable fitter likelihood arithmetic.

### emg 1311: terminal double-only fit still missing

The baseline and post-fix 100-family probes each failed to obtain a terminal
result within separate 600-second wall-clock guards, after 30- and 120-second
attempts. Native mode returns its existing stop. Holds are
`sigma=1.0518147225573364`; the 12 observations and probe provenance are retained
in [the before retry](2026-11-12-seam-rerun-data/retry2-before-double.jsonl.gz)
and [the after retry](2026-11-12-seam-rerun-data/retry2-after-double.jsonl.gz).
Missing: a terminal default-control double-only result, hence there is no returned
likelihood to attribute. These are audit timeouts, not numerical or budget stops
introduced into the fitter, and elapsed times are not dedicated-core benchmarks.

All other guards settled. Feller–Pareto 1749/1750 return seam passes after about
31 seconds; generalized-beta 2495/2497/2499 return stops. The final native 2495
retry returned after about 95 seconds under its 600-second guard. Double-only
CMB 852 and EMG 1343 also returned stops.

### Burr 636/639: coefficient accuracy remains open

With shape1 held, an independent stationary solve in centered log coordinates,
repeated at 100 and 160 digits, gives shape2
`875130348796.006366...`, versus returned `875134658462.2328`:
relative coefficient error `4.9246e-6`. The fixed-shape likelihood is strictly
concave in power and powered log-scale; the numerical stationary calculation
and residuals are retained, not presented as an outward-rounded interval proof.

The maximum value is `124.546334057817267...`; the corrected public-point value
is `124.54633405249429`. Its relative value gap, `4.2739e-11`, meets the
likelihood-value contract despite the coefficient miss. Missing: refinement of
the held-shape score solve using accurate centered log ratios. This job preserves
the coefficients and statuses as instructed.

### Negative hypergeometric 5175/5230: capped candidates are not maxima

These existing uncertified grid results retain `m=20` and return respectively
`(N,r)=(6000029,1)` and `(6000027,3)` on the five large-count observations listed
above. At fixed returned r, increasing N by one strictly improves the likelihood.
The probability ratios are rational at these integer inputs; exact integer
comparisons prove both ratios exceed one. Independent 100/160-digit sums quantify
the log-likelihood improvements as `2.33332847e-6` and `3.33332673e-7`.

The points are the last improving candidates before the existing five-million-cell
partial-grid return. Correcting their values does not repair their maximization
or coefficient contract. Missing: a search that reaches a maximizer, for example
by bounding and refining the conditional population search before comparing r.
No search or status replacement was made in this arithmetic-only job.

The [independent maximum/neighbor study](2026-11-12-seam-rerun-data/maximum-values.json)
and [reproducer](2026-11-12-seam-rerun-data/maximum-check.py) contain these inputs,
stationary values, exact neighbor ratios, and precision repeats. They also check
generalized-beta 2568 and binomial 457: their public-point likelihood gaps from
the respective fitted maxima are approximately `3.05e-26` and `3.78e-24` relative.
For binomial, the integer profile was checked at the selected size and its two
neighbors; probability was maximized independently at that size.

## Validation, late fix, and machines

| Check | Machine | Final result |
|---|---|---|
| Native C++ dev, `just cpp-test` | workstation, GCC 15 | Initial 107/108 shards passed; sole binomial failure corrected and covered by the fresh focused retest below. |
| Fresh binomial native retest | workstation, GCC 15, `-O2 -DNDEBUG` | 22/22 cases, 7,209 assertions passed. |
| Fresh binomial double-only retest | workstation, GCC 15, `-O2 -DNDEBUG` | 22/22 cases, 7,209 assertions passed. |
| `just cpp-test-no-long-double`, all tiers | workstation, GCC 15 | 135/135 shards passed, including the late fix. |
| `just test`, dev | workstation | FAIL 0, WARN 0, SKIP 155, PASS 29,059. |
| `just test-full` | workstation | FAIL 0, WARN 0, SKIP 0, PASS 57,899. |
| `just site-check` | workstation | Passed: 100 compiled/linked C++17 examples, 100 family pages, browser checks, MathML and link checks. |
| Final binomial archive replay, both modes | workstation, GCC 15, `-O2` | 65/65 terminal rows per mode: 63 passes and two unchanged stops. |

The initial native failure was a strict ordinary binomial golden moved by two ulps
by blanket recomputation. The final code retains an already accurate free-fit
value when it differs from the stable sum only within `128 * epsilon` relative
summation roundoff. Material discrepancies still receive the correction. The
existing golden and expectation were not changed. Per the brief, only the affected
family was retested after this late local fix; the full native dev suite was not
repeated. The subsequent full double-only and R suites used the final code.

All four prescribed remote C++/R attempts returned simbox exit 75. Their local
fallbacks supplied the completed runs, including the required GCC 15 double-only
run. There was no completed GCC 13 run in this job. The R tiers and site check ran
sequentially under one four-slot local lease after remote availability checks;
no slot was held while waiting for simbox. The site build also rebuilt its WASM
assets. Queue waits and elapsed run records are not performance benchmarks.

[Validation records](2026-11-12-seam-rerun-data/validation-summary.json) preserve
the initial native exit and the separate successful retest, rather than labelling
an unrun full native rerun green. [Source-policy checks](2026-11-12-seam-rerun-data/source-policy.json)
confirm eight append-only test files, unchanged protected paths, and a clean diff
check. No existing test was weakened.

## Retained evidence and reproduction

[Final native rows](2026-11-12-seam-rerun-data/final-native.jsonl.gz) and
[final double-only rows](2026-11-12-seam-rerun-data/final-double.jsonl.gz) reconcile
the full censuses, longer retries, and final binomial-only replay. Before/after
raw rows, summaries, and source fingerprints are retained separately. The checker
rejects stale-source output reuse.

[All retained logs](2026-11-12-seam-rerun-data/log-index.json) and the
[SHA-256 evidence index](2026-11-12-seam-rerun-data/evidence-index.json) identify the
complete test and census evidence. The
[baseline reproducer](2026-11-12-seam-rerun-data/reproduce-baseline.sh) extracts the
recorded commit into ignored scratch using read-only Git operations and builds
both modes at `-O2`; no header/source-tree archives are stored in evaluation data.
