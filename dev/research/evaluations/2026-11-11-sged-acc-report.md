# Certified SGED accuracy and the SGED/SSTD/SNORM likelihood seam

TASK-150.2.6, contribution to TASK-150.2.7. Requested report date: 2026-11-11; final checks completed 2026-11-12. Worktree `sged-acc`, branch `codex/sged-acc`; supplied baseline `3b2a16fed4787c71b96bf23b1cb86569973b0a9c`. No commits or git mutations were made.

The certified fixed-shape/skew SGED routes now propagate the requested tolerance to their root and public-coordinate enclosures. All 17 reported coefficient misses are corrected. Across the complete certified replay, the worst scaled coefficient error fell from 9.90517e-9 to **5.7112117539e-11** at `tol=1e-10`; no component moved farther from its independent reference. The subunit scale certificate also reports its ideal maximized likelihood instead of a rounded public-coordinate residual. No inaccurate successful likelihood payload remains in the three-family archive. Five inherited SNORM boundary comparisons remain open, as detailed below.

## Changes and certificate scope

The mode solver requires a normalized bracket of width at most `min(1e-9, eta/8)`, where `eta=min(tol,1e-7)`. It propagates the bracket through the profiled scale and the public mean transformation. Each public enclosure includes the actual returned coefficient as well as its mathematical endpoints; its width is bounded by `eta/4` times the coefficient scale. Reconstruction and representability checks share that allowance. The additional cap at 1e-7 prevents a very loose tolerance from accepting a grossly displaced mean by using that displaced mean to enlarge its own error allowance. The original normalized reconstruction guard is retained.

A profiled scale which changes only at the rounding level is retained when it remains within `min(eta/8,32*eps)` relative to the refined result. This avoids last-bit changes away from an already accurate reference. With mean held and shape at least one, strict convexity in inverse scale gives a unique root. Its log bracket has width at most `min(1e-9,3*tol/4)`, and the stored scale must recover its log coordinate within `tol/8`. Projecting the coarse representative onto the nested refined bracket preserves a more accurate enclosed value instead of replacing it with a worse midpoint.

For held mean and subunit shape, the existing finite-domain, all-cell certificate remains responsible for global coverage. Terminal cells have log width at most `min(1e-10,tol/8)`. An objective incumbent excluded by derivative bounds no longer widens the root enclosure. The representative is projected onto an actual surviving cell, with a fresh cost check against the incumbent; it is never selected from an unchecked gap between cells. The final scale enclosure must satisfy both log and relative width `min(1e-7,tol/2)`. Separated winners retain the existing comparison-tie outcome, even with loose coefficient tolerance. One extra evaluation is reserved within the existing certificate budget.

The subunit likelihood is now `constant - certified_cost`. The prior public-coordinate check could leave a cusp rounding residual in the reported maximum. For mean 0.1, shape 0.5, skew 0.3 and data `[-3,-1,-0.2,0,0.3,0.4,2]`, the independent ideal maximum is **-14.05912276616022571831211972695625**; the old value was -14.059122793173428. The new value agrees with the ideal maximum. See the [independent cusp calculation](2026-11-11-sged-acc-data/cusp-maximum-reference.json) and its [script](2026-11-11-sged-acc-data/cusp-reference.py).

These are exact-arithmetic certificates conditional on the computed comparisons, with practical floating-point margins. They do not claim outward-rounded interval arithmetic. Compensated mode-score comparisons use 64*eps times their magnitude and exponent conditioning; inverse-scale scores retain 512*eps. Subunit derivative comparisons use 64*eps times their absolute term sum and conditioning; cost comparisons retain the 4096*eps allowance. Monotonicity, convexity and the existing all-cell coverage supply the mathematical argument. The arbitrary-precision comparisons below are independent numerical evidence for accuracy, not substitutes for that argument.

Only SGED certificate headers and its dossier changed production behavior. SSTD and SNORM required regression additions but no fitter changes: the supplied baseline already removed their old successful seam failures. No d/p/q/r, special functions, truth tables, code generators, shared binding code, message strings, goldens or JS tests were changed.

## Coefficients against independent roots

The reference calculation uses exact binary64 observations and held values, independent Gamma-moment formulas and arbitrary-precision density differentiation. Convex-power modes use monotone bisection; held-mean scale and free-skew checks use independent scalar coordinates. References use 110 decimal digits, with score/curvature checks where applicable. Density comparisons use 110–1600 digits according to scale. No production density, score or Hessian is used by the reference calculation.

The table reports the largest free-coefficient error scaled by `max(abs(theta*), s_i)`; location uses the reference scale as its natural scale. Rows 6170 through 6188 are the 17 original misses. Row 6078 is the preserved free-skew route, whose error is unchanged. Native and no-long-double results have the same errors in this table.

| Archive row | Before scaled error | After scaled error |
|---|---:|---:|
| 6170 | 7.9794748325e-10 | 5.0677554280e-12 |
| 6158 | 7.9789692338e-10 | 5.0170701978e-12 |
| 6190 | 7.4134161754e-10 | 4.6612917317e-12 |
| 6176 | 4.5107459437e-10 | 4.3620940618e-11 |
| 6184 | 4.3750507702e-10 | 3.0051513613e-11 |
| 6181 | 4.3693819662e-10 | 2.9484634418e-11 |
| 6183 | 3.8615820213e-10 | 3.6912218190e-11 |
| 6189 | 3.2969152496e-10 | 3.8653266238e-11 |
| 6204 | 2.9767611088e-10 | 1.0177135611e-11 |
| 6179 | 2.8775110059e-10 | 5.4920379221e-11 |
| 6174 | 2.6835537462e-10 | 3.5524653704e-11 |
| 6175 | 2.4009762698e-10 | 7.2668527200e-12 |
| 6177 | 1.9481643968e-10 | 2.0193488384e-11 |
| 6187 | 1.8262595645e-10 | 8.0029573008e-12 |
| 6182 | 1.7352744887e-10 | 5.7112117539e-11 |
| 6186 | 1.6467681778e-10 | 4.8261417439e-11 |
| 6188 | 1.4111270714e-10 | 2.4697462978e-11 |
| 6078 | 2.0990399758e-13 | 2.0990399758e-13 |

The original 17 now also pass C++ and R checks at `tol=1e-11`. The preserved 6078 route is checked at its original default tolerance. Four additional archived coefficient misses (6160, 6161, 6162, 6169) and the smooth subunit scale witness were corrected in the expanded replay. The latter previously missed by 9.90517e-9; its independent scale is 1.4916863638394512395771637229847117165. See the [smooth-scale reference](2026-11-11-sged-acc-data/smooth-subunit-reference.json).

The complete replay contains **66 certified records, 48 unique fits**, in each arithmetic mode. All meet the coefficient contract, and the largest error of their reported likelihoods against the independent value at the maximizing root is 5.87100e-15 relative to `max(1,abs(logLik))`. The [joined comparison table](2026-11-11-sged-acc-data/coefficient-comparison.json.gz) contains every before/after coefficient, independent root and component error, including the rest of the certified replay. The [root calculation and checks](2026-11-11-sged-acc-data/all-certified-roots.json.gz), [reference script](2026-11-11-sged-acc-data/certified.py), and [acceptance assertions](2026-11-11-sged-acc-data/verify.py) provide the detailed evidence.

Across all 670 replay records, both arithmetic modes have 293 successes, no new stops and no new successes. Every changed parameter vector belongs to a held-parameter fit. All-free parameter vectors and the three default golden refits (coefficients, likelihood, certification and outcome) are unchanged. There are no component-error increases. These are replay assertions, not a claim about every possible dataset; see [verification](2026-11-11-sged-acc-data/verification.json) and the [full summary](2026-11-11-sged-acc-data/summary.json.gz).

## Likelihood seam and cusp allowance

The archived three-family corpus contains 640 datasets: 256 SGED, 256 SSTD and 128 SNORM. Every dataset was refitted against both the supplied baseline and the changed source, in native and no-long-double modes. For each successful fit, the reported likelihood was compared with a compensated sum of the settled d/p/q log density and with an independent arbitrary-precision value at the exact returned binary64 coefficients. Errors below are relative to `max(1,abs(reference))`.

| Family | Archived datasets | Successful refits | Max reported logLik error | Max summed d/p/q error |
|---|---:|---:|---:|---:|
| sged | 256 | 99 | 3.91962e-14 | 2.07146e-15 |
| sstd | 256 | 99 | 4.18151e-11 | 3.57679e-16 |
| snorm | 128 | 65 | 1.44484e-16 | 2.13250e-16 |

All 263 successful archive refits meet 1e-10 in both modes. In no-long-double mode, the largest SSTD d/p/q error is 6.8363e-16 and the largest SNORM fitter error is 3.7983e-16; neither changes the conclusion. The additional 30 records comprise the 18 original certified replays, three golden refits and nine subunit shape/skew witnesses.

Six added SGED witnesses (shape 0.3 or 0.5, each with skew 0.3, 1.3 or 3) have non-Lipschitz cusp maxima. Their ideal maximum and exact density at stored coefficients differ by more than 1e-10 relative. Every discrepancy is one-sided and below the dossier allowance, conservatively bounded here by `n*(64*eps)^nu`; the accurate d/p/q sum agrees with the independent density at the stored coefficients. The largest absolute gap is 0.0003159663, below its 0.0004907769 allowance. This is a coefficient-representation effect at a cusp, not an inaccurate density or an inaccurate maximized value. All six gaps and allowances are recorded in [verification.json](2026-11-11-sged-acc-data/verification.json). Shape 0.9 witnesses satisfy the ordinary seam tolerance.

The November 9 archive's old 65 SGED, 61 SSTD and 19 SNORM fitter-side seam failures are no longer successful fits on the supplied baseline. In native arithmetic, the 65 SGED rows now return uniform/half-GED limits; the 61 SSTD rows return half-t limits; 14 SNORM rows return named half-normal limits, and five retain unresolved boundary comparisons. These outcomes are identical before and after this job. In no-long-double arithmetic, SSTD row 6697 retains a free-coordinate score stop, and SNORM rows 6576 and 6590 retain arithmetic/root-enclosure stops where native arithmetic names the limit; row 6587 uses an arithmetic message for the same orientation comparison. No such diagnostic logLik was counted as a successful estimate. Exact row-by-row attribution is in [summary.json](2026-11-11-sged-acc-data/summary.json.gz).

## Regressions and completed checks

New C++ cases were appended to each family's test.cpp, and R cases were appended to its test.R. Existing expectations were not relaxed or deleted. Coverage includes the 18 original certified SGED cases, tighter tolerance for the 17 owned routes, one independent likelihood seam witness per family, smooth subunit scale, the nine cusp/smooth combinations, the independent cusp maximum, subspacing tolerance rejection, two analytically separated scale maxima at loose tolerance, and the displaced-public-mean loophole. The latter has an [independent two-point root](2026-11-11-sged-acc-data/loose-reference.json).

| Check | Machine | Result |
|---|---|---|
| C++ dev (`just cpp-test`) | simbox, GCC 13.3 | 108/108 passed |
| C++ full (`UMVL_TEST_TIER=full just cpp-test`) | simbox, GCC 13.3 | 135/135 passed |
| C++ no-long-double | simbox, GCC 13.3 | 135/135 passed |
| R dev (`just test`) | simbox, R package compiled with GCC 15 | FAIL 0, WARN 1, SKIP 154, PASS 25711 |
| R full (`just test-full`) | simbox, R package compiled with GCC 15 | FAIL 0, WARN 1, SKIP 0, PASS 52469 |
| C++ no-long-double | workstation, GCC 15.2 | 135/135 passed |
| Final SGED C++ native and no-long-double | workstation fallback, GCC 15.2 | 1/1 shard in each mode passed |
| Final SGED R full-tier file | workstation fallback, GCC 15.2 | FAIL 0, WARN 0, SKIP 0, PASS 1100 |
| `just site-check` | workstation | passed, including 99 family pages and 99 C++ snippets |

The initial full R runs each emitted one expected shape-restriction warning from the new SGED seam witness. Its test now explicitly asserts that warning; final SGED reruns have WARN 0. After the late SGED-only refinements, only SGED C++ shards and its full-tier R file were rerun, following the requested policy. The final frozen-source rerun used the workstation because simbox returned exit 75. SSTD/SNORM code and their regressions had no late changes.

The suite child return codes and machines are recorded in [test-runs.txt](2026-11-11-sged-acc-data/test-runs.txt), with compact [test output](2026-11-11-sged-acc-data/test-summaries.txt). An older long-running wrapper saw a trailing shell parse error after recording successful child suites, because its helper was edited earlier in the session. The actual suite commands, including site-check, returned 0; the current helper passes `bash -n`. The final SGED wrapper itself returned 0. No test failure is hidden by that wrapper issue.

The additional repository-wide Markdown link check reports 45 existing findings across seven files that are byte-for-byte unchanged from HEAD (missing local literature/PDF files, old generated inventory fragments and existing note links). None belongs to this report or the changed dossier. Every report destination resolves, and `git diff --check` passes. The [link-check record](2026-11-11-sged-acc-data/markdown-link-check.txt) preserves the findings and baseline comparison; those unrelated files were left untouched.

## Open boundary outcomes

**Inherited SNORM orientation comparison: rows 6579, 6583, 6587, 6589 and 6593.** Each retains competing half-normal and reflected-half-normal boundary candidates, `comparison_tie=true`, `budget_exhausted=false`, and no successful finite fit. Repeating at iterlim 200, 2000 and 10000 leaves the outcomes and likelihood bounds unchanged. Row 6583 has slightly different candidate values, still within the practical comparison margin; several other pairs coincide at printed precision. Increasing the budget is insufficient. A sharper or analytic comparison of the two constrained boundary orientations, including their coverage, is missing. The candidates are diagnostics and must not be treated as returned coefficients or a valid finite maximized likelihood. See [budget replay and candidate bounds](2026-11-11-sged-acc-data/snorm-stops-budget.txt) and its [source](2026-11-11-sged-acc-data/snorm-stops.cpp).

The additional no-long-double stops at SSTD 6697 and SNORM 6576/6590 are inherited precision-mode limitations, with native named-limit outcomes and no successful payload to repair. Their score/arithmetic enclosure remains unavailable in that mode. They are not certified away by this seam study. No remaining inaccurate successful logLik, or unclassified cusp exception, was found.

## Reproduction

Run [reproduce.sh](2026-11-11-sged-acc-data/reproduce.sh) from this worktree. It rebuilds the supplied baseline with `git archive` into ignored scratch space, without retaining a source archive in the evidence directory, and uses the shared one-slot wrapper for each computation. It expects the already present fit-audit, acc-fb-t and dpqr-extreme evidence directories and existing compiler/BH/mpmath dependencies. The exact selected data and holds are also retained in [cases.json](2026-11-11-sged-acc-data/cases.json.gz). [run-checks.sh](2026-11-11-sged-acc-data/run-checks.sh) records the suite commands and simbox fallback policy. Build products and verbose logs remain ignored scratch; no source tree, figure, or external published artifact was archived with this report.
