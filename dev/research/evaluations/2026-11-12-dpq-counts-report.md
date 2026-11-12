# Count-family tails at extreme parameters

Date: 2026-11-12. Worktree: `dpq-counts`, branch `codex/dpq-counts`.
Baseline: `217ab8b533cac25ccc8eba8d66eccde1b376f0b4`. No commits or publication.

The specified production witnesses for **bnbinom, cmp, lgser, pig and skellam now meet the accuracy contract** in the native and double-only consumers. Restored truth rows also pass the R and JavaScript consumers. Five archived parameter members, 1088 value rows and 220 quantile rows have been restored to active coverage, without changing their numerical references. All required suites finished green. The remaining scope and validation limitations are listed under Open below.

## Causes, evaluators and bounds

These are exact-arithmetic remainder arguments conditional on the computed comparisons, with practical floating-point margins. They are not outward-rounded interval certificates. Independent high-precision comparisons and frozen-table regressions provide separate numerical evidence.

### Skellam

The finite wrong values came from cancellation in the large-rate mass exponent and the difference-of-Poisson CDF route. For positive count above one million, smaller rate at most 16 and dominant rate/count below two, the mass now uses a positive Poisson convolution. Its successive ratios are bounded by `(a/(k+j+1))*b/(j+1)`, decreasing with j, so a geometric majorant bounds the omitted sum. Acceptance requires relative remainder below `2e-16`; the 1000-term guard returns NaN without closure.

The large-rate CDF sums the smaller tail in scaled, compensated coordinates. Skellam log concavity bounds subsequent ratios by the next descending ratio; the geometric remainder must be below `2e-14`. For negative counts the termwise Bessel bound gives ratio at most `mu2/(1-v)`, including cuts whose next integer is not representable. A nonfinite seed, unresolved count increment or million-cell exhaustion returns NaN.

Near the mean, with `m=mu1-mu2`, `d=k-m` and `z=2*sqrt(mu1*mu2)`, the Debye exponent is evaluated as

`-d^2 * integral_0^1 (1-t)/hypot(m+t*d,z) dt`.

For `abs(d)/(mu1+mu2)<0.01`, the denominator is at least 0.99 times the rate sum. The derivative bounds `|g^(j)| <= j!/R^(j+1)` for `g=1/R` bound the composite Simpson remainder with 256 panels below `2e-15` relative to the integral. This removes subtraction of large nearly equal exponents.

### Beta negative binomial

The near-Poisson zero mass formerly subtracted gamma increments of order size to obtain a result of order one. For alpha at least one million and `(r+beta)/alpha <= 0.001`, a degree-12 mixed Taylor difference cancels those terms algebraically. The omitted Taylor terms have a geometric majorant in `(r+beta)/alpha`; the omitted mixed Stirling correction is of relative order `alpha^-4`.

For `r,beta >= 1`, writing `E=alpha+r+beta`, the mass ratio is

`R_j = [j+r+beta-1+(r-1)*(beta-1)/(j+1)]/(j+E)`.

For `j>=k` this is at most `(j+D)/(j+E)`, where `D=r+beta-1+(r-1)*(beta-1)/(k+1)`. If `E>D+1`, telescoping bounds the uncomputed upper tail by `p_k*R_k*(k+E)/(E-D-1)`. The new small-cut route accepts at relative remainder `2e-14`, otherwise returns NaN after 100000 cells. It covers cuts below 10000; the existing bounded far-count route remains in place.

Final review also found a finite wrong density beyond that cut: at count 10000 and `(r,alpha,beta)=(1e8,1e16,1e8)`, the prior implementation returned **-82240** instead of **-82108.928170136018222…**. Near-Poisson masses now retain the rising-factorial subtraction in extended precision, accepting only when `32*epsilon_ext*sum(abs(terms))`, the output-rounding allowance and Stirling allowance close the density tolerance. A checked far-count chart handles cases where its subtraction margin closes; otherwise the result is NaN. Independent gamma identities check counts 10000, 10001, one million, `1e10` and `1e100` in both modes, and the first two in JS. See [boundary references](2026-11-12-dpq-counts-data/bnb-far-reference.json).

### Conway–Maxwell–Poisson

At unit rate and `0<nu<=1e-7`, the factorial-power normalizer is too diffuse for the previous direct moment evaluator. The probability evaluator now combines a compensated 100000-cell prefix with an Euler–Maclaurin tail for `f(x)=Gamma(x+1)^(-nu)`. With `delta=nu*psi(N+1)`, log concavity gives `f(N+t)/f(N)<=exp(-delta*t)`.

The trapezoid remainder is bounded by `integral |f''| / 12`. Bounds on digamma and its derivatives give the explicit integrated majorant. Positive Simpson panels use an analytic fourth-derivative bound after scaling by delta, with omitted integration tail at most `exp(-80)/delta`. The combined relative remainder must be below `2e-12`; depth 50 or one million panels without closure returns NaN. The fitter's moment evaluator is unchanged.

For counts at least `1e305`, the leading Stirling term is scaled by nu before multiplication, avoiding log-Gamma overflow when the requested log weight remains finite. The omitted correction is at most `nu/(12*x)`. At the largest binary64 count the native, double and JS density and upper log tail are finite and agree with the high-precision log-weight check to normalized error `6.52e-17`. See [far-weight evidence](2026-11-12-dpq-counts-data/cmp-far-reference.json).

### Logarithmic series

A compensated finite prefix resolves the small cuts at the largest binary64 theta below one. Larger cuts use the Stieltjes survival expansion obtained by integrating the partial-fraction expansion of coth, in terms of scaled `E1`. Terms through `B6` are retained; the signed `B8` term bounds the remainder and must be below `1e-12` of the sum. Scaled E1 itself uses a signed next-term remainder for large arguments. If the new remainder cannot close, the existing geometric evaluator is retained and returns NaN when its bound cannot close. Quantiles invert these tails; sampling is unchanged.

The sole shared special-function change is the Boost E1 wrapper through `special/special.hpp`, with the existing no-throw policy. No zero-family files were edited.

### Poisson inverse Gaussian

The heavy upper tails could not close within the existing ten-million-cell remainder guard. Small finite prefixes are complemented only while roundoff amplification is controlled; for cuts at most 200 the opposite probability must exceed approximately 0.095.

For larger heavy tails, the inverse-gamma representation of the Bessel factor yields an alternating Taylor expansion bounded by its next absolute term. Euler's beta integral sums each term over the count tail. With `A=2*mean^2/(shape+2*mean^2)`, the coordinate `v=n*(1-A)*(exp(w)-1)/A` resolves the boundary layer. Analytic bounds on four log derivatives certify Simpson panels. The omitted `v>V` contribution is at most `exp(-V)*sum(abs(coefficients))`. The integrated route requires `n>100000`, `c<n/4`, and combined relative quadrature/truncation bound below `1e-11`; small cuts bridge to it by positive recurrence.

Nonrepresentable scaled coordinates, underflow of `1-A`, or unclosed depth-50/million-panel bounds return NaN. An unnecessary large-coordinate refusal was removed. At count `2.0004e19`, mean 10 and shape 0.1, an independent 80-digit mass/geometric-tail enclosure bounds the exact upper log tail between -9999500333187631.753 and -9999500333187624.150. The returned -9999500333187624 meets the log contract by normalized error below `7.75e-16`. See [enclosure](2026-11-12-dpq-counts-data/pig-large-coordinate-reference.json); the JS coordinate check also passed.

## Reference preservation and consumer evidence

| Family | Restored value rows | Restored quantile rows | Active value rows | Active quantile rows |
|---|---:|---:|---:|---:|
| bnbinom | 206 | 44 | 3576 | 748 |
| cmp | 216 | 44 | 2519 | 572 |
| lgser | 215 | 44 | 1468 | 308 |
| pig | 423 | 88 | 2300 | 528 |
| skellam | 28 | 0 | 2623 | 616 |
| Total | 1088 | 220 | 12486 | 2772 |

The preservation check compares the multiset of parameter-labelled value and quantile payloads against baseline, including previously archived rows. Sampler selections are unchanged. All five `mle.hpp`, `golden.json` and `truth.py` files are byte-identical to baseline. See [preservation receipts](2026-11-12-dpq-counts-data/preservation.json) and [final source hashes](2026-11-12-dpq-counts-data/final-preservation.json).

The independent consumer replay covers 62607 d/p/q comparisons, 9795 additional representable natural-density comparisons, and 148 large-count lattice checks for the five requested families **per precision mode**, with no failures. Family-focused receipts supersede earlier binaries after local fixes. The broader six-family replay has 74467 comparisons per mode; its 833 failures are all archived zero-modified-logarithmic diagnostics, with zero active failures. See [native](2026-11-12-dpq-counts-data/counts-native-summary.json), [double](2026-11-12-dpq-counts-data/counts-double-summary.json), and the family-specific receipts beside them.

The 18 original scalar witness checks have maximum normalized error `4.774e-15`. Natural probabilities use relative error; log outputs use absolute error divided by `max(1,abs(reference))`. The Skellam natural mass `3.716303015795054e-303` and tail near `1e-300` are tested directly, including the reflected case. See [actual outputs](2026-11-12-dpq-counts-data/witnesses.json).

## Fits and log-likelihood preservation

The five goldens and all matching frozen archive datasets were replayed before and after: 313 cases per mode, with 247 successful fits and 66 unchanged stops. Every coefficient, status and stop reason is unchanged. No golden was regenerated.

Only Skellam reported log-likelihoods changed: native case 6337 by approximately `1.187e-12`; double cases 6337 and 6351. An independent 80-digit positive Poisson convolution supports these corrections. For double case 6337 the absolute error falls from `2.245e-9` to `1.119e-15`; case 6351 improves from `1.100e-8` to `3.912e-9`. See [native preservation](2026-11-12-dpq-counts-data/fit-preservation.json), [double preservation](2026-11-12-dpq-counts-data/fit-preservation-double.json), and [log-likelihood references](2026-11-12-dpq-counts-data/fit-loglik-reference.json).

The final beta-negative-binomial repair was followed by a fresh 113-case replay in each mode. Coefficients, statuses and reported log-likelihoods remain identical to baseline. This stronger seam check also exposed the inherited double-only fitter discrepancy described below. See [final replay and baseline-binary comparison](2026-11-12-dpq-counts-data/fit-preservation-final-bnbinom.json).

## Finished checks

Every final run used the **workstation, GCC 15**. Simbox attempts exited 75 (unreachable); the required fallback commands ran locally through the CPU slot wrappers, with thread pinning and low priority. Receipts: [suite ledger](2026-11-12-dpq-counts-data/suites.tsv), [simbox attempts](2026-11-12-dpq-counts-data/attempts.tsv), [suite launcher](2026-11-12-dpq-counts-data/final_checks.sh).

| Check | Result |
|---|---|
| `just cpp-test` | 108/108 dev shards passed |
| `just cpp-test-no-long-double`, workstation | 135/135 shards passed |
| `just test` | FAIL 0, WARN 0, SKIP 154, PASS 25769 |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 54383 |
| `just js-build` and `just js-test` | Build passed; 618/618 tests passed |
| Full JS truth diagnostics | 385937 checks, no failures |
| `just site-check` | Passed, including 99 pages, browser checks and math rendering |
| Late targeted C++ checks | PIG added cases; CMP and bnbinom shards in both modes passed |
| Late R full-tier family selection, bnbinom and cmp | FAIL 0, WARN 0, SKIP 6 empty filtered tests, PASS 1157 |
| Late JS rebuild and family selection | 210/210 tests; 30671 truth checks, no failures |
| Final site check after late dossier change | Passed |
| Final independent references and preservation assertions | Passed, with inherited fitter seam recorded separately |
| `git diff --check` | Passed |

Full suites were run once; subsequent family-local fixes received targeted checks rather than another full run. The shared E1 addition was included in the full checks. Logs and scripts are in [the evidence directory](2026-11-12-dpq-counts-data/). The complete and targeted JS truth receipts are [here](2026-11-12-dpq-counts-data/js-full-truth-results.json) and [here](2026-11-12-dpq-counts-data/js-targeted-truth-results.json).

Execution note: the initial site preflight used a one-slot reservation and unexpectedly triggered the site's default four-worker WASM build. Subsequent builds and site checks used `run4`; no other computation from this job ran alongside that initial build.

## Open

1. **Truth regeneration declarations remain stale, outside the explicit file authorization.** The closed exclusions were lifted from the authorized `truth.json` files. Their generators still declare them at `bnbinom/truth.py:86`, `cmp/truth.py:122`, `lgser/truth.py:22`, `pig/truth.py:116`, and `skellam/truth.py:124–125`. Regeneration would archive the repaired cases again. What is missing is authorization to remove only those exclusion declarations; no reference formula or numeric value needs changing. Permission was requested during the job and no response was received. The five generator files were therefore left untouched.
2. **Zero-modified logarithmic routing remains separate.** Its near-unit rows still yield 833 diagnostic failures per mode in the archived-row replay. It uses the shared zero-family core rather than calling the repaired `plgser`, so the lgser fix does not flow through. No zero-modified files or exclusions were changed. The separate zero-family job must route or repair that evaluator and replay its archived member.
3. **Inherited double-only bnbinom fitter log-likelihood, archive case 579.** For data `[1000000,1000001,1000003,1000008,1000011]`, held size `1.0672234787479615` and beta `5.36473383510874`, the unchanged fitted alpha is `0.0825043451682516`. The fitter reports `-86.55337674075822`; the public density sum is `-86.55337672897528` and the independent reference is `-86.5533767289753`. The normalized fitter error is `1.36135e-10`; the density error is `1.642e-16`. Replaying the saved baseline binary reproduces the same coefficients, reported likelihood and density sum exactly. The cancellation is in the separate likelihood accumulation in `bnbinom/mle.hpp`, which this job was not authorized to edit. This is an existing fitter defect, not an evaluator regression; it needs a likelihood-arithmetic fix and fitter validation.
4. **GCC 13 cross-machine check unavailable.** Simbox could not be reached. GCC 15 native, GCC 15 double-only and WASM checks passed; no GCC 13 pass is claimed.
5. **Artifact display unavailable for this worktree.** The requested `show` command finished with HTTP 403: the viewer has not registered this worktree as a readable root. The report remains at its requested path. Opening an agent thread here in the viewer or adding this path to its configured roots is needed to display it in the pane; no files or viewer settings outside this worktree were changed. See [display receipt](2026-11-12-dpq-counts-data/viewer-status.json).

The NaN regions described above and in each dossier remain bounded refusals: no capped tail or unchecked estimate is returned as a completed evaluation. The numerical evidence closes the listed witnesses and restored rows; it does not establish accuracy at every binary64 parameter tuple outside those covered charts.

Merge note (Claude): open item 1 is closed at merge. The TASK-196 exclusion declarations were removed from the five `truth.py` generators and only those five tables' `input_sha256` fingerprints were refreshed; their numerical bytes are unchanged (checked with `truth_metadata.numerical_bytes`).
