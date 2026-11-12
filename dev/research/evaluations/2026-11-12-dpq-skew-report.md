# TASK-200: extreme GED and skew-family d/p/q

All ten parameter points identified by oracle-gaps and all 307 archived skew witnesses now pass the exact-input replay. The SGED finite quantile error is corrected. The four families have no remaining truth exclusions: 96 value rows and 96 quantile rows have been reactivated, including the two archived ST members, without changing reference values. The final numerical replay passes 18,148 comparisons in each of native C++, double-only C++, standalone WASM and the full generated JS module.

Baseline: `217ab8b533cac25ccc8eba8d66eccde1b376f0b4`. Worktree: `codex/dpq-skew`. No commits, pushes, board edits, package installations or changes to fitters were made. All started commands have finished. Validation receipts, compressed logs and source/binary hashes are retained in [the evidence directory](2026-11-12-dpq-skew-data/validation-receipts.json).

## Causes and corrections

Parameter order below is `(mean, sd, nu[, xi])` for GED/SGED/SSTD and `(xi, omega, alpha, nu)` for ST. The point manifest retains the individual observations, probability selectors and independent references: [oracle-points.json](2026-11-12-dpq-skew-data/oracle-points.json).

| Family and parameter points | Cause | Correction |
|---|---|---|
| GED: `(0,1000,2e8)`, `(-1e8,1e8,2e8)` | Rounding the physical difference and standardized radius before a highly sensitive gamma power | Retain the physical affine transform and log radius in three words; propagate their error through the power and both tail endpoints |
| SGED: the same two points with `xi=1` | Inherited GED edge failure | Delegate the symmetric member to the corrected GED evaluation |
| SGED: `(0,1,1e8,1e8)`, `(0,1,1e8,1e-8)` | Moment/skew-centering cancellation amplified by thin-side scaling and the gamma power; central inverse cancellation | Three-word moment, variance, physical numerator and inverse transform, each with an acceptance margin |
| SSTD: `(0,1,1e8,1e8)`, `(0,1,1e8,1e-8)` | Half-shift gamma-ratio and skew-moment cancellation | Cancel leading gamma terms algebraically, retain the physical skew numerator, and propagate its error through Student tails |
| ST: `(0,1,-1e8,1e8)`, `(0,1,1e8,1e8)` | Unresolved angular endpoint layer and subtraction of large log kernels; cancellation in a small positive-side CDF | Scale the angular endpoint layer, evaluate the kernel ratio directly, and form a small CDF as central mass plus the thin wedge |

The ST inverse bracket was a downstream consumer of the defective CDF. Its adjacent-float bisection and failed-evaluation checks remain unchanged. STD is an unchanged fitting and evaluation control.

For SGED, the corrected central inverse distinguishes exact binary64 probability arguments. At the extreme-skew point, lower-tail `p=0.5` gives approximately `-2.8491093372415545e-16`; lower-tail `log(p)=-0.6931471805599453` gives approximately `-2.447438646570303e-16`. These are different exact inputs. The old maximum relative quantile error was about 22.136; it was a finite wrong value, not merely a refused evaluation.

Two additional ST cases were closed while validating the reformulation:

- At location zero, scale one, `nu=5`, `alpha=1e12`, `x=1e-12`, the lower CDF is `8.412994158666847e-13`. The baseline returned `8.412565404600893e-13`, a relative error of `5.0963314352551015e-5`. Independent density and original angular integrals agree to 105 digits. The endpoint scaling is now used at large skew for ordinary as well as large degrees of freedom. See [the before/after check](2026-11-12-dpq-skew-data/st-small-cdf.py).
- At `nu=DBL_MAX`, `alpha=1e8`, `x=±2e-16`, an intermediate version of the new kernel lost `h` to underflow while `nu*h` still mattered. A bounded normal-face bridge closes this development finding. The two lower-CDF references are `3.183098782049451e-9` and `3.1830989416263634e-9`. Both C++ modes and JS now return their rounded references. This was an intermediate-reformulation defect, not an additional finite-error claim about the baseline. See [the finite-degree reference bound](2026-11-12-dpq-skew-data/st-normal-face.py).

## Arithmetic and acceptance bounds

The new helpers are `special/three_word.hpp`, `skew_coordinate.hpp`, `skew_probability.hpp` and `skew_t_angular.hpp`. They use C++17 arithmetic and the existing special-function seam. No precision gate was removed without a replacement. Sampling algorithms and fitter implementations are unchanged.

The three-word helper uses error-free additions, FMA product residuals, compensated division and square roots, and three-word logarithms, exponentials and log-Gamma calculations. Its practical arithmetic allowance is

`B3(M) = 32768 * epsilon^3 * (1 + M) + 128 * denorm_min`.

Gamma recurrence reaches 64 and retains 20 Stirling corrections. Twenty-three split constants were checked against independent 100-digit expansions. These are practical floating-point margins, supported by the comparisons below; they are not outward-rounded interval bounds.

For the skew transform, write `t=min(xi,1/xi)`, reflect the physical standardized observation to `w`, and let `m` be the base first absolute moment. Evaluation retains

`s^2 = (1-m^2)(1+t^4) + (2m^2-1)t^2`,

`N = w*s - m*(1-t^2)`.

The numerator allowance is `E_N = B3(abs(w)+abs(m)) + (1+2*abs(w))*E_m`. The thin-side radius divides both numerator and allowance by `t^2`; the side is accepted only when `abs(N)>E_N`, apart from the exact symmetric case. This avoids losing the physical residual before the thin-side division.

GED/SGED keep the log radius through the gamma power. For log-radius error `E_w`, the density bounds the power perturbation by `exp(nu*w)*expm1(nu*E_w)`. CDFs evaluate the radius endpoints and transform both into the requested tail. The base gamma allowance is `2e-12 + 16*epsilon*abs(log(Q))`. The broad-side gamma central mass has logarithmic sensitivity at most one in the log radius, by its integral representation. Large-shape replacements carry explicit moment/scale errors: GED log scale has allowance `2/nu`; the SGED moment limit has `32/nu^2+B3(1)`.

The SGED central inverse retains the input probability and centering subtraction in three words. When `nu*log(c)<-100`, its explicit inverse has displacement remainder bounded by `lambda*Gamma(1+1/nu)*exp(-100)/(nu+1)`. The result must satisfy either the relative quantile bound or the probability bound, using `f<=2/sd`.

SSTD computes the half-shift gamma ratio after algebraically cancelling its leading terms with `log1p(0.5/z)`. Its normal-moment replacement carries `2/nu+B3(1)`. Student tail error includes radius uncertainty times a hazard bound and a `2e-12` base allowance. On the broad central side, incomplete-beta values at both radius-error endpoints replace a fixed-slope approximation, including as `nu` approaches two.

The shared probability gate checks the selected tail after endpoint propagation. Log probabilities must meet `1e-10*max(1,abs(log(p)))`; natural probabilities include the actual `exp` rounding displacement and must meet relative `1e-10`. A zero is accepted only when both uncertainty endpoints prove rounding to zero. Subnormal probabilities are accepted when their actual rounding error permits it; an arbitrary log-tail cutoff no longer bypasses the gate.

For ST, with `theta=atan(1/abs(alpha))`, the substitution `r=theta*exp(-v/k)` uses

`h=t^2/(nu*sin(theta)^2+t^2)`, `k=1+nu*h*(1-theta^2)`.

The kernel ratio is evaluated as `[1+h*((sin(theta)/sin(r))^2-1)]^(-nu/2)` using `log1p` and `expm1`, avoiding subtraction of large log kernels. The transformed integrand decays at least exponentially; truncation at `v=64` contributes at most `exp(-64)/k`. The small-angle log-sinc remainder is of order `theta^6`, below `1e-36` on this branch. Adaptive quadrature must converge and pass the propagated kernel margin plus a practical `2e-11` reserve. The integral identity and truncation analysis are analytic; quadrature convergence and its floating-point reserve have independent numerical validation, not an outward-rounded quadrature enclosure.

Near the normal face, for `nu>1e32` and `abs(t)/sin(theta)<=2`, evaluation at `nu0=1e32` carries relative allowance `512*(nu^(-1/2)+nu0^(-1/2))`. The proof uses the Gamma-scale mixture: its mean-one mixing variable has variance `2/nu`, hence `E(abs(sqrt(W)-1))<=sqrt(2/nu)`. The normalized Gaussian angular integral has derivative at most 1.3 and is greater than 0.02 on this ray range. Together with the central-mass coupling bound, this gives the stated conservative allowance, which participates in the selected-tail checks. The lower bound follows also from the Gaussian Mills-ratio upper bound `(x^4+9*x^2+8)/(x^5+10*x^3+15*x)`: at `x=2` it gives angular mass greater than `11*exp(-2)/71>0.02`. For the `DBL_MAX` reference comparison, the finite-degree discrepancy is bounded by `3.82e-152` relative.

## Numerical evidence and exclusions

The [exact-input replay](2026-11-12-dpq-skew-data/check.py) uses the settled truth tables and archived witness references. Comparisons preserve binary64 inputs and tail/log selectors. It does not regenerate references. Density and log-CDF comparisons use the contract's scaled error, natural probabilities use relative error, and nonzero quantiles use relative error. The [reference inventory](2026-11-12-dpq-skew-data/reference-preservation.json) proves that all reference rows, including reinstated archived members, are unchanged from the baseline.

| Consumer | Comparisons | Failures | Recovered witnesses |
|---|---:|---:|---:|
| Native C++ | 18,148 | 0 | SGED 195; SSTD 112 |
| C++ with extended arithmetic disabled | 18,148 | 0 | SGED 195; SSTD 112 |
| Standalone WASM | 18,148 | 0 | SGED 195; SSTD 112 |
| Full generated JS/WASM module | 18,148 | 0 | SGED 195; SSTD 112 |

The frozen baseline had 737 failing comparisons per C++ mode: 729 NaNs and eight finite SGED quantile errors. The final replay has no NaNs or failures in this corpus. All 242 distinct measurements belonging to the ten original oracle-gap points are covered by the replay and pass separately. Both natural SGED tails at `nu=3.8792797496374307`, `xi=1e8` are recovered. The remainder of the 307 archived witnesses is **zero in every region**.

Reactivated rows are GED 20 values; SGED 32 values and eight quantiles; SSTD six values; and ST 38 values and 88 quantiles from two archived members. Their consumers run in both C++ modes and JS. The JS ST-selected truth diagnostic additionally reports 2,372 checks with no failures.

The final largest errors in either C++ mode are `4.759608934908351e-14` for density, `2.7284224006016915e-12` for CDF, and `1.2806898974049816e-13` for quantile, under the replay's respective error definitions. See [error-breakdown.json](2026-11-12-dpq-skew-data/error-breakdown.json). Supplementary current-source ST checks pass in both C++ modes and all four JS checks pass.

Independent 240-digit moment/coordinate comparisons cover 815 distinct skew inputs. The largest actual-error/claimed-margin ratios are `0.000426941` for moments and `0.000287636` for coordinates; there are no margin failures. Another 36 uniform-limit edge checks use 400-digit arithmetic, including `nu=DBL_MAX`, and have largest normalized error `1.78715e-16`. These are numerical checks of the practical margins, not proofs over the whole parameter domain. Scripts and results are retained as [margins.py](2026-11-12-dpq-skew-data/margins.py), [margins.json](2026-11-12-dpq-skew-data/margins.json), [edge.py](2026-11-12-dpq-skew-data/edge.py) and [edge.json](2026-11-12-dpq-skew-data/edge.json).

## Fit preservation

The five golden samples and 1,056 fit-audit archive rows were replayed before and after with `tol=1e-10`, `maxit=200` and the archive's explicit holds. Each C++ mode checks 1,061 cases: GED 145, SGED 257, SSTD 257, STD 145 and ST 257. Coefficients, status and log likelihood are identical as recorded to 17 significant digits; there are no timeouts. The final replay was repeated after the last ST change.

The five `mle.hpp` files and five `golden.json` files are byte-identical to the baseline. See [native preservation](2026-11-12-dpq-skew-data/fit-preservation.json), [double-only preservation](2026-11-12-dpq-skew-data/fit-preservation-double.json) and [source preservation hashes](2026-11-12-dpq-skew-data/source-preservation.json). This establishes preservation of these fits; it does not add a new global MLE certificate.

## Final test runs

Every successful run below used the workstation. Five simbox attempts returned 75, including the no-long-double attempt; their fallback runs used the prescribed local slot wrappers. The workstation GCC 15 double-only check completed. **Open: GCC 13/simbox validation could not run because simbox was unreachable.** The missing evidence is a successful simbox suite run; no GCC 13 pass is claimed.

| Check | Result |
|---|---|
| C++ dev tier, initial run | 108/108 shards |
| C++ full tier after final shared-helper changes | 135/135 shards |
| Workstation `cpp-test-no-long-double` after final shared-helper changes | 135/135 shards |
| R dev tier after shared-helper changes | 25,803 pass; 0 failures/warnings; 154 skips |
| R full tier | 54,335 pass; 0 failures/warnings/skips |
| Full JS build and test suite | Build passed; 618/618 tests |
| Final ST-only C++ reruns, native and double-only | 1/1 shard in each mode |
| Final ST-selected R full-tier rerun | 568 pass; 0 failures/warnings; six skips |
| Final full-module WASM rebuild and ST/static JS rerun | Build passed; 208/208 tests; 0 skips |
| Final site validation | Passed, including catalogue, links, MathML, numerical and browser checks |

The final refinements were confined to ST's `dpqr.hpp`, its dossier and regression tests. Accordingly, after the full suites passed, only the affected ST checks were repeated, as required by the late-fix rule. The final 18,148-comparison replay in every consumer and both 1,061-case fit replays use the finished source. The initial site run found three unsupported dossier fragments; the links were corrected and site validation passed twice afterward. Existing warnings in unrelated binomial/Bhattacharjee code were nonfatal; R reported zero warnings.

The complete run history, including unavailable simbox attempts and the corrected site failure, is in [validation-receipts.json](2026-11-12-dpq-skew-data/validation-receipts.json). It links all compressed logs. [source-fingerprints.json](2026-11-12-dpq-skew-data/source-fingerprints.json) records the checked source, evidence scripts, JSON and final binaries.

A supplementary repository-wide `check_links.py` scan reported 45 existing broken destinations in unrelated files, including archived code-like Markdown and missing local literature. Every file producing those failures is byte-identical to the baseline. The report and all four changed dossiers have no broken destinations; the [focused check](2026-11-12-dpq-skew-data/report-links.json) passes. Those unrelated global link failures remain outside this job's scope.

## Remaining precision regions

No requested truth exclusion or archived witness remains open. The implementation still refuses evaluations whose propagated bound exceeds the contract: unresolved skew-join sign, unrepresentable moment/log coordinate, excessive gamma-power or selected-tail uncertainty, insufficiently accurate subnormal rounding, or unconverged angular quadrature. These conditions and bounds are recorded in each family's `d/p/q/r` section.

A concrete retained SGED precision region is `nu=1`, `xi=1e22`, mean zero, sd one, `x=-1`: the centering numerator is about `7.1e-45`, below its practical margin of about `2.3e-42`. The probability returns NaN on both scales. This is a limitation of the certified arithmetic path, not a claim that the mathematical probability is impossible to compute. It lies outside the recovered witness corpus. No exclusion was removed for this region.

Artifact display was attempted with the prescribed `bin/show` command. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report remains at the requested path; no viewer settings or files outside the worktree were changed.

## Reproduction

Run from this worktree with `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` set to `$PWD/build/tmp`. The host drivers reserve their own slots and must not be nested inside another reservation:

```sh
python3 dev/research/evaluations/2026-11-12-dpq-skew-data/final-runs.py
python3 dev/research/evaluations/2026-11-12-dpq-skew-data/post-guard-runs.py
python3 dev/research/evaluations/2026-11-12-dpq-skew-data/late-st-runs.py
```

For the numerical evidence alone, use `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-dpq-skew-data/seal-current.py`, followed by the final-consumer steps listed in `late-st-finish.py`. `study.py` reconstructs baseline source from the named commit into ignored scratch storage; the evidence directory contains no source-tree archive. Reference values and golden samples are never regenerated by this study.

Merge note (Claude): the stale exclusion declarations in the four `truth.py` generators (the TASK-196 entries and sged's two TASK-189 natural-tail rows, all recovered here) were removed and only those four tables' `input_sha256` fingerprints refreshed; numerical bytes unchanged. st's new `## d/p/q/r` section was moved before `## Details`.
