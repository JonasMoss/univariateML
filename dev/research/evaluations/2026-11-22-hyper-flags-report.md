# TASK-214.45 — Flag-only reporting for non-unique fits

The full R suite went from **29 warnings to zero**. Hypergeometric, negative hypergeometric, and triangular fits use the existing `unique`, `maximizers`, and `outcome_message` attributes. The standard R print and summary methods display “Maximum is not unique.” The selected coefficients and log likelihoods agreed with the stored baselines in all 482 cases per precision mode.

## Warning source and change

- **Hypergeometric.** The warning originated in the `Tie`, `Unidentified`, and `Ok` arms of `bindings/common/distributions/hyper/fit.hpp` on base `b6a00871`. The latter arm warned for the composition/draw-count swap, constant samples, and the one-draw Bernoulli ratio. The adapter now supplies the two adjacent or swapped rows and a diagnostic. Flat count ranges, point-mass families, and common-ratio families are described by a range or formula with representative rows. See `bindings/common/distributions/hyper/fit.hpp:105`.
- **Negative hypergeometric.** The same adapter-level pattern covered adjacent counts, unidentified composition at `r = 0`, and the constant-sample point-mass family. Adjacent fits carry both count vectors. The flat composition range and the population family with `m = N` have compact descriptions and representative rows. See `bindings/common/distributions/nhyper/fit.hpp:67`.
- **Triangular.** The adapter warned for either a near-tie in candidate log likelihoods or a proved reflection tie. A proved tie now carries the chosen and reflected coefficient triples. The C++ symmetry check also covers a free mode with both endpoints held, when the exact sum of the endpoints equals the exact sum of the sample extremes. For a symmetric sample, reflection about that center preserves the likelihood and the held endpoint constraints. The fitter resolves close comparisons when its bounds separate the candidate modes. An unresolved comparison carries the surviving modes and clears both certification and uniqueness. See `bindings/common/distributions/triang/fit.hpp:73` and `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:309`.
- **R transport and decision.** The adapters use the same path as binom, Cauchy, and ZIP. The R conversion attaches the candidate table and the adapter's diagnostic at `bindings/r/scripts/common_fit.h:58`. Both generic methods use `ml_print_outcome` at `bindings/r/R/code_helpers.R:481`. The decision “Non-unique maxima are reported by the outcome flag only (Claude, for Jonas to confirm)” is dated 2026-11-22 at `dev/roadmap/decisions.md:9`.

## Triangular mode comparison after review

For a balanced sample at two distinct observations $x<y$ with both endpoints held, the two mode likelihoods have the order of $(b-y)/(b-x)$ and $(x-a)/(y-a)$. Their difference has the sign of $(y-x)(a+b-x-y)$. Error-free binary64 sums resolve this sign even when the computed log likelihoods coincide. With endpoints 0 and 1, the represented values `.2` and `.8` sum exactly to `1 + 2^-54`, so the certified mode is `.8`.

For other close comparisons with both endpoints held, the fitter uses outward bounds on the likelihood ratio of the two modes. The common density height cancels. For free endpoints it compares feasible profile likelihoods with tangent upper bounds and a practical evaluation margin. Where the comparison remains unresolved, the fit retains its representative, returns the competing coefficient vectors through `maximizers`, and records the unresolved-comparison diagnostic in `outcome_message`. The outcome carries `unique = FALSE` and `certified = FALSE`. See `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:43`, `mle.hpp:309`, and `bindings/common/distributions/triang/fit.hpp:91`.

## Tests changed

All rows in the table check that the call emits no warning. The selected-coefficient assertions still run, including the revised mode value below.

| Test | Former warning expectation | Outcome and candidates checked |
|---|---|---|
| `bindings/r/distributions/hyper/test.R:33` | Adjacent success counts “tied” | `unique = FALSE`, `m = 9, 10` |
| `bindings/r/distributions/hyper/test.R:37` | Adjacent population counts “tied” | `unique = FALSE`, `N = 19, 20` |
| `bindings/r/distributions/hyper/test.R:41` | “every `m`” | `unique = FALSE`, range endpoints `m = 0, 7`, diagnostic describes the range |
| `bindings/r/distributions/hyper/test.R:46` | “Every `N`” | `unique = FALSE`, `N = 3, 4` as witnesses, diagnostic describes the full range starting at 3 |
| `bindings/r/distributions/hyper/test.R:103` | Adjacent draw counts “tied” | `unique = FALSE`, `k = 9, 10` |
| `bindings/r/distributions/hyper/test.R:107` | “Every `k`” | `unique = FALSE`, range endpoints `k = 0, 7` |
| `bindings/r/distributions/nhyper/test.R:21` | Composition “unidentified” | `unique = FALSE`, range endpoints `m = 0, 20` and the diagnostic |
| `bindings/r/distributions/nhyper/test.R:48` | Adjacent compositions “tied” | `unique = FALSE`, `m = 6, 7` |
| `bindings/r/distributions/nhyper/test.R:120` | Adjacent stopping counts “tied” | `unique = FALSE`, `r = 1, 2` |
| `bindings/r/distributions/triang/test.R:28` | “not unique” for free fit of `c(0, 1)` | `unique = FALSE`, both reflected triples, retained diagnostic, and standard print |
| `bindings/r/distributions/triang/test.R:68` | “not unique” for held-endpoint fit of `c(.2, .8)` | Pinned mode changes from `.2` to `.8`, with `unique = TRUE` and `certified = TRUE`. The two rounded profile likelihoods still agree within tolerance. The exact tie `c(.25, .75)` carries both modes. |

The hypergeometric tests also check both swapped rows and print/summary for a fully free fit, each listed witness against the held-population likelihood grid, two representations of the one-draw ratio, and the constant-sample point-mass family. The negative hypergeometric test checks two points on the constant-sample population family. The R and C++ triangular tests check the exact symmetry, the corrected `.8` mode, and an overlapping likelihood-ratio comparison with both modes retained. The C++ cases ran in both precision modes. The conformance file had no explicit warning assertion to convert. Its full run completed with zero warnings.

## Validation

| Check | Machine | Result |
|---|---|---|
| Full R baseline, master `b6a00871`, from the task brief | Workstation | 65,860 passes, 0 failures, 29 warnings, 1 skip. Warning contexts were hyper (25), nhyper (1), and conformance (3). |
| `just test-full` before the review fix | Workstation after simbox exit 75 | 36 truth checks passed. R had 66,025 passes, 0 failures, 0 warnings, and the existing ZIPF skip. |
| `just test-changed --no-long-double` after review | Workstation | 5/5 native and 5/5 double-only C++ tests. Targeted R had 1,897 passes, 0 failures, 0 warnings, and 6 skips. Its JS build and 238 selected JS tests passed. |
| `just site-check` after review | Workstation | Passed, including the 85 family pages and their C++ examples. |
| Initial native and double-only snapshots | Workstation | 482 cases per mode. Each mode had 39 raw binding differences, all in `warnings` or `candidates`. Each mode had 35 non-unique results with at least two distinct rows and the selected estimate first. No changes to C++ outcomes, selected coefficients, or likelihoods on these inputs. |

The review reran all 196 triangular cases in each precision mode. The only additional change was archive case `008215` with the upper endpoint held. Its close profiles remain unresolved, so it carries 12 distinct modes and both flags are false. The selected coefficients and log likelihood were identical in all cases. Both modes showed the same differences against the stored baseline. [Triangular review snapshots](2026-11-22-hyper-flags-data/review-snapshots.txt)

The snapshot driver ran with `--family hyper,nhyper,triang` in both modes. The comparison used the corresponding stored baselines and `build/scratch/diff_baseline.py`, first on raw output and then with the two estimate payload fields removed. The held-endpoint triangular test covers an additional exact tie outside the snapshot cases. [Snapshot comparisons](2026-11-22-hyper-flags-data/snapshots.txt) · [Comparison script](2026-11-22-hyper-flags-data/compare.py) · [Suite results](2026-11-22-hyper-flags-data/suite-results.txt)

## Open

- The all-zero negative hypergeometric routes with a free population or both design and composition free can still take an `Ok` shortcut with `unique = TRUE`. At `r = 0` the law is a point mass for every admissible composition and population. These routes had no tie-warning path. A follow-up needs hold-specific uniqueness checks, including the singleton zero-population control, and payload tests for those routes.
- The reported discrete candidate sets come from the existing tie classifications. An exhaustive characterization of incidental exact ties in the joint grids would require collecting and proving equality for other surviving profiles. The present comparison checks the reported witnesses and outcome fields.
