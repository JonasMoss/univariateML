# TASK-229 B2 — Continuous-family maximizer sets

Thirteen families publish version 1 maximizer sets. The selected estimate remains the first representative. Both direct snapshot modes preserve the estimates, log likelihoods, and outcome kinds of all 12,019 stored cases. The beta-negative-binomial reflection retains comparison candidates with `unique = NA`. The location-scale Student-t symmetry stop retains its witnesses.

## Published sets

| Family | Complete set and proof | Matched snapshot records with typed payloads per mode |
|---|---|---:|
| GEV | One Box with held location and scale and shape in `[-1, 1]`. Zero residual gives density `1/(eσ)` throughout the fitted shape domain. [Shape certificate](../../../univariateML-cpp/include/cppunivariate/distributions/gev/shape_certificate.hpp#L99) | 1 |
| Gompertz | One Box with held `a` and `b > 0`. The density at zero equals `a` for every admissible `b`. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/mle.hpp#L427) | 1 |
| Skew normal | One Box with held location and scale and every finite skew. The skew factor is one at zero residual. The analytic branch returns directly without a Newton solve in that flat coordinate. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp#L268) | 1 |
| Skew t | One Box with held location, scale and degrees of freedom and every finite skew. The skew factor is one at the held location. [Skew certificate](../../../univariateML-cpp/include/cppunivariate/distributions/st/certificate.hpp#L29) | 1 |
| Laplace | The closed real median interval and a fixed scale. The absolute-deviation sum reaches its minimum exactly on the interval and determines the same profiled scale throughout it. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/mle.hpp#L29) | 21 |
| Discrete Laplace | The closed median interval on the integer lattice, with the scalar scale optimum fixed. The same deviation sum determines the scale throughout the interval. The selected location is still the smallest integer median. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dlaplace/mle.hpp#L42) | 3 |
| GED | The median Box at a proved Laplace shape uses the original median endpoints and the common held or profiled SD. A balanced two-knot sample at held subunit shape and SD has two exact Points by strict concavity of the power loss between the knots and monotonicity outside. [Set construction](../../../univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp#L567) | 2 |
| Generalized beta | One Product over the two positive shapes at held shape2 equal to one and held endpoint. The transformed exponential log likelihood is strictly concave in the product. Its maximizing level is the reciprocal of the mean log ratio and carries the rounding flag. The representative convention still gives one effective likelihood degree of freedom. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp#L184) | 1 |
| Tukey lambda | A Box with held scale and the uniform member's closed translation interval, or two Points at the only support contacts for held shape in `(1, 2)`. The concavity of the quantile derivative gives the contact equality condition. The free-shape restriction selects the uniform member at shape one. Translated bounds carry rounding flags when needed, and rounded contacts carry coefficient enclosures. [Constant-location fitter](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp#L46) | 4 |
| Triangular | Exact Points at held symmetric endpoints. Free-endpoint pairs use the analytic balanced-pair formula or an enclosure from strong concavity in reciprocal endpoint widths. Mode coverage excludes the other knots and reflection establishes equality. [Enclosure](../../../univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp#L199), [mode coverage](../../../univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp#L537) | 4 |
| Skew GED | Two enclosed reciprocal skews at held mean, SD, and shape `9/4`. The centered skew profile has one crossing, and the reciprocal map preserves its likelihood. Bisection supplies both coefficient bounds. [Certificate](../../../univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp#L553) | 1 |
| Fatigue | Two enclosed reciprocal scale branches for constant shifted observations and held shape above two. The likelihood has a unique optimum in `w = sqrt(β/y) + sqrt(y/β)`, and the optimum has two inverse images. The threshold is held exactly. [Set construction](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/mle.hpp#L172) | 0 |
| Standardized Student t | Two enclosed Points for equally weighted atoms more than 30 held SDs apart. Minimizing the product of the two density denominators gives the two reflected locations as a function of shape. The off-center profile has exactly one shape root and the variance bound excludes the centered branch. [Profile proof](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md#L320), [coefficient enclosure](../../../univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp#L196) | 0 |

The independent recipes reconstruct intervals, exact knots, relation levels, and reference roots from observations and holds. The three-knot triangular case uses a separate quadratic score calculation. The SGED and standardized Student-t roots use 70-digit calculations. The density replay checked the 50 selected fits with typed payloads in each precision mode and their representative rows. Its largest relative discrepancy among the selected fits was 3.4 × 10⁻¹⁴. [Recipes](../../../dev/validation/flat_maxima.py#L426), [density replay](2026-11-23-maxset-cont-data/density.txt)

## Snapshot and consistency results

| Workstation replay | Fresh records | Matched records changed | New fixtures | Missing |
|---|---:|---:|---:|---:|
| Native | 12,029 | 42 | 10 | 0 |
| Double-only | 12,029 | 42 | 10 | 0 |
| Modifier routes, native | 2,003 | 0 | 0 | 0 |
| Modifier routes, double-only | 2,003 | 0 | 0 | 0 |

Each direct mode gained 40 typed payloads on matched records. This replaces legacy witness rows and descriptions where present. The discrete Laplace migration removes the three tie warnings. The analytic SN and ST plateaus now return ordinary core success statuses with zero iterations and omit their redundant comparison bounds. The remaining two matched changes are the beta-negative-binomial golden, which reports undetermined uniqueness while retaining its certification, and the triangular subnormal class, which reports undetermined uniqueness and withdraws certification. In that triangular class, the observations are consecutive subnormals and binary64 spacing prevents a contract-sized enclosure of the free endpoints. Both evaluated modes remain available.

The ten new fixtures cover one discrete median lattice, one fatigue reciprocal pair, two GED fits, two Laplace hold patterns, one standardized Student-t pair, two rounded Tukey supports, and one triangular three-knot sample. Field-level before-and-after evidence records all matching changes and all added keys. [Native](2026-11-23-maxset-cont-data/fields-native.jsonl), [double-only](2026-11-23-maxset-cont-data/fields-double.jsonl), [modifier native](2026-11-23-maxset-cont-data/diff-routes-native.txt), [modifier double-only](2026-11-23-maxset-cont-data/diff-routes-double.txt), [reproducer](2026-11-23-maxset-cont-data/replay.sh)

The combined consistency check judged all four snapshots. It reported zero new findings and 112 existing ledger entries. These two entries now pass. [Consistency output](2026-11-23-maxset-cont-data/strict.txt)

- `phenomenon:flat-maximum|native|genbeta|class|constant-flat-power-product`
- `phenomenon:flat-maximum|double|genbeta|class|constant-flat-power-product`

## Changed test assertions

| Tests | Change |
|---|---|
| GEV and Gompertz, C++ and R | Check typed Box membership and witness likelihoods in C++. In R, match the rendered interval or ray and accept all distinct valid representative rows. |
| SN, C++ and R | Check the full skew axis and equal point likelihoods through the Box and its representatives. |
| ST, C++ and JavaScript | Check the analytic success status, the full skew axis, and unchanged endpoint likelihoods. JS keeps the selected alpha at −1 and checks that a second distinct representative is available. |
| Discrete Laplace, R | Expect a silent fit and verify the integer interval and the same smallest median. |
| GED, R and JavaScript | Replace legacy message, candidate, and redundant comparison-bound assertions with the typed median endpoints, held coordinates, exact selected-row identity where exercised, and representative point likelihoods. The normalization test checks the original median endpoints in the wire Box and the displayed rows. |
| Generalized beta, R | Check the rounded Product, the representative products, and the effective degree of freedom. Keep the check of the selected product. |
| Tukey lambda, C++ and R | Check Box or Points membership and endpoint densities. Match the common rendered diagnostic and accept all distinct admissible representative rows. |
| SGED, C++ and R | Check both enclosed Points and keep the reciprocal and density assertions. Match the approximation label in R. |
| Triangular, C++ and R | Require a Points payload for the proved reflected modes. Keep the reflected coefficient and likelihood assertions. |
| Fatigue, R | Check both typed Points and their two representative rows. Comparison candidates remain separate. |
| Standardized Student t, C++, R and JavaScript | Check the certified enclosed pair at default tolerance. Keep the former comparison-bound and witness checks at a sub-epsilon tolerance. The JavaScript location-scale Student-t witness-stop assertions continue to run. |
| Beta-negative-binomial, C++ and R | Check undetermined uniqueness and retain the reflected candidate and likelihood checks. R verifies the absence of a typed set and the presence of the two comparison rows. |

## Workstation verification

| Check | Result |
|---|---|
| Native and double-only C++ via `just test-changed --no-long-double` | 135/135 and 180/180 passed on GCC 15.2.0. The recipe selected all 85 families because the diff includes the shared fixture table and JS assertions. |
| R dev tier via the same recipe, then focused GED rerun | The first run passed 37,395 expectations with 157 skips and seven failures on legacy assertions in three GED tests. The assertions were updated against the typed contract. The focused rerun passed 529 expectations with eight skips and zero warnings or failures on R 4.6.1. |
| `just js-build` and `just js-test`, then focused JS rerun | The build succeeded. The first test run passed 591 cases and found two assertions that still expected legacy GED rows and the Student-t default stop. Both updated cases passed the focused rerun against the same build. |
| `just site-check` | Passed for 85 family pages and all 85 generated C++17 examples. The browser, numerical, inference, graph, catalogue, and link checks passed. |
| Snapshot comparisons, combined consistency, and typed density replay | Passed as reported above. |

The simbox SSH connection timed out. All final checks ran on the workstation. The late fixes changed test assertions, so the affected R and JavaScript checks were rerun against the compiled implementations. [Suite evidence](2026-11-23-maxset-cont-data/suite-results.txt)

## Open

- **Beta-negative-binomial.** The global ordered cover confines every canonical maximizer to a coarse box of relative radius 2.5 × 10⁻⁹. A reflection separates the labels. To publish enclosed Points, the canonical box still needs a proof of a single stationary point and a simultaneous enclosure at the requested tolerance for size, alpha, and beta. The optional conditional alpha profile enclosure establishes neither obligation for the whole joint fit. [Cover](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/joint_certificate.hpp#L202), [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#L554)
- **Location-scale Student t.** The centered-scale profile is excluded by a strict likelihood comparison. Reflection proves attainment on both sides for the subunit-shape symmetry stop. Enumeration and contract-sized location/scale enclosures for every off-center maximizing branch remain open. The separate held-`df = 1` Cauchy-curve route has no set transport into the three location-scale Student-t coordinates. [Symmetry proof](../../../univariateML-cpp/include/cppunivariate/distributions/lst/symmetry_certificate.hpp#L55), [Cauchy delegation](../../../univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp#L113)
- **Remaining ambiguous comparisons.** Triangular fits with overlapping mode evidence or insufficient endpoint resolution retain candidates and report `NA`. GED retains unknown uniqueness when the selected fit survives an unresolved shape comparison or a subunit knot reflection lacks complete, accurate coverage. Other SGED and fatigue comparisons retain their existing candidate or stop mechanisms. The standardized Student-t reduction covers the stated balanced separated case, with its comparison stop retained when the coefficient enclosure cannot close. [Triangular outcome](../../../univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp#L605), [GED outcome](../../../univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp#L707), [SGED outcome](../../../univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp#L686), [fatigue outcome](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/mle.hpp#L53)
