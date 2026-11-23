# TASK-229 part A — Typed maximizer sets

Binom, nhyper, GPD, and Cauchy now publish complete version 1 maximizer sets for their proved non-unique fits. All four snapshot comparisons preserved the selected coefficients, log likelihoods, and outcome kinds. The direct comparisons recorded new set payloads and undetermined uniqueness flags. The modifier-route comparisons recorded the schema addition. The worktree base was `10d1fe08`.

## Core and bindings

- The C++17 value uses fixed storage for up to five coordinates, three pieces, and four points in a Points piece. Builders check point enclosures, normalize open integer bounds, propagate count orders, normalize rational levels, and validate the four relation forms. Hold slicing removes empty pieces, deduplicates exact values, and checks singleton unions. The semicircle solver scales the product before taking its square root and can intersect a held scale with both arcs. The count-ratio solver divides before multiplying at the count cap. [Core value](../../../univariateML-cpp/include/cppunivariate/core/maximizer_set.hpp#L381) · [Relation solver](../../../univariateML-cpp/include/cppunivariate/core/maximizer_set.hpp#L174) · [Core tests](../../../univariateML-cpp/tests/test_maximizer_set.cpp)
- The common binding owns an immutable copy only for a published set. The initial site build exposed the WebAssembly stack cost of putting the whole value inline in every binding result. The shared pointer keeps ordinary binding results small while the core retains fixed storage. The common renderer validates the returned coefficients and public names, produces the representative rows and diagnostic, and retains the returned estimate verbatim as row one. Additional rows are illustrative. Comparison evidence has its own payload. [FitResult](../../../bindings/common/fit.hpp#L38) · [Representatives](../../../bindings/common/fit.hpp#L125) · [Renderer](../../../bindings/common/fit.hpp#L198)
- R exposes the versioned list as `attr(fit, "maximizer_set")` and the rows as a data frame. Print and summary use the same stored description and show a representative table for sets with free axes. JavaScript uses the same schema and exposes the rows as objects. `fit_payload()` exposes the typed payload and preserves its candidate alias for legacy fits. JSON uses round-trip binary64 decimals and explicit `"-inf"` and `"inf"` bound strings. R uses native infinities. Rational levels use decimal strings for the numerator and denominator. Rounded analytic bounds and levels carry a flag and are displayed as approximate. [R conversion](../../../bindings/r/scripts/common_fit.h#L36) · [R printing and inference](../../../bindings/r/R/code_helpers.R#L476) · [JS conversion](../../../bindings/js/bindings.cpp#L61) · [Wire encoder](../../../bindings/common/maximizer_set.hpp#L134) · [R test helper](../../../bindings/r/tests/testthat/helper-render.R#L1)
- Public uniqueness takes three values. `TRUE` denotes identified uniqueness, `FALSE` denotes established multiplicity, and R `NA` or JS `null` denotes an unresolved comparison. The unresolved fits retain their comparison candidates and inference refuses them. Legacy families with established multiplicity continue to use their existing rows and prose during migration. Both snapshot modes and their modifier-route variants carry the new field. The snapshot probe retains its read path for older results. [Core outcome](../../../univariateML-cpp/include/cppunivariate/core/outcome.hpp#L28) · [Snapshot fields](../../../dev/validation/fit-snapshot.py#L181)

## Pilot evidence

| Pilot | Published pieces and proof |
|---|---|
| Binom | A zero sample has likelihood one on the union of `size = 0, prob in [0,1]` and `prob = 0, size in [0,Inf)` on the integer lattice. Holds select the applicable slices. A positive constant sample with an exact adjacent ratio of one gives two exact Points by monotonicity of the size increment. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/binom/mle.hpp#L144) |
| Nhyper | Zero samples give `r = 0, 0 <= m <= N <= 9007199254740991`. A positive constant gives the point-law Box with `m = N`, `r = x`, and `x <= N <= 9007199254740991`. Adjacent scalar fits give exact Points when the likelihood ratio equals one. For composition and stopping-count comparisons, the certificate tests the per-observation products and can cancel factors of the full integer ratio within the work budget. The population crossing already uses exact integer cancellation. [Fitter and adjacent proof](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/mle.hpp#L528) |
| GPD | With scale held, zero excess at `mu = x` gives a shape ray `xi in [-1,Inf)`. On a constant sample, the uniform member gives `xi = -1` and `mu in [x-sigma,x]`. The free-threshold and free-shape fit carries both pieces. The subtraction residual identifies a rounded lower endpoint. Overflow of that endpoint uses the signed-infinity encoding with the rounding flag. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/mle.hpp#L175) · [Held threshold certificate](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/certificate.hpp#L162) |
| Cauchy | For a balanced two-point sample with anchors `a < b`, the product of the two density denominators minus `(b-a)^2 * scale^2` equals `((location-a)*(b-location)-scale^2)^2`. Its equality set is the positive-scale semicircle. Below half the gap, fixed scale intersects the two reflected locations. The held-scale fit publishes enclosures from the two isolated numerical branches, with the scale held exactly. The numerical certificate uses the fitter's accuracy and comparison margins. [Free semicircle](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp#L250) · [Held-scale enclosures and proof](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp#L415) |

The adjacent binomial tie encodes as follows.

```json
{"version":1,"coordinates":["size","prob"],"pieces":[{"kind":"points","points":[{"estimate":[1,0.5]},{"estimate":[2,0.5]}]}]}
```

The zero-urn Box uses order `[[0,1]]` over coordinates `["m","N","r"]`. The balanced Cauchy relation uses `"form":"semicircle"`, anchors `[1,5]`, driver 0, and dependent 1. The six captured examples include the binomial and GPD unions and the two Cauchy enclosures. [Wire examples](2026-11-22-maxset-core-data/wire_examples.json)

## Undetermined uniqueness audit

| Path | Reason the fit reports `NA` or `null` |
|---|---|
| Triang | Held-endpoint modes survive overlapping comparison bounds. The evaluated modes are retained under `candidates`. The existing proved reflection cases retain their legacy multiplicity output. [Outcome](../../../univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp#L492) |
| Cauchy | A held-scale comparison can supply an accurate selected root while the other roots remain indistinguishable. The exact balanced two-point proof and disjoint accurate enclosures are required for the typed pair. [Outcome](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp#L581) |
| Nhyper | An adjacent composition or stopping-count candidate can fall inside the numerical tie margin before the exact ratio test succeeds. The fit keeps the selected integer estimate and both evaluated comparison points if the equality check cannot close. [Fit wrapper](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/mle.hpp#L592) |
| Hyper | A scalar adjacent comparison inside the rounding margin supplies an undecided pair when the per-observation integer equalities cannot establish the tie. The adapter keeps the two evaluated points. Analytic zero and point-law cases remain on the legacy path. [Tie audit](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/mle.hpp#L745) · [Adapter](../../../bindings/common/distributions/hyper/fit.hpp#L105) |
| Fatigue | Nonconstant samples can yield two isolated conditional roots with overlapping likelihood evidence. The successful candidate keeps that evidence and reports unknown uniqueness. The closed-form reciprocal pair for constant data retains established multiplicity. [Outcome](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/mle.hpp#L55) · [Constant pair](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/certificate.hpp#L93) |

The stored direct snapshots had 11 uniqueness changes in each mode. They were triang archive `008215` and fatigue archives `001457`, `001458`, `001459`, `001460`, `001461`, `001462`, `001469`, `001471`, `009357`, and `009817`. Regression cases exercise the Cauchy, nhyper, and hyper comparison paths separately. [Snapshot comparison](2026-11-22-maxset-core-data/snapshot_summary.txt)

## Strict equality and replay

The four pilot fixtures construct expected sets from the data and holds. The checker decodes the v1 value, verifies both uniqueness flags, partitions Box axes at the endpoints of both unions, and tests coverage of each feasible cell in both directions. Integer cells split at the count-order constraints. Their feasibility uses integer difference constraints, with one lattice step for a strict inequality. Compatible Relations compare normalized levels and merged driver intervals. The semicircle splits into two monotone arcs and uses high-precision projection of the dependent-axis bounds. Exact isolated points on a relation are checked algebraically before merging their coverage. Unresolved comparisons between expression forms fail with a diagnostic. [Independent recipes](../../../dev/validation/flat_maxima.py#L343) · [Box coverage](../../../dev/validation/maximizer_sets.py#L226) · [Set comparison](../../../dev/validation/maximizer_sets.py#L427)

For the reflected Cauchy fixture, the two reference locations were calculated independently with 70-digit decimal arithmetic and rounded once. Matching is one-to-one, with eight ulps allowed in branch containment. The fitted enclosure has to meet the coefficient tolerance at its farthest endpoint, with the documented centered scale used near zero and a two-ulp transport allowance. Holds and lattice coordinates compare exactly. The checker also checks the first-row identity, representative membership and likelihoods, and nested hold slices at equal likelihood. The density replay independently evaluates the point likelihood of each representative on the fitted law. [Strict integration](../../../dev/validation/fit-consistency.py#L209) · [Density replay](../../../dev/validation/fit-density.py#L221)

The negative tests cover a missing ray, an omitted endpoint, a missing count slice, an extra order cell, a missing semicircle point, a missing or overlapping enclosed branch, an overly wide enclosure, and malformed wire values. The positive tests cover equivalent decompositions and exact JSON transport. The R and JS tests cover the count cap, subnormals, adjacent binary64 values, and signed infinities. [Coverage and transport tests](2026-11-22-maxset-core-data/check_wire.py) · [R round-trip tests](../../../bindings/r/distributions/gpd/test.R#L228) · [JS round-trip tests](../../../bindings/js/stop-test.mjs#L658)

## Snapshot comparisons and final checks

All checks ran on the workstation, with GCC 15.2.0 for native code and R 4.6.1. The simbox runner returned 75 and the local slot runners handled the fallback.

| Check | Result |
|---|---|
| `just test-changed --no-long-double` | Green. Native C++ 135/135, double-only C++ 180/180, the recipe's R dev tier 37,384 passes with 157 skips, and JS 593/593. |
| `just test-full` | Truth checks 36/36. R full had 66,915 passes, one skip, zero warnings, and zero failures. |
| `just cpp-test` after the final canonicalisation changes | 135/135. The new core target exercises five cases and 40 assertions. |
| `just js-build`, `just js-test` | Build succeeded and all 593 tests passed. A traced serial rerun established progress after the initial serial run was stopped. The changed-test recipe subsequently passed with the uninstrumented JS runner. |
| `just site-check` | Final rerun passed. It checked 85 family pages and compiled and linked all 85 C++ examples. The browser, numerical, graph, and catalogue checks passed. |
| Pilot `fit-consistency.py` | Native and double-only green. The late replay of 478 records per mode equaled the corresponding records in the full snapshots. |
| Typed pilot `fit-density.py` | All 27 returned estimates and 57 representative rows passed in each precision mode. |
| Four `diff_baseline.py` comparisons | Each direct run had 12,019 records, with 12,017 baseline matches by key and two new Cauchy fixtures. Of the shared keys, 11,981 changed only by gaining the nullable field, 25 gained typed pilot payloads, and 11 changed to undetermined uniqueness. Each modifier-route run had 2,003 records and only the nullable field changed. |

The direct payload differences include the canonical descriptions, representative rows, and removal of duplicate witness rows and redundant comparison-wide bounds from the typed proofs. The supplemental comparator exposes every changed field beyond an added `null`, and the selected coefficients, reported likelihoods, execution counts, and outcome kinds matched in all four modes. The 114 existing TASK-229 signatures refer to legacy cases and routes. The pilot checks reported zero new strict failures and cleared zero TASK-229 signatures. [Snapshot summary](2026-11-22-maxset-core-data/snapshot_summary.txt) · [Supplemental comparator](2026-11-22-maxset-core-data/snapshot_changes.py) · [Strict ledger](../../../dev/validation/strict-known.json)

## Changed assertions

| Test | Assertion change |
|---|---|
| C++ binom zero and adjacent cases | Check both original witness vectors for set membership and evaluate their likelihoods, using typed Boxes or Points in place of the old `StopDetail` rows. |
| C++ GPD held-scale flat and translation cases | Check the Box payload and its endpoint witness likelihoods. The flat shape-ray check asserts the fitted likelihood and the single Box in place of redundant comparison bounds and rows. |
| C++ triang and fatigue comparison cases | Assert unknown uniqueness through `uniqueness_known`. Keep the existing candidate counts and numeric checks. |
| R binom zero and adjacent cases | Require at least two distinct zero-atom representatives with the same first estimate and hold checks. Match the common diagnostic and the two exact size clauses. |
| R nhyper zero, positive atom, and adjacent cases | Match the rendered description. Check first-row identity, lattice membership, holds, and point-law densities in place of a prescribed second plateau row. Keep the two exact adjacent-population coefficient checks and match both population values in the description. |
| R GPD shape ray, translation, and union | Match the interval text and allow additional distinct rows for the union. Keep membership and density checks. |
| R Cauchy held-scale cases | Match the common description for the proved pairs. Assert `NA` and retain the comparison diagnostic for the unresolved non-two-point sample. Keep candidate and density checks. |
| R triang unresolved pair | Assert `NA`, preserve the same two modes under `candidates`, and check absence of a published maximizer set and its representatives. |
| Shared R outcome test | Check the typed Cauchy first-row identity, two enclosed branches, and separate comparison rows in place of choosing a row from the likelihood-bound ordering. |
| JS structured Cauchy stops | Match the common success diagnostic while retaining checks of the comparison rows and their coefficient values. |

New assertions cover the core builders and every relation form, the Cauchy semicircle, R and JS round trips, and the hyper and nhyper undetermined comparison paths.

## Part B

| Family or route | Piece mapping |
|---|---|
| Hyper | Ordered Box unions for zero and deterministic atoms, integer Boxes for unidentified scalar intervals, exact Points for proved adjacent ties, and Count Ratio for identified one-draw empirical proportions. |
| Bbinom and CMB | Size-zero and Bernoulli Boxes, with the corresponding zero or endpoint unions in CMB. Use exact Points for proved adjacent sizes and Part Over Sum for the bbinom size-one shape ray. |
| CMP, nbinom, PIG, dweibull | Boxes for the CMP zero rate, PIG zero shape, and the discrete Weibull flat shape. Nbinom uses the union of its size-zero and probability-one Boxes. |
| ZIB, ZINB, ZIP | Add the inflation-one Box to the applicable base zero-atom set. Map ZIB's mixed binary size-one path to the complemented Product. ZIP's zero atom has an inflation-one Box. |
| Zmbinom, zmgeom, zmlogarithmic, zmnbinom, zmpois | Boxes for absent positive components with the positive-component domains and held or fitted zero mass. Normalize the logarithmic hurdle coefficient alias. |
| Ztbinom, zmbinom, ztnbinom, zmnbinom | Map constant-one binomial paths to the size-one and probability-zero Box union. The negative-binomial paths use probability one and the allowable size interval. Include zero mass in the hurdle coordinates. |
| Tbinom, tpois, zipf, geom and nbinom window routes | Tbinom uses the open-probability Box and feasible atom. Tpois, ZIPF, geometric and held-size NB windows use the relevant interval Boxes. Apply route-specific support and holds before encoding. |
| GEV, Gompertz, SN, ST, Laplace, dlaplace, GED | Boxes for shape or skew plateaus and median intervals, with the appropriate fixed nuisance coordinates. Use the integer lattice for the discrete median interval. |
| Genbeta and Tukey lambda | Genbeta uses the Product with a rounded reciprocal-log level. Tukey lambda uses Boxes for uniform translations and Points for the two proved support contacts. |
| Triang, SGED, fatigue | Exact or enclosed Points for proved triangular modes, enclosed reciprocal SGED skews, and the constant-sample reciprocal fatigue pair. Keep the undetermined paths as comparisons. |
| Bnbinom, std, lst | Enclosed Points require complete accurate branch isolation. Keep existing comparison or stop behavior while the proofs are open. |

Migrate the legacy payload assertions with their routes, replay all 114 ledger signatures, and remove the legacy adapter path after typed coverage is established.

## Open

Hyper's nonconstant swap branch still needs an exhaustive comparison of surviving orbits to publish a complete set. For bnbinom the reflection proof needs global coverage and transport of the full coefficient enclosure, including alpha. The std and lst symmetry cases still need accurate isolation of every maximizing branch. These proof gaps belong to the remaining family migration. [Design inventory and coverage boundary](../../../dev/design/maximizer-sets.md)
