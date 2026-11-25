# TASK-255. Certified likelihood values with unknown maximizer cardinality

The fit outcome now carries value certification and cardinality independently. A certified fit can return the same estimate and likelihood with `unique = NA` in R and `unique = null` in JS. A singleton assertion requires a proof for the route taken. The four snapshot comparisons against the stored baselines changed 3,118 uniqueness fields, all from true to unknown. Every other field matched exactly.

## Outcome and public mappings

| Location | Contract |
|---|---|
| [C++ outcome](../../../univariateML-cpp/include/cppunivariate/core/outcome.hpp#L45) | `singleton(certified)` records proved cardinality one. `unknown_cardinality(certified)` preserves the supplied certification and sets `uniqueness_known = false`. `undetermined()` is the uncertified candidate constructor. |
| [Shared fit rendering](../../../bindings/common/fit.hpp#L206) | The typed maximizer-set requirement applies to known non-unique estimates. Unknown cardinality can use the ordinary point payload. |
| [R fit metadata](../../../bindings/r/scripts/common_fit.h#L128) | Unknown maps to `NA_LOGICAL`. [Model selection](../../../bindings/r/R/code_model_select.R#L164) carries the fit attribute through and uses `NA` when the attribute is absent. |
| [JS fit metadata](../../../bindings/js/bindings.cpp#L62) | Unknown maps to `null`. |
| [Strict source check](../../validation/strict-uniqueness.py) | An explicit proof inventory covers 218 constructor, delegate and cardinality assignment sites. An added site or changed guard expression requires review of the [proof table](../../validation/singleton-proof-sites.csv). `just strict` runs the check before the numerical validators. |

The starting audit was the 291 `unique-default-unsubstantiated` routes across 50 families in the [claims ledger](2026-11-24-claims-inventory-data/ledger.csv). The [family and route table](2026-11-25-uniq-na-data/route-decisions.csv) records before and after claims with their proof or missing premise. The source inventory also covers universal and closed-form singleton routes. The [certificate study](2026-11-25-uniq-cover-math-report.md) and [transfer study](2026-11-25-transfer-math-report.md) supply the route criteria. Dossiers for binomial, fatigue, folded normal, GEV, Lomax and Tukey lambda now describe the corresponding boundaries.

Universal or identified conditional routes keep true, including normal, gamma, exponential, discrete Weibull, strictly concave count slices and injective transfers. Where existing computations identify a single eligible region, the flag records the successful guard. Examples include a strict GEV profile or inward coefficient, one decreasing crossing in the fatigue cover, and strict adjacent comparisons in integer profiles. Exact expansion or FMA comparisons cover the relevant constant-sample count cases. A fully pinned, feasible parameter vector has cardinality one.

For held-location folded normal, set `u = 1/σ²` and `tᵢ = μ xᵢ u`. The second derivative in `u`, multiplied by `u²`, is `−n/2 + Σᵢ tᵢ²/cosh²(tᵢ)`. For nonzero `t`, the Taylor series gives `cosh(t) > 1 + t²/2 ≥ √2 |t|`. Thus each summand is strictly less than one half, including at zero, and the profile is strictly concave. The held-scale path uses its monotone score. A joint value certificate alone leaves cardinality unknown. For standardized skew Student, the bounded-skew route sets true when the curvature enclosure is strictly negative throughout the surviving interval hull. The numerical guards use the package's exact-arithmetic certificate standard conditional on computed comparisons with a practical floating-point margin.

## Snapshot comparison

Baseline source was master `7916e5e5`. The comparisons used the stored [fit snapshots](2026-11-15-fit-snapshot-data/) and ran both the supplied comparison and the retained [exact comparison](2026-11-25-uniq-na-data/compare.py).

| Mode | Aligned cases | True to unknown, certified | True to unknown, uncertified | Other changes |
|---|---:|---:|---:|---:|
| Native | 12,081 | 767 | 455 | 0 |
| Double only | 12,081 | 767 | 455 | 0 |
| Modifier routes, native | 2,006 | 321 | 16 | 0 |
| Modifier routes, double only | 2,006 | 321 | 16 | 0 |

There were zero added or missing case keys. Parameters, likelihoods, outcome kinds, raw statuses, limit information, and certification matched the baselines. The [move ledger](2026-11-25-uniq-na-data/snapshot-moves.csv) groups every change by mode, family, held coefficients, source, raw status and certification. In each base mode the largest groups were beta-binomial (105), skew Student (98), Tukey lambda (89), Feller–Pareto (87), GB2 (82), and standardized skew Student (73).

## Validation

All completed runs used the workstation under the slot lock. The C++ compiler was GCC 15.2. Simbox exited 75, so the long checks ran locally.

| Check | Result |
|---|---|
| `just cpp-test` | 136/136 |
| `just cpp-test-no-long-double` | 181/181 |
| `just test-full` | 67,741 passed, zero failures or warnings, one existing Zipf reference fixture skip |
| `just js-build` and `just js-test` | WASM build passed, 600/600 tests |
| `just site-check` | Passed with the final dossier text, before the late metadata fixes |
| `fit-density.py` and `fit-consistency.py` | Zero new failures across the final snapshots |
| `just strict` | Zero new failures. Singleton inventory 218 sites, fit judge 14,087 cases per precision, refinement 3,274 passes, certificate replay 7,722 judged cases, seam 7,395 passes and 2,300 expected stops per precision. |

The `test-changed --no-long-double` dry run selected all 86 families because of the shared changes. The full component recipes covered that selection and used the full R tier in place of the wrapper's dev-tier default. The first full R run exposed a missing singleton assertion for identified discrete Weibull fits, through the held-shape bootstrap test. The C++ native and JS first runs also exposed guard and test-import issues. The final full C++, R and JS runs above include the corrections.

New tests exercise the independent C++ outcome fields and folded-normal paths, preserve R model-selection `NA`, and verify JS `null` on the certified joint folded-normal fit. Covariance parity tests use explicit parameter points on routes whose public inference now correctly requires a proved singleton.

## Open

- **TASK-254, skew normal and standardized skew normal.** Both folders were left to the active skew-normal job. Retain proved held routes, and for free skew establish how many isolated roots attain the global value after comparison with the half-normal face. Enumerate the full reflected set when equality is proved. Migrate the remaining legacy outcome constructor in standardized skew normal so the compatibility overload can be retired.
- **TASK-256, Lomax and its transfers.** A displaced root still needs a valid upper bound on its maximum, or an exact identity identifying the competing solutions. Propagate singleton status through the reciprocal, shift and power maps only once the source route establishes it.
- **Routes with unresolved root counts.** Competing fatigue crossings need upper bounds on their full surviving intervals. Weak or overlapping GEV face comparisons and endpoint cases need separation or a complete contact count. Value-gap covers for Huber and free-shape Tukey lambda need local root isolation or curvature on the surviving hull. Subunit skew generalized error and other scalar skew or Student searches need a root count. Beta-negative-binomial joint routes need isolation of the canonical solution and proof of the full swap orbit. The route table records the other unresolved slices.
- **Strict replay coverage.** The replay emitted 330 undecided diagnostics, including tolerance-flagged ties without exact equality, boundary or budget limits, and Skellam cases beyond the replay's Bessel-series bounds. These are recorded with case keys and reasons in the [open replay list](2026-11-25-uniq-na-data/strict-open.txt). The strict known-failure file is unchanged.
