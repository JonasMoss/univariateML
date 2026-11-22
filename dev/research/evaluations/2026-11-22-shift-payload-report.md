# Shifted limit payloads for held-threshold fits

TASK-214.30 · `codex/shift-payload` · starting master `89b214edc3873622c95bfb705c1e738228b56f66`

Feller–Pareto and Pareto II now identify their nonzero-threshold limits and carry the threshold in both selected limits and registered comparison or budget candidates. Pareto IV now carries the translation in the C++ result, where its bindings can read it directly. The patch also records the pointwise factor at tied held endpoints in shifted Pareto and power paths. No fitting algorithm, likelihood value or certificate margin was changed.

## Reproduction

The workstation replay built the C++ binding-neutral probe and the R package from an archive of `89b214ed` in scratch. The C++ density consumer and R `dml` independently took each baseline payload’s coefficients and evaluated the same law on both coordinate systems. The baseline payloads had the correct excess-law coefficients and lacked the coordinate that told a consumer to subtract 2. [C++ master payloads](2026-11-22-shift-payload-data/master-cpp.jsonl) · [R master replay](2026-11-22-shift-payload-data/master-r.txt) · [C++ master density replay](2026-11-22-shift-payload-data/master-density-native.jsonl)

| Fit on original observations | Master model | Reported log likelihood | Master model evaluated on original data | Corrected payload evaluated on original data |
|---|---|---:|---:|---:|
| Feller–Pareto, eight copies of 4, `min = 2`, `shape2 = shape3 = 2` | `trgamma` | −4.909645111040875 | −36.2741127776022 | −4.909645111040876 |
| Pareto II, 3 through 12, `min = 2` | `exp` | −27.047480922384253 | −30.683844558747886 | −27.047480922384253 |

For a continuous base law with density `f`, the density of the translated law at `x` is `f(x - min)` because the translation has unit Jacobian. The coefficients and likelihood bounds from the excess fit therefore carry over directly. At an exact tied held endpoint, certain Pareto and power escape paths have a pointwise log factor, such as `-log(2)` per tie for the held-scale Burr path delegated to by Pareto IV. Adding that factor at the excess-law endpoint reproduces the likelihood along the path. A change of density at one point preserves the distribution. These are identities. The binary64 agreement in the table and in the tied samples is numerical evidence for the representations. [Core5b-a diagnosis](2026-11-14-core5b-a-report.md#open-shared-protocol-entry) · [Corrected native density replay](2026-11-22-shift-payload-data/fixed-density-native.jsonl) · [Corrected double-only density replay](2026-11-22-shift-payload-data/fixed-density-double.jsonl) · [Corrected R replay](2026-11-22-shift-payload-data/fixed-r.txt)

The two extra replays use Pareto IV on `{3,3,4,5}` with `min = 2, scale = 1` and Feller–Pareto on `{3,3,2.5,2.25}` with `min = 2, shape1 = scale = 1`. The corrected payloads reproduce −3.9656687089990204 and −0.6900728473062736, respectively. The Pareto IV master binding already reported the offset. Its missing endpoint factor accounted for a remaining discrepancy of `2 * log(2)`. The corrected selected limits and candidate rows agree with their C++ predecessors on base coefficients and likelihood bounds. [Corrected C++ payloads](2026-11-22-shift-payload-data/fixed-cpp.jsonl)

## Survey

The survey followed the fitters’ delegates and comparison witnesses, including budget paths and adapters that supplied a model name independently of the core. “Physical” means the law’s own coefficients already describe the original axis.

| Family or route | Limit or comparison payload | Offset on master |
|---|---|---|
| Feller–Pareto, held `min`, delegated GB2 faces | Weibull, inverse Weibull, transformed gamma, inverse transformed gamma, Pareto, power | Missing on registered excess-law payloads |
| Feller–Pareto, lognormal and double-Pareto faces | Lognormal or double Pareto on excesses | Already present in the shifted binding payloads. Lognormal now uses a core identity |
| Pareto II, held `min`, delegated Lomax | Exponential limit and exponential budget witness | Missing |
| Pareto IV, held `min`, delegated Burr | Weibull, Pareto and associated comparison witnesses | Binding adapter appended `min`, C++ core omitted it. The tied held-scale payload also lacked the endpoint factor |
| Pareto III, held `min` | Log-logistic family points and nonexistence stops | The family points include `min` |
| GPD, held threshold or uniform plateau | Own-law witnesses, including `xi = -1`, and a scale-collapse supremum | Own-law witnesses use physical `mu`. A collapse can report a value without a proper-law coefficient vector |
| Lomax and inverse Pareto | Exponential and inverse exponential, including budget witnesses | Physical rate or reciprocal transport, with a fixed origin |
| Burr, inverse Burr, GB2, transformed gamma and inverse transformed gamma | Weibull and inverse Weibull, Pareto and power, transformed and ordinary gamma faces, lognormal, double Pareto | Fixed origin. Their non-translation transformations are accounted for in the law or its physical coefficients |
| Fréchet and GEV | Gumbel on the free-location Fréchet route, own-law GEV witnesses and collapse paths | Physical location for Gumbel and GEV |
| Generalized gamma | Pareto and power, including comparisons in log coordinates | Physical endpoints after returning to the original axis |
| Generalized beta and nonstandard beta | Power or unit gamma for generalized beta, and collapse for nonstandard beta | Power uses its physical endpoint. Unit gamma carries its upper bound |
| Half-t, half-normal and half-Cauchy | Half-normal escape from half-t, own-law witnesses and collapse stops | Half-normal and family witnesses use physical `mu` |
| Fatigue life | Own-law candidates and concentration | Own-law witnesses include the threshold |
| Bhattacharjee, GED, skew GED and Huber | Uniform, normal, half-GED, reflected half-GED or Laplace as applicable | Physical endpoints or center coordinates |
| Skew-normal, two-piece normal, skew-t and two-piece t | Half-normal and half-t, reflected counterparts, and normal/skew-family faces | Physical location in the face coefficients |
| Location-scale t and standardized t | Normal, location-scale t, or a scale-collapse path | Physical location |
| Gompertz and shifted Gompertz | Exponential Gompertz face, own-law shifted-Gompertz witnesses | Fixed origin for the exponential. The shifted-Gompertz parameters specify its own law |
| Exponentially modified Gaussian | Normal and translated exponential | Normal location or explicit `mu` |
| Truncated normal modifier | Translated, reflected or truncated exponential | Existing payloads carry the endpoint or endpoints |
| Noncentral hypergeometric | Translated negative binomial | Existing `shift` coordinate |
| Discrete normal | Point mass and adjacent two-point limit | Physical atom locations |

## Protocol and implementation

A scalar affine field would share one transformation schema across models. It would require each serializer and consumer to propagate and interpret that field. The existing shifted identities already make the translation part of the model’s coefficient contract, so this patch follows that convention. R and JS can use their existing model-specific named-coefficient serialization. The new threshold-specific identities put `min` after the base coefficients. The `unavailable:` prefix signals the absence of a standalone fitter for that identity. The binding’s advice still names the base fitter and the required subtraction.

- `core/stop_detail.hpp:140` validates the new coefficient layouts. At line 158, `translate_stop_faces` lifts both selected limits and registered witnesses. The selected Pareto or power witness and its matching candidate carry the optional `endpoint_log_factor` after `min`. [Core](../../../univariateML-cpp/include/cppunivariate/core/stop_detail.hpp#L140)
- The excess fits attach the offset at `fpareto/mle.hpp:162`, `pareto2/mle.hpp:99` and `pareto4/mle.hpp:68,106`. The Burr and GB2 held-endpoint producers record the pointwise factor alongside the likelihood they already compute. [Feller–Pareto](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp#L153) · [Pareto II](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/mle.hpp#L96) · [Pareto IV](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/mle.hpp#L63) · [Burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/scale_certificate.hpp#L67) · [GB2](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/endpoint_certificate.hpp#L24)
- `bindings/common/fit.hpp:194,224` renders model names and coordinates. R puts them in the condition’s named `estimates` and candidate data frame. JS puts them in the error’s `estimates` and candidate rows. Those conversions already forward model-specific coordinates. [Shared binding](../../../bindings/common/fit.hpp#L194) · [R](../../../bindings/r/scripts/common_fit.h#L18) · [JS](../../../bindings/js/bindings.cpp#L33)
- `fit-density.py:198` resolves the shifted identity to its base density and subtracts the payload’s offset. For a held-endpoint factor it adjusts the log density only at the stated endpoint. It also understands the existing translated exponential and negative-binomial payloads. `fit-judge.py` consumes reported likelihoods for comparisons and `strict_common.py` loads the records. Density evaluation is in `fit-density.py`. [Strict density consumer](../../validation/fit-density.py#L198)

## Snapshots

The 36 surveyed families produced 6,832 cases per precision mode on the workstation. The truncated-normal modifier added 285 per mode. Each comparison used the corresponding stored archive with `build/scratch/diff_baseline.py`.

| Snapshot | Changed records vs. stored | New | Removed | Attribution |
|---|---:|---:|---:|---|
| Native | 18 | 32 | 0 | Reproduced at starting master, zero worktree moves |
| Double-only | 18 | 32 | 0 | Reproduced at starting master, zero worktree moves |
| Truncated-normal route, native | 0 | 0 | 0 | Exact stored match |
| Truncated-normal route, double-only | 0 | 0 | 0 | Exact stored match |

In each family mode, 15 changed records came from the held-SD `sged` location solver at `0a769bfd`. Six changed from numerical stops to estimates, while nine changed estimates, iteration counts and likelihood rounding. Three `nhyper` records changed candidates and warnings with the zero/nonunique work at `eb299654` and `ab494b5a`. The 32 added flat-maximum cases were already in the starting master’s fixture matrix. They comprise 23 `nhyper`, three `gpd`, and one each in `genbeta`, `gev`, `gompertz`, `sged`, `sn` and `st`. Both direct master replays matched the worktree exactly on every flagged family. All frozen `min` holds for the three affected families were zero. The four explicit nonzero-threshold replays show the expected payload changes and unchanged likelihood fields. [Native moves](2026-11-22-shift-payload-data/native-moves.tsv) · [Double-only moves](2026-11-22-shift-payload-data/double-moves.tsv) · [Native route comparison](2026-11-22-shift-payload-data/routes-native-diff.txt) · [Double-only route comparison](2026-11-22-shift-payload-data/routes-double-diff.txt) · [Frozen threshold coverage](2026-11-22-shift-payload-data/coverage.txt)

## Tests

Added C++, R and WASM likelihood tests for both counterexamples and for Pareto IV with held-endpoint ties. Added a C++ Pareto II budget candidate test with a negative threshold and an R Feller–Pareto tied-power check. Existing witness regressions retain their original samples, status assertions, coefficient checks and likelihood comparisons, with shifted model names and offsets asserted where applicable.

| Check | Machine and result |
|---|---|
| `just test-full` | Workstation after simbox returned 75. Truth checks passed. R had 66,860 passing expectations, zero failures or warnings, and one Zipf reference-data skip |
| Native and double-only C++ stages of `just test-changed --no-long-double` | Workstation, GCC 15.2. Native 134/134 and double-only 179/179 |
| R stage of `just test-changed --no-long-double` | Workstation. 37,329 passing expectations, zero failures or warnings, 157 tier skips |
| `just js-build` | Workstation, passed |
| Final `just js-test` after WASM build | Workstation, 591/591 |
| Fit snapshots and C++/R density replays | Workstation. Four corrected payloads matched in both C++ precision modes and both original counterexamples matched in R |
| Density consumer on shifted rows of the frozen snapshots | Workstation. In each mode the 44 EMG and two truncated-normal selected translated-exponential payloads passed |

The initial C++ build exposed a missing test include. Later C++ and JS runs identified older assertions that still expected excess-law identities. The C++ recipe was rerun after its test edits. Its final JS stage led to JS assertion-only edits and a standalone full `just js-test` rerun on the same WASM build. The snapshot comparisons and coverage inventory finished before a scratch-driver logging error skipped the queued JS rerun. The JS rerun and shifted-density snapshot check were then executed directly. [Frozen shifted-density check](2026-11-22-shift-payload-data/shifted-density-snapshot-check.txt)

## Open

- The strict density check can catch missing offsets on selected limits when the input is in its matrix. Running its density machinery on the two master records reproduced both disagreements. The frozen cases for these three families all hold `min` at zero, and the existing candidate check covers estimate-tie candidates. Extending coverage would require nonzero-threshold fixtures and evaluating finite face candidates against their bounds in limit, budget and unresolved outcomes. Both bounds and the selected limit’s point value have distinct checks.
- Zero-offset tied-endpoint paths still use their existing base-model payloads. For example, Pareto IV with `min = 0, scale = 1` on `{1,1,2,3}` has the same two endpoint factors as the shifted replay, while its binding retains a base Pareto payload. Extending the pointwise-factor representation to zero-offset and unshifted parent payloads would cover that separate consistency gap.
- Some exploratory Feller–Pareto exits inherit a named GB2 face without a coefficient vector. The corrected transport covers materialized witnesses. Evaluating the remaining exits would require retaining their selected face’s coefficients in GB2. The shifted double-Pareto payload already carries its offset and coefficients, and the strict density table lacks a registered density for that law.
