# Threshold limit payloads reproduce their likelihoods

TASK-233 · `codex/payload-offsets` · starting HEAD `73e8c077`, the status-only child of `0b587ddd`

The four snapshot comparisons show 21 new fixtures per family mode and 150 existing records with added face payloads per family mode. Existing fit estimates, populated likelihood fields, outcome kinds and certification flags matched. Both modifier-route snapshots matched exactly. The expanded density check resolved 12 discrepancies and introduced zero relative to the starting replay. Its 66 inherited findings are listed under Open.

## Fixtures and face checking

The frozen Feller–Pareto, Pareto II and Pareto IV cases held the threshold at zero. The new cases use `min = 2` for the ordinary shifted paths and `min = -3` for the exponential budget witness. Three zero-threshold endpoint cases make the pointwise factor independently observable. The Pareto II budget fixture sets `tol = 1e-8` and `iterlim = 1` through an optional probe input. The frozen rows keep their original input bytes and default controls. [Fixture definitions, fit-snapshot.py:127](../../validation/fit-snapshot.py#L127)

| Family | Added fixtures | Paths reached |
|---|---:|---|
| Feller–Pareto | 14 | Weibull and inverse Weibull through the unit-shape delegates. Transformed gamma and inverse transformed gamma through both the held-power delegates and the exploratory search. Lognormal, double Pareto, one-sided Pareto and power search faces. Tied held endpoints for both one-sided laws at thresholds 2 and 0 |
| Pareto II | 2 | Selected exponential face at threshold 2. Exponential face candidate in a budget stop at threshold −3 |
| Pareto IV | 5 | Held-power Weibull face, joint Weibull and Pareto faces, and held-scale Pareto with tied endpoints at thresholds 2 and 0 |

The density checker evaluates finite named face candidates in limit, budget and unresolved outcomes against their stated intervals. It retains the existing estimate-tie checks and the selected limit’s separate point-value check. All evaluations use the original observations, applying the payload’s translation before calling the base density and adding the log factor only at the specified endpoint. The allowance remains `1e-10` times the finite likelihood scale plus the summation term. A one-sided infinite bound contributes no scale to that allowance. [Candidate and density checks, fit-density.py:225](../../validation/fit-density.py#L225)

The double-Pareto checker uses the asymmetric Laplace density on log excesses with left rate `l`, right rate `r`, and kink `log(k)`. It subtracts `log(y)` for the Jacobian from log excess to excess `y`. An independent 100-digit mpmath check sums the two power-law pieces directly in excess coordinates, with normalizer `l*r/((l+r)*k)`. It agreed with the registered density in both modes. [Density implementation, fit-density.py:123](../../validation/fit-density.py#L123) · [Independent check](2026-11-23-payload-offsets-data/check-fixtures.py)

The table uses the new checker on both the starting and final payloads. These examples exercise translations already implemented by the preceding shift-payload job. The negative-threshold row checks a candidate enclosure. [Prior shift report](2026-11-22-shift-payload-report.md) · [Native replay](2026-11-23-payload-offsets-data/native-fixtures.jsonl) · [Double-only replay](2026-11-23-payload-offsets-data/double-fixtures.jsonl)

| Payload | Starting evaluation | Final evaluation | Reported point or bound |
|---|---:|---:|---:|
| Pareto II exponential, `x = 3:12`, `min = 2` | −27.047480922384 | −27.047480922384 | −27.047480922384 |
| Pareto II exponential budget witness, `x = {-2,-2,37,56}`, `min = -3` | −16.915304622885 | −16.915304622885 | Both bounds −16.915304622885 |
| Feller–Pareto shifted lognormal | −283.145269784910 | −283.145269784910 | −283.145269784910 |
| Feller–Pareto shifted double Pareto | −199.305220095791 | −199.305220095791 | −199.305220095791 |

## Pointwise factors at zero offset

The producers computed the endpoint penalty in the likelihood, while the previous payload transport appended it during a nonzero translation. The factor now travels as the trailing coordinate of the Pareto or power witness, including a matching candidate. Translation inserts `min` before the factor. The base identities remain available for zero-offset and unshifted parents. [Payload representation, stop_detail.hpp:122](../../../univariateML-cpp/include/cppunivariate/core/stop_detail.hpp#L122) · [Translation and factor attachment, stop_detail.hpp:158](../../../univariateML-cpp/include/cppunivariate/core/stop_detail.hpp#L158) · [Binding names, fit.hpp:316](../../../bindings/common/fit.hpp#L316)

- At the Burr held scale `s`, the density is `(k*c/s)*2^(-1-k)`. Along the Pareto escape, `k` tends to zero and `k*c` tends to the Pareto shape. The limiting density at `s` is half the Pareto endpoint density. The inverse Burr change of variables multiplies both densities by the same reciprocal Jacobian, so its power endpoint is also halved. The reciprocal delegate is covered directly because GB2 uses it for a held unit shape. [Burr proof, scale_certificate.hpp:71](../../../univariateML-cpp/include/cppunivariate/distributions/burr/scale_certificate.hpp#L71) · [Inverse Burr, mle.hpp:285](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp#L285)
- For GB2 with one beta shape fixed at `h`, let the other shape `v` tend to zero while the GB2 power times `v` tends to the face rate. The beta normalizer satisfies `1/B(v,h) ~ v`. At the held scale both softplus arguments are zero, so the density tends to `(rate/scale)*2^(-h)`. The Pareto and power endpoint density is `rate/scale`. Thus the log factor is `-h*log(2)` per tie in either direction. Translation has unit Jacobian and preserves the factor. [GB2 proof, endpoint_certificate.hpp:27](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/endpoint_certificate.hpp#L27)

| Zero-threshold payload on original data | Starting evaluation | Final evaluation | Reported likelihood |
|---|---:|---:|---:|
| Pareto IV, `{1,1,2,3}`, scale 1 | −2.579374347879 | −3.965668708999 | −3.965668708999 |
| Feller–Pareto power, `{1,1,.5,.25}`, scale 1, shape1 .4 | 0.696221513814 | 0.141703769366 | 0.141703769366 |
| Feller–Pareto Pareto, `{1,1,2,4}`, scale 1, shape3 .4 | −3.462661569546 | −4.017179313994 | −4.017179313994 |

The differences are exactly `2*log(2)` for Pareto IV and `2*.4*log(2)` for the two GB2 paths in exact arithmetic. In both precision modes the selected likelihood and its candidate bounds pass for each corrected payload. [Before/after records](2026-11-23-payload-offsets-data/native-fixtures.jsonl)

## GB2 face coefficients

The exploratory GB2 search carried the winning face status and comparison likelihood but discarded some face coefficients. It now retains the winning transformed-gamma, inverse transformed-gamma, Pareto or power vector, and appends its face witness after any existing family witness. The inverse transformed-gamma scale is transported by reciprocity. The Burr and inverse Burr delegates identify the named law explicitly before Feller–Pareto transports the excess fit. Exploratory selections keep their uncertified outcomes. [Delegate identities, gb2/mle.hpp:329](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp#L329) · [Face selection, gb2/mle.hpp:424](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp#L424)

| Feller–Pareto search face, `min = 2` | Starting evaluation | Final evaluation | Final reported likelihood |
|---|---|---:|---:|
| Transformed gamma | Empty vector | −75.516250576538 | −75.516250576538 |
| Inverse transformed gamma | Empty vector | −467.579859880366 | −467.579859880366 |
| Pareto | Empty vector | −160.348591393159 | −160.348591393159 |
| Power | Empty vector | 39.305043349591 | 39.305043349591 |

Two archive records, `001595` and `001598`, share a near-constant sample. Their original named Pareto exits had empty vectors. The first replay with coefficients exposed a standardized-log comparison value of `128.67957567055043` at a literal-coefficient density sum of `128.67957055576446`. GB2 now evaluates exploratory one-sided face payloads at their public endpoint and rate. Both new fields, the point likelihood and candidate bounds, report `128.67957055576446` for these cases. The original search comparison and chosen outcome are preserved. [Payload evaluation, gb2/mle.hpp:694](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp#L694) · [Narrow Pareto replay](2026-11-23-payload-offsets-data/narrow-pareto.jsonl)

A selected Feller–Pareto limit with an empty vector explicitly reports that its limiting coefficients are outside the representable range. A zero or infinite materialized scale or rate, or a non-finite density at the available one-sided coefficients, cannot supply a finite face witness. Unbounded constant-sample concentration has its own status and carries no proper-law vector. Every added limit fixture had a finite vector. [Binding diagnostic, fpareto/fit.hpp:26](../../../bindings/common/distributions/fpareto/fit.hpp#L26)

## Snapshot comparisons and tests

The workstation used GCC 15.2 for both precisions. The report compares all four fresh snapshots with the stored archives using `build/scratch/diff_baseline.py`. The affected families and added fixtures were also replayed against headers archived from `73e8c077`. After the one-sided likelihood correction, only GB2 and Feller–Pareto needed a second snapshot replay in each precision. Those results replace their original rows in the final comparison. [Complete record and field comparison](2026-11-23-payload-offsets-data/comparison.tsv)

| Mode | Records | Existing records changed | Added | Removed |
|---|---:|---:|---:|---:|
| Native | 12,053 | 150 | 21 | 0 |
| Double-only | 12,053 | 150 | 21 | 0 |
| Modifier routes, native | 2,006 | 0 | 0 | 0 |
| Modifier routes, double-only | 2,006 | 0 | 0 | 0 |

Each family mode has 68 Feller–Pareto and 82 GB2 payload changes. The replay checked 37 selected limits and face candidates across the 21 fixtures in each mode. R adds direct checks for zero-threshold Feller–Pareto and Pareto IV, and checks both exploratory transformed-gamma directions by evaluating the returned coefficients. Existing C++, R and JS endpoint assertions retain their status and coefficient checks and now check the additional factor. [Native diff](2026-11-23-payload-offsets-data/native-diff.txt) · [Double-only diff](2026-11-23-payload-offsets-data/double-diff.txt) · [Native route diff](2026-11-23-payload-offsets-data/routes-native-diff.txt) · [Double-only route diff](2026-11-23-payload-offsets-data/routes-double-diff.txt)

| Final check | Machine | Result |
|---|---|---|
| `just test-changed --no-long-double`, with `UMVL_TEST_TIER=full` | Workstation | Exit 0. Native C++ 180/180 and double-only C++ 180/180 |
| Full R tier in the final changed runner | Workstation | 67,041 pass, zero failures or warnings, one Zipf reference-data skip |
| `just truth-check` | Workstation | 36/36 |
| `just js-build` and `just js-test` | Workstation | Build passed and 593/593 tests passed |
| `fit-consistency.py` on fresh snapshots | Workstation | Zero failures |
| `fit-density.py` on fresh snapshots | Workstation | 66 inherited failures, zero new relative to the starting replay, 12 resolved |

Simbox returned 75 for the original `just test-full` attempt. The workstation truth checks passed and that full R run identified three older assertions expecting two-field Burr payloads. The final runner executed the full R tier after updating them, and the truth recipe was rerun on the final tree. The first native C++ pass also identified the existing family-witness order assumption and inverse Burr’s old payload width. Both assertions passed in the final native and double-only runs. [Suite output summary](2026-11-23-payload-offsets-data/suite-results.txt) · [Consistency output](2026-11-23-payload-offsets-data/fresh-consistency.txt)

## Open

The expanded checker returns a failure status for 66 inherited candidate discrepancies, 33 per precision. The starting replay reproduces each one. For example, the non-selected Pareto IV Weibull witness on the archived near-constant class evaluates to `93.877761880676786` against its reported interval `[97.477799483493428, 97.477799483496284]`. The corresponding Burr and inverse Burr candidates also have physical-coordinate mismatches. These witnesses need enclosures for the public coefficients, or a representation that preserves the coordinates used for their bounds. Other inherited rows include uniform candidates with observations outside the serialized support and finite density sums outside their enclosures. The logs identify every case. [Starting density audit](2026-11-23-payload-offsets-data/start-density.txt) · [Final density audit](2026-11-23-payload-offsets-data/fresh-density.txt)
