# TASK-367 · Pareto IV threshold subtraction

The held-scale fit on `(-5, 10)` now reports certified `shape2 / 2^54 = 0.22777021020091306`. The held-scale Pareto face on `(5, 9)` reports exponent `2573485501354569.5`. Both meet the default relative coefficient tolerance. The native and double-only replays agree on these outputs. [Native replay](2026-11-26-pareto4-subtract-data/fixed.txt) · [Double-only replay](2026-11-26-pareto4-subtract-data/fixed-double.txt)

## Baseline and independent reference

I replayed all hold masks on the unedited job base `2d89fd4a` before changing the fitter. Set `U = 2^54`, `min = -U` and `scale = U`. The reference script uses 100 decimal digits in mpmath, works with the original integer offsets and reproduces the problem on the offsets obtained by binary64 subtraction. It differentiates the profiled log likelihood for the finite root and evaluates the closed-form exponent for the face. The prior math report supplies the outward score signs and the exact face comparison. [Baseline replay](2026-11-26-pareto4-subtract-data/baseline.txt) · [Reference code](2026-11-26-pareto4-subtract-data/reference.py) · [Reference output](2026-11-26-pareto4-subtract-data/reference.txt) · [Outward sign checks and derivation](2026-11-25-pareto34-math-report.md)

| Sample | Quantity | Original-input reference | Rounded-input reference | Baseline emitted |
|---|---|---:|---:|---|
| `(-5, 10)` | `shape2 / U` | 0.22777021020090577982701159805039 | 0.28471276275113222161598527445867 | 0.28471276275114127, certified |
| `(5, 9)` | Pareto exponent | 2573485501354569.683673469387755 | 3002399751580331.222222222222222 | Numerical failure |

For the root, binary64 subtraction changes the offsets from `(-5, 10)` to `(-4, 8)`, producing the 25% error. For the face, it changes `(5, 9)` to `(4, 8)`, increasing the exponent by 16.7%. On this baseline the unscaled interval sum loses its positive lower bound and stops before emitting that exponent.

## Implementation and accuracy

- `pareto4/shifted_scale.hpp:6` computes the error-free low part of the excess. Near a held scale, Sterbenz's lemma makes the difference between the rounded excess and scale exact. The residual is added to that difference before division and `log1p`, so the rounding error scales with the small log ratio. Away from the scale, the logarithm of the rounded excess receives the `log1p(residual / excess)` correction.
- `pareto4/shifted_scale.hpp:35` propagates practical floating-point intervals for the log ratios and the log-excess Jacobian. It normalizes the log intervals before summing or scoring. The fixed-scale proof gives a unique finite root when a nonconstant sample has an observation below the scale. The implementation requires separated score signs and checks the coefficient intervals before publishing that root. The same normalization keeps the Pareto exponent denominator informative near the face. The intervals follow the Burr certificate's practical padding model. The exact-arithmetic proof is conditional on the computed comparisons.
- `pareto4/mle.hpp:65` selects the original-input scalar route for inexact subtraction with held scale. If its certificate cannot close and the delegated search returns a finite candidate, the candidate must pass the original-input accuracy check and is flagged uncertified. The singleton inventory row at `pareto4/mle.hpp:96` records the strict-concavity proof for successful scalar certificates.
- `pareto4/shifted_scale.hpp:22`, `pareto4/centered.hpp:26` and `pareto3/mle.hpp:77` use centered log ratios with the residuals of the anchor and raw spacing. The near-anchor quotient includes their first-order corrections. The neglected products are second order in binary64 roundoff. Pareto III already polished on the original observations, and the replay found no wrong certified coefficient on its successful routes.
- `pareto4/shifted_faces.hpp:6` encloses centered powered excesses on the affected held-power route. The range bound or the widened two-observation comparison can prove a Weibull face. A finite candidate goes through the original-input local accuracy check and retains unknown cardinality and an uncertified flag. At `shifted_faces.hpp:80`, two distinct exact and rounded supports are related by an increasing affine map in log coordinates. In that case the joint Pareto decision transports and the exponent and limiting height use the original spacing.

## Route sweep

The replay evaluated `(-5, 10)`, `(5, 9)`, `(1, 5)` and `(-1, 1)` at the distant held threshold. Held values in the sweep were first shape 1, power `U/4` and scale `U`. The unrestricted threshold path decides unboundedness before any excess translation.

| Pareto IV holds in addition to the threshold | Result of the sweep |
|---|---|
| None | On `(-5, 10)` the Pareto exponent moved from `3002399751580331` to `2401919801264265`. The original-input reference is `2401919801264264.866666666666667`. |
| Scale | The root and face fixtures above were corrected. |
| First shape | The concentrated path uses the compensated anchor and spacing. Existing numerical restoration stops remained on the affected examples. |
| First shape and scale | On `(-5, 10)` the fitted power divided by `U` moved from `0.25114061049066178` to `0.20091248839252679`, in agreement with the original-input Pareto III result. |
| Power | On `(-5, 10)` the rounded problem selected Weibull. The original-input fit returns first shape `0.60616327990858043` with certification withdrawn. The 100-digit reference gives `0.60616327990857864798230917816382` and a finite gain of `0.07175833092003138` per observation over the Weibull face. |
| Power and scale | On `(-5, 10)` the first shape moved from `0.81960841043598243` to `0.70650936363417616`. The 100-digit original-input value is `0.70650936363417601918601929544356`. |
| Both shapes | The represented coefficients remained within the scale tolerance on the sweep. Likelihood evaluation uses the original inputs when subtraction is inexact. |
| Both shapes and scale | Likelihood evaluation uses the held coefficients and original inputs. |

For Pareto III the sweep covered free shape and scale, held shape, held scale and both held. The successful coefficients agreed before and after the correction on these samples. Rounded support collisions can still stop in the parent nomination before polishing.

C++ pins the two requested cases, the held-scale and power first-shape error, the joint two-support Pareto exponent and the held-power finite fit. R pins the two requested cases through `mlpareto4` and its limit payload. Existing tests and goldens were preserved. [C++ tests](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/test.cpp#L393) · [R tests](../../../bindings/r/distributions/pareto4/test.R#L263) · [Replay source](2026-11-26-pareto4-subtract-data/replay.cpp)

## Checks

All final runs were on the workstation. The explicit command was `/tmp/umvl-cpu/run4 just test-changed --no-long-double`. The runner classified the single edited singleton-proof inventory row as shared validation input and selected all 86 families. Native C++ passed 136/136 and double-only C++ passed 181/181. R dev passed 38,012 assertions with 157 skips, zero failures and zero warnings. JS built and passed 601/601. `just site-check` passed with the existing Quarto Chromium executable supplied through `BROWSER_EXECUTABLE`. The first site run located no browser in the default Playwright cache. [Suite records](2026-11-26-pareto4-subtract-data/suites.txt)

Native and double-only fit snapshots each matched all 313 stored records across Pareto III and IV. Both modifier modes have zero records for these families because neither is registered in the modifier replay. Fit density and fit consistency each found zero new issues, and strict uniqueness covered 218 source sites. [Native diff](2026-11-26-pareto4-subtract-data/diff-native.txt) · [Double-only diff](2026-11-26-pareto4-subtract-data/diff-double.txt) · [Native modifier inventory](2026-11-26-pareto4-subtract-data/diff-routes-native.txt) · [Double-only modifier inventory](2026-11-26-pareto4-subtract-data/diff-routes-double.txt) · [Density](2026-11-26-pareto4-subtract-data/fit-density.txt) · [Consistency](2026-11-26-pareto4-subtract-data/fit-consistency.txt) · [Uniqueness](2026-11-26-pareto4-subtract-data/strict-uniqueness.txt)

## Open

- The general joint Burr transfer still needs an entry point that accepts original-input log boxes. An affine log map is available for two support values, and a general sample can change shape under subtraction. The same generalization needs original-input root and height bounds for joint finite and Weibull winners.
- The old representation guards are still active for rounded artificial ties and for a rounded held-endpoint equality in a nonconstant Pareto IV sample on the two-free-shape route. For example, `(1, 5)` with both shape parameters free and scale held at `U` still stops at the equality guard. Pareto III can likewise stop before its centered refinement if the rounded parent input collapses. Extending the centered route through those cases requires revisiting the existing tests that pin these stops.
- The joint Pareto payload holds the endpoint as one binary64 excess. For `(-5, 10)` the exact endpoint is `U-5`, so the payload uses the supported representable endpoint `U-6` and reports the original-input limiting height. Matching that height by evaluating the transmitted face requires a compensated endpoint representation in the payload.
