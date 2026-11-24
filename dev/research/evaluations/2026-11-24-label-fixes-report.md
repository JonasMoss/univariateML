# TASK-238 outcome label fixes

Branch `codex/label-fixes` starts at `12974141`. The changes apply the existing outcome contract to the evidence each fit has. All four snapshot replays and both strict checks finished on the workstation.

## 1. Skellam

The old joint search could return the zero-rate member as certified whenever the best computed interior value was within $5\times10^{-9}n$. The cover bounded the likelihood gap, and the zero-rate branch skipped local polish. A positive inward score refutes the face even when the gain falls below the selection margin.

For nonnegative data with positive mean, let $d=\bar x$ and $t=\mu_2$. Maximizing over the log rate ratio at fixed product gives $\mu_1=d+t$. The profile score at zero is $g_0=d\sum_i 1/(x_i+1)-n$. Reflection gives the other face. A positive score separated from zero refutes the face. A separated better interior likelihood also refutes it, including when $g_0<0$.

For a negative score separated from zero, the curvature upper bound $M=2\sum_i 1/(|x_i|+1)$ gives $g(t)\leq g_0+Mt$. The fit uses this to prove strict decrease on an interval adjacent to zero. It certifies the face only when the remaining finite intervals and the Poisson-mode tail bound give strict exclusion, with practical rounding allowances on the comparisons. This proves uniqueness conditional on those computed comparisons. When the strict comparison stays open, an unrefuted selected face carries `certified = FALSE` and `unique = NA`. A successful local search can supply an uncertified result when the cover exhausts its allowance. [Skellam mle.hpp:138](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp#L138)

The local Newton polish checks the joint interior candidate's coefficient accuracy. The likelihood-gap cover admits a competing maximum inside its gap, whether the face itself is feasible or excluded. Joint interior estimates therefore carry `certified = FALSE` and `unique = NA` even when the old selection rule picked the interior. The dossier and the derived capability rows grade overall Skellam certification as partial. [Skellam mle.hpp:233](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp#L233) · [Skellam README.md:59](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md#L59) · [capabilities.csv:2572](../../validation/capabilities.csv#L2572)

With one positive rate held, strict concavity makes the inward sign decisive. The old likelihood-margin override could still select the face after a positive score. The fit now takes the bracketed and polished interior root on that branch. A face whose sign cannot be separated from zero can retain an uncertified estimate, with uniqueness known from strict concavity. The exact constant-data score-zero case remains certified. [Skellam mle.hpp:34](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp#L34)

| Sample | Inward score per observation | Evidence and result |
| --- | ---: | --- |
| `(1, 2)` | $-0.375$ | The strict cover closes. The unique certified face is $(1.5,0)$. |
| `(0, 3, 3)` | $0$ exactly | The fit retains $(2,0)$ with unknown uniqueness and `certified = FALSE`. |
| 17 zeros and 161 fours | $2.52493\times10^{-5}$ | The positive score refutes the face. The interior smaller rate is about $5.90583\times10^{-5}$ and gains $7.45557\times10^{-10}$ per observation, inside the old face preference. |
| 15 zeros and 142 fours | $8.92531\times10^{-5}$ | The interior smaller rate is about $2.08775\times10^{-4}$ and gains $9.31536\times10^{-9}$ per observation. The old logic selected the interior even though the face still lay within the $10^{-8}$ total per-observation gap. The new fit returns an uncertified interior estimate. |
| 19 zeros and one 20 | $-1/21$ | The feasible rates $(1.5,0.5)$ gain $0.0184644$ per observation. The negative face score establishes local decrease, and the feasible interior point refutes global optimality of the face. |
| Two zeros, with $\mu_1=1.00005$ held | $5.00000\times10^{-5}$ | Strict concavity and the positive score select the unique certified interior root, $\hat\mu_2\approx9.99967\times10^{-5}$. It gains $2.49986\times10^{-9}$ per observation, inside the former held-route margin. |

The exact score formulas give the sign arguments. The gains and small stationary rates use an independent 80-digit positive-series likelihood calculation with mpmath. C++ tests exercise the near-face cases in both arithmetic modes and reflect the one-sided samples. R verifies the public certification and uniqueness attributes. [Reference script](2026-11-24-label-fixes-data/skellam-reference.py) · [Reference values](2026-11-24-label-fixes-data/skellam-reference.json) · [Skellam test.cpp:169](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/test.cpp#L169) · [Skellam test.R:187](../../../bindings/r/distributions/skellam/test.R#L187)

## 2. Skew-normal

Both half-normal detectors use a $10^{-8}n$ comparison with the finite candidate and have a separate route for a difference at the comparison threshold. Both dossier entries now set `numerical: true` and describe that comparison. Generated `univariateML-r/man/mlsn.Rd` lines 41 and 43 say that the numerical search does not establish a finite maximum above its tolerance. The positive route links to `mlhnorm()`. [Skew-normal README.md:45](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md#L45) · [Skew-normal mle.hpp:490](../../../univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp#L490)

## 3. Fatigue

An overlapping numerical comparison on a nearby sample can return one uncertified estimate with unknown uniqueness. The two roots are comparison diagnostics. The dossier now describes that result. For exactly constant data and held $\alpha>2$, the profiled density in $w=\sqrt{\beta/y}+\sqrt{y/\beta}$ is proportional to $w\exp(-w^2/(2\alpha^2))$. Its unique maximum $w=\alpha$ has two distinct inverse images. The typed pair, after both public coefficient enclosures pass, now reports `certified = TRUE` and `unique = FALSE`. The existing exact-pair test now asserts the corrected flag. The separate near-constant test still verifies the uncertified estimate and its two diagnostic candidates. An R test checks the flag on the public fit. [Fatigue README.md:121](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md#L121) · [Fatigue mle.hpp:57](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/mle.hpp#L57) · [Fatigue mle.hpp:172](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/mle.hpp#L172) · [Fatigue test.cpp:170](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/test.cpp#L170)

## 4. Lomax, inverse Pareto and Pareto II

The Lomax dossier now states the held-$\kappa$ acceptance conditions from the solver. The log-rate bracket has opposite verified signs and width at most `tol/2`. Conversion to the represented rate must change the logarithm by at most `tol/4`. Inverse Pareto and Pareto II had no stale $10^{-3}$ threshold. Their dossiers now identify the delegated acceptance check and its coordinates. [Lomax README.md:130](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md#L130) · [Lomax mle.hpp:273](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/mle.hpp#L273) · [Lomax mle.hpp:305](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/mle.hpp#L305) · [Inverse Pareto README.md:121](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/README.md#L121) · [Pareto II README.md:152](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md#L152)

The exact constant-sample Lomax path was also under-labelled. For $z=\lambda x>0$, its profiled loss per observation relative to the exponential is $\log((1+z)\log(1+z)/z)>0$, since $\log(1+z)>z/(1+z)$. This proves the escape. The direct route certifies it, and the reciprocal and translated delegates propagate the proof only when the original sample is constant. The margin-based routes retain their uncertified limit flag. R tests exercise all three constant-data conditions and the nonconstant Lomax comparison. [Lomax certificate.hpp:153](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/certificate.hpp#L153) · [Inverse Pareto mle.hpp:33](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/mle.hpp#L33) · [Pareto II mle.hpp:117](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/mle.hpp#L117)

## 5. Remaining inventory rows

The missing numerical-detector flag, the global claim based on the Skellam likelihood gap, the proved Fatigue set, and the three proved constant-sample limit rows are addressed above. The half-t, transformed-gamma, skew-normal and nonconstant Lomax-family limit rows retain their labels because their bounds allow positive gain over the limit. The local-tolerance rows concern evidence independent of global certification. The other default-uniqueness rows were inferred from the default flag and the dossier's family-wide uniqueness field. Establishing a per-input flag error in those routes would require evidence for the individual return guard and hold pattern. [Family notes CSV](2026-11-24-claims-inventory-data/family-notes.csv)

## Snapshot changes and checks

| Mode | Cases | Changed | Added or deleted |
| --- | ---: | ---: | ---: |
| Native | 12,081 | 25 | 0 |
| Double only | 12,081 | 25 | 0 |
| Modifier routes, native | 2,006 | 0 | 0 |
| Modifier routes, double only | 2,006 | 0 | 0 |

The same records moved in both base modes.

- **Skellam, 16 joint interior estimates.** Four archive cases, the golden, four class cases and seven resamples changed only `certified`, `unique`, and the corresponding shared outcome fields. Their coefficients, reported likelihoods and iterations are identical to the baseline.
- **Skellam, `huge`.** The cover exceeded its allowance. The new fit returns the unrefuted Poisson face instead of a budget stop. Its exact inward score per observation is about $-4.66\times10^{-10}$ and the joint global comparison stays open. The returned face carries `certified = FALSE` and `unique = NA`.
- **Skellam, `wide`.** The cover exceeded its allowance, then the interior candidate failed local polish. The stored baseline witness has both rates near $9.37\times10^{10}$, which puts the smallest conditional-count mode near $9.37\times10^{10}$ as well. The differentiable series rejects such a mode at the default enumeration cap of $1.5\times10^7$. The record moves from a budget stop to numerical nonconvergence and returns no estimate. [Skellam accuracy.hpp:5](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/accuracy.hpp#L5)
- **Fatigue, `constant-flat-reciprocal-scales`.** The typed pair and its representative stay the same, with the certified flag corrected.
- **Lomax, inverse Pareto and Pareto II, `one` and `constant`.** Each of the six limit records gains certification. The renderer drops its generic margin sentence. The limit models, coefficients and likelihoods stay the same.

`build/scratch/diff_baseline.py` compared each mode with the stored baseline. Every changed field is recorded in [snapshot-moves.json](2026-11-24-label-fixes-data/snapshot-moves.json). The [comparison script](2026-11-24-label-fixes-data/snapshot-moves.py) reads the stored files and the fresh merged snapshots in `build/snapshots`. [Run provenance](2026-11-24-label-fixes-data/snapshot-provenance.json) records the compiler and source digest for each fresh mode.

| Workstation check | Result |
| --- | --- |
| `just test-changed --no-long-double`, GCC 15.2 | 20 families selected. Native 29/29 and double-only 29/29 CTest shards passed. R dev reported 5,496 passes, 38 skips, zero warnings and zero failures. |
| JavaScript steps in `test-changed`, Emscripten and Node | Build succeeded and 317/317 tests passed. |
| `just site-check` | Passed, including 86 catalogue pages and 86 compiled C++ examples. |
| `fit-density.py`, all fresh modes | Zero new findings. |
| `fit-consistency.py`, all fresh modes | Zero new findings. |

An initial native pass reached the prior Fatigue assertion on the certified flag. After updating that assertion for the exact pair, the complete targeted recipe passed in both configurations. No existing stop assertion changed.

## Open

- **Joint Skellam interior certification.** The current cover bounds likelihood gain and the polish checks local accuracy. To certify an interior winner, the fit still needs an isolated stationary-root enclosure and strict exclusion of competing regions against an attainable likelihood lower bound. The estimate is available with its global and uniqueness claims withdrawn.
- **Stationary and numerically unresolved faces.** The proof needs a stronger near-zero curvature bound when the inward score is zero, and tighter comparisons when the practical rounding allowances overlap. The stationary sample above exercises the fallback.
- **Very large interior Skellam rates.** The `wide` case needs a stable differentiable likelihood or an equivalent coefficient accuracy enclosure that avoids enumeration near the paired-count mode. Density and special-function changes were outside this job.
