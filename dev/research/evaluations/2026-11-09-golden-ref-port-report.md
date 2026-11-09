# Golden references port (TASK-149.1)

2026-11-09. Worktree `golden-ref-port`, branch `codex/golden-ref-port`, master base `a99ca0dc`. No commits, board edits, fitter changes, default changes, or changes to frozen inputs.

Ported the independent-reference machinery, 98 canonical reference blocks, C++ golden comparisons, R and JS regression policies, and quality-harness support. Bhatt’s canonical fixture and C++ tests/tolerance definitions are unchanged, as requested for the concurrent bhatt-allfree job. All local final development and full tiers completed successfully. The required simbox no-long-double run is open because that machine’s filesystem became read-only; its earlier double-intermediate fit capture completed successfully.

## References and frozen fields

All 98 references were recomputed or re-verified from the current README density at 60 working digits with a 50-digit Newton step target. The 98 input digests agree with the imported independent references. Frozen field values and their original JSON value text are byte-identical to master for every field except the intentionally replaced `estimates`, `logLik`, and added `reference`. Bhatt’s entire canonical file is byte-identical to master. No new canonical families or changed frozen inputs were found.

The port fixes an input-digest instability: writing a reference can change a held estimate’s representation from integer `0` to floating `0.0`. Held values now come from authoritative frozen `fit_args`. Ten affected families were rechecked after this fix; their original digests were recovered. A write-and-verify regression test covers this case. See [reference-checks.json](2026-11-09-golden-ref-port-data/reference-checks.json).

**N** is damped Newton on symbolic derivatives of the README log density. A reference requires a total score norm below `1e-45` and negative-definite observed log-likelihood Hessian; singular, nonconverged, infeasible and unchecked candidates fail without being written. **L** is the Laplace median interval and mean absolute deviation, with the upper median as representative. **S** uses support monotonicity and sample extremes. **M** uses the lower integer median for discrete Laplace. **E** exhausts a finite hyper/nhyper design. **P** independently profiles binomial probabilities and enumerates integer size below an analytically excluded Poisson-envelope tail. Boundary-active coefficients require a correctly signed one-sided score. Held binary64 inputs are interpreted exactly.

These are different kinds of evidence. Newton score/curvature checks establish local numerical evidence. The imported scoped dossier statements and multistart records are retained when their inputs agree; all 48 dossier records match the current parsed statements and reasons (see [dossier-evidence-checks.json](2026-11-09-golden-ref-port-data/dossier-evidence-checks.json)). This port does not independently re-prove those statements. The fresh all-family quality run also passed. Multistart agreement is numerical evidence, not a global optimality proof. Integer-tail comparisons are certified in exact arithmetic conditional on their computed high-precision comparisons, without an outward-rounded interval claim. Genbeta retains the high-precision rescoring that resolves its apparent binary64 oracle improvement; the current quality-oracle candidate is rescored again in the final audit. Integer winner/tail gaps are checked against a practical margin of `1e-40 * (1 + abs(logLik))` at 50-digit reference precision. See [final-audit.json](2026-11-09-golden-ref-port-data/final-audit.json). Triangular `c` remains explicitly unrefined at a kink; the regular-slice reference and multistart comparison do not prove that coordinate globally.

## Measurements and allowances

Every canonical family was replayed in native GCC 15.2, workstation double-intermediate mode, simbox double-intermediate mode (GCC 13), workstation R (GCC 15.2), and WASM. The double modes define `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`. Frozen `fit_args` are replayed, including held coefficients and required designs; the remaining controls use their defaults. R captures each double as a 17-digit string. Measurements include every coefficient and log likelihood: relative error against a nonzero reference, absolute error at zero, and zero for equal signed infinities. Calibration takes the maximum of error against the rounded binary64 fixture and against the stored 25-digit reference, over all five modes.

Allowances round upward on the decimal 1–2–3–5 lattice, with at most three times the measured error and no blanket floor. Missing families, missing modes, nonfinite errors and failed fits stop the audit. These are empirical allowances for these frozen samples and platforms, not universal accuracy bounds. For the 98 calibrated families, C++/JS golden comparisons remove the additive unit scale; R adds a componentwise assertion. Bhatt retains its pre-port JS per-component widths and additive scale, alongside its unchanged R and C++ allowances. One per-family envelope covers coefficients and log likelihood; independent density/score assertions retain their own tighter checks. Non-default/held checks retain separately scoped allowances, including CMP’s held-lambda solve and the explicit-tolerance replicated Lomax test.

73 of 98 calibrated families measure at or below `1e-10`; 25 remain worse. Relative to the earlier rollout, 6 allowances tighten, 90 are unchanged, and two widen. Complete component errors and every mode are retained in [audit.json](2026-11-09-golden-ref-port-data/audit.json); old measurements are retained in [previous-measurements.json](2026-11-09-golden-ref-port-data/previous-measurements.json).

The two increases relative to the old rollout are SSTD (`5e-6` to `2e-5`) and GB2 (`5e-8` to `2e-7`). SSTD’s largest error is the current native/R all-free finite-difference BFGS candidate under unchanged `gradtol=1.220703e-4`. GB2’s largest error is the workstation double-intermediate all-free profile result under unchanged `gradtol=1e-7`; native error remains about `2.38e-13`. Their stopping rules do not bound every coefficient by the control value. The mode-specific differences are numerical observations; this port does not attribute them to a proved single arithmetic operation.

GED improves from about `2.25e-6` to `2.58e-11`, beta-prime from `1.45e-7` to `8.35e-12`, and Tukey lambda from `9.92e-8` to `7.30e-15`; these leave the old worse-than-`1e-10` table. EMG improves from `1.71e-7` to `4.07e-9`; Fréchet and Huber also tighten. SSTD and SGED’s all-free routes still use the legacy finite-difference controls; newer held-subset certificates must not be confused with a tighter all-free coefficient guarantee.

## Families still worse than 1e-10

| Family | Old measured error | Current error | Worst mode / component | Allowance | Cause |
|---|---:|---:|---|---:|---|
| slash | 1.52e-05 | 1.52e-05 | wasm / mu | 2e-05 | scalar profile width reltol=1e-8 and likelihood rounding; near-zero location amplifies relative error |
| sstd | 4.99e-06 | 1.07e-05 | r-native / mean | 2e-05 | finite-difference BFGS gradtol=1.220703e-4 inherited from CRAN; curvature amplifies coefficient error |
| sged | 2.28e-06 | 2.08e-06 | wasm / mean | 3e-06 | finite-difference BFGS gradtol=1.220703e-4 inherited from CRAN; curvature amplifies coefficient error |
| st | 1.31e-06 | 1.03e-06 | wasm / xi | 2e-06 | all-free BFGS score stop at reltol=1e-6; global/face certificate does not impose 1e-10 coefficient accuracy |
| sn | 4.32e-07 | 4.32e-07 | wasm / xi | 5e-07 | scalar profile refinement reltol=1e-9; likelihood rounding limits coefficient resolution |
| zmnbinom | 3.79e-07 | 3.79e-07 | wasm / size | 5e-07 | delegated positive negative-binomial profile reltol=1e-10 |
| bnbinom | 3.08e-07 | 3.08e-07 | double / beta | 5e-07 | finite-difference BFGS gradtol=1e-8 and profile stopping |
| ztnbinom | 2.71e-07 | 2.71e-07 | wasm / size | 3e-07 | positive negative-binomial profile reltol=1e-10 |
| gb2 | 3.11e-08 | 1.97e-07 | double / scale | 2e-07 | all-free beta-shape profile score stop at gradtol=1e-7; profile curvature amplifies coefficient error |
| ht | 7.9e-08 | 7.9e-08 | wasm / nu | 1e-07 | profile objective/scale stops; small likelihood error does not bound nu error |
| lomax | 7.86e-08 | 7.86e-08 | wasm / lambda | 1e-07 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| skellam | 3.96e-08 | 3.96e-08 | wasm / mu2 | 5e-08 | scalar profile refinement reltol=1e-9; likelihood rounding limits coefficient resolution |
| zinb | 3.79e-08 | 3.79e-08 | wasm / size | 5e-08 | negative-binomial scalar profile width reltol=1e-10 and likelihood rounding |
| sgomp | 2.88e-08 | 2.88e-08 | wasm / eta | 3e-08 | scalar profile reltol=1e-10; curvature amplifies coefficient error |
| dgamma | 2.17e-08 | 2.17e-08 | wasm / shape | 3e-08 | profile simplex width reltol=1e-8; likelihood rounding limits coefficient resolution |
| pareto3 | 1.2e-08 | 1.2e-08 | wasm / scale | 2e-08 | delegated damped logistic Newton step reltol=1e-10; likelihood rounding can shrink the accepted step |
| kumar | 4.96e-09 | 4.96e-09 | wasm / b | 5e-09 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| emg | 1.71e-07 | 4.07e-09 | r-native / mu | 5e-09 | certified profile with default reltol=1e-8; coefficient resolution and floating-point comparisons |
| genbeta | 3.85e-09 | 3.85e-09 | r-native / shape1 | 5e-09 | profile score stop gradtol=1e-7; shallow profile amplifies coefficient error |
| fatigue | 3.14e-09 | 3.14e-09 | r-native / beta | 5e-09 | absolute parameter-step abstol=1.220703e-4 inherited from CRAN |
| weibull | 2.96e-09 | 2.96e-09 | wasm / shape | 3e-09 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| foldnorm | 2.74e-09 | 2.74e-09 | wasm / sigma | 3e-09 | scalar profile refinement reltol=1e-10; likelihood rounding limits coefficient resolution |
| cmb | 2.96e-10 | 2.96e-10 | wasm / nu | 3e-10 | Newton moment-score stop at reltol=1e-10; inverse curvature amplifies coefficient error |
| gumbel | 2.33e-10 | 2.33e-10 | wasm / sigma | 3e-10 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| pig | 1.89e-10 | 1.89e-10 | wasm / shape | 2e-10 | scalar profile reltol=1e-9; curvature amplifies shape error |

Bhatt is also above `1e-10`: maximum error 2.01e-07 against the earlier independent reference. Its reference and allowance remain reserved for bhatt-allfree, rather than being recalibrated here. The five measurements and the unchanged-file check are retained in [bhatt-deferred.json](2026-11-09-golden-ref-port-data/bhatt-deferred.json).

## Changes from master’s stored-output tolerances

The old master assertions compared with the fitter’s earlier output. Switching to an independent reference can require a wider allowance even when the fitter is unchanged. The following table identifies every R allowance wider than master’s old threshold, its measured error, and its cause. C++ default golden checks use the same calibrated allowance; unrelated identity and historical checks preserve their own intent.

| Family | Master R allowance | New allowance | Current error | Cause |
|---|---:|---:|---:|---|
| cauchy | 1e-12 | 3e-12 | 2.31e-12 | default absolute-step solve (abstol=1.220703e-4) |
| cmb | 1e-12 | 3e-10 | 2.96e-10 | Newton moment-score stop at reltol=1e-10; inverse curvature amplifies coefficient error |
| fatigue | 1e-12 | 5e-09 | 3.14e-09 | absolute parameter-step abstol=1.220703e-4 inherited from CRAN |
| foldnorm | 1e-12 | 3e-09 | 2.74e-09 | scalar profile refinement reltol=1e-10; likelihood rounding limits coefficient resolution |
| fpareto | 1e-12 | 1e-11 | 5.75e-12 | conditional profile solve (reltol=1e-7) |
| gb2 | 5e-10 | 2e-07 | 1.97e-07 | all-free beta-shape profile score stop at gradtol=1e-7; profile curvature amplifies coefficient error |
| genbeta | 1e-12 | 5e-09 | 3.85e-09 | profile score stop gradtol=1e-7; shallow profile amplifies coefficient error |
| gumbel | 1e-12 | 3e-10 | 2.33e-10 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| invpareto | 1e-12 | 1e-10 | 5.18e-11 | default Newton/profile solve (reltol=1.220703e-4) |
| kumar | 1e-12 | 5e-09 | 4.96e-09 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| lomax | 1e-12 | 1e-07 | 7.86e-08 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| nbinom | 1e-12 | 1e-10 | 5.51e-11 | default hybrid Newton size step (reltol=1.220703e-4) |
| pareto3 | 1e-12 | 2e-08 | 1.2e-08 | delegated damped logistic Newton step reltol=1e-10; likelihood rounding can shrink the accepted step |
| sgomp | 1e-12 | 3e-08 | 2.88e-08 | scalar profile reltol=1e-10; curvature amplifies coefficient error |
| skellam | 1e-12 | 5e-08 | 3.96e-08 | scalar profile refinement reltol=1e-9; likelihood rounding limits coefficient resolution |
| slash | 1e-12 | 2e-05 | 1.52e-05 | scalar profile width reltol=1e-8 and likelihood rounding; near-zero location amplifies relative error |
| sn | 1e-12 | 5e-07 | 4.32e-07 | scalar profile refinement reltol=1e-9; likelihood rounding limits coefficient resolution |
| st | 5e-07 | 2e-06 | 1.03e-06 | all-free BFGS score stop at reltol=1e-6; global/face certificate does not impose 1e-10 coefficient accuracy |
| weibull | 1e-12 | 3e-09 | 2.96e-09 | Newton relative-step reltol=1.220703e-4 inherited from CRAN |
| yulesimon | 1e-12 | 5e-11 | 4.53e-11 | log-shape bracket width (reltol=1e-10) |
| zeta | 1e-12 | 1e-11 | 5.16e-12 | score/bracket stopping (reltol=1e-10) |
| zinb | 4e-09 | 5e-08 | 3.79e-08 | negative-binomial scalar profile width reltol=1e-10 and likelihood rounding |
| ztnbinom | 1e-12 | 3e-07 | 2.71e-07 | positive negative-binomial profile reltol=1e-10 |

## Family ledger

“Master error” is the old stored fixture’s error against the independent reference; it is not the current fitter’s error. Evidence is scoped as above. The JSON ledger retains the exact method, refined and unrefined coordinates, global record, all five modes, and component errors.

| Family | Method | Global evidence | Master error | Old measured error | Current error | Old allowance | New allowance |
|---|---|---|---:|---:|---:|---:|---:|
| [bbinom](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md) | N | 8 starts; gap 2.3e-13 | 4.59e-16 | 8.05e-16 | 8.05e-16 | 1e-15 | 1e-15 |
| [beta](../../../univariateML-cpp/include/cppunivariate/distributions/beta/README.md) | N | dossier | 2.5e-15 | 4.42e-15 | 4.42e-15 | 5e-15 | 5e-15 |
| [betapr](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md) | N | 8 starts; gap 5.1e-13 | 7.74e-10 | 1.45e-07 | 8.35e-12 | 2e-07 | 1e-11 |
| [binom](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md) | P+N | 8 starts; gap 2.3e-13 | 1.33e-15 | 1.56e-16 | 1.56e-16 | 2e-16 | 2e-16 |
| [bnbinom](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md) | N | 8 starts; gap 2.3e-12 | 8.6e-08 | 3.08e-07 | 3.08e-07 | 5e-07 | 5e-07 |
| [burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md) | N | 8 starts; gap 2.3e-13 | 1.84e-07 | 1.98e-14 | 1.98e-14 | 2e-14 | 2e-14 |
| [cauchy](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md) | N | dossier | 2.31e-12 | 2.31e-12 | 2.31e-12 | 3e-12 | 3e-12 |
| [cmb](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) | N | 8 starts; gap 9.1e-13 | 2.96e-10 | 2.96e-10 | 2.96e-10 | 3e-10 | 3e-10 |
| [cmp](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md) | N | dossier | 1.82e-16 | 9.35e-16 | 9.35e-16 | 1e-15 | 1e-15 |
| [dgamma](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md) | N | 8 starts; gap 1e-12 | 2.14e-09 | 2.17e-08 | 2.17e-08 | 3e-08 | 3e-08 |
| [dlaplace](../../../univariateML-cpp/include/cppunivariate/distributions/dlaplace/README.md) | M+N | dossier | 1.36e-16 | 1.36e-16 | 1.36e-16 | 2e-16 | 2e-16 |
| [dnorm](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md) | N | dossier | 6.98e-13 | 6.98e-13 | 6.98e-13 | 1e-12 | 1e-12 |
| [dunif](../../../univariateML-cpp/include/cppunivariate/distributions/dunif/README.md) | S | dossier | 2.07e-16 | 2.07e-16 | 2.07e-16 | 3e-16 | 3e-16 |
| [dweibull](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md) | N | dossier | 1.61e-16 | 1.61e-16 | 1.61e-16 | 2e-16 | 2e-16 |
| [emg](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md) | N | 8 starts; gap 1.1e-13 | 7.94e-08 | 1.71e-07 | 4.07e-09 | 2e-07 | 5e-09 |
| [exp](../../../univariateML-cpp/include/cppunivariate/distributions/exp/README.md) | N | dossier | 4.36e-16 | 4.36e-16 | 4.36e-16 | 5e-16 | 5e-16 |
| [fatigue](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md) | N | 8 starts; gap 2.3e-13 | 3.14e-09 | 3.14e-09 | 3.14e-09 | 5e-09 | 5e-09 |
| [foldnorm](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/README.md) | N | 8 starts; gap 0 | 2.74e-09 | 2.74e-09 | 2.74e-09 | 3e-09 | 3e-09 |
| [fpareto](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md) | N | 8 starts; gap 1.6e-12 | 5.75e-12 | 5.75e-12 | 5.75e-12 | 1e-11 | 1e-11 |
| [frechet](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) | N | 8 starts; gap 0 | 1.33e-13 | 1.33e-13 | 1.9e-14 | 2e-13 | 2e-14 |
| [gamma](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/README.md) | N | dossier | 6.1e-16 | 6.1e-16 | 6.1e-16 | 1e-15 | 1e-15 |
| [gb2](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md) | N | 8 starts; gap 2.3e-13 | 2.38e-13 | 3.11e-08 | 1.97e-07 | 5e-08 | 2e-07 |
| [ged](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md) | N | 8 starts; gap 2.3e-13 | 2.25e-06 | 2.25e-06 | 2.58e-11 | 3e-06 | 3e-11 |
| [genbeta](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) | N | 8 starts; gap 8.7e-09 | 3.85e-09 | 3.85e-09 | 3.85e-09 | 5e-09 | 5e-09 |
| [geom](../../../univariateML-cpp/include/cppunivariate/distributions/geom/README.md) | N | dossier | 4.21e-15 | 4.21e-15 | 4.21e-15 | 5e-15 | 5e-15 |
| [gev](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md) | N | 8 starts; gap 1.1e-13 | 1.8e-12 | 3.64e-11 | 6e-14 | 5e-11 | 1e-13 |
| [gompertz](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md) | N | dossier | 1.59e-12 | 3.88e-13 | 3.88e-13 | 5e-13 | 5e-13 |
| [gpd](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) | N | 8 starts; gap 1.1e-13 | 2.08e-13 | 1.36e-15 | 1.36e-15 | 2e-15 | 2e-15 |
| [gumbel](../../../univariateML-cpp/include/cppunivariate/distributions/gumbel/README.md) | N | dossier | 2.33e-10 | 2.33e-10 | 2.33e-10 | 3e-10 | 3e-10 |
| [hcauchy](../../../univariateML-cpp/include/cppunivariate/distributions/hcauchy/README.md) | N | dossier | 4.71e-15 | 4.71e-15 | 4.71e-15 | 5e-15 | 5e-15 |
| [hnorm](../../../univariateML-cpp/include/cppunivariate/distributions/hnorm/README.md) | S+N | dossier | 1.85e-16 | 1.85e-16 | 1.85e-16 | 2e-16 | 2e-16 |
| [ht](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md) | N | 8 starts; gap 2.3e-13 | 7.9e-08 | 7.9e-08 | 7.9e-08 | 1e-07 | 1e-07 |
| [huber](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md) | N | 8 starts; gap 0 | 6.15e-13 | 6.15e-13 | 6.15e-13 | 1e-12 | 1e-12 |
| [hyper](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md) | E | 0 starts; gap 0 | 1.32e-16 | 1.32e-16 | 1.32e-16 | 2e-16 | 2e-16 |
| [invburr](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md) | N | 8 starts; gap 0 | 3.12e-08 | 7.27e-16 | 7.27e-16 | 1e-15 | 1e-15 |
| [invexp](../../../univariateML-cpp/include/cppunivariate/distributions/invexp/README.md) | N | dossier | 7.3e-16 | 7.3e-16 | 7.3e-16 | 1e-15 | 1e-15 |
| [invgamma](../../../univariateML-cpp/include/cppunivariate/distributions/invgamma/README.md) | N | dossier | 1.32e-15 | 1.79e-15 | 1.79e-15 | 2e-15 | 2e-15 |
| [invgauss](../../../univariateML-cpp/include/cppunivariate/distributions/invgauss/README.md) | N | dossier | 3.06e-15 | 1.08e-15 | 1.08e-15 | 2e-15 | 2e-15 |
| [invparalogis](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md) | N | 8 starts; gap 2.3e-13 | 4.91e-07 | 2.19e-15 | 2.19e-15 | 3e-15 | 3e-15 |
| [invpareto](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/README.md) | N | 8 starts; gap 0 | 5.18e-11 | 5.18e-11 | 5.18e-11 | 1e-10 | 1e-10 |
| [invtrgamma](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md) | N | 8 starts; gap 7.4e-13 | 1.95e-11 | 5.81e-14 | 5.81e-14 | 1e-13 | 1e-13 |
| [invweibull](../../../univariateML-cpp/include/cppunivariate/distributions/invweibull/README.md) | N | dossier | 9.52e-15 | 9.52e-15 | 9.52e-15 | 1e-14 | 1e-14 |
| [kumar](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md) | N | dossier | 4.96e-09 | 4.96e-09 | 4.96e-09 | 5e-09 | 5e-09 |
| [laplace](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/README.md) | L | dossier | 8.36e-16 | 8.36e-16 | 8.36e-16 | 1e-15 | 1e-15 |
| [lgamma](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/README.md) | N | dossier | 1.4e-15 | 1.4e-15 | 1.4e-15 | 2e-15 | 2e-15 |
| [lgser](../../../univariateML-cpp/include/cppunivariate/distributions/lgser/README.md) | N | dossier | 4.36e-16 | 4.36e-16 | 4.36e-16 | 5e-16 | 5e-16 |
| [llogis](../../../univariateML-cpp/include/cppunivariate/distributions/llogis/README.md) | N | dossier | 1.91e-13 | 1.91e-13 | 1.91e-13 | 2e-13 | 2e-13 |
| [lnorm](../../../univariateML-cpp/include/cppunivariate/distributions/lnorm/README.md) | N | dossier | 8.05e-16 | 6.04e-15 | 6.04e-15 | 1e-14 | 1e-14 |
| [logis](../../../univariateML-cpp/include/cppunivariate/distributions/logis/README.md) | N | dossier | 4.06e-14 | 4.06e-14 | 4.06e-14 | 5e-14 | 5e-14 |
| [logitnorm](../../../univariateML-cpp/include/cppunivariate/distributions/logitnorm/README.md) | N | dossier | 1.44e-15 | 1.18e-15 | 1.18e-15 | 2e-15 | 2e-15 |
| [lomax](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md) | N | 8 starts; gap 5.7e-14 | 7.86e-08 | 7.86e-08 | 7.86e-08 | 1e-07 | 1e-07 |
| [lst](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) | N | 8 starts; gap 2.3e-13 | 5.34e-09 | 1.67e-13 | 1.67e-13 | 2e-13 | 2e-13 |
| [naka](../../../univariateML-cpp/include/cppunivariate/distributions/naka/README.md) | N | dossier | 1.98e-15 | 1.98e-15 | 1.98e-15 | 2e-15 | 2e-15 |
| [nbinom](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md) | N | dossier | 5.51e-11 | 5.51e-11 | 5.51e-11 | 1e-10 | 1e-10 |
| [nhyper](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md) | E | 0 starts; gap 0 | 1.91e-16 | 1.91e-16 | 1.91e-16 | 2e-16 | 2e-16 |
| [norm](../../../univariateML-cpp/include/cppunivariate/distributions/norm/README.md) | N | dossier | 3.16e-14 | 1.24e-14 | 1.24e-14 | 2e-14 | 2e-14 |
| [nsbeta](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md) | N | 8 starts; gap 4.3e-13 | 7.29e-16 | 1.93e-15 | 1.93e-15 | 2e-15 | 2e-15 |
| [paralogis](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md) | N | 8 starts; gap 0 | 2.64e-07 | 1.92e-15 | 1.92e-15 | 2e-15 | 2e-15 |
| [pareto](../../../univariateML-cpp/include/cppunivariate/distributions/pareto/README.md) | S+N | dossier | 6.67e-16 | 4.95e-16 | 4.95e-16 | 5e-16 | 5e-16 |
| [pareto2](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md) | N | 8 starts; gap 4.5e-13 | 7.71e-16 | 1.15e-15 | 1.15e-15 | 2e-15 | 2e-15 |
| [pareto3](../../../univariateML-cpp/include/cppunivariate/distributions/pareto3/README.md) | N | dossier | 1.2e-08 | 1.2e-08 | 1.2e-08 | 2e-08 | 2e-08 |
| [pareto4](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md) | N | 8 starts; gap 4.5e-13 | 4.56e-15 | 4.56e-15 | 4.56e-15 | 5e-15 | 5e-15 |
| [pig](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md) | N | dossier | 5.13e-08 | 1.89e-10 | 1.89e-10 | 2e-10 | 2e-10 |
| [pois](../../../univariateML-cpp/include/cppunivariate/distributions/pois/README.md) | N | dossier | 2.28e-15 | 2.28e-15 | 2.28e-15 | 3e-15 | 3e-15 |
| [power](../../../univariateML-cpp/include/cppunivariate/distributions/power/README.md) | S+N | dossier | 1.33e-16 | 1.33e-15 | 1.33e-15 | 2e-15 | 2e-15 |
| [rayleigh](../../../univariateML-cpp/include/cppunivariate/distributions/rayleigh/README.md) | N | dossier | 1.12e-16 | 1.12e-16 | 1.12e-16 | 2e-16 | 2e-16 |
| [sged](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | N | 8 starts; gap 2.3e-13 | 3.93e-07 | 2.28e-06 | 2.08e-06 | 3e-06 | 3e-06 |
| [sgomp](../../../univariateML-cpp/include/cppunivariate/distributions/sgomp/README.md) | N | 8 starts; gap 1.1e-13 | 2.88e-08 | 2.88e-08 | 2.88e-08 | 3e-08 | 3e-08 |
| [skellam](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md) | N | 8 starts; gap 9.1e-13 | 3.96e-08 | 3.96e-08 | 3.96e-08 | 5e-08 | 5e-08 |
| [slash](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md) | N | 8 starts; gap 6.1e-12 | 1.52e-05 | 1.52e-05 | 1.52e-05 | 2e-05 | 2e-05 |
| [sn](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md) | N | 8 starts; gap 5.7e-14 | 4.32e-07 | 4.32e-07 | 4.32e-07 | 5e-07 | 5e-07 |
| [snorm](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md) | N | 8 starts; gap 1.1e-13 | 1.05e-15 | 2.33e-13 | 2.44e-13 | 3e-13 | 3e-13 |
| [sstd](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | N | 8 starts; gap 1.1e-13 | 3.56e-06 | 4.99e-06 | 1.07e-05 | 5e-06 | 2e-05 |
| [st](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md) | N | 8 starts; gap 3.4e-13 | 6.05e-09 | 1.31e-06 | 1.03e-06 | 2e-06 | 2e-06 |
| [std](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | N | 8 starts; gap 2.3e-13 | 8.91e-06 | 2.59e-13 | 2.59e-13 | 3e-13 | 3e-13 |
| [tbinom](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md) | N | 8 starts; gap 1.7e-12 | 3.21e-15 | 3.91e-15 | 3.91e-15 | 5e-15 | 5e-15 |
| [tlambda](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md) | N | 8 starts; gap 9.9e-12 | 9.92e-08 | 9.92e-08 | 7.3e-15 | 1e-07 | 1e-14 |
| [tnorm](../../../univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md) | N | dossier | 1.02e-14 | 1.02e-14 | 1.02e-14 | 2e-14 | 2e-14 |
| [tpois](../../../univariateML-cpp/include/cppunivariate/distributions/tpois/README.md) | N | dossier | 2.56e-15 | 2.56e-15 | 2.56e-15 | 3e-15 | 3e-15 |
| [trgamma](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/README.md) | N | 8 starts; gap 1.1e-13 | 6.36e-16 | 2.05e-14 | 2.05e-14 | 3e-14 | 3e-14 |
| [triang](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md) | N | 8 starts; gap 1.4e-14 | 7.06e-13 | 7.06e-13 | 7.06e-13 | 1e-12 | 1e-12 |
| [unif](../../../univariateML-cpp/include/cppunivariate/distributions/unif/README.md) | S | dossier | 5.92e-17 | 5.92e-17 | 5.92e-17 | 1e-16 | 1e-16 |
| [weibull](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md) | N | dossier | 2.96e-09 | 2.96e-09 | 2.96e-09 | 3e-09 | 3e-09 |
| [yulesimon](../../../univariateML-cpp/include/cppunivariate/distributions/yulesimon/README.md) | N | dossier | 4.53e-11 | 4.53e-11 | 4.53e-11 | 5e-11 | 5e-11 |
| [zeta](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/README.md) | N | dossier | 5.16e-12 | 5.16e-12 | 5.16e-12 | 1e-11 | 1e-11 |
| [zib](../../../univariateML-cpp/include/cppunivariate/distributions/zib/README.md) | P+N | 8 starts; gap 1.6e-11 | 3.74e-15 | 3.74e-15 | 3.74e-15 | 5e-15 | 5e-15 |
| [zinb](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md) | N | 8 starts; gap 3.4e-13 | 3.79e-08 | 3.79e-08 | 3.79e-08 | 5e-08 | 5e-08 |
| [zip](../../../univariateML-cpp/include/cppunivariate/distributions/zip/README.md) | N | dossier | 3.36e-15 | 3.36e-15 | 3.36e-15 | 5e-15 | 5e-15 |
| [zipf](../../../univariateML-cpp/include/cppunivariate/distributions/zipf/README.md) | S+N | dossier | 5.18e-14 | 5.18e-14 | 5.18e-14 | 1e-13 | 1e-13 |
| [zmbinom](../../../univariateML-cpp/include/cppunivariate/distributions/zmbinom/README.md) | P+N | 8 starts; gap 1.9e-11 | 4.99e-16 | 4.99e-16 | 4.99e-16 | 5e-16 | 5e-16 |
| [zmgeom](../../../univariateML-cpp/include/cppunivariate/distributions/zmgeom/README.md) | N | dossier | 1.85e-15 | 1.85e-15 | 1.85e-15 | 2e-15 | 2e-15 |
| [zmlogarithmic](../../../univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/README.md) | N | dossier | 3.41e-15 | 4.76e-17 | 4.76e-17 | 5e-17 | 5e-17 |
| [zmnbinom](../../../univariateML-cpp/include/cppunivariate/distributions/zmnbinom/README.md) | N | 8 starts; gap 6.8e-13 | 3.79e-07 | 3.79e-07 | 3.79e-07 | 5e-07 | 5e-07 |
| [zmpois](../../../univariateML-cpp/include/cppunivariate/distributions/zmpois/README.md) | N | dossier | 3.18e-15 | 3.18e-15 | 3.18e-15 | 5e-15 | 5e-15 |
| [ztbinom](../../../univariateML-cpp/include/cppunivariate/distributions/ztbinom/README.md) | P+N | 8 starts; gap 2.6e-11 | 5.34e-16 | 5.34e-16 | 5.34e-16 | 1e-15 | 1e-15 |
| [ztgeom](../../../univariateML-cpp/include/cppunivariate/distributions/ztgeom/README.md) | N | dossier | 8.88e-18 | 8.88e-18 | 8.88e-18 | 1e-17 | 1e-17 |
| [ztnbinom](../../../univariateML-cpp/include/cppunivariate/distributions/ztnbinom/README.md) | N | 8 starts; gap 6.8e-13 | 2.71e-07 | 2.71e-07 | 2.71e-07 | 3e-07 | 3e-07 |
| [ztpois](../../../univariateML-cpp/include/cppunivariate/distributions/ztpois/README.md) | N | dossier | 3.02e-16 | 3.02e-16 | 3.02e-16 | 5e-16 | 5e-16 |
| bhatt | deferred | earlier independent reference retained separately | — | 2.01e-07 | 2.01e-07 | owned by bhatt-allfree | unchanged here |

## Final validation

| Check | Machine | Result |
|---|---|---|
| Independent reference re-verification | workstation | 98/98; all imported input digests match |
| Reference regression tests | workstation | 10/10 |
| Quality density/score/oracle harness | workstation | 99/99; explicit score/Hessian skips retained |
| Default-fit captures | workstation / simbox / WASM | all 99 families in every mode; R also retains one replicated-Lomax check |
| `just cpp-test` | workstation | 100% tests passed, 0 tests failed out of 106 |
| `just test` | workstation | [ FAIL 0 \| WARN 0 \| SKIP 154 \| PASS 22497 ] |
| `just js-build` | workstation | exit 0 |
| `just js-test` | workstation | 575 passes; 0 failures |
| `env UMVL_TEST_TIER=full just cpp-test` | workstation | 100% tests passed, 0 tests failed out of 129 |
| `just test-full` | workstation | [ FAIL 0 \| WARN 0 \| SKIP 0 \| PASS 45332 ] |
| `just cpp-test-no-long-double` | workstation | 100% tests passed, 0 tests failed out of 129 |
| `just cpp-test-no-long-double` | simbox | blocked before compilation: read-only filesystem |
| Frozen JSON value/text checks; `git diff --check` | workstation | 98/98 frozen fixtures; bhatt byte-identical; clean diff |

Commands ran in the foreground under the shared CPU locks, at reduced priority, with at most four local workers and BLAS/OpenMP pinned to one thread. No started command remains running. See [checks.json](2026-11-09-golden-ref-port-data/checks.json) for exit codes and timings. No family README metadata/prose, fitter signatures, core fitter or binding fit implementations, settled d/p/q/r sources, or prohibited status/roadmap files changed.

## Open platform run and retained limitations

**Open: simbox no-long-double suite.** The initial simbox double-intermediate capture succeeded for all 99 families. During the later R build, simbox stopped with an I/O failure; repeated remote execution checks also reported `Input/output error`. A subsequent `nice -n 10 just cpp-test-no-long-double` attempt failed before compilation because the mirrored worktree filesystem was read-only (`os error 30` while creating `build/tmp/just-*`). The workstation no-long-double suite completed successfully. Missing evidence: a green no-long-double suite on simbox after its filesystem is writable again. The failure is retained in [simbox-unavailable.log](2026-11-09-golden-ref-port-data/simbox-unavailable.log). The failed simbox R capture was replaced by the successful workstation R capture, and is not presented as a successful simbox R run.

Bhatt’s reference/tolerance integration belongs to the explicitly excluded concurrent job. Triangular kink/global evidence and numerical multistart limitations remain explicit above. The tolerance increases and remaining default-fit errors are recorded for subsequent fitter work; this port changes no fitting defaults or algorithms.

Reproduction scripts and captures are in [the data directory](2026-11-09-golden-ref-port-data/). Run `verify_references.py`, regenerate the native/double/R/WASM captures (and simbox double capture when available), then `audit.py` and `apply_tolerances.py`; the latter changes only golden comparison allowances. `run_checks.py dev` and `run_checks.py full` run under `/tmp/umvl-cpu/run4`. The frozen-input audit uses read-only `git show HEAD:path` as its baseline.

