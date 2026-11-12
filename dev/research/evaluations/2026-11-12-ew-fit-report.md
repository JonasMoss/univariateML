# Exponentiated Weibull fitter (TASK-180)

Implemented on `codex/ew-fit`, from baseline `af824a98d78f2f2325c7a675c20c84c45e6bb826`. No commit was made. All required validation commands completed successfully on the workstation. Simbox returned exit 75 for all four attempted suites; each command then ran through the local four-slot fallback.

## Delivered

- [C++ fitter](../../../univariateML-cpp/include/cppunivariate/distributions/expweibull/mle.hpp) for the free model and all seven nonempty hold subsets, with structured nonexistence, limit, numerical and accuracy outcomes.
- [Common binding](../../../bindings/common/distributions/expweibull/fit.hpp), [R adapter](../../../bindings/r/distributions/expweibull/adapter.cpp), generated R wrapper and documentation, and generated JavaScript registration, types and modifier metadata.
- [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/expweibull/README.md), appended [C++ tests](../../../univariateML-cpp/include/cppunivariate/distributions/expweibull/test.cpp), [R tests](../../../bindings/r/distributions/expweibull/test.R), and the new [golden sample](../../../univariateML-cpp/include/cppunivariate/distributions/expweibull/golden.json).
- Interior inference classification in the dossier and regenerated canonical availability inventory: regular with full-rank information, with numerical inference explicitly unavailable pending a validated adapter. `just gen` also regenerated the capability and modifier inventories.
- A frozen held-subset census and archive-style face stress set, with native and double-width replays and independent high-precision comparisons.

Existing families' default fits and golden files, probability kernels, special-function code, truth tables and code generators were unchanged. The shared regression changes add the new family's tolerance and raise the frozen family count to 101. Protected status, news and roadmap files were unchanged.

## Method and guarantees

For positive finite observations, write `y = log(x)` and `t = exp(b * (y - log(a)))`. When exponent is free, the fitter profiles it exactly as `c = -n / sum(log(1 - exp(-t)))`. Damped Newton search uses quantile-matched starts in the remaining coordinates. Independent observed-information refinement checks all free log coefficients, including the profiled exponent, for positive information and contracting corrections. Public coefficient projection and the likelihood evaluated at those coefficients are checked before acceptance. Work allowances follow `min(iterlim, 100000)` per solve and per line search.

The [first analysis](2026-11-10-ew-theory-report.md) and [boundary exhaustion analysis](2026-11-11-ew-theory2-report.md) supply the structural proofs:

- With exponent held, strict concavity in `(b, -b log(a))` gives a unique finite maximum for nonconstant data. Single-free fits also have uniqueness results; the exponent's equation is explicit.
- With scale held and both shapes free, observations all strictly below scale have a strictly winning, unattained power envelope. With endpoint ties and at least one observation below scale, each tie subtracts `log(e - 1)` from that envelope. The tied stop therefore identifies `unavailable:power-envelope-endpoint-ties`, preserving its own likelihood instead of delegating to ordinary power likelihood evaluation.
- Constant-data concentration gives proved unboundedness when scale is free with either shape free; when Weibull shape is free at the held scale; or when both shapes are free above the held scale. Other constant-data hold subsets follow their scalar equations or the power-envelope result.
- Power and inverse Weibull are reachable faces of the free model. A face separated above a finite candidate refutes that candidate and produces a numerical stop with both witnesses. This comparison alone does not establish the global winning face.

Certificates use the package's conditional floating-point standard. Face comparisons use a practical likelihood allowance of `256 * epsilon(double) * (1+n) * (1+abs(face logLik))`. An inverse-information calculation propagates a practical score allowance of `16 * epsilon(double)` to coefficients. An inconclusive extra allowance withdraws certification while retaining a successfully checked local fit. Free-model fits and the unresolved two-free regimes return `certified = FALSE`.

Held exponent one uses the existing Weibull solver with a public-likelihood restoration check. A new scale enclosure protects the tiny held-shape case: if `D` is the log-data range, the exact fixed-shape log scale lies between the mean log observation and that mean plus `b*D^2/8`, by the weighted-variance bound for the log moment-generating function. The enclosure includes arithmetic and public-projection allowances. Exact constant-data scale roots remain available even at sub-spacing tolerance requests; other unattainable requests use the shared tolerance/conditioning diagnostics.

The R all-held path uses the package's existing generic evaluator, with zero free degrees of freedom. It does not add the adapter's certification attribute. The C++ and common-binding all-held paths do report certification.

## Held-subset census

The [driver](2026-11-12-ew-fit-data/run-study.py), [frozen observations](2026-11-12-ew-fit-data/held-samples.csv), [raw results](2026-11-12-ew-fit-data/held-study.csv) and [summary](2026-11-12-ew-fit-data/summary.json) retain 12 samples and 96 fits. Samples comprise four 80-point quantile samples with generating exponents 0.2, 1, 2.5 and 20, two-point data, strict endpoint and tied-endpoint examples, three constant samples, near-constant data and a wide log-range sample. Held values are scale 2, shape 1.5 and exponent 2.5. Mask bits 1, 2 and 4 hold those coordinates respectively.

Times are workstation milliseconds with one reserved worker, including fitting and checking; they are observations from this run rather than benchmark guarantees.

| Held | Certified Ok | Uncertified Ok | Proved stops | Numerical failures | Median ms | Max ms |
|---|---:|---:|---|---:|---:|---:|
| none | 0 | 5 | 3 unbounded | 4 | 35.747 | 106.588 |
| scale | 0 | 5 | 2 unbounded, 3 power, 2 tied power | 0 | 0.004 | 35.928 |
| shape | 0 | 8 | 3 unbounded | 1 | 0.991 | 17.384 |
| scale, shape | 12 | 0 | — | 0 | 0.234 | 4.366 |
| exponent | 9 | 0 | 3 unbounded | 0 | 0.635 | 9.754 |
| scale, exponent | 11 | 0 | 1 unbounded | 0 | 0.536 | 9.070 |
| shape, exponent | 12 | 0 | — | 0 | 0.174 | 3.443 |
| all | 12 | 0 | — | 0 | 0.027 | 0.428 |

The five numerical failures are free fits of `two_point`, `below_endpoint`, `endpoint_ties` and `wide`, plus the held-shape `near_constant` fit. A [replay at iterlim 1000](2026-11-12-ew-fit-data/held-study-budget1000.csv) retained exactly those statuses. Its free-fit median/max times were 4,892/16,041 ms. These are search or local-accuracy failures, with no claim of proved nonexistence.

## Frozen face stress set and numerical evidence

The [archive inputs](2026-11-12-ew-fit-data/archive-samples.csv) contain 41 samples: the census, 15 seeded random samples crossing shapes 0.3/1.5/8 and exponents 0.05/0.2/1/5/50, nine endpoint-tie samples crossing three log gaps and three tie multiplicities, and five rescaled samples from `1e-100` to `1e100`. Both precision modes replay the same frozen binary64 observations. Input hashes are retained in the summary.

| Precision mode | Fits | Ok | IterationFailure | Accuracy failures | Power | Tied power | Unbounded |
|---|---:|---:|---:|---:|---:|---:|---:|
| native extended intermediates | 328 | 265 | 33 | 0 | 15 | 3 | 12 |
| double-width intermediates | 328 | 263 | 33 | 2 | 15 | 3 | 12 |

The [native checks](2026-11-12-ew-fit-data/archive-check.json) and [double checks](2026-11-12-ew-fit-data/archive-double-check.json) found no stop-classification, hold-preservation or certification-regime violations. Independent 70-digit score equations checked the free coordinates of every accepted case; all-held cases received independent likelihood evaluation. There were no unresolved reference solves among accepted fits.

| Accepted set | Cases checked | Worst relative coefficient error | Worst likelihood error divided by max(1, abs(reference)) |
|---|---:|---:|---:|
| native census | 74 | 3.515e-13 | 2.299e-15 |
| double census | 73 | 3.515e-13 | 8.629e-15 |
| native archive | 265 | 8.628e-13 | 3.879e-14 |
| double archive | 263 | 1.129e-12 | 3.891e-14 |

These comparisons establish agreement with independent local roots. Global face ordering remains open in the regimes identified above. Raw [native references](2026-11-12-ew-fit-data/archive-reference.json) and [double references](2026-11-12-ew-fit-data/archive-double-reference.json) retain individual errors and residuals.

The two double-width accuracy rejections are genuine at the default `1e-10` coefficient target. A [100-digit replay](2026-11-12-ew-fit-data/double-failed-accuracy.json) measured exponent error `5.076e-10` for the free near-constant case, and scale/exponent errors `9.001e-10`/`2.992e-10` for the free random shape-0.3, exponent-50 case. Conversely, the [native replay](2026-11-12-ew-fit-data/failed-accuracy.json) verified those native candidates to `1.179e-13` and `8.628e-13`; they are retained as uncertified fits when the extra roundoff allowance is inconclusive.

Independent differentiation checks cover the full and profiled objectives. Worst value/gradient/Hessian errors, normalized by max(1, absolute reference), were `7.979e-17 / 9.482e-17 / 9.855e-17` natively and `7.216e-16 / 8.328e-16 / 2.863e-15` with double-width intermediates. The [native](2026-11-12-ew-fit-data/derivative-check.json) and [double](2026-11-12-ew-fit-data/derivative-double-check.json) records retain the comparison points.

## Golden and final validation

`just goldens` created exactly one new sample: 500 observations, seed 13434, generating coefficients `(2, 1.5, 2.5)`. The fitted coefficients are approximately `(2.115606220671031, 1.475752539238937, 2.3365517882968025)`, with log likelihood `-816.4989336738917`. `just golden-reference --write expweibull` found worst relative coefficient discrepancy `2.8e-16`, likelihood discrepancy `+2.7e-14`, score norm `6.81e-59` and negative-definite Hessian at the 50-digit local reference. `just quality expweibull` passed.

[Retained suite results and machine records](2026-11-12-ew-fit-data/final-checks/results.json) link the complete logs. Every local command below exited zero, through the required CPU-slot wrappers and worktree temporary directory.

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | workstation, simbox fallback | 108/108 dev shards passed |
| `just test` | workstation, simbox fallback | FAIL 0, WARN 0, SKIP 156, PASS 29261 |
| `just test-full` | workstation, simbox fallback | FAIL 0, WARN 0, SKIP 0, PASS 58380 |
| `just cpp-test-no-long-double` | GCC 15 workstation, simbox fallback | 135/135 full shards passed |
| `just js-build` | workstation | passed |
| `just js-test` | workstation | 628/628 tests; 390718 truth checks, zero failures |
| `just site-check` | workstation | passed, including 101 families and 357 inference rows |

After the late family-only tiny-shape fix, targeted native and double-width C++ checks passed 22 cases each, with 18,211 and 18,209 assertions respectively, plus common-binding payload checks in both modes. The full R and double-width C++ runs include that fix; the family contributed 78 passing full-tier R assertions. JavaScript explicitly passed the new family's shared golden, all held subsets and held-value diagnostic. The earlier complete native dev run was followed by the targeted family rerun, as specified for a late family-only fix. A final test-only range-loop copy cleanup was followed by the same successful targeted checks. [Targeted log](2026-11-12-ew-fit-data/final-checks/target.log).

## Open items

1. **Global guarantees:** free-model interior/face ordering and uniqueness for the other two-free hold regimes remain unproved. A practical exhaustive certificate would need to close those comparisons or enumerate all relevant stationary points. Current successful local fits expose `certified = FALSE` there.
2. **Stress-search coverage:** the archive retains 33 `IterationFailure` outcomes in both modes, individually listed in its check JSONs, and the two measured double-width accuracy failures above. Numerical outcomes cover search/verification failures and finite candidates refuted by a reachable face. Five census failures persisted after a tenfold iteration allowance. Resolving them needs a successful locally accurate search or a proved global face/nonexistence comparison; their current numerical stops make neither existence nor nonexistence claims.
3. **Parent Weibull issue:** in double-width mode, fixed shape equal to `double::denorm_min()` and data `{0.5,1,2,4}` make the unchanged parent Weibull solver return scale `1.4715177646857691` with Ok status, whereas the exact optimum rounds to approximately `sqrt(2) = 1.4142135623730951`. The new fitter's enclosure returns the correct scale in both modes. The kept [reproducer](2026-11-12-ew-fit-data/subnormal-scale.py) and [results](2026-11-12-ew-fit-data/subnormal-scale.json) isolate this parent-family issue for separate work.
4. **Unavailable validation:** GCC 13 on simbox could not be checked because every connection attempt returned 75. All corresponding commands passed locally, including the mandatory GCC 15 precision run.
5. **Repository-wide Markdown links:** the optional checker reported 47 findings, all in eight files byte-identical to baseline; none came from this job. The [baseline comparison](2026-11-12-ew-fit-data/link-baseline-check.json) retains every finding and source hash. These include missing local assets and checker false positives. Required site validation passed.
6. **Artifact display:** the prescribed `bin/show` command rejected this report with HTTP 403: this worktree is outside the viewer's registered readable roots. The report and its evidence remain in this worktree. Display requires the viewer to recognize this worktree as an active readable root; no viewer access settings or other checkout were changed.

## Reproduction

From this worktree, with the existing development dependencies:

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-ew-fit-data/run-study.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-ew-fit-data/summarize-json.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-ew-fit-data/target-check.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-ew-fit-data/subnormal-scale.py
python3 dev/research/evaluations/2026-11-12-ew-fit-data/validate.py
```

The validation driver reserves slots separately for each computing command and performs the simbox-to-workstation fallback. It runs outside a slot reservation itself. The study driver replays retained observations when present, preserving the exact cross-precision inputs.
