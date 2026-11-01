# R covariance for univariateML fits (2026-11-01)

Tested source: `a8f1c3dcbc0c8a8f2d6f2c13292c07eef3b48e12` plus the uncommitted vcov-r worktree changes. Linux x86-64, R 4.6.1, GCC 15.2.0. No C++ inference-layer changes or new package dependencies. `confint` is unchanged.

## Interface and mathematical scope

Fit objects retain the checked observations after `na.rm`, a named free mask in README coefficient order, and the existing held-coefficient names. The adapters mark fitted structural parameters free, so inference does not silently condition on a fitted endpoint or design parameter. Fully held fits with data retain the observations; data-free frozen objects and older objects without retained inference data cannot supply covariance. Existing printing, summary, likelihood, bootstrap and model-selection paths are unchanged; the package suites exercise them.

`vcov(object)` defaults to `type = "sandwich"`; `type = "observed"` selects inverse observed information. The output is in natural README coordinates for the free coefficients only. Attributes give the type, omitted held names, observed-information eigenvalue condition number, and C++ validity conditions. The Rcpp entry point uses `family_inference` followed by `inference_covariance`; the dispatcher selects the analytic normal, gamma, beta and Poisson implementations first. Unavailable families or submodels, boundary/limit fits, restricted fits, invalid or nonfinite inputs and numerical, singular, indefinite and ill-conditioned solves stop with a reason. A successful solve with condition number above 1e6 warns that numerical sensitivity may be substantial. The condition number depends on units and is not a test of inferential validity.

For individual scores `s_i` in the free natural coordinates and total observed information `H = -sum_i d² log f_i / d theta²`, observed covariance is `H^-1` and sandwich covariance is `H^-1 (sum_i s_i s_i^T) H^-1`. There is no additional sample-size factor because both quantities are totals. The sandwich uses per-observation cross-products, not the outer product of the total score. Held coefficients are treated as known, and fitted nuisance coefficients remain in the free matrix. Model-based inference requires correct specification and appropriate regularity. Misspecification-robust inference additionally needs a well-defined pseudo-true parameter, finite score variance, nonsingular sensitivity, independent observations and suitable estimator asymptotics. Neither the matrix nor these checks proves those conditions or finite-sample coverage. The family dossiers' `inference_numeric` entries and [C++ report](2026-11-01-vcov-scores-report.md) record regime-specific scope and evidence. In particular, near-singular skew-normal and Bhattacharjee members and heavy tails can give unreliable Wald uncertainty even when a solve succeeds.

## Validation

All final commands were foreground, one command per `/tmp/umvl-cpu/run4`, with OpenMP/BLAS pinned to one thread and `nice -n 10` supplied by the slot wrapper. Temporary files used `build/tmp` and logs used `build/scratch`. The following passed:

| Command | Result |
|---|---|
| `just cpp-test` | 105/105 dev shards |
| `just test` | 19,945 pass, 0 fail, 0 warn, 151 tier skips |
| `just test-full` | 41,413 pass, 0 fail, 0 warn, 0 skips |
| `just js-build` | pass |
| `just js-test` | 400/400 |
| `just site-check` | pass; existing site dependencies copied before build |
| `Rscript .github/scripts/assemble.R` | pass |
| `Rscript .github/scripts/r-check.R` | 0 errors, 0 warnings, 2 notes |

The first tarball check found an undocumented new internal constructor argument; its roxygen documentation was corrected and assembly and check rerun. The final two notes concern pre-existing relative paths in generated help and skipped optional HTML/math validation because `tidy` and R package `V8` are absent. The dev/full suites preceded this documentation-only correction; no executable source changed afterward.

The targeted R test file passed 47 assertions. For frozen golden data at the fitted coefficients, independent R central differences of individual log densities (step `1e-4 * max(1, |theta|)`) check both matrices at relative tolerance `8e-4`: normal, gamma, beta, Poisson, binomial with known size, normal with mean held, and three-parameter beta prime. Each also compares the R return with the direct C++ entry point at `1e-12`. Independent closed-form observed information checks cover all four analytic pilots, including normal `sd²/n` and `sd²/(2n)`. Tests exercise names, dimensions, held names, unavailable family and subset, boundary, nonfinite observation, indefinite and ill-conditioned information, missing retained data/mask, and a successful but poorly conditioned warning. The independent differences are numerical checks, not proofs; direct entry-point agreement checks the binding, not independence from C++.

## Cost and timings

Reproduce with `just regen` followed by `/tmp/umvl-cpu/run4 Rscript dev/research/evaluations/2026-11-01-vcov-r-measure.R`. This scans the golden data and recorded estimates (normally n=500), applies recorded non-null held fit arguments, and times one default sandwich call per family, including R dispatch; unsuccessful scopes are recorded rather than substituted. It does not time fitting. Wall-clock measurements are single observations at timer resolution and are not benchmarks with uncertainty. Slowest successful calls in this run were CMP 0.513 s, beta-negative-binomial 0.144 s, discretized gamma 0.089 s, Conway–Maxwell binomial 0.037 s, and Bhattacharjee 0.024 s. Normal and Poisson rounded below 0.001 s; gamma took 0.001 s.

At n=500, `object.size` increased by 4,616 bytes for normal and gamma, 4,552 bytes for Poisson, and 4,696 bytes for three-parameter beta-negative-binomial. This includes the observation vector, named mask and attribute overhead. R may share an input vector until it is modified; `object.size` counts reachable storage per object and is not an incremental resident-memory measurement. The main cost scales approximately as eight bytes per retained double observation, plus fixed object and mask overhead.

## Open evidence

The new R tests do not independently establish calibration or coverage under correct or misspecified models, nor do they cover every family, subset, tail or near-singular regime. The C++ report documents its simulation evidence and limitations; an independent R coverage study under explicit hypotheses and misspecification remains open. The R tests exercise ill-conditioned and indefinite rejection; singular rejection is delegated to the C++ matrix diagnostics and existing C++ tests rather than separately constructed in R. Timing is a single workstation sweep, not a repeated cross-platform benchmark. Existing fit objects serialized before this change do not contain the observations and cannot be used for `vcov` without refitting.
