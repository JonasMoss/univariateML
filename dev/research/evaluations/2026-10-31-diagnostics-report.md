# Diagnostics: TASK-132 and TASK-118 (2026-10-31)

Worktree: `diagnostics`, branch `codex/diagnostics`. Baseline: `14598b7db24e109ddfa03bbe952953a102ef0b46`. No commits, pushes, board edits, dependency installs or other-worktree changes were made. Goldens are unchanged.

## Audit

The standalone sweep calls all 98 families over 553 supported held subsets: 5,488 rows (of 618 possible subsets, 65 exact structural exceptions are excluded). Baseline defects: 170; after fixes: 0. A defect is a success with invalid input, a nonfinite likelihood, or a coefficient outside its declared domain. The core results are observed before binding error handling.

Each held coefficient comes from its frozen golden. Workloads are deterministic (no RNG): empty, one observation, four repeats of the first golden observation, all zeros, negative observations, mixed unit endpoints, finite support endpoints and points beyond them, NaN, positive infinity and negative infinity. The double-span count APIs also receive fractional counts. Integer-span APIs cannot represent fractional or nonfinite observations; the R dev test checks rejection before conversion for every applicable family. Closed proper discrete parameter endpoints are accepted. Infinite truncation bounds explicitly allowed by the tnorm dossier are also accepted; the finiteness column still records their infinity.

`diagnostics.py` generates the checked-in standalone C++ source from dossiers, goldens and shared call routing. Its final exclusion filter uses exact free-coordinate exceptions; an initial conformance-style superset filter omitted 15 supported subsets, which were restored before the final runs. Binding guards are removed only in validation routes. Empty-input min/max preparation for tbinom is made safe in that route; the actual core still receives empty data. All coefficient mappings are checked during generation. The CMake dev/full test has a 60-second timeout. No R or Python dependency is added to standalone core consumers.

Sources: [generator](../../validation/diagnostics.py), [C++ sweep](../../validation/diagnostics-audit.cpp), [contract](../../validation/diagnostics.md), [row-level results](2026-10-31-diagnostics-audit.csv). Status numbers are the family enum values; baseline `-1` means the result had no status member.

### Baseline defects by family and input class

Counts are held-subset rows, not independent samples. `lower`/`upper` repeat an endpoint; `below`/`above` are beyond it. Some labels intentionally contain the same observations (for example zero and a zero lower endpoint). Families absent from this table have no defective rows.

| Family | Defective input classes (row counts) | Total |
|---|---|---:|
| beta | empty: 4 | 4 |
| binom | below: 2, negative: 1 | 3 |
| exp | below: 1, empty: 1, nan: 1, negative: 1, neginf: 1, posinf: 1 | 6 |
| gamma | empty: 1 | 1 |
| geom | below: 2, empty: 2, negative: 2 | 6 |
| gumbel | nan: 2, neginf: 2, posinf: 2 | 6 |
| invexp | below: 1, empty: 1, lower: 1, nan: 1, negative: 1, neginf: 2, posinf: 2, unit-boundary: 1, zero: 1 | 11 |
| invgamma | empty: 1 | 1 |
| invgauss | below: 1, constant: 1, empty: 1, lower: 1, nan: 1, negative: 1, neginf: 1, one: 1, posinf: 1, unit-boundary: 1, zero: 1 | 11 |
| invweibull | empty: 1 | 1 |
| laplace | nan: 4, neginf: 4, posinf: 4 | 12 |
| lgamma | empty: 1 | 1 |
| lgser | below: 2, empty: 2, lower: 1, negative: 2, unit-boundary: 2, zero: 2 | 11 |
| llogis | empty: 1 | 1 |
| lnorm | below: 1, constant: 1, empty: 1, lower: 1, nan: 1, negative: 1, neginf: 1, one: 1, posinf: 1, unit-boundary: 1, zero: 1 | 11 |
| logitnorm | above: 4, below: 4, constant: 1, empty: 4, lower: 4, nan: 4, negative: 4, neginf: 4, one: 1, posinf: 4, unit-boundary: 4, upper: 4, zero: 4 | 46 |
| naka | empty: 1, negative: 1 | 2 |
| norm | nan: 3, neginf: 4, posinf: 4 | 11 |
| nsbeta | empty: 1, nan: 1, negative: 1, neginf: 1, posinf: 1, unit-boundary: 1, zero: 1 | 7 |
| pareto | empty: 1, nan: 1, negative: 1, neginf: 1, posinf: 1, unit-boundary: 1, zero: 1 | 7 |
| pois | below: 2, empty: 2, negative: 2 | 6 |
| unif | empty: 4 | 4 |
| weibull | empty: 1 | 1 |

## Core fixes and statuses

- `exp`, `pareto`, `lnorm`, `invexp`, `invgamma`, `invweibull`, `weibull`, `llogis`, `gamma`, `beta`, `naka` and `unif`: empty inputs now carry non-success instead of inheriting aggregate `Ok`. `lgamma` and `nsbeta` inherit the corrected delegate result.
- `norm`, `laplace` and `gumbel`: reject nonfinite observations before reductions or sorting; conditional normal/Laplace and Gumbel paths also validate held parameters.
- `lnorm`: preserve normal delegate status through the log transform and Jacobian, including constant positive samples. Conditional fixed-scale fits retain their valid successes.
- `logitnorm`: add a status member, reject invalid/unit-endpoint data, preserve normal collapse status, and distinguish numerical failure from zero-scale nonexistence.
- `invexp`: invalid data have a status; preserve exponential delegate status through reciprocal transformation; held paths validate original support before transforming.
- `pareto`, `gamma`, `beta`, `weibull`, `invweibull`, `llogis`, `invgamma` and `naka`: validate original data before transformation or reduction. In particular squaring negative Nakagami observations no longer hides their invalid support.
- `invgauss`: distinguish invalid/empty input, unbounded constant-data shape likelihood and nonfinite numerical results.
- `binom`: reject negative counts and invalid finite/infinite sizes before free-probability branches; the search is unchanged.
- `pois`, `geom`, `lgser`: add statuses for invalid/empty samples and unresolved numerical likelihoods. Preserve proper Poisson/geometric zero-count endpoint fits. Log-series all-ones retains its stop and compatibility `all_ones` field, with an explicit point-mass-limit status.
- Normal, Laplace, lognormal, logit-normal and Pareto also mark unresolved conditional likelihoods as numerical failure; appended extreme held-parameter checks exercise this distinction.

New enums: `LogitnormStatus { Ok, InvalidInput, Unbounded, IterationFailure }`; `PoisStatus` and `GeomStatus { Ok, InvalidInput, NumericalFailure }`; `LgserStatus { Ok, InvalidInput, NumericalFailure, PointMass }`. `BetaStatus::InvalidInput`, `GammaStatus::InvalidInput` and `GammaStatus::TransformFailure` are appended after existing values. TransformFailure distinguishes numerical loss in inverse-gamma/Nakagami delegates from invalid original input; analytic joint constant-data nonexistence is checked before transformation, preserving its status/message even at extreme magnitudes; inverse exponential uses its existing IterationFailure for unrepresentable reciprocals. Existing enum values/names and result fields are retained.

Each directly fixed family has appended TASK-132 `test.cpp` cases. `lgamma` and `nsbeta` have delegate-regression cases. Canonical dossiers link the shared status contract. No search algorithm, golden, d/p/q/r, special-function seam or truth fixture changed.

## Shared-layer diagnostics

Normal/Laplace, lognormal, logit-normal, inverse Gaussian, Pareto, inverse exponential, Weibull/inverse Weibull, log-logistic, Gumbel, uniform and logarithmic-series now use core statuses for the detected condition. Poisson/geometric stop on their status. Gamma/beta and their shared delegates map the new invalid-input value. Existing constant-data/nonexistence messages are retained, including the exact empty-gamma compatibility message `Gamma MLE failed` asserted by JavaScript. New invalid-input messages identify required support/nonempty finite data and compatible holds. New numerical-failure messages identify an unresolved likelihood rather than incorrectly calling nonfinite data constant. R and JavaScript consume the same mappings. The existing inverse-exponential reciprocal-overflow and Nakagami squared-underflow precision messages are retained via numerical/transform statuses, rather than misclassifying valid original data as invalid.

## TASK-118: required coefficient offers

The missing-required message keeps its kind, hold example and advice. Its “or use `p = NULL` to estimate it where supported” clause is now conditional on the actual other holds and exact structural README exceptions. Actual argument values are evaluated so a variable containing NULL counts as free; matched names preserve positional calls.

| Required coefficient | When NULL is offered |
|---|---|
| bbinom `size` | Both alpha and beta held |
| cmb `size` | Both prob and nu held |
| hyper `N` | Both m and k held |
| hyper `k` | N held |
| nhyper `N` | m held |
| nhyper `r` | m or N held |
| fatigue `mu`; fpareto `min`; genbeta `scale`; gpd `mu`; nsbeta `min`/`max`; pareto2/3/4 `min` | Never |
| Other required coefficients | Unchanged offer; no blocking structural exception |

Conformance checks every missing required coefficient under every subset of other holds. Its error-reason matching also now distinguishes exact free-coordinate exceptions from unsupported adapter coefficients, following the schema; it no longer accepts a free-fit exception as a reason for a failure after extra coefficients are held. Explicit examples cover pareto2, hnorm, cmb, positional calls and a NULL-valued variable.

## Roxygen literal inline code

The generic roxygen-line emitter changes literal backtick spans beginning with `r` plus whitespace into Rd code macros, including examples such as `r = 3`, `r <= min(x)` and `r <= min(m,min(x))`. It operates on every family and does not edit dossiers or generated d/p/q/r. Final regeneration must contain no “failed to evaluate inline markdown code” message.

## Validation

All builds/tests use the shared CPU slot locks, nice level 10 and pinned BLAS/OpenMP threads. Temporary files and logs are under this worktree’s `build/`. Environment: Linux x86_64, GNU C++ 15.2.0 in C++17 mode, existing R 4.6.1, Rcpp/BH headers; no dependency packages installed. The standalone baseline snapshot was populated with read-only `git show HEAD:<mle.hpp>` over the worktree include tree.

The [source SHA-256 manifest](2026-10-31-diagnostics-source-sha256.txt) identifies the uncommitted source tested, in addition to the baseline Git revision.

| Command (all under `/tmp/umvl-cpu/run4`) | Final result |
|---|---|
| `just cpp-test` | 105/105 shards; 39.01 sec |
| `just cpp-test-no-long-double` | 126/126 shards; 72.60 sec |
| `just test` | `[ FAIL 0 | WARN 0 | SKIP 151 | PASS 19778 ]`; roxygen inline-code diagnostic absent |
| `just js-build` | Completed successfully |
| `just js-test` | 400 passed; 0 failed |
| `just test-full` | `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 39931 ]`; roxygen inline-code diagnostic absent |

Full-tier skip: `test-dist-sn.R:136:3` — {sn} is not installed. This independent R comparison remains unavailable; no dependency was installed.

The diagnostic sweep took 1.74 seconds in the final native dev run (60-second test timeout). Status/domain checks are exact; other tolerances are those in the family tests, plus the documented Pareto IV double-only comparison adjustment. The final full R run occurred once, after native, double-only, R dev and JavaScript checks completed. No final test failures are permitted.

Source manifest: 98 files; SHA-256 `a71ee313779576e5d44da6653c0637e3ead7292155b075675b0ef01872659a2c`. Logs remain in `build/scratch/final-*.log`; the table and row-level CSV preserve the results outside ignored build output.

### Pareto IV double-only baseline tolerance

The first no-long-double run passed 125/126 shards, failing the existing Pareto IV golden coefficient check. A standalone comparison against the untouched baseline include snapshot and the edited include tree returned bit-identical output in both: shape1 = 0.31572653763270259, shape2 = 0.66237153567540563, scale = 1.0887475136519189, logLik = -3607.2382431667843, status Ok. Relative coefficient differences from the extended-precision golden were -3.04794e-7, +2.65616e-7 and -4.17293e-7. Independent log-density summation differed from the golden by -1.07803e-12; the analytic total score norm in log(shape1), log(shape2), log(scale) was 2.01363e-5.

Only the double-only comparison tolerance in `test_support.hpp` now uses 5e-7, matching its existing wasm tolerance. Native tolerance remains 1e-8. The fitter and golden are unchanged. An appended Pareto IV case independently checks the total likelihood to 1e-8 absolute and total log-coordinate score norm below 1e-3. These demonstrate numerical agreement and local stationarity, not global optimality. The dossier records the reason.

Reproducer: [pareto4-precision.cpp](../../validation/pareto4-precision.cpp), compiled twice with `c++ -std=c++17 -O2 -DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS -I<include-tree> -IunivariateML-cpp/tests -I/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include`, first with `build/diagnostics-baseline/include`, then `univariateML-cpp/include`. Both executables read the same unchanged golden.

Compiler logs contain nonfatal warnings, including pre-existing indentation warnings, unused validation-route locals and a GCC LstCost array-bounds nomination. The latter is behind `lst_detail::conditional`’s finite positive-df guard, so that route selects the fixed-df branch and does not read p[2]. Search code was left untouched. R test warnings and the requested roxygen diagnostic are reported in the validation table.

An initial C++ run also caught a new test incorrectly classifying exponential data {0,2} as invalid; the test was corrected and an explicit valid-endpoint case appended. An initial targeted R run exposed an error in the new positional-CMB test (it assumed a successful call was an error); the test was corrected. It also exposed an initial inline-code pattern that missed spans containing `n`; the emitter was corrected. The first dev run additionally caught two new test errors (the positional example omitted the wrapper’s na.rm position, and expect_s3_class did not accept info in this testthat version), plus two precision regressions caused by classifying transformation loss as invalid data. The tests and numerical status classification were corrected; precision-message regressions and held-transform cases are covered. The first JavaScript run caught an exact-message compatibility check for empty gamma input; the status mapping was refined to retain that existing text without losing the core InvalidInput status. None is a permitted final failure. The initial audit route crash was validation-only empty tbinom min/max preparation, not a core crash.

## Mathematical scope and open coverage

The scale-collapse statement for normal and its one-to-one log/logit transforms follows directly from the likelihood: with all transformed observations equal to the fitted mean, likelihood increases without bound as scale tends to zero. A held positive scale removes that path. Inverse Gaussian’s profiled quadratic term vanishes on constant positive data, so shape tends to infinity. Input rejection and status propagation are deterministic checks. The audit is numerical regression evidence over the stated finite workloads; it is not an existence, uniqueness, calibration or global-optimality proof for arbitrary samples. No new numerical search or capped estimate was introduced.

Integer-span representability limits are covered at the R conversion boundary, not by inventing an invalid C++ integer representation. Existing non-success statuses may contain NaN/infinite estimates, which the harness records and permits. Closed parameter-domain endpoints permitted by R7 are not treated as defects. General optimizer certification and adversarial samples outside this sweep remain the families’ documented mathematical scope. There are no remaining defects in the audited rows.

Artifact display is blocked by the viewer's readable-root policy: `bin/show` returned HTTP 403 because this worktree is absent from the existing thread registry and readable roots. Refreshing the registry did not resolve it. No viewer configuration or files outside this worktree were edited. Opening a Nook thread in this directory, or explicitly adding this directory to `CODEX_MATH_WEB_VIEW_ROOTS`, is needed to display the report in the pane.
