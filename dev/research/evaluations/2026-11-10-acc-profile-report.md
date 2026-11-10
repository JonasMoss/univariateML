# Default-fit coefficient accuracy: acc-profile (TASK-150.2.2)

The eleven assigned fitters now finish with coefficient-based local accuracy checks at the selected maximum. All 44 default fits (native C++, double-intermediate C++, R and WASM) agree with their stored 50-digit references within `1e-10`. The frozen audit replay contains 223 cases: all finish successfully, with no new stops and no coefficient errors above `1e-10`. Final validation finished on 2026-11-11.

The following errors are the largest relative errors among free coefficients, compared with the high-precision reference. Held coefficients are excluded. This comparison is at least as strict as the accuracy contract's natural location floor. The baseline is the supplied commit `c60d92ea667e8dcb55ec6c6cbd1aaae474846d45`.

| Family | Before, native | After, native | After, double | After, R | After, WASM | Golden allowance |
|---|---:|---:|---:|---:|---:|---:|
| sn | 4.32e-07 | 9.32e-15 | 9.32e-15 | 9.34e-15 | 9.32e-15 | 2e-14 |
| skellam | 3.96e-08 | 3.58e-16 | 2.50e-16 | 3.63e-16 | 2.50e-16 | 5e-15 |
| sgomp | 2.88e-08 | 6.58e-16 | 6.58e-16 | 6.40e-16 | 6.58e-16 | 1e-15 |
| dgamma | 2.14e-09 | 4.16e-17 | 2.17e-15 | 4.39e-17 | 1.07e-15 | 5e-15 |
| pareto3 | 1.78e-16 | 6.54e-17 | 6.54e-17 | 5.27e-17 | 6.54e-17 | 1e-15 |
| zinb | 3.79e-08 | 1.03e-15 | 3.78e-15 | 1.04e-15 | 3.78e-15 | 1e-14 |
| zmnbinom | 3.79e-07 | 4.65e-15 | 5.32e-15 | 4.66e-15 | 5.32e-15 | 1e-14 |
| ztnbinom | 2.71e-07 | 2.89e-15 | 2.28e-15 | 2.88e-15 | 2.28e-15 | 1e-14 |
| ht | 7.90e-08 | 3.53e-15 | 3.53e-15 | 3.54e-15 | 3.53e-15 | 1e-14 |
| emg | 4.07e-09 | 4.73e-15 | 1.91e-14 | 4.73e-15 | 2.84e-14 | 5e-14 |
| genbeta | 3.85e-09 | 7.58e-16 | 3.37e-15 | 7.68e-16 | 3.37e-15 | 1e-14 |

Each shared golden allowance covers coefficients **and log likelihood**, against both the reference and the saved rounded golden targets. It is 1.5 times the largest measured error across the four modes, rounded upward on the port's `1, 2, 3, 5, 10` lattice; every allowance is at most three times that joint error. Thus an allowance can exceed three times the coefficient-only error shown above. All allowances tighten those at the baseline. All eleven `golden.json` files remain byte-for-byte unchanged. C++ and R use these allowances directly; the existing JS generator derives its policy from the C++ constants. [Full component checks](2026-11-10-acc-profile-data/checks.json) and [comparison data](2026-11-10-acc-profile-data/golden-errors.json) retain the unrounded results.

The shared refinement uses analytic first and second derivatives carried by a three-coordinate second-order jet. It factors the observed information by Cholesky and evaluates the **undamped** Newton correction in every free coordinate. The acceptance target is `tol/16`; above a practical rounding floor, the last corrections must contract at least fourfold and be compatible with quadratic convergence. At a floating-point plateau, two successive corrections below `tol/16` are required. This is a curvature-based Newton error estimate, conditional on the computed derivatives and information, with room for transformation error. It is not an outward-rounded enclosure. A small objective change or a damped step does not accept a fit. Failed refinement returns a numerical/search failure, with a separate accuracy-failure reason where rounded public coefficients cannot represent the solution. [Implementation](../../../univariateML-cpp/include/cppunivariate/core/likelihood_polish.hpp).

The family-specific changes use the likelihood structure:

- **sn:** joint location, normal scale and shape refinement, with scaled coordinates and a continued fraction for negative normal tails. The location floor is the fitted normal scale.
- **skellam:** differentiated, mode-centred positive Bessel sums for the two rates. A zero-rate Poisson face retains its closed-form fit, with a compensated sample mean.
- **sgomp:** joint rate/shape derivatives with the small-shape cancellation removed analytically. The held `eta = 0` exponential model uses its compensated closed-form rate; a selected exponential face receives the same accurate rate.
- **dgamma:** derivatives of the incomplete-gamma cell integrals, including shape derivatives, followed by joint shape/rate refinement. Density-at-midpoint derivatives are not substituted for cell-mass derivatives.
- **pareto3:** joint log-shape/log-scale refinement after the existing log-logistic search, retaining its global certificate flag and held endpoint.
- **zinb, zmnbinom, ztnbinom:** a common log-size/logit-probability refinement. Integer rising factorials avoid subtracting nearly equal polygamma values near the Poisson face. ZINB includes free inflation in the joint information; ZMNB retains the closed binomial zero proportion. The coupled solve controls the public size and probability together.
- **ht:** half-step gamma corrections and the Gaussian limit terms cancel before differentiation, preserving information about weakly identified degrees of freedom. The free endpoint remains the sample minimum; an active degrees-of-freedom restriction stays fixed during refinement.
- **emg:** joint location/log-scale/log-rate refinement; the negative-tail quadratic cancellation is performed analytically. The existing location-only sign bracket is retained and tightened to a scale-relative target, with compensated score bounds.
- **genbeta:** product coordinates `shape1 * shape3` separate the gamma-limit terms. A gamma-ratio recurrence and Stirling remainder, and a small-power expansion, stabilize the derivatives. With power held, the beta solve or a monotone digamma-increment bracket controls the free shapes. Existing certified scalar submodels and the ridge convention are retained.

The positive log coordinates give relative coefficient error estimates; logit coordinates give at least the same relative control on probabilities. For genbeta, the error in log `shape1` is the difference of two solved coordinates, covered by the refinement margin. Scaled location and scale exports in sn, ht, pareto3 and emg are checked against the coordinates actually represented by the returned doubles. These local changes preserve the existing global search decisions, limiting models and uncertified-result flags; they do not upgrade an uncertified global result into a globally certified one.

The five changed defaults are sn/skellam `reltol`, dgamma/emg `reltol`, and genbeta `gradtol`, all now `1e-10` in the core, bindings and dossiers. Public control names are retained; the control rename belongs to TASK-150.1. HT retains its existing API and uses the `1e-10` local target.

The frozen audit selection is replayed before and after through the common binding seam. Its references are numerical high-precision evidence for the selected maxima, rather than a new global-optimality proof. Pareto3 was already accurate on this baseline; its refinement remains within the contract.

| Family | Cases | Largest error before | Largest error after |
|---|---:|---:|---:|
| sn | 29 | 9.31e-09 | 4.17e-16 |
| skellam | 26 | 3.71e-08 | 6.19e-16 |
| sgomp | 11 | 4.22e-08 | 1.61e-15 |
| dgamma | 15 | 1.20e-08 | 4.78e-13 |
| pareto3 | 3 | 1.24e-16 | 7.68e-13 |
| zinb | 2 | 5.41e-08 | 1.69e-15 |
| zmnbinom | 38 | 6.52e-07 | 5.52e-13 |
| ztnbinom | 18 | 6.45e-08 | 3.04e-15 |
| ht | 24 | 5.08e-06 | 2.55e-12 |
| emg | 37 | 2.19e-08 | 9.60e-12 |
| genbeta | 20 | 1.98e-08 | 3.63e-12 |

[Replay generator](2026-11-10-acc-profile-data/replay.py), [frozen cases and references](2026-11-10-acc-profile-data/replay-references.json), [before output](2026-11-10-acc-profile-data/replay-before.jsonl), [after output](2026-11-10-acc-profile-data/replay-after.jsonl), and [errors](2026-11-10-acc-profile-data/replay-errors.json) make the 223 comparisons reproducible. The largest remaining replay error is `9.60e-12` (emg).

Additional regressions separate mathematical identities from numerical comparisons:

- For genbeta with `shape2 = 2` and power 1, `psi(a+2)-psi(a) = 1/a + 1/(a+1)`. For `c = -mean(log(x))`, the unique maximizer is `(2-c+sqrt(c*c+4))/(2*c)`; the score is strictly decreasing. At ten million copies of the double `.999999`, naive double summation alone perturbs the shape by `1.1922e-10` relatively. Compensated statistics and score/information reductions fix this. Fresh native, double, R and WASM fits have respective errors `3.58e-12`, `8.32e-13`, `3.58e-12`, `8.32e-13`. [Checker](2026-11-10-acc-profile-data/replication.cpp), [R](2026-11-10-acc-profile-data/replication.R), [WASM](2026-11-10-acc-profile-data/replication.mjs), [naive-reduction experiment](2026-11-10-acc-profile-data/replication-statistic-before.txt).
- For ten million constant Skellam observations `k = 2000000001`, the global maximizer is `(k, 0)`: conditioning on the second Poisson count bounds every mass by the maximum Poisson mass at k. Naive double summation gives mean `2000000000.4503601`, a `2.75e-10` relative miss. The compensated mean returns k exactly in both C++ modes.
- For held sgomp `eta = 0`, the exponential score has the unique root `b = n/sum(x)`. With ten million observations, all `.1` except the last 1, naive summation perturbs b by `1.61e-10` relatively. The corrected rate has `3.8e-17` relative error in both C++ modes. [Boundary checker](2026-11-10-acc-profile-data/boundary-rounding.cpp), [native results](2026-11-10-acc-profile-data/boundary-native.jsonl), [double results](2026-11-10-acc-profile-data/boundary-double.jsonl).
- The new representability checks have a proved failure case. For held half-Cauchy degrees of freedom 1, endpoint 0 and data `(0,d,2d)`, the unique scale maximum is `d*sqrt((sqrt(73)-5)/6)`. When d is the smallest positive double, no positive double scale satisfies relative `1e-10`; both C++ modes report `AccuracyFailure`. This is numerical representability failure, not nonexistence. The analogous normal submodel of sn, with data `(0,0,0,d)` and held scale d and shape 0, has unrepresentable location `d/4`; its existing numerical failure is retained and regression-tested.
- The former genbeta shallow-fixture coefficients were inaccurate. They are replaced in C++, R and JS by an independent 90-digit stationary reference, while retaining successful-fit and uncertified-global-result assertions and tightening `1e-7` to `1e-10`. [Reference calculation](2026-11-10-acc-profile-data/shallow-reference.py), [native reference](2026-11-10-acc-profile-data/shallow-reference.json), [double-input reference](2026-11-10-acc-profile-data/shallow-double-reference.json). Five near-normal half-t seeded cases have independent 90-digit checks; the largest new error is `4.55e-15`. [References](2026-11-10-acc-profile-data/ht-references.json).

All final runs below used the **workstation**, through the CPU-slot wrappers. Simbox was not used, as instructed. The final full tiers ran after the numerical changes; all commands have finished.

| Command | Final result | Retained log |
|---|---|---|
| `just cpp-test` | 106/106 shards | [log](2026-11-10-acc-profile-data/cpp-dev.log.gz) |
| `UMVL_TEST_TIER=full just cpp-test` | 132/132 shards | [log](2026-11-10-acc-profile-data/cpp-full.log.gz) |
| `just cpp-test-no-long-double` | 132/132 shards | [log](2026-11-10-acc-profile-data/cpp-double-full.log.gz) |
| `just test` | PASS 23010; FAIL 0; WARN 0; SKIP 154 | [log](2026-11-10-acc-profile-data/r-dev.log.gz) |
| `just test-full` | PASS 45825; FAIL 0; WARN 0; SKIP 0 | [log](2026-11-10-acc-profile-data/r-full.log.gz) |
| `just js-build` | passed | [log](2026-11-10-acc-profile-data/js-build.log.gz) |
| `just js-test` | 595/595; zero failures/skips | [log](2026-11-10-acc-profile-data/js-test.log.gz) |
| `just site-check` | passed | [log](2026-11-10-acc-profile-data/site-check.log.gz) |

[Run records and log hashes](2026-11-10-acc-profile-data/final-runs.json), [toolchain and dependency versions](2026-11-10-acc-profile-data/environment.json), and [source hashes](2026-11-10-acc-profile-data/source-hashes.json) record the tested state. Run `bash dev/research/evaluations/2026-11-10-acc-profile-data/reproduce.sh` from the repository root to regenerate the default-fit captures, audit replay and replication checks; it reserves its own slots. [Verifier](2026-11-10-acc-profile-data/verify.py) checks all four modes, frozen goldens and the allowance policy.

No d/p/q/r, special-function seam, truth table or R code-generator source changed. The protected status, news and roadmap files are unchanged. The work is uncommitted, on `codex/acc-profile` at the supplied baseline.

The artifact-pane delivery attempt returned HTTP 403: Nook's readable roots omit this worktree. Displaying the report there requires adding `/home/jonas/Files/programming/univariateML.wt/acc-profile` to `CODEX_MATH_WEB_VIEW_ROOTS`, or opening a Nook thread rooted there. The report remains in the requested worktree; no viewer configuration was changed.
