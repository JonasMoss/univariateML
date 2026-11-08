# TASK-154: normal residual scaling

Completed 2026-11-08 in `codex/norm-scale`, based on
`cfb7c4b37d13d1f23ca8ceb546333eb87290b1f6`. No commits, board edits, dependency
installations, or publication actions were performed. The tested source files
are identified by the [SHA-256 manifest](2026-11-08-norm-scale-data/source-manifest.json).

## Result and mathematical basis

The conditional normal fit now scales residuals before squaring, for every
held/free combination. The reported counterexample, `x = {1e-200, 2e-200}`
with mean held at zero, returns `sd = 1e-200 * sqrt(2.5)` with status `Ok`.
The corresponding `1e200` case also succeeds in both precision modes.

For residuals `r[i] = x[i] - mean`, let `b = max(abs(r[i]))` and
`Q = sum((r[i]/b)^2)`. The free scale is `b * sqrt(Q/n)`; likelihood evaluation
uses `Q * (b/sd)^2`, without forming a physical squared scale or residual sum.
If subtraction itself overflows, residuals are first halved and the final RMS
is doubled. The free-mean fallback sums `x[i]/n` directly instead of multiplying
that finite mean back by `n`. This also recovers finite fits near the largest
representable double.

In exact arithmetic these are identities. With mean held, the scale score
changes sign once at the positive residual RMS. With sd held, the location
maximum is the sample mean. With both free and nonconstant observations, these
give the unique normal MLE. Scale collapse occurs exactly when every residual
is zero; the implementation tests equality of the original observations and
location, independently of the computed sum. An unrepresentable free scale,
or a subnormal free scale lacking the supported relative precision, produces
`IterationFailure`, not a collapse declaration. Holding a positive sd permits
zero residuals and returns a finite likelihood.

Lognormal and logit-normal conditional fits now delegate to this same normal
solver and add their Jacobians. The conditional logit-normal collapse payload
retains its previous NaN coefficients. Huber and EMG Gaussian faces reuse the
normal fit; EMG's large-shape Gaussian bound also normalizes its residual
moment. Truncated normal uses RMS and standardized moments directly, including
its final score enclosure. Two-piece normal restores physical units only
after forming its side-scale variance in normalized coordinates. These changes
preserve the respective likelihood/profile identities and existing search or
certificate arguments; they introduce no new numerical search or work cap.

The identities above are the mathematical argument. The precision measurements
below are numerical evidence, not an outward-rounded floating-point proof.

## Audit of related sites

Paths in this table are relative to
`univariateML-cpp/include/cppunivariate/distributions/`. Both direct normal
calls and residual/second-moment constructions were inspected across `*.hpp`,
including certificate headers.

| Site | Verdict |
|---|---|
| `norm/mle.hpp`: `moments`, conditional `norm_mle` | Changed: shared scaled residual RMS, standardized likelihood, subtraction-overflow fallback, and exact collapse comparison. |
| `lnorm/mle.hpp`: free and conditional `lognormal_mle` | Free fit already delegated. Replaced the conditional raw-square implementation and overflow-only fallback with the normal delegate. |
| `logitnorm/mle.hpp`: `logitnorm_mle` | Replaced the conditional raw squares with the normal delegate; retained the conditional collapse payload contract. |
| `hnorm/mle.hpp`: free scale and held-scale likelihood | Already divides by the largest residual for the RMS, or by held sigma for likelihood terms. Unchanged; added extreme-scale coverage. |
| `foldnorm/mle.hpp`: `m2`, variance, held-mu residual moment | Already computed after scaling data and held coefficients. Unchanged; added extreme-scale coverage. |
| `snorm/mle.hpp`: final free-fit side-scale conversion | Changed: form the variance before multiplying by physical scale. This removes another double-mode underflow/overflow site. |
| `snorm/certificate.hpp`: held-subset moments and side scales | Already uses normalized coordinates. Unchanged. |
| `bhatt/mle.hpp`: `legacy_candidate` normal member | Inherits the normal repair. Also propagates non-Ok normal statuses instead of turning a failed delegate into `Ok` or false collapse. Other proposal variances use normalized data. |
| `bhatt/certificate.hpp`: `normal_member_certificate` | Already protected by scaled residuals and an exact equality test. Retained; added huge-scale coverage alongside its existing tiny-scale regression. |
| `bhatt/certificate.hpp`: normal call in `WidthCertificate::run` | Inherits the normal repair; already checks its status before using the witness. |
| `emg/mle.hpp`: dominating normal face | Changed to the conditional normal delegate. |
| `emg/certificate.hpp`: `cert_normal_face` | Already normalizes by the largest residual. Unchanged. |
| `emg/shape_bounds.hpp`: `large_shape_upper` Gaussian moment | Changed: normalized residual moment and residual-to-sd ratio, preserving its upper-bound formula. Added a unit-equivariance regression. |
| `huber/mle.hpp`: Gaussian faces in `huber_candidate_mle` and `huber_mle` | Changed both raw-square scale calculations to the normal delegate. Finite-epsilon solvers already normalize their data. |
| `tnorm/mle.hpp`: initial moments; both-held and held-sd likelihood moments; held-mean RMS; score-enclosure normalization | Changed all physical variance/RSS constructions to shared RMS or standardized moments. Existing Newton and acceptance logic is retained. |
| `std/mle.hpp`: normal preconditioner and conditional normal face | Inherits `moments`/`norm_mle` repair. Remaining scale iterations operate on standardized data. |
| `lst/mle.hpp`: conditional normal face and weighted scale iterations; `lst/certificate.hpp`: initial/weighted moments | Normal face inherits the repair; other moments already use normalized data. Unchanged. |
| `sstd/mle.hpp`: normal preconditioner, `snorm_mle` face; `sstd/hold_search.hpp`: held `snorm_mle` delegate | Inherits the shared moments and two-piece-normal repairs. Other squared moments are standardized. Unchanged. |
| `ged/mle.hpp`: seed variance fallback, normal-member location and power-profile scale | Existing fallback uses stable normal moments; the conditional power moment divides by its residual radius. Unchanged. |
| `sged/mle.hpp`: seed variance fallback and standardized profiles; `sged/skew_certificate.hpp`: normal-shape delegate | Existing fallback uses stable normal moments; the normal-shape delegate inherits the snorm repair. Unchanged. |
| `logis/mle.hpp`: Welford seed | Normal-moment preconditioning precedes the raw seed at extreme units/offsets. Already guarded; unchanged. |
| `gumbel/mle.hpp`: variance seed | Zero/nonfinite raw variance triggers normalized fitting using normal moments; constancy is tested separately by equality. Already guarded; unchanged. |
| `st/mle.hpp`: half-normal delegate; `ht/mle.hpp`: normal face | Half-normal delegate already scales its RMS; half-t normal-face squares are divided by the sample maximum. Unchanged. |
| `rayleigh/mle.hpp`: free RMS and held-scale likelihood | Already scales by the maximum, or by held sigma. Unchanged. |
| `dnorm/mle.hpp`: cell variance | Valid observations are bounded integers, so the tiny/huge continuous-unit square failure does not arise. Unchanged. |
| `trgamma/mle.hpp` (also used by inverse trgamma), `burr/mle.hpp`, `burr/fixed_scale.hpp`, `burr/certificate.hpp` (also inverse Burr), `paralogis/mle.hpp` | Moments concern bounded logarithms of finite positive inputs or further normalized log coordinates. Unchanged. |
| `lomax/mle.hpp`, `gompertz/mle.hpp`: second-moment seeds/face check | Extreme physical units are removed by mean-based normalization before these calculations. Unchanged. |
| `naka/mle.hpp`: squared-data transformation and mean power | Its scale coefficient is mean squared amplitude, not an RMS. At the requested `1e±200` units that coefficient is itself unrepresentable; the existing typed transformation failure is appropriate. Unchanged. |
| `fatigue/mle.hpp`, `fatigue/certificate.hpp`; `invgauss/mle.hpp` | The nominated squares are dimensionless shape/dispersion calculations, not an unscaled residual RMS for the physical scale coefficient. No normal-sd delegate or physical RMS repair applies here. Unchanged. |
| `norm/inference.hpp`; `norm/score.hpp` | Exact variance intervals are inference rather than scale fitting and already report numerical failure when their variance is unavailable. Score routines using `norm_mle` inherit its repair. Unchanged. |

The audit used `rg` for `norm_mle`, `norm_detail::moments`, square/variance
accumulators, `sqrt`, `M2`, and raw products, followed by inspection of the
coordinate normalization and the parameter being estimated. Squared score,
Hessian and discrete-count terms were excluded from the RMS-scale category.

## Regression and independent evidence

C++ regressions were appended to the norm, lnorm, logitnorm, hnorm, foldnorm,
snorm, bhatt, emg and tnorm test files. The existing Huber double-mode failure
expectation and truncated-normal underflow-stop expectation now require the
recovered result. The previous logit-normal failure-payload expectation remains
intact. R and JS cover all four normal held subsets, transformed-normal held
locations, normalized half/folded/Bhatt scale fits, two-piece normal, and Huber
and EMG normal-limit witnesses at `1e-200` and `1e200`. C++ additionally checks
near-maximum-double subtraction and non-collapse on nonzero subnormal data.

For the transformed families, constant observations `1` and `0.5` have exact
transformed value zero. Holding their transformed locations at `1e±200`
therefore tests actual representable residuals; exponentiating tiny/huge
transformed samples would instead round away the intended data or overflow.

The [independent check](2026-11-08-norm-scale-data/check.py) evaluates all four
normal subsets at both units using 100-digit Decimal arithmetic on the exact
binary64 observations. It compiles a separate normal probe in extended and
forced-double modes: 16/16 comparisons passed. Maximum relative sd error was
`5.35e-17`; maximum absolute log-likelihood error was `8.81e-14`.
[Full numerical results](2026-11-08-norm-scale-data/normal-oracle.json).

The first full JS run exposed a pre-existing stale binomial-family assertion:
it demanded an unresolved stop and infinite upper bound for four cases whose
signed certificates already prove their Poisson-family limits. The
[baseline reproducer](2026-11-08-norm-scale-data/check-binomial-baseline.py)
compiled 37 headers extracted from the starting commit into standalone WASM.
All 37 are unchanged by this job; all four baseline cases return certified
limits with finite bounds and both finite and face witnesses.
[Baseline evidence](2026-11-08-norm-scale-data/binomial-baseline.json).
The JS expectation now checks those models, bounds and witnesses. No binomial
fitter or binding implementation changed.

## Final validation

All commands finished. Local computations used the shared slot wrappers;
long suites used simbox as shown. Jobs ran at nice level 10 with BLAS/OpenMP
threads pinned to one. C++ used GCC 15.2 locally and GCC 13.3 on simbox; R
package compilation used GCC 15.2 on both machines. JS used the installed
Emscripten toolchain and Node 22.22.1 on the workstation.

| Final check | Machine | Result |
|---|---|---|
| `just cpp-test` (dev) | Workstation | 106/106 passed |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129 passed |
| `just cpp-test-no-long-double` | Workstation | 129/129 passed |
| `just cpp-test-no-long-double` | simbox | 129/129 passed |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21914 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44737 |
| Forced-double R `test_local(filter="^dist-norm$")` | Workstation | FAIL 0, WARN 0, SKIP 1, PASS 111 |
| `just js-build` | Workstation | Passed, also rebuilt by the site dependency |
| Final `just js-test` | Workstation | 539/539 passed |
| `just site-check` | Workstation | Passed, including browser and build-time MathML checks |
| Independent normal Decimal/probe check | Workstation | 16/16 passed |
| Standalone starting-commit binomial WASM check | Workstation | 4/4 certified limits; 37 unchanged headers |
| `git diff --check` and frozen-file comparison against HEAD | Workstation | Passed |

For the forced-double R run, a worktree-local Makevars included the existing
user compiler configuration and added
`CPPFLAGS += -DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`; compiler output
confirmed the flag. It used one worker and the generated package's copied
`test-dist-norm.R`. The single skip is the full-tier multistart check, which
passed in the full native R run. Simbox runs explicitly used four build/test
workers. Scratch commands and logs are under `build/scratch/`.

Goldens, truth tables, `dpqr.hpp`, `special/`, and code-generation scripts remain
byte-identical to the starting revision. No default parameter or control was
changed. Ordinary frozen-data regressions passed without rewriting references.
The site checker refreshed an unrelated tracked screenshot; its original bytes
were restored after validation.

Open items for TASK-154: none.

## Artifact-pane delivery

Open presentation item: the viewer's readable roots do not include this
worktree. The requested `bin/show` call returned HTTP 403 with that specific
reason. The report and evidence are present at their requested worktree paths;
adding this worktree to `CODEX_MATH_WEB_VIEW_ROOTS` (or opening a viewer agent
thread rooted here) is required for pane display. Viewer configuration and
other checkouts were not changed.
