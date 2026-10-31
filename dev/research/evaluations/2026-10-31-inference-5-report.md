# TASK-102.5: exact and nonsmooth inference, 2026-10-31

Implemented and calibrated the requested C++ procedures in branch `codex/inference-5`,
from source revision `bfdf257a2ec2c44fb0b986d2ade376239e9c0d55`. All final suites passed. The six opt-in
family `inference.hpp` headers extend the existing interval/status contract;
13 appended doctest cases exercise analytic pivots, equivariance, invalid input,
confidence levels and underflow. No fitter, authored R/JS source, default fit,
golden, special-function backend or d/p/q/r implementation changed.

The canonical mathematical and algorithm documentation is
[core/inference.md](../../../univariateML-cpp/include/cppunivariate/core/inference.md).
[Source identities and final run records](inference-5/source-identities.json)
identify the uncommitted implementation. No commit or board edit was made.

## Procedures and theory

All exact model claims below are finite-sample real-arithmetic results under
iid sampling from the stated model. Quantiles are numerical calls through the
existing no-throw special-function seam. Independent checks support tested
points, not universal floating-point error bounds. Alpha defaults to .05.

| Procedure / C++ function | Hypothesis, nuisance and theory |
|---|---|
| `laplace_location_wald_interval` | Laplace location with scale fitted by mean absolute deviation about the existing upper sample median. Per-observation Fisher is diag(1,1)/sigma² in (mu,sigma), full rank with zero cross term. SE=sigma_hat/sqrt(n). The median quantile expansion and consistent scale give root-n model-normal inference. The Laplace family is DQM despite its kink; its ordinary sample location Hessian is unsuitable. This is asymptotic, not exact or robust under another density. |
| `median_sign_interval` | Unique median of independent continuous observations sharing that median; iid in this calibration. Binomial(n,1/2) sign inversion returns [X_(k),X_(n-k+1)] with the largest k whose lower tail at k-1 is at most alpha/2. Coverage is at least 1-alpha, with no randomization; k=0 returns the real line. Scale is irrelevant. With positive density at the median the endpoint error is order n^(-1/2). Atoms/ties do not retain the stated binomial equality. |
| `median_sandwich`, `median_sandwich_interval` | Under iid continuous sampling with unique median m and g(m)>0, median variance is 1/(4 n g(m)²). Gaussian KDE at the upper sample median uses h=1.06*(sample MAD/log(2))*n^(-1/5); MAD is median absolute deviation, also using the upper median. No bandwidth floor or fallback is hidden. Zero/nonfinite MAD or unresolved variance fails. The median estimating equation has sensitivity g(m) and meat 1/4; this is a nonsmooth sandwich, not an ordinary Hessian inverse. Under local KDE consistency and a positive consistent MAD scale, studentized median inference is root-n valid without the Laplace model. With two smooth local derivatives the density error is O_p(n^(-2/5)); at a Lipschitz Laplace cusp it is O_p(n^(-1/5)). These are density estimation rates, not coverage-error rates. |
| `pareto_shape_exact_interval` | Pareto a>0, b>0. If b is unknown, T=sum(log(X_i/min X)); exponential memorylessness gives 2aT ~ chi-square(2(n-1)), n>=2. If b is supplied as a known constant, use sum(log(X_i/b)) and 2n df. Divide chi-square quantiles by 2T. Unknown endpoint b has rate n and joint DQM fails; this does not invalidate the shape pivot or its root-n asymptotic behavior. Known-b shape alone is DQM with finite information 1/a². Both versions are model-only. |
| `exp_rate_exact_interval` | Rate lambda, origin known to be zero: 2 lambda sum X ~ chi-square(2n), n>=1 and positive total. Divide the quantiles by 2 sum X. Exact under the exponential model; ordinary rate inference is also regular root-n with information 1/lambda². No unknown origin is fitted. |
| `unif_exact_interval` | Both a,b unknown, n>=2, positive sample range R. From the joint extreme density, R/(b-a) ~ Beta(n-1,2). Endpoint gaps (min X-a)/R and (b-max X)/R have survival (1+t)^(-(n-1)). Inverting these pivots gives exact marginal intervals for a, b and width. Their errors/lengths have order 1/n. Free endpoints are non-DQM; no joint Fisher Wald assertion is made. The three marginal intervals are not a simultaneous 95% box. |
| `unif_endpoint_known_opposite_interval` | The other endpoint is a known constant, n>=1. The extreme gap divided by width has Beta(n,1) law; reflect for the lower endpoint. Translating/reflection of this interval gives the width interval with identical coverage events, so those width results are also covered by the known-opposite rows below. This endpoint submodel remains non-DQM with rate n. |
| `norm_variance_exact_interval` | Positive normal variance, mean fitted: Q=sum((X-Xbar)²), Q/variance ~ chi-square(n-1), n>=2. If the mean is supplied as a known constant, use deviations from it and n df, allowing n=1. Interval [Q/q_upper,Q/q_lower] targets variance, not sd. A centered online sum avoids subtractive cancellation. Regular root-n theory also applies; the pivot is exact only under normal sampling. It complements the existing exact t interval for the mean. |
| `triang_mode_wald_interval`, `triang_mode_tests` | Known a<b and interior a<c<b. The score is -1/(c-a) left of c and 1/(b-c) right of c; per-observation information is 1/((c-a)(b-c)), DQM and rank one. Ordinary negative pointwise Hessian has expectation -I and omits the kink contribution. Consistency, bounded threshold-score empirical processes and the local likelihood expansion yield the efficient root-n model limit. Wald uses sqrt((c_hat-a)*(b-c_hat)/n). Pointwise score and LR statistics have asymptotic chi-square(1) references; independently simulated null LR critical values supply the finite-model alternative. No robust triangular procedure, free-endpoint joint covariance, boundary-mode procedure or general two-endpoint LR interval is exposed. |

Known-parameter arguments mean known constants, not nuisance estimates plugged
back in as though known. Fits used by triangular inference must succeed under
the existing certified finite-candidate fitter. All numerical fields are
unusable on failure. Empty/invalid samples, invalid probabilities/constants,
zero denominators/spread and unresolved arithmetic return statuses; no capped
search estimate, pseudoinverse or Hessian rescue was introduced. The sign
interval's deliberate infinite limits are the sole infinite-endpoint success.

## Calibration design and interpretation

The launcher and streaming production bridge reuse the inference-4 design:
[launcher](../../validation/inference-5.sh),
[calibration](../../validation/inference-5.py),
[probe](../../validation/inference-5-probe.cpp).
There are 40 independently seeded cells, 5,000 validation samples each,
and 120 procedure/setting/sample-size rows. Triangular LR uses a separate
10,000-sample reference bank for each of 12 mode/n cells: 200,000 validation
and 120,000 reference datasets in total. Sampling is single-threaded NumPy;
actual production C++ intervals/tests determine acceptance. Failures count as
noncoverage rather than being discarded: **zero procedure failures** occurred.

Seed is `102520261031` plus the integer from the first 12 hex digits of
SHA256(cell key); reference keys have a separate suffix. Source-hashed,
workload-keyed cells are atomically saved and resumable. [Summary](inference-5/summary.json)
and the per-cell JSON files retain counts, conditional MCSEs, reference critical
values and signatures. Initial 100-replicate smoke cells were replaced because
the final workload signature differs. No tuning was selected from the final
Monte Carlo outcomes.

Tables report coverage probabilities, with **± one Monte Carlo standard error**.
For fixed procedures MCSE=sqrt(p_hat*(1-p_hat)/5000). Simulated-LR entries include
both validation and reference-bank uncertainty: with M=10000 and
k=ceil(.95*(M+1)), the unconditional null acceptance probability is
p0=k/(M+1)=9501/10001 for a continuous statistic. Reference probability MCSE is
sqrt(p0*(1-p0)/(M+2))=0.00218; total coverage SE is
sqrt(p0*(1-p0)*(1/N+(1-1/N)/(M+2)))=0.00377. The raw validation-only SE remains
in JSON. The bank is shared within its validation cell, so ignoring its
uncertainty would understate error. Two-SE bands are about 95% Monte Carlo
uncertainty bands, not statistical confidence intervals for the parameters.

Independent endpoint checks passed at rtol 2e-10 and atol 2e-12: SciPy gamma,
beta/Lomax, normal and binomial calculations, Gaussian KDE, and exhaustive
triangular sample-knot/design-endpoint likelihood comparison on the retained
check sample. Analytic df=2 exponential quantiles and affine/reflection
identities are separately exercised in doctest. These are finite numerical
checks; the pivot derivations supply the exact-law claims.

### Laplace and the continuous normal alternative

| Setting | Procedure | n=10 | n=30 | n=100 | n=1000 |
|---|---|---:|---:|---:|---:|
| Laplace(0,1) | model Wald | 0.8334 ± 0.0053 | 0.8918 ± 0.0044 | 0.9224 ± 0.0038 | 0.9372 ± 0.0034 |
| Laplace(0,1) | median sandwich | 0.9696 ± 0.0024 | 0.9754 ± 0.0022 | 0.9814 ± 0.0019 | 0.9790 ± 0.0020 |
| Laplace(0,1) | exact sign | 0.9792 ± 0.0020 | 0.9564 ± 0.0029 | 0.9658 ± 0.0026 | 0.9518 ± 0.0030 |
| Normal(0,1) alternative | model Wald | 0.7244 ± 0.0063 | 0.7678 ± 0.0060 | 0.7738 ± 0.0059 | 0.7896 ± 0.0058 |
| Normal(0,1) alternative | median sandwich | 0.9390 ± 0.0034 | 0.9512 ± 0.0030 | 0.9564 ± 0.0029 | 0.9570 ± 0.0029 |
| Normal(0,1) alternative | exact sign | 0.9798 ± 0.0020 | 0.9538 ± 0.0030 | 0.9646 ± 0.0026 | 0.9570 ± 0.0029 |

The model Wald is materially anticonservative at small n, and still below
nominal at n=1000 under Laplace. Under the normal alternative its limiting
coverage is 2 Phi(z_.975*2/pi)-1 = 0.787878, since the fitted Laplace
scale tends to sqrt(2/pi) while the median variance is pi/(2n). The observed
0.7896 at n=1000 agrees with that separate calculation. Computing a model
Fisher variance therefore does not establish misspecification robustness.

The declared KDE rule oversmooths the Laplace cusp and yields conservative
sandwich coverage on this grid. Under the smooth normal alternative it is
close to nominal, with some finite-n error. The sign interval has the following
model-independent nonrandomized coverage, explaining its conservative rows:

| n | k | Exact nonrandomized sign coverage |
|---:|---:|---:|
| 10 | 2 | 0.97851562 |
| 30 | 10 | 0.95722605 |
| 100 | 40 | 0.96479980 |
| 1000 | 469 | 0.95370880 |

### Pareto shape, known and unknown endpoint

| Setting | Procedure | n=10 | n=30 | n=100 | n=1000 |
|---|---|---:|---:|---:|---:|
| a=0.7, b=2 | shape endpoint fitted | 0.9470 ± 0.0032 | 0.9538 ± 0.0030 | 0.9530 ± 0.0030 | 0.9502 ± 0.0031 |
| a=0.7, b=2 | shape endpoint known | 0.9456 ± 0.0032 | 0.9528 ± 0.0030 | 0.9516 ± 0.0030 | 0.9488 ± 0.0031 |
| a=3, b=2 | shape endpoint fitted | 0.9524 ± 0.0030 | 0.9448 ± 0.0032 | 0.9500 ± 0.0031 | 0.9480 ± 0.0031 |
| a=3, b=2 | shape endpoint known | 0.9510 ± 0.0031 | 0.9470 ± 0.0032 | 0.9496 ± 0.0031 | 0.9476 ± 0.0032 |

The a=.7 case has infinite original-data mean; the exact log-spacing pivot
needs no original-data moment assumption. It is distinct from fitting a
regular joint endpoint/shape model.

### Exponential rate and normal variance

| Setting | Procedure | n=10 | n=30 | n=100 | n=1000 |
|---|---|---:|---:|---:|---:|
| rate=2, origin=0 | exact rate | 0.9506 ± 0.0031 | 0.9450 ± 0.0032 | 0.9534 ± 0.0030 | 0.9476 ± 0.0032 |

| Setting | Procedure | n=10 | n=30 | n=100 | n=1000 |
|---|---|---:|---:|---:|---:|
| mean=2, sd=1.5 | variance mean fitted | 0.9492 ± 0.0031 | 0.9506 ± 0.0031 | 0.9512 ± 0.0030 | 0.9462 ± 0.0032 |
| mean=2, sd=1.5 | variance mean known | 0.9466 ± 0.0032 | 0.9518 ± 0.0030 | 0.9526 ± 0.0030 | 0.9466 ± 0.0032 |

### Uniform endpoints and width

| Setting | Procedure | n=10 | n=30 | n=100 | n=1000 |
|---|---|---:|---:|---:|---:|
| Uniform(0,1) | minimum both fitted | 0.9506 ± 0.0031 | 0.9474 ± 0.0032 | 0.9532 ± 0.0030 | 0.9484 ± 0.0031 |
| Uniform(0,1) | maximum both fitted | 0.9510 ± 0.0031 | 0.9492 ± 0.0031 | 0.9526 ± 0.0030 | 0.9516 ± 0.0030 |
| Uniform(0,1) | width both fitted | 0.9504 ± 0.0031 | 0.9450 ± 0.0032 | 0.9510 ± 0.0031 | 0.9528 ± 0.0030 |
| Uniform(0,1) | maximum minimum known | 0.9508 ± 0.0031 | 0.9494 ± 0.0031 | 0.9524 ± 0.0030 | 0.9514 ± 0.0030 |
| Uniform(0,1) | minimum maximum known | 0.9510 ± 0.0031 | 0.9462 ± 0.0032 | 0.9530 ± 0.0030 | 0.9484 ± 0.0031 |

Each row is a marginal interval under the declared known/fitted nuisance
setting; no simultaneous coverage claim follows from these rows.

### Triangular mode with known endpoints

| Setting | Procedure | n=10 | n=30 | n=100 | n=1000 |
|---|---|---:|---:|---:|---:|
| a=0, b=1, c=0.2 | mode Wald | 0.7944 ± 0.0057 | 0.8304 ± 0.0053 | 0.8864 ± 0.0045 | 0.9298 ± 0.0036 |
| a=0, b=1, c=0.2 | mode score chi2 | 0.9678 ± 0.0025 | 0.9682 ± 0.0025 | 0.9288 ± 0.0036 | 0.9460 ± 0.0032 |
| a=0, b=1, c=0.2 | mode LR chi2 | 0.9254 ± 0.0037 | 0.9288 ± 0.0036 | 0.9382 ± 0.0034 | 0.9456 ± 0.0032 |
| a=0, b=1, c=0.2 | mode LR simulated | 0.9430 ± 0.0038 | 0.9448 ± 0.0038 | 0.9480 ± 0.0038 | 0.9486 ± 0.0038 |
| a=0, b=1, c=0.5 | mode Wald | 0.8220 ± 0.0054 | 0.8508 ± 0.0050 | 0.8958 ± 0.0043 | 0.9400 ± 0.0034 |
| a=0, b=1, c=0.5 | mode score chi2 | 0.9812 ± 0.0019 | 0.9554 ± 0.0029 | 0.9424 ± 0.0033 | 0.9490 ± 0.0031 |
| a=0, b=1, c=0.5 | mode LR chi2 | 0.9282 ± 0.0037 | 0.9338 ± 0.0035 | 0.9378 ± 0.0034 | 0.9516 ± 0.0030 |
| a=0, b=1, c=0.5 | mode LR simulated | 0.9550 ± 0.0038 | 0.9526 ± 0.0038 | 0.9498 ± 0.0038 | 0.9552 ± 0.0038 |
| a=0, b=1, c=0.8 | mode Wald | 0.7840 ± 0.0058 | 0.8324 ± 0.0053 | 0.8960 ± 0.0043 | 0.9330 ± 0.0035 |
| a=0, b=1, c=0.8 | mode score chi2 | 0.9654 ± 0.0026 | 0.9648 ± 0.0026 | 0.9404 ± 0.0033 | 0.9514 ± 0.0030 |
| a=0, b=1, c=0.8 | mode LR chi2 | 0.9246 ± 0.0037 | 0.9280 ± 0.0037 | 0.9414 ± 0.0033 | 0.9482 ± 0.0031 |
| a=0, b=1, c=0.8 | mode LR simulated | 0.9446 ± 0.0038 | 0.9418 ± 0.0038 | 0.9528 ± 0.0038 | 0.9518 ± 0.0038 |

| Relative mode c | n | Simulated 95% LR critical |
|---:|---:|---:|
| 0.2 | 10 | 4.197162 |
| 0.2 | 30 | 4.290658 |
| 0.2 | 100 | 4.157274 |
| 0.2 | 1000 | 3.888530 |
| 0.5 | 10 | 4.489711 |
| 0.5 | 30 | 4.350149 |
| 0.5 | 100 | 4.257195 |
| 0.5 | 1000 | 3.945003 |
| 0.8 | 10 | 4.366236 |
| 0.8 | 30 | 4.279228 |
| 0.8 | 100 | 4.122373 |
| 0.8 | 1000 | 3.930313 |

Triangular Wald is not a reliable finite-n default on this grid, even at
n=1000. Unadjusted chi-square LR undercovers at small n; null-specific simulated
LR is much closer to nominal, with the reference error included above.
The score statistic also has discreteness effects: its count below null c is
Binomial(n,(c-a)/(b-a)); the current production API returns its model-information
statistic, not an exact binomial inversion.

A connected LR interval cannot simply be assumed. With known a=0,b=1 and five
observations each at .1 and .9, the two global mode maxima are .1 and .9, but
LR(.5)=10 log(25/9)=10.2165124753 exceeds the 95% chi-square(1) cutoff.
The accepted set has separated components; small distinct perturbations preserve
the strict rejection, so this is not an artifact confined to ties. The
[production replay](inference-5/topology.json) matches the algebra within 2e-12.
After building the launcher, reproduce with:

```sh
printf 'triang 0.5 10 .1 .1 .1 .1 .1 .9 .9 .9 .9 .9\n' | build/scratch/inference-5-probe
```

## Recommendations and TASK-102.6 availability handoff

Use the exact pivots under their declared models, and prefer the sign interval
for finite-sample median coverage. Keep Laplace model Wald and the fixed-rule
median sandwich explicit asymptotic choices with the observed finite-sample
caveats. For fully specified triangular nulls with known endpoints, prefer
null-specific simulated LR testing over the calibrated grid's poor Wald
performance; do not interpolate these 12 critical values into an asserted
continuous confidence interval.

All new availability below is **opt-in C++ only**. R inference integration is
outside this job; passing R regressions does not expose these procedures in R.

| Target / regime and nuisance | Available after this job | Precise limitation / open work |
|---|---|---|
| Laplace location; scale fitted, sigma>0 | Model Fisher Wald, root-n; calibrated | Finite-n undercoverage. Ordinary location Hessian unsuitable. No exact Fisher-Wald coverage claim. |
| Continuous unique median; scale irrelevant | Conservative exact sign/order-statistic interval | Atoms/ties require a separate convention/theory; no randomization. |
| Median under a continuous alternative, g(m)>0 | MAD-bandwidth density sandwich and root-n interval; Laplace and normal calibration | No uniform finite-sample guarantee or validated adaptive/bias-corrected bandwidth; zero MAD fails. |
| Pareto shape, a,b>0; b known | Exact chi-square pivot, 2n df | Model-only; not a robust shape procedure. |
| Pareto shape; b fitted as min X | Exact chi-square pivot, 2(n-1) df, n>=2 | Joint endpoint/shape remains non-DQM; no joint Wald covariance inferred. |
| Exponential rate; origin zero known | Exact chi-square interval, 2n df | Unknown origin is outside the contract. |
| Continuous uniform endpoints; both fitted | Exact marginal endpoint and width intervals, n>=2 | Rate n, non-DQM. No simultaneous-box guarantee. |
| Continuous uniform single endpoint; opposite known | Exact endpoint/translated width interval, n>=1 | Rate n, non-DQM. Known means genuinely fixed. |
| Normal variance; mean fitted or known | Exact chi-square interval, n-1 or n df | Normal-model claim; nonnormal variance inference needs a separate method and moments. |
| Triangular interior mode; endpoints known | Model Wald interval; pointwise model score/LR; independent simulated LR calibration | Wald undercoverage. General simulated-LR confidence-set API needs all components, null-specific calibration and controlled simulation error. A single connected LR interval is disproved above. |
| Triangular mode at a or b, or endpoints fitted | No new procedure | Interior information/limit argument does not cover these regimes. Boundary-mode code needs a separate one-sided analysis and calibration; free endpoints have divergent squared scores. |
| Triangular mode under misspecification | No robust procedure | Sensitivity must include the kink and true g(c), may vanish or change sign; density/sensitivity estimation, identification and calibration remain unvalidated. |

The robust triangular sensitivity candidate is stated in the canonical note;
its algebra is not a validated robust procedure. The open entries concern
regimes or interfaces beyond the implemented/calibrated hypotheses, not
unfinished requested interior calibration.

## Final validation and reproduction

Every build/test ran as a foreground command through its own four-slot lock,
with nice level 10 and BLAS/OpenMP threads pinned to one. No simbox, agents,
package installs, Git writes or board edits were used. TMPDIR, JUST_TEMPDIR
and XDG_RUNTIME_DIR were set to `$PWD/build/tmp`; scratch/logs are in
`build/scratch/`. The calibration stage wall time was 10m24s including the
shared-slot wait; one streaming worker computed the cells, within the stated
one-hour/four-core ceiling. Monte Carlo used the default compiler arithmetic;
no-long-double coverage here is deterministic test coverage, not a second
Monte Carlo experiment.

| Final command (each prefixed `/tmp/umvl-cpu/run4`) | Result |
|---|---|
| `just cpp-test` | 105/105 dev shards passed |
| `just cpp-test-no-long-double` | 126/126 shards passed |
| `bash dev/validation/inference-5.sh --reps 5000 --reference-reps 10000` | 40 cells, 120 rows, zero procedure failures; independent comparisons passed |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19843 |
| `env UMVL_TEST_TIER=full just cpp-test` | 126/126 full shards passed |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41289 |

R dev skips are the declared full-tier exclusions plus the unavailable optional
`sn` comparison; R full has only that optional-package skip. New doctests add
no skip or permitted-failure annotations. CTest reports shard status and does
not expose successful-case assertion totals. Existing compiler/roxygen
messages are not a warning-free build claim; the table's WARN counts are
specifically testthat results. During implementation two compile-only errors
(an unavailable binomial seam name and a mixed-type test initializer) were
corrected before these final green runs. No failed numerical calibration cell
was excluded. Tests/fixtures were not regenerated to change expected estimates.

Environment: Linux-7.0.0-38-generic-x86_64-with-glibc2.43; c++ (Ubuntu 15.2.0-16ubuntu1) 15.2.0;
R version 4.6.1 (2026-06-24); BH 1.90.0.1; Python
3.14.5, NumPy 2.4.6,
SciPy 1.18.0. Counts belong to the tested bfdf257
source tree plus the recorded changes, rather than the older 14598b7 baseline.
The retained record has full hashes and stage timestamps; no commit was made.

To reproduce all six foreground runs, their logs and the report, run
`bash dev/validation/inference-5-check.sh` from the repository root. The
[controller](../../validation/inference-5-check.sh) acquires each reservation
itself; do not wrap the controller in another slot lock. To regenerate only
the report from existing green logs, use
`/tmp/umvl-cpu/run python3 dev/validation/inference-5-report.py`.
The launcher reuses cells only when source hashes, seed and workloads match;
`--reset` forces deterministic regeneration. The postprocessor refuses to
write this report unless all six stages passed and retained calculation-source
hashes still match. The final source identity file additionally records the
current algorithm note and report generator.
