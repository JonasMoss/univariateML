# Covariance for regular families with moving joins

TASK-102.15, 2026-11-07. Completed in `codex/infer-joins`, based on
`681d7ac5ce6965db4066b4117c12eff69e75cd0c`, with uncommitted changes identified
by the [final source manifest](2026-11-07-infer-joins-data/source-sha256.json).
All implementation, derivative checks, calibration and final required suites
finished before this report was written.

## Delivered scope

Observed and sandwich covariance now use analytic natural-coordinate scores
and a.e. Hessians in six families. The R and JavaScript interfaces reach the
same C++ dispatch and retain their fit-status, restriction and matrix-condition
checks.

| Family | Enabled scope | Exclusions |
|---|---|---|
| snorm | Finite mean, sd > 0, finite xi > 0; every free subset, including free skew at xi=1 | Half-normal and other limit fits; singular or ill-conditioned numerical solves |
| sstd | Finite mean, sd > 0, finite nu > 2, finite xi > 0; every free subset, including free skew at xi=1 | nu=2, skew-normal/half-family limits and other failed fits |
| Huber | Finite mu, sigma > 0, finite epsilon > 0; every free subset, including epsilon fitted | Normal and Laplace limits |
| PIG | Finite mean > 0 and shape > 0; either or both coefficients free | Zero point mass, Poisson and infinite-mean faces |
| GPD | Known finite threshold, sigma > 0, xi > -1/2; sigma and/or xi free, including free xi=0 | Free threshold and critical/Poisson endpoint regimes |
| GEV | Finite mu, sigma > 0, xi > -1/2; every supported free subset, including free xi=0; free xi < 1 | Critical/Poisson endpoint regimes and the active upper fitted shape bound |

The four principal families and both optional GPD/GEV expansions are enabled
only after their checks passed. Tukey lambda remains explicitly unavailable;
its concrete remaining work is recorded below. No new Fisher-information
implementation, likelihood-ratio procedure or nonregular endpoint procedure is
claimed.

No fitting code, default fits, goldens, `dpqr.hpp`, `special/`, truth files,
R code-generation scripts, generated authored d/p/q/r, roadmap or prohibited
status files changed. No package/dependency installation, sub-agent, board edit,
commit, push or publication was performed. The prescribed R test workflow built
its temporary test package. The board was read at session start. Work stayed in
this worktree, with authorized simbox mirrors for simulations and long suites.

## Derivatives and mathematical meaning

The implementation is
[`core/inference_join.hpp`](../../../univariateML-cpp/include/cppunivariate/core/inference_join.hpp),
with the formulas, selection conventions, approximation bounds and computational
tradeoffs preserved in the
[shared inference note](../../../univariateML-cpp/include/cppunivariate/core/inference.md#analytic-derivatives-at-moving-c1-joins).
It uses second-order analytic chain rules; it does not perturb parameters to
estimate derivatives. The output is the total natural-coordinate score,
`H = -sum Hessian(log density)` and observation OPG
`J = sum score_i score_i^T`, with free coordinates selected in README order.
Observed covariance is `H^-1`; sandwich covariance is `H^-1 J H^-T`, without an
extra sample-size factor. Held coefficients are known constants. Fitted nuisance
parameters remain in the full free matrix.

Snorm and sstd differentiate the selected two-piece density, including the
shape-dependent centering and standardization. At an exactly represented join,
the right-hand continuation supplies the Hessian. Huber differentiates its loss
piece together with the normalizer and its first two epsilon derivatives; its
exact-join convention selects the central continuation. Scores agree between
pieces; second derivatives can differ. These are explicit one-sided null-set
conventions. They require the usual sensitivity assumptions for rounded or
atomic data at joins.

PIG differentiates the exact positive mass-ratio recurrence in log space. Its
reference checker instead uses the inverse-Gaussian mixture's Bessel-K formula.
The production recurrence uses constant storage and work proportional to the
sum of observed counts. It returns numerical failure for counts outside the
integer-loop range or nonfinite arithmetic. Large counts may therefore cost
more than the existing asymptotic density evaluation; no truncated recurrence
or unchecked asymptotic derivative is returned.

For GPD/GEV, `h = z*L(xi*z)` with `L(t)=log1p(t)/t` retains the shape score and
curvature at xi=0. A degree-14 expansion for |t| < .01 avoids cancellation;
its exact-arithmetic value and derivative remainder bounds are recorded in the
shared note. Otherwise the log1p quotient is used directly. Support checks
precede differentiation. These bounds concern truncation, with floating-point
accuracy checked numerically; no outward-rounded enclosure is claimed.

The chain rules, selected-piece algebra and recurrence identities concern exact
arithmetic. The DQM, finite full-rank information and integrable a.e. Hessian
justification comes from the earlier
[moving-feature](2026-11-04-infer-moving-report.md),
[two-piece](2026-11-04-dqm-b-report.md) and
[discrete-family](2026-11-04-dqm-c-report.md) analyses. This job supplies
implementation and finite numerical evidence. It does not prove global fitter
optimality, uniform accuracy or estimator asymptotics throughout a regime.

An initial C++ run exposed last-bit asymmetry between mixed chain-rule entries.
The implementation now averages the two accumulated cross entries to satisfy
the existing exact-symmetry API contract. Join selection remains one-sided.
The initial GPD projection test also needed packed-index correction, and initial
R tests needed explicit `epsilon=NULL` and a known GPD threshold. All corrected
final runs passed; no failed assertion was accepted as a permitted failure.

## Independent derivative validation

The [reproducer](2026-11-07-infer-joins-data/check-derivatives.py) and
[results](2026-11-07-infer-joins-data/derivatives.json) use 80-digit mpmath
scalar differentiation in natural coordinates. The reference freezes the
appropriate analytic continuation at exact joins. Tests include both sides at
distances down to 1e-9, exact symmetric snorm/sstd joins, exact Huber joins,
sstd nu=2.15, 5 and 40, skew=0.35, 1 and 2.4, varied PIG mean/shape and counts
through 200, free GPD/GEV shape zero and ±1e-8, and negative-shape observations
within relative distance 1e-8 of an upper endpoint. Representative nonempty
held subsets are included. There are 307 checked parameter/observation/subset
requests.

Errors below are `abs(actual-reference)/(1+abs(reference))`. Acceptance
thresholds are 2e-8 for scores and 2e-7 for observed Hessians.

| Family | Requests | Maximum score error | Maximum Hessian error |
|---|---:|---:|---:|
| snorm | 31 | 2.28e-15 | 5.05e-15 |
| sstd | 127 | 7.27e-14 | 4.86e-14 |
| huber | 33 | 7.11e-17 | 1.23e-16 |
| pig | 31 | 4.12e-14 | 1.37e-13 |
| gpd | 32 | 3.99e-09 | 7.97e-09 |
| gev | 53 | 3.99e-09 | 7.97e-09 |

The [special-family validator](../../../dev/validation/inference-numeric-special.py)
also passes all 13 families and three additional check groups, including golden
total scores, the expanded all-free snorm/sstd/GEV scopes, conditional charts and
24 structural-singularity cases. Its
[retained results](2026-11-07-infer-joins-data/special-results.json) include source
hashes, the 40-digit reference settings and tolerances. Existing score/Hessian
thresholds of 2e-6 / 2e-5 and golden total-score threshold 1e-3 were preserved.
C++ projection tests exercise every supported mask; R and JS checks exercise
public covariance, held subsets, free symmetry, negative/zero shape and critical
regime rejection. Manual parameter-point interface tests establish dispatch and
packing; calibration below uses genuine refits.

## Fitted-covariance calibration

The resumable [driver](2026-11-07-infer-joins-data/calibrate.py) and
[C++ workload](2026-11-07-infer-joins-data/calibrate.cpp) ran on simbox:
300 replications for each of 12 truth configurations at n=200, 800 and 3200,
10,800 attempts in total. All nuisance parameters were refitted, including sstd
nu/skew and Huber epsilon. GPD alone holds its threshold at zero. Location is
zero and scale one where not otherwise stated in the table. Core fits use
`tol=1e-10`, `maxit=2000`, and must return `Ok`; both covariance statuses must
also be `Ok`. Each replication initializes `mt19937_64` with
`20261107 + 100000*tag + 1000*n + rep`, with tags and sampling code retained in
the workload.

The [full summary](2026-11-07-infer-joins-data/calibration.json) retains failures,
conditional and unconditional coverage, Wilson intervals, empirical estimator
SDs and mean standard errors. Intervals are natural-coordinate normal Wald
intervals with cutoff 1.959963984540054. Failures count as noncoverage in the
reported unconditional rates. Standard-error/SD summaries condition on usable
replications and are not substituted for coverage.

Each coverage cell below gives the range over free coordinates, in percent:
**observed / sandwich**. Success counts are ordered n=200 / 800 / 3200, each
out of 300.

| Truth configuration | Usable counts | n=200 | n=800 | n=3200 |
|---|---|---|---|---|
| snorm, xi=1.5 | 300 / 300 / 300 | 90.3–97.7 / 90.0–97.7 | 93.3–96.0 / 93.3–96.0 | 93.0–95.7 / 93.0–96.3 |
| snorm, xi=1 | 300 / 300 / 300 | 93.7–94.7 / 93.3–94.7 | 95.3–96.7 / 95.3–96.7 | 94.0–96.7 / 94.0–96.3 |
| sstd, nu=5, xi=1.5 | 297 / 300 / 300 | 92.7–96.3 / 90.3–94.0 | 94.3–96.3 / 94.3–96.3 | 94.3–95.7 / 93.0–95.7 |
| sstd, nu=5, xi=1 | 294 / 300 / 300 | 93.0–94.3 / 91.3–93.3 | 94.0–95.7 / 94.0–95.7 | 93.3–96.0 / 93.0–96.0 |
| Huber, epsilon=1 | 295 / 300 / 300 | 84.0–88.3 / 75.3–88.7 | 93.3–93.7 / 91.0–93.0 | 94.3–96.3 / 93.7–96.0 |
| PIG, mean=2, shape=3 | 300 / 300 / 300 | 91.7–95.3 / 90.7–94.7 | 95.0–95.3 / 95.0–95.0 | 96.3–96.7 / 96.0–96.7 |
| GPD, xi=-0.25 | 300 / 300 / 300 | 93.0–95.7 / 88.7–96.0 | 92.0–95.0 / 89.3–95.3 | 96.7–96.7 / 96.0–96.3 |
| GPD, xi=0 | 300 / 300 / 300 | 95.0–95.7 / 92.7–93.7 | 93.3–94.0 / 92.3–92.7 | 94.3–94.7 / 94.0–94.7 |
| GPD, xi=0.25 | 300 / 300 / 300 | 95.7–97.0 / 93.3–96.7 | 94.7–96.3 / 95.0–96.0 | 94.7–96.7 / 95.0–96.3 |
| GEV, xi=-0.25 | 300 / 300 / 300 | 93.0–95.7 / 88.3–95.0 | 94.3–96.3 / 92.3–96.3 | 95.7–96.7 / 95.0–96.0 |
| GEV, xi=0 | 300 / 300 / 300 | 93.3–96.3 / 93.0–96.3 | 93.3–95.3 / 92.7–95.7 | 93.7–95.3 / 94.0–95.3 |
| GEV, xi=0.25 | 300 / 300 / 300 | 93.0–94.7 / 91.3–93.3 | 93.0–95.7 / 93.7–95.7 | 94.3–96.3 / 94.3–96.3 |

There were 10,786 usable attempts. At n=200, sstd selected the skew-normal face
seven times and had two ill-conditioned covariance solves; Huber selected the
normal face four times and the Laplace face once. No failed optimizer iterate
was accepted. Every n=800 and n=3200 attempt had usable fitted covariance.

The [completion diagnostic](2026-11-07-infer-joins-data/check-calibration.py)
requires zero failures at n=3200 and inclusion of nominal 95% in every marginal
95% Wilson interval there. All 68 checks passed
([record](2026-11-07-infer-joins-data/calibration-check.json)). At that sample
size the marginal rates span 93.0%–96.7%. This is a pointwise diagnostic with
300 replications per configuration, rather than a simultaneous or uniform
coverage guarantee.

**Small-sample caveat:** Huber with epsilon fitted is poorly calibrated at
n=200: observed scale coverage is 84.0%, sandwich scale coverage is 75.3%, and
sandwich epsilon coverage is 78.7%. Negative-shape GPD/GEV sandwich coverage
also falls below 90% for a coordinate at n=200. Availability is not a promise of
accurate small-sample Wald intervals. These results do not validate every held
subset, extreme parameter value or near-critical endpoint. Both methods were
calibrated under the model; no misspecification coverage claim is made.
Sandwich use continues to require finite score variance, nonsingular sensitivity,
an identifiable interior KL target and an estimating-equation expansion.

## Consumer agreement and final suites

The dossier scopes, C++ dispatch and generated availability catalog agree.
The catalog has 353 procedure rows, including 195 covariance scopes: 121
implemented, four unreviewed and 70 invalid for this procedure. Consolidating the
old symmetric snorm/sstd entries reduces the total row count. Mathematical
classification links remain separate from the new implementation and coverage
evidence. The site checker now resolves a named calibration record within the
multi-configuration JSON while continuing to validate historical evidence.

[Final machine-readable results](2026-11-07-infer-joins-data/test-results.json):

| Final check | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 dev shards passed |
| `just cpp-test-no-long-double` | simbox | 129/129 full shards passed |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21049 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43869 |
| Targeted public R vcov | simbox | 129 assertions passed, no failures/warnings/skips |
| `just js-build` | workstation | green |
| `just js-test` | workstation | 442/442 passed, zero failures/skips |
| Targeted JS/native covariance | workstation | 17/17 passed |
| `just site-check` | workstation | green; 99 C++17 examples compile/link; 353-row inference catalog and browser checks |
| Independent join/endpoint derivatives | workstation | 307/307 requests passed |
| Special derivative validator | simbox | all 16 family/check groups passed |
| Registry, availability `--check`, R metadata | workstation | green; 66-entry registry unchanged and current |
| Calibration accounting and source identity | workstation | green; final derivative and simulation source hashes match |

The dev skips require the full tier; the full tier has no skips. Simbox did not
require exit-75 fallback. All builds, tests and numerical workloads used the
supplied slot wrappers, with their `nice -n 10` and BLAS/OpenMP settings. The
final source-record command waited about 17 minutes for a local slot and was
left in the foreground. R is 4.6.1; C++ uses C++17 and GCC 15.2.0; the workstation
reference uses Python 3.14.5 and mpmath 1.3.0. The site checker used installed
R 4.5 BH headers; R/ordinary native checks used installed R 4.6 BH headers.
Existing compiler/roxygen/shell-startup messages appeared; the testthat WARN
counts above are zero. Durable counts, tolerances and hashes are linked here;
full stdout logs remain under ignored `build/scratch/infer-joins-*.log`.

Reproduction from this worktree (remote result JSON must be copied back before
another mirror sync can replace it):

```sh
mkdir -p build/tmp build/scratch
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-infer-joins-data/check-derivatives.py
/tmp/umvl-cpu/simbox-run python3 dev/research/evaluations/2026-11-07-infer-joins-data/calibrate.py
rsync -a -e "ssh -F $HOME/.ssh/config -o BatchMode=yes" simbox:umvl-wt/infer-joins/dev/research/evaluations/2026-11-07-infer-joins-data/calibration.json dev/research/evaluations/2026-11-07-infer-joins-data/
/tmp/umvl-cpu/simbox-run env UMVL_SOURCE_REVISION=681d7ac5ce6965db4066b4117c12eff69e75cd0c python3 dev/validation/inference-numeric-special.py
rsync -a -e "ssh -F $HOME/.ssh/config -o BatchMode=yes" simbox:umvl-wt/infer-joins/build/scratch/inference-numeric-special/results.json dev/research/evaluations/2026-11-07-infer-joins-data/special-results.json
/tmp/umvl-cpu/simbox-run just cpp-test
/tmp/umvl-cpu/simbox-run just cpp-test-no-long-double
/tmp/umvl-cpu/simbox-run just test
/tmp/umvl-cpu/simbox-run just test-full
/tmp/umvl-cpu/run4 just js-build
/tmp/umvl-cpu/run just js-test
/tmp/umvl-cpu/run4 just site-check
/tmp/umvl-cpu/run python3 dev/validation/inference_availability.py --check
/tmp/umvl-cpu/run python3 dev/validation/inference-family-registry.py
/tmp/umvl-cpu/run Rscript dev/validation/inference-numeric-metadata.R
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-infer-joins-data/check-calibration.py
```

## Open procedure work

**Tukey lambda:** no adapter was enabled. Inspection of its log-tail inversion
and the existing moving-feature analysis identifies the missing inference path:
a residual-checked inverse and analytic implicit first/second derivatives of
`mu + sigma*Q_lambda(u) = x`, followed by derivatives of
`-log(sigma)-log(Q_lambda'(u))`. Endpoint proximity, lambda=0 and the fitted
lambda=-1 restriction need explicit handling. Independent high-precision checks
and fitted-nuisance calibration for lambda < 1/2 are still missing. Reusing the
nested density stencil would propagate inverse-solver error and cross support;
it was not adopted. This concrete gap is also preserved in the shared note.

No change to settled d/p/q/r, the special-function seam or truth data was needed
for the six delivered adapters. A future Tukey implementation should provide an
inference-only inverse certificate or explicitly scope any required density
change. Critical/Poisson endpoint inference, active-bound inference, global
uniform coverage and misspecification calibration remain separate work.

## Report presentation

The prescribed artifact-pane `show` command returned HTTP 403 because this
worktree is outside the viewer's configured readable roots. The report and its
linked evidence remain at their worktree paths; no viewer configuration or file
outside this worktree was changed.

Final checks passed for all 16 local report links, every recorded source hash,
the forbidden-file scope and whitespace. All started commands had finished.

The site theory-browser check regenerated one historical screenshot under
`2026-11-02-viz-clip/` as a test side effect. Its original HEAD bytes were restored
afterward, preserving that earlier report's evidence. It is excluded from this
job's source manifest. No numerical source changed during this cleanup.
