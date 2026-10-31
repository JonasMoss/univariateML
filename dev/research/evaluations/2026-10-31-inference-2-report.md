# TASK-102.2: scores and information for normal, gamma, beta and Poisson

Completed 2026-10-31 in `codex/inference-2`, based on
`24222f6411edc7fbad6e4e2594f46f12211ef289`, with the uncommitted additions
listed below. No commits, board edits, installs, fitter changes, R/JS exposure,
d/p/q/r changes, special-function changes or golden regeneration were made.

The implementation follows the [inference plan](../../roadmap/2026-10-30-inference-and-dqm.md)
and the [conditional classification](2026-10-31-inference-1-report.md).
It introduces the shared [contract](../../../univariateML-cpp/include/cppunivariate/core/inference.hpp),
four `distributions/<id>/score.hpp` headers, aggregator includes, appended
family doctests, and the reproducible [independent validator](../../validation/inference-2.py).
The family implementation headers are
[normal](../../../univariateML-cpp/include/cppunivariate/distributions/norm/score.hpp),
[gamma](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/score.hpp),
[beta](../../../univariateML-cpp/include/cppunivariate/distributions/beta/score.hpp), and
[Poisson](../../../univariateML-cpp/include/cppunivariate/distributions/pois/score.hpp).

**Contract.** `norm_inference`, `gamma_inference`, `beta_inference` and
`pois_inference` take observations, every parameter value, a free mask, and
an optional score-row callback. All take `span`; Poisson also accepts `ispan`
to match its fitter. Natural coordinates mean the README parameters, not
exponential-family canonical coordinates. Parameter order is `(mean, sd)`,
`(shape, scale)`, `(shape1, shape2)` and `(lambda)`, respectively.

Mask bit i selects parameter i as free; the default selects all parameters.
Every subset is supported, including zero free parameters. Held coefficients
are supplied as finite parameter values, removed from the score vector and
matrix, and treated as known constants. All cross terms among free coefficients
are retained. Estimating a nuisance coefficient requires keeping it in the free
vector; removing it does not profile or account for its estimation.

`InferenceResult` records parameter names/count, the free mask, free indices in
README order, dimension, observation count, natural coordinates, total
normalization and validity conditions. Score entries occupy the first p array
elements; matrices occupy the first p*p elements, packed row-major. Parameter
names occupy the first `parameter_count` elements. Storage currently supports
at most two parameters, deliberately matching this initial regular set.

For independent observations, the returned quantities are exactly

$$S=\sum_i s_i,\qquad J=\sum_i s_i s_i^T,\qquad
H=\sum_i h_i=-\sum_i\nabla^2\ell_i,\qquad F=nI_1.$$

There is no division by n. J is not the outer product of S. H is the observed
information at the supplied point, including points away from an MLE; it may
be indefinite. F is model Fisher information, not estimating-equation
sensitivity under misspecification. No optimizer inverse Hessian is used.

The callback receives each free-coordinate observation score in input order.
It can copy rows into caller-owned storage or accumulate without storing an
n-by-p matrix. The implementation uses one reusable row vector of length p
and fixed-size aggregate arrays. For example:

```cpp
std::vector<double> rows;
const auto result = cppunivariate::gamma_inference(x, 2.0, 3.0, 3,
    [&](const std::vector<double>& row) {
      rows.insert(rows.end(), row.begin(), row.end());
    });
```

Callbacks must not throw. A failed call may already have emitted a prefix;
consume rows only after checking overall status. Empty samples, unknown mask
bits, invalid parameters, invalid observations and nonfinite arithmetic have
explicit statuses. No exception, raw-pointer interface, `using` declaration,
printing, inverse or pseudoinverse is added to the library.

Matrix statuses are separate from overall calculation status. A singular
empirical matrix does not destroy a valid score. `opg_status`,
`observed_status` and `fisher_status` report numerical singularity and ill
conditioning. A zero scalar or exactly zero scaled determinant is singular;
a PSD matrix with a nonpositive diagonal is also flagged. For two coordinates,
absolute scaled determinant at most 64 machine epsilons is ill conditioned.
Scaling divides all entries by their maximum absolute value. This is a
coordinate-dependent numerical diagnostic, not a certified condition number
or mathematical rank theorem. Matrix `Ok` does not establish positive
definiteness. Nonfinite score or matrix arithmetic makes overall status
`NumericalFailure`; numerical values from an overall failed call are unusable.
Finite-precision rank loss is visible even when model information is
mathematically positive definite.

**Implemented formulas.** The following are per-observation scores s,
negative log-density Hessians h, and model information I1. The public call
sums them over the data and projects onto the requested free subset.
Write psi for digamma and psi1 for trigamma.

For normal parameters `(mean, sd)=(m,t)`, let z=(x-m)/t:

$$s=\begin{pmatrix}z/t\\(z^2-1)/t\end{pmatrix},\qquad
h=\frac1{t^2}\begin{pmatrix}1&2z\\2z&3z^2-1\end{pmatrix},\qquad
I_1=\frac1{t^2}\begin{pmatrix}1&0\\0&2\end{pmatrix}.$$

Require finite m, finite t>0, and finite real observations.

For gamma parameters `(shape, scale)=(a,t)`:

$$s=\begin{pmatrix}\log x-\log t-\psi(a)\\x/t^2-a/t\end{pmatrix},\qquad
h=\begin{pmatrix}\psi_1(a)&1/t\\1/t&2x/t^3-a/t^2\end{pmatrix},\qquad
I_1=\begin{pmatrix}\psi_1(a)&1/t\\1/t&a/t^2\end{pmatrix}.$$

Require finite a,t>0 and finite x>0. The scale parameter is the README scale,
not the historical rate parameter.

For beta parameters `(shape1, shape2)=(a,b)`, write d=psi(a+b), q=psi1(a+b):

$$s=\begin{pmatrix}\log x-\psi(a)+d\\\log(1-x)-\psi(b)+d\end{pmatrix},\qquad
h=I_1=\begin{pmatrix}\psi_1(a)-q&-q\\-q&\psi_1(b)-q\end{pmatrix}.$$

Require finite a,b>0 and 0<x<1. The implementation uses `log1p(-x)`.

For Poisson parameter lambda>0:

$$s=x/\lambda-1,\qquad h=x/\lambda^2,\qquad I_1=1/\lambda.$$

Require finite positive lambda and nonnegative integer observations. The
attained zero-rate model is excluded from this interior derivative contract,
even with all coefficients held. All-zero observations at a positive lambda
are valid: observed information is singular, while model Fisher information
is positive. This case has a dedicated doctest.

These formulas follow by differentiation of the dossier log densities and
standard moment identities. DQM, finite model information and full model rank
at these interior points have the exponential-family argument in the linked
plan: sufficient-statistic covariance is locally finite and nonsingular, and
the map to README coordinates has nonsingular Jacobian. Normal and Poisson
information are explicitly positive; gamma uses a*psi1(a)>1; beta uses the
linear independence of log X and log(1-X) modulo constants. Every nonempty
held submodel retains a positive principal information block. These facts are
separate from sample OPG rank, observed-Hessian definiteness, estimator
consistency, estimator asymptotics and misspecification robustness. Numerical
checks below are evidence at finite points, not proofs of these theorems or
uniform floating-point accuracy.

**Validation and final runs.** All builds and tests ran locally under the
shared slot locks, with `nice -n 10`, at most four cores, and BLAS/OpenMP pinned
to one thread by the lock wrappers. No simbox was used. Environment: Linux
7.0.0-38-generic x86_64, GCC/G++ 15.2.0, C++17, Boost 1.90 (BH 1.90.0-1),
R 4.6.1, Rcpp 1.1.2, Python 3.14.5, mpmath 1.3.0 and SymPy 1.14.0.

Reproduction commands, from this worktree:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run4 nice -n 10 env CMAKE_BUILD_PARALLEL_LEVEL=4 just cpp-test
/tmp/umvl-cpu/run4 nice -n 10 env CMAKE_BUILD_PARALLEL_LEVEL=4 just cpp-test-no-long-double
/tmp/umvl-cpu/run4 nice -n 10 python3 dev/validation/inference-2.py
/tmp/umvl-cpu/run4 nice -n 10 just test
```

The validator uses `Boost_INCLUDE_DIR`/`BOOST_INCLUDE_DIR` when supplied, or
reads the plain include path from the configured CMake cache. It compiles a
small C++ probe against production headers; no R linkage is involved.

| Final check | Result |
|---|---|
| `just cpp-test` | 105/105 dev shards passed; 0 failures; 35.61 s test execution |
| `just cpp-test-no-long-double` | 126/126 shards passed, including full/slow cases; 0 failures; 105.44 s test execution |
| Independent validator | All 112 parameter/subset cases passed, seed 20261031 |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19842; 78.988 s R test execution |
| `git diff --check` | Passed |

Build/lock waiting time is additional to the reported test times. R skips are
full-tier cases. The R full tier was not run: this job changes neither fitting,
d/p/q/r nor the test harness infrastructure. The no-long-double recipe itself
runs the full C++ suite. Existing unrelated compiler indentation warnings do
not represent test failures. Initial iteration caught and fixed a nonexistent
Poisson test helper and missing explicit Boost include discovery in the
validator; all final commands above exited zero. No failures were permitted.

Family doctests exercise all 14 hold masks (4 each for normal/gamma/beta, 2 for
Poisson), including empty free vectors, on five-observation samples. They
check callback row sums against S, row outer products against J, projection
against the full matrices, doubling with duplicated observations, invalid
parameters/data/masks, empty samples, arithmetic failure, singular OPG and
valid indefinite H. Existing fitters are called with the corresponding holds;
every free total-score component at their successful MLE is below 1e-7
(iterative fitter tolerance 1e-12, maxit 2000). This is a local numerical
stationarity check on these samples, not a new global fitter guarantee.
Central differences of the production C++ log densities use step 1e-4 and
doctest relative tolerance 2e-6 for scores, Hessian diagonals and cross terms.

The independent validator draws eight seeded interior points per family and
checks every mask, totaling 112 calls. Non-normal positive parameters are
uniform in [1.2,5]; normal mean is in [-2,2], sd in [0.7,2.5]. Observations
are finite interior points (Poisson counts 0 through 11). It parses each
README `log_density` with SymPy and evaluates derivatives at 45-digit mpmath
precision. Central differences of that density use coordinate steps
1e-5*max(1,abs(parameter)). For every full point it integrates the squared
score and cross products directly: normal on the real line, gamma on the
positive half line, beta on (0,1), and Poisson by summing counts 0 through
179. The omitted Poisson tail is negligible at this parameter range; this
finite sum is numerical expectation evidence, not a general tail algorithm.

Errors below are scaled by 1+absolute reference value:

| Comparison | Acceptance tolerance | Maximum observed scaled error |
|---|---:|---:|
| Score vs symbolic derivative | 2e-13 | 2.256e-16 |
| Observed information vs symbolic derivative | 2e-13 | 2.133e-16 |
| Observed information vs high-precision central difference | 2e-8 | 2.346e-10 |
| OPG vs independent score products | 3e-13 | 3.300e-16 |
| Fisher vs integrated/summed score products | 1e-12 | 7.114e-17 |
| Gamma coordinate transformation | 2e-13 | 3.167e-16 |

For gamma rate r=1/t, the parameter Jacobian from (a,r) to (a,t) is
D=diag(1,-1/r²). The validator checks the score rule D-transpose*s, Fisher
congruence D-transpose*I*D, and the arbitrary-point observed-information rule
D-transpose*h*D minus s_scale*2/r³ in the rate-rate entry against independent
rate-density derivatives. Thus the observed-Hessian check does not silently
apply a congruence rule that is only sufficient at stationarity.

Detailed logs are under `build/scratch/`: `cpp-dev-final.log`,
`cpp-no-long-double.log`, `validation-final.log` and `r-dev.log`. The validator
also writes `inference-2/results.json` with dependency versions, source revision
and SHA-256 hashes of the five implementation headers. These scratch files
are regenerable; the essential commands, results and limitations are retained
here. Goldens and preexisting test cases are unchanged; only cases appended to
the four family `test.cpp` files are added.

**Next work for TASK-102.3.** This job completes the requested independent
checks for the four-family pilot. Open follow-up: extend parameter ranges to
small shapes, highly concentrated/imbalanced beta models, extreme units and
large counts; characterize cancellation and numerical rank diagnostics there.
There are no certified uniform error bounds. Extend the two-parameter result
storage before adopting it for larger families. Recover other derivative
candidates only after checking current parameter names, objective totals,
profile semantics and free subsets. Distinguish pointwise Hessians from model
information and nonsmooth sensitivity. Neither shared symbolic provenance nor
a finite grid independently verifies a model definition or supplies a theorem
for the next family.

**Next work for TASK-102.4.** No covariance or statistical test procedure is
implemented. It needs checked positive-definite/conditioning-aware linear
solves and explicit singular/failure propagation; correct totals for model
covariance and H-inverse*J*H-inverse-transpose; parameter/contrast delta-method
handling; and nuisance-aware target inference. Inverting a target block while
fitting nuisance treats nuisance as known. Establish consistency, estimator
expansions, moment and sensitivity assumptions separately for model-based and
misspecification-robust procedures. Validate coverage and type-I error under
explicit hypotheses, with Monte Carlo uncertainty, sample sizes, seeds and
failure accounting. Useful pilot targets remain normal mean with variance
known/fitted, gamma mean with shape fitted, beta mean with concentration
fitted, and positive Poisson rate. Exact normal/Poisson procedures, boundaries,
nonregular cases and bootstrap validity require their own contracts and
calibration. Successful derivative checks establish none of these properties.
