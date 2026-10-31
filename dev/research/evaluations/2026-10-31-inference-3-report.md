# TASK-102.3: inference contract hardening

Completed 2026-10-31 on branch `codex/inference-3`, source revision
`2e698164ac9e3bf222ecf425d67580f9d2c8c846`, with the uncommitted changes
identified by the hashes below. No commits, board edits, installs, simbox use,
fitter changes, R/JS source changes, golden changes or truth regeneration were
made. All builds, tests and numerical validation used the shared workstation
slot locks, with at most four cores and BLAS/OpenMP pinned to one thread.

This is the shared numerical implementation record for
[core/inference.hpp](../../../univariateML-cpp/include/cppunivariate/core/inference.hpp)
and the four score headers:
[normal](../../../univariateML-cpp/include/cppunivariate/distributions/norm/score.hpp),
[gamma](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/score.hpp),
[beta](../../../univariateML-cpp/include/cppunivariate/distributions/beta/score.hpp)
and [Poisson](../../../univariateML-cpp/include/cppunivariate/distributions/pois/score.hpp).
It extends the [TASK-102.2 record](2026-10-31-inference-2-report.md) and follows
the [canonical inference plan](../../roadmap/2026-10-30-inference-and-dqm.md).
The reproducible validator is
[inference-3.py](../../validation/inference-3.py); inference-2.py remains unchanged.

## Storage and compatibility

The result now supports eight parameters with fixed arrays: eight names,
indices and score entries, and 64 entries for each matrix. Only the first
`dimension` score entries and first `dimension*dimension` matrix entries
are active; matrices remain packed row-major, with the same free-index mapping.
`inference_max_parameters` states the capacity. Models exceeding it return
`DimensionExceeded` before accessing model methods or forming a bit shift;
a compile-time branch also permits testing rejection with a nine-parameter
model that has no observation implementation.

The four family functions retain their signatures, defaults, masks, README
coordinates, total normalization and vector callback interface. Result array
capacities have increased; consumers must use the active dimension rather
than assume the old physical array length. The default discard path now uses
a stack row array. Existing vector callbacks retain one reusable vector,
allocated once per call, outside the observation loop. Metadata strings can
still allocate once per call. No observation matrix or per-observation heap
allocation is introduced.

Appended normal doctests exercise a synthetic three-parameter producer:
score accumulation, all three Fisher diagonal entries, condition number four,
and singular rank-one OPG. This producer tests storage and indexing, not a new
statistical family. Dense three- and eight-dimensional matrices with known
eigenvalues test the general diagnostic; the eight-dimensional example has
condition number nine. A nine-parameter case tests the capacity status.

The quantities remain total score S, per-observation OPG sum J, negative
log-likelihood Hessian H, and model Fisher information F=n*I1. J is not the
outer product of S. Held coefficients are known constants; masking fitted
nuisance parameters does not account for their estimation.

## Cancellation fixes and arithmetic limits

**Beta.** Both score components use a digamma increment
`psi(z+h)-psi(z)`; Fisher diagonal entries use the trigamma difference
`psi1(z)-psi1(z+h)`. The original direct subtractions lose relative precision
in imbalanced models. The existing held-shape fitter already supplied the
large-z digamma-increment strategy. The score implementation extends that
strategy, leaving the fitter and special-function seam unchanged.

For z>=20 define r=h/z, q=1/(1+r), d=r/(1+r), u=1/z, and
`D_n=d*(1+q+...+q^(n-1))`. This evaluates `1-q^n` without subtracting
nearly equal numbers. The numerical expansions are

```text
psi(z+h)-psi(z)
  = log1p(r) + u*d/2 + u^2*D_2/12 - u^4*D_4/120
    + u^6*D_6/252 - u^8*D_8/240 + remainder

psi1(z)-psi1(z+h)
  = u*d + u^2*D_2/2 + u^3*D_3/6 - u^5*D_5/30
    + u^7*D_7/42 - u^9*D_9/30 + remainder
```

For z<20 and h<0.01*z, exact recurrences move z to at least 20,
adding the corrections below at each step. There are at most 20 steps;
this is a bounded recurrence, not an optimization or numerical search.

```text
Delta_psi(z,h) = Delta_psi(z+1,h) + h/[z*(z+h)]
Delta_tri(z,h) = Delta_tri(z+1,h) + h*(2*z+h)/[z^2*(z+h)^2]
```

The code factors these corrections to avoid subtracting reciprocals.
For z<20 with h>=0.01*z it retains direct special-function differences.
The recurrences and power-difference identity are exact mathematical
identities. The truncated expansions and floating-point implementation have
the numerical evidence below; this report does not prove a uniform error bound.

Decision history, 2026-10-31: a preliminary switch at z=100, with direct
differences below it, failed the expanded independent OPG check at
`7.253648e-13` against the unchanged `3e-13` tolerance. A small score
error was amplified by the other score in a cross-product. The final
switch at 20 and recurrence for small increments passed without relaxing
tolerances. Threshold probes include z=19,20,99,100; appended doctests check
the exact h=1 digamma and trigamma recurrences through z=1e6.

**Poisson.** The score is evaluated as `(x-lambda)/lambda` rather than
`x/lambda-1`. This preserves the difference for nearby large counts and
rates, including lambda=1e12 with x=lambda+1. Both expressions are the same
analytic derivative. Counts in the tested range are exactly representable
integers in double precision.

**Gamma.** The shape score uses `log(x/scale)` when the ratio is positive
and finite, avoiding subtraction of two large logarithms after a unit
change. If forming the ratio overflows or underflows, it falls back to
`log(x)-log(scale)`, preserving finite shape-only derivatives.

**Extreme natural units.** At normal sd=1e-200 or 1e200, information
proportional to sd^-2 overflows or underflows double precision. Gamma scale
information has the same issue at these unit extremes. Positive information
that rounds to zero is explicitly marked as arithmetic failure in these
models; overflow is caught by the shared finite checks. Selected blocks
therefore return overall `NumericalFailure`, with unusable numerical fields,
rather than a clipped value or an apparent zero-information model.

The validator asserts exactly ten such failures: six normal calls (three
nonempty masks at each endpoint) and four gamma calls (scale free, with or
without shape free, at each endpoint). Gamma shape-only inference remains
valid at both endpoints; unused unrepresentable scale entries do not poison
that block. Empty free subsets still validate parameters and observations.

## Rank and conditioning contract

Each free matrix has its own status and condition field:
`opg_status/opg_condition`, `observed_status/observed_condition`,
and `fisher_status/fisher_condition`. A symmetric Jacobi iteration operates
on the free block scaled by its largest absolute entry. It stops when the
largest off-diagonal entry is at most double epsilon. It allows at most
128*p*p rotations, returning `NumericalFailure` if the residual stopping
criterion is not met. It never returns an unchecked capped eigensolve.

Let rho=min(abs(eigenvalue))/max(abs(eigenvalue)). The reported condition is
1/rho, in the supplied README units; it is an approximate spectral condition
number. Thresholds, with double epsilon=2^-52:

| Diagnostic | Criterion |
|---|---|
| Singular | zero matrix, or rho <= 64*epsilon = 1.4210854715202004e-14 |
| IllConditioned | above the singular threshold, but rho <= sqrt(epsilon) = 1.4901161193847656e-8 |
| Ok | rho above sqrt(epsilon) |
| NumericalFailure | nonfinite entries, failed eigensolve residual, or a materially negative eigenvalue in a PSD-designated block |

Thus the condition cutoffs are approximately 7.0368744e13 and 6.7108864e7.
A zero minimum eigenvalue gives infinite condition. The empty block has
condition one and status Ok. For PSD blocks a negative eigenvalue below
-64*epsilon in max-entry-scaled units is a numerical failure; smaller signed
roundoff can be classified as numerical singularity.

Observed H uses absolute eigenvalues and may be indefinite away from an MLE;
an Ok condition status does not establish positive definiteness. Conditions
depend on coordinates and units. Mathematical full model rank can coexist
with numerical singularity in natural units. Singular/ill-conditioned matrices
do not invalidate otherwise finite scores; nonfinite arithmetic does invalidate
the overall result. No inverse or pseudoinverse is formed.

The validator independently diagonalizes the rounded C++ Fisher blocks with
100-digit mpmath arithmetic. It checks all nonempty blocks' status thresholds.
It compares numerical condition values only for reference condition <1e10,
with scaled tolerance 2e-6; beyond that range it verifies status, not accurate
large condition values. The maximum scaled condition error was
1.082830e-10. Known-spectrum doctests additionally
exercise dense blocks up to the storage limit and diagonal blocks on either
side of the ill-conditioned/singular regimes.

## Independent validation ranges and tolerances

The validator compiles a standalone C++ probe against production score headers,
using the configured plain Boost include path. It parses each unchanged
README `log_density` with SymPy and evaluates its derivatives with 100-digit
mpmath arithmetic, seed 20261031. References use the exact supplied binary64
parameter/observation values, avoiding decimal-to-binary input discrepancies.

All masks are tested at every point, for 436 calls:

| Range | Workload | Calls |
|---|---|---:|
| Moderate | Original eight seeded points per family: positive parameters in [1.2,5]; normal mean [-2,2], sd [0.7,2.5]; Poisson counts 0–11 | 112 |
| Gamma small | Shape 0.001,0.01,0.1; scale 1; x/shape=0.2,1,3 | 36 |
| Beta small | (0.001,0.001), (0.001,1), (1,0.001) | 36 |
| Beta concentrated | Equal shapes 20,100,1e6 | 36 |
| Beta imbalanced | Ten shape pairs, including (1,1e6), its reverse, (0.001,1000), its reverse, and threshold probes; ratios up to 1e6 | 120 |
| Normal units | sd=1e-200,1e-150,1e-100,1e-50,1,1e50,1e100,1e150,1e200; mean 0, x=sd | 36 |
| Gamma units | Same nine scales; shape 2, x=2*scale | 36 |
| Poisson large | Rates 1e6,1e9,1e12; x=rate-1,rate,rate+1,rate+100 | 24 |

Each beta pair uses three interior x values based on 0.7,1,1.3 times its mean,
clipped to [1e-12,0.999999999]. The appended C++ beta grid also combines
shapes 0.001,1,1e6, reaching ratios 1e9. Gamma doctests combine small shapes
with the extreme scale endpoints, including the valid shape-only subsets.

For extreme ranges, model Fisher is checked by applying known moments to the
independently differentiated Hessian: normal first and second moments, gamma
E[X]=shape*scale, Poisson E[X]=lambda; beta's Hessian is independent of x.
This avoids unreliable extreme-shape quadrature. The moderate grid retains
direct integration/summation of score products, independent high-precision
central differences, and the gamma scale-to-rate coordinate transformation,
including the arbitrary-point Hessian's second-derivative score correction.

Errors use abs(actual-reference)/(1+abs(reference)).

| Quantity | Acceptance tolerance |
|---|---:|
| Score, symbolic Hessian, moment-based Fisher | 2e-13 |
| Per-observation score products / OPG | 3e-13 |
| High-precision central Hessian difference, moderate grid | 2e-8 |
| Fisher vs integrated/summed score products, moderate grid | 1e-12 |
| Gamma coordinate transformation, moderate grid | 2e-13 |
| Condition number when reference <1e10 | 2e-6 |

Maximum scaled errors per range follow. Arithmetic failures above are asserted
separately and excluded from derivative-error maxima. A zero-dimensional call
has no numerical entries to compare.

| Range | Score | Observed H | Model F | OPG | Condition |
|---|---:|---:|---:|---:|---:|
| moderate | 1.802394e-16 | 2.133294e-16 | 7.114140e-17 | 2.174775e-16 | 3.550530e-15 |
| gamma-small | 1.209182e-16 | 4.023513e-17 | 4.023513e-17 | 2.668412e-16 | 1.373770e-16 |
| beta-small | 1.164957e-16 | 1.353761e-16 | 1.353761e-16 | 2.352762e-16 | 4.130405e-17 |
| beta-concentrated | 7.351981e-16 | 3.513715e-16 | 3.513715e-16 | 5.353173e-16 | 1.082830e-10 |
| beta-imbalanced | 7.036201e-16 | 7.991964e-17 | 7.991964e-17 | 1.396448e-15 | 0.000000e+00 |
| gamma-units | 1.436405e-17 | 2.254022e-16 | 8.391392e-17 | 1.391142e-17 | 1.609865e-16 |
| normal-units | 8.391392e-17 | 2.254022e-16 | 8.360621e-17 | 2.254022e-16 | 0.000000e+00 |
| poisson-large | 4.791694e-21 | 7.285552e-23 | 4.525184e-23 | 2.092256e-25 | 0.000000e+00 |

Full-block condition comparisons in highly ill-conditioned ranges are excluded
from the numeric condition maxima; their statuses are still checked.
Additional moderate-grid maxima: central differences
2.345548e-10, integrated/summed Fisher
7.114140e-17, and gamma transformations
3.166747e-16.

Relative errors are also recorded, excluding reference magnitudes <=1e-90
and exact zeros. They are diagnostics, not a uniform relative guarantee at
stationary scores. The comparison probe evaluates the original unprotected
first beta score/first information diagonal and Poisson score directly with
the same C++ special-function backend.

| Range | Original first-score max relative error | Revised all-score max relative error | Original first-information-diagonal max relative error | Revised all-information-entry max relative error |
|---|---:|---:|---:|---:|
| beta-small | 2.439395e-13 | 8.822387e-16 | 1.616674e-13 | 3.619852e-16 |
| beta-concentrated | 3.834799e-09 | 1.620031e-10 | 1.209210e-16 | 1.388872e-14 |
| beta-imbalanced | 1.587194e-03 | 9.144036e-11 | 1.482829e-10 | 7.662413e-14 |
| poisson-large | 8.890058e-05 | 6.228159e-17 | — | — |

These are maxima over the stated finite grids; the old first-coordinate and
new all-coordinate sets are identified explicitly. Small remaining relative
score errors arise when finite-precision terms cancel near zero. All final
scaled tolerances passed. In particular, the formerly failing OPG comparison
now lies within a global maximum scaled OPG error of
1.396448e-15.

Existing doctests continue to verify callback row sums, per-row outer-product
sums, every held subset, total normalization under duplicated observations,
and local fitted-score residuals at moderate points. New cases add small
shapes, extreme units, large nearby Poisson counts, stable beta recurrences,
storage overflow and multidimensional conditioning. These are numerical
checks, not consistency proofs or coverage evidence.

## Final runs and environment

All final commands exited zero. No failures were permitted. The preliminary
OPG failure described above was corrected before these final runs.

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run4 nice -n 10 env CMAKE_BUILD_PARALLEL_LEVEL=4 just cpp-test
/tmp/umvl-cpu/run4 nice -n 10 env CMAKE_BUILD_PARALLEL_LEVEL=4 just cpp-test-no-long-double
/tmp/umvl-cpu/run4 nice -n 10 python3 dev/validation/inference-3.py
/tmp/umvl-cpu/run4 nice -n 10 just test
```

| Final run | Result |
|---|---|
| C++ dev | 105/105 shards passed; 56.65 s ctest execution |
| C++ no-long-double | 126/126 full shards passed; 97.65 s ctest execution |
| Independent validator | 436 calls passed, including 10 asserted arithmetic failures |
| R dev | FAIL 0, WARN 0, SKIP 151, PASS 19842; 77.536 s elapsed test execution |
| Source-input hash verification | All validator-recorded source hashes match final files |

Build time and CPU-slot waiting time are additional. R skips require the full
tier. R full was not run: this change concerns inference headers and their
dedicated validator, and changes no fitting, d/p/q/r or general test-tier
infrastructure. The no-long-double recipe runs the full C++ suite.

Environment: Linux-7.0.0-38-generic-x86_64-with-glibc2.43; C++17,
c++ (Ubuntu 15.2.0-16ubuntu1) 15.2.0; Boost 1.90 via BH 1.90.0-1;
R 4.6.1 at /opt/R/4.6.1, Rcpp 1.1.2;
Python 3.14.5 (main, Jun  2 2026, 22:25:17) [Clang 22.1.3 ]; mpmath 1.3.0,
SymPy 1.14.0, PyYAML 6.0.3. The system also has an older R;
the tested PATH selects /usr/local/bin/R, linked to /opt/R/4.6.1/bin/R.

Logs and full regenerable results are under `build/scratch/`:
`cpp-dev-final.log`, `cpp-no-long-double.log`, `validation-final.log`,
`r-dev.log`, and `inference-3/results.json`. Essential results and source
identities are retained here rather than relying on ignored output.

## What TASK-102.4 can rely on

The four-family pilot now supplies the same total natural-coordinate score,
OPG, observed Hessian and model Fisher contract with storage through eight
parameters, explicit dimension rejection, conditioning diagnostics on each
free block, and tested derivative arithmetic over the stated extreme ranges.
Default fits and goldens remain unchanged. The earlier observation-score sum,
normalization and gamma coordinate-transformation checks remain green.

The exponential-family DQM/finite-information/full-model-rank arguments for
these positive interior parameters remain those in the canonical plan and
prior report. Numerical singularity and arithmetic overflow do not contradict
those real-arithmetic model properties. The synthetic storage model makes no
additional statistical regularity claim.

TASK-102.4 still needs conditioning-aware, positive-definiteness-aware solves;
explicit propagation or justified coordinate rescaling of rejected blocks;
correct nuisance-aware contrasts and total-sum covariance formulas;
and coverage/type-I error calibration with failure accounting and Monte Carlo
uncertainty. Observed H can be indefinite, and its coordinate transformation
away from stationarity needs the score correction. Model Fisher, score OPG,
observed H and misspecification sensitivity remain distinct objects.

Open mathematical guarantee: no certified uniform floating-point/truncation
bound for the beta expansions across the whole positive parameter domain has
been established. That claim would require remainder and rounding bounds,
including near-zero scores and ranges beyond this grid. The finite validation
and exact recurrence identities here support the stated numerical scope.
No covariance, sandwich robustness, bootstrap validity or estimator asymptotic
theorem is established by these derivative tests.

## Tested source identities

SHA-256 hashes include the unchanged README density sources, appended tests
and validator. These identify the tested uncommitted implementation on the
revision above.

```json
{
  "univariateML-cpp/include/cppunivariate/core/inference.hpp": "e72e93140e08dafb171450e7b1d218234656c4eb5de1a9a8febd041fe2648bff",
  "univariateML-cpp/include/cppunivariate/distributions/norm/score.hpp": "dc733705697060a386af31218f5c345087b4491fdeae564dca319abf780dc19a",
  "univariateML-cpp/include/cppunivariate/distributions/norm/test.cpp": "dbbf611f50e4812f213cc63cdde6ecd9efc283fd5bcf440198d6e8d54a44a61d",
  "univariateML-cpp/include/cppunivariate/distributions/norm/README.md": "3ed1d5fb6c30bb512ac8598f48a32177b7c6ce94f5ab08e2b7c3879380606812",
  "univariateML-cpp/include/cppunivariate/distributions/gamma/score.hpp": "7b13490035d7a3e390ddc4a1ffb1ed8fba0fd0f78ba0471f12956226a37b1589",
  "univariateML-cpp/include/cppunivariate/distributions/gamma/test.cpp": "f857f36d6bca9baeb0609f703a1ac3582e6817e29b939b7d11b45e11c45ffe74",
  "univariateML-cpp/include/cppunivariate/distributions/gamma/README.md": "a4cea57a0834dfe877d95d6e46483a48063137d4108b8adbdd7eec15645fdf6f",
  "univariateML-cpp/include/cppunivariate/distributions/beta/score.hpp": "5aa1c1ac1560111ef6c9b7b5dc2ff3775e402e616c65d23240aa19faa0542ab0",
  "univariateML-cpp/include/cppunivariate/distributions/beta/test.cpp": "08afc243ccc68bef3fb7f63e45896299cbc809edb32df90ef972d6f4d0593a51",
  "univariateML-cpp/include/cppunivariate/distributions/beta/README.md": "5e97f872baee250d69e41ae0b38b66f169f4d3e52e67a8c19faf8519bf3c323a",
  "univariateML-cpp/include/cppunivariate/distributions/pois/score.hpp": "c9ec90d530f39f272fbdb370863bd4873d9dd9b3163541406e2b515fd5d2f793",
  "univariateML-cpp/include/cppunivariate/distributions/pois/test.cpp": "af5d960a69485e0287674afa98fce264db245d49d61c1863e3fc56a33ee76358",
  "univariateML-cpp/include/cppunivariate/distributions/pois/README.md": "784805b697e7f3f80372878c39304bde6f1659397d66e0a419dc282f93fa4c96",
  "dev/validation/inference-3.py": "3b63102d5688dcb53fb64596a2c237234637a7a72d937ff804c88e1043897f4e"
}
```
