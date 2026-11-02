# Half-t outer df certificate — 2026-11-02

TASK-116 replaces the uncertified outer grid decision with a bounded search in
log df. The earlier grid and golden-section refinements remain as the
initializer; a returned finite fit or selected face is then checked against
all omitted shape intervals and tails. The half-t golden was not changed.

The certificate is **in exact arithmetic conditional on the computed
comparisons**, with a practical total allowance of `1e-8 * n` log-likelihood
units. It is not an outward-rounded floating-point proof. This is the
package convention requested for this job. A spent 100,000-interval budget,
an unrepresentable search extent, an unresolved scale bracket or a failed
numerical guard returns `IterationFailure`; the binding stops with a
certificate-specific error. A selected normal or scale-zero face is a
numerical margin decision, not an exact proof that the finite model has no
attained maximum within that margin.

## Mathematics and algorithm

The [half-t dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md)
contains the derivations, including the existing monotone log-scale score,
the small-df pointwise bound, the uniform large-df bound against the
half-normal supremum, the tied scale-zero boundary and the interval
profile-score enclosure. Gamma-ratio bounds give a computable small-df
cutoff. For the large-df tail, `log(1+u) >= u-u^2/2` bounds the profile by
the normal supremum plus an explicit data-dependent `C/nu`; a subset of the
largest positive residuals gives a practical uniform lower scale bound.
The exact half-minimum tie is bounded separately on a neighbourhood of
`nu = 1`, where a uniform interior scale-root bracket degenerates.

On a compact interval the scale-score contributions are monotone in df
individually, so their endpoint minima and maxima bound every scale root.
The envelope score is bounded using these scale brackets. The score is
rearranged using `h(w) = log(1+w)-w/(1+w)` to avoid the worst cancellation;
small `w` uses successive series bounds. At large df, an integral
representation and polynomial bounds for `tanh` bound the normalizer score
without subtracting nearly equal digammas. A root-bracket residual allowance
accounts for the inner scale solve. The implementation uses endpoint checks
when the interval score has one sign, otherwise bisects. It retains slack in
the `1e-8*n` margin and stops if its practical rounding guard is exceeded.
The endpoint and fixed-df scale results are mathematical statements; the
actual bisections, comparisons and special-function evaluations are numerical.
Global uniqueness of the df maximum remains unknown and is not assumed.

## Paired checks and runtime

The retained [comparison driver](2026-11-02-ht-cert-data/compare.cpp) uses
seed 1162026 and 600 samples of size 24: 200 absolute Student t samples with
0.4 df and held threshold, 200 with 80 df and held threshold, and 200 with
3 df, an estimated threshold and two tied minima. It compares the original
95787451 fitter against the new fitter. There were 431 finite-fit pairs and
169 matching non-finite statuses; no status changes, numerical failures or
likelihood losses occurred. The maximum original-minus-new log likelihood
was zero. In Round 1 the same 600 samples were also checked in R using a
stored fixture. Round 2 removed that 312 KB fixture and the dependent R
test; C++ retains the seeded regression against the original algorithm.
This is numerical evidence, not a proof of global optimality.

An earlier version exhausted 100,000 intervals on near-normal samples 282
and 363. Both were ordinary finite fits, so the failures were investigated:
the large-df normalizer enclosure was unnecessarily loose. The implemented
integral-based bound and one-sided interval pruning resolved both; the
complete 600-sample rerun had no failures. The first R full-tier attempt
then found a test-harness error: the installed testthat did not accept the
new test's `info` argument to `expect_gte`. Removing that argument fixed the
test; both R tiers were rerun successfully. The first failed attempts are not
counted as passes.

The paired [n=500 benchmark](2026-11-02-ht-cert-data/bench.txt) uses seed
116500, absolute t(3) data, five repetitions, g++ 15.2.0 `-O2`, and one
local CPU slot. Median time was 0.0933 s for the original and 1.6431 s for
the certified fitter, approximately 17.6 times longer. All five returned
finite fits with identical computed likelihoods; the new search used 640
intervals. The 600-sample driver took 2.455 s original and 41.990 s new in
total. These timings are workload-specific; the interval budget and
conservative bounds can cost more on difficult samples.

[check_bounds.py](2026-11-02-ht-cert-data/check_bounds.py) passed 450
numerical tail/profile comparisons, 500 numerical root and score enclosure
comparisons, and 300 high-precision large-df normalizer comparisons. The
SciPy optimizations and mpmath comparisons check the formulas but do not
replace their proofs or establish outward-rounded arithmetic.

## Round 1 final validation

All final runs used this workstation, the working tree based on
`9578745158d3d15c210707f34a7ce83f37b4296e` with the uncommitted TASK-116
changes, `/tmp/umvl-cpu/run4` one command at a time, single-threaded
BLAS/OpenMP, and the worktree `build/tmp` temporary directory. The R run used
R 4.6.1, Rcpp and BH; the C++ compiler was g++ 15.2.0. The site check reported
Python 3.14.5 and SymPy 1.14.0. No simbox runs were used.

| Command | Result |
|---|---|
| `just cpp-test` | Pass, 106/106 dev shards, including the 600-case half-t test. |
| `just cpp-test-no-long-double` | Pass, 127/127 shards, including the 600-case test. |
| `just test` | Pass: 20,123 passes, 152 tier skips, 0 failures, 0 test warnings. |
| `just test-full` | Pass: 42,327 passes, 0 skips, 0 failures, 0 test warnings; half-t had 716 passing expectations. |
| `just js-build` | Pass. |
| `just js-test` | Pass, 413/413 tests; 0 failures or skips. |
| `just site-check` | Pass, including 99 family pages, MathML, browser checks and C++ snippet compilation. |

The shell printed existing missing `.cargo/env` startup messages; the site
WebAssembly build also printed an existing binomial self-comparison compiler
warning. These were not testthat warnings or test failures. The full-tier
assertion-API failure described above was fixed and the listed R results are
from the successful reruns.

Reproduce the standalone comparison and benchmark from the repository root,
using the installed BH headers and the requested CPU-slot wrappers. The
original header is recovered read-only from the base revision into ignored
scratch; no Git-writing operation is needed:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
git show 95787451:univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp | sed 's@"../../core/span.hpp"@"cppunivariate/core/span.hpp"@' > build/scratch/ht-old.hpp
/tmp/umvl-cpu/run g++ -std=c++17 -O2 -IunivariateML-cpp/include -I/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include -Ibuild/scratch dev/research/evaluations/2026-11-02-ht-cert-data/compare.cpp -o build/scratch/ht-compare
/tmp/umvl-cpu/run build/scratch/ht-compare
/tmp/umvl-cpu/run g++ -std=c++17 -O2 -IunivariateML-cpp/include -I/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include -Ibuild/scratch dev/research/evaluations/2026-11-02-ht-cert-data/bench.cpp -o build/scratch/ht-bench
/tmp/umvl-cpu/run build/scratch/ht-bench
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-02-ht-cert-data/check_bounds.py
```

## Round 2: scale-box performance and final validation

The initial Round 1 profile used g++ 15.2.0 with `-O2 -pg -fno-inline`,
five repetitions of the same `n=500` benchmark, and one local CPU slot.
Instrumentation changes runtime, so its counts rather than its elapsed time
identify the work: 640 outer intervals per fit, 1,494 `scale_box` calls per
fit, and approximately 175.9 million `logistic` evaluations per fit. There
were 1,840 digamma calls per fit. The instrumented profile attributes much
of the remaining time to shared-library machinery and is not a reliable
libm time breakdown. The recorded counts are in
[profile-round2.txt](2026-11-02-ht-cert-data/profile-round2.txt).

The optimized scale solve caches squared positive distances relative to the
largest positive distance. On the ordinary range the scale score is evaluated
as the algebraically equivalent rational expression, with the scale factor
computed once per bisection step. Extreme scaled-distance or scale-factor
ranges use the original log-domain calculation. A single df requires only
one bisection, rather than two identical bisections. Each bisection now
retains a log-scale bracket to width `1e-8` or adjacent floating-point
numbers. The bound still includes the endpoint-score bound on the loss from
the retained inner bracket; the outer interval, face and tail bounds and the
`1e-8*n` practical margin are unchanged. The
[family dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md)
records the fast path and fallback. Outward rounding is still not used; the
certificate remains conditional on computed comparisons with the practical
floating-point allowance.

On the same `n=500`, seed 116500, absolute t(3) workload, five paired
single-slot repetitions gave median **0.086662 s original** and **0.172176 s
Round 2**, or **1.99 times** the original in that run. This meets the 3x
and 0.3 s targets. Round 1 had measured 1.643061 s for the certified fitter;
Round 2 is about 9.5 times faster than that earlier measurement. Both
rounds used 640 intervals on this sample. Every paired benchmark run returned
the same finite-fit status and computed log likelihood. Raw measurements
and build details are in [bench-round2.txt](2026-11-02-ht-cert-data/bench-round2.txt).

The [Round 1 versus Round 2 driver](2026-11-02-ht-cert-data/compare-rounds.cpp)
regenerated the same 600 seeded samples and compared both certificates:
431 finite pairs and 169 matching limit statuses, with **zero changes** in
status, threshold, computed log likelihood, df or scale. The driver reported
maximum absolute differences of zero in log likelihood, log df and log scale;
iterations are not compared because they are search diagnostics. Total
single-slot time was 38.706 s in Round 1 and 4.774 s in Round 2. The
[retained Round 1 certificate](2026-11-02-ht-cert-data/cert-round1.hpp) is
9,258 bytes and permits this paired check to be reproduced. The C++ test also
regenerates all 600 samples from the seed and compares with the original
pre-certificate algorithm. It tests the cached and log-domain root brackets,
including extreme distances and df. These are regression and numerical
checks, not an outward-rounded floating-point proof.

The 312 KB `cert-baseline.json` and its R full-tier consumer were removed.
The retained evaluation files are each below 50 KB; the largest is the
9,258-byte Round 1 certificate. The smaller R half-t regression tests remain.

Final Round 2 commands were run locally, one at a time through
`/tmp/umvl-cpu/run4`, with the same worktree temporary-directory and
single-threaded BLAS/OpenMP settings as above. The source is the uncommitted
Round 2 working tree based on `9578745158d3d15c210707f34a7ce83f37b4296e`.

| Command | Round 2 result |
|---|---|
| `just cpp-test` | Pass, 106/106 dev shards, including the 600-sample and scale-box tests. |
| `just cpp-test-no-long-double` | Pass, 127/127 shards. |
| `just test` | Pass: 20,123 passes, 151 tier skips, 0 failures and 0 test warnings. |
| `just test-full` | Pass: 41,726 passes, 0 skips, 0 failures and 0 test warnings. |
| `just js-build` | Pass. |
| `just js-test` | Pass, 413/413 tests, with no failures or skips. |
| `just site-check` | Pass, including browser, MathML, family-link and C++ snippet checks. |

The existing shell `.cargo/env` startup messages and the unrelated binomial
WebAssembly self-comparison compiler warning appeared again. They were not
reported as R test warnings or failures.

To reproduce the Round 1 versus Round 2 comparison after preparing the
`build/tmp` environment above, create an isolated header tree and compile the
retained driver. The current `mle.hpp` is identical to Round 1 for this
purpose; the retained file supplies the prior `cert.hpp` implementation.

```sh
mkdir -p build/scratch/ht-round1/cppunivariate/distributions/ht
cp dev/research/evaluations/2026-11-02-ht-cert-data/cert-round1.hpp build/scratch/ht-round1/cppunivariate/distributions/ht/cert.hpp
sed -e 's@"../../core/span.hpp"@"cppunivariate/core/span.hpp"@' -e 's@"../../special/special.hpp"@"cppunivariate/special/special.hpp"@' univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp > build/scratch/ht-round1/cppunivariate/distributions/ht/mle.hpp
/tmp/umvl-cpu/run g++ -std=c++17 -O2 -IunivariateML-cpp/include -I/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include -Ibuild/scratch dev/research/evaluations/2026-11-02-ht-cert-data/compare-rounds.cpp -o build/scratch/ht-rounds
/tmp/umvl-cpu/run build/scratch/ht-rounds
```
