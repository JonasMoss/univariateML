# TASK-152: GED and SGED residual underflow

Completed 2026-11-08 in `codex/ged-density`, based on `cfb7c4b3`.
All assigned items are complete and all final required runs passed. No commits
or board edits were made.

## Changes and mathematical basis

[GED density](../../../univariateML-cpp/include/cppunivariate/distributions/ged/dpqr.hpp)
now extends its existing overflow fallback to nonzero residuals whose standardized
quotient is zero or subnormal. For distinct finite observations and locations,
`L = log|x-mean| - log(sd)` retains the distance before the kernel is evaluated
as `exp(nu * (L - logscale(nu)))`. Subnormal quotients also need this route:
they may remain nonzero while losing significant digits.

GED's CDF already computes its kernel argument logarithmically. Its tail choice
now uses `q < mean`, preserving the sign when the quotient becomes negative
zero. GED's quantile already combines the logarithms of scale and magnitude
before exponentiation; regression roundtrips check it at the affected scales.

[SGED evaluation](../../../univariateML-cpp/include/cppunivariate/distributions/sged/dpqr.hpp)
retains the sign and logarithmic magnitude of its affine residual
`sigma*(x-mean)/sd + mu` when an intermediate leaves the normal range. Signed
logarithmic addition preserves both the location residual and centering shift;
the density and incomplete-gamma tails consume that magnitude directly. This
also repairs overflow before division or skew scaling.

The centering moment
`Gamma(2/nu) / sqrt(Gamma(1/nu)*Gamma(3/nu))` can itself become subnormal or
underflow at very small held shapes. Its logarithm is retained in that case.
SGED quantiles similarly combine the signed unit quantile, centering shift,
and final scale before exponentiation. Ordinary evaluation retains the previous
arithmetic path. A nonfinite existing skew normalizer is outside this rescue;
a regression preserves its prior NaN result rather than manufacturing a finite
quantile.

These transformations are exact algebraic identities. Their floating-point
accuracy is supported by the numerical comparisons below, not by outward-rounded
interval bounds. Fitter algorithms, stopping rules, and coefficient outputs were
preserved. In particular, the existing C++ normalization-failure checks remain;
R's fully held evaluation follows its density path and now obtains the correct
likelihood. The GED dossier's obsolete density limitation was corrected.

## Other-family check

GED and SGED are the two symmetric-residual kernels with a freely supplied
fractional exponent. The related positive-power kernels in Weibull, inverse
Weibull, Frechet, transformed gamma, inverse transformed gamma, and Nakagami
already form their power arguments from logarithms before exponentiation.
The Burr, GB2, and Pareto power families likewise use logarithmic ratios.
Normal/half-normal, Laplace, Huber, and Student/skew-Student do not have the
subunit residual-power mechanism that amplifies this lost quotient. No further
family changes were needed for this defect.

## Numerical evidence and regressions

The checker evaluates the defining density and incomplete gamma at 100 decimal
digits, lifting the binary64 inputs exactly. Quantiles use an independent
high-precision gamma inverse bracket. Representative results are:

| Case | Before | Independent reference |
|---|---:|---:|
| GED log density, `x=1e-100, mean=0, sd=1e300, nu=.02` | -612.22457872605537 | -612.224579676587918242… |
| GED lower log CDF, `x=-1e-100, mean=0, sd=1e300, nu=.0005` | approximately -1.26752e-18 | approximately -41.20946876846 |
| SGED log density, `x=0, mean=0, sd=1e300, nu=.0003, xi=1.3` | 4796.43334942614 | -106.798025104960350… |
| SGED 0.1 quantile, same coefficients | 0 | approximately -7.945797767436369e-80 |

The original GED example loses a kernel of approximately `9.50532505e-7`.
The corrected implementation agrees with all 29 reference records in both
native and no-long-double builds. The largest absolute log-density error is
`6.29e-12`; the largest relative quantile error is `5.01e-12`. WASM passes
79 scalar comparisons against the same independent references.

The tiny-shape asymmetric quantiles are checked directly against the
high-precision inverse: their concentration makes CDF roundtrips at rounded
binary64 quantiles ill-conditioned. The symmetric underflow cases do have
representable tail/quantile roundtrips and are checked that way too.

Three appended C++ regression cases cover 50 assertions, including both signs,
subnormal quotients, genuine zero residuals, symmetric and asymmetric SGED,
vanished and subnormal centering moments, quantiles, endpoints, and a nonfinite
normalizer. Appended R tests exercise `mlged`/`mlsged` with every coefficient held,
`dml`, `pml`, `qml`, and `logLik`.

A separate comparison with the starting commit finds **8,100 ordinary density,
CDF, and quantile values bit-identical**, across both tail and log flags.
All **298 tracked golden/truth files** were compared byte-for-byte with
`HEAD` and remain identical; no frozen values were regenerated.

Evidence:

- [Independent evaluator](2026-11-08-ged-density-data/check.py) and [C++ probe](2026-11-08-ged-density-data/probe.cpp).
- [Native comparisons](2026-11-08-ged-density-data/comparison-native.json) and [no-long-double comparisons](2026-11-08-ged-density-data/comparison-no-long-double.json).
- [Ordinary-value checker](2026-11-08-ged-density-data/check_unchanged.py) and [before/after output](2026-11-08-ged-density-data/unchanged.txt).
- [WASM checker](2026-11-08-ged-density-data/check_wasm.mjs) and [validation summary with source checksums](2026-11-08-ged-density-data/validation.json).

## Final validation

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | workstation, GCC 15.2 | 106/106 passed |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 passed |
| `just cpp-test` | simbox, GCC 13.3 | 106/106 passed |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129 passed |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129 passed |
| `just test` | simbox | 21,852 passed; 154 tier skips; 0 failures/warnings |
| `just test-full` | simbox | 44,675 passed; 0 failures/warnings/skips |
| `just js-build` and `just js-test` | workstation | build passed; 536/536 tests passed |
| `just site-check` | workstation | passed, including 99 compiled C++ snippets |

The final site check rebuilt the WASM binary from the completed source; the
final JavaScript suite and 79-reference checker then passed on that binary.
The two density-header checksums matched between workstation and simbox.
Local builds and suites used the shared slot wrappers; simbox used the prescribed
runner with four build/test workers. BLAS/OpenMP were held to one thread.

The first JavaScript run exposed a stale binomial-family test: it expected
`unresolved` with an infinite upper bound for four inputs whose existing C++
certificates and R tests now require a proved Poisson limit. The test now requires
`limit`, a finite upper bound, and both witnesses. Binomial fitting code and
binding messages were preserved. The final complete JavaScript suite passed.

## Reproducing the independent checks

From the repository root, with the existing BH and mpmath installations:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
data=dev/research/evaluations/2026-11-08-ged-density-data
/tmp/umvl-cpu/run g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$data/probe.cpp" -o build/scratch/ged-density-probe
/tmp/umvl-cpu/run python3 "$data/check.py" build/scratch/ged-density-probe build/scratch/ged-density-reference.json
/tmp/umvl-cpu/run python3 "$data/check_unchanged.py" build/scratch/ged-density-unchanged "$boost"
/tmp/umvl-cpu/run node --v8-pool-size=1 "$data/check_wasm.mjs"
```

For the second C++ probe, add
`-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS` to the compile command and use a
separate output executable/JSON. The WASM check requires the built JavaScript
binding. No assigned items remain open.
