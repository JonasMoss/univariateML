# Burr production-port evidence

Run from the worktree root after configuring temporary directories:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-burr-port-data/run.sh
```

The script builds the public-fitter comparison with C++17 and `-O2`. Its
BH locator uses R only to find existing Boost headers for the legacy fitter.
The two certificate-only diagnostic programs compile with `-fno-exceptions`
and only the standalone core include directory, without Boost or R headers.
There is no dependency installation, simulation regeneration, or Git write.

- [probe.cpp](probe.cpp): one warmup plus ten alternating certified/legacy
  fits per sample, reporting medians, outcomes, coefficients and seam enclosures.
- [check.py](check.py): verifies exact fixture/input agreement, all 50 retained
  outcomes and the extra tie stop; independently evaluates likelihoods and
  scores with mpmath at 80 digits and compares the 23 finite fits with the
  retained high-precision stationary roots. Thresholds are `1e-8` relative
  coefficient error, `1e-8` absolute log-likelihood error and `1e-7` total-score
  norm. These are numerical checks, separate from the certificate theorem.
- [results.json](results.json), [summary.json](summary.json): per-input results,
  timing medians, maximum discrepancies and SHA256 source/input identities.
- [components.cpp](components.cpp), [components.json](components.json): ten
  41-point golden reconnaissance batches, comparing proposal solves with the
  same solves plus primal/dual arithmetic. This is a microbenchmark, not a full
  runtime decomposition.
- [coordinates.cpp](coordinates.cpp), [coordinates.json](coordinates.json):
  independently reproduce the accepted curvature-neighborhood widths under
  three coordinate choices. They use stored roots only as proposals.
- [validation.py](validation.py), [validation.json](validation.json): parse
  completed final suite logs and record commands, results, environment and log
  hashes. Run the parser through one CPU slot after all five suites finish.

The original 50 inputs and their expected outcomes are read from the existing
[seam samples](../2026-11-03-burr-xi-data/samples.json) and
[root outcomes](../2026-11-04-burr-root-data/global.json). No original input
or reference is rewritten. These include the original seed-138 samples, the
n=500 frozen golden and the closure counterexamples/crossing proxies.
The extra 20-observation binary64 seam proxy is frozen in
[certificate-fixtures.inc](../../../../univariateML-cpp/include/cppunivariate/distributions/burr/certificate-fixtures.inc)
and the R test. It was found by adjusting the largest log observation in a
seed-138 exponential sample until the numerical Weibull seam slope was near
zero; its enclosed slope contains zero. Its construction is not a certificate
premise and it is not asserted to have exactly zero slope.

The expected budget and tie outcomes are guarded stops with no estimate.
All retained summaries are small; complete transient build/test logs stay in
`build/scratch/`. The source checksums identify the uncommitted implementation
on top of the recorded base revision. The report is
[2026-11-04-burr-port-report.md](../2026-11-04-burr-port-report.md).
