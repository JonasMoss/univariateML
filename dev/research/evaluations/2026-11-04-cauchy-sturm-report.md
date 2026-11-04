# Held-scale Cauchy location certification — TASK-116

Completed 2026-11-04 in worktree `cauchy-sturm`, branch `codex/cauchy-sturm`,
based on `54b0d06a3e447f944940fc0dd4b5b87235d2acc2`. No commits were made.
All final commands finished before this report was written.

The held-scale location fitter now uses an exhaustive interval certificate,
replacing the observation-gap grid and local golden-section searches. Success
identifies a unique winning stationary-root bracket and returns an approximation
to that root. Otherwise the core returns a failure status and the binding stops.
Separated tied or numerically indistinguishable global candidates have their own
`AmbiguousMaximum` diagnostic. The joint default fitter, held-location scale
solver and golden files were not changed or regenerated; their regressions pass.

The implementation and full proof are in the canonical
[Cauchy dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md#held-scale-location-certificate)
and [fitter](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp).
R diagnostic handling lives in the existing
[common Cauchy binding](../../../bindings/common/distributions/cauchy/fit.hpp),
which the unchanged thin Rcpp adapter calls. Only Cauchy family files, that
family's bindings, and this report were authored. No settled d/p/q/r, special
functions, truth fixtures, generators, status files, roadmap or board were edited.

## Argument and numerical contract

With median-centred standardized observations `y` and location `t`, omit the
constant `-n*log(pi*scale)` and write

```text
L(t) = -sum log(1+(y_i-t)^2)
G(t) = sum 2*(y_i-t)/(1+(y_i-t)^2)
L''(t) = sum 2*((y_i-t)^2-1)/(1+(y_i-t)^2)^2.
```

Every maximum is within the observed range: outside it all score terms point
strictly inward. On any location interval, each score term's extrema occur at
its residual endpoints or residuals `-1,1`. Each curvature term's maximum occurs
at a residual endpoint or `+-sqrt(3)`; its value at the latter is `1/4`.
Summing termwise bounds therefore encloses the score and bounds curvature from
above. The negative sum of `log(1+d_i^2)`, where `d_i` is the distance from each
observation to the interval, bounds its likelihood from above.

The algorithm partitions the full range. A box can be discarded by a definite
score sign or by a likelihood upper bound below a known feasible likelihood.
Strictly negative curvature plus a verified positive/negative endpoint-score
bracket isolates exactly one strict local maximum. Endpoint expansion is used
to handle roots at partition boundaries; its curvature is checked again before
using either endpoint sign. Duplicate discoveries are merged only when their
brackets' hull is strictly concave, excluding two distinct roots there.
Unresolved intervals are split. A successful finite partition has therefore
excluded or enclosed every possible competing global maximum. Finally, one
candidate's evaluated likelihood must strictly exceed every distinct candidate's
likelihood upper bound, including the practical comparison margin.

Root bisection retains a bracket, requires width `w <= 1e-10` in held-scale units,
and requires total dimensionless location score `abs(G) < 1e-7`. Since
`abs(L'') <= 2*n`, a bracketed root's likelihood is at most its midpoint
likelihood plus `1e-7*w+n*w*w`. Conversion to the returned physical location
must retain the score gate and remain within `1e-10` of that bracket. The
corresponding returned-point gap formula uses `D=w+1e-10` in
`1e-7*D+n*D*D`.

These are exact-arithmetic implications **conditional on the computed
comparisons**. Compensated sums are used, with practical score/curvature margin
`128*epsilon*n` and likelihood margin
`512*epsilon*n*(1+2*log(hypot(1,range(y))))`. Elementary functions, standardized
coordinates and bisection signs use floating-point arithmetic. Neither the
root bracket nor the likelihood-gap formula is claimed as an outward-rounded
floating-point enclosure. This meets the certificate standard in the brief;
the margins are practical allowances, not a proved bound on every library's
rounding error.

Ties are genuine: for `(-a,a)` and held scale `s<a`, the two global maxima are
`+-sqrt(a*a-s*s)`. The binding reports "global maxima are tied or numerically
indistinguishable" and returns no estimate when candidates cannot be ordered.
It does not infer exact equality from rounded values or choose an arbitrary
mode. A flat quartic maximum, such as `(-1,1)` at scale one, instead exhausts
the numerical certificate and stops with `IterationFailure`. Neither outcome
claims mathematical non-existence: with scale held a maximum always exists.
There is no fallback to the previous unchecked search and no score-only rescue
of an unresolved global comparison.

`iterlim` limits each root solve and subdivision depth; at most `64*iterlim`
boxes are processed. Here `iterations` counts boxes. Standardized magnitudes
above `1e150`, nonfinite transformations, lost returned-location accuracy,
unresolved curvature/brackets and exhausted budgets stop rather than return a
capped estimate. Conditional tolerances are fixed and documented; `abstol`
remains the joint-fit step control. Structural `holds: []` remains unchanged.

The dated design choice is to bound the rational score directly instead of
building its degree-`2n-1` numerator and a floating-point polynomial remainder
chain. Growing degree and coefficient conditioning motivated this choice; no
claim is made that every possible fast polynomial method is unsuitable. The
old gap grid lacked an exhaustive bound and its many full-data evaluations
produced quadratic growth on distinct samples. The new method costs linear
work per full-data bound/score evaluation. Root comparisons can additionally
require pairwise hull checks; there is no uniform linear-time or universal
successful-certification theorem. Budgets turn difficult instances into explicit
failures. The tests and timings below establish practical performance on the
stated workloads through `n=100000`.

## Independent check and regressions

The retained [checker](2026-11-04-cauchy-sturm-data/check_held_scale.py)
uses only the Python standard library and a standalone C++17 driver. It reads
the old header with read-only `git show 54b0d06a:...`, and builds old, new native,
and new double-width-intermediate drivers under `build/scratch/cauchy-check`.

For five prescribed samples and 32 integer samples generated with seed `116`
(sizes 3–7, sampling without replacement from `-20:20`), it constructs the score
numerator using exact rational polynomial arithmetic. Exact Sturm variations
count and isolate **all real roots**, including examples with nine real score
roots. Root intervals shrink below `1e-14`; evaluating and ranking their
likelihoods is numerical, with `1e-10` comparison tolerance. The production
fitter agrees within `2e-8` in location and `1e-10` in log likelihood on unique
winners, and reports ambiguity on the prescribed tied examples. All 37 samples
passed in both precision modes: 74 comparisons. Exact root counting and these
numerical agreements are independent evidence, separate from the interval proof.

Appended C++ cases and R cases cover two-point analytical maxima, genuine ties,
the quartic flat maximum, iteration exhaustion, unrepresentable standardized
ranges, singleton and repeated observations, units `1e-300` and `1e300`, a
winning mode among separated clusters, held coefficients, and deterministic
Cauchy samples of size `100000`. Reflection gives an independent winning-side
argument for unequal two-point clusters. Existing golden, held-subset,
non-existence and independent multistart tests remain active. One existing
full-tier R assertion was extended to recognize the newly documented tie
error; it does not accept arbitrary numerical failures.

## Timings against the previous fitter

Single-run wall-clock seconds on the workstation, one CPU slot, GNU C++17
`-O2`, scale one, `iterlim=100`. Timings cover the fit (including its copy/sort)
and small result-formatting overhead, excluding driver compilation, data
construction and input parsing. They are workload observations, not stable
microbenchmark estimates.

| Workload | n | Old scan | New native | New double |
|---|---:|---:|---:|---:|
| Cauchy quantiles | 100 | 0.012700 | 0.000184 | 0.000218 |
| Cauchy quantiles | 1000 | 0.604049 | 0.001597 | 0.001593 |
| Cauchy quantiles | 100000 | not run | 0.112794 | 0.112119 |
| Unequal separated clusters | 100000 | 0.218977 | 0.037574 | 0.038017 |
| Tied separated clusters | 100000 | 0.217812 | 0.068106 | 0.067723 |
| Quantiles plus extreme outlier | 100000 | not run | 0.193694 | 0.193404 |

Quantiles are `tan(pi*((j+0.37)/n-0.5))`, `j=0,...,n-1`. Cluster workloads
use values `-10,10`, with multiplicities `40000,60000` or `50000,50000`.
The outlier workload appends `1e12` to
99999 quantiles. The new method used respectively 43, 11, 19 and 83 boxes for
the four large workloads. The tied-cluster times end in `AmbiguousMaximum`;
the old fitter silently returns one mode. Every other table entry returned
success. Distinct `n=100000` old scans were not timed: smaller distinct samples
already show the costly growth; large repeated-cluster baselines were measured.
Old and new objective sums use different accumulation, so tiny differences in
reported likelihoods are not interpreted as a better old maximizer.

## Final validation

All runs used the **local workstation**, never simbox. Environment: Linux
`7.0.0-38-generic`, x86_64 GNU/Linux; GNU C++ `15.2.0`; R `4.6.1`
(2026-06-24); BH `1.90.0-1`; Python `3.14.5`. The standalone checker needs no
third-party Python modules. The Justfile configured the shared compiler cache
and the existing BH headers for suite builds. CPU slot wrappers applied
`nice -n 10`; BLAS/OpenMP threads were pinned to one. Final commands used
worktree-local temporary directories and logs.

| Final command / check | Machine | Result |
|---|---|---|
| Retained independent checker | workstation, one slot | 74/74 small-sample comparisons; all timing workload status checks passed |
| `just cpp-test` | workstation, four slots | 106/106 dev shards passed |
| `just cpp-test-no-long-double` | workstation, four slots | 127/127 tests passed |
| `just test` | workstation, four slots | FAIL 0, WARN 0, SKIP 154, PASS 20538 |
| Focused Cauchy R, full tier | workstation, four slots | FAIL 0, WARN 0, SKIP 0, PASS 187 |
| `just test-full` | workstation, four slots | FAIL 0, WARN 0, SKIP 0, PASS 43353 |
| `UMVL_TEST_TIER=full just cpp-test` | workstation, four slots | 127/127 tests passed |
| `git diff --check` | workstation | passed |

The 154 R dev skips are tier exclusions, including the new large-sample case;
the full tier has none. There were no failed or permitted-failure tests. An
initial development C++ dev run also passed 106/106, before the final run above.
Existing CMake CMP0167 and compiler warnings in other family/shared sources
were emitted during builds; they were not test failures and those sources
were not edited. The final suite sequence exited zero at `2026-11-04T12:53:03Z`.

Reproduction from the repository root (do not nest slot wrappers):

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-cauchy-sturm-data/check_held_scale.py
/tmp/umvl-cpu/run4 bash -c '
set -e
just cpp-test
just cpp-test-no-long-double
just test
env UMVL_TEST_TIER=full \
  UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions" \
  PKG_BUILD_EXTRA_FLAGS=false Rscript -e '\''options(pkg.build_extra_flags=FALSE); testthat::test_local("univariateML-r",filter="dist-cauchy",stop_on_failure=TRUE)'\''
just test-full
UMVL_TEST_TIER=full just cpp-test
'
git diff --check
```

The actual final runner used that sequence with `set -e`, individual logs
`build/scratch/final-{cpp-dev,cpp-double,r-dev,r-cauchy,r-full,cpp-full}.log`,
and a timestamped summary `build/scratch/final-validation.log`. The final
independent log is `build/scratch/independent-final.log`. These ignored files
are supplementary; the retained checker, commands and results above are the
lasting reproduction record.

## Limits and tested source identity

No required implementation or validation item remains open. Numerical failure
on flat, crowded, poorly representable or budget-exhausting samples is part of
the documented two-outcome contract. Universal successful isolation, a uniform
runtime bound independent of mode geometry, exact tie recognition from rounded
likelihoods and outward-rounded arithmetic are not supplied. None is used as an
unstated premise for a successful certificate. No needed changes to the settled
d/p/q/r, special-function, truth or generator components were found in this job.

SHA-256 after the final suites; only this report was added afterward:

```text
80fc1184006118517ed0eb9306be9201f1aac9608c90895c685495aa14409718  univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp
404820bcc657ba8170e26911eedd315ea84be3eb5c9d2239644d5cec794b2195  univariateML-cpp/include/cppunivariate/distributions/cauchy/test.cpp
21d2b9d20ce24e91d99de824989b8c5ef094f5d258237c7db26040e2ddd1c0e1  univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md
1b605e3ad0b01722a6b358b3a1d66bb6edbc53f38b6176b8c0fef388980fc1a1  dev/research/evaluations/2026-11-04-cauchy-sturm-data/check_held_scale.py
0cd77595d2dcde80df32953cca357dd813d9b15a328dc64e18a0a6f4b7318a66  bindings/common/distributions/cauchy/fit.hpp
36310a06e024674c5aff109a68f4d7431cb95abd626178194da8b3c7b6b2465c  bindings/r/distributions/cauchy/test.R
```


## Artifact delivery

The prescribed `codex-math-web/bin/show` command was run on this report after
all validation finished. It returned HTTP 403 because the viewer's readable
roots exclude this worktree. The report remains at the requested path. No viewer
configuration, main-checkout file or other-worktree file was changed to bypass
that boundary.
