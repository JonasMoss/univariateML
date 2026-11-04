# TASK-116: certified all-free Lomax fit

The all-free production fitter now returns a globally certified stationary
maximum to its stated coefficient accuracy, or stops. The exponential outcome
requires a global upper bound within the declared margin; an unresolved search
returns a budget or arithmetic failure. The default golden coefficients and
golden fixture are unchanged within the existing 1e-10 regression tolerance.

Tested source is base revision 54b0d06a3e447f944940fc0dd4b5b87235d2acc2 on
codex/lomax-cert plus the uncommitted family and binding changes. The retained
[validation record](2026-11-04-lomax-cert-data/certificate-validation.json)
contains SHA256 fingerprints of the exact fitter headers and benchmark
reproducer. Those fingerprints were checked again after every final suite
finished. No commit or Git write was made.

The canonical [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md)
contains the existence criterion, proof, arithmetic allowances, controls and
failure behavior. The implementation is in
[mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/mle.hpp)
and [certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/certificate.hpp).
The existing held-subset algorithms are unchanged.

Put $y_i=x_i/\bar x$, $z=\log(\lambda\bar x)$,
$A=\overline{\log(1+e^z y)}$, $B=\overline p$ and
$C=\overline{p(1-p)}$, with $p_i=\operatorname{logistic}(z+\log y_i)$.
The unique conditional maximizer is $\kappa=1/A$. The per-observation
profile gap from the fitted exponential is

$$
 h=z-\log A-A,\qquad
 h'=1-B/A-B,\qquad h''=B^2/A^2-C/A-C.
$$

The global argument is analytic; no general unimodality or joint uniqueness
theorem is assumed.

- Since $C\le B\le A$ and $C\le1/4$, $h''\ge-5/4$.
  Endpoint chords plus $5(b-a)^2/32$ therefore bound every interval above.
  For small $q=e^z$, the sharper lower curvature bound
  $-qM_2/[2(1-qM_2/2)]-q$, where $M_2=\overline{y^2}$,
  prevents excessive subdivision near the exponential endpoint.
- The complete left tail satisfies
  $h\le-\log(1-qM_2/2)$. At $q=10^{-10}/\max(1,M_2)$, its
  upper bound is below $5.1\,10^{-11}$ before arithmetic allowances.
  With $L=-\overline{\log y}$, the complete right tail satisfies
  $h\le L-\log(z-L)$ for $z>L$, hence is below the exponential
  for $z\ge L+e^L$. These are tail theorems, not search caps.
- Intervals are removed by likelihood upper bounds or by a uniformly nonzero
  profile score. The latter exclusion follows from differentiability: an
  interior global maximum must have zero score, including at an artificial
  subdivision boundary. Monotonicity of A and B encloses the score; a verified
  negative curvature bound also permits endpoint-score exclusion.
- A winning neighborhood requires strict negative curvature throughout and
  opposite signed endpoint scores. Bisection gives a root bracket narrower
  than $7\,10^{-7}$ in log rate. Overlapping neighborhoods handle uncertain
  score signs on subdivision boundaries. Every remaining interval must be
  excluded before a finite fit is returned. A replacement winner must also
  dominate the previous root's upper value; the additional 1e-12 allowance
  exceeds the center-to-root value error from the narrow bracket.

These steps certify the global root in exact arithmetic conditional on the
computed enclosures and comparisons. Exact duplicate compression preserves
multiplicities; no log-bin approximation decides the result. The earlier
grid nomination route has been removed from the all-free decision path.

The practical binary64 allowance is

$$
 \eta=256\epsilon\{1+|\log x_{\max}|
 +|\log\overline{x/x_{\max}}|+\max_i|\log y_i|\}(1+n\epsilon),
 \qquad \epsilon=2^{-52}.
$$

Compensated sums are used. A gap receives allowance
$\eta(1+|z|+|\log A|+A)$, positive moments receive relative allowance eta,
and score and curvature decisions require an additional 4 eta margin.
The dossier states the elementary-function assumptions, including relative
accuracy for positive exp/log1p results before underflow. This is the package's
practical conditional arithmetic contract, not an outward-rounded interval
proof or a universal libm error theorem.

Stored coefficients must round-trip to their proposed log coordinates within
1e-8. The historical Newton rate must additionally remain inside the enlarged
root bracket after physical-unit restoration. These checks catch positive
subnormal results with inadequate relative precision while preserving accurate
subnormal rates. Historical coefficients are retained only inside the verified
coefficient neighborhood, preserving the default golden. The bracket, restoration
and retention tolerances keep coefficient errors below approximately 1e-6
conditional on the arithmetic allowances.

The public outcomes are:

- Finite: a root is isolated, beats the exponential margin, and every competitor
  is excluded.
- Exponential: the global profile improvement is bounded by 1e-8 per observation.
  This policy can reject a mathematical finite MLE that ties or improves by less
  than the margin; it recommends mlexp.
- Failure: unresolved comparisons, normalization or restoration precision,
  representability, or exhausted work limits stop without coefficients.

The global budget is 128 times min(iterlim,100000) subdivision evaluations.
Each root-isolation attempt has at most iterlim refinement passes and requires
at least two successful passes. The reported iterations count covers global
subdivision evaluations, excluding the separately bounded root refinements.

The [binding diagnostics](../../../bindings/common/distributions/lomax/fit.hpp)
distinguish budget and arithmetic failures through an appended diagnostic field;
the existing LomaxStatus values are preserved for delegates. The R help retains
limits.numerical for its tolerance-aware template. Removing it would incorrectly
assert mathematical non-existence for a finite winner inside the allowed margin.
The fit metadata records certified: yes, and the help explicitly describes the
certified global upper bound.

The retained [reproducer](2026-11-04-lomax-cert-data/check_certificate.py)
checks 17 workloads: 14 finite outcomes and three exponential outcomes. It solves
the finite scalar scores independently at 80 decimal digits using the exact
binary64 input observations, checks coefficients to relative tolerance 5e-7,
and checks total likelihood to 1e-8 times sample size. It separately requires
the unchanged golden coefficients within 1e-10. These high-precision solves
are numerical corroboration; the analytic bounds and conditional enclosures
provide the global certificate.

The largest observed relative coefficient discrepancy was below 7.86e-8.
It comes from retaining the historical default Newton solution: its rate
differs from the high-precision stationary root by 7.86e-8 and its shape by
5.35e-8. Golden regression accuracy and error relative to the stationary
root are distinct checks. The new targeted C++ run passed all 20 cases and
9348 assertions, including exhaustion, extreme rescaling, unrepresentable
rates, subnormal intermediate ratios, inaccurate restoration and a valid
positive subnormal rate. The final R full tier also includes the existing
220-sample independent profile/density oracle and held-subset comparisons.

The before/after benchmark compiles the complete unchanged baseline fitter from
the recorded base revision and the replacement into the same C++17, -O2 binary.
Each entry is the mean of five calls, on the local workstation through one
CPU slot at nice priority 10; compilation and lock waiting are excluded.

| Workload | Baseline ms | Certified ms | Global evaluations |
|---|---:|---:|---:|
| Golden, n=500 | 5.372 | 0.799 | 43 |
| Seed 116 Lomax sample, n=10000 | 8.865 | 16.448 | 40 |
| Shallow maximum (1,1,1,12,12) | 0.055 | 0.142 | 336 |
| Repeated (1,29), n=6000 | 0.559 | 0.127 | 53 |
| Shallow sample scaled by 1e307 | 0.078 | 0.134 | 336 |

The golden workload is faster; the 10000-observation workload and shallow
five-point maximum are slower. The JSON retains all workloads and timings.
The generated sample uses Python seed 116; all other inputs, scalings and
replications are encoded in the reproducer or the unchanged golden fixture.

Every final command below finished green on the local workstation, sequentially
inside one /tmp/umvl-cpu/run4 reservation. BLAS/OpenMP used one thread per
worker, builds used at most four threads, and processes inherited nice priority
10. The dev skips are the expected tier exclusions; the full tiers had no
skips or permitted failures.

| Command | Final result |
|---|---|
| just cpp-test | 106/106 shards passed |
| CTEST_PARALLEL_LEVEL=4 just cpp-test-no-long-double | 127/127 shards passed |
| just test | FAIL 0, WARN 0, SKIP 153, PASS 20515 |
| UMVL_TEST_TIER=full just cpp-test | 127/127 shards passed |
| just test-full | FAIL 0, WARN 0, SKIP 0, PASS 43332 |

The final source also passed fingerprint, JSON, Python syntax, file-scope,
local-report-link and whitespace checks. No d/p/q/r, special-function seam,
truth fixture, code generator, golden, other family, NEWS, status or roadmap
source was changed. No package installation, simbox, sub-agent, board edit,
commit or publication was used.

Environment: Linux-7.0.0-38-generic-x86_64-with-glibc2.43; c++ (Ubuntu 15.2.0-16ubuntu1) 15.2.0; Python 3.14.5, mpmath 1.3.0; R 4.6.1, Rcpp 1.1.2, BH 1.90.0.1, testthat 3.3.2, roxygen2 8.0.0.

Reproduce the retained independent check from the worktree root:

    mkdir -p build/tmp build/scratch
    export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
    export XDG_RUNTIME_DIR="$PWD/build/tmp"
    /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-lomax-cert-data/check_certificate.py --output dev/research/evaluations/2026-11-04-lomax-cert-data/certificate-validation.json

The fitter and its C++ tests remain independent of R. The comparison helper
uses Git only to read the pinned historical header, and mpmath only for the
independent validation.

Artifact display remains blocked by viewer configuration: the prescribed show
command returned HTTP 403 because this worktree is outside the viewer's
configured readable roots. Display requires a viewer allow-root update outside
this job's permitted worktree. The report and retained evidence remain at the
requested paths. No viewer configuration was changed and no report copy was
placed outside the worktree.
