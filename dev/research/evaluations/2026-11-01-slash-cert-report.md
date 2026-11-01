# Slash held-scale location certificate

The default held-scale location search now combines the previous grid and local
refinements with branch-and-bound over the sample range. It returns a result
whose log-likelihood gap is at most `1e-9 * (1 + abs(L*))` in exact arithmetic,
conditional on the computed likelihood, score and bound comparisons. `L*` is
the incumbent likelihood in the original coordinates. The margin is a
practical floating-point allowance, not a proved rounding-error bound or an
outward-rounded enclosure. Free-scale location search remains numerical.

The final paired comparison has 600 seeded samples and all four held subsets:
2,400 pairs, no status transitions, and no likelihood losses exceeding
`1e-12 * max(1, abs(old_ll))`; all log likelihoods were identical at printed
binary64 precision. The held-scale golden median increased from 33.395 ms to
35.492 ms (1.063 times). No golden values changed. This follows the conditional
certificate convention in the [hypergeometric report](2026-11-01-hyper-certs-report.md).

## Method and limits

The [slash dossier](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md)
gives the proofs. The sample range contains a maximum. The log likelihood is
Lipschitz with constant `n * sqrt(2) / sigma`. For standardized kernel `h`,
`h''(z) = -E(u^2) + z^2 Var(u^2) <= z^2/4`; its closed form also bounds
`h''(z)` by `2/z^2 + 1/expm1(z^2/2)` away from zero. These yield interval
bounds after summing and accounting for scale.

The existing observation-gap grid and golden-section refinements first supply
the incumbent, retaining the previous search's candidate. Consecutive grid
intervals cover the full sample range. The search prunes an interval using the
intersection of endpoint Lipschitz cones; otherwise it evaluates the midpoint
and can prune with the midpoint score and the local curvature bound. Remaining
intervals are bisected. A better midpoint is retained and its interval polished
using golden-section refinement. The best candidate is never replaced by a
lower likelihood. Pruning uses the stated margin in the original log-likelihood
coordinates. Up to one million intervals are processed; exhausting that budget,
nonfinite required calculations, inability to represent a new midpoint, or a
failed required refinement gives `IterationFailure`, rather than a partial
maximum. A nonfinite transformed held-scale result also fails. The binding
already maps non-OK statuses to an error; no binding change was needed.

The proofs describe exact arithmetic, conditional on the ordinary computed
comparisons. In particular, the small-argument score series, density evaluation,
normalization, transformations, and summation are not outward rounded, and the
margin is not a bound for all possible floating-point errors. The subinterval
budget may reject hard or extreme samples. The retained tests illustrate the
contract; they do not prove the floating-point condition.

For the free-scale inner root, an exact-arithmetic bracket was derived and is
recorded in the dossier. It is not substituted into the existing solver: its
lower endpoint approaches zero near a tied observation, and it does not bound
profile values uniformly near half-mass collapse. The existing fixed lower
scale bracket and outer grid remain numerical. A validated scale-root solve and
a uniform profile/collapse bound remain open for a free-scale certificate.
The [bracket check](2026-11-01-slash-cert-data/brackets.py) tests 600 generated
residual sets, including ties and scales from `1e-100` to `1e100`; its
[results](2026-11-01-slash-cert-data/brackets.csv) are numerical evidence for
the algebra, not fitter acceptance or a rounding proof.

## Paired acceptance and timing

[compare.cpp](2026-11-01-slash-cert-data/compare.cpp) compares the current
fitter against the frozen `65da739f` [baseline](2026-11-01-slash-cert-data/baseline.hpp).
`StdRng<>(13320261101ULL)` generates 150 samples for each of `n = 8, 32, 128,
500`; each size includes ordinary slash samples, quarter-mass ties, separated
groups shifted by ±30, and scales `1e-5` and `1e5`. Each sample exercises all
four held subsets using `tol=1e-9`, `maxit=200`, held location 2 and the
relevant generating scale. Mask bit 1 holds location and bit 2 holds scale.
All 2,400 old and new statuses were `Ok`: the list of status changes is empty.
No old-to-new likelihood loss exceeded the acceptance threshold; in fact no
likelihood changed. The sample set is a regression check, not a global proof.

[paired.csv](2026-11-01-slash-cert-data/paired.csv),
[golden.csv](2026-11-01-slash-cert-data/golden.csv), and
[summary.txt](2026-11-01-slash-cert-data/summary.txt) preserve results and
elapsed times. Reproduce with `bash dev/research/evaluations/2026-11-01-slash-cert-data/run.sh`
from the repository root; it acquires its own single-slot locks. The old fit
runs before the new one in each pair, so these measurements are subject to
order, CPU frequency, and other shared-load effects. Standard-library random
distributions and compiled results may vary by toolchain.

| n | Held subset | Before mean (ms) | After mean (ms) | After / before |
|---:|---|---:|---:|---:|
| 8 | scale | 0.0336 | 0.0504 | 1.500 |
| 32 | scale | 0.1981 | 0.2636 | 1.331 |
| 128 | scale | 2.0905 | 2.4952 | 1.194 |
| 500 | scale | 31.9163 | 34.8907 | 1.093 |
| 500 golden | scale, median of 3 | 33.3945 | 35.4923 | 1.063 |

The summary contains the other held subsets and the golden measurements. The
appended C++ tests exercise a separated-mode sample against a 20,001-point
likelihood scan, explicit subdivision-budget failure, and finite-result or
failure behavior at extreme coordinates. The scan is numerical, not a proof.
The focused slash run passed 14 test cases and 32,652 assertions before the
additional extreme-coordinate case; both final C++ suites include that case.

## Final validation

Tested base `65da739faecfaaa5728156aaec5c56d777e9397e` with the uncommitted
changes in this worktree. Final `mle.hpp` SHA-256
`b42001f70f97ea85a5af0115add0f15b2b21515f8ea1c8d8aaa61b70bc0952d6`,
`test.cpp` SHA-256
`48b64f2bdf609276de4f3c39e526a7d2347cbba819925503c9a7ea8a58917148`.
Linux 7.0.0-38-generic x86-64, GCC 15.2.0, R 4.6.1, Node 22.22.1; benchmark
compiled as C++17 with `-O2`. Final commands ran sequentially on the local
workstation through `/tmp/umvl-cpu/run4`; BLAS and OpenMP were pinned to one
thread and temporary files were under `build/tmp`. No failures are permitted.
Logs are under ignored `build/scratch/`.

| Command | Result |
|---|---|
| `just cpp-test` | 106/106 passed |
| `just cpp-test-no-long-double` | 127/127 passed |
| `just test` | 20,094 passed, 151 tier skips, 0 failures, 0 warnings |
| `just test-full` | 41,697 passed, 0 skips, 0 failures, 0 warnings |
| `just js-build` | passed; existing binomial self-comparison compiler warning |
| `just js-test` | 413/413 passed, 0 skipped |

No commits, board edits, package installs, generated d/p/q/r changes, or golden
edits were made.
