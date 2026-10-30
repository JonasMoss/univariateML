# huberspeed review report

No commits, Git writes, board edits, package installs, or sub-agents.
Only Huber sources and this report changed. Existing golden is unchanged.

## Files and implementation

- `univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp`:
  cache normalized sorted observations, prefix sums/squares and the MAD;
  use exact sufficient statistics above n=4096 for the entire original grid
  and refinement of every nominated maximum; warm-start consecutive fits.
  No quantile compression is needed: each reduction costs O(log n).
  Polish the winning bracket with the original full reductions, reusing
  normalized data and initialization. For expensive cold Newton trajectories,
  recover with prefix sums after roughly 16 million observation visits
  (at least 12 iterations), then check the original full-data scores.
  Keep the original solver as the final fallback.
  The trailing C++ `exact_scan` switch runs the original full scan for tests;
  no R API changes. Fixed-epsilon fitting keeps its original algorithm.
- `univariateML-cpp/include/cppunivariate/distributions/huber/test.cpp`:
  five n=4097 samples (Gaussian, Huber quantiles, rounded/tied Huber,
  contaminated Gaussian and Laplace), all four held-coordinate subsets:
  20 comparisons to the full scan, with 1e-8 parameter tolerances.
  Also 81 configurations comparing prefix losses, scores and curvature
  against explicit reductions, including tied observations and kinks.
- `codex-report.md`: this report.

## Mathematics: proved algebraically

For z=a*y-b, split sorted observations at (b-c)/a and (b+c)/a.
Let k,S,Q be core count, sum and sum of squares; D and T are signed
right-minus-left tail count and sum. The exact loss is

    (a*a*Q - 2*a*b*S + b*b*k)/2 + c*(a*T-b*D) - c*c*(n-k)/2.

The score sums are a*Q-b*S+c*T and a*S-b*k+c*D; the quadratic
loss Hessian has entries Q,-S,k, with half curvature at exact kinks.
Holding location m transforms the scale terms by gp -= m*gh and
Q -> Q-2*m*S+m*m*k. These are sufficient statistics of the full sample,
not an approximation to a compressed distribution.

Gaussian/Laplace face calculations, comparisons, tie slack, proved
cutoffs, grid spacing and refinement stopping width are unchanged.

## Checked numerically

- `just test`: 4353 passed, 0 failed, 0 warnings, 0 skips. An early
  overlapping package regeneration interrupted one build; the successful
  run was sequential.
- `just quality huber`: 1/1 passed.
- `UMVL_GOLDEN_IDS=huber just goldens`: 1 golden regenerated, no diff.
- Optimized C++17 (`g++ -O2`), one thread under the shared slot lock,
  seeded Huber(2,3,0.8) draws, generation excluded:

  | n | Free-epsilon seconds |
  |---:|---:|
  | 10,000 | 0.02135 |
  | 100,000 | 0.18815 |
  | 1,000,000 | 5.56272 |

  Reproduction source: `/tmp/umvl-codex/huberspeed/random-bench.cpp`,
  mt19937 seed 7931, sequential draws across sample sizes. Logs and
  benchmark executables are in that same scratch directory.
- Final `just cpp-test`: 5/5 ctest targets passed (53.97 s).
  The 20 above-threshold comparisons all passed, including status,
  likelihood and 1e-8 parameter checks. The two new cases cover 20 fits
  against the original scan and 486 scalar reduction checks (81 x 6).
  A redundant Huber-only rerun was cancelled while waiting for CPU slots;
  the full C++ suite above includes all eight Huber test cases.
- `git diff --check`: clean. All builds, tests and nontrivial computations
  used `/tmp/umvl-cpu/run` (one thread, nice priority, no parallel builds).

## Assumptions and open questions

- The inherited grid search assumes no winning peak fits between grid
  points without being nominated; no new unimodality theorem is used.
- Exact algebra does not imply bitwise equality of floating-point reductions.
  Long-double prefixes reduce cancellation; final full reductions preserve
  the historical rounding convention near the selected maximum.
- Existing small-sample trajectories are retained. At large n, switching
  a stalled cold solve to prefix recovery may move a nearly flat numerical
  maximizer beyond 1e-8 even with indistinguishable likelihoods. The strict
  compatibility claim is the tested samples, not every possible dataset.
- Searches over sparse scale mixtures did not find a robust multimodal
  profile; no multimodal-profile fixture is claimed.
- Timing comparisons to the earlier 177 s report use different generated
  samples; generation is excluded and optimized single-thread C++ is used.
