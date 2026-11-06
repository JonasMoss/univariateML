# NB-family certificates — TASK-116.5

`ztnbinom` and `zmnbinom` now declare `fit.certified: 'yes'`. Each successful numerical fit has a global score certificate and a signed coefficient-accuracy enclosure; unresolved arithmetic, additional extrema and exhausted budgets stop without returning an unchecked estimate. `zinb` remains unchanged and `certified: 'no'`: its held-inflation profiles require a separate global certificate. The exact open item and a conditional nonuniqueness result are below.

All required final commands passed, including both full tiers. All three families' goldens, densities, truth fixtures and generated d/p/q/r sources are unchanged. No commits, board edits, package installations, main-checkout edits or other-worktree edits were made.

The base revision is `164d41784756606a48ddc0cedc67752acab4050b`, branch `codex/nbfam-cert`. The final uncommitted source and retained evidence are identified by [source-sha256.txt](2026-11-06-nbfam-cert-data/source-sha256.txt), SHA-256 `ddc49d992265a2ec151afaa0876074c350414ec1b0d3e0bfb1451732ddcdc8e4`. [validation.json](2026-11-06-nbfam-cert-data/validation.json) records the environment, commands, machine, timings and counts. Production numerical source was frozen before the final runs.

## Implemented certificate and existence results

The canonical [ztnbinom dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ztnbinom/README.md) contains the proof, formulas, margins, stopping rules and decision history. The public entrypoint in [certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/ztnbinom/certificate.hpp) uses the former grid/refinement fitter only to propose coefficients. Its likelihood-near-tie rule never decides a limit.

For positive mean `m > 1`, probability is profiled by its strictly monotone conditional-mean equation. Write `t(r) = -log p(r)`, and `w_j` for the empirical share of observations above `j`. For the total profile log likelihood `Q`, the normalized size score is

```
F(r) = r Q'(r) / n = m J(t(r)) - H(r),
J(t) = 1 - t / expm1(t),
H(r) = sum_j j w_j / (r+j).
```

Probability-profile brackets, monotonicity of `t`, `J` and `H`, and explicit bounds for `F'` cover every remaining size interval. The initial interval includes size zero and uses the continuous logarithmic-series score. The infinite interval uses the analytic bounds

```
C/r - (m^2 lambda + m lambda^2/12)/r^2 <= F(r) <= C/r + D/r^2,
C = m lambda/2 - sum_j j w_j,
D = sum_j j^2 w_j,
lambda / (1-exp(-lambda)) = m.
```

These bounds follow from the conditional-mean equation, the inequalities `t/2-t^2/12 <= J(t) <= t/2`, and elementary bounds on the harmonic recurrence. They establish a final score sign without capping size or extrapolating a near-tie likelihood. The moment comparison has a margin scaled by the actual moment terms, independently of the initial-tail margin.

A successful joint interior fit requires coverage showing one decreasing score root, positive initial score and negative final score. A successful logarithmic endpoint requires a negative score throughout; a Poisson stop requires a positive score throughout. A minimum, extra extrema, a tangent or unresolved sign causes a numerical stop. Thus the algorithm certifies individual successful samples without assuming a general joint-unimodality theorem. General joint uniqueness remains open.

Two held-subset results are proved. At fixed finite nonnegative size, the probability estimate is unique by exponential-family conditional-mean matching, including the attained endpoint for all ones. At fixed `0 < prob < 1`, the size log likelihood is strictly concave: its second derivative is `-n/r^2 - sum_i sum_{j=1}^{x_i-1} (r+j)^(-2) + n t^2/(4 sinh^2(rt/2))`. The positive last term is strictly smaller than `n/r^2`. The size estimate is therefore unique, possibly at the attained logarithmic endpoint. Probability one and all-one data leave free size unidentified. Fully held coefficients need only likelihood evaluation.

The certificate first isolates an interior root to relative size width at most `5e-7`, with a log-probability span at most `5e-7`. A historical proposer outside that narrow enclosure survives only after separate padded positive/negative signs at size times `1 +/- 1e-6`, with log-probability span below `2e-6`. This preserves the existing goldens while bounding accuracy independently of the optimizer's objective plateau. The initial narrow-bracket prototype moved the truncated golden; [the retained comparison](2026-11-06-nbfam-cert-data/initial-golden-check.txt) and the independent root explain the signed proposer enclosure. No existing tolerance was relaxed.

The practical binary64 score margin is `1024*epsilon*(1+absolute_term_scale)*(1+recurrence_length*epsilon)`, with compensated sums. Mean comparisons use `256*epsilon*(1+m)`; small-argument functions have explicitly bounded Bernoulli-series remainders. Likelihood bounds get separate raw-term allowances. These are exact-arithmetic arguments conditional on the computed comparisons, with stated practical floating-point margins. They are not outward-rounded intervals or universal libm error proofs.

The outer budget is `16 * min(iterlim, 100000)` score/segment evaluations. Mean solves and proposer refinements have separate bounded loops. Counts above 100000, totals above `2^52`, unrepresentable coordinates and unresolved comparisons stop. These are numerical workload/resolution stops, not claims that a mathematical MLE fails to exist.

The [zmnbinom dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zmnbinom/README.md) inherits the positive-count certificate. Its independent zero-indicator maximizer remains the empirical zero share, or the supplied hold. The dedicated result transports the delegate's statuses, iteration count, failure reason, limiting coefficients and likelihood bounds across every hold subset. Indicator likelihood shifts use an arithmetic allowance. All-zero and default two-point representatives remain attained.

Poisson errors now carry `univariateML_limit`, model `ztpois` or `zmpois`, named limiting coefficients and `limit_loglik`. Supremum bounds evaluate the Poisson likelihood over its signed rate bracket, rather than treating a rounded rate as exact. Budget errors carry `univariateML_budget`; other numerical failures retain a precise reason and available likelihood bounds. The existing `Profile search exhausted` diagnostic prefix is preserved.

## Numerical evidence and practical limits

The independent [checker](2026-11-06-nbfam-cert-data/check.py) and [C++ probe](2026-11-06-nbfam-cert-data/probe.cpp) use seed 1165, 60-digit mpmath arithmetic, tolerance `1e-10` and default `maxit = 200`. [oracle.json](2026-11-06-nbfam-cert-data/oracle.json) retains all 89 workloads: the three golden positive-count samples with free/held variants, 48 seeded truncated-NB samples, 12 positive Poisson samples and 20 endpoint/near-face/held cases. The ZINB golden is used here only for its positive-count component; this does not certify ZINB's full mixture likelihood.

At the default budget, 69 calls returned certified finite or attained-endpoint fits, nine returned certified Poisson stops and 11 stopped on budget. All successful fits passed independent root and likelihood checks. Maximum relative size error was `4.78e-7`, maximum relative probability error `3.65e-7`, and maximum absolute total-log-likelihood error per observation `1.77e-13`. Acceptance thresholds were `2e-6` relative coefficient error and `1e-7` likelihood error per observation. Separately, 72 computed score/derivative interval bounds enclosed independent high-precision evaluations. These are numerical checks of the implementation; the global argument is the analytic coverage proof above.

Retrying all 11 budget stops with `maxit = 2000` certified nine more fits, using 3204–5965 outer evaluations. One positive Poisson sample and the near-face example still exhausted 32000 evaluations. The certificate is deliberately conservative near boundaries and does not guarantee completion at any fixed budget.

The near-face sample has 3958 ones, 4063 twos and 1979 fours. Its mean is two and its negative Poisson-tail coefficient establishes a finite maximum above the face. The historical fitter reports `PoissonLimit`; the certificate returns a budget stop instead. This removes a false non-existence report without pretending that the finite maximizer has been resolved. A separate C++ regression covers large-count equidispersion where the truncated-Poisson tail difference is exponentially small and binary64 cannot resolve its sign.

## Open: ZINB global certification

**ZINB production files and bindings remain unchanged.** Certification needs global coverage of all size/probability branches with held inflation, a certified same-inflation ZIP boundary, comparisons between competing maxima with a stated margin, and stop-detail propagation. Free-inflation reductions through the truncated and ordinary-NB fits do not settle those held-inflation problems. The existing ordinary-NB call nominates a candidate; it does not certify the outer mixture profile, and an unresolved required face cannot simply be dropped by a new certificate.

A monotone probability solve or blanket uniqueness assertion is insufficient. With `n0` zeros, `k` positives, count sum `S`, held size `r` and held interior inflation `pi`, stationary probabilities satisfy

```
E(p) = a [kr - (kr+S)p] + p^r [nr - (nr+S)p] = 0,
a = pi/(1-pi), n = n0+k.
```

Every root lies between `u = kr/(kr+S)` and `v = nr/(nr+S)`. Within that interval, `E(p)=0` is equivalent to `R(p)=a`, where

```
R(p) = p^r (nr+S)(v-p) / ((kr+S)(p-u)).
```

Its critical points solve the quadratic `r(v-p)(p-u) - p(v-u) = 0`. Hence there are at most three stationary probabilities. For 99 zeros, one positive count of 10 and held size 10, the two critical probabilities are approximately `0.565190282` and `0.875898827`; `R` there is approximately `1.094840941` and `4.077806882`. Between these levels there are three continuous stationary branches, with two maxima and one minimum. Outside `[u,v]` the likelihood derivative has a fixed sign, and its endpoint likelihoods vanish, so the two maxima are the only global contenders.

At held inflation 0.7, independent 60-digit calculations give:

| Probability | Type | Total log likelihood |
|---:|---|---:|
| 0.513044715 | maximum | -38.8973094872 |
| 0.720923481 | minimum | -39.5200204042 |
| 0.957450207 | maximum | -32.8450194550 |

The [crossover checker](2026-11-06-nbfam-cert-data/zinb-tie.py), run successfully on both machines, finds a tie near inflation `0.76839477568758077746`, with probabilities `0.5084685769193578` and `0.9333550301013148`, total log likelihood about `-29.9421386704954`. At inflation `1e-8` below and above that value, the low-probability minus high-probability peak heights are respectively `-7.28822015e-7` and `+7.28821960e-7`; [zinb-tie.json](2026-11-06-nbfam-cert-data/zinb-tie.json) records the roots and comparisons.

The root-count and continuity statements are analytic. Conditional on the checked endpoint peak-height signs, the intermediate value theorem establishes a tied global maximum for some held inflation in that bracket. The displayed tie location and roots are numerical approximations. This settles that uniqueness cannot hold generally for the held-size/held-inflation subset; it does not prove nonuniqueness of the fully free joint fit.

The remaining implementation must enumerate and bound these competing branches, their changes with free size, and the infinite-size face. Even an exhaustive fixed-size probability solve would not certify the current outer size mesh. The current `zip_mle` held-inflation path also uses a mesh and local refinement, so its successful return is not a substitute for that boundary certificate. These are the concrete unresolved global bounds and comparisons; outward-rounded arithmetic is not the blocker.

## Timings and final validation

[Timing source](2026-11-06-nbfam-cert-data/timing.cpp) and [raw measurements](2026-11-06-nbfam-cert-data/timing.txt) use C++17 `-O2`, one reserved CPU, three batches of 100 sequential fits to each 500-observation golden. Medians are:

| Family | Legacy proposer path | Certified/default fit |
|---|---:|---:|
| ztnbinom | 14.684 ms | 15.795 ms |
| zmnbinom | 10.544 ms | 11.646 ms |
| zinb | unchanged | 1.165 ms |

The first two overheads are approximately 8% and 10% on this workload. The modified-family baseline reconstructs the historical positive-count proposer and zero-indicator factor. These are shared-workstation batch measurements, not a general performance claim. The old proposer still expands observation-wise rising-factorial sums; large-count workloads can cost considerably more. The certificate additionally scales with the recurrence length and subdivision budget.

The workstation used Linux x86_64, GCC 15.2.0, R 4.6.1, Rcpp 1.1.2, BH 1.90.0-1, Python 3.14.5, NumPy 2.4.6, mpmath 1.3.0, Node 22.22.1 and Emscripten 6.0.13. Simbox used GCC 15.2.0 and R 4.6.1 through the supplied runner. All computing used the CPU reservations, nice scheduling and pinned BLAS/OpenMP settings. JavaScript tests and site checks additionally pinned all child processes to one CPU. No simbox exit-75 fallback occurred.

| Final command | Machine | Result | Compute/build elapsed |
|---|---|---|---:|
| [just cpp-test](2026-11-06-nbfam-cert-data/cpp-dev.txt) | simbox | 106/106 shards | 70.54 s |
| [just cpp-test-no-long-double](2026-11-06-nbfam-cert-data/cpp-no-long-double.txt) | simbox | 127/127 shards | 68.85 s |
| [UMVL_TEST_TIER=full just cpp-test](2026-11-06-nbfam-cert-data/cpp-full.txt) | simbox | 127/127 shards | 43.70 s |
| [just test](2026-11-06-nbfam-cert-data/r-dev.txt) | workstation | 20,685 pass; 0 fail/warn; 154 tier skips | 662.08 s |
| [just test-full](2026-11-06-nbfam-cert-data/r-full.txt) | workstation | 43,500 pass; 0 fail/warn/skip | 324.69 s |
| [just js-build](2026-11-06-nbfam-cert-data/js-build.txt) | workstation | pass | 72.25 s |
| [just js-test](2026-11-06-nbfam-cert-data/js-test.txt) | workstation (1 CPU) | 421/421; 0 fail/skip | 122.43 s |
| [just site-check](2026-11-06-nbfam-cert-data/site-check.txt) | workstation (1 CPU) | pass | 106.15 s |

The no-long-double command uses `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`. R dev's 154 skips are full-tier gates; the full run has no skips. There are no permitted failures in the final runs. The site run checked 99 family pages with build-time MathML, 99 C++17 snippets, inference and theory browser checks, relations and the binding assets. The 421 JavaScript tests reflect this base revision's suite, rather than the older 413-test baseline.

Times include regeneration/build work after reservation and exclude reservation waits. The initial local R build took longer than later cached regeneration. The JavaScript build spent roughly 117 minutes waiting for the shared four-slot reservation, then 72.25 seconds computing; its full wall time is retained in the metadata. No waiting command was interrupted. Read-only host diagnostics confirmed active competing builds/tests, not stale locks.

The first simbox R dev attempt had four diagnostic-prefix mismatches and one failure in the unchanged `mlst` scale-equivariance test. The prefix was restored, leaving existing tests untouched. The targeted R run then passed 135 checks, with eight full-tier skips, and the final local dev and full suites passed the same scale-equivariance check. [Initial failures](2026-11-06-nbfam-cert-data/initial-r-failures.txt) and [initial metadata](2026-11-06-nbfam-cert-data/r-dev-initial.json) are retained. The cause of the simbox-specific `mlst` discrepancy was not settled within this three-family scope; no `st` code or test was changed or skipped. Incidental host shell startup messages about a missing Cargo environment file did not prevent the final JavaScript/site commands from passing.

For reproduction, use the existing reservation wrappers and temporary-directory environment. The recorded [validation runner](2026-11-06-nbfam-cert-data/run_validation.py) runs one foreground command and records its result; [single_core.py](2026-11-06-nbfam-cert-data/single_core.py) pins the two single-slot checks. Independent checks can be rerun after compiling `probe.cpp` against the core and an installed plain Boost/BH include directory:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
# Compile probe.cpp as C++17 -O2 with -IunivariateML-cpp/include and a plain Boost include path.
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-nbfam-cert-data/check.py \
  build/scratch/nbfam-probe dev/research/evaluations/2026-11-06-nbfam-cert-data/oracle.json
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-nbfam-cert-data/zinb-tie.py \
  dev/research/evaluations/2026-11-06-nbfam-cert-data/zinb-tie.json
```
