# Faster skew-normal fitting, 2026-11-01

The default free fit is 1.58 times faster on the golden sample: three-run median 0.361516 s before and 0.229162 s after. Across 612 seeded samples and all eight held subsets (4,896 paired fits), statuses, log likelihoods and profile-evaluation counts are identical. All six required final commands passed. No golden files were changed; no commits were made.

The tested source is worktree `codex/sn-speed`, based on `f9092af03bdaa982e9abc21f1a527db2d3f3113b`, with the uncommitted changes to `sn/mle.hpp`, `sn/test.cpp` and dossier prose. Tested `mle.hpp` SHA-256: `5ff2b2384ab6e4394d1d5bad460da4d54e2e8ac9137d44f12824ac55acf66f44`. The original header SHA-256 was `f4de5af684fe52746b00708457529b67306e28a427a079e6a632731ab4df7fd8`. The retained [baseline.hpp](2026-11-01-sn-speed-data/baseline.hpp) freezes that fitter with only include paths and symbols adjusted for paired compilation. All work ran locally with GCC 15.2.0, C++17, `-O2`, the installed BH headers and R 4.6.1. CPU work used the shared slot wrappers, which apply `nice -n 10` and pin BLAS/OpenMP to one thread; final suites each used `run4`. No simbox, package installs, board edits or external worktree changes were used.

The inner fitter already used analytic Newton steps in inverse scale and scaled location. The installed change caches log-Phi values at the accepted objective point. For `alpha*z < -10`, one evaluation of the existing 24-level continued fraction supplies the log-CDF, inverse Mills ratio and its derivative; the next Newton step reuses them. The objective accumulation, approximation, tail threshold and derivative formulas are unchanged. With location and scale both held, the unused derivative pass is omitted. The public iteration count continues to count profile evaluations.

A line-search trial overwrites every cache entry only for positive inverse scale. The next Newton iteration starts only after acceptance, so its cache describes the accepted point. A failed line search or iteration exhaustion still invokes the existing independent returned-point score check. The objective, initialization, tolerances, iteration limits, grid, refinement and face comparisons are unchanged. Memory cost is three temporary double vectors of length n (24n payload bytes) per inner solve; rejected objective trials also compute tail derivatives that may not be reused. The appended C++ test recomputes returned-point likelihood and score independently of the cache across both signs, alpha zero, shapes up to absolute value 1,000 and all four location/scale hold patterns. These are numerical checks, not a global optimality proof.

Warm starts and a coarser shape grid were considered but not adopted: preserving the existing search and status behavior would require separate evidence, and neither supplies a global shape certificate. Earlier concavity work is recorded in the [log-concavity evaluation](2026-11-01-logconcave-2-report.md). Changing log-Phi approximations was unnecessary once duplicate evaluations were removed. Early face decisions were not adopted: the existing face work is negligible next to millions of inner special-function evaluations, and a safe early decision would require an upper bound for the remaining finite-shape profile.

The counter-instrumented golden run uses default controls (`tol=1e-9`, `maxit=100`) and holds coefficients at their golden estimates. [profile.csv](2026-11-01-sn-speed-data/profile.csv) contains totals, and [profile-points.csv](2026-11-01-sn-speed-data/profile-points.csv) records iterations, objective evaluations, log-Phi evaluations and tail recurrences per shape evaluation. The free search visits 159 grid points and makes 43 refinement evaluations. Fixed scale takes 243 inner solves. Fixed location bypasses the Newton solver: its scalar shape solve makes 28,000 Mills-ratio calls on this sample, followed by 500 log-density evaluations. Each free-shape fit visits 1,000 observations across the two faces; held shape omits those comparisons.

| Held | Inner solves | Newton iterations, old → new | Objective calls | log-Phi evaluations, old → new | Tail recurrences, old → new |
|---|---:|---:|---:|---:|---:|
| none | 202 | 2893 → 2893 | 5762 | 4,303,453 → 2,874,500 | 35,094 → 17,547 |
| xi | 0 | 0 → 0 | 0 | 500 → 500 | 0 → 0 |
| omega | 243 | 3080 → 3080 | 11693 | 7,362,930 → 5,846,500 | 47,140 → 23,570 |
| xi, omega | 0 | 0 → 0 | 0 | 500 → 500 | 0 → 0 |
| alpha | 1 | 5 → 5 | 5 | 5,000 → 2,500 | 0 → 0 |
| xi, alpha | 1 | 1 → 1 | 1 | 1,000 → 500 | 0 → 0 |
| omega, alpha | 1 | 5 → 5 | 5 | 5,000 → 2,500 | 0 → 0 |
| xi, omega, alpha | 1 | 1 → 0 | 1 | 1,000 → 500 | 0 → 0 |

Objective calls include trials with invalid inverse scale, which return before evaluating observations. Log-Phi and tail counts cover objective and Newton passes; the separate independent score/shape Mills calls are recorded separately in the CSV. The free fit averages 14.3 Newton iterations per inner solve; the trace includes wing solves reaching the existing 100-iteration budget. This profile identifies repeated inner special functions and backtracking objectives as the principal work; it does not assign exclusive wall time to individual functions. The optimization reduces evaluation cost rather than the number of solves or iterations.

The paired workload in [compare.cpp](2026-11-01-sn-speed-data/compare.cpp) uses `StdRng<>(13320261101ULL)`: 17 samples for every combination of n = 8, 32, 128, 500 and generating alpha = -1,000, -30, -8, -2, 0, 2, 8, 30, 1,000 (612 samples). Each sample runs all eight held subsets with default controls; held coefficients are the generating xi = 0, omega = 1 and alpha. All finite and boundary results are included. Raw records are in [paired.csv](2026-11-01-sn-speed-data/paired.csv), with executable assertions and timing summaries in [summarize.py](2026-11-01-sn-speed-data/summarize.py) and [summary.txt](2026-11-01-sn-speed-data/summary.txt).

The acceptance threshold is `old_ll - new_ll <= 1e-12 * max(1, abs(old_ll))`. There are zero losses at that threshold, zero likelihood differences at 17-digit output precision, zero status changes and zero profile-evaluation-count differences. Aggregate statuses in each version are Ok 3,742; right half-normal boundary 581; left half-normal boundary 573; InvalidInput 0; IterationFailure 0. Every status change is therefore enumerated by the empty set. The golden free-fit likelihood is unchanged at -456.82400184138714. No golden updates or strictly better golden replacements were needed. This is paired regression evidence against the frozen fitter, not an independent oracle or proof of global optimality.

Mean elapsed time over 153 samples per n, including face outcomes:

| n | Free old (s) | Free new (s) | Speedup | omega held old (s) | omega held new (s) | Speedup |
|---:|---:|---:|---:|---:|---:|---:|
| 8 | 0.004292 | 0.002512 | 1.71× | 0.004811 | 0.003536 | 1.36× |
| 32 | 0.018173 | 0.011010 | 1.65× | 0.026050 | 0.020468 | 1.27× |
| 128 | 0.065525 | 0.039921 | 1.64× | 0.120505 | 0.099028 | 1.22× |
| 500 | 0.306796 | 0.191836 | 1.60× | 0.643775 | 0.544220 | 1.18× |

Speedups by held subset (ratio of mean elapsed times); the fixed-location scalar solver is unchanged, and its roughly one-percent differences are timing noise rather than an algorithmic gain:

| Held | n=8 | n=32 | n=128 | n=500 |
|---|---:|---:|---:|---:|
| none | 1.71× | 1.65× | 1.64× | 1.60× |
| xi | 1.05× | 1.02× | 1.01× | 1.00× |
| omega | 1.36× | 1.27× | 1.22× | 1.18× |
| xi, omega | 1.04× | 1.02× | 1.00× | 1.01× |
| alpha | 1.86× | 1.88× | 1.89× | 1.87× |
| xi, alpha | 1.70× | 1.81× | 1.84× | 1.85× |
| omega, alpha | 1.80× | 1.32× | 1.50× | 1.21× |
| xi, omega, alpha | 1.75× | 1.99× | 2.05× | 2.08× |

Golden-data timings hold coefficients at the golden estimates, with three paired repetitions per subset. These are workstation elapsed-time observations with old always timed before new, not a randomized performance experiment. CPU frequency and other shared work can affect absolute times. The records [golden.csv](2026-11-01-sn-speed-data/golden.csv) use default controls:

| Held | Old median (ms) | New median (ms) | Speedup |
|---|---:|---:|---:|
| none | 361.516040 | 229.161665 | 1.58× |
| xi | 1.877540 | 1.870504 | 1.00× |
| omega | 473.835612 | 367.558572 | 1.29× |
| xi, omega | 1.808629 | 1.793689 | 1.01× |
| alpha | 0.327119 | 0.173336 | 1.89× |
| xi, alpha | 0.069241 | 0.039778 | 1.74× |
| omega, alpha | 0.319716 | 0.170173 | 1.88× |
| xi, omega, alpha | 0.067751 | 0.032364 | 2.09× |

The earlier report's approximately 2.6 s golden timing used `tol=1e-8, maxit=1000`, rather than the default controls. Repeating those controls gives median 2.060960 s before and 1.761199 s after (1.17×), in [golden-legacy.csv](2026-11-01-sn-speed-data/golden-legacy.csv). Those 24 paired fits also have identical statuses, likelihoods and profile-evaluation counts. Their speedup is smaller because retaining many more iterations leaves more backtracking-objective work relative to reusable derivative work.

The final commands below each finished successfully through `/tmp/umvl-cpu/run4`, after any slot wait. [test-results.txt](2026-11-01-sn-speed-data/test-results.txt) preserves their outcomes and source identity; detailed local logs are under `build/scratch/`.

| Command | Final result |
|---|---|
| `just cpp-test` | 105/105 |
| `just cpp-test-no-long-double` | 126/126 |
| `just test` | FAIL 0; WARN 0; SKIP 151; PASS 19843 |
| `just test-full` | FAIL 0; WARN 0; SKIP 1; PASS 41289 |
| `just js-build` | pass, exit 0 |
| `just js-test` | 400/400; fail 0; skipped 0 |

R development counts match the supplied baseline. The full R tier's sole skip is the optional comparison against the unavailable R package `sn` (`test-dist-sn.R:136`); no package was installed. The first C++ invocation failed to compile the new test because this doctest version accepts one argument per `CAPTURE` call. Splitting the captures fixed it; the subsequent complete C++ runs above passed. There are no permitted test failures or unfinished commands.

Reproduce the paired sweep and profiling from the repository root with `bash dev/research/evaluations/2026-11-01-sn-speed-data/run.sh`. It uses the installed Boost headers, builds drivers under `build/scratch/`, and runs CPU work through the slot locks. The profiling script generates counter-instrumented header copies only under `build/scratch/`; no counters or printing enter production headers. Temporary directories for all runs were set to `$PWD/build/tmp` via `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR`. Then run each final command in the table separately through `run4`. Do not wrap `run.sh` in another slot wrapper because it acquires slots itself.

Open mathematical guarantee, inherited from the original fitter: the finite free-shape grid and local refinement still do not prove that no larger maximum lies between sampled points or beyond the logarithmic wings. The missing result is a global profile bound or a complete root/isolation argument, including the approach to both half-normal faces. Fixed-shape concavity and returned-point score checks do not supply it. This patch adds no new numerical search, truncation, stopping rule or claim of global certification. The speed implementation and requested finite-sample acceptance are complete; a global certificate remains separate open work.
