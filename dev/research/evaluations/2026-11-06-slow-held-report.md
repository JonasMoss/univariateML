# TASK-133: slow held-subset fits

Bounded reuse of exact likelihood/profile evaluations speeds up held-scale fits in tlambda, EMG and Bhattacharjee. On the final paired n = 500 runs, sigma-held wall-time medians changed from **18.241 to 1.471 s**, **0.519 to 0.189 s**, and **0.650 to 0.392 s**, respectively. All **14,880 seeded comparison records** and **768 supplemental decision records** matched byte for byte. All final validation commands finished successfully.

The baseline is master `f30584cd1819744e130850bcb1e58a5d1c52ed28`; the final source is that revision plus the uncommitted changes in this worktree. [Source hashes](2026-11-06-slow-held-data/source-sha256.csv) identify the tested headers, dossiers, goldens and comparison harness. Only the three requested fitters and their dossier prose changed. No lst/std files, goldens, adapters, d/p/q/r, special functions, truth fixtures, code generators, NEWS, status files or roadmap files changed. No commits, board edits, installations or publication were performed.

The baseline census used all 99 families, all 640 held subsets, each family's exact 500-observation golden sample and current golden estimates as held values. It called the binding-neutral C++ fitters with package-default controls. Each call ran once on the workstation under one CPU-slot lock. These are wall times, including fitting and result construction; they exclude compilation and data loading. The [complete census](2026-11-06-slow-held-data/census-baseline.tsv) records every result and stop message. Of 640 calls, 576 returned fits and 64 stopped with diagnostics; these are fitter outcomes, not test failures. Every observed call above 0.5 s is listed below, including default fits for context.

| Family | Held | Before s | Outcome |
|---|---|---:|---|
| bhatt | none | 0.990 | ok |
| cmb | nu | 28.634 | stop |
| emg | none | 0.528 | ok |
| fpareto | min | 0.973 | stop |
| gb2 | shape2, shape3 | 0.537 | ok |
| ged | sd | 0.563 | ok |
| lst | sigma | 5.164 | ok |
| nhyper | none | 1.582 | ok |
| sged | sd, nu, xi | 0.788 | ok |
| slash | none | 6.306 | ok |
| snorm | mean | 0.792 | ok |
| sstd | mean | 0.931 | ok |
| sstd | mean, nu | 0.678 | ok |
| sstd | nu, xi | 1.994 | ok |
| sstd | sd, nu, xi | 0.526 | ok |
| st | none | 15.461 | ok |
| st | xi | 9.909 | ok |
| st | omega | 7.349 | ok |
| st | xi, omega | 3.288 | ok |
| st | alpha | 0.766 | ok |
| st | xi, alpha | 0.553 | ok |
| st | omega, alpha | 0.635 | ok |
| st | nu | 1.136 | ok |
| st | xi, nu | 0.713 | ok |
| st | omega, nu | 0.816 | ok |
| std | sd | 7.728 | ok |
| tlambda | none | 3.025 | ok |
| tlambda | mu | 0.739 | ok |
| tlambda | sigma | 15.331 | ok |
| tlambda | mu, sigma | 3.666 | ok |

The CMB nu-held call stopped after exhausting numerical work without excluding a larger finite maximum; it suggested CMP with nu held. The Feller–Pareto call selected the double-Pareto boundary. Neither row is a successful finite fit. The other stopped census calls and their reasons remain in the complete TSV. The threshold is an observed single-run threshold, not a distribution-wide timing guarantee.

The unchanged std sd-held and lst sigma-held package-default timings were **7.728 s** and **5.164 s**. A separate three-run historical-control sweep, using tol `1e-8` and maxit `1000`, gave baseline medians **7.928 s** and **6.745 s**; [all 108 measured subsets of the ten previously profiled families](2026-11-06-slow-held-data/baseline-timings.csv) are retained. These measurements use different controls from the package-default census. No speed-up is claimed for lst, std or any other unchanged family.

The final paired measurements below use `-O2`, tol `1e-8`, maxit `1000`, the golden n = 500 observations, and the frozen held values in [the existing comparison driver](../../validation/fit-speed-comparison.cpp). Those held values agree with the current goldens for the three modified families. Three repetitions alternate before/after binaries for each family and subset. Wall columns are medians of that series. CPU columns are medians from a separate three-repetition series using GNU time, user plus system CPU time, including process startup and JSON loading. They therefore must not be subtracted from the wall columns. The CPU timer resolves 0.01 s. [All 24 final subset pairs, wall-time ranges and outputs](2026-11-06-slow-held-data/final-timings.csv), [CPU measurements](2026-11-06-slow-held-data/cpu-timings.txt) and [their fit outputs](2026-11-06-slow-held-data/cpu-fit-outputs.txt) are retained.

| Family | Held | Before wall s | After wall s | Before CPU s | After CPU s |
|---|---|---:|---:|---:|---:|
| tlambda | none | 4.836 | 4.026 | 4.88 | 4.97 |
| tlambda | mu | 1.365 | 1.162 | 0.96 | 0.92 |
| tlambda | sigma | 18.241 | 1.471 | 20.12 | 2.62 |
| tlambda | mu, sigma | 3.847 | 0.671 | 4.25 | 0.65 |
| emg | none | 0.814 | 0.891 | 1.21 | 1.21 |
| emg | mu | 0.798 | 0.645 | 0.35 | 0.79 |
| emg | sigma | 0.519 | 0.189 | 0.74 | 0.50 |
| emg | lambda | 0.542 | 0.594 | 0.74 | 0.64 |
| bhatt | none | 2.013 | 1.146 | 2.35 | 1.94 |
| bhatt | sigma | 0.650 | 0.392 | 0.74 | 0.27 |
| bhatt | a | 0.528 | 0.510 | 0.74 | 0.81 |
| bhatt | mu | 0.546 | 0.683 | 0.69 | 0.68 |

All six timed outputs per subset had identical statuses, coefficients and likelihoods. Workstation scheduling and CPU-frequency variation are substantial: even unchanged default paths vary. The evidence supports gains on the sigma-held paths and tlambda with mu and sigma held; it does not establish a speed-up for every subset or for the default fits. In particular, the mu-held EMG CPU result worsened in this small timing series, while the width-held Bhattacharjee timings did not demonstrate a gain. These rows are retained rather than counted as improvements.

Local numerical work used `/tmp/umvl-cpu/run`; the helper pins BLAS/OpenMP to one thread and lowers priority with nice 10. Temporary files stayed in `build/tmp` and scratch in `build/scratch`. The workstation used Linux x86-64, GCC 15.2.0, R 4.6.1 and BH 1.90.0.1. The C++ suites on simbox used GCC 13.3.0; its R compilation used g++-15. All long final C++/R suites ran through `simbox-run`, without an exit-75 fallback. Site, JavaScript, relation, quality, census and paired-comparison runs used the workstation.

For tlambda, a per-start map reuses likelihoods only when sigma is held and the represented three-coordinate search point has already been evaluated. Repeated simplex shrink points can otherwise repeat hundreds of quantile inversions without changing the simplex. The all-free path retains its original likelihood evaluator. Starts, simplex ordering, arithmetic, shape faces, convergence tests, unresolved-start comparisons and iteration accounting are unchanged.

EMG and Bhattacharjee cache the complete physical-parameter profile evaluation within one fit, across its simplex starts. A cached entry contains the same location solution, likelihood and raw evaluation status. Fully free fits bypass the cache. The cache does not store optimizer results, so each optimizer still executes its original iteration budget, convergence and failed-optimizer score checks. Bhattacharjee's screening bounds, and both families' final face comparisons and structured diagnostics, are unchanged.

Each map holds at most 4096 entries; reaching that size clears it before insertion. This is a memory bound, not a bound on parameter search or numerical work. Cache clearing merely recomputes values later. NaN coordinates bypass caching to preserve ordered-map validity; zero coordinates also bypass it so that signed zeros cannot be coalesced. With data, held parameters and floating-point environment fixed, an identical nonzero coordinate tuple evaluates the same deterministic function in the same reduction order. Substituting its saved value therefore preserves each comparison. Induction over optimizer iterations preserves the search trajectory and its decisions. This establishes evaluation equivalence under those assumptions; the compiled comparisons below check its implementation. No approximation, pruning, early termination or new capped estimate is introduced.

The [profile counters](2026-11-06-slow-held-data/profile-counts.txt) independently show work removed on the golden workloads:

| Family | Held | Profile requests | Executed evaluations | Reused |
|---|---|---:|---:|---:|
| emg | sigma | 317 | 213 | 104 |
| emg | lambda | 305 | 256 | 49 |
| bhatt | sigma | 240 | 168 | 72 |
| bhatt | a | 246 | 215 | 31 |

The initial cache trial also covered fully free profiles, but found zero reusable evaluations among 298 EMG requests and 246 Bhattacharjee requests. That scope was rejected to avoid lookup/allocation overhead on the default path. The [rejected extension](2026-11-06-slow-held-data/rejected-all-free-cache.patch) and `initial-sweep-timings.csv` retain the experiment; its after timings are exploratory prototype results, not the final paired table. No special-function replacement or inversion warm start was attempted in this revision; the earlier rejected approaches and their reasons remain in the [October report](2026-10-31-fit-speed-report.md).

The seeded comparison uses seed `13320261031`, 200 samples at each of n = 8, 16 and 32, and 20 at n = 500, with all eight held subsets including none/all for each modified family. Samples are normal, with a quadratic perturbation every fifth sample. Fixed values, controls and sampling order are identical across binaries. The acceptance rule requires equal status, relative coefficient error at most `1e-8` and relative likelihood error at most `1e-12`, without an absolute floor; NaNs match only NaNs, and infinities only the same infinity. All rows met the stronger condition of identical emitted 17-digit records.

| Family | Records | Byte-identical records | Differences | Non-Ok outcomes in each binary |
|---|---:|---:|---:|---:|
| tlambda | 4960 | 4960 | 0 | 0 |
| emg | 4960 | 4960 | 0 | 1846 |
| bhatt | 4960 | 4960 | 0 | 1132 |

[Output hashes and status counts](2026-11-06-slow-held-data/equivalence.json) and both gzip-compressed output files per family retain the evidence. Non-Ok records remain boundary/failure outcomes; matching them does not turn them into successful fits. The primary driver emits coefficients, likelihood and status. It does not emit iteration counts or stop payloads.

The [supplemental driver](2026-11-06-slow-held-data/decisions.cpp) closes that recording gap on 768 additional records: all subsets and all three families, budgets 1 and 1000, seven constant/tied/extreme-scale/small-data samples and nine normal samples from seed `13320261106` at n = 8, 32 and 500. It emits iterations, tlambda restriction bits, and every EMG/Bhattacharjee stop-payload field and candidate. Both [before](2026-11-06-slow-held-data/decisions-before.txt.gz) and [after](2026-11-06-slow-held-data/decisions-after.txt.gz) records were identical, including forced low-budget failures.

Final validation used the final source after restricting caching to held profiles:

| Command | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 passed |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 127/127 passed |
| `just cpp-test-no-long-double` | simbox | 127/127 passed |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20822 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43638 |
| `just site-check` | workstation | passed; 99 family pages and C++17 snippets |
| `just js-test` | workstation | 430/430 passed |
| `just relations` | workstation | 157 records verified |
| `just quality tlambda emg bhatt` | workstation | all three passed |

The dev-tier skips are tier exclusions; the full tier had no skips. Existing compiler warnings and shell notices about a missing cargo environment appeared in logs; they are distinct from the zero R test warnings. The site check regenerated a tracked diagnostic screenshot; its original bytes were restored, leaving no unrelated figure change. [Final C++/R log](2026-11-06-slow-held-data/final-suites-confirmed.log.gz), [site/JS/relations/quality log](2026-11-06-slow-held-data/final-site-quality.log.gz), and [machine/result inventory](2026-11-06-slow-held-data/validation.json) are retained. No running command remains.

From the worktree root, `/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-06-slow-held-data/reproduce.sh` reconstructs the three baseline headers with read-only `git show`, builds both binaries, and repeats the timing, seeded and supplemental comparisons. The companion `census.sh`, `cpu-timings.sh`, `count-profiles.sh` and `summarize.py` retain the census, CPU, evaluation-counter and table-generation commands. The initial and final timing files distinguish exploratory and final runs. No source change is needed in the settled d/p/q/r, special-function, truth or code-generation components.

The mathematical guarantee added here is deterministic evaluation reuse, not a new global MLE theorem. Existing tlambda multistart mode coverage and the EMG/Bhattacharjee outer scale/shape searches retain the numerical limitations documented in their dossiers. Conditional concavity results, face bounds, floating-point comparison margins and failure-score rules retain their previous scope. The finite seeded and golden workloads do not prove equivalence under a changing rounding environment or establish a universal runtime bound.

Artifact display remains open: the requested `show` command returned HTTP 403 because the viewer's readable roots do not include this worktree. The report exists at the requested path; registering this worktree with the viewer is needed to display it in the pane. No viewer configuration or main-checkout file was changed.
