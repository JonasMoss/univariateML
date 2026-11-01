# TASK-116: fixed-shape profiles for sn, ged and sged

Source revision: `fd905f1b1c155ad03c6dd2305124f3156421788e`, with the uncommitted dossier and C++ example-test changes in this worktree. No production fitter, binding, golden or tolerance was changed. The requested default-fit implementation and its acceptance comparison remain **open**; the fixed-shape results below do not establish an outer-search certificate. In particular, the diagnostic comparison is not a before/after comparison of free fits and must not be treated as one.

## Mathematics and existing fitters

The three dossiers give the fixed-shape arguments and distinguish proved concavity from numerical searches. For `sn`, the existing free fit already solves a fixed-alpha concave location-scale problem and compares the two half-normal faces. Its finite alpha grid and floating-point Newton/score check do not certify a global free-shape maximum. Replacing its inner solver with the normal-convolution solver would not itself add an outer certificate; no speed improvement was established.

For `ged`, the existing conditional fit has a monotone location score (a median at shape one) and an analytic power-mean scale profile; the default free fit nevertheless uses multistart BFGS and compares the Laplace and uniform faces. A concave inner solve can improve the location-scale part, but an uncapped, certified free-shape search and its acceptance behavior have not been established. For `sged`, fixing finite skew and tail shape at least one gives a concave inner problem; below one the kernel is not log-concave. The default free fit varies both shapes with multistart BFGS and compares skew-Laplace, half-GED and uniform limits. Holding only tail shape does not reduce this to the one-dimensional EMG outer profile.

A further analytic reduction for `sged` profiles the two side scales for fixed mode and shape. With $A=\sum_{x_i<c}(c-x_i)^\nu$, $B=\sum_{x_i>c}(x_i-c)^\nu$, and $p=1/(\nu+1)$, the profiled objective decreases with $A^p+B^p$ when $A,B>0$. This does not prove unimodality in mode or a bound on the remaining shape search. The shape-one kink and vanishing-side or infinite-shape limits need separate treatment. No numerical search based on this reduction was installed as a default.

## Fixed-shape diagnostic, not free-fit acceptance

[Driver](2026-11-01-logconcave-2-data/inner-check.cpp), [summary script](2026-11-01-logconcave-2-data/summarize.py), and records: [sn](2026-11-01-logconcave-2-data/sn-inner.csv), [ged](2026-11-01-logconcave-2-data/ged-inner.csv), [sged](2026-11-01-logconcave-2-data/sged-inner.csv). With seed `11620261101`, 600 samples per family: 200 each at n=8, 32, 128. GED generating shapes were 1, 1.2, 2, 4, 8; SGED additionally cycled skew 0.5, 1, 2; SN cycled alpha -8, -2, 0, 2, 8. Fits used tolerance `1e-8` and maxit 1000. The driver calls the unchanged default fitter. For successful finite GED fits it recomputes the conditional profile at the fitted shape; for successful SGED fits with shape greater than one it solves the fixed-shape/skew side-weighted location score by bisection and profiles scale. SN's reported inner likelihood is simply copied from its current fit, not an independent check.

| Family | Existing status counts (enum integer) | Finite inner comparisons | Minimum inner minus old | Maximum inner minus old | Losses beyond relative tolerance |
|---|---|---:|---:|---:|---:|
| sn | Ok 312; right half-normal 149; left half-normal 139 | 312 | 0 | 0 | 0 |
| ged | Ok 301; uniform 299 | 301 | -1.99e-13 | 4.72e-8 | 0 |
| sged | Ok 221; BFGS failure 5; uniform 197; right half-GED 88; left half-GED 89 | 158 | -1.99e-13 | 2.76e-7 | 0 |

The threshold is `1e-12 * max(1, abs(old logLik))`, as in round four. GED boundary statuses and SGED boundary, failure and shape-one results are excluded from the finite inner comparison. No status changes were measured because there is no new free fitter. These results give numerical evidence about selected inner solutions, not a global proof, a guarantee on likelihood error, or the requested 600-sample old/new free-fit acceptance.

Golden-data single-run baseline timings on one shared CPU slot, c++ 15.2.0, `-O2`, n=500: SN 2.615 s, GED 0.0415 s, SGED 0.406 s. [Timing code](2026-11-01-logconcave-2-data/golden-time.cpp) and [records](2026-11-01-logconcave-2-data/golden-timing.csv). There is no after timing: the production fitters are unchanged. These are single wall-clock observations, not a controlled performance study.

To reproduce, set `TMPDIR`, `JUST_TEMPDIR`, and `XDG_RUNTIME_DIR` to `$PWD/build/tmp`, create that directory, and compile each driver with `/tmp/umvl-cpu/run c++ -std=c++17 -O2 -IunivariateML-cpp/include -I/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include <driver> -o <executable>`. Run each family through `/tmp/umvl-cpu/run <executable> <family>` from the repository root. Run the summary with `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-01-logconcave-2-data/summarize.py`. All shared-slot wrappers pin BLAS/OpenMP to one thread and apply `nice -n 10`.

## Final validation

Each command ran separately through `/tmp/umvl-cpu/run4`, with temporary directories under `build/tmp`. Logs are under `build/scratch/`. The example tests exercise selected concavity inequalities and a below-one counterexample; they do not prove concavity or optimizer correctness.

| Command | Result |
|---|---|
| `just cpp-test` | pass, 105/105 |
| `just cpp-test-no-long-double` | pass, 126/126 |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19843 |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41289; optional R package `sn` unavailable |
| `just js-build` | pass |
| `just js-test` | pass, 400/400 after build |
| `git diff --check` | pass |

The first `just js-test` failed because the generated WebAssembly module did not exist in this worktree. Building it with `just js-build` and rerunning the test resolved that failure.

## Open work

- Develop a certified or otherwise task-compliant outer search for free GED shape and SGED tail shape/skew, including Laplace, uniform and half-family limits, with explicit numerical-failure behavior when certification fails. Check SN's numerical outer search against the same requirement. The preceding inner arguments do not settle any of these points.
- Integrate a candidate into the default fit only after that search and the existing held-subset and boundary behavior are resolved. Then run at least 600 paired old/new free fits per family across shapes and sample sizes, enumerate and explain every status change, check the specified likelihood-loss tolerance, and compare golden timings. Preserve existing goldens unless a replacement has strictly higher likelihood.
- These are implementation and acceptance gaps, not merely missing documentation. The current free fitters retain their pre-existing numerical search behavior and have not been upgraded or certified by this work.
