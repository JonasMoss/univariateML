# Existence batch 1 follow-ups — 2026-11-01

Source: `fd905f1b1c155ad03c6dd2305124f3156421788e` plus the uncommitted changes in this worktree. The outcome is limited by the two open items below; the site check did not pass.

## Changes

- Added machine-readable both-free, constant-data unbounded-likelihood records for lnorm, invgauss, invgamma and weibull. The records preserve the existing held-parameter records. The R conformance test checks the new records.
- The Weibull unbounded diagnostic identifies either a fitted or held scale. Logistic constant-data collapse reports an unbounded likelihood. R regressions cover both free and held cases.
- Lomax and inverse Pareto diagnostics and generated R help describe the profile search's tolerance-based comparison and recommend the appropriate limit model, without claiming that the detector proves nonexistence. R regressions inspect both the messages and help.
- The `ml_check_args` help title no longer ends with a period; the generated Rd title has a regression check.
- The negative-binomial fallback now returns `PrecisionFailure`, not `Underdispersed`, when a nonnegative score remains at its large-size search cutoff or a valid finite sign bracket cannot be established. The binding reports a precision failure rather than recommending `mlpois` in that case. Nonfinite midpoint scores also stop.
- The raw-moment variance can suffer cancellation for large tightly clustered counts. Before a raw-moment result is classified as underdispersed, the fitter recomputes a centered variance; if the two calculations disagree about overdispersion, it returns `PrecisionFailure`. For the two observations $m-k,m+k$ with $m=k^2-1$, the exact mean is $m$ and population variance is $k^2=m+1$. At $k=9743$ the original double raw-moment computation loses that excess. C++ and R regression tests check the resulting precision failure. The centered calculation is not an exact-arithmetic certificate for all inputs.

## Negative-binomial bracket and comparison

For a positive-mean overdispersed sample, the cited Aragón–Eberly–Eberly result gives a unique finite profile-score root. The score is positive near zero and, because its leading large-size term is proportional to mean minus population-form sample variance, negative for sufficiently large size. Consequently exact opposite score signs bracket that unique root. The previous existence audit verified the publication's identity and abstract but could not retrieve its full proof; this job did not independently audit it. The dossier separates this mathematical argument from the numerical implementation.

The comparison program is [`dev/validation/nbinom-existence-status.cpp`](../../validation/nbinom-existence-status.cpp). With GCC 15.2.0, C++17, `-O2`, the installed BH headers, `std::mt19937` seed 20261101, and `tol=1.220703e-4`, it compared the original header at the source revision above with the changed header. Its 47,342 samples comprised 1,000 seeded negative-binomial draws, 46,338 two-count samples with exact variance excess one (`k=2,...,46339`), and four near-equality samples of lengths 10, 100, 1,000 and 10,000. The two-count and near-equality samples used `maxit=1`; random samples used 100. Output status transitions were 30,611 `Ok` to `Ok`, 20 `Underdispersed` to `Underdispersed`, and 16,711 `Underdispersed` to `PrecisionFailure`. All changes were in the exact-variance-excess-one construction; none of the seeded random or four near-equality samples changed status. This is a regression comparison, not a proof that statuses are unchanged on all data. `std::negative_binomial_distribution` sequences are library-dependent.

Reproduce in this worktree after placing the baseline include tree in `build/scratch/old-include` and replacing its nbinom header with `git show fd905f1:univariateML-cpp/include/cppunivariate/distributions/nbinom/mle.hpp`:

```sh
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -std=c++17 -O2 -Ibuild/scratch/old-include -I"$boost" dev/validation/nbinom-existence-status.cpp -o build/scratch/nb-old
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/validation/nbinom-existence-status.cpp -o build/scratch/nb-new
build/scratch/nb-old > build/scratch/nb-old.txt
build/scratch/nb-new > build/scratch/nb-new.txt
python3 -c 'from collections import Counter; a=open("build/scratch/nb-old.txt").read().splitlines(); b=open("build/scratch/nb-new.txt").read().splitlines(); print(len(a),len(b),Counter(zip(a,b)))'
```

The actual comparison was run under `/tmp/umvl-cpu/run`, with one worker and `nice -n 10`; scratch files are ignored. The test tolerance above is the fitter tolerance, not an error bound on the computed score.

## Validation

All commands below ran individually under `/tmp/umvl-cpu/run4` with thread limits from the slot wrapper, `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` set to the worktree's `build/tmp`. Logs are under `build/scratch/` and are ignored. The tested compiler was GCC 15.2.0; the R build used the installed Rcpp and BH packages, and Node was v22.22.1.

| Command | Result |
|---|---|
| `just cpp-test` | Pass: 105/105 development shards. |
| `just cpp-test-no-long-double` | Pass: 126/126 tests. |
| `just test` | Pass: 19,865; 0 failures, 0 warnings, 151 skips. |
| `just test-full` | Pass: 41,311; 0 failures, 0 warnings, 1 skip (`sn` optional package absent). |
| `just js-build` | Pass; built the local WebAssembly artifact after the fitter change. |
| `just js-test` | Pass: 400/400. An earlier attempt before building the artifact failed because the generated module was absent. |
| `just site-check` | **Failed**: data generation succeeded for 98 families, then `astro: not found` stopped the site build. `site/node_modules` is absent; no packages were installed, as instructed. |

`git diff --check` passed. Tests check regression and internal behavior, not the existence theorem or interval accuracy.

## Open

1. **Numerical certification.** The Newton route still accepts its step-convergence criterion without a certified root enclosure; the fallback's floating-point score signs are not interval bounded. A rounded zero can be used as a bisection direction. A proof or independently validated score bounds, robust sign evaluation and a safeguarded enclosure (or a precision-failure status whenever they cannot be obtained) are needed to satisfy the requested numerical guarantee. The current centered-variance recheck also does not certify every possible input or platform.
2. **Cutoff regression.** No data reproducer reaching the exact `1e15 * max(1, mean)` branch was found. The large-count and near-equality checks exercise related cancellation and classification behavior, not that branch itself.
3. **Site validation.** Install or supply an existing compatible Astro/Pagefind dependency tree in an allowed location, then rerun `just site-check`. The present job explicitly forbade package installs and work in other worktrees; no shared toolchain was found and the requested path question received no answer during the run.
