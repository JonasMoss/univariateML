# Truncation route fixes from the estimated-bound review

TASK-459 · `codex/fam-truncroutes` · base `d7f83bb4c49339a117d27c8cda35db3a23ec632b`

| Route | Result on the exact audit sample |
|---|---|
| Normal, dyadic sample on `[-1,1]` | Certified finite MLE with mean zero and scale approximately 528.791407249585. The result also holds with mean fixed at zero. Holding scale at two gives a certified location fit at zero. |
| Negative binomial, `(1,2,2)` with size two | Certified upper limit with size two, integer cuts one and two, and masses `(2/5, 3/5)`. The log likelihood is `log(18/125)`. |
| Binomial, `(1,1,2)` on `[1,2]` | Certified non-unique maximum. The typed relation covers every integer size `N ≥ 2` with `prob = 1/N`, and every member has log likelihood `log(4/27)`. |

The C++ and public R regressions replay all three estimated-cut patterns for each sample. The exact arithmetic and independent scale calculation are reproducible with [check.py](2026-11-28-fam-truncroutes-data/check.py). The samples and existence arguments are from the [exact audit inputs](2026-11-27-trunc-est-math-report.md#exact-audit-inputs), the [reviewed implementation checks](2026-11-27-mod-rev5-report.md#evidence-and-implementation-checks), and the [modifier proof rows](../existence-atlas/modifier-proofs.csv).

## Normal truncation

At the mean-matched linear face the free quadratic score is proportional to the empirical variance minus the face variance. A negative score gives a strict inward improvement. A positive score and concavity give an excluded maximum at the face. With mean held, the analogous comparison uses the empirical and uniform mean squared distances from the held mean. With scale held, the scalar location equation has a root whenever the empirical mean is interior to the window.

The fitter no longer uses the height allowance of `1e-8` per observation to select a face. The finite-window rate bracket bounds the normalized density ratio by `exp(±δ)` when the tilt lies within `δ` of the evaluated tilt in unit-window coordinates. Minimizing the second moment over its center gives the same bounds on the variance ratio. The comparison adds a binary64 arithmetic margin and an allowance for summing the empirical squares. The one-sided exponential comparison uses the squared mean distance from the finite cut. When a global enclosure does not close, the existing checked local result can still return uncertified.

The near-face refinement evaluates the score in two-component arithmetic. It integrates the finite-window exponential series on `[-1,1]`, where the degree-80 tail is bounded by Cauchy's estimate on the circle of radius four. On a ray it integrates the alternating quadratic series against the exponential, bounding the tail by the integrated first omitted term. On the refinement domains, the model density dominates a positive multiple of uniform on `[-1,1]` or `[0,1]`. This gives lower bounds `2.9e-5` and `5e-4` on the information, respectively. Twice the padded score norm divided by the lower bound gives the enclosing radius. The accepted enclosure lies at negative quadratic tilt and its coefficient images meet the public tolerance. The arithmetic pad is in binary64 and the certificate uses the package's conditional floating-point standard. [Face decision](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp) · [Near-face refinement](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_near_face.hpp)

For the dyadic sample the exact variance gap is `2097151/13194139533312`. Its gain is bounded above by `1.44e-13` per observation, so a height-based comparison at the old threshold loses the finite maximum. Two existing C++ modifier expectations changed for the same reason.

- The one-sided grid at variance ratio `0.9999` now expects a certified finite fit. Its inward variance gap is approximately `1.09026222683e-4`.
- The finite-window grid at shrink `1` now expects a certified finite fit. Its inward gap is approximately `2.84622958002e-5`.

The adjacent grid cases with the opposite sign still require their exponential limits. [Modifier tests](../../../univariateML-cpp/tests/modifiers/tnorm.cpp)

## Count windows

The negative-binomial boundary at zero probability has weights `(r)_k/k!` on the retained integer window. The mean increases with the natural parameter by the conditional variance, so `8/5 < 5/3` proves the boundary maximizes the limiting likelihood on the audit sample. For positive size the payload identifies the law by its size and retained cuts. Its height is evaluated relative to the largest weight. The prefix identity `Σ(k=0…j) (r)_k/k! = (r+1)_j/j!` provides a wide-window likelihood when the recurrence reaches its work cap. The size-one payload remains the discrete uniform. [Count status mapper](../../../univariateML-cpp/include/cppunivariate/distributions/geom/truncation.hpp) · [Negative-binomial weights](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/truncation.hpp)

For a window retaining `L, L+1` with both counts observed, write `A = (L+1) n_(L+1) / n_L`. Matching the empirical odds gives `(N-L)p = A(1-p)`. Each integer `N ≥ L+1` reaches the saturated two-cell likelihood. The `adjacent_binomial` relation stores the integer axis, lower count and reduced rational `A` in the typed maximizer set. R and JavaScript serialize the relation with the same coordinates and exact rational level. [Binomial fitter](../../../univariateML-cpp/include/cppunivariate/distributions/binom/tbinom_mle.hpp) · [Maximizer-set schema](../../design/maximizer-sets.md)

## Verification

Every build and test ran in the job worktree on the workstation through `run4`, with BLAS and OpenMP pinned to one thread per worker. The arithmetic replay used one slot.

| Check | Result |
|---|---|
| Native C++ via `just test-changed --no-long-double` | 136/136 targets passed |
| Double-only C++ via the same recipe | 181/181 targets passed |
| Focused R tests for norm, nbinom, binom and geom | 534 passed, four expected tier skips |
| `just test-full` | 36 truth checks passed. R passed 68,250 assertions with zero failures, zero warnings and one existing skip. |
| `just js-build` and `just js-test` | Build succeeded and 601/601 tests passed |
| Exact replay and `git diff --check` | Passed |

The first native iteration compiled the old grid expectations before their corrections. After the current C++ checks passed, the development R run exposed a missing `size = NULL` in the new binomial test call. The correction touched only the test invocation. The focused and full R reruns passed, and the JavaScript checks ran separately because the combined recipe had stopped at R.

## Open

`just site-check` generated data from 86 dossiers and prepared the site binding. It then stopped at `astro: not found` with exit code 127. The worktree has no `site/node_modules`. The Astro build and post-build site validation require the missing site dependencies.
