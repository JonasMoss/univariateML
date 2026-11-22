# Short-window conditional normalization

TASK-228, worktree `codex/short-window`, based on `32fc18bd`.

## Evaluation

R and JS enumerate finite geometric and negative-binomial windows with at most 256 retained counts below `2^53`. They intersect the window with the count support before enumerating. Enumeration also covers short windows whose total mass is resolvable by parent tails, because an interior CDF cut can still require subtracting nearly equal tails. The geometric case at `prob = 1e-5` and `[100000, 100010]` exercises this distinction. [R evaluator](../../../bindings/r/R/modifiers.R) · [JS evaluator](../../../bindings/js/modifiers.mjs)

The evaluator anchors the largest retained log PMF and stores the log masses relative to it. In exact arithmetic the adjacent negative-binomial ratio is `P(k+1)/P(k) = (1-p)(size+k)/(k+1)`. Geometric uses `size = 1`. The implementation evaluates the log ratio as `log1p(-p) + log1p((size-1)/(k+1))`, using `log(size)` for the second term at zero to retain even very small positive sizes. It scans cumulative ratios to select the peak and accumulates outwards from it. The parent's log-scale d/p/q kernel supplies the absolute PMF at the peak. The generic path without an adjacent ratio scans the parent's log PMFs directly, which also handles point masses.

Walking ratios avoids subtracting absolute log PMFs of magnitude tens or hundreds of millions. Each sum shifts by its own maximum, then adds `log1p` of the remaining exponentials. The anchor cancels before conditioning. The smaller conditional tail is summed directly and the larger tail is recovered with a stable complement. The inverse compares those conditional tails with the existing discrete quantile band and uses comparisons without that band for sampling. JS reuses the prepared weights across searches for a vector and compensates both the ratio walk and the sum of shifted masses. A plain JS sum moved some natural-scale quantiles one count above the exact jump near the end of the 256-count geometric window. The added regression uses oracle survival probabilities at sampled jumps in that window. The ratio and normalization identities are exact in real arithmetic. The replay measures the binary64 error on the supplied cases.

## C++ placement and family coverage

The five discrete families with implemented closed-window modifiers are geometric, negative binomial, Poisson, binomial, and zeta. Poisson and binomial already share a native retained-mode evaluator in R and WASM. It walks adjacent ratios with compensated accumulation and checks the geometric remainder before omitting terms. Finite zeta already shares a native harmonic evaluator with mode-relative weights. Its configured support is `[1,N]` and includes finite exponents for which the infinite parent is improper. These three native paths cover the short cases in the replay. [Poisson and binomial kernel](../../../univariateML-cpp/include/cppunivariate/distributions/pois/tpois_dpqr_core.hpp) · [Finite zeta evaluator](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp)

One C++ implementation would be preferable for the remaining duplicated evaluation. The geometric and negative-binomial C++ window routes present here are fitter routes. Their public finite-window d/p/q operations are composed in the bindings from parent d/p/q calls, so this change lives at that existing seam. Consolidation would require a native conditional evaluator taking a parent log PMF and optional log-ratio callback, building one relative-weight state per parameter vector and window, and exposing vector operations through the Rcpp and WASM modifier interfaces. The existing zeta entry points provide a model for the binding interfaces. Such an evaluator would also need to carry the two tail conventions, the public and sampling inverse comparisons, and the zero-mass and unresolved-window outcomes.

## Independent comparison

The 100-digit mpmath reference extends the prior log-gamma calculation. The fixture checks d/p/q on natural and log scales and both CDF and quantile tails. Cases include negative binomial with size `1e8` and probability `.5` on `[2,12]` and `[0,5]), geometric with probability `.3` on `[1e9,1e9+10]` and at the 256-count cap, and the geometric interior guard described above. Poisson with rate `1000` and binomial with size `3000` and probability `.5` both use `[2,12]`. Their log window masses are approximately −937.08 and −2003.37. The zeta case uses exponent `−30` and `[1,12]`. [Reference generator](2026-11-22-short-window-data/reference.py) · [Log normalizers](2026-11-22-short-window-data/log-masses.txt) · [Shared fixture](../../../bindings/r/tests/testthat/fixtures/short-window.tsv)

The d, p and q columns give the maximum relative JS difference from R across the probes on each window. Matching zeros and infinities contribute zero. All 208 quantiles matched exactly in both bindings and the oracle. The separate oracle columns pool all operations. [R replay](2026-11-22-short-window-data/replay.R) · [JS replay](2026-11-22-short-window-data/replay.mjs) · [Comparison script](2026-11-22-short-window-data/compare.py)

| Window | d | p | q | R vs oracle | JS vs oracle |
| --- | ---: | ---: | ---: | ---: | ---: |
| nbinom-left | 1.42e-16 | 0 | 0 | 1.12e-14 | 1.12e-14 |
| nbinom-zero | 1.32e-16 | 0 | 0 | 1.12e-14 | 1.12e-14 |
| geom-remote | 1.38e-16 | 1.66e-16 | 0 | 6.02e-16 | 6.02e-16 |
| geom-interior-guard | 0 | 2.04e-16 | 0 | 3.49e-16 | 3.49e-16 |
| geom-at-cap | 1.85e-16 | 1.85e-16 | 0 | 6.25e-15 | 6.25e-15 |
| pois-tiny | 0 | 0 | 0 | 6.39e-15 | 6.39e-15 |
| binom-tiny | 0 | 0 | 0 | 1.12e-14 | 1.12e-14 |
| zeta-finite | 0 | 0 | 0 | 5.78e-15 | 5.78e-15 |

The former geometric guard expectation on `[1e9,1e9+10]` in each binding now checks the first conditional mass `.3/(1-.7^11)`. The JS guard test also checks refusal at 257 geometric counts. Both suites check refusal for a 257-count negative-binomial window with size `2` and probability `.3`. The oracle gives a finite log mass for that event. The JS test also compares seeded short-window draws against the geometric finite-sum inverse. [R modifier tests](../../../bindings/r/tests/testthat/test_modifiers.R) · [JS reference test](../../../bindings/js/test.mjs) · [JS guard test](../../../bindings/js/fit-test.mjs)

## Validation

All final runs used the workstation `JonasBI` with GCC 15.2.0 and R 4.6.1. Simbox returned exit 75 for the full R suite, which ran locally under the four-slot reservation.

| Check | Result |
| --- | --- |
| `just test-changed --no-long-double`, native C++ stage | 134/134 |
| `just test-changed --no-long-double`, double-only C++ stage | 179/179 |
| `just test-changed --no-long-double`, R development stage | 36,997 passed, 157 skips, zero failures or warnings |
| `just js-build` in the changed-family runner | Passed |
| `just test-full` | Truth checks 36/36. R full suite 66,432 passed, one skip, zero failures or warnings |
| `just site-check` | WASM rebuilt, 85 family pages verified, 85 C++ snippets compiled and linked |
| `just js-test` after the site WASM build | 588/588, including the geometric guard and jump regressions |
| Modifier-route snapshots against the stored 2026-11-15 baselines | 1,950 exact matches per mode. [Native](2026-11-22-short-window-data/routes-native.txt) · [Double-only](2026-11-22-short-window-data/routes-double.txt) |

The first changed-family run reached the JS suite and failed on the old geometric refusal expectation. After updating that expectation and compensating the JS sums, the focused regressions passed and the standalone JS suite passed against the site build. The late changes were confined to JS.

## Open

Wider generic windows still use the parent-tail evaluator and can reach its resolution guard. The 257-count negative-binomial fixture has log mass about −356,674,924 and exercises that boundary. Resolving such a case would require a wider relative summation or another family-specific interval calculation.
