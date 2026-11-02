# TASK-116: certified outer shape search for discretized gamma

Tested base revision: `77a0aa214e1a51094204a72d6b9672c16fb1f27b`, with the uncommitted dgamma fitter, dossier, test and evaluation changes in this worktree. The golden fixture was not regenerated. All computations and final suites below ran locally; simbox was not used.

The free-shape fitter now uses its previous logarithmic grid and local refinements to initialize a branch-and-bound search. The search encloses the profiled likelihood on a finite log-shape interval, compares both tails with the best finite candidate, and either bounds the remaining improvement by `1e-8 * n` or returns `IterationFailure`. Held shape continues to use the strictly concave log-rate solve; both held parameters evaluate the likelihood. `fit.certified` is now `yes` under the package convention: the mathematical bounds are exact-arithmetic statements conditional on computed comparisons, with a practical floating-point likelihood margin. They are not outward-rounded interval arithmetic, and no uniform error bound is asserted for the special-function backend.

## Bounds and implementation

The [distribution dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md) gives the derivations and existence conditions. For small shape, the positive-cell probability bound and a lower bound on the gamma integral give an explicit cutoff uniform in rate. For large shape, gamma Chernoff bounds applied to two nonadjacent occupied cells give a finite cutoff when both parameters are free; a held-rate lower-tail bound covers the remaining held-rate cases. The constant and adjacent-cell unrestricted cases retain their existing point-mass and shifted-Bernoulli statuses.

On a bounded shape interval, the log of each unnormalized cell integral is convex in shape. Its shape derivative is the conditional mean of `log(Y)`, including in the zero cell; that derivative increases with shape and decreases with rate. Convex secants therefore enclose the profile score without assuming unimodality. Conditional means of `Y` increase with shape, giving rate-score bounds that enclose the optimal rate across a whole shape interval. Strict concavity in log rate supplies an upper bound for the effect of the inner solve's retained bracket. Subdivision uses the midpoint likelihood, this inner allowance and the worst slope over each half interval. The algorithm reserves `5e-9 * n` for the interval certificate and another `5e-9 * n` for the recomputed likelihood at the returned rate. It widens secant bounds by a practical cancellation allowance, stops on nonfinite or collapsed calculations, and returns `IterationFailure` after `1000 * iterlim` subdivisions. These checks do not establish shape uniqueness or rigorous error bounds for Boost's special functions.

The retained [bounds check](2026-11-02-dgamma-cert-data/check_bounds.py) passed 300 seeded cases, and the [secant check](2026-11-02-dgamma-cert-data/check_secants.py) passed 100 seeded quadrature cases. These numerical checks are not proofs. The C++ tests include finite-difference checks of score enclosures and failure behavior; R tests check the returned likelihood and failure propagation.

## Paired fit and timing checks

The [comparison driver](2026-11-02-dgamma-cert-data/compare.cpp) uses the preserved [original fitter](2026-11-02-dgamma-cert-data/legacy.hpp), `std::mt19937_64` seed `11620261103`, sample sizes 10, 30 and 80, generating shapes 0.15, 0.35, 1, 3, 10 and 50, and rates 0.25, 1 and 4. Its [results](2026-11-02-dgamma-cert-data/comparison.csv) contain 600 paired seeded fits and 20 paired timing fits. Reproduce the summary with `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-02-dgamma-cert-data/summarize.py`. Standard-library random-distribution sequences are compiler-library dependent.

All 600 status pairs agreed: 424 finite fits, 93 point-mass cases and 83 adjacent-cell cases. Among finite pairs, new minus old log likelihood ranged from `-2.8422e-13` to `1.7053e-13`; none lost more than `1e-8 * n`. There were no status disagreements to investigate. The golden C++ and R regression checks passed without changing the fixture. The [R replay](2026-11-02-dgamma-cert-data/check-r.R) used the reproduced [sample vectors](2026-11-02-dgamma-cert-data/samples.csv) and checked all 600 baseline statuses and likelihoods through the R binding: 424 finite fits and 176 expected boundary errors passed, with the same likelihood-difference range. `samples.cpp` reproduces those vectors.

For 20 paired `n = 500` samples generated with shape 2 and rate 1, median elapsed time was 46.06 ms for the original fitter and 306.39 ms for the certified search, approximately 6.7 times as long. All timing fits returned `Ok`. These are single-slot workstation measurements, not a general runtime guarantee; difficult intervals may exhaust the configured budget.

## Final checks

Workstation environment: Ubuntu, g++ 15.2.0, R 4.6.1, Node 22.22.1; the C++ benchmark used BH headers from the local R 4.6 library. The site check reported Python 3.14.5, SymPy 1.14.0 and its local R 4.5 BH include path. Commands used worktree-local `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR`; final suites ran individually through `/tmp/umvl-cpu/run4`, with BLAS/OpenMP pinned by the runner. The targeted C++ checks and paired computations ran through `/tmp/umvl-cpu/run`.

| Command | Local result |
|---|---|
| `just cpp-test` | 106/106 dev shards passed |
| `just cpp-test-no-long-double` | 127/127 shards passed |
| `just test` | 20,244 passed, 151 skipped, 0 failed, 0 warnings |
| `just test-full` | 41,847 passed, 0 skipped, 0 failed, 0 warnings |
| `just js-build` | passed |
| `just js-test` | 413/413 passed |
| `just site-check` | passed; 99 family pages, MathML, links and generated snippets checked |

The R replay was run after `just regen` with `/tmp/umvl-cpu/run Rscript dev/research/evaluations/2026-11-02-dgamma-cert-data/check-r.R`. The checks establish the reported agreement and integration behavior, not universal floating-point accuracy or a performance bound for all data.
