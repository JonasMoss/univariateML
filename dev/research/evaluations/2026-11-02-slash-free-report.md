# TASK-116: free-scale slash location certificate

2026-11-02. Tested source: working tree based on `58832d7548156236a276f3b2c93f879f82639fd9`, including the uncommitted slash changes recorded below. No commits or pushes were made. All final runs used the workstation through `/tmp/umvl-cpu/run4`, sequentially; simbox was not used.

## Result

The free-scale location search now uses branch-and-bound on the sample range. The existing observation-gap grid and refinements provide its initial incumbent. It returns a finite fit only after the interval bounds have closed the likelihood gap; exhausting the one-million-interval budget, failing a required scale solve, or being unable to subdivide an unbounded interval returns `IterationFailure`. The existing half-mass preference selects collapse when the boundary is within `1e-8 n` of the best interior value. The conditional score comparisons and the practical margin are not outward-rounded interval arithmetic.

The derivation and complete implementation conditions are in the [slash dossier](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md#conditional-search-certificate). `fit.certified` is now `yes`, under the conditional-comparison convention used by this package. The proof is an objective-gap certificate and does not assert unimodality or isolation of every mode.

For an interval, the score evaluated at the nearest and furthest possible residuals gives uniform lower and upper scale-root bounds whenever the requisite signs are obtained. A positive lower bound gives the profile the held-scale Lipschitz constant `n sqrt(2) / sigma_lo`. A second bound follows from the profile curvature identity and explicit bounds on the location curvature, location/log-scale cross derivative, and negative log-scale curvature. The latter uses `g'(t) >= exp(-2t)` for the increasing scale-score term. At or near a half-mass point, a separate residual likelihood bound also applies when a positive uniform lower scale is unavailable: the nearest residuals dominate each density term, and their scale supremum is finite at exactly half mass. It is infinite if more than half of the nearest residuals are zero; subdivision then continues or fails closed. These arguments cover odd and even sample sizes.

The objective margin is `1e-9 (1 + |logLik|)` in the original coordinates, capped at `1e-9 n` if a half-mass boundary exists. While the current interior incumbent is within the collapse preference, the interval cutoff is the boundary plus `1e-8 n`; otherwise it is the incumbent plus its margin. Scale bisection brackets contribute an explicit likelihood error bound. The margin is a practical allowance for computed comparisons, not a proven bound on accumulated floating-point or libm error. Outward-rounded arithmetic and a rigorous machine-error bound remain open; neither is claimed here.

## Comparison and regression evidence

[compare.cpp](2026-11-02-slash-free-data/compare.cpp) constructs 600 deterministic samples with `std::mt19937` seed `1162026`, including rounded samples, near-half ties, and half-mass samples with nine observations at 9, 10, or 11, ten zeros and one observation at -1. The original fitter from the tested base revision and the final fitter produced identical statuses on all 600 samples and identical reported log likelihoods on all successful pairs. The saved [original results](2026-11-02-slash-free-data/baseline.csv) and [certified results](2026-11-02-slash-free-data/certified.csv) give sample number, size, status, location, scale and log likelihood. The random-distribution sequence is tied to this C++ standard-library implementation; it is not a cross-library reproducibility claim. The slash golden was not changed.

An earlier version returned `IterationFailure` on one rounded sample because it attempted to bisect two adjacent represented grid values before trying their interval bound. Checking the bound first fixed the case; both C++ and R regression tests retain the sample. The first C++ dev run also exposed failure on the symmetric half-mass sample `(0, 0, -1, 1)` when the quadratic bound became too loose near collapse. Applying the residual bound as a fallback fixed that case. The [original failed-shard excerpt](2026-11-02-slash-free-data/initial-cpp-shard-failure.txt) and the subsequent green runs record the change. An earlier residual-bound-only [prototype](2026-11-02-slash-free-data/residual-bound-probe.py) took 52,583 subdivisions on five observations; it is a numerical diagnostic, not a proof or the implemented algorithm.

C++ tests cover the residual bound against conditional profiles, an exhausted subdivision budget, adjacent floating-point grid points, the golden and pre-existing half-mass cases. R tests cover half-mass and near-half-mass multimodality and, in the full tier, 600 additional seeded continuous samples against fixed candidate likelihoods and direct density sums. Those R candidate checks are not independent proofs of global optimality; the certificate is mathematical and conditional on computed comparisons.

## Runtime

The [benchmark source](2026-11-02-slash-free-data/benchmark.cpp) generates 500 slash observations with `std::mt19937` seed `116`, using a standard normal divided by a uniform draw. Both builds used `g++ -O2 -std=c++17`, the same BH headers and the same local single-slot command. In the final paired run the original took **30.108 s** and the certified fitter **69.689 s** (about **2.3 times** as long); both reported `mu = -0.080494160573387818`, `sigma = 1.0006407564716753` and `logLik = -1424.1925253056531`. These are single measurements on a shared, loaded workstation, not a performance distribution. An earlier, less-loaded run measured 2.93 s for the original; absolute times therefore vary substantially with contention.

To reproduce the comparison, extract the base `mle.hpp` into a scratch header, rewriting its relative `core/` and `dpqr.hpp` includes to `cppunivariate/core/` and `cppunivariate/distributions/slash/dpqr.hpp`, respectively. Compile `compare.cpp` or `benchmark.cpp` twice with `-IunivariateML-cpp/include`, the installed BH include directory, and `-DSLASH_HEADER` naming the base scratch header or `cppunivariate/distributions/slash/mle.hpp`. Run each binary through `/tmp/umvl-cpu/run`; output is CSV for the comparison. Scratch files belong in `build/scratch/`.

## Final verification

Environment: Linux workstation, g++ 15.2.0, BH headers from the local R 4.5 library, Python 3.14.5 and SymPy 1.14.0. Temporary/runtime directories were set to `build/tmp`; OpenMP and BLAS were pinned by the slot wrapper. All of the following final commands ran locally, one at a time through `/tmp/umvl-cpu/run4`:

| Command | Result |
|---|---|
| `just cpp-test` | PASS, 106/106 dev shards |
| `just cpp-test-no-long-double` | PASS, 127/127 tests |
| `just test` | PASS, 20,418 passes, 152 skips, 0 failures, 0 warnings |
| `just test-full` | PASS, 43,221 passes, 0 skips, 0 failures, 0 warnings |
| `just js-build` | PASS |
| `just js-test` | PASS, 413/413 tests |
| `just site-check` | PASS, including 99 family pages, MathML, links, graph and C++ snippet checks |

The dev-tier skips are tier exclusions; the full tier had no skips. The shell printed an existing missing `~/.cargo/env` startup message during JavaScript commands, without affecting their exit status or tests.

For identification of the tested uncommitted source, SHA-256: `mle.hpp` `aac79b59c14d35a21bb500249bd05115ac410c92bf65003bfe36d2dd3d78e5e6`; `test.cpp` `3991e94a4a903e7682767bc3a01abb375c2f7beeda548c2a13f7fb08ba218759`; R `test.R` `65923c1ac92336166ef9038efb65a9e41633b6765b274196539ad9e338fcd6fe`; slash `README.md` `92a4d7815239f239fed63158c655d128c180c4d57b8a688d4d4ec6e2c92d4b82`.
