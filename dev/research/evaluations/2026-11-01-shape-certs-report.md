# TASK-116: one-dimensional profile search evidence

Source: `79db7f41d6647921549871368b3f5a57819e8465`, with the dossier and evaluation changes in this worktree. Workstation: Ubuntu, g++ 15.2.0, R 4.6.1, Node 22.22.1; C++ uses the installed BH headers. No fitter, binding, golden, or status implementation changed. No global search certificate was completed. The existing searches remain numerical, and their `Ok` results must not be interpreted as proofs of global optimality.

The brief's family names need two corrections: `dgamma` is the discretized gamma, not a continuous double gamma; `slash` has no free shape, and its outer coordinate is location. The work below addresses the actual code.

## Mathematical findings

The family dossiers contain proofs, conditions, and TASK-116 tables:

- [Half-t](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md): free threshold is the sample minimum, and fixed-shape log-scale has a strictly decreasing score subject to its tie criterion. General shape unimodality is false. For a compact shape interval separated from scale collapse, the dossier gives a data-dependent bracket for the scale root and an explicit finite bound for the profile derivative in log shape. The small-shape pointwise upper bound is known. **Open:** calculate a validated derivative enclosure and a finite, uniform large-shape tail bound, including error in the scale solve and special functions. Convergence to the normal face and comparison at `nu = 1e8` do not themselves certify the tail.
- [Discretized gamma](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md): the fixed-shape log-rate problem is strictly concave, and the implementation brackets its root and stops using a score or bracket-width criterion. Differentiation under the cell integral gives a conditional-log-observation identity for the shape score, with elementary bounds for positive cells. The small-shape uniform bound and the qualitative large-shape faces establish existence in the stated regimes. **Open:** bound the zero-cell score, the profiled rate and numerical errors, and derive finite tail cutoffs and interval bounds sufficient to exclude missed modes. Shape unimodality is not established.
- [Slash](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md): the held-scale location log likelihood is globally Lipschitz with constant `n*sqrt(2)/sigma`; the maximizing location lies within the sample range. Both held-scale and free-scale profiles can be multimodal. **Open:** use validated likelihood/slope evaluations and finite subdivision to implement the held-scale certificate; for free scale, also bound the scale root and neighborhoods of half-mass collapse. The existing fixed-bracket numerical scale bisection is not an error-controlled interval solve.

The bounds are exact-arithmetic statements, not claims of outward-rounded library arithmetic. In particular, a finite-grid probe, a converged local refinement, or agreement with a normal or collapse face is not a certificate. No code change was made because the bounds and error control needed for a complete certificate were not obtained.

## Seeded profile probes

[`probe.cpp`](2026-11-01-shape-certs-data/probe.cpp) generates 600 samples per family using `std::mt19937_64`, seed `11620261101`, sample sizes 10, 30 and 80. Half-t and discretized-gamma generating shapes are 0.5, 1, 3 and 10; slash has its fixed shape. It compares each OK fit to extra shape or location grid points, reusing the current inner profile calculations. Output and timing per case: [`probes.csv`](2026-11-01-shape-certs-data/probes.csv); reproducible summary: [`summarize.py`](2026-11-01-shape-certs-data/summarize.py). Standard-library random distributions can differ between library implementations; the stated compiler environment is part of the reproduction.

| Family | OK and compared | Other statuses | Probes above fit by more than `1e-12 * max(1, abs(logLik))` | Largest probe minus fit |
|---|---:|---|---:|---:|
| half-t | 489 | 111 `NormalLimit` | 0 | -4.37e-9 |
| discretized gamma | 537 | 52 `TwoPointLimit`, 11 `PointMass` | 0 | -3.36e-7 |
| slash | 600 | none | 0 | -4.96e-9 |

These are checks against finite sampled points, not independent global optimizers; the same inner implementations are reused. Limit-status cases are counted but not compared. No before/after fit or status changes occurred because the fitter code is identical. Thus there is no implementation-induced likelihood loss, but this experiment does not establish the requested 600-per-family comparison of a new certified implementation against the original, nor does it establish global optimality. **Open:** once a certificate is implemented, run the paired acceptance comparison on at least 600 samples per family, including all status transitions and relative likelihood losses.

## Golden timing

[`golden-time.cpp`](2026-11-01-shape-certs-data/golden-time.cpp) and [`golden-time.csv`](2026-11-01-shape-certs-data/golden-time.csv) record 20 local single-slot repetitions per family, `n = 500`, g++ `-O2`, with all statuses OK. Median milliseconds were half-t 101.209, discretized gamma 34.6682, slash 974.1035. These are measurements of the unchanged implementation, not paired before/after timings. **Open:** collect paired timings when an implementation exists; the current results are a baseline, not an estimate of a change in speed.

Compile the two drivers with `g++ -std=c++17 -O2 -IunivariateML-cpp/include -I<BH include>`, then run each from the repository root under `/tmp/umvl-cpu/run`; the retained CSV files contain the outputs. Run the summary with `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-01-shape-certs-data/summarize.py`.

## Final checks

All final commands ran locally, individually under `/tmp/umvl-cpu/run4`, with `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` in `build/tmp` and BLAS/OpenMP pinned to one thread. Simbox was attempted for the R dev run but returned 75 (unreachable), so the local fallback was used.

| Command | Result |
|---|---|
| `just cpp-test` | 105/105 dev shards passed |
| `just cpp-test-no-long-double` | 126/126 shards passed |
| `just test` | 19,898 passed, 151 skipped, 0 failed, 0 warnings |
| `just test-full` | 41,366 passed, 0 skipped, 0 failed, 0 warnings |
| `just js-build` | passed on rerun; the first attempt failed with `No space left on device` before build setup. Disposable worktree build files were removed before rerunning. |
| `just js-test` | 400/400 passed |
| `just site-check` | passed; 98 family pages, MathML and links checked. Dependencies were copied from the prescribed local site installation. |

The normal CMake Boost lookup fell back to the installed BH headers as the recipes intend. The tests establish regression and integration behavior, not the global profile claims left open above.
