# CMB free-size search

2026-11-01, round 2. Tested revision `1fc8af0a5e4cf62b04d77e647182143b27903f65` plus the uncommitted changes in this worktree. Goldens and generated d/p/q/r implementations were not changed.

## Implemented: dispersion held

With `size = NULL`, `nu` held and `prob` free, constant samples return the attained point mass at the smallest feasible size. For zero dispersion the smallest feasible size maximizes the likelihood. For negative dispersion, the endpoint-normalizer bound in the [CMB dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) excludes the entire remaining tail; the fitter checks every preceding size.

For positive dispersion, the fitter obtains the CMP face by fixed-dispersion mean matching, and explores the first 256 feasible sizes. If no finite candidate beats the face by `1e-9*(1+abs(face))`, it stops with `FaceUnresolved`, or `FaceTie` if a finite likelihood numerically matches the face within that margin. Both messages identify the CMP model and say that a larger finite maximum is not excluded. Neither stop proves escape or non-existence.

If a candidate beats the face strictly, the fitter optimizes a finite truncation of the CMP kernel. It increases the truncation until its profile is below the candidate, then combines that upper comparison with the monotone `O(1/N)` correction in the dossier to find a cutoff. Every feasible size below the cutoff is evaluated, including those below the truncation point. The fixed-size likelihood is profiled by the existing mean-matching solver. The final maximum is certified by these inequalities in exact arithmetic, conditional on the profile computations and comparisons. The `1e-9` margin and truncated-profile bracket allowance are practical floating-point allowances, not proved outward-rounded enclosures. The CMP moment routine bounds truncation error but not roundoff; fixed-size solvers and comparisons are numerical. Directed rounding and validated error bounds remain open.

The search charges one unit per normalizer support term for each finite-size profile and accounts for CMP face work against a 10^7-unit budget. This is a conservative work proxy for candidate enumeration, not a bound on actual floating-point operations: a profile can evaluate its normalizer more than once. It also stops if the required truncation exceeds 8192, the size/representation cap is reached, or a fixed-size/face solve fails. None returns the best partial fit. The previously implemented search with both canonical coordinates held is unchanged.

## Open: free dispersion

The superlevel bounds from round 1 remain in the dossier. They give a uniform `O(1/N)` correction for samples spanning at least three integers, but the `{}` and `{prob}` held sets remain not implemented. To turn that bound into the requested procedure, the truncated two-coordinate profile needs a checked global optimizer and stopping rule; the resulting candidate must also be compared with fixed-size dispersion-limit faces. For `{prob}`, the reachable limit face depends on the held odds and needs a corresponding constrained comparison. Constant and adjacent-support cases need separate treatment. The bound alone does not supply these comparisons, and no search cap is returned as an estimate. The holds metadata records these specific gaps.

## Validation

The [seeded inequality check](2026-11-01-cmb-size-data/check_bound.py) from round 1 uses Python 3.14.5, seed 1142026, 10,000 random parameter cases, sizes 5–300 and 20 observations; its largest inequality residual was `-2.65700278446e-07`, and 9,939 superlevel dispersion checks passed at tolerance `1e-9`. It checks the algebra numerically, not the optimizer or rounding certification.

The new C++ tests check positive, zero and negative held dispersions against explicit fixed-size profiles through 300, a seeded binomial sample (`std::mt19937`, seed 1142026), an unresolved CMP face, a numerical tie, a work-budget failure and attained constants. R full-tier tests independently optimize the finite-size probability profiles through 300 for a deterministic sample and a seed-1142026 sample, verify the held metadata, and check the face, tie and resource diagnostics. The tie fixture uses `nu = 0.0030483655389936895`, found numerically, and is a tolerance-based test rather than a proof of an exact tie. These finite grids do not independently prove the infinite tail.

The first targeted C++ run exposed a test-grid error: its seeded sample had maximum 3 but the grid started at 4. The grid was fixed and the targeted tests passed. The first JavaScript suite had one failure: its generic held-subset test did not recognize the positive-dispersion resource stop on the CMB golden sample. The reachable CMP limit was added to the metadata, and that resource diagnostic now names the available limit while explicitly leaving larger finite maxima unresolved. The rebuild and full JavaScript rerun passed.

Commands ran separately through `/tmp/umvl-cpu/run4`, with BLAS/OpenMP pinned to one thread, four build threads, `nice -n 10`, and temporary files in `build/tmp`. R was 4.6.1 and GCC was 15.2.0. Ephemeral logs are under `build/scratch/`; the site dependencies were copied from the existing local installation.

| Command | Final result |
|---|---|
| `just cpp-test` | Pass, 105/105 development shards. |
| `just cpp-test-no-long-double` | Pass, 126/126 tests. |
| `just test` | Pass: 19,898 passed, 0 failed, 0 warnings, 152 tier skips. |
| `just test-full` | Pass: 41,366 passed, 0 failed, 0 warnings, 1 optional-package skip. |
| `just js-build` | Pass. |
| `just js-test` | Pass, 400/400. |
| `just site-check` | Pass. |

The C++ suites preceded the last binding-only diagnostic and metadata change; they tested the same C++ source. The R tiers were rerun after that change. No commit was made.
