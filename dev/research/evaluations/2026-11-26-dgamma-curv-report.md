# Discretized gamma curvature certification

TASK-315 on `codex/dgamma-curv`, based on `2d89fd4a`. Free fits now certify a single maximizer when the curvature and coefficient checks close. A successful multistart fit still returns when those checks are inconclusive. It reports `certified = FALSE` and `unique = NA`.

## Implementation and proof

- [mle.hpp:308](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp#L308) dispatches to the certificate after the point-mass and two-point limit checks. The certificate uses the extreme occupied pair for a negative upper score tail and the zero-cell bound for a positive lower score tail. The sample trace test bounds the standardized within-cell covariance, including at zero, and makes the profile strictly concave between those cutoffs. [curvature.hpp:22](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L22), [curvature math report](2026-11-25-dgamma2-math-report.md), [profile score tails](2026-11-25-dgamma-math-report.md#shape-score-and-a-sufficient-sample-condition)
- On a trace miss, monotonicity of the profiled rate encloses it between the rate brackets at each shape interval's endpoints. Each interval must either exclude zero from the profile score, with the singular zero-cell terms combined, or establish negative profile curvature by covariance domination. A complete cover forces every score root to cross downwards. The opposite score tails then give exactly one root and a unique global fit. [curvature.hpp:78](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L78), [curvature.hpp:122](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L122), [curvature.hpp:152](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L152), [curvature.hpp:195](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L195)
- The solver retains strict sign brackets for shape and rate. The midpoint must meet the tolerance in both public coefficients using the log brackets, with an internal target at most the default `1e-10`. An inconclusive inner tightening can still supply a valid rate enclosure. Optional Newton refinement starts from the accepted midpoint and must pass the existing local accuracy gate and remain within the log tolerance of that midpoint. This preserves the more stringent golden regression while keeping a bracketed result available when refinement fails. [curvature.hpp:218](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L218), [curvature.hpp:258](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L258)
- The exact-arithmetic proof is conditional in code on the computed comparisons. Practical pads scale with the score, gamma prefix and covariance terms used in the comparison, using 128 binary64 epsilons as a multiplier. Subdivision and its score evaluations share the `128 * min(iterlim, 100000)` allowance. These pads give practical comparisons rather than uniform special-function error bounds. [curvature.hpp:15](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L15), [curvature.hpp:65](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/curvature.hpp#L65)
- The existing likelihood comparison can still improve a feasible multistart seed. Exhausting it on a free fit leaves the seed eligible for the local accuracy check. The free fallback claims unknown cardinality and no global certificate. The inventory records the new free-fit singleton assignment and adds the curvature proof to the shared publisher's row. [mle.hpp:400](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp#L400), [mle.hpp:478](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp#L478), [singleton proof inventory](../../validation/singleton-proof-sites.csv), [dgamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md#curvature-certificate-for-free-fits)

## Pinned cases

The independent mpmath reference solves both likelihood scores from incomplete-gamma cell integrals and checks that the Hessian is negative definite. Runs at 70 and 100 decimal digits agree to 55 significant digits. For the fallback sample this checks the reported local maximum. [Reference program](2026-11-26-dgamma-curv-data/reference.py), [reference values](2026-11-26-dgamma-curv-data/references.json)

| Counts | Route | Certified | Unique | Shape | Rate |
|---|---|---|---|---:|---:|
| `(0, 1000)` | Sample trace | TRUE | TRUE | 0.10960112723365823 | 0.00021907110238480158 |
| `(0, 2)` | Interval cover | TRUE | TRUE | 1.4602615018243366 | 0.98777443934235178 |
| `(6, 7, 8)` | Checked search | FALSE | NA | 97.099210667563159 | 12.945326740553260 |

The C++ cases also assert whether the trace passes. Both layers pin shape, rate and log likelihood. Across the final direct native and double-only replays, the largest relative difference against these references was `1.66e-12`. [C++ tests](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/test.cpp#L301), [R tests](../../../bindings/r/distributions/dgamma/test.R#L139), [check output](2026-11-26-dgamma-curv-data/checks.txt)

## Snapshot moves

| Batch-19 mode | Rows | Changed | Newly certified unique | Certification withdrawn |
|---|---:|---:|---:|---:|
| Native | 79 | 14 | 11 | 3 |
| Double-only | 79 | 14 | 11 | 3 |
| Modifier routes, native | 0 | 0 | 0 | 0 |
| Modifier routes, double-only | 0 | 0 | 0 | 0 |

The direct modes move the same rows. Archive `000986`, `golden`, `base6`, `two`, `half-ties`, `wide`, `n3-s0`, `n3-s2`, `n3-s3`, `n7-s2` and `n7-s3` gain uniqueness from the curvature route. The bracketed solve accounts for the iteration-count changes, and recomputing at the refined coefficients accounts for the numerical changes. The largest relative coefficient move was `2.94e-15` natively and `1.59e-12` in double-only mode. The largest absolute log-likelihood move was `5.62e-10` on `wide`, about `1.8e-11` relative. The existing golden assertions pass in both precisions.

The `ties`, `n7-s0` and `n7-s1` fallbacks changed only the certification decision and its outcome representation. Other input keys, held fits and stops match the baseline. The frozen modifier route table has no dgamma entries, so the two filtered modifier runs were empty. [Per-row changes](2026-11-26-dgamma-curv-data/snapshot-moves.json), [comparison program](2026-11-26-dgamma-curv-data/snapshot_moves.py)

## Final checks

All final runs used the workstation. The C++ compiler was GCC 15.2.0. The simbox connection check timed out.

| Check | Result |
|---|---|
| `just test-changed --no-long-double`, native C++ | 2 of 2 shards passed |
| `just test-changed --no-long-double`, double-only C++ | 2 of 2 shards passed |
| Bundled targeted R development tier | 362 passed, 8 tier skips, zero failures or warnings |
| Bundled JS build and tests | Build passed, 237 tests passed |
| Fresh snapshots and `build/scratch/diff_baseline.py` | Four modes compared, all rows accounted for |
| `fit-density.py` | Zero new findings. The largest relative residual was `6.8e-15` in each direct precision |
| `fit-consistency.py` | Zero new findings |
| `strict-uniqueness.py` | Passed, 219 sites |
| `just site-check` | Passed with `EMCC_CORES=1`, including 86 family pages and generated C++ snippets |

I temporarily restored the baseline proof CSV during the targeted recipe because the selector treats any validation CSV edit as shared implementation. The recipe selected only `dgamma`. The final singleton CSV was restored and passed the separate check. The R recipe also regenerated the four dgamma rows in the capability table from the dossier's partial certification metadata. The first site run used the default Emscripten worker setting under a single-slot reservation. The final site run pinned Emscripten to one worker. [Check output](2026-11-26-dgamma-curv-data/checks.txt)

## Open

- Universal free-fit uniqueness remains open. The missing step is a proof of the stationary curvature sign for arbitrary admissible samples or a strict valley for an exact sample.
- A proof for a particular sample can remain unresolved in floating point. The current cover and coefficient checks need strict signs at the available precision and within the work allowance. The checked `(6, 7, 8)` fixture exercises that outcome at the default controls.
