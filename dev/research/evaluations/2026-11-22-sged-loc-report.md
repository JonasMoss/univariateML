# SGED held-SD location (TASK-150.9)

Base commit `b6a0087141ceb753324e1d46441d8f320271042e`, branch `codex/sged-loc`.

## Reference and cause

For `x = c(0, 1e100)`, `sd = 1`, `nu = 1.01`, `xi = 100`, the unique maximizing join is approximately `1.000000000000818058458539650601273941773371084999170558492466e-304`. The public mean is `1.004872151548589714443081510290171795444820651454323417484268`. The maximized log likelihood is approximately `-9.809916223900376607464524367713936119226347675912433643e100`. The compiled R binding returns certified mean `1.0048721515485897` and log likelihood `-9.8099162239002159e100` at both `tol = 1e-10` and `tol = 10`. [Reference results](2026-11-22-sged-loc-data/reference.json), [R binding replay](2026-11-22-sged-loc-data/binding-replay.txt).

The independent calculation bisects the monotone score in physical coordinates at 560 decimal digits using exact binary64 inputs. It evaluates the shift and likelihood using gamma moments. The evidence gives 72 significant digits for the original sample and eight neighbours. They cover gaps from 1 to `1e300`, both skew directions, a negative observation, unequal endpoint counts, three distinct observations, and a symmetric root on an interior knot. [Reference script](2026-11-22-sged-loc-data/reference.py).

In the old certificate, the sample midpoint and half-span are each of order `5e99`. The exact join maps to a coordinate indistinguishable from the normalized left endpoint in binary64. The inverse reconstruction guard then compares a roundoff allowance of order `1e85` with a public-mean allowance of roughly `2.5e-8` even at `tol = 10`. Its fixed normalized recovery threshold of `1e-9` also rejects some fits when observations have a very small spread relative to the public mean. [Original finding](2026-11-21-loose-2-report.md#open).

## Fix and justification

When SD and both shapes are held and the power is above one, the weighted power loss is strictly convex. For two distinct observations $a<b$ with counts $n_a,n_b$, the unique score root satisfies
$$
\frac{b-c}{c-a}=\exp(k), \qquad
k=\frac{\log(n_a/n_b)+2\nu\log\xi}{\nu-1}.
$$
The solver works from the nearer observation and computes a displacement of $(b-a)/(1+\exp(|k|))$. It evaluates the logistic fraction with a nonpositive exponent and combines it with the gap in log space when the fraction would lose relative accuracy. The displacement enclosure includes allowances for the computed odds and gap. [Dispatch, line 15](../../../univariateML-cpp/include/cppunivariate/distributions/sged/certificate.hpp), [closed solve, line 135](../../../univariateML-cpp/include/cppunivariate/distributions/sged/location_certificate.hpp).

For three or more distinct observations, score signs at the knots locate an interval and a midpoint comparison selects its nearer endpoint. The score is evaluated as a function of displacement from that observation using compensated sums of log-rescaled terms. Geometric steps resolve displacements across large magnitude ranges. Uncertain signs leave the existing signed bracket in force or require signed probes on either side. Solver exhaustion preserves a finite feasible witness when one can be evaluated. [Residual arithmetic, line 8, score and bracketing, lines 164 and 196](../../../univariateML-cpp/include/cppunivariate/distributions/sged/location_certificate.hpp).

Write $E$ for the displacement radius plus the practical floating-point allowance for reconstructing the mean, and $S=\max(|\widehat\mu|,\sigma)$. The fitter requires $E\le\eta S$, with $\eta=\min(\mathtt{tol}/2,1/2)$. The scale at the exact mean is at least $(1-\eta)S$, and $\eta/(1-\eta)\le\mathtt{tol}$. This yields the accuracy contract in public units. A separate likelihood check uses the sum of the minimum per-observation penalties over the bracket to upper-bound the maximized likelihood. The likelihood at the bracket representative and the summed density at the stored public mean close that bound within the likelihood tolerance and practical arithmetic margins. [Acceptance, line 75](../../../univariateML-cpp/include/cppunivariate/distributions/sged/location_certificate.hpp), [SGED dossier, line 292](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md).

## Canonical snapshots

Both workstation precision modes produced the same differences against the stored baselines.

| Family | Native changed | Double-only changed | Maximum relative move among estimate pairs |
| --- | ---: | ---: | ---: |
| SGED | 15 / 285 | 15 / 285 | `1.15242084303e-10`, mean in `006190` |
| GED | 0 / 173 | 0 / 173 | 0 |

Every changed SGED row holds SD, power and skew. Nine estimate pairs changed in mean, log likelihood and iteration count. Six numerical stops became certified estimates in both modes.

| Case | Cause of the former stop |
| --- | --- |
| `006196` | Spread of order `1e-8` near 0.106. Rounding in the public mean is amplified by inverse normalization. |
| `006197` | Spread of order `1e-12` near 0.106. The same reconstruction loses more normalized digits. |
| `006199` | Spread of order `1e-8` near 1. The public mean rounds too coarsely for the fixed normalized recovery threshold. |
| `006200` | Spread of order `1e-12` near 1. The same reconstruction loses more normalized digits. |
| `006201` | Observations are of order `1e-150`. The order-one moment shift absorbs the mode when reconstructing the public mean. |
| `006203` | Observations have an offset of `1e8` and order-one spread. Public-mean rounding exceeds the fixed normalized recovery threshold. |

The independent replay checked all 16 convex held-SD snapshot inputs. For the 15 estimates in each mode, the largest mean error divided by $\max(|\mu_*|,\sigma)$ was `3.5438995e-11`, and the largest log likelihood error divided by $\max(1,|\ell_*|)$ was `6.4017557e-15`. Case `006190` has a small mean relative to its held SD, which accounts for the larger baseline-relative move. [Snapshot diffs](2026-11-22-sged-loc-data/snapshot-diffs.txt), [native provenance](2026-11-22-sged-loc-data/snapshot-native.provenance.json), [double-only provenance](2026-11-22-sged-loc-data/snapshot-double.provenance.json), [snapshot references](2026-11-22-sged-loc-data/snapshot-reference.json), [error checks](2026-11-22-sged-loc-data/snapshot-reference-check.txt).

To reproduce the comparison, run `dev/validation/fit-snapshot.py --family ged,sged` with separate `--output` directories for native and `--no-long-double`. Compare each output with the same-mode stored baseline filtered to those two families using `build/scratch/diff_baseline.py`. The evidence summary script computes the counts and maximum move from the same records. Passing case numbers `6190` through `6205` to `reference.py` regenerates the archive reference. [Stored baselines](2026-11-15-fit-snapshot-data/), [summary script](2026-11-22-sged-loc-data/snapshot-summary.py).

## Tests and checks

- The R regression formerly allowed a numerical stop at `tol = 10`. It now requires a certified fit and checks the reference mean to relative `1e-10`. [R test, line 1013](../../../bindings/r/distributions/sged/test.R).
- The appended C++ test checks certification, the nine reference means at `mltest::kFitReferenceTol` with `.scale(0)` for nonzero means, exact equality for the symmetric zero, and the reference likelihoods. Its `tol = 1e-20` case checks the structured tolerance failure. [C++ test, line 1420](../../../univariateML-cpp/include/cppunivariate/distributions/sged/test.cpp).
- The initial native check identified a missing finite witness on solver exhaustion and the zero-reference comparator issue. The final targeted recipe reran native and completed the double-only, R and JavaScript stages.
- Final workstation results with GCC 15.2. `just test-changed --no-long-double` passed 4/4 SGED C++ shards in each mode. The targeted R run reported 842 passes, 10 skips, zero failures and zero warnings. The same recipe built the JavaScript bindings and passed 230/230 tests.
- Workstation follow-up checks passed. GED CTest passed in both modes, `just quality sged` passed, and `just site-check` passed, including the 85 family pages and generated C++ examples. Canonical replays completed 458 cases per mode. All final checks ran on the workstation.

## Open

- Snapshot `006202` still reports a numerical stop. Its optimum log likelihood is about `-3.636600133905258e513`, outside binary64 range. A finite likelihood result for this case would require a wider result representation. [Archive reference](2026-11-22-sged-loc-data/snapshot-reference.json).
- SGED includes GED’s density header. The validation here uses the GED density from the task base. The concurrent pged-tail changes are outside this replay. [SGED density dependency, line 7](../../../univariateML-cpp/include/cppunivariate/distributions/sged/dpqr.hpp).
