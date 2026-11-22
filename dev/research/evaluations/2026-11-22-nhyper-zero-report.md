# TASK-224 — All-zero negative hypergeometric fits

## Maximizer sets

Let $B=2^{53}-1$, the largest supported count, and assume a nonempty sample of zeros. At $r=0$ the stopping requirement is met before the first draw, so $P(T=0)=1$ for every integer $0\le m\le N\le B$. At $r>0$ at least $r$ draws are required, so $P(T=0)=0$ and the sample likelihood is zero. Thus every admissible triple with $r=0$ and the specified holds attains the likelihood upper bound of 1. A held positive $r$ makes the all-zero sample infeasible.

All triples below are in $(m,N,r)$ order. Held values $m_0$ and $N_0$ are admissible integers.

| Holds | Full set of maximizers | Unique exactly when |
|---|---|---|
| None | $(m,N,0)$ for $0\le m\le N\le B$ | Never |
| $r=0$ | $(m,N,0)$ for $0\le m\le N\le B$ | Never |
| $m=m_0$ | $(m_0,N,0)$ for $m_0\le N\le B$ | $m_0=B$ |
| $m=m_0,\ r=0$ | $(m_0,N,0)$ for $m_0\le N\le B$ | $m_0=B$ |
| $N=N_0$ | $(m,N_0,0)$ for $0\le m\le N_0$ | $N_0=0$ |
| $N=N_0,\ r=0$ | $(m,N_0,0)$ for $0\le m\le N_0$ | $N_0=0$ |
| $m=m_0,\ N=N_0$ | $(m_0,N_0,0)$ | Always |
| $m=m_0,\ N=N_0,\ r=0$ | $(m_0,N_0,0)$ | Always |

A held zero population forces the singleton $(0,0,0)$ even when both other counts are free. A held composition of $B$ forces $(B,B,0)$ when population is free.

## Fitter and payload

The all-zero shortcuts set the uniqueness flag by the cardinality of the admissible set. With joint composition and population free, the representative remains $(0,0,0)$. With either count held, the representative uses the lower endpoint of the free count. The held-`N`, held-`r` composition shortcut retains its `Unidentified` status for $N>0$. Every feasible all-zero route remains a certified estimate with log likelihood zero. [mle.hpp:167](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/mle.hpp#L167) · [mle.hpp:326](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/mle.hpp#L326) · [mle.hpp:435](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/mle.hpp#L435) · [mle.hpp:472](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/mle.hpp#L472)

The shared binding supplies the full range in `outcome_message` and places the selected estimate first in `maximizers`. For free composition at a held population it lists both composition endpoints. For free population at a held composition it lists the two smallest populations. With both free it lists $(0,0,0)$ and $(0,1,0)$. These rows are witnesses to the range described in the diagnostic. Singleton fits carry `unique = TRUE`. [fit.hpp:67](../../../bindings/common/distributions/nhyper/fit.hpp#L67) · [Dossier:154](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#L154)

The C++ regression covers each of the eight hold patterns, both singleton boundaries with `r` held and free, and checks the shared estimate kind, certification, uniqueness, representative, and equal likelihood at the other endpoint. It verifies the zero-observation mass on a small urn grid for every feasible stopping count. The R regression covers the same routes through `mlnhyper`, checking successful estimates, flags, exact representative rows, their probability-one mass, and the range text. [C++ tests:313](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/test.cpp#L313) · [R tests:247](../../../bindings/r/distributions/nhyper/test.R#L247)

## Why strict missed it

Both phenomenon classes exist for nhyper. The snapshot generator applies the golden holds $N=50$ and $r=3$ to the `zeros` class $[0,0,0,0]$ and the `constant` class $[7,7,7,7]$. The former has the expected `failure(invalid-input)` outcome because zero lies below the held stopping count. The latter exercises a positive stopping count. `fit-consistency.py` selects class records and compares `one` and `constant` by kind and reason. Its other constant-sample rules cover free location and scale, or free scale or rate. The flag appears in display codes, and the declared `two_point` rule can assert it. There is no corresponding uniqueness assertion for `zeros` or `constant`. [fit-snapshot.py:61](../../validation/fit-snapshot.py#L61) · [fit-snapshot.py:121](../../validation/fit-snapshot.py#L121) · [fit-consistency.py:204](../../validation/fit-consistency.py#L204) · [fit-consistency.py:242](../../validation/fit-consistency.py#L242) · [fit-consistency.py:296](../../validation/fit-consistency.py#L296)

## Validation

| Check | Machine | Result |
|---|---|---|
| `just test-changed --no-long-double` | Workstation, GCC 15.2, four slots | Selected nhyper. C++ passed 1/1 in each mode. R passed 992 expectations with 0 failures, 0 warnings and 6 skips. JS built successfully and passed 230/230 tests. |
| `just site-check` | Workstation, one slot | Passed, including 85 family pages and 85 generated C++ examples. |
| Stored nhyper snapshots, native and double-only | Workstation, one slot | 147 records per mode, with identical keys. Both modes showed the same three inherited warning and candidate changes. |
| All-zero replay against branch base `7a57d011`, native and double-only | Workstation, one slot | 12/12 cases per mode. Five flipped both uniqueness fields and gained two witnesses. The other seven matched the base output. |

The stored changes are archive cases `005151`, `005152`, and `005154`. These are positive constant samples with free counts and the representative $(8,8,8)$. The earlier hyper-flags change replaced their warning with the candidate rows $(8,8,8)$ and $(9,9,8)$. Coefficients, likelihoods, and outcome flags match the stored baselines in all 147 cases in each mode. The frozen corpus has no feasible all-zero nhyper record. [Hyper-flags report](2026-11-22-hyper-flags-report.md) · [Snapshot comparisons](2026-11-22-nhyper-zero-data/snapshots.txt)

For the supplemental replay, the comparison passes the same 12 explicitly held inputs to the current and base snapshot probes. The affected holds are none, `r`, `m`, `m/r`, and `N` with positive population. Only top-level `unique`, `outcome.unique`, and the candidate rows change on these five inputs. The snapshot renderer clears estimate messages, so the R assertions verify the range text. [Comparison script](2026-11-22-nhyper-zero-data/compare.py) · [Suite results](2026-11-22-nhyper-zero-data/suite-results.txt)

To reproduce the comparison, run `dev/validation/fit-snapshot.py --family nhyper` in each precision mode, adding `--no-long-double` for the double build. Use output directories `build/scratch/nhyper-zero/native` and `build/scratch/nhyper-zero/double`. Extract `7a57d011` into scratch with `git archive` and run its snapshot driver with `--family nhyper --classes zeros` and absolute output paths to `build/scratch/nhyper-zero/base/native` and `build/scratch/nhyper-zero/base/double`. Run the comparison script from the worktree root with `build/scratch/diff_baseline.py` present.

## Open

Add a hold-aware strict phenomenon case for feasible all-zero samples. For nhyper, exercise the eight hold patterns with `r` free or held at zero, along with held $N=0$ and $m=B$ singleton controls. For each route, derive the maximizer set from the held values and count domain. Require a certified estimate at log likelihood zero and assert that `unique` equals the singleton test. For a nonsingleton set, require at least two distinct admissible witnesses with the same likelihood and require a diagnostic describing the whole set. The current phenomenon inventory and rules need this coverage before strict can detect a return of the defect.
