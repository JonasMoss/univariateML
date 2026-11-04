# TASK-116: certified Pareto IV default and corrected golden

The public Pareto IV fit now calls `pareto4_mle_certified` when `min` is
held and all three regular coefficients are free. The common adapter reports
certificate tie, budget exhaustion and numerical failure separately, with
“no verified MLE”; none can be rescued by a local score check. Exported R
tests cover these refusals, constants, excess overflow and artificial ties.
An appended native case checks the public route on all 51 Burr workloads.
Held-subset algorithms and free-threshold refusal remain unchanged.

The dossier now describes the certified default. Its aggregate `fit.certified`
remains `partial`, as for Burr, because held subsets retain separate algorithms
and numerical face comparisons. Translation by a held threshold has unit
Jacobian and preserves the Burr likelihood. The guarantee is certified in exact
arithmetic, conditional on computed comparisons, with Burr’s practical floating-point
margin and libm assumptions; it is not outward-rounded interval arithmetic or
a universal uniqueness theorem. Accepted finite outcomes have a sample-specific
unique global maximum certificate. See the [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md).

Only the Pareto IV golden changed, and only its three coefficient values.
`UMVL_GOLDEN_IDS=pareto4 just goldens` selects one family, but current sampler
defaults produce a different sample. After verifying that behavior, the original
500 observations and historical metadata were preserved and refitted through
exported `mlpareto4`. The likelihood rounds to its existing value.
The independent 80-digit analytic-score solve reproduces the previously
reported root: maximum relative coefficient error 4.57e-15, absolute likelihood
error 1.37e-12, total log-coordinate score norm 2.96e-13. This corroborates
the approximation numerically; globality comes from the certificate.
[Reproduction and retained evidence](2026-11-04-pareto4-switch-data/README.md).

All final runs completed on the **local workstation**, under the shared CPU
locks with nice scheduling and one-thread BLAS/OpenMP. Tested base:
`0460ccc8f9c108b5ff72fb750a4b42f6217c3d02`, plus the uncommitted sources
identified in [validation.json](2026-11-04-pareto4-switch-data/validation.json).

| Command (each through `/tmp/umvl-cpu/run4 nice -n 10`) | Result |
|---|---|
| `just cpp-test` | 106/106 passed |
| `just cpp-test-no-long-double` | 127/127 passed |
| `just test` | FAIL 0, WARN 0, SKIP 154, PASS 20551 |
| `env UMVL_TEST_TIER=full just cpp-test` | 127/127 passed |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43366 |

Development skips are tier exclusions; the full run has none. No permitted
failures or unresolved task items remain. Other platforms and JS/wasm were
not tested. No Git writes, board edits, sub-agents, installations or simbox
were used. Settled d/p/q/r, special functions, truth fixtures, generators and
other family goldens were unchanged; no changes to them are needed.

Artifact-pane display was attempted; the viewer returned HTTP 403 because this
worktree is outside its configured readable roots. The report remains here;
no viewer configuration or files outside the worktree were changed.
