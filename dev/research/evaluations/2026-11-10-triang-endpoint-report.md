# TASK-101.7.6: triangular endpoint maxima

Completed on 10 November 2026 in `codex/triang-endpoint`, based on
`cd145ca576a27ea5f34976a2a50938b5af9c4032`. Changes are uncommitted and confined
to this worktree. No board edits, package installations or Git writes were made.

## Result

**All ten cases are category (b): an accurate continuous maximum loses
likelihood when its endpoint is represented as binary64.** The endpoint search
already selected the correct continuous maximizing mode. Its returned public
endpoint was also already the best representable endpoint. Independent
400-digit calculations now establish these facts for every observation mode,
rather than merely exhibiting an improvement over the rounded point.

At `tol=1e-10`, four cases now return certified `Ok`: **8165, 8168, 8172 and
8213**. Their public coefficients and likelihood agree with the continuous
reference within the requested relative tolerance. The other six retain
`NumericalFailure`: even the best representable public point loses too much
likelihood. At the unchanged native `tol=1e-12`, cases **8168 and 8213** recover;
8165 and 8172 require the looser requested tolerance.

Every selected public coefficient has relative error below `9e-17` against the
continuous reference. Thus coefficient accuracy alone would miss the six
substantial likelihood losses.

| Case | Best public likelihood loss | Relative likelihood loss | Native `1e-12` | `1e-10` |
|---|---:|---:|---|---|
| 8165 | 1.82271526e-10 | 1.37203146e-12 | Stop | Ok |
| 8168 | 6.80778730e-11 | 5.25119435e-13 | Ok | Ok |
| 8172 | 7.82594674e-10 | 6.13033864e-12 | Stop | Ok |
| 8173 | 1.16161225e-4 | 6.27945897e-7 | Stop | Stop |
| 8194 | 3.52298317e-3 | 1.90775172e-5 | Stop | Stop |
| 8213 | 1.26985671e-10 | 9.98406744e-13 | Ok | Ok |
| 11117 | 1.79707854e-5 | 9.60742507e-8 | Stop | Stop |
| 11118 | 1.79707854e-5 | 1.05883449e-7 | Stop | Stop |
| 11119 | 5.64341002e-3 | 3.02465835e-5 | Stop | Stop |
| 11122 | 5.64341002e-3 | 3.33433575e-5 | Stop | Stop |

Loss means continuous maximum minus best public log likelihood; relative loss
uses `max(1, abs(reference logLik))`. Here all reference likelihood magnitudes
exceed 127. “Stop” denotes the existing numerical-failure status, justified by
the irreducible public likelihood loss at that tolerance.

## Certificate and implementation

For a mode at the observed minimum, the near endpoint is held or equals the
mode. Let `w` be the distance from the mode to the free far endpoint, `D` the
near-endpoint slack, and `d_i` the positive observation distances from the mode.
Reflection gives the identical upper-mode problem. Apart from the constant
`n log(2)`, the profile is

```
L(w) = -n log(w + D) + sum_{d_i > 0} log(1 - d_i/w).
w L'(w) = -n w/(w + D) + sum_{d_i > 0} d_i/(w - d_i).
```

In reciprocal width `p=R/w`, with `R=max(d_i)`, the objective is strictly
concave. Signed scores at the inward and outward adjacent public endpoints
bracket its unique continuous optimum. Comparing the candidate with those two
neighbours then identifies the best representable endpoint: outside that
bracket, concavity supplies monotonicity. The implementation evaluates the
neighbouring likelihood differences with `log1p`, avoiding subtraction of
nearly equal total likelihoods.

The fitter additionally retains a global continuous upper bound from every
visited endpoint profile and every pruned interval. It accepts a public
endpoint beyond the original evaluation-roundoff gate only when the neighbour
check passes, the existing search converged, and

```
global_upper - public_logLik <= tol * max(1, abs(public_logLik)) + margin.
```

The global margin is the existing
`256 * epsilon * (n + sum(abs(public log densities)) + abs(predicted logLik))`.
Neighbour differences use
`64 * epsilon * n * abs(outward_endpoint - endpoint) / w`.
The generous pruning allowance remains in pruning comparisons; visited-profile
acceptance bounds replace that allowance with the operand-scaled margin above.

These are **exact-arithmetic certificates conditional on the computed
comparisons, with practical floating-point margins**. They are not
outward-rounded interval certificates. The original support and raw-density
checks remain authoritative. The endpoint Newton search, its held-endpoint
factors, starts, stopping rule and work caps required no repair. Default
controls remain `tol=1e-12, maxit=100`; goldens are unchanged.

The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md)
records the public-precision rule. Appended C++ subcases and R regression checks
cover all ten cases, their best public coefficients/likelihoods, and rejection
at `tol=1e-14`. Four existing expected-stop assertions were updated to the
intended recovered successes; no existing test was rewritten to introduce a
new stop.

## Independent evidence and replay

The [reference checker](2026-11-10-triang-endpoint-data/reference.py) uses
400-digit mpmath arithmetic on the exact binary64 inputs. It solves all five
observation-mode profiles for each case and evaluates likelihoods directly in
original coordinates. Concavity tangent upper bounds close the continuous
reference gap below `1e-110`. Independent width-score signs and neighbouring
public likelihoods identify each best public endpoint; continuous upper bounds
exclude all competing modes. The [reference records](2026-11-10-triang-endpoint-data/reference.json)
retain the points, bounds, scores, neighbour gains and coefficient errors.
This is high-precision numerical evidence supporting the conditional
certificate, rather than an interval-arithmetic proof of each floating-point
comparison.

All **168 triangular audit cases** were replayed before and after, at native
and `1e-10` controls, with pruning enabled and exhaustive enumeration:
**1,344 fits** in total.

| Controls | Before Ok | After Ok | After justified numerical stops |
|---|---:|---:|---:|
| Native `1e-12` | 158 | 160 | 8 |
| Uniform `1e-10` | 158 | 162 | 6 |

All 158 prior successes remain successes. Before/after coefficients, raw log
likelihoods, iteration counts and nonuniqueness flags are identical; only the
listed status recoveries change. Pruning and exhaustive enumeration agree in
status, coefficients, likelihood and nonuniqueness; pruning can save iterations.
Original-data support and independent raw-likelihood checks pass for every
successful replay. See [validation.json](2026-11-10-triang-endpoint-data/validation.json).

Reproduce the complete replay and references from the worktree root with:

```sh
bash dev/research/evaluations/2026-11-10-triang-endpoint-data/replay.sh
```

The script obtains the baseline header with read-only `git show`, uses the CPU
slot lock, and writes scratch files under `build/`. The [provenance record](2026-11-10-triang-endpoint-data/provenance.json)
contains source/data hashes and corpus origin.

## Final runs

All final suites completed successfully on the **workstation, GCC 15.2.0**,
using the required slot wrappers and one BLAS/OpenMP thread per worker. R is
4.6.1, BH 1.90.0.1, Python 3.14.5 and mpmath 1.3.0.

| Run | Result |
|---|---|
| `just cpp-test` | 106/106 shards passed |
| `just cpp-test-no-long-double` | 129/129 shards passed |
| `just test` | 22,731 passes; 0 failures; 0 warnings; 154 tier skips |
| `just test-full` | 45,566 passes; 0 failures, warnings or skips |
| `just js-test` | 586/586 passed |
| `just site-check` | Passed, including browser and numerical checks |
| Targeted triangular C++ | 34 cases, 4,039 assertions passed |
| Replay/reference validation | Passed, including complete result comparison |

The site builds also rebuilt WebAssembly. An unrelated tracked browser
screenshot regenerated by `site-check` was restored byte-for-byte from `HEAD`.
The [run receipts](2026-11-10-triang-endpoint-data/test-runs.json) link retained
compressed logs. `git diff --check` passes. Settled d/p/q/r, special-function,
truth-fixture and code-generation sources required no changes.

**Open — second-machine coverage:** simbox remained unreachable on all three
no-long-double attempts (exit 75 before remote testing). The required local
fallback passed all 129 shards. Repeating that suite on simbox/GCC 13 requires
the machine to become reachable. No triangular endpoint case remains
unclassified or unresolved.

**Artifact-pane delivery:** the report was written and its local links verified,
viewer’s readable roots. The viewer documents thread working directories and
`CODEX_MATH_WEB_VIEW_ROOTS` as its registration mechanisms. Updating that
external application or creating another agent thread is outside this job’s
scope; the report remains at the path requested above.
