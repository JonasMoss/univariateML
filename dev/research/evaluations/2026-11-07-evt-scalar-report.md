# Fréchet and GEV scalar certificates — TASK-116.30

J10a is complete. Fixed-shape Fréchet fits with free location now use globally
single-crossing equations, including the two-free-parameter location/scale
profile. GEV with held scale and held nonpositive shape has a certified location
solve. The requested subsets either return the unique global maximizer to the
stated numerical tolerance or stop with a statistical or numerical diagnostic.
All required final suites passed. Goldens were preserved, and default benchmark
coefficients and likelihoods were bit-identical to the baseline.

Both family-level `fit.certified` values remain `partial`: free-shape searches
and the other conditional GEV routes are outside these certificates. Their
`fit.note` entries now describe the enlarged coverage precisely.

## Scope and mathematical results

| Family and holds | Result |
|---|---|
| Fréchet: `lambda`, `sigma` held; `mu` free | Unique finite location maximum for every nonempty finite sample and every positive held shape and scale. |
| Fréchet: `lambda` held; `mu`, `sigma` free | Unique finite joint maximum exactly when `E=lambda*(n-k)-k>0`, with `k` the original sample-minimum multiplicity. At zero, the supremum is finite and unattained; below zero, it is infinite. |
| Fréchet: `mu` held | Existing inverse-Weibull certificates retained. |
| GEV: `sigma`, `xi` held; `mu` free; `-1<xi<0` | Strictly concave location likelihood and a bracket containing its unique global maximum, including constants and single observations. |
| GEV: held `xi=0` or `xi=-1` | Existing analytic Gumbel location and attained reverse-exponential endpoint cases retained. |

The proofs and numerical contracts are in the canonical
[Fréchet dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md)
and [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md).

### Fréchet: stronger than a bounded search

Write `epsilon=min(x)-mu`, `g_i=x_i-min(x)`, and
$S_p=\sum_i(g_i+\epsilon)^{-p}$. With scale profiled out,
$\hat\sigma^\lambda=n/S_\lambda$, and the location-gap score has the sign of
$n\lambda R-(\lambda+1)$, where

$$R=\frac{S_{\lambda+1}}{S_\lambda S_1}.$$

For $q_i=(g_i+\epsilon)^{-1}$, let
$E_p(q)=\sum q_i^{p+1}/\sum q_i^p$. The weighted covariance of $q$ and
$\log q$ is positive for nonconstant data, so $E_p(q)$ increases strictly
with $p$. Therefore

$$\frac{d\log R}{d\epsilon}
=-(\lambda+1)E_{\lambda+1}(q)+\lambda E_\lambda(q)+E_1(q)<0.$$

Its endpoint limits are $1/k$ and $1/n$. This proves the existence criterion,
uniqueness and globality without assuming joint concavity or enumerating roots.
Scale has a unique conditional maximum at every positive gap. Holding scale
instead leaves the strictly decreasing ratio $S_{\lambda+1}/S_1$, with limits
infinity and zero, and hence exactly one location root.

These strict results exclude distinct equal finite maxima in the newly
certified regimes. No uniqueness claim is made for the remaining joint
free-shape problems. Useful counterexamples to unconditional finite existence
are:

- With only `lambda=1` held, `x=(0,0,1,2)` has the unattained supremum
  $2\log2-4$.
- With only `lambda=1` held, `x=(0,0,0,1)` has unbounded likelihood.
- Constants have a unique location fit when shape and scale are held; their
  scale-collapse obstruction applies when scale is free.

At `E=0`, the reported global supremum is
$n\log(n\lambda)-n-n\log k-(\lambda+1)\sum_{g_i>0}\log g_i$.
The exponent uses fused multiply-add. In particular, the double `1/3` lies
below the exact collapse boundary for `(0,1,2,3)`, even though ordinary
multiplication by three rounds to one. The adjacent larger double is never
classified as statistical non-existence. Scaled log differences preserve the
finite supremum for `(-1e308,1e308)` at held shape one.

### GEV: curvature and an explicit bracket

At held scale and `-1<xi<0`, put $t_i=1+\xi(x_i-\mu)/\sigma$. Then

$$\ell_{\mu\mu}=\frac{1+\xi}{\sigma^2}
\sum_i t_i^{-2}(\xi-t_i^{-1/\xi})<0.$$

Individual score crossings occur at
$x_i+\sigma[1-(1+\xi)^{-\xi}]/\xi$. Their extremes, intersected with
$\mu>\max x+\sigma/\xi$, bracket the global root. The implementation uses
`log1p` and `expm1` for the crossing and score differences near zero shape.
The Fréchet 500-point golden sample, held `sigma=2, xi=-0.3`, provides a
regression outside the former transformed search window: its optimum has
transformed coordinate greater than 32. The old search stopped; the new solve
returns the certified location, approximately `24.731852345`.

## Implementation and numerical limits

The new Fréchet helper is
[certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/certificate.hpp).
It evaluates ratios with `r_i=epsilon/(g_i+epsilon)`, whose power sums retain
at least one unit minimum term. A log-gap bracket expands until its scores have
opposite signs, then bisection isolates the unique root. There is no fixed
location window. GEV uses its analytic location bracket directly.

These are exact-arithmetic certificates conditional on computed comparisons,
with practical floating-point safeguards; they are not outward-rounded
interval certificates:

- Fréchet root width is `reltol*(1+abs(log_gap))`. Diagnostic witness likelihood
  margins are `256*epsilon_machine*n*(1+lambda)^2` in normalized units.
- Its normalized returned point must satisfy
  $256\epsilon_{\rm mach}\lambda(1+|\log\sigma|+\max_i|\log(g_i+\epsilon)|)
  \le10^{-7}$. This conservative precision check rejects enormous held powers
  that can otherwise turn rounded log distances into a spurious point fit.
- GEV root width is `reltol*sigma`, with total dimensionless location-score
  residual below `1e-3`.
- Restored successful likelihoods must agree with their normalized values,
  including the Jacobian, within `1e-7*n`. Unrepresentable results stop
  numerically. An overflowing Fréchet normalization with positive `E` is a
  numerical stop, never a non-existence assertion.

Bracket and root loops each use `min(iterlim,100000)` solver iterations.
Exhaustion is `IterationFailure`, exposed as a structured `numerical` stop,
consistent with [work budgets](../../design/work-budgets.md). There is no
certificate-enumeration budget in the delivered monotone algorithm.

`StopDetail` is appended to the family results. Common binding headers classify
the stops for both R and JS. Fréchet retains its historical core `InvalidData`
status for the rejected fixed-shape collapse holds, while their explicit
supremum payload makes the public stop `nomle`. Invalid inputs remain ordinary
errors. Solver exhaustion retains a finite witness when available and leaves
an unclosed global upper bound infinite. Restored witness bounds include the
likelihood reevaluated at the reported original-coordinate coefficients;
unrepresentable witnesses are removed. Arithmetic failures can have no
candidate. No stopped payload becomes an estimate.

An initial compact-tail subdivision implementation was superseded by the
single-crossing proof. Its
[non-built prototype](2026-11-07-evt-scalar-data/superseded-subdivision.hpp)
is retained as historical evidence. Monotonicity removes its subdivision and
tie-enumeration machinery and establishes uniqueness as well as globality.

## Final validation

Base: `7c52f905f21bee7d997d59211d4093e2593ec727`, branch `codex/evt-scalar`, plus
uncommitted changes identified by the [source hashes](2026-11-07-evt-scalar-data/source-state.txt).
No commits were made. C++ is C++17; workstation compiler is GCC 15.2,
simbox standalone C++ uses GCC 13.3, and simbox R 4.6.1 builds with GCC 15.2.
Workstation runs used `/tmp/umvl-cpu/run` or `run4`; simbox used the supplied
mirror runner. Long jobs ran at nice 10, with BLAS/OpenMP pinned to one thread
and four build/test workers in the final suite driver. Temporary files stayed
under `build/tmp`.

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 shards passed; 75.28 s test time |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129 passed; 216.88 s |
| `just cpp-test-no-long-double` | simbox | 129/129 passed; 185.13 s |
| `just cpp-test-no-long-double` | workstation | 129/129 passed; 270.85 s |
| `just test` | simbox | 21,253 passes; 154 skips; zero failures or warnings |
| `just test-full` | simbox | 44,071 passes; zero failures, warnings or skips |
| `just js-build`, `just js-test` | workstation | Build succeeded; 470/470 tests passed |
| `just site-check` | workstation | Passed, including 99 generated C++ snippets, family MathML and browser checks |

The [suite driver](2026-11-07-evt-scalar-data/run-suites.sh),
[simbox log](2026-11-07-evt-scalar-data/simbox-validation.log.gz),
[workstation no-long-double log](2026-11-07-evt-scalar-data/cpp-no-long-double-workstation.log)
and [workstation validation log](2026-11-07-evt-scalar-data/workstation-validation.log)
retain the commands and output. Compiler output includes existing warnings in
other families; the R `WARN` counts above are testthat counts. The dev skips
are retained in the log; full ran with no skips. Queue time is excluded from
the displayed test times.

Coverage includes all held subsets on goldens, independent R multistart
comparisons, dense feasible competitors, affine transformations, constants,
single observations, near-zero GEV shapes, the old location-window failure,
collapse-adjacent doubles, overflow and rounded-witness cases, and enormous
held powers. The defaults matched the original golden coefficients and
likelihoods within their unchanged tolerances; no fixture was regenerated.

Independent formula checks, seed `11630`, passed 1,494 finite-difference,
curvature and exterior-envelope checks (worst Fréchet score discrepancy per
observation `9.16e-10`, threshold `1e-7`), and 10,000 monotonicity identities
with allowance `1e-12*(lambda+1)`. These are finite numerical checks, separate
from the algebraic proof. Eight targeted native stop/payload checks passed on
both machines. The WASM check passed collapse-sign classification, the finite
supremum across an overflowing range, and the numerical precision stops.
The scripts and [formula results](2026-11-07-evt-scalar-data/math-check.json)
are retained alongside the report.

## Before/after timings

Workstation, GCC 15.2, `-O2 -std=c++17`, one reserved CPU slot. The
[benchmark](2026-11-07-evt-scalar-data/benchmark.sh) reconstructs the two baseline
family headers from `7c52f905`. Five paired batches use 200 fits at `n=7` and
20 fits at `n=500`; the latter is Fréchet golden seed `13082`. Times below are
median microseconds per fit. CPU frequency and other workstation load were
not controlled.

| Workload | Before | After |
|---|---:|---:|
| Fréchet default, n=500 | 4,153.57 | 4,218.53 |
| Fréchet held lambda=2, n=500 | 3,209.68 | 332.37 |
| Fréchet held lambda=2, sigma=1, n=500 | 1,156.39 | 281.88 |
| GEV held xi=-0.3, sigma=2, n=7 | 29.63 | 7.43 |
| GEV default, n=500 | 1,945.08 | 1,942.84 |
| GEV held xi=-0.3, sigma=2, n=500 | 1,170.99 (failed) | 545.42 (certified success) |

Both default routes were bit-identical before and after at both sample sizes.
The failed GEV baseline is an availability comparison, not a speedup between
two successful fits. [Raw batches, ranges and medians](2026-11-07-evt-scalar-data/timing-summary.json)
are retained; the earlier apparent default Fréchet slowdown did not persist in
the final paired measurements.

## Open outside J10a

- **Fréchet, free `mu` and `lambda`, with `sigma` free or held:** no certified
  outer-shape coverage. The new fixed-shape inner solves do not by themselves
  bound joint location-gap/shape escapes. Free scale additionally requires
  uniform Gumbel-face and half-tie neighborhood comparisons. No joint
  uniqueness theorem or counterexample is asserted.
- **GEV remaining conditional and joint profiles:** positive held-shape
  location searches and free-scale/free-shape profiles retain their existing
  numerical routes. A fixed-positive-shape location certificate can be
  transported from Fréchet, but that route was outside the listed nonpositive
  GEV work. Shape-varying searches still need global coverage across moving
  support seams and endpoint/collapse corners; finite meshes and local
  refinement do not provide it.

These are the J10b/general-profile gaps described in the
[triage report](2026-11-07-partial-triage-report.md), not omissions from the
three requested J10a routes. The family flags remain `partial` for this reason.
Settled d/p/q/r, special functions, truth fixtures and codegen were unchanged;
no required change to them was identified.

## Delivery issue

The report file and its links/source hashes were verified. The required browser
pane push was attempted, but the viewer returned HTTP 403: this worktree is
outside its registered readable roots. Display requires a viewer thread/root
registration for `/home/jonas/Files/programming/univariateML.wt/evt-scalar`.
No viewer configuration or files outside the worktree were changed to bypass
that restriction. This display issue does not affect the completed certificates
or validation results.
