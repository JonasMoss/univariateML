# Generalized gamma nonexistence for balanced two-point samples

TASK-285 · Baseline `2d89fd4a`

## Proof and implementation

For positive endpoints $a<b$, put $c=(\log a+\log b)/2$ and $d=(\log b-\log a)/2$. With equal replication, the standardized log observations have the mean log likelihood of the pair $(-1,1)$. The Lean theorem `balanced_sample_noMLE` proves that each finite parameter vector has raw mean log likelihood strictly below $-1-\log d-c$, and that this value is the supremum. The signed shape paths give the power and Pareto witnesses, each with exponent $1/d$. The power endpoint is $b$ and the Pareto endpoint is $a$. [Math report](2026-11-25-gbshape-math-report.md) · [Lean statement and paths](../../lean/UnivariateML/Family/GenGammaTwoPoint.lean)

[mle.hpp:50](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/mle.hpp) counts the two distinct raw values by equality when all coefficients are free and the sample size is even. It requires two positive finite values and exactly half of the observations at each. The result is `TiedPowerPareto`, which maps to certified `Nonexistence(Unattained)`. The R condition is `univariateML_nomle` with reason `unattained` and both named candidates. Their likelihood fields store the binary64 evaluation of the common total supremum. The decision itself uses exact counts and the theorem. The return at [mle.hpp:292](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/mle.hpp) preserves the raw-scale likelihood when the input falls in the relative-log regime. The dossier records the replication scope at [README.md:93](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/README.md).

## Held coefficients

The production binding replay used `c(1,1,2,2)` with held values $\mu=0.25$, $\sigma=1$, and $Q=0.4$ where applicable. Each held payload matched the baseline in its own precision mode. [Replay script](2026-11-26-gengamma-twopoint-data/probe.py) · [Replay results](2026-11-26-gengamma-twopoint-data/probe.json) · [Held-route theory](2026-11-10-gg-theory-report.md)

| Held | Result | Basis |
|---|---|---|
| $\mu$ | Uncertified finite estimate | With the mode between the log observations, both signs of shape incur a scale penalty and a finite maximum exists. The search reports a local candidate with unknown cardinality. |
| $\sigma$ | Uncertified finite estimate at $Q=0$ | The fixed-scale mode bound excludes the shape tails. Reflection makes the location-profiled likelihood even in $Q$. The reported seam candidate has unknown cardinality. |
| $\mu,\sigma$ | Uncertified finite estimate | The fixed-scale mode bound gives a finite maximum of the scalar shape likelihood. The search reports a local candidate with unknown cardinality. |
| $Q$ | Certified unique estimate | The location-profiled objective is strictly concave in signed log-power on the allowed half-line. |
| $\mu,Q$ | Certified unique estimate | The scale solve uses a strictly concave objective in signed log-power at the held mode. |
| $\sigma,Q$ | Certified unique estimate | The strictly decreasing location score has an explicit zero. |
| $\mu,\sigma,Q$ | Certified evaluation at the supplied vector | The feasible parameter set is a singleton. |

For the interior held-mode claim, let $m=\min(\mu,\log 2-\mu)>0$. For each sign of $Q$, half the observations have $Q(y-\mu)\ge0$. On that side $e^w-1-w\ge w^2/2$ for $w=Q(y-\mu)/\sigma$. The mean likelihood is therefore bounded above by $-\log\sigma-m^2/(4\sigma^2)-R(Q^{-2})-\tfrac12\log(2\pi)-c$, where $R$ is the nonnegative Stirling remainder and its value at $Q=0$ is taken by continuity. The bound excludes both scale extremes uniformly and tends to minus infinity as $|Q|$ grows, uniformly in scale. This establishes existence. Global comparison and cardinality for the reported candidate remain uncertified.

The replay also held $\mu=0$ exactly. Both builds returned `univariateML_numerical` with reason `no-convergence` and a finite search witness at very large negative $Q$. On this slice, the fixed-location Pareto path with $\sigma|Q|=d$ reaches the same supremum in the limit. The all-free strict upper bound applies to every finite point on the slice, so nonexistence is proved here as well.

## References and validation

The independent reference evaluates the power and Pareto log densities at their profiled face coefficients using 80 decimal digits in mpmath. The C++ and R tests pin the rounded values below, the witness models and endpoints, and the outcome classes. The six-point input has relative spread $2^{-23}$ around 8 and alternates endpoints. The 3:1 case retains the baseline's certified Pareto route under the existing floating-point face comparison. [Reference script](2026-11-26-gengamma-twopoint-data/reference.py) · [Reference values](2026-11-26-gengamma-twopoint-data/reference.json) · [C++ tests at line 694](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/test.cpp) · [R tests at line 136](../../../bindings/r/distributions/gengamma/test.R)

| Sample | Reported face exponent | Total log likelihood |
|---|---:|---:|
| `c(1,1,2,2)` | 2.885390081777927 | −1.147653956553452 |
| `rep(c(8 + 2^-20, 8), 3)` | 16777216.99999998 | 81.33654475055311 |
| `c(1,1,1,2)` | 5.770780163555854 | 2.318081946246274 |

- **Stored snapshots.** `diff_baseline.py` found zero differences, additions, or removals for 28 native rows and 28 double-only rows. The frozen rows contain no all-free replicated balanced pair. The additional TASK-285 replay shows both balanced samples moving from `failure(no-convergence)` to certified `nonexistence(unattained)`. The near miss and all held replays are identical to baseline within each mode.
- **Modifier snapshots.** Both requested `--modifier-routes --family gengamma` invocations reached `KeyError: 'gengamma'`. The frozen modifier manifest contains no generalized gamma route, and each stored modifier baseline contains zero `gengamma` rows. Both family-filtered modifier comparisons therefore had zero applicable rows. [Frozen modifier inputs](2026-11-13-retire-16-data/route-inputs.json)
- **Strict checks.** `fit-density.py` and `fit-consistency.py` on the fresh native and double snapshots each reported zero new findings and zero known findings. The density check evaluated 18 items per mode with maximum relative discrepancy $2.1\times10^{-15}$. `strict-uniqueness.py` passed with 218 inventoried proof sites.
- **Targeted suite.** `just test-changed --no-long-double` exited 0 on the workstation with GCC 15.2. CTest passed 1/1 in each precision mode. R dev reported 361 passes, 9 skips, 0 warnings, and 0 failures. The WASM build passed and JavaScript reported 237/237 passes.
- **Site.** `just site-check` passed on the workstation. The first attempt reached the browser check with a missing default Playwright cache. The successful rerun set `BROWSER_EXECUTABLE` to the existing Quarto headless Chromium. All snapshot, replay, reference, and strict runs also used the workstation.

## Open

The fixed-location endpoint example still reaches the generic numerical stop. Its proved Pareto path is established above. Surfacing the result in the held fitter needs an exact endpoint-recognition rule and a held-route outcome payload. For the shape-free finite held fits in the table, a global profile comparison and cardinality certificate remain open.
