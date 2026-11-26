# Discrete Weibull coefficient accuracy after rounding shape1

TASK-349 · Baseline `2d89fd4a117a7ee7850558e6a04b4c4d74b87589` · Branch `codex/dweibull-refit`

For `mldweibull(c(8, 10))`, master certified a shape of `11.597027132635398`, with relative error `6.86874055687e-7` against the joint MLE. The corrected fitter returns `11.597019166951243`, with relative error `6.41037452252e-13` at the default `tol = 1e-10`. Both runs return probability `0x1.fffffffff9fc1p-1`. The 90-digit reference shape starts with `11.59701916694380933928363931678462655`.

## Cause and fix

Write $t=-\log q$, $\alpha=\log t$, and $\beta=\mathrm{shape2}$. The old fitter rounded $q$ to binary64 and then maximized over $\beta$ at that rounded $q$ whenever $t<10^{-6}$. Near one, the small absolute rounding error $\Delta q$ can make a large change in $\alpha$, since its first-order effect is $-\Delta q/(qt)$. The first-order displacement of the held-$q$ root is $-\ell_{\beta\alpha}\Delta\alpha/\ell_{\beta\beta}$ at the joint root. On $(8,10)$, $t$ is about $2.73552921524930\times10^{-12}$ and the relative error in $q$ is only $5.07\times10^{-17}$, while the resulting shape error exceeds tolerance by a factor of about 6,869. [Prior analysis](2026-11-25-dweibull-math-report.md#numerical-evidence-and-coefficient-restoration)

`finish` now returns the rounded probability and the shape from the joint solve. It evaluates `logLik` at that public pair. At the sharpest representable grid point, $(10,12)$, the new log likelihood is about $1.56\times10^{-6}$ lower than that of the old conditional refit. The coefficient and public log-likelihood checks measure distinct properties. [mle.hpp:474](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/mle.hpp#L474)

**Exact arithmetic.** The certificate probes shape endpoints inside the relative-error band for the returned shape. At each endpoint, opposite inner score signs enclose the maximizing $\alpha$. The raw shape score decreases with $\alpha$, so evaluation at the upper inner endpoint gives a lower bound on the profile score and evaluation at the lower inner endpoint gives an upper bound. A strictly positive lower bound at the lower shape endpoint and a strictly negative upper bound at the upper shape endpoint enclose the unique profile maximizer. Profiled $q$ increases with shape, so the same two inner brackets bound the jointly maximizing $q$. The test uses those probability bounds on the log scale and checks the actual rounded $q$ against both. For a relative tolerance below one this amounts to checking that $\log(q_{\mathrm{returned}})-\log(q_*)$ lies between $\log(1-\mathrm{tol})$ and $\log(1+\mathrm{tol})$ throughout the enclosure. The held routes use the corresponding scalar check. [mle.hpp:365](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/mle.hpp#L365) · [Monotonicity proof](2026-11-25-dweibull-math-report.md#quantitative-localization-and-signed-brackets)

**Floating point.** The implementation requires computed score signs to clear practical margins scaled by machine epsilon, the log-hazard scale, and the number and weight of occupied cells. The derivative proposes an inner bracket radius. Certification requires fresh sign evaluations at both ends, plus margins for probability restoration and comparison. When the checks are inconclusive, the successful search fit carries `certified = FALSE`. The exact uniqueness proof still supplies `unique = TRUE` for that fit. The existing failure and limit paths retain their outcomes. [mle.hpp:61](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/mle.hpp#L61) · [mle.hpp:405](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/mle.hpp#L405)

## Reproduction and route search

The replay compiled the C++ probe from a scratch tree obtained with `git archive 2d89fd4a univariateML-cpp/include` before editing the fitter. It compiled the probe again against the corrected worktree. mpmath 1.3.0 used 90 decimal digits and differentiated the survival difference directly. The reference solved the joint score system, the held-shape probability score, and a signed 275-step bisection for the held-probability shape. Every solved free-coordinate score had absolute residual below $10^{-75}$ in the replay. The evidence retains 55 significant reference digits, exact binary64 holds, and fitted values at round-trip precision. [Replay script](2026-11-26-dweibull-refit-data/replay.py) · [C++ probe](2026-11-26-dweibull-refit-data/probe.cpp) · [Baseline output](2026-11-26-dweibull-refit-data/baseline.json) · [Corrected output](2026-11-26-dweibull-refit-data/fixed.json)

| All-free sample | Reference shape | Master shape | Master relative shape error | Corrected relative shape error |
|---|---:|---:|---:|---:|
| (5, 7) | 7.9369416340898969 | 7.9369416340959136 | 7.58e-13 | 3.49e-16 |
| (6, 8) | 9.1594685331321826 | 9.1594685349523051 | 1.99e-10 | 2.30e-15 |
| (7, 9) | 10.379193015081321 | 10.379192989392369 | 2.48e-9 | 9.42e-12 |
| (8, 10) | 11.597019166943809 | 11.597027132635398 | 6.87e-7 | 6.41e-13 |
| (9, 11) | 12.813498283847933 | 12.813117332121369 | 2.97e-5 | 1.10e-15 |
| (10, 12) | 14.028986273267453 | 14.029532710031967 | 3.90e-5 | 1.65e-11 |
| (12, 14) | 16.457877990879498 | Probability rounds to one | | |
| (15, 17) | 20.097911333009296 | Probability rounds to one | | |
| (7, 10) | 7.2070804371873180 | 7.2070804371922614 | 6.86e-13 | 7.25e-12 |
| (8, 11) | 8.0166466720924564 | 8.0166466725020555 | 5.11e-11 | 1.65e-12 |
| (9, 12) | 8.8246598237950902 | 8.8246598502747098 | 3.00e-9 | 4.70e-12 |

Six master fits had certified shapes outside tolerance. All nine representable all-free fits now certify and meet tolerance in both coefficients. The two excluded-endpoint stops also reproduced after the change.

The search covered all four hold masks on all eleven samples. With $q$ held, the exact supplied double was the rounded joint probability, or the largest double below one when the joint probability rounded to one. Its worst fitted-shape error was $9.31\times10^{-13}$. With shape held at its rounded joint value, the worst relative probability error was $5.08\times10^{-17}$ among the nine representable fits, and the two remaining fits stopped at the excluded endpoint. With both held, both values were returned exactly. The held routes showed no instance of the refit defect. Their public-coefficient checks use ordinary scalar brackets or the fully held singleton. Across all forty successful corrected fits, the maximum scaled difference between reported and 90-digit public-pair log likelihoods was $2.10\times10^{-15}$.

## Regression tests and final checks

The C++ and R cases pin the full restoration grid and the `(8,10)` result at the default tolerance. At `tol = 1e-13`, `(8,10)` exercises the uncertified fallback and still agrees with the independent reference at the requested tolerance. The existing peaked-sample test retains its central-difference stationarity check on an explicit held-$q$ fit and checks the all-free fit's profile score at its profiled probability. This keeps the existing held-score threshold and tests the changed joint-fit behavior. No existing numerical pins changed. [C++ tests](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/test.cpp#L390) · [R tests](../../../bindings/r/distributions/dweibull/test.R#L137) · [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md#r-notes)

| Check | Machine | Result |
|---|---|---|
| Native and double-only ordinary snapshots against stored baselines | Workstation, GCC 15.2 | 80 matched in each mode, zero differing, missing, or added records |
| Native and double-only modifier snapshots | Workstation | Zero applicable records in each mode. Explicit family filtering raised `KeyError` in each because the family is absent from the frozen route inventory. Both stored route baselines also have zero family records. |
| `fit-density.py` and `fit-consistency.py` on the fresh snapshots | Workstation | Zero new findings for each. Density checked 62 likelihoods per mode, with maximum scaled difference $2.2\times10^{-16}$. |
| `strict-uniqueness.py` | Workstation | Passed, 218 source sites. Existing discrete Weibull proof inventory entry covers both certification outcomes. |
| `just test-changed --no-long-double` | Workstation, GCC 15.2 | Passed. Native and double-only CTest each 1/1. R dev 341 pass, 8 skip, 0 fail, 0 warn. JavaScript build passed and tests passed 237/237. |
| `just site-check` | Workstation | Passed, including compilation and linking of 86 generated C++17 snippets and the HTML spot checks. |

The changed cases are covered by the new regression pins and replay. The stored ordinary snapshots contain no changed record, and the golden files were untouched. [Stored baselines](2026-11-15-fit-snapshot-data/) · [Modifier inputs](2026-11-13-retire-16-data/route-inputs.json)

## Open

- A uniform floating-point enclosure would require proven evaluation and summation error bounds for the weighted scores and the coordinate transforms. The implemented checks use practical margins and the exact-arithmetic monotonicity argument.
- A finite optimum whose probability rounds to an excluded endpoint still requires a representation policy. Returning the nearest interior double would call for quantifying its likelihood loss, especially where its distance from one is many orders of magnitude larger than the maximizing $1-q$.
