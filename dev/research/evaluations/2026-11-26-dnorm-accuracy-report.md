# Discretized-normal coefficient accuracy

TASK-368 · Base `2d89fd4a`

The fitter now checks an enclosure of the unique stationary point in public coefficient units. A single observed cell uses its analytic scale or exact midpoint. Adjacent cells near a held-mean seam retain the small score terms that identify scale. All 158 frozen base records match their stored results bit for bit.

## Master reproduction

The compiled master probe returned certified estimates in all four cases below. Each scale is shown to round-trip precision for the compiled columns. The independent references use exact binary inputs, 820-digit mpmath arithmetic for the seam cases and 120 digits for the joint fits. The scale references in the table come from differentiating the single-cell mass. The fixed-scale location is exact by reflection. The adjacent-cell references use bisection on independently evaluated normal-CDF scores, with an inverse-scale bracket narrower than $10^{-65}$ relative. [Master probe](2026-11-26-dnorm-accuracy-data/master.txt) · [References to 45 digits](2026-11-26-dnorm-accuracy-data/master-reference.txt) · [Reproduction script](2026-11-26-dnorm-accuracy-data/replay.sh)

| Fit on `$0$` | Certified result on master | Reference | Final result |
|---|---:|---:|---:|
| Mean held at $-2^{-52}$, scale | 0.11777975643978041 | 0.117779742319532281499 | 0.11777974231953228 |
| Mean held at $-2^{-60}$, scale | 0.10521908075134388 | 0.109646972936213314698 | 0.10964697293621331 |
| Mean held at $-2^{-100}$, scale | 0.10521908075134388 | 0.0849321800288019042722 | 0.084932180028801901 |
| Scale held at $2^{-1074}$, location | NaN | 0.5 exactly | 0.5 exactly |

For distance $\delta>0$ from the held mean to the nearer cell edge, put $B=1/\sigma$, $U=(1+\delta)\phi((1+\delta)B)$ and $V=\delta\phi(\delta B)$. The cell-mass derivative is $U-V$, so equating those terms gives $\sigma_*^2=(1/2+\delta)/\log(1+1/\delta)$. At the root, the negative log likelihood curvature in relative inverse-scale units is $B_*^2H=B_*^3\delta(1+2\delta)\phi(\delta B_*)/p$ per observation. It tends to zero as $\delta$ tends to zero. At $\delta=2^{-100}$, the exact Newton step before $B=10$ lies between $0.49/B$ and $1/B$ from the bounds on $V/U$ and the observed information. The information decreases on this trajectory, so the full step passes the old line search. At the first crossing of 10, the decrement is at most $U/(pB)<10^{-20}$, and the extra step leaves $B\le10.2$. The analytic root has $B_*>11$. Thus the old default test permits a relative scale error greater than $11/10.2-1>7.8\%$ in exact arithmetic. In the fixed-scale NaN case, $1/\sigma$ overflows and the starting $A=(\mu-c)/\sigma$ is computed as $0\cdot\infty$. The early evaluation exits with the initialized zero score, which the old raw-score gate accepted. [Detailed bounds and existence proof](2026-11-25-dnorm-math-report.md#accuracy-failures-and-public-coefficient-checks)

## Implementation and accuracy

- The solver uses centered coordinates $A=(\mu-c)/\sigma$ and $B=1/\sigma$. A held fit uses two signs of the monotone scalar score. The held-mean check forms edge distances directly against the held input and factors the nearer endpoint density out of the score using the known interval width. A joint fit uses a box centered at the candidate and a numerical preconditioner $M$. For each cell use $p=B\int_{-1/2}^{1/2}\phi((m+u)B-A)\,du$. Differentiating under this integral expresses the third derivatives of the negative log mass through covariances and third central moments of bounded functions. Their ranges bound the Hessian variation across the box. The weighted contraction test for $z\mapsto z-Mg(z)$ gives a root inside the box. Strict concavity proves it is the unique MLE. The entire enclosure is converted into public units, including the residual from adding the data center back to the location. The contract uses $\max(\lvert\mu_*\rvert,\sigma_*)$ for location and $\sigma_*$ for scale. [Score evaluation](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/mle.hpp#L103) · [Coefficient enclosure](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/certificate.hpp#L21)
- The inclusion argument is exact arithmetic conditional on the evaluated bounds. The numerical bounds use magnitude-dependent padding based on 32 binary64 epsilons. The decrement only schedules a check, and the solver also checks the last candidate. If the full margin fails, a successful Newton candidate can survive with `certified = FALSE` only if its coefficient enclosure closes under the smaller numerical margin. A candidate that fails both checks yields a failure. Publication checks finite coefficients and evaluates the likelihood at the rounded public pair. [Solver exit and fallback](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/mle.hpp#L206) · [Publication guard](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/held.hpp#L5)
- The single-cell scale uses $\log(1+\delta)-\log\delta$ for small $\delta$ and a reciprocal-scaled ratio for large $\delta$. With scale held, symmetry fixes the unique location at the cell midpoint before taking any reciprocal. The extreme-scale fixed-pair evaluation handles infinite standardized endpoints in the fitter. [Single-cell dispatch](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/mle.hpp#L159) · [Scale formula](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/held.hpp#L23)
- For adjacent cells with held mean, multiplying the inverse-scale score by both cell masses separates the two outer terms from the seam term. With balanced counts the seam difference is an integral of $\phi(u)-\phi(u-B)$ over $[-t,t]$, where $t=\delta B$. Integrating its fourth-order expansion retains the factor $t$ in logs even below binary64 resolution. The monotone score brackets the scale, with rounding included in the final width. With small held scale, a location bracket in $q=(\mu-\mathrm{seam})/\sigma$ uses the sign of the limiting two-cell odds. On the bracket the omitted outer tails and densities are bounded by $e^{-392}$. The location check measures reconstruction error in $q$ units and detects when even the closest representable location misses tolerance. For separated cells at tiny scale, the conditional mean is within $\sigma$ of the projection onto each cell. The projected score with that error bound avoids overflow in squared standardized endpoints. [Adjacent scale](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/held.hpp#L37) · [Adjacent location](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/held.hpp#L116) · [Separated cells](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/held.hpp#L167) · [R tolerance diagnostic](../../../bindings/r/distributions/dnorm/adapter.cpp#L11)

## Routes checked

| Holds | Cases and results |
|---|---|
| Mean only | The four single-cell edge distances (2^{-52},2^{-60},2^{-100},2^{-1074}), balanced adjacent cells at the same distances, and an unbalanced adjacent histogram at $2^{-100}$. On balanced cells $(-1,0)$ at mean $-2^{-100}$, master certified scale 0.10573136325885885 against reference 0.0606222877745911967260. On $(-1,-1,0)$ at that hold, master certified 2.2058727619243856e-17 against 1.83146252051206182243e-30. Both now pass. A held mean of $10^{308}$ is covered by the endpoint-distance bounds. |
| Scale only | A constant cell and balanced adjacent cells give exact locations at $2^{-1074}$. On $(-1,-1,0)$ with that scale, the reference location is approximately $-0.430727299295\sigma$. The closest binary64 location is zero, so the fit returns a tolerance failure. With scale $2^{-100}$ the same histogram gives an accurate finite location. The separated-cell test on $(-2,-2,0)$ has location within $\sigma$ of $-2/3$. A held scale of $10^{308}$ is covered by the range of cell midpoints. |
| Both held | A single observation at subnormal scale with its mean in the cell interior or at the edge gives log likelihood $0$ or $-\log 2$ to rounding. Master produced NaN at the edge. |
| Neither held | The adjacent histogram preserves its certified limit. The finite asymmetric histogram and its $2^{51}$-translated version agree with independent joint references within tolerance. The translated fit retains its joint scale when the public location rounds. |

The maximum normalized coefficient error among representable final probe cases was $4.88\times10^{-11}$ in each backend. C++ and R pin the four reported regressions and additional seam cases. The C++ tests also pin the separated-cell bound. Existing assertions and golden values were retained. [Native results](2026-11-26-dnorm-accuracy-data/final-reference-native.txt) · [Double-only results](2026-11-26-dnorm-accuracy-data/final-reference-double.txt) · [C++ regressions](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/test.cpp#L235) · [R regressions](../../../bindings/r/distributions/dnorm/test.R#L122) · [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md#L115)

## Verification

All runs used the workstation. C++ used GCC 15.2.0 and the installed R/BH headers.

| Check | Result |
|---|---|
| Native and double-only base snapshots | 79 records in each mode, zero moves or missing records |
| Native and double-only modifier snapshots | Zero dnorm rows in the route registry and both stored baselines. The runner was filtered to the empty dnorm route set because its modifier CLI expects a registered route name. |
| `fit-density.py`, `fit-consistency.py` | Zero new failures. Maximum reported-versus-replayed relative log likelihood discrepancy $2.5\times10^{-16}$ across the checked rows. |
| `strict-uniqueness.py` | Passed, 218 singleton proof sites |
| `just test-changed --no-long-double` | Exit 0. Native and double-only C++ each passed 3/3 selected tests for dnorm and its dgamma header dependant. R had 632 passes, 8 skips, zero failures or warnings. The recipe also built the JS binding and passed 241/241 JS tests. |
| `just site-check` | Built 185 pages. The checker passed against that build with installed Brave and a short socket path after the default Playwright browser cache was found missing. All 86 snippets compiled and linked. |

[Validation output](2026-11-26-dnorm-accuracy-data/validation.txt)

## Open

- A uniform floating-point error bound for endpoint probabilities and their derivatives would strengthen the numerical margins used in the contraction and sign checks. The analytic inclusion proof and the compiled high-precision comparisons establish distinct parts of the claim.
- For tiny held scales where a separated-cell location is close to zero, the projected score's $n\sigma$ error bound can be too wide at the location floor. A sharper conditional-mean bound near the gap and a scale-aware summation of signed distances are needed to resolve those cases.
- The standalone density still needs to handle a subnormal held scale at an exact cell edge. The fitter evaluates that case through its endpoint fallback. Carrying the endpoint handling into the public density would close the discrepancy.
