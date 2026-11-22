# TASK-230 and TASK-231 — Continuous flat maxima

All ten inventory cases now return certified estimates with `unique = FALSE`, two admissible witnesses and a description of the full maximizing set. The same ten cases pass in workstation native and double-only arithmetic. The representative is the first row of `maximizers`. The binding emits no warning for the tie itself. The free-shape Tukey lambda route retains its upper-restriction flag.

## Maximizer sets and outcomes

The likelihood and dossier agree with each set in the [inventory](2026-11-22-zero-unique-report.md). Write $x_1$ for the common observation and $n$ for the sample size. The held values in the table are those in the strict cases. The detection conditions use the exact sample and hold relations in the third column.

| Family and holds | Full maximizing set | Detection and witnesses | Outcome before → after |
| --- | --- | --- | --- |
| GEV, $\mu=0$, $\sigma=1$ | $-1\le\xi\le1$ | Every observation equals the held location, so the residual is zero and every fitted shape gives density $1/(e\sigma)$. Witnesses $\xi=0,1$. [shape_certificate.hpp:100](../../../univariateML-cpp/include/cppunivariate/distributions/gev/shape_certificate.hpp#L100) | Uncertified unique estimate → certified non-unique estimate |
| Gompertz, $a=1$ | $b>0$ | The sample maximum is zero on the nonnegative support. The density at zero is $a$ independent of $b$. Witnesses $b=1,2$. [mle.hpp:428](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/mle.hpp#L428) | Certified unique estimate → certified non-unique estimate |
| GPD, $\mu=0$, $\sigma=1$ | $\xi\ge-1$ | Every excess is zero, where each fitted shape gives density $1/\sigma$. Witnesses $\xi=0,-1$. [certificate.hpp:162](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/certificate.hpp#L162) | `F:unresolved` → certified non-unique estimate |
| GPD, $\sigma=1$, $\xi=-1$ | $x_1-\sigma\le\mu\le x_1$ | The closed uniform interval must contain the common observation. Witnesses $\mu=0,-1/2$ for the strict fixture. The support check recovers the subtraction residual if the rounded excess equals $\sigma$. [mle.hpp:186](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/mle.hpp#L186) | `F:unsupported` → certified non-unique estimate |
| GPD, $\sigma=1$ | The same uniform threshold interval, together with all $\xi\ge-1$ at $\mu=x_1$ | The pointwise bound is $1/\sigma$. At positive excess, equality requires the uniform member. At zero excess all fitted shapes attain it. Witnesses $(\mu,\xi)=(0,0),(0,-1)$. [mle.hpp:186](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/mle.hpp#L186) | `F:unsupported` → certified non-unique estimate |
| Tukey lambda, $\sigma=2$, $\lambda=1$ | $-2\le\mu\le2$ | The held uniform member gives a constant density on its closed support of radius $\sigma/\lambda$. Witnesses $\mu=0,-1$. [mle.hpp:47](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp#L47) | Certified unique estimate → certified non-unique estimate |
| Tukey lambda, $\sigma=2$, $\lambda=2$ | $-1\le\mu\le1$ | The second uniform member has radius $\sigma/2$. Witnesses $\mu=0,-1/2$. [mle.hpp:47](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp#L47) | Certified unique estimate → certified non-unique estimate |
| Tukey lambda, $\sigma=2$ | $\lambda=1$ and $-2\le\mu\le2$ | For $\lambda<1$, convexity of $u^{\lambda-1}$ bounds the peak density by $2^{\lambda-2}/\sigma<1/(2\sigma)$. The uniform member attains the larger density. Witnesses $\mu=0,-1$. [mle.hpp:47](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp#L47) | Uncertified unique estimate → certified non-unique estimate |
| Tukey lambda, $\sigma=3$, $\lambda=3/2$ | $\mu=-2$ or $\mu=2$ | For a held shape in $(1,2)$, the density denominator $\sigma[u^{\lambda-1}+(1-u)^{\lambda-1}]$ has its minimum only at $u=0,1$. The two witnesses are the support contacts, checked in public coordinates. [mle.hpp:63](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp#L63) | `F:unresolved` → certified non-unique estimate |
| SGED, mean 0, SD 1, $\nu=9/4$ | The two reciprocal skews at the unique centered profile root | Every observation equals the held mean and the held shape is exactly $9/4$. Signed bisection encloses one root on the reciprocal-symmetry coordinate. The two witnesses lie on opposite sides of $\xi=1$. [mle.hpp:553](../../../univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp#L553) | `F:unresolved` → certified non-unique estimate |

For SGED, the exact-arithmetic argument uses $u=1-\min(\xi,1/\xi)^2$ and the derivative sign $K-H(u)$ from the inventory. The polynomial controlling $H'$ has one zero on $[0,1]$, and $H(0)<K<H(1)$, so $H(u)=K$ has one crossing. The retained interval calculation verifies the signs and brackets the crossing in $(0.0008,0.001)$. The code uses a $128\epsilon$ allowance for computed signs and bounds the relative coefficient error before returning the skew and its reciprocal. The finite likelihoods of the two witnesses are evaluated in public coordinates. The existence and equality of the two maxima follow from the profile and reflection. [SGED dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#fixed-shape-with-both-moments-held), [interval sign margins](2026-11-22-flat-cont-data/sged-signs.txt).

## Regression coverage

The C++ tests check the estimate kind, certification, uniqueness, witness membership, public density and set diagnostic. The R tests exercise the public fitters and check the same reporting contract through `unique`, `maximizers` and `outcome_message`. The singleton controls fully hold the coefficients in GEV, Gompertz, Tukey lambda and SGED. GPD checks the fully held point and the unique uniform shape when the threshold and scale are held and the sample includes a positive excess.

| Family | C++ and R regressions |
| --- | --- |
| GEV | [C++:1103](../../../univariateML-cpp/include/cppunivariate/distributions/gev/test.cpp#L1103), [R:508](../../../bindings/r/distributions/gev/test.R#L508) |
| Gompertz | [C++:298](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/test.cpp#L298), [R:129](../../../bindings/r/distributions/gompertz/test.R#L129) |
| GPD | [C++:487](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/test.cpp#L487), [R:200](../../../bindings/r/distributions/gpd/test.R#L200) |
| Tukey lambda | [C++:401](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/test.cpp#L401), [R:303](../../../bindings/r/distributions/tlambda/test.R#L303) |
| SGED | [C++:1452](../../../univariateML-cpp/include/cppunivariate/distributions/sged/test.cpp#L1452), [R:1020](../../../bindings/r/distributions/sged/test.R#L1020) |

## Snapshot comparisons and suites

Each mode has 901 direct-fit records. Against branch base `7d0dd9ec`, exactly the ten rows above changed in each mode. The five cases that already returned estimates kept their parameters and log likelihoods. The frozen snapshots contain 891 matching keys and predate the ten explicit flat-maximum fixtures. Among those 891, the only differences are 15 inherited SGED records per mode. All 15 agree with the branch-base replay. The held-location fix accounts for nine changes to mean, log likelihood and iteration count and six former numerical stops that now attain certified estimates. [Per-record before and after](2026-11-22-flat-cont-data/snapshots.txt), [SGED held-location evidence](2026-11-22-sged-loc-report.md#canonical-snapshots).

| Check | Machine | Result |
| --- | --- | --- |
| `just test-changed --no-long-double` | Workstation, GCC 15.2, four slots | Selected the five families plus Fréchet. Nine C++ targets passed in each mode. R passed 2,270 expectations with zero failures, zero warnings and 18 skips. JS built and passed 252/252 tests. |
| `fit-consistency.py`, current native and double-only snapshots | Workstation, one slot | Zero failures. All 20 listed findings for these families now pass. |
| `just site-check` | Workstation, one slot, `EMCC_CORES=1` | Passed, including 85 family pages and 85 generated C++ examples. |
| Stored and branch-base fit snapshot comparisons | Workstation, GCC 15.2, one slot | 901 records per mode. Ten case changes against base and 15 inherited changes among the 891 frozen keys. |

The first JS run identified an existing rejection for a nonconstant GPD sample with free threshold. The new GPD threshold route was narrowed to constant samples before the final rerun. The first site check detected changed inference metadata. The final dossier documents the new fitting subset and preserves the inference registry. The replay script rebuilds the branch base from Git into ignored scratch and invokes the frozen baseline comparator and strict check. [Suite output](2026-11-22-flat-cont-data/suite-results.txt), [strict output](2026-11-22-flat-cont-data/strict.txt), [replay script](2026-11-22-flat-cont-data/replay.sh).

## Open

- The GPD free-threshold feature in this job covers constant samples with held scale and uniform or free shape. The existing rejection for nonconstant samples also covers slices where a uniform interval gives an analytic maximum. A broader fitting contract would require agreement with the binding behavior for those slices. The inference registry still describes free-threshold inference as unavailable. Its historical rationale predates the constant-sample fit.
- For a sufficiently narrow translation interval at a large offset, binary64 can have only one admissible coefficient value even though the real set contains infinitely many. The uniform-location witness check reports conditioning when a second point cannot be represented. For Tukey lambda's two-contact case, the existing certificate remains the fallback if both contacts cannot be recovered at the requested accuracy. No inventory case hits either limitation.
