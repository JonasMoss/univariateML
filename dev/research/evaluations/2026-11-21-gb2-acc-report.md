# Feller–Pareto and GB2 fit accuracy, negative-binomial brackets, and beta likelihood

TASK-214.44 and the two Feller–Pareto entries of TASK-214.13.
Baseline `8b3b2dba`, branch `codex/gb2-acc`. The reference checker
reconstructs the archive inputs from their exact binary64 values and
solves the likelihood equations at 100 decimal digits. The reference
JSON records the fits, likelihoods, and the free-scale profile studied
for the two unresolved cases. [Reference checker](2026-11-21-gb2-acc-data/reference.py) · [Reference results](2026-11-21-gb2-acc-data/reference.json)

## Feller–Pareto and GB2

With both beta shapes held, the log likelihood is strictly concave in
the free coordinates of $(A,c)$, where $A=a s$, $s$ is the standard
deviation of the log data, and $c=a(\log b-m)$. The fitted log scale is
$m+s c/A$. When power is held, its sensitivity to the only free
coordinate $c$ is $s/A=1/a$. The old radius and score allowance
included a relative power error and the $n/A$ power score. With
$A$ near $2\times10^{-8}$, that radius made the curvature allowance
smaller than the score margin. The held-power gate now uses radius
$\min(\mathrm{tol},10^{-9})a/4$, varies only $c$ in its curvature
bound, and drops the power-score allowance. The curvature inequality
$\|g\|<Mr/2$ then encloses the unique stationary point, conditional on
the computed comparisons and their floating-point margins.
[gb2/concave_certificate.hpp:26](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/concave_certificate.hpp#L26)

At extreme common scales, the old input-error allowance charged the
absolute log level to each standardized observation and to scale
restoration. The common log origin cancels in the score. GB2 now uses
relative logs in this regime, restores the scale against the data
anchor, and evaluates the likelihood at the returned coefficients.
This also handles the extreme held-power case 010745, where the
previous scale-restoration check was sensitive to the absolute log
level. The free-power curvature radius retains its earlier formula.
[gb2/mle.hpp:282](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp#L282) · [gb2/mle.hpp:389](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp#L389)

In 001752, both beta shapes are free and power and scale are held.
The old probability transformation rounded the variation of the beta
observations before the concentration solve. Since concentration
depends on a second-order dispersion statistic, the input-error check
could not establish shape accuracy. The new reduction retains logit
differences from the first observation. It evaluates the Bernoulli KL
remainder around the mean logit and adds the KL term for displacement
of the shape odds. It brackets the log concentration and the centered
log odds with computed signs. The latter width also controls the
quadratic change in the dispersion statistic. With fixed power and
scale, strict concavity of the beta likelihood in its two shapes
identifies the enclosed stationary point as the unique maximum.
The reduction checks the likelihood at the rounded shape ratio.
[gb2/concentrated_beta.hpp:15](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/concentrated_beta.hpp#L15)

| Archive group | Cases | Largest relative free-coefficient error, either mode |
|---|---|---:|
| Feller–Pareto extreme scale | 001674, 001675, 010736, 010745 | $2.65\times10^{-12}$ |
| Feller–Pareto held power, small spread | 001699, 001700, 001702, 001703, 010320, 010321 | $1.09\times10^{-16}$ |
| Feller–Pareto held power and scale, concentrated beta shapes | 001752 | $1.28\times10^{-15}$ |
| GB2 held power, small spread | 002191, 002192, 002194, 002195, 010404, 010405 | $1.43\times10^{-13}$ |

For 001625 and 001628, power is held and scale and both beta shapes
are free. The five observations span about $5\times10^{-12}$ in
relative units. The baseline native joint polish failed after the
search nominated shapes around $1.57\times10^{16}$. The double-only
search ended unresolved. At the native nomination, the independent
log-concentration score at fixed shape ratio and scale is
$+2.499999772$ and the exact-point likelihood is about $89.78786$.
The candidate is still ascending in concentration. Fixing scale
temporarily at the first observation gives a conditional solution
with shapes around $1.72308\times10^{23}$ and likelihood
$127.8143079840948$. Both modes retain that witness, with shape errors
below $2.74\times10^{-15}$ against the conditional reference. Both
return `unresolved`. The approximately $3.78\times10^{-12}$
likelihood half-width on the witness exceeds the $1.12\times10^{-12}$
gain from the central profile to the fitted inverse transformed-gamma
face. [fpareto/mle.hpp:66](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp#L66) · [100-digit profile and face fits](2026-11-21-gb2-acc-data/reference.json)

## Negative binomial

The certificate previously closed to relative width $5\times10^{-7}$,
so a midpoint could miss tighter user tolerances. A Newton proposal
now survives only when the distances to both signed endpoints are
small enough. Otherwise the fitter bisects to width at most
$1.9\,\mathrm{tol}$ times the lower endpoint and takes the midpoint.
If a midpoint sign is ambiguous, interior probes can advance either
signed endpoint. The profile probability $r/(r+\bar x)$ has relative
sensitivity at most one to size $r$. The enclosure is conditional on
padded signs, and the stopping slack leaves room for binary64 rounding
at the tested tolerances. [nbinom/certificate.hpp:153](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/certificate.hpp#L153)

For near-Poisson data whose integer dispersion numerator can be
computed exactly, the score now removes the leading mean and
dispersion terms before summation. The recurrence remainder is
$j^2/[r(r+j)]$ and the logarithmic remainder is
$\log(1+u)-u+u^2/2$ with $u=\bar x/r$. The signed leading term
$(\bar x-S^2)/(2r)$ is restored separately. This makes the practical
sign allowance scale with the remaining terms. For samples containing
only large counts, centering the Euler–Maclaurin log integrals about
the mean removes the linear sum. The same computed-sign rule thereby
passes the existing one-iteration, large-count, and near-Poisson tests.
[nbinom/certificate.hpp:49](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/certificate.hpp#L49) · [nbinom/certificate.hpp:88](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/certificate.hpp#L88)

For the size-3330 fixture, the 100-digit size root begins
$3330.4334203884434586$. The legacy proposals were $3330.4587091$
(native) and $3330.4066901$ (double-only), relative errors of
$7.59\times10^{-6}$ and $8.03\times10^{-6}$ from score cancellation.
The new size errors were $4.41\times10^{-7}$ and $8.01\times10^{-8}$
at `tol = 1e-6`, then $8.82\times10^{-15}$ and $2.10\times10^{-14}$
at `tol = 1e-13`. [Control probe](2026-11-21-gb2-acc-data/nbinom-controls.cpp) · [Measured controls](2026-11-21-gb2-acc-data/nbinom-controls.txt)

The first dependant test run exposed a separate probability-rounding
issue in the existing bnbinom witness fixture. At size about
$1.00\times10^{14}$, rounding the profiled probability shifts the
implied mean by about $8.15\times10^{-4}$ and puts the returned point
about $3.32\times10^{-6}$ log-likelihood units below the Poisson limit.
The fitter now checks that comparison near probability one and retains
the finite nbinom witness on a precision stop. The late change was
retested through both C++ modes, the targeted R and JS suites, and the
final snapshots. [nbinom/certificate.hpp:191](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/certificate.hpp#L191)

## Beta

The likelihood uses compensated sums of $\log x$ and $\log(1-x)$.
Against the 100-digit ordinary golden value
$147.781339914636667158\ldots$, the native relative error fell from
$5.14\times10^{-15}$ to $2.83\times10^{-15}$. In double-only mode it
fell from $3.99\times10^{-15}$ to $9.10\times10^{-16}$. The existing
golden test passes in both modes. [beta/mle.hpp:273](../../../univariateML-cpp/include/cppunivariate/distributions/beta/mle.hpp#L273)

## Canonical snapshots and strict entries

The comparison includes the thirteen selected families and the
zero-inflated nbinom route, which directly calls the changed size
certificate. All 2,580 cases per mode matched baseline inputs.
The largest movement is measured between two estimates as
$|\hat\theta_\mathrm{new}/\hat\theta_\mathrm{old}-1|$ for nonzero
baseline coefficients. The JSON also records scaled likelihood moves
and per-case reference errors. [Comparison](2026-11-21-gb2-acc-data/snapshot-comparison.json) · [Snapshot provenance](2026-11-21-gb2-acc-data/snapshot-provenance.json)

| Family or route | Cases per mode | Changed native / double | Largest relative coefficient move |
|---|---:|---:|---:|
| bbinom | 148 | 0 / 0 | 0 |
| beta | 115 | 10 / 11 | $4.30\times10^{-16}$ |
| betapr | 212 | 3 / 3 | 0 |
| binom | 87 | 0 / 0 | 0 |
| bnbinom | 135 | 0 / 0 | 0 |
| fpareto | 572 | 42 / 46 | $2.65\times10^{-12}$ |
| gb2 | 572 | 37 / 39 | $4.33\times10^{-14}$ |
| genbeta | 220 | 0 / 0 | 0 |
| geom | 51 | 0 / 0 | 0 |
| lgser | 51 | 0 / 0 | 0 |
| nbinom | 87 | 30 / 30 | $1.75\times10^{-10}$ |
| nsbeta | 128 | 7 / 7 | $3.89\times10^{-16}$ |
| pois | 51 | 0 / 0 | 0 |
| nbinom with zero inflation | 151 | 1 / 1 | 0 |

Every baseline estimate remained an estimate. The zero-inflated
nbinom route changed only its iteration count on `resample/n3-s2`.
The outcome kind changes are as follows.

- Feller–Pareto extreme-scale numerical stops became estimates under
  relative logs for 001674, 001675, 010736, 010737, 010745 and classes
  `huge` and `tiny`.
- Feller–Pareto held-power numerical stops became estimates under the
  scalar curvature enclosure for 001698, 001699, 001700, 001701,
  001702, 001703, 001707, 010079, 010319, 010320, and 010321.
- Feller–Pareto conditioning stops became estimates under the centered
  beta-shape solve for 001744, 001745, 001747, 001748, 001752, 010329,
  and 010330.
- GB2 extreme-scale numerical stops became estimates under relative
  logs for 002161, 002162, 002163, and 002199.
- GB2 held-power numerical stops became estimates under the scalar
  curvature enclosure for 002190, 002191, 002192, 002193, 002194,
  002195, 002203, 009983, 010403, 010404, and 010405.
- GB2 conditioning stops became estimates under the centered beta-shape
  solve for 002248, 002249, 002251, 002252, 002260, 010413, and 010414.
- Native Feller–Pareto 001625 and 001628 changed from `numerical` to
  `unresolved` with their conditional witnesses. Double-only already
  returned `unresolved` and now carries the same witnesses.

The final `fit-judge.py` run had zero failures and listed
`mode|both|fpareto|archive|001625` and
`mode|both|fpareto|archive|001628` as passing. [Judge output](2026-11-21-gb2-acc-data/fit-judge.txt)

## Tests and runs

Appended archive subcases compare each free coefficient to its
independent reference at `mltest::kFitReferenceTol` with `.scale(0)`.

- Feller–Pareto archive 001674, 001675, 001699, 001700, 001702, 001703,
  001752, 010320, 010321, 010736, and 010745.
  [fpareto/test.cpp:347](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/test.cpp#L347)
- GB2 archive 002191, 002192, 002194, 002195, 010404, and 010405.
  [gb2/test.cpp:694](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/test.cpp#L694)
- Feller–Pareto shared input for 001625 and 001628 checks the unresolved
  outcome and its finite conditional witness.
  [fpareto/test.cpp:471](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/test.cpp#L471)
- Negative-binomial size-3330 sample checks size and probability at
  `tol = 1e-6` and `1e-13`.
  [nbinom/test.cpp:1115](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/test.cpp#L1115)

All runs used the workstation. The completed quality check, focused
two-mode checks, and four-family replay survived the 13:10 interruption.
The official suite and dependant replays were restarted. The final
`just test-changed --no-long-double` passed 13 of 13 C++ targets in
each GCC 15.2 mode. R reported 3,552 passed, 20 skipped, and zero
failures or warnings. JS built and passed 284 of 284 selected tests.
The retry used serial testthat, `MAKEFLAGS=-j2` and `EMCC_CORES=2`.
`just quality fpareto gb2 nbinom beta` passed all four. The final
`just site-check` passed after the dossier updates. [Test results](2026-11-21-gb2-acc-data/test-results.txt)

## Open

For 001625 and 001628, the 100-digit free-scale profile increases at
all sampled mean logits from $-12$ to $12$ toward the inverse
transformed-gamma face. The open item is a uniform profile sign or
global likelihood enclosure, including the tails, that excludes
unsampled finite maxima. An accuracy-floor classification would
instead require a viable joint stationary point and a score
noise-over-curvature bound there. The shared `unresolved` outcome
preserves the conditional witness while that comparison remains open.
