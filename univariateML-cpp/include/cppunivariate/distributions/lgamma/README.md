---
id: lgamma
inference_numeric:
  - regime: interior
    available: true
    condition: "finite shapelog > 0 and ratelog > 0; either or both free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "a parameter is zero or nonfinite"
    reason: "a gamma parameter is on a boundary or outside the finite model"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Log-gamma
support: {type: real, lower: 1, upper: inf, closed: [false, false]}
params:
- name: shapelog
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: ratelog
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dlgamma
  sampler: rlgamma
  control:
    shapelog: Optional held shapelog; NULL estimates it.
    ratelog: Optional held ratelog; NULL estimates it.
    abstol: Absolute step tolerance in shapelog.
    iterlim: Maximum number of iterations.
log_density: shapelog*log(ratelog) - loggamma(shapelog) + (shapelog - 1)*log(log(x)) - (ratelog + 1)*log(x)
limits:
  - name: unbounded likelihood
    firmness: proved
    when: 'As shape and rate grow to infinity with shape/rate fixed at $\log(x_0)$, the density at $x_0>1$ diverges.'
    detect: 'Both coefficients are free and every observation equals $x_0>1$.'
    status: Degenerate
    message: does not exist
    kind: nonexistence
references:
- Hogg, R. V. and Klugman, S. A. (1984), Loss Distributions, Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mllgamma())
  mllgamma(x)

group: gamma and Weibull
construction:
- transformation
relations:
  transform_of:
  - family: gamma
    transform: log
    at:
      shape: shapelog
      scale: 1/ratelog
fit:
  method: delegation
  certified: 'yes'
  note: 'lgamma/README.md, Details; lgamma/mle.hpp (public fitter and held overloads); lgamma/test.cpp: Gamma shape
    has a unique monotone digamma equation; scale is analytic, with bracketed fallback and held-coordinate solves.
    Transformation preserves the maximum. Exact constant-data collapse stops; iterative solves retain
    their convergence checks and floating-point limitations.'
areas:
- actuarial
- reliability and survival
classification: specialist
classification_reason: Used to model insurance sizes above one whose logarithms have gamma variation.
summary: Log-gamma provides a model for insurance sizes above one whose logarithms have gamma variation.
aliases: []
key_references:
- 3
existence:
  uniqueness: proved
  statement: >-
    For observations above one, the MLE with both coefficients free exists
    exactly when the sample is nonconstant and is then unique. Holding either
    $k$ or $\lambda$ gives a unique finite estimate of the other coefficient
    for every such sample, including constant samples.
  reason: >-
    The one-to-one transformation $y=\log x$ has a coefficient-independent
    Jacobian and reduces the problem to gamma estimation with shape $k$
    and rate $\lambda$. Profiling rate gives
    $\log k-\psi(k)=\log\bar y-\overline{\log y}$.
    The left side decreases strictly from infinity to zero, and the right
    side is positive exactly for nonconstant data by strict Jensen inequality.
    Holding shape gives $\hat\lambda=k/\bar y$; holding rate leaves a
    strictly increasing digamma equation with one positive root.
---

The log-gamma distribution models observations above one whose logarithms follow a gamma distribution.

## Details

The coefficients $k$ and $\lambda$ are the gamma shape and rate of the logarithms. This is the actuarial log-gamma convention. Fitting applies gamma estimation to the logarithms and subtracts their sum from the gamma log likelihood to account for the Jacobian.

Holding either coefficient gives a unique finite estimate of the other, even for constant observations above one. With both free, constant data have unbounded likelihood as shape and rate tend to infinity at a fixed ratio. Both-held fits evaluate the likelihood.

For constant $x_0>1$, put $y_0=\log x_0$ and rate $=k/y_0$. Stirling's formula gives density at $x_0$ asymptotic to $\sqrt{k/(2\pi)}/(x_0 y_0)$ as shape $k\to\infty$, proving unboundedness at fixed shape/rate ratio.

## R notes

`shapelog` and `ratelog` refer to the gamma law of `log(x)`. The support requires `x > 1`.

## Decision history

On 2026-10-30 the core fitter took ownership of the previous R adapter's exact transform and sum order. Internally, NaN marks a free coefficient. Held rate is passed to the gamma fitter as reciprocal scale and returned unchanged. Gamma iteration failure is preserved, and constant samples are distinguished from transforms collapsed by floating-point precision. This adds no search or convergence guarantee beyond the gamma solver. The C++ regression compares all four held subsets with the previous adapter computation; golden agreement is numerical evidence.

The 2026-10-31 diagnostic audit checks invalid and empty samples, including delegate paths. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the TASK-132 cases in the family C++ tests.
