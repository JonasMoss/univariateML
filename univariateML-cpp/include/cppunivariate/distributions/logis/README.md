---
id: logis
inference_numeric:
  - regime: interior
    available: true
    condition: "finite location and finite scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Logistic
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: location
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: scale
  symbol: 's'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: core_dlogis
  sampler: stats::rlogis
  control:
    scale: Optional held scale; NULL estimates it.
    location: Optional held location; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)).
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: -(x-location)/scale - log(scale) - 2*log(1 + exp(-(x-location)/scale))
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 23.
  Wiley, New York.
holds: []
limits:
  - name: scale collapse
    firmness: proved
    when: '$s$ tends to zero at the fitted or held location $\mu$'
    detect: '$s$ is free and every observation equals the fitted or held location $\mu$'
    status: Unbounded
    message: "unbounded"
examples: |
  set.seed(1)
  x <- rml(200, mllogis())
  mllogis(x)

group: skewed and robust location-scale
construction:
- location-scale
relations:
  special_case_of:
  - family: tlambda
    at:
      mu: location
      sigma: scale
      lambda: 0
fit:
  method: convex-solve
  certified: 'yes'
  note: 'logis/README.md, Details; logis/mle.hpp (public fitter and held overloads); logis/test.cpp: Joint likelihood
    is strictly concave in location/scale and inverse scale; Newton seeks its unique stationary maximum. Both conditional
    scores are monotone and bisected.'
areas:
- robust modelling
classification: classical
classification_reason: A standard model for symmetric errors with heavier tails than Gaussian errors.
summary: Logistic provides a model for symmetric errors with heavier tails than Gaussian errors.
aliases:
- "sech-squared"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists for every
    nonconstant real sample. Constant data have unbounded likelihood as
    scale shrinks at their common value. Holding location gives a unique
    finite scale estimate unless every observation equals that location.
    Holding scale gives a unique finite location estimate, including for
    constant data. Holding both leaves only likelihood evaluation.
  reason: >-
    In coordinates $a=\mu/s$ and $b=1/s>0$, the log-likelihood is
    $n\log b-2\sum_i\log\{2\cosh((bx_i-a)/2)\}$.
    Its Hessian is negative definite: the first term is strictly concave
    in $b$, and each summand has strictly negative curvature in $bx_i-a$.
    For nonconstant data, $\sum_i|bx_i-a|$ grows at least linearly in $b$
    after minimising over $a$, so the likelihood tends to $-\infty$ as
    $b\to\infty$. It also tends to $-\infty$ as $b\to0$ or as $|a|\to\infty$
    with $b$ bounded away from zero and infinity. Thus a unique maximum
    is attained. Held scale fixes $b$; held location imposes $a=\mu b$,
    and the same argument applies unless all residuals vanish.
---

The logistic distribution models symmetric errors with heavier tails than the normal distribution. The location $\mu$ is its centre and median, and the positive scale $s$ controls its spread.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives these boundary conditions, including the relevant held parameters.

For nonconstant data the joint fit has a unique maximum. The likelihood is strictly concave in location divided by scale and inverse scale, although it need not be concave in location and scale themselves. The fit uses Newton iteration with a line search. Its absolute step target applies to location and log scale; when that target lies within $2^{12}$ binary64 spacings of $\max(|\bar x|,\mathrm{sd})$, the iteration runs on $(x-\bar x)/\mathrm{sd}$, where the target bounds the location step relative to sd.

Holding location leaves a unique scale maximum unless every observation equals that location, when scale can collapse to zero. Holding scale gives a unique location maximum even for constant samples. Both conditional scores are solved by bisection. Holding both evaluates the likelihood. A location bracket that narrows to adjacent doubles before its width target is accepted only when the computed scores at both ends exceed their rounding bound. When the data spread is many orders of magnitude beyond the held scale, the computed score is exactly zero between the middle observations; no bracket certifies and the fit reports `IterationFailure`, although the maximum exists near their midpoint.
