---
id: llogis
inference_numeric:
  - regime: interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Log-logistic
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: scale
  symbol: '\alpha'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dllogis
  sampler: rllogis
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    abstol: Positive accuracy tolerance for the centered logistic score brackets.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: log(shape) - log(scale) + (shape - 1)*log(x/scale) - 2*log(1 + (x/scale)**shape)
references:
- Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley.
- Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition,
  Wiley.
- 'Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical
  Software, 25(7), 1-37.'
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
limits:
- name: unbounded likelihood
  firmness: proved
  when: 'As shape $\beta\to\infty$ with scale $\alpha=x_0>0$, the density at $x_0$ is $\beta/(4x_0)$ and diverges.'
  detect: 'Both coefficients are free and every observation equals $x_0>0$.'
  status: Unbounded
  message: does not exist
  kind: nonexistence
- name: unbounded likelihood
  firmness: proved
  fixed: [scale]
  when: The free shape $\beta$ tends to infinity while scale $\alpha$ is held at the common observation.
  detect: Shape is free and every observation equals the held scale $\alpha$.
  status: Unbounded
  message: unbounded
examples: |
  set.seed(1)
  x <- rml(200, mlllogis())
  mlllogis(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: burr
    at:
      shape1: 1
      shape2: shape
      scale: scale
  - family: invburr
    at:
      shape1: 1
      shape2: shape
      scale: scale
  - family: pareto3
    at:
      shape: shape
      scale: scale
      min: 0
  transform_of:
  - family: logis
    transform: log
    at:
      location: log(scale)
      scale: 1/shape
fit:
  method: delegation
  certified: 'yes'
  note: 'llogis/README.md, Details; llogis/mle.hpp (public fitter and held overloads); llogis/test.cpp: Log transform
    transfers to the unique logistic maximum; held shape gives monotone log-location and held scale a monotone shape
    score, both bisected. Exact constant-data collapse stops; finite solves use convergence tolerances
    on their monotone scores.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model survival times with a nonmonotone hazard and polynomial upper tail.
summary: Log-logistic provides a model for survival times with a nonmonotone hazard and polynomial upper tail.
aliases:
- "Fisk"
key_references:
- 4
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists for every nonconstant
    positive sample. Constant data have unbounded likelihood. With shape held,
    the scale estimate exists uniquely even for constant data. With scale held,
    a unique finite shape estimate exists unless every observation equals that
    scale. Holding both leaves likelihood evaluation.
  reason: >-
    The transformation $y_i=\log x_i$ gives a logistic likelihood with location
    $\mu=\log\alpha$ and inverse scale $\beta$. In coordinates
    $(a,b)=(\beta\mu,\beta)$ it is, up to a data-only term,
    $n\log b-2\sum_i\log\{2\cosh((by_i-a)/2)\}$.
    Its Hessian is negative definite. For nonconstant data it tends to
    $-\infty$ at the boundary after profiling $a$, so its maximum exists
    uniquely. Holding shape fixes $b$. Holding scale imposes $a=b\log\alpha$;
    the same strict concavity and boundary argument applies unless all
    residuals vanish.
---

The log-logistic distribution describes positive data whose logarithm has a logistic distribution. It is used for size, lifetime and loss data. The scale $\alpha$ is the median, and the shape $\beta$ controls spread on the logarithmic axis and the polynomial upper tail.

## Details

Fitting requires a nonempty sample of strictly positive finite observations.
The fit standardizes centered logarithms $\log1p((x_i-x_0)/x_0)$ and solves the logistic location and profile-power scores by bracketing. Strict concavity in power and power times log scale identifies the unique root.
The Jacobian changes the log-likelihood by a term depending only on the data,
so it preserves its maximizer. Holding shape fixes the spread on the log axis;
holding scale fixes its centre. Both conditional scores are monotone and solved
by bisection. Scale restoration uses expm1 near the anchor; the likelihood is recomputed at that representable scale. Rounding the median can prevent its likelihood certificate from transferring on ulp-wide samples; such finite fits carry an explicit uncertified flag. Holding shape permits constant data. With scale held, the
likelihood is unbounded only when every observation equals that scale.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and their held-parameter conditions.

## Decision history

The diagnostic contract recorded on 2026-10-31 rejects empty or invalid input,
distinguishes degenerate samples by the free coefficients and preserves delegate
failures through transformations. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the TASK-132 cases in `test.cpp` for the validation scope.
