---
id: gb2
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Transformed beta (GB2)
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'q'
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: shape2
  symbol: 'a'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape3
  symbol: 'p'
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: scale
  symbol: 'b'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dgb2
  sampler: rgb2
  control:
    gradtol: Tolerance on the per-observation profile score in log beta shapes.
    iterlim: Maximum iterations per outer search start.
log_density: log(shape2)-log(x)-loggamma(shape3)-loggamma(shape1)+loggamma(shape3+shape1)+shape3*shape2*log(x/scale)-(shape3+shape1)*log(1+(x/scale)**shape2)
limits:
  - {firmness: proved, model: weibull, when: 'With shape3 held at one, the Burr submodel tends to its Weibull face.', status: WeibullBoundary, fixed: [shape3], numerical: true}
  - {firmness: proved, model: invweibull, when: 'With shape1 held at one, the inverse Burr submodel tends to its inverse-Weibull face.', status: InvWeibullBoundary, fixed: [shape1], numerical: true}
  - name: unbounded likelihood
    firmness: proved
    when: The log-scale dispersion tends to zero at the common observation while the remaining free coefficients allow concentration.
    detect: All observations are equal, and the held coefficients still permit concentration at that value.
    status: InvalidData
    message: unbounded
  - {firmness: proved, model: trgamma, when: 'The beta shape $q$ tends to infinity with $a,p$ and $b/q^{1/a}$ fixed.', status: TrGammaBoundary, numerical: true}
  - {firmness: proved, model: invtrgamma, when: 'The beta shape $p$ tends to infinity with $a,q$ and $bp^{1/a}$ fixed.', status: InvTrGammaBoundary, numerical: true}
  - {firmness: proved, model: lnorm, when: 'The beta shapes satisfy $p=q=K\to\infty$, the power is $a=\sqrt{2\psi_1(K)}/\sigma$, and $b=e^\mu$, for fixed $\sigma>0$ and $\mu$. Here $\psi_1$ is the trigamma function.', status: LognormalBoundary, numerical: true}
  - {firmness: proved, model: pareto, when: 'The power $a$ tends to infinity with $qa>0$, $p$ and $b$ fixed.', status: ParetoBoundary, numerical: true}
  - {firmness: proved, model: power, when: 'The power $a$ tends to infinity with $pa>0$, $q$ and $b$ fixed.', status: PowerBoundary, numerical: true}
  - {firmness: proved, name: double Pareto, when: 'The power $a$ tends to infinity with $qa>0$, $pa>0$ and $b$ fixed.', status: DoubleParetoBoundary, numerical: true}
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlgb2())
  mlgb2(x)

summary: The generalized beta distribution of the second kind provides a flexible four-parameter model for positive data.
classification: specialist
classification_reason: Used to model income and loss sizes with independently adjustable tails and scale.
aliases:
- "generalized beta of the second kind"
- "GB2"
- "transformed beta"
- "generalized F"
key_references:
- 1
references:
  - "McDonald, J. B. (1984). Some Generalized Functions for the Size Distribution of Income. Econometrica, 52(3). doi:10.2307/1913469."

group: beta and generalised beta
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: fpareto
    at:
      shape1: shape1
      shape2: shape2
      shape3: shape3
      scale: scale
      min: 0
  transform_of:
  - family: betapr
    at:
      shape1: shape3
      shape2: shape1
      scale: scale**shape2
    transform: power
    power: shape2
fit:
  method: multistart
  certified: partial
  note: 'gb2/README.md, Details; gb2/mle.hpp (public fitter and held overloads); gb2/test.cpp: Held shape3=1 reduces exactly to Burr and held shape1=1 to inverse Burr, including certified joint and held-power routes and stop details; their held-scale two-shape profiles remain numerical. Holding both beta shapes gives the concave power/location likelihood; holding power and scale gives a concave beta-shape likelihood. Outside the unit-shape reductions, profiles with shape1 and/or shape3 free while shape2 or scale is free retain numerical multibasin searches and face comparisons.'
areas:
- actuarial
- economics and income
existence:
  uniqueness: unknown
  statement: >-
    With all four coefficients free, constant positive samples have unbounded
    likelihood. For nonconstant samples, a supremum may occur on a limiting
    model rather than at finite coefficients. No general existence or
    uniqueness criterion is established. Holding coefficients can remove
    particular escapes. For nonconstant data, holding both beta shapes gives
    a unique finite maximum in power and scale. Holding power and scale
    instead gives a unique finite maximum in the beta shapes. With power,
    scale and either beta shape held, the remaining shape has a unique
    finite maximum even for constant data.
  reason: >-
    On the log-observation scale, put $c=-a\log b$. At fixed $p,q$ the
    objective is $n\log a+p\sum_i(a\log x_i+c)
    -(p+q)\sum_i\log(1+\exp(a\log x_i+c))$ plus a constant.
    Its Hessian is negative definite. For nonconstant data it tends to
    negative infinity as power tends to zero or infinity after profiling
    location, so the maximum is attained. With power and scale held, the
    logistic transform of log observations reduces the shape problem to
    beta fitting. These arguments do not establish uniqueness of the outer
    beta-shape profile. The limiting paths below are proved, but their
    numerical likelihood comparisons do not exhaust all possible maxima.
---

The transformed beta distribution models positive observations with two power tails.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-28-gb2.md) describes the limiting paths and the scope of the boundary comparisons.

In the GB2 convention $(a,b,p,q)$, $a$ is the power, $b$ is the scale, and $p,q$ control the lower and upper power tails. Their exponents are $ap$ near zero and $aq$ at infinity.
Each coefficient may be held at a positive finite value. The numerical search
compares proper limiting models and stops when a face wins; a search that
cannot resolve an interior maximum reports an error.

The fit standardizes log observations and profiles the power and location
for each pair of beta shapes. The inner objective is strictly concave; the
outer search uses a grid and member-family starts, refines distinct basins
with Newton or BFGS, and expands the search box when needed. This reduces
the outer problem to at most two dimensions and improves conditioning.
A small profile score and an interior position are required for a returned
interior fit. The multibasin search is numerical: neither global optimality
nor exhaustion of all possible limit faces is proved. An unresolved search
reports failure. Available transformed-gamma, inverse transformed-gamma,
lognormal, Pareto, power and double-Pareto faces are compared, as applicable
to the held subset.

## R notes

A detected limiting model stops with advice to fit that model. A small score
checks stationarity of a returned fit; it does not certify global optimality.

## Decision history

For samples larger than 1000, the 2026-10-31 fit-speed revision forms the Burr,
inverse-Burr and beta-prime member seeds from 1000 deterministically selected
observations, one distinct index from each of 1000 contiguous index blocks.
This changes only candidate starts;
samples of size at most 1000 use the original member fits. Transformed-gamma and
reciprocal transformed-gamma boundary fits, all profile likelihoods, basin
refinement, boundary comparisons and score/failure gates still use the complete
sample. This is a numerical search change, not an exact-equivalence theorem for
arbitrary samples. A repeated-golden large-sample comparison and a regression
exercise the new branch; the prescribed small-sample comparison does not
exercise subsampling. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

2026-10-30 (TASK-125): the shared beta log-coordinate bridge uses the continued fraction when the backend input is subnormal or its result is below 1e-290, avoiding relative precision loss before taking logs. The existing independent truth rows measure the selected cases; this is not a uniform error bound. Logit quantile bisection returns NaN if it cannot bracket or reach adjacent floating-point endpoints.

On 2026-10-29 commit `470175d` introduced the profiled search and explicit
face comparisons. The implementation report
`dev/research/evaluations/2026-10-29-gb2-implementation-report.md` records the
derivation, independent numerical comparisons, and the failed guarantee
of global certification. On 2026-10-30 commit `fbe1d44` reduced repeated
work in profile evaluation; the report `dev/research/evaluations/2026-10-29-perf-report.md`
records the performance investigation. The search costs repeated passes
over the data per profile evaluation; the implementation report includes
measured workloads and the slower BFGS-only comparison.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).
