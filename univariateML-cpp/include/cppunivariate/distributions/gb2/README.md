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
  - name: unbounded likelihood
    firmness: proved
    when: A free concentration can collapse on constant observations
    detect: All observations are equal and the held coefficients still permit concentration
    status: InvalidData
    message: unbounded
  - {firmness: proved, model: trgamma, when: 'shape1 tends to infinity with shape2 and shape3 fixed and scale divided by shape1^(1/shape2) fixed', status: TrGammaBoundary}
  - {firmness: proved, model: invtrgamma, when: 'shape3 tends to infinity with shape1 and shape2 fixed and scale times shape3^(1/shape2) fixed', status: InvTrGammaBoundary}
  - {firmness: proved, model: lnorm, when: 'shape1 = shape3 = K tends to infinity, shape2 = sqrt(2*trigamma(K))/sigma, and scale = exp(mu), with sigma > 0 and mu fixed', status: LognormalBoundary}
  - {firmness: proved, model: pareto, when: 'shape2 tends to infinity with shape1*shape2 positive and fixed, and shape3 and scale fixed', status: ParetoBoundary}
  - {firmness: proved, model: power, when: 'shape2 tends to infinity with shape3*shape2 positive and fixed, and shape1 and scale fixed', status: PowerBoundary}
  - {firmness: proved, name: double Pareto, when: 'shape2 tends to infinity with shape1*shape2 and shape3*shape2 positive and fixed, and scale fixed', status: DoubleParetoBoundary}
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
  note: 'gb2/README.md, Details; gb2/mle.hpp (public fitter and held overloads); gb2/test.cpp: With beta shapes
    held, the inner power/location likelihood is strictly concave; free beta-shape multibasin profiles and face
    comparisons remain numerical.'
areas:
- actuarial
- economics and income
---

The transformed beta distribution models positive observations with two power tails.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-28-gb2.md) describes the limiting paths and the scope of the boundary comparisons.

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

In the GB2 convention $(a,b,p,q)$, the coefficients are
`shape2` = $a$, `scale` = $b$, `shape3` = $p$, and `shape1` = $q$.
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

## Decision history

On 2026-10-29 commit `470175d` introduced the profiled search and explicit
face comparisons. The implementation report
`dev/research/evaluations/2026-10-29-gb2-implementation-report.md` records the
derivation, independent numerical comparisons, and the failed guarantee
of global certification. On 2026-10-30 commit `fbe1d44` reduced repeated
work in profile evaluation; the report `dev/research/evaluations/2026-10-29-perf-report.md`
records the performance investigation. The search costs repeated passes
over the data per profile evaluation; the implementation report includes
measured workloads and the slower BFGS-only comparison.
