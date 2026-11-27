---
id: nsbeta
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0 and shape2 > 0; finite min < max held; either shape free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Nonstandard beta
support: {type: real, lower: "min", upper: "max", closed: [true, true], endpoint_log_density: {lower: "if (shape1 < 1) Inf else if (shape1 > 1) -Inf else log(shape2) - log(max-min)", upper: "if (shape2 < 1) Inf else if (shape2 > 1) -Inf else log(shape1) - log(max-min)"}}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: min
  symbol: 'a'
  role: lower endpoint
  domain: "(-inf, max)"
  simulate: 0
  advice: 'The fully free likelihood is unbounded. Omitting an endpoint is not supported. Supply a finite value to hold it or NULL to estimate it.'
  kind: threshold
- name: max
  symbol: 'b'
  role: upper endpoint
  domain: "(min, inf)"
  simulate: 1
  advice: 'The fully free likelihood is unbounded. Omitting an endpoint is not supported. Supply a finite value to hold it or NULL to estimate it.'
  kind: threshold
r:
  density: dnsbeta
  sampler: rnsbeta
  control:
    min: Known finite lower endpoint.
    max: Known finite upper endpoint larger than min.
    tol: Requested relative shape accuracy.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
    shape1: Known positive finite coefficient; NULL estimates it.
    shape2: Known positive finite coefficient; NULL estimates it.
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The shapes satisfy $\alpha=tu$ and $\beta=t(1-u)$ with $t\to\infty$, where $u=(x-a)/(b-a)$ is the common transformed observation and the endpoints are held.
    detect: All observations equal one point strictly between the held endpoints, and both shapes are free.
    status: Degenerate
    message: unbounded
    detect_firmness: proved
    proof: ["UnivariateML.Beta.constant_unbounded", "UnivariateML.NonstandardBeta.logLik_transfer"]
    detect_proof: ["UnivariateML.NonstandardBeta.joint_unbounded", "univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md#details"]
    fixed: [min, max]
  - kind: nonexistence
    name: unbounded free-endpoint likelihood
    status: Degenerate
    message: unbounded
    when: 'A singular free or held endpoint or shrinkage about a constant sample increases the likelihood without bound.'
    detect: 'A free endpoint has a free shape or a shape below one, a compatible held endpoint has a singular factor, or constant data admit concentration.'
    firmness: proved
    detect_firmness: proved
    proof: ["dev/research/evaluations/2026-11-25-thresh-math-report.md#beta-with-one-endpoint-held", "dev/research/evaluations/2026-11-25-thresh-math-report.md#beta-with-both-endpoints-free"]
    detect_proof: ["dev/research/evaluations/2026-11-25-thresh-math-report.md#beta-with-one-endpoint-held", "dev/research/evaluations/2026-11-25-thresh-math-report.md#beta-with-both-endpoints-free"]
log_density: (shape1-1)*log(((x-min)/(max-min))) + (shape2-1)*log(1-((x-min)/(max-min))) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)-log(max-min)
holds: []
fit_args:
  min: 0
  max: 1
examples: |
  set.seed(1)
  x <- rml(200, mlnsbeta())
  mlnsbeta(x, min = 0L, max = 1L)

group: beta and generalised beta
construction:
- base
relations:
  transform_of:
  - family: genbeta
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: max-min
    transform: shift
    shift: min
  - family: beta
    at:
      shape1: shape1
      shape2: shape2
    transform: affine
    shift: min
    scale: max-min
fit:
  method: profile-search
  certified: 'yes'
  note: 'nsbeta/README.md, Details; nsbeta/mle.hpp (public fitter and held overloads); nsbeta/test.cpp: Strictly
    concave beta likelihood in its two shapes; held-shape digamma scores decrease strictly. nsbeta uses supplied
    endpoints. Free bounds use signed profiles on the feasible interval.'
areas:
- proportions
classification: specialist
classification_reason: Used to model beta-shaped measurements on a supplied physical interval.
summary: Nonstandard beta provides a model for beta-shaped measurements on a supplied physical interval.
aliases:
- "non-standard beta"
- "four-parameter beta"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all four coefficients free, the likelihood is unbounded.
    With both endpoints held and observations strictly between them, the
    MLE of the two shapes exists and is unique exactly when the data are
    nonconstant. Holding either shape as well gives a unique finite MLE
    for the other shape, including for constant samples. With free endpoints, the corresponding shapes must be held at least one for a finite maximum; the fitted interval is unique on the attained branches.
  reason: >-
    Moving a free endpoint toward its sample extreme gives an unbounded
    likelihood if its corresponding shape is below one. With endpoints
    held, $y=(x-a)/(b-a)$ has a beta likelihood. Its log-normalizer has
    positive definite Hessian, the covariance of $(\log y,\log(1-y))$.
    Strict concavity and an interior mean sufficient statistic give a
    unique maximum for nonconstant samples. For a single free shape, the
    digamma score strictly decreases from positive infinity to a negative
    limit. For constant data with both shapes free, concentration at that
    interior point makes the likelihood diverge.
---

The nonstandard beta distribution models measurements on an interval with
supplied lower and upper endpoints. Its two shapes control behaviour near
the lower and upper endpoints, respectively.

## d/p/q/r

### Translated quantiles and shape extremes

A beta quantile rounded before affine transport can erase a nonzero result when the endpoints straddle zero. Central symmetric inversion retains the offset from one half through the compensated midpoint calculation. The probability normalizer also avoids subtraction of large log-gamma values when one shape diverges.

The concentrating symmetric tail uses t=log((x-min)/(max-x)), with a=shape1=shape2. Rounding its two beta coordinates can erase a resolved displacement. This probability bridge returns NaN for sqrt(a)*E > 2.5e-11 and 0 < |t| < 2E/1e-10, where E is eight binary64 machine epsilons. At t=0 the exact symmetric half-probability remains available; the stated margin is practical floating-point evidence.

The shape2 one/two beta-coordinate reductions retain the cumulative hazard before rescaling the interval. Their closed antiderivatives avoid backend normalizer loss in double-width builds; at shape2 two a positive-deviance sum retains the complement at the upper endpoint.

## Details

For a constant sample at an interior point $x$, hold the endpoints
fixed and put $u=(x-a)/(b-a)\in(0,1)$. Taking beta shapes
$tu,t(1-u)$, Stirling's formula gives density at $x$ asymptotic to
$\sqrt{t/(2\pi u(1-u))}/(b-a)$. Its $n$th power diverges.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md) describes the limiting paths and the scope of the boundary comparisons.

For at least two finite, nonconstant observations strictly between the held bounds, the MLE is unique and attained: the transformed sufficient-statistic mean is interior, giving coercivity, and the beta log likelihood is strictly concave in its two shapes. If both shapes are free, the constant-sample test uses the observations on their original scale.

With free endpoints, a shape of one gives exact contact at the corresponding sample extreme. Shapes above one require strict separation. With both shapes held, affine coordinates give a strictly concave objective on the feasible interval.

Hold the opposite endpoint beyond the sample. If the shape $s$ associated with the free endpoint satisfies $s<1$, moving that endpoint to within $\varepsilon$ of the corresponding sample extreme gives $k$ factors proportional to $\varepsilon^{s-1}$, where $k$ observations equal that extreme. All other factors approach positive constants, so the likelihood diverges.

With both bounds held, either held beta shape leaves a strictly decreasing score with one root for the other shape, even on constant samples strictly inside the bounds.

With either shape free, distances to the two supplied endpoints determine the log coordinates. Forming the smaller distance directly preserves the score when the other coordinate rounds to one. The two-shape fit also retains differences between the original observations when checking the precision score, whose leading data term is quadratic in those differences. See the [fixed-endpoint accuracy analysis](../../../../../dev/research/evaluations/2026-11-25-nsbeta-math-report.md#public-coefficient-and-restoration-checks).

## R notes

Supply each endpoint as a finite held value or pass NULL to estimate it. With min = 0 and max = 1 this is the ordinary beta fit.

## Decision history

### Diagnostic status contract (2026-10-31)

Invalid and empty samples carry non-success statuses, including delegate paths.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp`.
