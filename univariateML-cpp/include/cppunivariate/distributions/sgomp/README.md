---
id: sgomp
inference_numeric:
  - regime: "exponential member with shape known"
    available: true
    condition: "finite b > 0 free; eta = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite b > 0 and eta > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Shifted Gompertz
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: b
  symbol: 'b'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: eta
  symbol: '\eta'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dsgomp
  sampler: rsgomp
  control:
    b: Optional fixed positive rate; `NULL` estimates it.
    eta: Optional fixed nonnegative shape; `NULL` estimates it.
    reltol: Tolerance for the scalar profile searches.
    iterlim: Maximum iterations per scalar search.
log_density: log(b) - b*x - eta*exp(-b*x) + log(1 + eta*(1-exp(-b*x)))
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlsgomp())
  mlsgomp(x)

group: other positive
construction:
- base
relations: {}
fit:
  method: scalar-solve
  certified: partial
  note: 'sgomp/README.md, Details; sgomp/mle.hpp (public fitter and held overloads); sgomp/test.cpp: Held rate leaves
    a strictly monotone shape score; joint rate grid refines sampled peaks without certifying enumeration of all
    stationary points.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model waiting times with flexible onset in diffusion and adoption models.
summary: Shifted Gompertz provides a model for waiting times with flexible onset in diffusion and adoption models.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a finite MLE exists for every nonconstant
    nonnegative sample. Constant samples have unbounded likelihood.
    Holding $b$ gives a unique finite estimate of $\eta$, possibly zero.
    Holding $\eta$ gives a unique finite estimate of $b$ exactly when the sample sum
    is positive. An all-zero sample has unbounded likelihood when $b$ is free.
    Joint uniqueness is not established. With $\eta=0$ held and positive
    sample sum, the unique rate estimate is the exponential estimate. Holding both leaves only likelihood evaluation.
  reason: >-
    For held $b$, put $v_i=e^{-bx_i}$. The shape score is
    $\sum_i[-v_i+(1-v_i)/(1+\eta(1-v_i))]$.
    It decreases strictly if any observation is positive and tends to
    $-\sum_i v_i<0$. A nonpositive score at zero selects $\eta=0$.
    For all-zero data it is the negative constant $-n$, again selecting zero.
    With $\eta$ held, the second rate derivative is
    $-n/b^2-\eta\sum_i x_i^2v_i-\eta(1+\eta)\sum_i x_i^2v_i/[1+\eta(1-v_i)]^2<0$.
    For positive sample sum the log likelihood tends to minus infinity at
    both ends of the rate domain, proving a unique rate maximum.
    With both free and nonconstant data, likelihood envelopes bound the rate
    away from zero and infinity on every positive likelihood level set.
    Bounded rate then bounds the maximizing shape. These bounds prove
    attainment but do not prove uniqueness or completeness of the numerical
    search.
---

The shifted Gompertz distribution models nonnegative waiting times, including diffusion and adoption times. The coefficient $b$ sets the time scale through a rate, and $\eta$ changes the shape; there is no location coefficient.

## Details

At a fixed rate, increasing $\eta$ shifts probability toward longer waiting times.

The domain includes $\eta=0$, an attained exponential member with rate $b$. Zeros are valid observations. A maximum can occur at this member rather than at a positive shape. Holding the rate gives a unique shape estimate from a decreasing score, or the endpoint zero. Holding the shape leaves a strictly concave scalar rate likelihood, with an explicit exponential estimate when the held shape is zero.

For a nonconstant sample, the joint likelihood attains a maximum. This existence result follows from likelihood envelopes, independently of whether the numerical search finds it. In particular, writing $m=\min_i x_i$, an upper envelope is
$\ell\le n\log b+n\log n-b(\sum_i x_i-nm)$.
The coefficient of $-b$ is positive for nonconstant data, so this excludes arbitrarily large rates. For $b\max_i x_i\le1$, maximizing $n\log(1+\eta)-n\eta/e$ bounds the remaining shape contribution by $n$, giving $\ell\le n\log b+n$. This excludes rates near zero. At bounded rates the likelihood decreases without bound as the shape grows. Constant positive samples instead allow $b$ to increase and $\eta=\exp(bx_1)$ to concentrate the density at their common value. All-zero data have unbounded likelihood whenever the rate is free.

The fitter profiles shape and refines the rate maxima bracketed by a grid on the finite interval supplied by the envelopes. This search does not certify that every stationary point has been found. Joint uniqueness remains open. The strict concavity of either conditional likelihood does not imply joint concavity of the profiled likelihood. Failure to resolve a scalar search stops fitting.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) records the held-parameter and boundary arguments.

## R notes

The fitter prefers the attained `eta = 0` member when its log likelihood is within $10^{-8}n$ of the best candidate. Constant samples with both coefficients free, and all-zero samples with `b` free, stop with an unbounded-likelihood error.
