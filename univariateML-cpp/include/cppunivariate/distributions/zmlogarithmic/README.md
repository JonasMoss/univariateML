---
id: zmlogarithmic
inference_numeric:
  - regime: "known zero positive-count parameter"
    available: true
    condition: "prob = 0 held; 0 < p0 < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "positive-count member with zero mass known"
    available: true
    condition: "0 < prob < 1 free, p0 = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: false
    condition: "0 < prob < 1 free, p0 = 1 held; information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "primary interior"
    available: true
    condition: "0 < prob < 1 and 0 < p0 < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-modified logarithmic
support:
  type: integer
  lower: 0
  upper: inf
  closed:
  - true
  - false
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1)'
  simulate: 0.5
  kind: regular
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmlogarithmic
  sampler: rzmlogarithmic
  control:
    prob: "Optional held prob; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+x*log(prob)-log(x)-log(-log(1-prob)), True))
examples: |
  set.seed(1)
  x <- rml(200, mlzmlogarithmic())
  mlzmlogarithmic(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: lgser
    at:
      theta: prob
    type: zero-modified
    mass: p0
  special_case_of:
  - family: zmnbinom
    at:
      size: 0
      prob: 1-prob
      p0: p0
fit:
  method: scalar-solve
  certified: 'yes'
  note: "zmlogarithmic/mle.hpp and ztpois/profile.hpp: The zero-indicator likelihood separates analytically; the strictly increasing logarithmic-series mean gives a unique scalar probability root, computed by the bracketed lgser solver with a mean-residual gate and a 100-bisection fallback. All-one positive data attain probability zero; all-zero data leave it unidentified."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model logarithmic-series positive counts with a separately fitted zero probability.
summary: Zero-modified logarithmic provides a model for logarithmic-series positive counts with a separately fitted
  zero probability.
aliases:
- "hurdle logarithmic"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, every nonempty sample of nonnegative integers has an MLE. The zero probability is the observed zero fraction. If there is a positive observation, the positive-count probability is unique, including $p=0$ when every positive count is one. With all-zero data, $p_0=1$ and $p$ is unidentified. Holding either coefficient leaves the same maximization for the other, provided the held values give positive likelihood. Holding both leaves only likelihood evaluation.
  reason: >-
    The likelihood factors into the Bernoulli likelihood of the zero indicators and a logarithmic-series likelihood for the positive counts. The latter is a one-parameter exponential family with natural parameter $\log p$. Its mean increases strictly from one to infinity because its derivative in that parameter is its positive variance. Strict concavity therefore gives a unique root for a positive-count mean above one. At mean one the maximum is the attained member $p=0$. With no positive counts this factor is absent.
---

The zero-modified logarithmic distribution combines a freely chosen zero probability $p_0$ with logarithmic-series positive counts. Increasing $p$ increases the mean of the positive counts without changing the zero probability.

## Details

The zero indicators and positive counts contribute separate likelihood factors. The fitted zero probability is their empirical fraction. Holding it leaves the positive-count fit unchanged. Holding the positive-count probability leaves only estimation of the zero fraction.

If every positive observation is one, the maximum is an attained distribution on zero and one, with $p=0$. If there are no positive observations, the positive-count probability is unidentified. If there are no zeros, the free zero probability is zero. Held $p_0=0$ is incompatible with observed zeros, and held $p_0=1$ is incompatible with positives. Held $p=0$ permits only ones among the positive counts.

## R notes

`prob = NULL` and `p0 = NULL` estimate their respective coefficients. Supplied values are preserved. All-zero data return `p0 = 1` when it is free, and use a conventional representative for a free `prob`, with a warning about nonidentifiability. With both coefficients free, positive counts all equal to one produce a warning that `mlbinom(x, size = 1)` fits the attained two-point law exactly. The boundary convention at `prob = 0` agrees with actuar. Incompatible held values stop.
