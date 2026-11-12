---
id: zmpois
inference_numeric:
  - regime: "known zero Poisson rate"
    available: true
    condition: "lambda = 0 held; 0 < p0 < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "positive-count member with zero mass known"
    available: true
    condition: "finite lambda > 0 free, p0 = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: false
    condition: "finite lambda > 0 free, p0 = 1 held; information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "primary interior"
    available: true
    condition: "finite lambda > 0 and 0 < p0 < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-modified Poisson
support:
  type: integer
  lower: 0
  upper: inf
  closed:
  - true
  - false
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmpois
  sampler: rzmpois
  control:
    lambda: "Optional held lambda; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(-lambda+x*log(lambda)-loggamma(x+1)-log(1-exp(-lambda))), True))
examples: |
  set.seed(1)
  x <- rml(200, mlzmpois())
  mlzmpois(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: pois
    at:
      lambda: lambda
    type: zero-modified
    mass: p0
fit:
  method: scalar-solve
  certified: 'yes'
  note: "zmpois/mle.hpp and zmpois/profile.hpp: The zero fraction maximizes the Bernoulli factor; positive counts delegate to the monotone zero-truncated Poisson rate solve. All-one positive data attain rate zero; all-zero data leave rate unidentified."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model Poisson positive counts with a separately fitted zero probability.
summary: Zero-modified Poisson provides a model for Poisson positive counts with a separately fitted zero probability.
aliases:
- "hurdle Poisson"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, every nonempty count sample has an attained
    maximum. It is unique if a positive count occurs, including $\lambda=0$ when
    all positive counts are one. All-zero data give $p_0=1$ and leave the rate
    unidentified. Holding either coefficient preserves attainment for compatible
    data. A free zero probability is uniquely the empirical zero fraction. A free
    rate is unique when positives occur and is unidentified otherwise. Holding all
    coefficients leaves likelihood evaluation. Held $p_0=0$ excludes zeros, held
    $p_0=1$ excludes positives, and held $\lambda=0$ permits positive counts only
    at one.
  reason: >-
    The likelihood factors into a Bernoulli likelihood for the zero indicator and
    a zero-truncated Poisson likelihood for the positives. The first has its
    unique maximum at the empirical zero fraction. In the second, the conditional
    mean $\lambda/(1-e^{-\lambda})$ increases strictly from one to infinity: its
    derivative with respect to log rate is the positive conditional variance.
    Therefore the positive sample mean determines a unique rate, with the
    continuous endpoint $\lambda=0$ for mean one. Without positives the rate drops
    out of the likelihood.
---

The zero-modified Poisson distribution assigns probability $p_0$ directly to zero. Conditional on being positive, a count follows a zero-truncated Poisson law with rate $\lambda$. This allows both more and fewer zeros than the corresponding ordinary Poisson distribution.

## d/p/q/r

### Large-rate probabilities

At rate $10^8$ and zero probability one half, the mode log mass is
-10.822426086574135. Cancellation in an uncentered Poisson log mass is
visible at this scale. The same member has lower quantile 99916056 at the
binary64 log-half input; its positive-component threshold must be retained
before rounding the mixed CDF.

## Details

The likelihood separates the zero indicators from the positive counts. A free zero probability is the empirical zero fraction, including zero when no zeros occur. The rate is fitted only to the positive observations. Holding either coefficient leaves the fit of the other unchanged.

When all positive observations are one, the rate estimate is zero. This is an attained two-point law on zero and one. With no positives, the rate is unidentified, even if the zero probability is held below one. Held values must give positive probability to every observation. In particular, a held zero rate cannot explain counts above one.

## R notes

Supply lambda or p0 to hold it; `NULL` estimates it. All-zero samples warn that the rate is unidentified. A supplied rate is preserved and a free rate uses the representative `lambda = 1`. With free zero probability they return `p0 = 1`. A free fit with positives all equal to one warns that `mlbinom(x, size = 1)` fits the attained two-point law exactly.
