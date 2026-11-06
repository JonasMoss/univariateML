---
id: zmgeom
inference_numeric:
  - regime: "known unit geometric probability"
    available: true
    condition: "prob = 1 held; 0 < p0 < 1 free"
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
name: Zero-modified geometric
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
  domain: (0, 1]
  simulate: 0.5
  kind: regular
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmgeom
  sampler: rzmgeom
  control:
    prob: "Optional held prob; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(log(prob)+(x-1)*log(1-prob)), True))
examples: |
  set.seed(1)
  x <- rml(200, mlzmgeom())
  mlzmgeom(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: geom
    at:
      prob: prob
    type: zero-modified
    mass: p0
  special_case_of:
  - family: zmnbinom
    at:
      size: 1
      prob: prob
      p0: p0
fit:
  method: closed-form
  certified: 'yes'
  note: "zmgeom/mle.hpp and zmpois/profile.hpp: The zero fraction and reciprocal positive-sample mean maximize the separated Bernoulli and geometric factors; the all-one positive endpoint is attained and all-zero data leave the base probability unidentified."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model geometric positive counts with an independently adjustable zero probability.
summary: Zero-modified geometric provides a model for geometric positive counts with an independently adjustable
  zero probability.
aliases:
- "hurdle geometric"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, every nonempty count sample has an attained
    maximum. If positives occur it is unique, with $\hat p=1/\bar x_+$ and $\hat
    p_0$ the empirical zero fraction, where $\bar x_+$ is the positive sample
    mean. All-zero data give $p_0=1$ and leave $p$ unidentified. Holding either
    coefficient preserves attainment for compatible data. A free $p_0$ is unique,
    and a free $p$ is unique exactly when positives occur. Holding all
    coefficients leaves likelihood evaluation. Held $p_0=0$ excludes zeros, held
    $p_0=1$ excludes positives, and held $p=1$ permits positive counts only at
    one.
  reason: >-
    The likelihood factors into a Bernoulli zero-indicator likelihood and the
    positive geometric likelihood. For $k>0$ positive observations the latter is
    $p^k(1-p)^{\sum_{x_i>0}(x_i-1)}$, uniquely maximized at $p=k/\sum_{x_i>0}x_i$.
    This includes the attained endpoint $p=1$ when all positives are one. The
    Bernoulli factor is uniquely maximized at the empirical zero fraction. Without
    positives the likelihood is independent of $p$.
---

The zero-modified geometric distribution assigns probability $p_0$ to zero and uses a geometric law on the positive integers for the remaining counts. The coefficient $p$ controls the positive counts independently of the zero probability.

## Details

The zero-indicator and positive-count likelihoods factorize. A free zero probability is the empirical zero fraction. With positive observations, the free geometric probability is the reciprocal of their mean. Holding either coefficient leaves the other estimate unchanged.

Positive observations all equal to one give the attained endpoint $p=1$, a two-point law on zero and one. With no positives the geometric probability is unidentified. Held zero-probability endpoints must agree with whether zeros or positives occur. A held unit geometric probability cannot explain counts above one.

## R notes

Supply prob or p0 to hold it; `NULL` estimates it. All-zero samples warn that the base probability is unidentified. Supplied values are preserved and a free base probability uses `prob = 0.5`. With free zero probability they return `p0 = 1`; samples with no zeros return `p0 = 0`. A free fit with positives all equal to one warns that `mlbinom(x, size = 1)` fits the attained two-point law exactly.
