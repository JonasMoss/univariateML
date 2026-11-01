---
id: tpois
inference_numeric:
  - regime: "singleton retained support"
    available: true
    condition: "known truncation bounds with exactly one retained integer; finite lambda > 0 free; the conditional law is a point mass, so information is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: primary interior
    available: true
    condition: "finite lambda > 0 free; known a and b held; retained support has at least two integers"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Truncated Poisson
support: {type: integer, lower: "max(0, floor(a)+1)", upper: "floor(b)", closed: [true, true]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: a
  symbol: 'a'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 0
  kind: design
- name: b
  symbol: 'b'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 10
  kind: design
r:
  density: dtpois
  sampler: rtpois
  control:
    lambda: Optional fixed parameter; NULL estimates it.
    a: "Strict lower bound; NULL estimates it as min(x) - 1."
    b: "Inclusive upper bound; NULL estimates it as max(x)."

log_density: "x*log(lambda)-loggamma(x+1)-log(Sum(exp(k*log(lambda)-loggamma(k+1)), (k, Max(0, floor(a)+1), floor(b))))"
limits:
- name: point mass at lowest support point
  firmness: proved
  when: '$\lambda \to 0$'
  detect: all observations at a strictly positive lowest support point
  status: LowerPointMass
  fixed: [b]
- name: point mass at highest support point
  firmness: proved
  when: '$\lambda \to \infty$'
  detect: all observations at finite highest support point
  status: UpperPointMass
  fixed: [a]
holds: []
fit_args:
  a: 0
  b: 10
examples: |
  set.seed(1)
  x <- rml(200, mltpois())
  mltpois(x, a = 0L, b = 10L)

group: modified counts
construction:
- truncation
relations:
  truncation_of:
  - family: pois
    at:
      lambda: lambda
fit:
  method: multistart
  certified: 'yes'
  note: 'tpois/README.md, Details; tpois/mle.hpp (public fitter and held overloads); tpois/test.cpp: Conditional
    mean is strictly increasing in log rate (derivative is conditional variance), giving a unique bracketed root;
    bounds profile to sample extremes and zero mass separates analytically.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model Poisson counts recorded only within a specified conditioning window.
summary: Truncated Poisson provides a model for Poisson counts recorded only within a specified conditioning window.
aliases: []
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
---

The Poisson law conditioned on known bounds `a < x <= b` models truncated counts.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) derives the stated limits and held-parameter conditions.

Bounds must be supplied explicitly to avoid silently ignoring the sampling design.
Untruncated bounds `a = -Inf, b = Inf` recover the ordinary Poisson interior fit.
The conditional mean is strictly increasing in the natural parameter, since its
derivative is the positive conditional variance. Thus every sample mean strictly
between the support edges has one global maximum. Edge-only data escape to a
point mass; a singleton support does not identify the parameter.
The normalizer uses mode-centred recurrences with a two-million-term budget per tail.

When the conditioning event includes zero, the point mass at zero is attained
at `lambda = 0`.
Otherwise the corresponding zero-probability conditioning limit is reported as an escape.

For `a = 0, b = Inf`, interior fits agree with `mlztpois`. The zero-rate
conditioning limit is defined as an attained member only for `mlztpois`. Thus
`mlztpois` retains its attained boundary while `mltpois` reports the point-mass escape.

With a free bound, the conditioning support tightens to the corresponding sample extreme. The strict lower bound is nonunique in `[min(x) - 1, min(x))`; the returned representative is `min(x) - 1`. On singleton support every positive rate maximizes the likelihood; a free rate is represented by one. For nonsingleton support the empirical mean must lie strictly between the effective integer bounds, except that a zero mean attains rate zero when zero is in the support. Otherwise the fitter stops at the lower or upper point-mass limit.

Scalar d/p/q/r use the same positive-probability conditioning contract. At
`lambda = 0`, they accept a nonempty window containing zero and return its
atom at zero; a window excluding zero returns NaN. Its weak-limit atom at the
first allowed count is not an attained member here. Use the separately defined
ztpois family for its attained positive-truncation zero-rate limit.
