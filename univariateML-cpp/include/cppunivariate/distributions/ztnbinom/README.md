---
id: ztnbinom
inference_numeric:
  - regime: "known zero negative-binomial size"
    available: true
    condition: "size = 0 held; 0 < prob < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite size > 0 and 0 < prob < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated negative binomial
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
params:
- name: size
  symbol: 'r'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: dztnbinom
  sampler: rztnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
    prob: "Optional held prob; NULL estimates it."
log_density: loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)
limits:
- model: ztpois
  firmness: proved
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: size and prob are free and the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
examples: |
  set.seed(1)
  x <- rml(200, mlztnbinom())
  mlztnbinom(x)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: nbinom
    at:
      size: size
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: zmnbinom
    at:
      size: size
      prob: prob
      p0: 0
fit:
  method: multistart
  certified: 'no'
  note: 'ztnbinom/README.md, Details; ztnbinom/mle.hpp (public fitter and held overloads); ztnbinom/test.cpp: Default
    and nontrivial conditional fits use multistart, finite grids or local profile refinement without a global certificate;
    all-held density evaluation is not counted as certification.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model negative-binomial counts sampled only when positive.
summary: Zero-truncated negative binomial provides a model for negative-binomial counts sampled only when positive.
aliases:
- "positive negative binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-truncated negative binomial distributions model positive counts.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) derives the stated limits and held-parameter conditions.

Actuar defines the point mass at one at `prob = 1`. This boundary is returned as an attained member.
Actuar defines `size = 0` as the logarithmic series with `theta = 1 - prob`; this attained member is returned. Fixed size may be zero.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.

Holding `prob < 1` removes the infinite-size escape. Size zero remains the attained logarithmic member and is compared with all bracketed profile peaks. Held `prob = 1` is compatible only with ones. The scalar grid search is numerical, not a certificate that every profile peak has been found.
