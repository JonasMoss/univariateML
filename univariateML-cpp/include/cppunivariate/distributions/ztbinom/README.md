---
id: ztbinom
inference_numeric:
  - regime: "size one"
    available: true
    condition: "known size = 1; 0 < prob < 1 free; the zero-truncated law is a point mass, so information is zero"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: primary interior
    available: true
    condition: "known integer size >= 2 held; 0 < prob < 1 free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated binomial
support: {type: integer, lower: "1", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[1, inf)'
  integer: yes
  simulate: 10
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dztbinom
  sampler: rztbinom
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Maximum scalar search iterations."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
log_density: loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob)-log(1-(1-prob)**size)
limits:
- model: ztpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlztbinom())
  mlztbinom(x, size = 10)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: zmbinom
    at:
      size: size
      prob: prob
      p0: 0
fit:
  method: multistart
  certified: partial
  note: 'ztbinom/README.md, Details; ztbinom/mle.hpp (public fitter and held overloads); ztbinom/test.cpp: Fixed
    size has exact conditional probability profiling; held probability has integer tail bounds. Free-size/free-probability
    grids lack a global unimodality certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model binomial success counts sampled only when positive.
summary: Zero-truncated binomial provides a model for binomial success counts sampled only when positive.
aliases:
- "positive binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-truncated binomial distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 0`. This boundary is returned as an attained member.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.

A held probability is preserved exactly. With probability strictly between zero and one, the fixed-probability integer search bounds the remaining tail before returning; exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations.

With both size and probability free, the search profiles probability at its exact conditional-mean-matching value for each candidate size and scans an exponential grid of candidate sizes, refining each local peak. Unlike `mlbinom`'s free-size search, no unimodality proof is known for this truncated profile (DeRiggi 1983 covers the untruncated binomial only), so this remains a numerical grid search, not a certified one; see `dev/research/evaluations/2026-10-29-tail85-report.md`. The grid has no fixed magnitude limit: it keeps growing until it has gone ten exponential steps without an improvement, bounded only by `iterlim` (a larger `iterlim` lets it search further, never a hard cap). Cross-checked against independent reference-density multistart oracles, including tied and near-Poisson samples.

DeRiggi (1994) reports a counterexample to general continuous positive-binomial likelihood unimodality; this does not itself establish a counterexample for integer sizes. The grid and its Poisson-face comparison do not certify the global optimum. See the [size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) for the reference and the remaining gap.
