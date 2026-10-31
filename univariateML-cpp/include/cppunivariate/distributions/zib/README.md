---
id: zib
name: Zero-inflated binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  integer: yes
  simulate: 10
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzib
  sampler: rzib
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Maximum scalar search iterations."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
    pi: "Optional fixed pi in `[0, 1]`; NULL estimates it."
log_density: Piecewise((log(pi+(1-pi)*(1-prob)**size), Eq(x,0)), (log(1-pi)+loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob), True))
limits:
- model: zip
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: ZipLimit
- model: zip
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the matching held-mixture Poisson face wins the integer profile
  status: ZipLimit
  fixed:
  - pi
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlzib())
  mlzib(x, size = 10)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-inflated
    mass: pi
fit:
  method: multistart
  certified: partial
  note: 'zib/README.md, Details; zib/mle.hpp (public fitter and held overloads); zib/test.cpp: Fixed size has exact
    conditional probability profiling; held probability has integer tail bounds. Free-size/free-probability grids
    lack a global unimodality certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model bounded success counts with an additional structural-zero component.
summary: Zero-inflated binomial provides a model for bounded success counts with an additional structural-zero component.
aliases: []
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
---

Zero-inflated binomial distributions model counts with a separate zero component.

## Details

Holding size fixed removes the infinite-size escape. The binomial face remains attained; all-zero data return `prob = pi = 0`.
Fixed `size = 0` is the attained point mass at zero and requires all-zero data.

A held probability is preserved exactly. With probability strictly between zero and one, the fixed-probability integer search bounds the remaining tail before returning; exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations.

Holding inflation leaves a scalar probability profile at each integer size. Its stationary points are isolated by the quadratic turning-point equation; the infinite-size comparison uses ZIP with the same held inflation.

With size free and either or both of probability and inflation also free, the search scans an exponential grid of candidate sizes (profiling the remaining free coefficients exactly at each one) and refines each local peak. As with `mlztbinom`, no unimodality proof is known for this profile, so this remains a numerical grid search, not a certified one; see `dev/research/evaluations/2026-10-29-tail85-report.md`. The grid has no fixed magnitude limit: it keeps growing until ten exponential steps pass without an improvement, bounded only by `iterlim`. Cross-checked against independent reference-density multistart oracles, including tied and near-Poisson samples.

The grid and ZIP-face comparison do not certify the global optimum. The [size-search investigation](../../../../../dev/research/evaluations/2026-10-31-size-search-report.md) gives the reduction to positive-binomial and binomial branches and identifies the remaining gap.
