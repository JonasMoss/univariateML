---
id: zib
name: Zero-inflated binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  role: trials
  domain: '[0, inf)'
  integer: yes
  simulate: 8.0
  kind: design
- name: prob
  role: probability
  domain: '[0, 1]'
  simulate: 0.4
  kind: regular
- name: pi
  role: probability
  domain: '[0, 1]'
  simulate: 0.4
  kind: regular
r:
  density: extraDistr::dzib
  sampler: extraDistr::rzib
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Maximum scalar search iterations."
    prob: "Optional fixed probability in [0, 1]; NULL estimates it."
    pi: "Optional fixed pi in [0, 1]; NULL estimates it."
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
---

Zero-inflated binomial distributions model counts with a separate zero component.

## Details

Holding size fixed removes the infinite-size escape. The binomial face remains attained; all-zero data return `prob = pi = 0`.
Fixed `size = 0` is the attained point mass at zero and requires all-zero data.

A held probability is preserved exactly. With probability strictly between zero and one, the fixed-probability integer search bounds the remaining tail before returning; exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations.

Holding inflation leaves a scalar probability profile at each integer size. Its stationary points are isolated by the quadratic turning-point equation; the infinite-size comparison uses ZIP with the same held inflation.

With size free and either or both of probability and inflation also free, the search scans an exponential grid of candidate sizes (profiling the remaining free coefficients exactly at each one) and refines each local peak. As with `mlztbinom`, no unimodality proof is known for this profile, so this remains a numerical grid search, not a certified one; see `notes/evaluations/2026-10-29-tail85-report.md`. The grid has no fixed magnitude limit: it keeps growing until ten exponential steps pass without an improvement, bounded only by `iterlim`. Cross-checked against independent reference-density multistart oracles, including tied and near-Poisson samples.
