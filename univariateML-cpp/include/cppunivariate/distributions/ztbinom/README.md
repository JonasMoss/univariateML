---
id: ztbinom
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
---

Zero-truncated binomial distributions model positive counts.

## Details

Actuar defines the point mass at one at `prob = 0`. This boundary is returned as an attained member.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.

A held probability is preserved exactly. With probability strictly between zero and one, the fixed-probability integer search bounds the remaining tail before returning; exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations.

With both size and probability free, the search profiles probability at its exact conditional-mean-matching value for each candidate size and scans an exponential grid of candidate sizes, refining each local peak. Unlike `mlbinom`'s free-size search, no unimodality proof is known for this truncated profile (DeRiggi 1983 covers the untruncated binomial only), so this remains a numerical grid search, not a certified one; see `dev/research/evaluations/2026-10-29-tail85-report.md`. The grid has no fixed magnitude limit: it keeps growing until it has gone ten exponential steps without an improvement, bounded only by `iterlim` (a larger `iterlim` lets it search further, never a hard cap). Cross-checked against independent reference-density multistart oracles, including tied and near-Poisson samples.
