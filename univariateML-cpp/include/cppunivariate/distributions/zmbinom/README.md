---
id: zmbinom
name: Zero-modified binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
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
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmbinom
  sampler: rzmbinom
  control:
    size: "Optional fixed size; NULL estimates size."
    reltol: "Tolerance for scalar profile search."
    iterlim: "Maximum scalar search iterations."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
    p0: "Optional fixed p0 in `[0, 1]`; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)+x*log(prob)+(size-x)*log(1-prob)-log(1-(1-prob)**size)), True))
limits:
- model: zmpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
- model: zmpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: the matching held-mixture Poisson face wins the integer profile
  status: PoissonLimit
  fixed:
  - p0
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlzmbinom())
  mlzmbinom(x, size = 10)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: binom
    at:
      size: size
      prob: prob
    type: zero-modified
    mass: p0
fit:
  method: multistart
  certified: partial
  note: 'zmbinom/README.md, Details; zmbinom/mle.hpp (public fitter and held overloads); zmbinom/test.cpp: Fixed
    size has exact conditional probability profiling; held probability has integer tail bounds. Free-size/free-probability
    grids lack a global unimodality certificate.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model bounded counts with an independently adjustable zero probability.
summary: Zero-modified binomial provides a model for bounded counts with an independently adjustable zero probability.
aliases: []
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-modified binomial distributions model counts with a separate zero component.

## Details

With no positives the fitter returns `p0 = 1`, keeps a supplied size and picks conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.

A held probability is preserved exactly. With probability strictly between zero and one, the fixed-probability integer search bounds the remaining tail before returning; exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations.

Holding p0 changes only the Bernoulli zero-indicator likelihood; the positive conditional fit is unchanged. An incompatible held p0 (zero with observed zeros, or one with positives) stops.

The free-size search delegates entirely to `mlztbinom` on the positive observations (the zero/Bernoulli part is closed-form), so it inherits that search's exponential grid and its caveats verbatim: no unimodality proof is known, so it is a numerical search, not a certified one; see `mlztbinom`'s own documentation and `dev/research/evaluations/2026-10-29-tail85-report.md`.
