---
id: zmnbinom
name: Zero-modified negative binomial
support:
  type: integer
  lower: 0
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
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmnbinom
  sampler: rzmnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for scalar profile search.
    iterlim: Maximum scalar search iterations.
    prob: "Optional held prob; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)), True))
limits:
- model: zmpois
  when: $size \to \infty$ with the positive conditional mean fixed
  detect: size and prob are free and the maximised face likelihood wins within 1e-8 times the sample size
  status: PoissonLimit
examples: |
  set.seed(1)
  x <- rml(200, mlzmnbinom())
  mlzmnbinom(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: nbinom
    at:
      size: size
      prob: prob
    type: zero-modified
    mass: p0
fit:
  method: multistart
  certified: 'no'
  note: 'zmnbinom/README.md, Details; zmnbinom/mle.hpp (public fitter and held overloads); zmnbinom/test.cpp: Default
    and nontrivial conditional fits use multistart, finite grids or local profile refinement without a global certificate;
    all-held density evaluation is not counted as certification.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model overdispersed positive counts with a separately fitted zero probability.
summary: Zero-modified negative binomial provides a model for overdispersed positive counts with a separately fitted
  zero probability.
aliases:
- "hurdle negative binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
---

Zero-modified negative binomial distributions model counts with a separate zero component.

## Details

Actuar defines `size = 0` as the logarithmic series with `theta = 1 - prob`; this attained member is returned. Fixed size may be zero.
With no positives the fitter returns `p0 = 1`, keeps a supplied size and picks conventional base parameters with a warning. With no zeros it returns `p0 = 0`. Positive observations all equal to one give an attained two-point law; the fitter warns that `mlbinom(x, size = 1)` fits it exactly.
Holding size fixed removes the infinite-size escape. The point mass at one remains attained for every fixed size.

The zero-indicator likelihood and the positive conditional likelihood factorize. Holding `p0`, including its empirical optimum, leaves the positive conditional fit unchanged. Held `p0 = 0` with zeros or `p0 = 1` with positives is incompatible and stops. With no positives, base parameters are unidentified; supplied values are preserved and free base parameters use conventional representatives. Holding `prob < 1` removes the infinite-size escape; with only `p0` held the positive conditional profile can still escape to `mlzmpois`. Size zero is the attained logarithmic member. Profile searches are numerical, not certified global optimization.
