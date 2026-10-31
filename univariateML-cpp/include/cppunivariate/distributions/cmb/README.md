---
id: cmb
name: Conway--Maxwell--binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, 10000000]'
  simulate: 10
  integer: yes
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
- name: nu
  symbol: '\nu'
  role: dispersion
  domain: (-inf, inf)
  simulate: 1
  kind: regular
r:
  density: dcmb
  sampler: rcmb
  control:
    size: "Required number of trials; NULL estimates it when prob and nu are both held. Fixed sizes are limited to 1e7 for allocation safety."
    prob: "Optional fixed canonical probability between zero and one inclusive; NULL estimates it."
    nu: "Optional fixed finite real dispersion; NULL estimates it. Both prob and nu may be held fixed."
    reltol: "Relative tolerance on the dispersion moment residual."
    iterlim: "Maximum number of dispersion bisection iterations."
log_density: "nu*(loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)) + x*log(prob) + (size-x)*log(1-prob) - log(Sum((factorial(size)/(factorial(k)*factorial(size-k)))**nu * prob**k * (1-prob)**(size-k), (k, 0, size)))"
limits:
  - name: point mass
    when: '$\nu$ diverges along an exposing direction toward an interior point mass'
    detect: constant interior data in a joint fit; central constant data with interior prob fixed
    status: PointMass
  - name: adjacent two-point law
    when: '$\nu \to +\infty$ with the canonical probability adjusted'
    detect: size at least two, with both adjacent integers observed in a joint fit; only the two central integers with interior prob fixed
    status: AdjacentLimit
  - name: endpoint two-point law
    when: '$\nu \to -\infty$'
    detect: all data in {0, size} with both present in a joint fit; any endpoint-only data with interior prob fixed
    status: EndpointLimit
references:
- 'Kadane, J. B. (2016). Sums of Possibly Associated Bernoulli Variables: The Conway-Maxwell-Binomial Distribution.
  Bayesian Analysis. doi:10.1214/15-BA955.'
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds:
- held: []
  status: not-implemented
  reason: "Free size with both coordinates free requires a global profile bound including Conway-Maxwell-Poisson, geometric and dispersion-limit faces; no reliable stopping rule is established."
- held:
  - prob
  status: not-implemented
  reason: "Free size with prob held requires a global profile bound for changing dispersion, including geometric and escaping-mass limits; no reliable stopping rule is established."
- held:
  - nu
  status: not-implemented
  reason: "Free size with nu held requires a global profile bound: positive nu admits a Conway-Maxwell-Poisson face, while negative nu has different escaping-mass limits."
fit_args:
  size: 10
examples: |
  set.seed(1)
  x <- rml(200, mlcmb())
  mlcmb(x, size = 10L)

group: Poisson, binomial and negative binomial
construction:
- base
- generalisation
relations: {}
fit:
  method: multistart
  certified: partial
  note: 'cmb/README.md, Details; cmb/mle.hpp (public fitter and held overloads); cmb/test.cpp: Moment matching exploits
    exponential-family convexity for fixed design/dispersion subsets; integer-tail or normalizer bounds certify
    only their stated scope, not every joint numerical solve.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model bounded counts with dispersion departing from independent Bernoulli trials.
summary: Conway--Maxwell--binomial provides a model for bounded counts with dispersion departing from independent
  Bernoulli trials.
aliases:
- "CMB"
- "COM-binomial"
- "CMP-binomial"
key_references:
- 2
---

The Conway--Maxwell--binomial distribution extends the binomial by raising its combinatorial factor to a real dispersion power.

## Details

The size is a required design quantity. The canonical probability is generally not the mean divided by size. Dispersion is unrestricted in sign, and `nu = 1` is binomial. The fitter profiles the canonical log odds by mean matching and solves the remaining dispersion moment equation. Finite log-sum-exp enumeration costs O(size) per evaluation; size is limited to 1e7 for allocation safety.

For size at least two, the joint MLE is attained when the mean sufficient statistic lies inside the convex polygon of the statistic points or is an endpoint vertex. Other boundary samples stop with a named limiting law. With fixed dispersion, endpoint point masses are attained. With fixed probability, dispersion can escape only toward the central mode(s) or the endpoints; constants elsewhere can have finite fits. With both coordinates fixed there is no optimization or escape. Size zero is the attained point mass at zero: unidentified free prob and nu are represented by 0.5 and one. Endpoint samples with free prob return prob zero or one, with unidentified free nu represented by one. Endpoint probability holds are accepted only for compatible observations. With size one, dispersion is unidentified: an unspecified dispersion is set to one with a warning for interior probabilities and marked fixed, leaving a Bernoulli fit.

With both `prob` and `nu` held, `size = NULL` enumerates feasible integer sizes until a decreasing upper bound excludes the entire remaining tail. For positive dispersion the bound uses a normalizer term at `max(x) + 1`; for negative dispersion it uses the endpoint terms. At zero dispersion the normalizer increases with size, so the smallest feasible size is optimal. This argument does not assume size-profile unimodality. Four million cumulative normalizer terms form a work budget; exhaustion stops rather than returning a capped estimate. Endpoint probabilities and size zero are handled exactly. The other free-size profiles remain unavailable: no global tail bound or reliable numerical stopping rule has been established. Positive held dispersion can approach a Conway–Maxwell–Poisson law, and changing dispersion with probability held can approach a geometric law.

## d/p/q/r

Finite log normalization is reused for tables; the binomial is the nu = 1 member. The size cap is 1e7.

### Exact dpqr submodels (TASK-134)

At size zero or probability zero/one, d/p/q evaluate the atom directly without
constructing the enumeration table. Quantile endpoint queries retain the
nominal design interval `[0, size]`. At `nu = 1`, d/p/q delegate to the binomial
implementation: the binomial theorem supplies its normalizer. Apart from
these and the geometric submodel below, members retain finite enumeration; no new complexity guarantee is made at the size cap
for nondegenerate, non-binomial members. Seeded sampling retains its existing
inverse-transform path and atomic shortcuts.

At `nu = 0`, weights are proportional to `exp(k*eta)` with
`eta = log(prob)-log1p(-prob)`. The normalizer is the finite geometric
sum `(1-exp((size+1)*eta))/(1-exp(eta))`; positive eta is handled by
reflection, and eta zero uses the uniform limit. Direct lower and upper
partial sums use `log1mexp` before quantile bisection on the finite design
support. Size one is binomial for every nu. These algebraic reductions are
exact; agreement of their floating-point evaluations with enumeration is
checked numerically.
