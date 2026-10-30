---
id: cmb
name: Conway--Maxwell--binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  role: trials
  domain: '[0, 10000000]'
  simulate: 10
  integer: yes
  kind: design
- name: prob
  role: probability
  domain: '[0, 1]'
  simulate: 0.3
  kind: regular
- name: nu
  role: dispersion
  domain: (-inf, inf)
  simulate: 1.5
  kind: regular
r:
  export:
    params:
      size: Known number of trials.
      prob: Canonical probability coordinate; generally not the mean divided by size.
      nu: Real dispersion coordinate.
    details: Support is 0 to size. size is a nonnegative integer at most 1e7; prob lies between
      zero and one (inclusive), and nu is any finite real number. The member nu = 1 is binomial.
      Normalization enumerates the finite support in log scale. Tables are shared by repeated
      parameter tuples within each call. Tails are summed directly. Random draws use inversion.
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
  - "Kadane, J. B. (2016). Sums of Possibly Associated Bernoulli Variables: The Conway-Maxwell-Binomial Distribution. Bayesian Analysis. doi:10.1214/15-BA955."
examples: |
  mlcmb(c(0, 1, 2, 2, 3, 4, 6), size = 10)
holds:
- held: []
  status: not-implemented
  reason: "Free size with a free CMB coordinate is not implemented: P0 omits Conway-Maxwell-Poisson and geometric size-limit faces; their constrained comparison remains unresolved."
- held:
  - prob
  status: not-implemented
  reason: "Free size with a free CMB coordinate is not implemented: P0 omits Conway-Maxwell-Poisson and geometric size-limit faces; their constrained comparison remains unresolved."
- held:
  - nu
  status: not-implemented
  reason: "Free size with a free CMB coordinate is not implemented: P0 omits Conway-Maxwell-Poisson and geometric size-limit faces; their constrained comparison remains unresolved."
fit_args:
  size: 10
---

The Conway--Maxwell--binomial distribution extends the binomial by raising its combinatorial factor to a real dispersion power.

## Details

The size is a required design quantity. The canonical probability is generally not the mean divided by size. Dispersion is unrestricted in sign, and `nu = 1` is binomial. The fitter profiles the canonical log odds by mean matching and solves the remaining dispersion moment equation. Finite log-sum-exp enumeration costs O(size) per evaluation; size is limited to 1e7 for allocation safety.

For size at least two, the joint MLE is attained when the mean sufficient statistic lies inside the convex polygon of the statistic points or is an endpoint vertex. Other boundary samples stop with a named limiting law. With fixed dispersion, endpoint point masses are attained. With fixed probability, dispersion can escape only toward the central mode(s) or the endpoints; constants elsewhere can have finite fits. With both coordinates fixed there is no optimization or escape. Size zero is the attained point mass at zero: unidentified free prob and nu are represented by 0.5 and one. Endpoint samples with free prob return prob zero or one, with unidentified free nu represented by one. Endpoint probability holds are accepted only for compatible observations. With size one, dispersion is unidentified: an unspecified dispersion is set to one with a warning for interior probabilities and marked fixed, leaving a Bernoulli fit.

With both `prob` and `nu` held, `size = NULL` enumerates feasible integer sizes until a decreasing upper bound excludes the entire remaining tail. For positive dispersion the bound uses a normalizer term at `max(x) + 1`; for negative dispersion it uses the endpoint terms. At zero dispersion the normalizer increases with size, so the smallest feasible size is optimal. This argument does not assume size-profile unimodality. Four million cumulative normalizer terms form a work budget; exhaustion stops rather than returning a capped estimate. Endpoint probabilities and size zero are handled exactly. The free-coordinate size profiles remain unavailable because their limiting laws and constrained likelihood comparisons are not established by P0b; in particular, positive held dispersion can approach a Conway–Maxwell–Poisson law, rather than a two-point law.
