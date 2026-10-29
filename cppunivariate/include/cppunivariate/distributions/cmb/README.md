---
id: cmb
name: Conway--Maxwell--binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  role: trials
  domain: '[0, inf)'
  simulate: 10
  integer: yes
  kind: design
- name: prob
  role: probability
  domain: (0, 1)
  simulate: 0.3
  kind: regular
- name: nu
  role: dispersion
  domain: (-inf, inf)
  simulate: 1.5
  kind: regular
r:
  density: dcmb
  sampler: rcmb
  control:
    size: "Required known number of trials, a nonnegative integer at least max(x), at most 1e7; always held fixed."
    prob: "Optional fixed canonical probability strictly between zero and one; NULL estimates it."
    nu: "Optional fixed finite real dispersion; NULL estimates it. Both prob and nu may be held fixed."
    reltol: "Relative tolerance on the dispersion moment residual."
    iterlim: "Maximum number of dispersion bisection iterations."
log_density: "nu*(loggamma(size+1)-loggamma(x+1)-loggamma(size-x+1)) + x*log(prob) + (size-x)*log(1-prob) - log(Sum((factorial(size)/(factorial(k)*factorial(size-k)))**nu * prob**k * (1-prob)**(size-k), (k, 0, size)))"
limits:
  - name: point mass
    when: '$\mathrm{logit}(p)$ or $\nu$ diverges along an exposing direction'
    detect: constant data in a joint fit; endpoint data with fixed nu; central constant data with fixed prob
    status: PointMass
  - name: adjacent two-point law
    when: '$\nu \to +\infty$ with the canonical probability adjusted'
    detect: all data on two adjacent integers in a joint fit; only the two central integers when prob is fixed
    status: AdjacentLimit
  - name: endpoint two-point law
    when: '$\nu \to -\infty$'
    detect: all data in {0, size} with both present in a joint fit; any endpoint-only data with prob fixed
    status: EndpointLimit
references:
  - "Kadane, J. B. (2016). Sums of Possibly Associated Bernoulli Variables: The Conway-Maxwell-Binomial Distribution. Bayesian Analysis. doi:10.1214/15-BA955."
examples: |
  mlcmb(c(0, 1, 2, 2, 3, 4, 6), size = 10)
holds:
- held: []
  status: not-implemented
  reason: Free size awaits resolution of the P0 reference-cap assumption (1e7 is an allocation limit).
- held:
  - prob
  status: not-implemented
  reason: Free size awaits resolution of the P0 reference-cap assumption (1e7 is an allocation limit).
- held:
  - nu
  status: not-implemented
  reason: Free size awaits resolution of the P0 reference-cap assumption (1e7 is an allocation limit).
- held:
  - prob
  - nu
  status: not-implemented
  reason: Free size awaits resolution of the P0 reference-cap assumption (1e7 is an allocation limit).
fit_args:
  size: 10
---

The Conway--Maxwell--binomial distribution extends the binomial by raising its combinatorial factor to a real dispersion power.

## Details

The size is a required design quantity. The canonical probability is generally not the mean divided by size. Dispersion is unrestricted in sign, and `nu = 1` is binomial. The fitter profiles the canonical log odds by mean matching and solves the remaining dispersion moment equation. Finite log-sum-exp enumeration costs O(size) per evaluation; size is limited to 1e7 for allocation safety.

For size at least two, the joint MLE exists exactly when the mean sufficient statistic lies inside the convex polygon of the statistic points. Boundary samples stop with an honestly named limiting law. With fixed dispersion, only endpoint point masses remain. With fixed probability, dispersion can escape only toward the central mode(s) or the endpoints; constants elsewhere can have finite fits. With both coordinates fixed there is no optimization or escape. Size zero is unidentified and rejected. With size one, dispersion is unidentified: an unspecified dispersion is set to one with a warning and marked fixed, leaving a Bernoulli fit.
