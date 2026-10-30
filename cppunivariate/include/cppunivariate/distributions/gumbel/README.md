---
id: gumbel
name: Gumbel
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 3.0
  kind: regular
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: dgumbel
  sampler: rgumbel
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    reltol: Relative step tolerance in log(sigma) of the underlying Gumbel fit.
    iterlim: Maximum number of iterations.
log_density: -log(sigma) - (x-mu)/sigma - exp(-(x-mu)/sigma)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 22. Wiley, New York."
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: Unbounded
    message: "unbounded"
examples: |
  set.seed(1)
  x <- rml(200, mlgumbel())
  mlgumbel(x)
---

The Gumbel distribution models maxima and extreme measurements.

## Details

This is the Gumbel distribution for maxima; `sigma` is positive.


Holding mu gives a unique inverse-scale maximum unless every observation equals that mu. Holding sigma gives an exact location profile and permits constant samples.
