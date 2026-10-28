---
id: power
name: PowerDist
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
  - {name: alpha, role: upper, domain: "(0, inf)", default: 1.0}
  - {name: beta, role: shape, domain: "(0, inf)", default: 2.0}
r:
  density: extraDistr::dpower
  sampler: extraDistr::rpower
  control:
    epsilon: Positive offset added to the sample maximum to place the fitted upper endpoint beyond the data.
log_density: log(beta) - beta*log(alpha) + (beta-1)*log(x)
references:
  - "Arslan, G. \"A new characterization of the power distribution.\" Journal of Computational and Applied Mathematics 260 (2014): 99-102."
examples: |
  mlpower(precip)
---

The power distribution models positive quantities below a finite upper endpoint.

## Details

The fitted support is $(0, \alpha)$. The reference `extraDistr::dpower` excludes the upper endpoint, so the likelihood supremum occurs as `alpha` decreases to `max(x)` but is not attained under that convention. The fitter returns `max(x) + epsilon`; `epsilon` is an endpoint offset, not a convergence tolerance.
