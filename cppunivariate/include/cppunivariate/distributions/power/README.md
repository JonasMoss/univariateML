---
id: power
name: PowerDist
support: {type: real, lower: 0.0, upper: inf, closed: [false, false]}
params:
- name: alpha
  role: upper
  domain: (0, inf)
  simulate: 1.0
  bound: upper
  kind: endpoint
- name: beta
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
r:
  density: extraDistr::dpower
  sampler: extraDistr::rpower
  control:
    epsilon: Positive relative offset from the sample maximum to place the fitted upper endpoint beyond the data.
log_density: log(beta) - beta*log(alpha) + (beta-1)*log(x)
references:
  - "Arslan, G. \"A new characterization of the power distribution.\" Journal of Computational and Applied Mathematics 260 (2014): 99-102."
examples: |
  mlpower(precip)
holds:
- held:
  - alpha
  status: not-implemented
  reason: The adapter does not accept `alpha`; this hold is not implemented (P2).
- held:
  - beta
  status: not-implemented
  reason: The adapter does not accept `beta`; this hold is not implemented (P2).
---

The power distribution models positive quantities below a finite upper endpoint.

## Details

The fitted support is $(0, \alpha)$. The reference `extraDistr::dpower` excludes the upper endpoint, so the likelihood supremum occurs as `alpha` decreases to `max(x)` but is not attained under that convention. The fitter returns `max(x) * (1 + epsilon)`; `epsilon` is a relative endpoint offset, not a convergence tolerance.
