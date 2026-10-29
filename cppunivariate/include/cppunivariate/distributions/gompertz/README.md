---
id: gompertz
name: Gompertz
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: a
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: b
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: extraDistr::dgompertz
  sampler: extraDistr::rgompertz
  control:
    reltol: Relative step tolerance in `b * mean(x)`, with an absolute floor of the same size in these dimensionless units.
    iterlim: Maximum number of iterations.
log_density: log(a) + b*x - (a/b)*(exp(b*x)-1)
limits:
  - model: 'exp'
    when: '$b \to 0$ with $a$ fixed'
    detect: 'mean(x^2) >= 2*mean(x)^2'
    status: 'Overdispersed'
references:
  - "Lenart, A. (2012). The Gompertz distribution and Maximum Likelihood Estimation of its parameters - a revision. MPIDR Working Paper WP 2012-008."
examples: |
  mlgompertz(precip)
holds:
- held:
  - a
  status: not-implemented
  reason: The adapter does not accept `a`; this hold is not implemented (P2).
- held:
  - b
  status: not-implemented
  reason: The adapter does not accept `b`; this hold is not implemented (P2).
---

The Gompertz distribution has an exponentially increasing hazard and is used to model lifetimes.

## Details

The hazard is $a\exp(bx)$, with both parameters positive.
