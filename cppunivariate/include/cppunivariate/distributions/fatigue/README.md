---
id: fatigue
name: Birnbaum-Saunders
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: alpha, role: shape, domain: "(0, inf)", default: 3.0}
  - {name: beta, role: scale, domain: "(0, inf)", default: 2.0}
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0.0}
r:
  density: extraDistr::dfatigue
  sampler: extraDistr::rfatigue
  control:
    abstol: Absolute Newton step tolerance in `beta`.
    iterlim: Maximum number of iterations.
log_density: log(x-mu+beta) - 3*log(x-mu)/2 - log(beta)/2 - log(2*alpha) - log(2*pi)/2 - ((x-mu)/beta + beta/(x-mu)-2)/(2*alpha**2)
references:
  - "From, S. G. and Li, L. (2006). Estimation of the parameters of the Birnbaum–Saunders distribution. Communications in Statistics, Theory and Methods, 35(12), 2157–2169. doi:10.1080/03610920600853563."
examples: |
  mlfatigue(precip)
---

The Birnbaum--Saunders distribution models lifetimes arising from cumulative fatigue damage.

## Details

The location `mu` is fixed at 0 and is returned as a coefficient for compatibility; only `alpha` and `beta` are estimated. `beta` is a scale parameter. Observations must be strictly positive.
