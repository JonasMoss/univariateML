---
id: burr
name: Burr
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: shape1, role: shape, domain: "(0, inf)", default: 1.0}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 2.0}
r:
  density: actuar::dburr
  sampler: actuar::rburr
  control:
    init: Optional positive starting value for `shape2`; the first element is used. `NULL` uses the automatic start.
    abstol: Absolute Newton step tolerance in `log(shape2)`.
    iterlim: Maximum number of iterations.
log_density: log(shape1) + log(shape2) + (shape2-1)*log(x) - (shape1+1)*log(1+x**shape2)
limits:
  - model: 'pareto'
    when: '$\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ fixed'
    detect: 'all(x >= 1)'
    status: 'AllGeOne'
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 1, Chapter 20. Wiley, New York."
examples: |
  mlburr(abalone$length)
---

The Burr type XII distribution is a heavy-tailed distribution for positive size and loss data.

## Details

Only the two shape parameters are estimated; the scale is fixed at 1.
