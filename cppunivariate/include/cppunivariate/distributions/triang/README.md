---
id: triang
name: Triangular
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: a, role: location, domain: "(-inf, b)", default: 0, fixable: true}
  - {name: b, role: location, domain: "(a, inf)", default: 1, fixable: true}
  - {name: c, role: location, domain: "[a, b]", default: 0.4, fixable: true, kink: true}
r:
  density: extraDistr::dtriang
  sampler: extraDistr::rtriang
  control:
    reltol: Absolute tolerance on the dimensionless concave endpoint Newton step.
    iterlim: Maximum iterations per endpoint solve.
log_density: log(2) - log(b-a) + Piecewise((log((x-a)/(c-a)), x < c), (log((b-x)/(b-c)), x > c), (0, True))
examples: |
  set.seed(3)
  mltriang(extraDistr::rtriang(100, 0, 1, 0.4))
  mltriang(extraDistr::rtriang(100, 0, 1, 0.4), a = 0, b = 1)
---

The triangular distribution describes bounded measurements with a single mode.

## Details

Endpoint modes are included, with one-sided density `2/(b-a)` at the mode.
Every nonconstant compatible sample has an attained maximum; constant data
have unbounded likelihood only when the held parameters allow the width to
collapse at the observation. Observations outside held endpoints, both held
endpoints observed, or held parameters inconsistent with `a < b` and
`a <= c <= b` give an error. Distinct maximizing modes need not be unique;
the smallest mode is returned for numerical ties, with a warning when another
candidate is within `1e-10 * length(x)` in log likelihood.
