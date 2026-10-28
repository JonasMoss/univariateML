---
id: zipf
name: Zipf
support: {type: integer, lower: 1, upper: inf, closed: [true, false]}
params:
  - {name: "N", role: upper, domain: "[1, inf)", default: 3.0}
  - {name: s, role: shape, domain: "[0, inf)", default: 0.5}
r:
  density: sads::dzipf
  sampler: sads::rzipf
  control:
    reltol: "Relative tolerance on the Newton step, with an absolute floor of the same size."
    iterlim: "Maximum number of Newton iterations."
log_density: "-s*log(x) - log(Sum(k**(-s), (k, 1, N)))"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  AIC(mlzipf(corbet))
  AIC(mllgser(corbet))
---

The finite Zipf distribution describes power-law frequencies on consecutive positive integers.

## Details

The fitted upper endpoint `N` is `max(x)` and is a positive integer. The support is the integers from one to `N`. A negative unconstrained shape estimate is rejected with status `ShapeNegative`; `s = 0` is the uniform boundary. With all observations equal to one the estimate is not unique and the fitter warns. This function fits observations directly; for rank-abundance data see `sads::fitzipf`.
