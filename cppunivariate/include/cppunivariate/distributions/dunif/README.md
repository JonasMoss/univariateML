---
id: dunif
name: Discrete uniform
support: {type: integer, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: min, role: lower, domain: "(-inf, max]", default: 2.0}
  - {name: max, role: upper, domain: "[min, inf)", default: 10.0}
r:
  density: extraDistr::ddunif
  sampler: extraDistr::rdunif
  control: {}
log_density: "-log(max - min + 1)"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  mldunif(corbet)
---

The discrete uniform distribution assigns equal probability to every integer between two endpoints.

## Details

The boundary maximum likelihood estimates are the sample minimum and maximum. Both endpoints must be integers, with `min <= max`; the support contains every integer between them, including both endpoints.
