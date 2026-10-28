---
id: geom
name: Geometric
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
  - {name: prob, role: probability, domain: "(0, 1]", default: 0.5}
r:
  density: stats::dgeom
  sampler: stats::rgeom
  control: {}
log_density: "log(prob) + x*log(1 - prob)"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  mlgeom(corbet)
---

The geometric distribution models the number of failures before the first success in independent Bernoulli trials.

## Details

The count starts at zero, following the R convention. All-zero data give the boundary estimate `prob = 1`.
