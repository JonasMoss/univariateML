---
id: geom
name: Geometric
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: stats::dgeom
  sampler: stats::rgeom
  control:
    prob: "Optional held prob; NULL estimates it."
log_density: "log(prob) + x*log(1 - prob)"
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  set.seed(1)
  x <- rml(200, mlgeom())
  mlgeom(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations:
  special_case_of:
  - family: nbinom
    at:
      size: 1
      prob: prob
fit:
  method: closed-form
  certified: unknown
  note: Primary route from the 2026-10-30 fitter inventory; guarantees across held subsets are not classified.
areas:
- counts and ecology
classification: classical
classification_reason: A standard distribution in introductory probability or applied statistical modelling.
---

The geometric distribution models the number of failures before the first success in independent Bernoulli trials.

## Details

The count starts at zero, following the R convention. All-zero data give the boundary estimate `prob = 1`.

Supplying `prob` evaluates the likelihood at that value. Held values outside the parameter domain or incompatible with the observed counts stop.
