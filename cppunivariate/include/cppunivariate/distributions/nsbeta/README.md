---
id: nsbeta
name: Nonstandard beta
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: shape1, role: shape, domain: "(0, inf)", default: 2}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 3}
  - {name: min, role: lower endpoint, domain: "(-inf, inf)", default: 0, required: true, required_reason: "With the bounds estimated the likelihood is unbounded: a shape below 1 makes the density singular at its bound, so let that bound approach the sample extreme. Hold both, e.g. `mlnsbeta(x, min = 0, max = 1)`, which is `mlbeta(x)`. Fits with bounds free and other parameters held are not supported."}
  - {name: max, role: upper endpoint, domain: "(-inf, inf)", default: 1, required: true, required_reason: "With the bounds estimated the likelihood is unbounded: a shape below 1 makes the density singular at its bound, so let that bound approach the sample extreme. Hold both, e.g. `mlnsbeta(x, min = 0, max = 1)`, which is `mlbeta(x)`. Fits with bounds free and other parameters held are not supported."}
r:
  density: extraDistr::dnsbeta
  sampler: extraDistr::rnsbeta
  control:
    min: Known finite lower endpoint.
    max: Known finite upper endpoint larger than min.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape1: Holding this parameter is not supported.
    shape2: Holding this parameter is not supported.
fit_args: {min: 0, max: 1}
log_density: (shape1-1)*log(((x-min)/(max-min))) + (shape2-1)*log(1-((x-min)/(max-min))) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)-log(max-min)
examples: |
  set.seed(3)
  mlnsbeta(simulate_default("mlnsbeta", 100), min = 0, max = 1)
---

This family has a known support endpoint and delegates fitting to beta.

## Details

For at least two finite, nonconstant observations strictly between the held bounds, the MLE is unique and attained: the transformed beta log likelihood is strictly concave in its two shapes. Constant transformed data have unbounded likelihood as both shapes grow.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

With the opposite bound held beyond the sample, a shape a < 1 and its bound approaching the corresponding extreme at distance epsilon gives k factors proportional to epsilon^(a-1); all other factors approach positive constants. Thus the likelihood diverges. The held-bounds fit transforms to beta and subtracts n log(max-min) from log likelihood.
