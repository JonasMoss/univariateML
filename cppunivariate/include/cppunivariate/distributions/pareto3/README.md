---
id: pareto3
name: Pareto type III
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: shape, role: shape, domain: "(0, inf)", default: 2}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1}
  - {name: min, role: lower endpoint, domain: "(-inf, inf)", default: 0, required: true, required_reason: "With min estimated the likelihood is unbounded: the density is singular at min when shape < 1, so let min rise to the sample minimum. Hold it, e.g. `mlpareto3(x, min = 0)`, which is `mlllogis(x)`. Fits with min free and other parameters held are not supported."}
r:
  density: actuar::dpareto3
  sampler: actuar::rpareto3
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape: Holding this parameter is not supported.
    scale: Holding this parameter is not supported.
fit_args: {min: 0}
log_density: log(shape) + log((1/scale)) + (shape - 1)*log((1/scale)*(x-min)) - 2*log(1 + ((1/scale)*(x-min))**shape)
examples: |
  set.seed(3)
  mlpareto3(simulate_default("mlpareto3", 100), min = 0)
---

This family has a known support endpoint and delegates fitting to llogis.

## Details

For at least two finite, nonconstant observations strictly above min, the held-threshold MLE is unique and attained. On log(x-min), the logistic likelihood is strictly concave in inverse scale and standardized location. Constant samples have unbounded likelihood as log-logistic shape grows.

The delegate coefficients map as shape=shape and scale=1/rate.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

At distance y above the endpoint the density is asymptotic to C y^(a-1), where a is shape. Choose 0<a<1 and hold other parameters positive. As min approaches the sample minimum from below, k minimum factors diverge as epsilon^(k*(a-1)); all remaining factors approach positive constants. Thus unrestricted likelihood is unbounded, including constant samples.
