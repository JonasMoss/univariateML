---
id: pareto4
name: Pareto type IV
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: shape1, role: shape, domain: "(0, inf)", default: 0.3}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 0.7}
  - {name: scale, role: scale, domain: "(0, inf)", default: 1, fixable: true}
  - {name: min, role: lower endpoint, domain: "(-inf, inf)", default: 0, required: true, required_reason: "With min estimated the likelihood is unbounded: the density is singular at min when shape2 < 1, so let min rise to the sample minimum. Hold it, e.g. `mlpareto4(x, min = 0)`, which is `mlburr(x)`. Fits with min free and other parameters held are not supported."}
r:
  density: actuar::dpareto4
  sampler: actuar::rpareto4
  control:
    min: Known finite lower endpoint.
    reltol: Delegate fitting tolerance.
    iterlim: Maximum delegate iterations.
    shape1: Holding this shape is not supported.
    shape2: Holding this shape is not supported.
fit_args: {min: 0}
log_density: log(shape1) + log(shape2) - log((x-min)) - shape2*log(scale/(x-min)) - (shape1+1)*log(1+((x-min)/scale)**shape2)
limits:
- when: $\mathrm{shape1} \to \infty$ with $\mathrm{scale}\,\mathrm{shape1}^{-1/\mathrm{shape2}}$
    fixed
  status: WeibullBoundary
  name: shifted weibull; subtract min before fitting
- when: $\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale
    fixed
  status: ParetoBoundary
  name: shifted pareto; subtract min before fitting
- when: $\mathrm{shape2} \to \infty$ with $\mathrm{shape1}\,\mathrm{shape2}$ and scale
    fixed
  detect: all(x >= scale)
  status: AllGeOne
  fixed: [scale]
  name: shifted Pareto with its endpoint held at min + scale
examples: |
  set.seed(3)
  mlpareto4(simulate_default("mlpareto4", 100), min = 0)
---

This family has a known support endpoint and delegates fitting to burr.

## Details

Identification and sample existence conditions transfer exactly from Burr on x-min. Free-scale likelihoods need the delegate global search and comparison with Weibull and Pareto faces, using the 1e-8*n tie rule. Held-scale fits retain the separate Burr sample conditions; constant observations exactly at min+scale give unbounded likelihood.

The Burr shape1, shape2 and scale coefficients are unchanged.

Observations must be strictly inside the held support. Free endpoints with other parameters held are not supported; those subsets need separate existence studies. The delegate retains its sample restrictions and limit comparisons.

At distance y above the endpoint the density is asymptotic to C y^(a-1), where a is shape2. Choose 0<a<1 and hold other parameters positive. As min approaches the sample minimum from below, k minimum factors diverge as epsilon^(k*(a-1)); all remaining factors approach positive constants. Thus unrestricted likelihood is unbounded, including constant samples.

With scale held, endpoint limit models retain the endpoint min + scale;
a free-endpoint Pareto or power fit would be a different constrained model.
