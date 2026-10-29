---
id: gev
name: Generalized extreme value
support: {type: real, lower: "if (xi > 0) mu - sigma/xi else -Inf", upper: "if (xi < 0) mu - sigma/xi else Inf", closed: [true, true], endpoint_log_density: {lower: "-Inf", upper: "if (xi == -1) -log(sigma) else if (xi < -1) Inf else -Inf"}}
params:
- name: mu
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: xi
  role: shape
  domain: '[-0.5, 1]'
  simulate: 0.3
  fit_domain:
  - lower: -1.0
    restriction:
      mechanism: endpoint
      exempt_when_held: []
      reason: xi >= -1 prevents an upper-endpoint singularity; the current fitter
        still uses -1/2 until P3.
  - upper: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held: []
      reason: xi <= 1 prevents collapse, including with mu or sigma held (P0 extreme).
  kind: regular
r:
  density: extraDistr::dgev
  sampler: extraDistr::rgev
  control:
    mu: Location to hold fixed, or NULL to estimate.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    xi: Shape to hold fixed, or NULL to estimate within [-1/2, 1]. Held shapes above one are accepted with location or scale held when the collapse criterion permits.
    reltol: Tolerance for dimensionless profile scores and root brackets.
    iterlim: Maximum iterations per scalar solve.
log_density: Piecewise((-log(sigma)-(x-mu)/sigma-exp(-(x-mu)/sigma), Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma)-(1+xi*(x-mu)/sigma)**(-1/xi), True))
limits:
- {name: held-location collapse, fixed: [mu], status: NoMLE, message: "minimum multiplicity permits scale collapse", when: "Held mu equals the minimum and the scale-collapse face wins or diverges."}
examples: |
  set.seed(1)
  mlgev(extraDistr::rgev(100, mu = 0, sigma = 2, xi = 0.3))
---

The generalized extreme value distribution models maxima; positive `xi` gives a heavy right tail, and `xi = 0` is the included Gumbel member.

## Details

For positive held shape, collapse is governed by the multiplicity of the sample minimum; ties at other values do not cause this collapse.

`extraDistr::qgev`, `rgev` and the upper-tail log of `pgev` are repaired at runtime; each repair switches itself off once the reference is fixed. Fitting and `logLik` do not use them.

Free-shape fitting currently restricts xi to [-1/2, 1]; changing the lower
bound and endpoint closure is reserved for P3. The default fit requires
three finite nonconstant observations. Only ties at the minimum enter the
collapse test. With held positive xi and free location and scale, a minimum
repeated k times requires xi*k < n-k. With mu held at the minimum and
scale free the same condition applies (with free xi, majority minimum ties diverge; half-minimum ties require
comparison with the finite collapse face). Other held-location or held-scale fits
permit small and constant samples when their likelihood has a finite
maximum; incompatible support is rejected. Held coefficients are exact.

With fixed shape, held scale leaves a scalar location fit (the Gumbel
location has an exact solution). Held location leaves a scalar scale fit
above its support bound. With shape free, the shape profile compares
interior maxima and both attained shape bounds. The implementation scans
and refines scalar brackets after standardization; global discovery is
checked against independent multistart density fits rather than proved.

With location or scale held, a held xi above one is accepted whenever
the stated collapse test permits it; the current lower bound remains -1/2.
