---
id: gev
name: Generalized extreme value
support: {type: real, lower: "if (xi > 0) mu - sigma/xi else -Inf", upper: "if (xi < 0) mu - sigma/xi else Inf", closed: [true, true], endpoint_log_density: {lower: "-Inf", upper: "if (xi == -1) -log(sigma) else if (xi < -1) Inf else -Inf"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: xi
  symbol: '\xi'
  role: shape
  domain: '(-inf, inf)'
  simulate: 0
  fit_domain:
  - lower: -1.0
    restriction:
      mechanism: endpoint
      exempt_when_held: []
      reason: xi >= -1 prevents an upper-endpoint singularity.
  - upper: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held: []
      reason: xi <= 1 prevents collapse, including with mu or sigma held (P0 extreme).
  kind: regular
r:
  density: dgev
  sampler: rgev
  control:
    mu: Location to hold fixed, or NULL to estimate.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    xi: Shape to hold fixed, or NULL to estimate within `[-1, 1]`. Held shapes above one are accepted when the collapse criterion permits.
    reltol: Tolerance for dimensionless profile scores and root brackets.
    iterlim: Maximum iterations per scalar solve.
log_density: Piecewise((-log(sigma)-(x-mu)/sigma-exp(-(x-mu)/sigma), Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma)-(1+xi*(x-mu)/sigma)**(-1/xi), True))
limits:
- {name: held-location collapse, fixed: [mu], status: NoMLE, message: "minimum multiplicity permits scale collapse", when: "Held mu equals the minimum and the scale-collapse face wins or diverges."}
- {name: free collapse, status: NoMLE, message: "minimum multiplicity permits scale collapse", when: "The sample minimum's multiplicity equals n/2 exactly (even n only) and no interior (mu, sigma, xi) point beats the xi -> 1 collapse face's limiting likelihood."}
examples: |
  set.seed(1)
  x <- rml(200, mlgev())
  mlgev(x)

summary: The generalized extreme-value distribution unifies the limiting laws for normalized block maxima.
classification: specialist
classification_reason: Used to model block maxima spanning the three extreme-value tail regimes.
aliases:
- "GEV"
- "Fisher–Tippett"
key_references:
- 1
references:
  - "Coles, S. (2001). An Introduction to Statistical Modeling of Extreme Values. Springer. doi:10.1007/978-1-4471-3675-0."

group: extreme value
construction:
- base
- generalisation
relations: {}
fit:
  method: multistart
  certified: 'no'
  note: 'gev/README.md, Details; gev/mle.hpp (public fitter and held overloads); gev/test.cpp: Default and nontrivial
    conditional fits use multistart, finite grids or local profile refinement without a global certificate; all-held
    density evaluation is not counted as certification.'
areas:
- extremes and environment
---

The generalized extreme value distribution models maxima; positive `xi` gives a heavy right tail, and `xi = 0` is the included Gumbel member.

## Details

For positive held shape, collapse is governed by the multiplicity of the sample minimum; ties at other values do not cause this collapse.

Free-shape fitting restricts xi to `[-1, 1]`, with the attained closed
reversed-exponential member at xi=-1. The default fit requires
three finite nonconstant observations. Only ties at the minimum enter the
collapse test. With held positive xi and free location and scale, a minimum
repeated k times requires xi*k < n-k. With mu held at the minimum and
scale free the same condition applies (with free xi, majority minimum ties diverge; half-minimum ties require
comparison with the finite collapse face). Other held-location or held-scale fits
permit small and constant samples when their likelihood has a finite
maximum; incompatible support is rejected. Held coefficients are exact.

With location, scale and shape all free, a minimum repeated more than
n/2 times always diverges. At exactly n/2 (even n only), the xi -> 1
collapse face has a finite but unattained supremum (free mu and sigma
both move toward the degenerate limit); the fit compares the best
interior candidate found by the ordinary shape-profile search against
that closed-form limit and returns the interior fit when it is
strictly higher, or stops with the same "scale collapse" reason
otherwise. Fewer ties than n/2 are unaffected. In extensive testing, no
interior candidate ever beat the limit at an exact n/2 tie, so this
remains a "stops with the reason" case in every sample tried; no proof
that this holds for every sample is claimed.

With fixed shape, held scale leaves a scalar location fit (the Gumbel
location has an exact solution). Held location leaves a scalar scale fit
above its support bound. With shape free, the shape profile compares
interior maxima and both attained shape bounds. The implementation scans
and refines scalar brackets after standardization; global discovery is
checked against independent multistart density fits rather than proved.

A held xi above one is accepted whenever the stated collapse test permits
it. Held xi below -1 is accepted for fully held evaluation, or with mu held
at or above the sample maximum; other subsets have endpoint divergence.

The fitting restriction is for existence. Fits with `xi` in `[-1, -1/2)`
are non-regular. At `xi = -1` the likelihood uses the closed upper endpoint
(R7a): the uniform GPD or reversed-exponential GEV is attained.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.
