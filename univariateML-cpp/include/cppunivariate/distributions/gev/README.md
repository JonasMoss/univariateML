---
id: gev
inference_numeric:
  - regime: "gumbel member with shape held"
    available: true
    condition: "finite mu and sigma > 0; xi = 0 held; mu and/or sigma free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels"
  - regime: unreviewed or unavailable
    available: false
    condition: "joint moving-support regime with xi free or nonzero"
    reason: "the moving-support C2 and fitted-subset classification for this numerical procedure is not yet completed"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
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
      reason: For xi below -1 the density is unbounded at the upper end of its support, so placing that end at the sample maximum makes the likelihood unbounded.
  - upper: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held: []
      reason: Without an upper bound on xi, shrinking the scale while the lower end of the support moves to the sample minimum makes the likelihood unbounded for every sample, also with mu or sigma held. The bound xi <= 1 prevents this whenever fewer than half of the observations are tied at the minimum.
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
- {firmness: numerical, name: scale collapse, fixed: [mu], status: NoMLE, message: "minimum multiplicity permits scale collapse", when: '$\sigma \to 0$ with $\mu$ held at the sample minimum', detect: '$\mu$ is held at the sample minimum and at least half of the observations are tied there (with $\xi > 0$ held, already when $\xi k \ge n - k$ for $k$ ties)', numerical: true}
- {firmness: numerical, name: scale collapse, status: NoMLE, message: "minimum multiplicity permits scale collapse", when: '$\xi \to 1$ and $\sigma \to 0$, with the lower end of the support at the sample minimum', detect: 'at least half of the observations equal the sample minimum and, at exactly half, no interior fit beats this limit', numerical: true}
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
  method: profile-search
  certified: 'partial'
  note: "gev/mle.hpp, gev_held_profile and gev_profile: Held xi=-1 has closed-form shifted reverse-exponential fits; held xi=0 and sigma give analytic location. These nontrivial submodels are certified. General free and conditional fits use scalar profiles, finite scans and root discovery without a global enumeration certificate; half-tie face comparisons remain numerical."
areas:
- extremes and environment
existence:
  uniqueness: unknown
  statement: >-
    Without restrictions the likelihood is unbounded for every sample, so
    fits keep the shape in $-1 \le \xi \le 1$. Within that range the only
    known obstacle is a tie at the sample minimum. If more than half of the
    observations equal the minimum, no MLE exists. If exactly half do, the
    supremum is approached as $\xi \to 1$ and the scale shrinks, unless an
    interior fit does better.
  reason: >-
    Near an end point of the support the density behaves like a power of the
    distance to it. For $\xi < -1$ that power is negative at the upper end,
    so moving the end point onto the sample maximum makes the likelihood
    unbounded. At the lower end, shrinking the scale onto a
    minimum tied $k$ times makes the likelihood behave like
    $\sigma^{(n-k)/\xi - k}$, which is unbounded when $\xi k > n - k$; for
    $\xi \le 1$ this needs $k > n/2$, and $k = n/2$ at $\xi = 1$ is the
    boundary case.
---

The generalized extreme value distribution models maxima. A positive shape $\xi$ gives a heavy right tail, a negative one a finite upper end point, and $\xi = 0$ is the Gumbel distribution.

## Details

For the held-location collapse, put $\mu=m=\min x_i$, fix $\xi>0$
and let $\sigma\downarrow0$. Each of the $k$ minimum terms is
$e^{-1}/\sigma$; for $d_i=x_i-m>0$ each other term is asymptotic to
$\xi^{-1-1/\xi}d_i^{-1-1/\xi}\sigma^{1/\xi}$. Hence the product is
asymptotic to a positive constant times
$\sigma^{(n-k)/\xi-k}$. Equality has a finite positive path limit,
not a proof that the global likelihood is maximized there. With location
free and $\xi=1$, set $\mu=m+\sigma/2$; the minimum terms become
$4e^{-2}/\sigma$ and the remaining terms are asymptotic to
$\sigma/d_i^2$, again giving a finite limit at $k=n/2$. Optimization
over all finite parameters and all joint paths is a separate open step.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-extreme.md) describes the limiting paths and the scope of the boundary comparisons.

Fits restrict the shape to $-1 \le \xi \le 1$, because outside that range the likelihood can be made unbounded: at the upper end of the support for $\xi < -1$, and at a tied sample minimum for large $\xi$. At $\xi = -1$ the upper end point belongs to the support, so the boundary fit, the reversed exponential distribution, is a proper member of the family. Fits with $-1 \le \xi < -1/2$ are non-regular: the usual asymptotic theory for the MLE does not apply there.

Only ties at the sample minimum matter. Let $k$ observations equal the minimum. With $\xi > 0$ held and $\mu$ and $\sigma$ free, the likelihood is bounded only if $\xi k < n - k$. With all three coefficients free, more than $n/2$ ties always make it unbounded. At exactly $k = n/2$ the collapse path has a finite limit; the fit compares the best interior fit with that limit and stops unless the interior fit is strictly better. The path alone does not establish the global supremum or its attainment. In every sample tried so far the limit has won, but no proof covers all samples. The same conditions apply with $\mu$ held at the sample minimum and $\sigma$ free.

Held shapes above 1 are accepted when the tie condition allows them. A held shape below $-1$ is accepted only when $\mu$ is held at or above the sample maximum, or when every coefficient is held.

With the shape held, holding the scale leaves a one-dimensional fit for the location (exact for the Gumbel case), and holding the location leaves one for the scale. With the shape free, the fit profiles over $\xi$ and compares interior maxima with both shape bounds. The search is checked against independent multistart fits; it is not certified. The default fit needs three finite, non-constant observations.

## R notes

Estimated coefficients on a `fit_domain` bound carry `restricted` and emit a warning of class `univariateML_restricted`; held coefficients are not flagged. Failed optimizers return a failure status unless the score norm in fitting coordinates is below `1e-3`; unresolved scalar searches stop.
