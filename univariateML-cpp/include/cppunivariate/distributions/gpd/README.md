---
id: gpd
name: Generalized Pareto
support: {type: real, lower: "mu", upper: "if (xi < 0) mu - sigma/xi else Inf", closed: [true, true], endpoint_log_density: {lower: "-log(sigma)", upper: "if (xi == -1) -log(sigma) else if (xi < -1) Inf else -Inf"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: design
  advice: Supply a threshold strictly below the data; estimating it has no unrestricted
    MLE.
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
    lower: -1.0
    restriction:
      mechanism: endpoint
      exempt_when_held: []
      reason: xi >= -1 prevents an upper-endpoint singularity.
  kind: regular
r:
  density: dgpd
  sampler: rgpd
  control:
    mu: Threshold, held fixed; every observation must be at or above it.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    xi: Shape to hold fixed, or NULL to estimate (lower bound -1).
    reltol: Relative tolerance for scale bisection and transformed profile root brackets, with a unit absolute floor for the latter.
    iterlim: Maximum number of bisection iterations per root.
log_density: Piecewise((-log(sigma)-(x-mu)/sigma, Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma), True))
limits:
  - {name: threshold scale collapse, fixed: [mu], status: InvalidData, message: "scale collapse at the threshold", when: "With free scale, all data equal mu, or threshold ties permit xi*k >= n-k (free xi always permits this path)."}
references:
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics,
  35(2), 185–191. doi:10.1080/00401706.1993.10485040.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds:
- held: []
  status: no-mle
  reason: 'Estimating `mu` is not supported: the free-threshold GPD likelihood has
    no MLE (endpoint and logistic escapes).'
- held:
  - xi
  status: no-mle
  reason: 'Estimating `mu` is not supported: the free-threshold GPD likelihood has
    no MLE (endpoint and logistic escapes).'
- held: [sigma]
  status: no-mle
  reason: 'Estimating `mu` is not supported: the free-threshold GPD likelihood has no MLE (endpoint and logistic escapes).'
- held: [sigma, xi]
  status: no-mle
  reason: 'Estimating `mu` is not supported: the free-threshold GPD likelihood has no MLE (endpoint and logistic escapes).'
fit_args:
  mu: 0.0
examples: |
  x <- datasets::precip[datasets::precip > 40] - 40
  mlgpd(x, mu = 0)

group: extreme value
construction:
- base
- generalisation
relations: {}
fit:
  method: multistart
  certified: partial
  note: 'gpd/README.md, Details; gpd/mle.hpp (public fitter and held overloads); gpd/test.cpp: Exponential shape-zero
    and fixed-shape endpoint members have exact subsets; general scale/shape profile search is numerical. The required
    threshold is held.'
areas:
- extremes and environment
classification: specialist
classification_reason: Used to model threshold exceedances in extreme-value analysis.
summary: Generalized Pareto provides a model for threshold exceedances in extreme-value analysis.
aliases:
- "GPD"
key_references:
- 2
---

The generalized Pareto distribution models exceedances above a user-chosen
threshold `mu` (required); positive `xi` gives a heavy right tail and
`xi = 0` is the exponential member with scale `sigma`. Observations must
lie at or above `mu` and, for negative shape, at or below `mu - sigma/xi`
when the density limit there is positive. Threshold ties with free shape
and scale permit scale collapse and stop.

## Details

The shape is restricted to `xi >= -1` because the unrestricted likelihood is unbounded as an upper endpoint approaches the sample maximum at `xi < -1`.
Data from a model with `xi < -1` give constrained fits, often at the floor `xi = -1`.

With held threshold and shape greater than -1, scale has a unique positive
score root, including for constant samples and one observation. Holding
scale instead leaves a scalar shape likelihood on the support-feasible
interval above -1; the attained floor is compared with interior maxima.
All three coefficients can be held; incompatible support is rejected.
Estimating the threshold remains unsupported under A4, including with
held scale, under the agreed A4 policy (some held-scale uniform plateaus
can exist, but estimating the threshold remains outside this API).
The scalar search enumerates sampled maxima and refines their brackets;
its global coverage is checked numerically, not certified.

The fitting restriction is for existence. Fits with `xi` in `[-1, -1/2)`
are non-regular. At `xi = -1` the likelihood uses the closed upper endpoint
(R7a): the uniform GPD or reversed-exponential GEV is attained.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.
