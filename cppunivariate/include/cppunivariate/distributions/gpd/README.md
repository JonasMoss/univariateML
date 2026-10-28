---
id: gpd
name: Generalized Pareto
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0, fixed: 0}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1}
  - {name: xi, role: shape, domain: "[-0.5, inf)", default: 0.3, fixable: true}
r:
  density: extraDistr::dgpd
  sampler: extraDistr::rgpd
  control:
    mu: Threshold, held fixed; every observation must exceed it.
    reltol: Relative tolerance for scale bisection and transformed profile root brackets, with a unit absolute floor for the latter.
    iterlim: Maximum number of bisection iterations per root.
log_density: Piecewise((-log(sigma)-(x-mu)/sigma, Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma), True))
references:
  - "Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics, 35(2), 185–191. doi:10.1080/00401706.1993.10485040."
examples: |
  set.seed(1)
  mlgpd(extraDistr::rgpd(100, mu = 10, sigma = 2, xi = 0.3), mu = 10)
---

The generalized Pareto distribution models exceedances above a user-chosen
threshold `mu` (default 0); positive `xi` gives a heavy right tail and
`xi = 0` is the exponential member with scale `sigma`. Observations must
exceed `mu` and, for negative shape, lie below `mu - sigma/xi`.

## Details

The shape is restricted to `xi >= -1/2` because the unrestricted likelihood is unbounded as an upper endpoint approaches the sample maximum at `xi < -1`.
Data from a model with `xi < -1/2` give constrained fits, often at the floor `xi = -1/2`, where the usual likelihood asymptotics do not apply. `extraDistr::qgpd` and `pgpd` lose accuracy for shapes within about `1e-12` of zero (see `KNOWN_ISSUES.md`); the fit is unaffected.
