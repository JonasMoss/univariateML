---
id: gev
name: Generalized extreme value
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1}
  - {name: xi, role: shape, domain: "[-0.5, 1]", default: 0.3, fixable: true}
r:
  density: extraDistr::dgev
  sampler: extraDistr::rgev
  control:
    reltol: Tolerance for dimensionless profile scores and root brackets.
    iterlim: Maximum iterations per scalar solve.
log_density: Piecewise((-log(sigma)-(x-mu)/sigma-exp(-(x-mu)/sigma), Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma)-(1+xi*(x-mu)/sigma)**(-1/xi), True))
examples: |
  set.seed(1)
  mlgev(extraDistr::rgev(100, mu = 0, sigma = 2, xi = 0.3))
---

The generalized extreme value distribution models maxima; positive `xi` gives a heavy right tail, and `xi = 0` is the included Gumbel member.

## Details

Reference functions extraDistr::qgev, rgev and the upper-tail log of pgev are repaired at runtime; each repair turns off when the reference is fixed.

The fitted family restricts `xi` to `[-1/2, 1]` and requires at least three finite observations with every tie multiplicity strictly less than half the sample size; these restrictions ensure an attained maximum, including when `xi` is fixed.
The unrestricted likelihood diverges for shapes below `-1` as the upper endpoint approaches the maximum, and for sufficiently large positive shapes as scale collapses at the minimum, so guarding ties alone does not suffice.
Fits at either shape bound are returned as attained members; the lower bound does not carry the usual regular likelihood asymptotics, and only `xi` may be held fixed.
