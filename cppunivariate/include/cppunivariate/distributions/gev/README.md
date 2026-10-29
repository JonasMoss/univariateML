---
id: gev
name: Generalized extreme value
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
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
    reltol: Tolerance for dimensionless profile scores and root brackets.
    iterlim: Maximum iterations per scalar solve.
log_density: Piecewise((-log(sigma)-(x-mu)/sigma-exp(-(x-mu)/sigma), Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma)-(1+xi*(x-mu)/sigma)**(-1/xi), True))
examples: |
  set.seed(1)
  mlgev(extraDistr::rgev(100, mu = 0, sigma = 2, xi = 0.3))
holds:
- held:
  - mu
  status: not-implemented
  reason: The adapter does not accept `mu`; this hold is not implemented (P2).
- held:
  - sigma
  status: not-implemented
  reason: The adapter does not accept `sigma`; this hold is not implemented (P2).
---

The generalized extreme value distribution models maxima; positive `xi` gives a heavy right tail, and `xi = 0` is the included Gumbel member.

## Details

`extraDistr::qgev`, `rgev` and the upper-tail log of `pgev` are repaired at runtime; each repair switches itself off once the reference is fixed. Fitting and `logLik` do not use them.

The fitted family restricts `xi` to `[-1/2, 1]` and requires at least three finite observations with every tie multiplicity strictly less than half the sample size; these restrictions ensure an attained maximum, including when `xi` is fixed.
The unrestricted likelihood diverges for shapes below `-1` as the upper endpoint approaches the maximum, and for sufficiently large positive shapes as scale collapses at the minimum, so guarding ties alone does not suffice.
Fits at either shape bound are returned as attained members; the lower bound does not carry the usual regular likelihood asymptotics, and only `xi` may be held fixed.
