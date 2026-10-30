---
id: snorm
name: Skew Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: xi
  role: skew
  domain: (0, inf)
  simulate: 3.0
  kind: regular
r:
  density: dsnorm
  sampler: rsnorm
  control:
    mean: "Optional held mean; NULL estimates it where the hold subset is supported."
    sd: "Optional held sd; NULL estimates it where the hold subset is supported."
    xi: "Optional held xi; NULL estimates it where the hold subset is supported."
log_density: log(sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)) - log(sd) + log(2/(xi+1/xi)) - log(2*pi)/2 - Piecewise(((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2/xi**2, (sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))>0), ((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2*xi**2, True))/2
limits:
  - {name: scale collapse, when: "free scale with constant data at the free or held mean", status: Unbounded, message: "Scale collapse"}
  - model: hnorm
    when: '$\xi \to \infty$'
    status: 'RightHalfNormalBoundary'
  - name: 'reflected translated half-normal'
    when: '$\xi \to 0$'
    status: 'LeftHalfNormalBoundary'
references:
  - "Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American Statistical Association, 93(441), 359–371."
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlsnorm())
  mlsnorm(x)
---

The Fernandez--Steel skew-normal distribution introduces skewness by giving the two sides of a normal density different scales.

## Details

This is the two-piece normal family, rather than the Azzalini skew-normal family. The coefficients `mean` and `sd` are its mean and standard deviation; `xi = 1` gives the normal distribution. The translated half-normal boundary has its mode at the sample minimum or maximum.

Use `mlhnorm(x, mu = NULL)` for the right boundary and `mlhnorm(-x, mu = NULL)` for the left boundary, reflecting the fitted half-normal back at the sample maximum.


Any subset of `mean`, `sd`, and `xi` can be held. With skewness held,
location and inverse scale have a concave likelihood. With skewness free,
both half-normal faces are compared under the held moment constraints.
The conditional search uses nested one-dimensional minimizations and scans
log skewness; this numerical search is not a global certificate. Constant
data at a held mean have unbounded likelihood when scale is free.

With both mean and sd held, an observation at the limiting half-normal
endpoint retains a finite residual on the shrinking side. Each such
observation contributes the penalty $m^2/(8h^4)$ to the limiting log
likelihood, where $m=\sqrt{2/\pi}$ and $h=\sqrt{1-2/\pi}$.
When either moment is free its vanishing adjustment removes this penalty.

A conditional search that reaches a finite numerical tail guard without a
winning analytic face stops with a convergence reason. A guard value is
never returned as an estimated coefficient.

## d/p/q/r

Uses the fGarch mean and standard deviation parameterisation.
