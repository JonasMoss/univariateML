---
id: foldnorm
name: Folded Normal
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: mu
  symbol: '\mu'
  role: parent normal location
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: parent normal scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dfoldnorm
  sampler: rfoldnorm
  control:
    mu: Optional fixed nonnegative parent location; NULL estimates it.
    sigma: Optional fixed positive parent scale; NULL estimates it.
    reltol: Positive tolerance for the scalar profile search.
    iterlim: Positive iteration limit for each bracket refinement.
log_density: -log(sigma) - log(2*pi)/2 - (x-mu)**2/(2*sigma**2) + log(1+exp(-2*x*mu/sigma**2))
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlfoldnorm())
  mlfoldnorm(x)

group: other positive
construction:
- folding
relations:
  transform_of:
  - family: norm
    transform: abs
    at:
      mean: mu
      sd: sigma
fit:
  method: profile-search
  certified: partial
  note: Details records an analytically bounded profile and convex-chord upper bounds to a likelihood
    tolerance; this does not establish uniqueness.
areas:
- reliability and survival
classification: specialist
classification_reason: Used within reliability and survival; classified by its specialised modelling role.
---

The folded normal distribution is the absolute value of a normal random variable.

## Details

The sign of the parent normal location is unidentifiable, so `mu` is restricted to nonnegative values. The attained member `mu = 0` is the half-normal with endpoint zero, whereas `mlhnorm` estimates its endpoint.

For nonconstant nonnegative data the maximum is attained, including the half-normal boundary. Constant data have an unbounded likelihood when both parameters are estimated. Zeros are allowed and use the continuous endpoint density. Either parameter can be held fixed; with positive scale fixed, constant data are also permitted.

The likelihood can have competing boundary and interior maxima. The fitter compares the boundary with all maxima located by a grid over an analytically bounded one-dimensional profile, followed by bracket refinement. Convex-chord upper bounds check the joint profile search to a likelihood tolerance. This does not assume uniqueness.

## d/p/q/r

The sign of mu is unidentifiable; mu = 0 gives the half-normal.
