---
id: foldnorm
name: Folded Normal
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
  - {name: mu, role: parent normal location, domain: "[0, inf)", default: 2, fixable: true}
  - {name: sigma, role: parent normal scale, domain: "(0, inf)", default: 1, fixable: true}
r:
  density: dfoldnorm
  sampler: rfoldnorm
  control:
    mu: Optional fixed nonnegative parent location; NULL estimates it.
    sigma: Optional fixed positive parent scale; NULL estimates it.
    reltol: Positive tolerance for the scalar profile search.
    iterlim: Positive iteration limit for each bracket refinement.
log_density: -log(sigma) - log(2*pi)/2 - (x-mu)**2/(2*sigma**2) + log(1+exp(-2*x*mu/sigma**2))
examples: |
  set.seed(7)
  mlfoldnorm(rfoldnorm(100, mu = 2, sigma = 1))
---

The folded normal distribution is the absolute value of a normal random variable.

## Details

The sign of the parent normal location is unidentifiable, so `mu` is restricted to nonnegative values. The attained member `mu = 0` is the half-normal with endpoint zero, whereas `mlhnorm` estimates its endpoint.

For nonconstant nonnegative data the maximum is attained, including the half-normal boundary. Constant data have an unbounded likelihood when both parameters are estimated. Zeros are allowed and use the continuous endpoint density. Either parameter can be held fixed; with positive scale fixed, constant data are also permitted.

The likelihood can have competing boundary and interior maxima. The fitter compares the boundary with all maxima located by a grid over an analytically bounded one-dimensional profile, followed by bracket refinement. Convex-chord upper bounds check the joint profile search to a likelihood tolerance. This does not assume uniqueness.
