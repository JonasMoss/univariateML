---
id: huber
name: Huber
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mu, role: location, domain: "(-inf, inf)", default: 0, fixable: true}
  - {name: sigma, role: scale, domain: "(0, inf)", default: 1, fixable: true}
  - {name: epsilon, role: shape, domain: "(0, inf)", default: 1.345, fixed: 1.345}
r:
  density: extraDistr::dhuber
  sampler: extraDistr::rhuber
  control:
    epsilon: Positive finite tuning constant, held fixed.
    gradtol: Tolerance on the mean standardized location and scale scores.
    iterlim: Maximum Newton iterations (and profile iterations if fallback is needed).
log_density: -log(sigma)-log(sqrt(2*pi)*erf(epsilon/sqrt(2))+2*exp(-epsilon**2/2)/epsilon)-Piecewise(((x-mu)**2/(2*sigma**2), Abs((x-mu)/sigma)<=epsilon), (epsilon*Abs((x-mu)/sigma)-epsilon**2/2, True))
examples: |
  set.seed(1)
  mlhuber(extraDistr::rhuber(100, epsilon = 1.345))
---

The Huber density has a Gaussian core and exponential tails.

## Details

The tuning constant `epsilon` is held fixed (default 1.345) and is not
estimated. The scale is the joint maximum-likelihood scale, not Huber's
Proposal 2 scale. When the maximising location is an interval (the sample
lies in the linear tails), its midpoint is returned. For nonconstant data
the maximum is attained, ties included; constant data have an unbounded
likelihood unless `mu` is held at a different value or `sigma` is held.
