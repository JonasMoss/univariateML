---
id: huber
name: Huber
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
- name: epsilon
  role: shape
  domain: (0, inf)
  simulate: 1.345
  kind: design
  advice: epsilon = 1.345 is a conventional tuning choice; NULL fits epsilon with
    normal and Laplace escapes.
r:
  density: extraDistr::dhuber
  sampler: extraDistr::rhuber
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    epsilon: Positive finite tuning constant (required, e.g. 1.345), or NULL to estimate it.
    gradtol: Tolerance on the mean standardized location and scale scores.
    iterlim: Maximum Newton iterations (and profile iterations if fallback is needed).
log_density: -log(sigma)-log(sqrt(2*pi)*erf(epsilon/sqrt(2))+2*exp(-epsilon**2/2)/epsilon)-Piecewise(((x-mu)**2/(2*sigma**2), Abs((x-mu)/sigma)<=epsilon), (epsilon*Abs((x-mu)/sigma)-epsilon**2/2, True))
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: UnboundedLikelihood
    message: "unbounded"
  - model: norm
    when: '$\epsilon \to \infty$ with epsilon free'
    fixed: []
    status: NormalLimit
  - model: laplace
    when: '$\epsilon \to 0$ with epsilon and sigma free, $b=\sigma/\epsilon$ fixed'
    fixed: []
    status: LaplaceLimit
examples: |
  set.seed(1)
  mlhuber(extraDistr::rhuber(100, epsilon = 1.345), epsilon = 1.345)
holds: []
fit_args:
  epsilon: 1.345
---

The Huber density has a Gaussian core and exponential tails.

## Details

The tuning constant `epsilon` is required. Supply a held value (for example 1.345), or set
`epsilon = NULL` to estimate it. The free shape uses a log-scale grid and
refines every detected profile maximum, comparing the normal and Laplace
faces with tolerance $10^{-8} n$. The Laplace scale is $b=\sigma/\epsilon$;
this face requires free `sigma`. The normal face is eligible with any
held location or scale. Both faces require free `epsilon`. For two distinct
observations the free location-scale-shape MLE does not exist.

The scale is the joint maximum-likelihood scale, not Huber's
Proposal 2 scale. When the maximising location is an interval (the sample
lies in the linear tails), its midpoint is returned. For held `epsilon` and nonconstant data
the maximum is attained, ties included; constant data have an unbounded
likelihood unless `mu` is held at a different value or `sigma` is held.

## Numerical search

For free shape, the grid has log spacing at most 0.125, from -9 to
`log(max(8, 2 * max(abs(x-mu_normal)/sigma_normal)))`, followed by
golden-section refinement of each detected local maximum. This is a
numerical global search, not a uniqueness theorem or an interval certificate.
The lower truncation is below the face comparison tolerance when scale is
free: since $\rho_c(z) \ge c|z|-c^2/2$, the improvement over the fitted
Laplace likelihood is at most $nc^2/2 < 10^{-8}n$ for $c<e^{-9}$.
The same bound raises the lower endpoint when all smaller shapes cannot
improve the current finite incumbent.
Above the upper endpoint the constrained normal fit is inside the quadratic
core and solves the concave inner problem; only the normalizer changes,
monotonically toward the normal face.

With held `sigma`, the lower endpoint is decreased until the uniform
log-density bound $\log f \le \log c-\log(2\sigma)+c^2/2$
is below the incumbent profile likelihood per observation.


Every coefficient can be held together with any others. Held coefficients are preserved in both the finite fit and the eligible normal and Laplace face comparisons.
