---
id: huber
inference_numeric:
  - regime: unreviewed or unavailable
    available: false
    condition: "joint kink-moving parameters"
    reason: "the piecewise quadratic log density has a moving second-derivative kink, so the joint pointwise C2 condition fails"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Huber
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
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
- name: epsilon
  symbol: '\epsilon'
  role: shape
  domain: (0, inf)
  simulate: 1.345
  kind: design
  advice: Hold epsilon at a positive tuning value, such as epsilon = 1.345. Set epsilon = NULL to estimate it, with possible normal or Laplace limit diagnostics.
r:
  density: dhuber
  sampler: rhuber
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    epsilon: Positive finite tuning constant (required, e.g. 1.345), or NULL to estimate it.
    gradtol: Tolerance on the mean standardized location and scale scores.
    iterlim: Maximum Newton iterations (and profile iterations if fallback is needed).
log_density: -log(sigma)-log(sqrt(2*pi)*erf(epsilon/sqrt(2))+2*exp(-epsilon**2/2)/epsilon)-Piecewise(((x-mu)**2/(2*sigma**2), Abs((x-mu)/sigma)<=epsilon), (epsilon*Abs((x-mu)/sigma)-epsilon**2/2, True))
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale $\sigma$ tends to zero at the fitted or held location.
    detect: Scale is free and every observation equals the fitted or held location.
    status: UnboundedLikelihood
    message: "unbounded"
  - model: norm
    firmness: proved
    when: 'The free shape satisfies $\epsilon\to\infty$, leaving the normal law at the eligible fitted or held location and scale.'
    fixed: []
    status: NormalLimit
    numerical: true
  - model: laplace
    firmness: proved
    when: 'The free shape and scale tend to zero with $b=\sigma/\epsilon$ fixed, giving a Laplace law.'
    fixed: []
    status: LaplaceLimit
    numerical: true
holds: []
fit_args:
  epsilon: 1.345
examples: |
  set.seed(1)
  x <- rml(200, mlhuber())
  mlhuber(x, epsilon = 1.345)

group: skewed and robust location-scale
construction:
- location-scale
relations: {}
fit:
  method: profile-search
  certified: partial
  note: 'huber/README.md, Details; huber/mle.hpp (public fitter and held overloads); huber/test.cpp: Held epsilon
    gives a concave location/inverse-scale fit; free epsilon is searched numerically with normal and Laplace comparisons.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model robust errors with a Gaussian centre and exponential tails.
summary: Huber provides a model for robust errors with a Gaussian centre and exponential tails.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With location, scale and shape free, a finite MLE need not exist: the supremum can lie at a normal or Laplace limit, and constant data permit unbounded scale collapse. No general uniqueness theorem is asserted. With shape held, a finite maximum is attained for nonconstant data, including ties. For constant data it is attained when scale is held or location is held away from the observation. With shape free, held location and scale are respected in the eligible limit comparisons; the general conditional classification remains numerical.
  reason: >-
    For fixed shape, write inverse scale $\eta=1/\sigma$ and scaled location $\nu=\mu/\sigma$. The log-likelihood is $n\log\eta-\sum_i\rho_\epsilon(\eta x_i-\nu)$ plus a constant, with convex Huber loss $\rho_\epsilon$. It is concave, but linear tails can give an interval of maximizing locations. Coercivity gives attainment except for the stated zero-residual collapse. Free shape is searched numerically and compared with the two limit likelihoods.
---

The Huber density models symmetric measurements with a Gaussian core and exponential tails. Location $\mu$ sets the centre, scale $\sigma$ sets the residual unit, and shape $\epsilon$ sets the transition from quadratic to linear log-density loss.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives these boundary conditions, including the relevant held parameters.

Shape may be held as a tuning constant or estimated. The free shape uses a log-scale grid and
refines every detected profile maximum, comparing the normal and Laplace
faces with tolerance $10^{-8} n$. The Laplace scale is $b=\sigma/\epsilon$;
this face requires free $\sigma$. The normal face is eligible with any
held location or scale. Both faces require free $\epsilon$. For two distinct
observations the free location-scale-shape MLE does not exist. The strict comparison with both limits is derived in the [free-shape analysis](../../../../../dev/research/evaluations/2026-10-29-huber-free-epsilon-report.md).

The scale is the joint maximum-likelihood scale, not Huber's
Proposal 2 scale. When the maximising location is an interval (the sample
lies in the linear tails), its midpoint is returned. For held $\epsilon$ and nonconstant data
the maximum is attained, ties included; constant data have an unbounded
likelihood unless $\mu$ is held at a different value or $\sigma$ is held.

## Numerical search

For free shape, the grid has log spacing at most 0.125, from -9 to
$\log\max\{8,2\max_i |x_i-\mu_N|/\sigma_N\}$, where $(\mu_N,\sigma_N)$ is the normal fit respecting the held coefficients, followed by
golden-section refinement of each detected local maximum. This is a
numerical global search, not a uniqueness theorem or an interval certificate.
Write $c=\epsilon$. The lower truncation is below the face comparison tolerance when scale is
free: since $\rho_c(z) \ge c|z|-c^2/2$, the improvement over the fitted
Laplace likelihood is at most $nc^2/2 < 10^{-8}n$ for $c<e^{-9}$.
The same bound raises the lower endpoint when all smaller shapes cannot
improve the current finite incumbent.
Above the upper endpoint the constrained normal fit is inside the quadratic
core and solves the concave inner problem; only the normalizer changes,
monotonically toward the normal face.

With held $\sigma$, the lower endpoint is decreased until the uniform
log-density bound $\log f \le \log c-\log(2\sigma)+c^2/2$
is below the incumbent profile likelihood per observation.


Every coefficient can be held together with any others. Held coefficients are preserved in both the finite fit and the eligible normal and Laplace face comparisons.

The location fallback must pass a mean standardized score check; holding scale does not waive convergence checks.

## R notes

Supply `epsilon = 1.345` for a conventional held tuning value, or `epsilon = NULL` to estimate shape. A selected normal or Laplace limit stops with advice to use `mlnorm` or `mllaplace`, respectively. Face comparisons use a tolerance of $10^{-8}n$, so a limit diagnostic can also occur when a finite maximizer improves on that limit by less than the tolerance. A failed location fallback stops unless its mean standardized score is below `max(gradtol, 1e-7)`, even with scale held.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).

When location and scale are both held, the Gaussian-face comparison instead
uses the direct gain
$ n\{\log Z(c)-\tfrac12\log(2\pi)\}-\tfrac12\sum_i\max(|(x_i-\mu)/\sigma|-c,0)^2 $.
This follows from $\rho_c(z)-z^2/2=-\max(|z|-c,0)^2/2$ in exact arithmetic;
it cancels the common quadratic before rounding. The comparison allowance
also includes the absolute terms in this gain and a term proportional to $n$
for the normalizer subtraction. Other held subsets retain full-objective
comparisons with the rounding guard.

For both coordinates held, a finite free-shape result also passes the total
log-shape score gate $|2n\exp(-c^2/2)/(cZ(c))-c\sum_{|z_i|>c}(|z_i|-c)|\le10^{-3}$,
where $z_i=(x_i-\mu)/\sigma$. A nonfinite or larger residual returns
`IterationFailure`. This detects a missed finite peak; stationarity alone
is not a global-optimality proof.
