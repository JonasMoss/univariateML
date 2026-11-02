---
id: bhatt
inference_numeric:
  - regime: "normal member with width known"
    available: true
    condition: "finite mu and sigma > 0; a = 0 held; mu and/or sigma free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite mu, sigma > 0 and a > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Bhattacharjee
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
- name: a
  symbol: 'a'
  role: half-width
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dbhatt
  sampler: rbhatt
  control:
    mu: Optional held mu; NULL estimates it.
    sigma: Optional positive held sigma; NULL estimates it.
    a: Optional nonnegative held a; NULL estimates it.
    reltol: Relative search tolerance in standardized coordinates.
    iterlim: Maximum refinement iterations per search start.
log_density: log((erf((x-mu+a)/(sqrt(2)*sigma))-erf((x-mu-a)/(sqrt(2)*sigma)))/(4*a))
limits:
  - model: unif
    firmness: proved
    when: 'The normal scale satisfies $\sigma\to0$ with positive half-width $a$ fixed or approaching a positive limit.'
    detect: 'With normal scale free, the compatible uniform-face log likelihood is at least the best numerical interior log likelihood minus $10^{-8}n$. Endpoint penalties depend on held coefficients.'
    status: UniformLimit
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlbhatt())
  mlbhatt(x)

group: normal convolutions
construction:
- compounding
relations:
  convolution_of:
  - family: norm
    operation: sum
    components:
    - family: norm
      at:
        mean: mu
        sd: sigma
    - family: unif
      at:
        min: -a
        max: a
    when: a > 0
fit:
  method: profile-search
  certified: partial
  note: 'bhatt/README.md, Details: fixed-shape location-scale objective is strictly concave; the shape grid and face comparison are numerical.'
areas:
- measurement and errors
classification: uncommon
classification_reason: Introduced for dimensional chains involving rectangular and normal errors; broader adoption
  is not established by the sources checked.
summary: Bhattacharjee provides a model for dimensional tolerance chains combining rectangular and Gaussian errors.
aliases: []
references:
- Bhattacharjee, G. P., Pandit, S. N. N. and Mohan, R. (1963). Dimensional chains involving rectangular and normal
  error-distributions. Technometrics, 5, 404–406.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant samples have unbounded likelihood.
    For nonconstant samples the only unattained nonzero limiting face is
    uniform; the normal case $a=0$ is an attained member. General joint
    uniqueness and a complete data-only existence criterion are not
    established. With positive $a$ held and location free, an MLE exists
    exactly when the sample range exceeds $2a$; otherwise the uniform
    supremum is unattained. Holding $\sigma>0$ guarantees attainment,
    whether location is held or free.
    With both scales held, free location has a unique finite maximum.
    Holding $a=0$ gives the corresponding normal fitting problem.
  reason: >-
    The density is bounded by both $1/(2a)$ for $a>0$ and
    $1/(\sqrt{2\pi}\sigma)$. Diverging scales or location lose likelihood;
    joint scale collapse loses likelihood for nonconstant data. Conditional
    location is strictly log concave. Uniform-face bounds and the endpoint
    penalties below establish the stated held-width criterion.
---

The Bhattacharjee distribution is the convolution of a normal distribution and a symmetric uniform distribution.

The coefficient $\mu$ is the centre, $\sigma$ is the normal standard
deviation and $a$ is the uniform half-width. Holding a coefficient fixes its
physical value, rather than a dimensionless shape ratio.

## Details

When every coefficient is held, the likelihood is evaluated at those values
without optimization.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) describes the limiting paths and the scope of the boundary comparisons.

For fixed dimensionless shape $k=a/\sigma$, the log-likelihood is strictly
concave in $(u,b)=(\mu/\sigma,1/\sigma)$. For nonconstant observations
it has a unique finite maximum in these coordinates; constant observations
collapse when location and scale are free. Holding the physical half-width fixed is a different constraint. This is a mathematical property,
not a certificate of the current search over shape. See the [proof and scope](../../../../../dev/research/evaluations/2026-10-31-logconcave-report.md).

A constant sample has unbounded likelihood when both scales are free and location is free or held at the common value. The normal and uniform faces are fitted conditionally on held scales and compared with the interior within $10^{-8}n$. Holding sigma removes the uniform face; holding positive a removes the normal member. With a held and sample range exactly 2a, endpoint masses contribute an optimized binomial penalty on the uniform face. For fixed positive scales the location likelihood is strictly log concave; the multistart fallback uses a grid of simplex starts and does not certify exclusion of other scale modes.

Identification follows from $E[X]=\mu$, $\kappa_4=-2a^4/15$ and $\operatorname{Var}(X)=\sigma^2+a^2/3$. For $a>0$, the density is bounded above by both $1/(2a)$ and $1/(\sqrt{2\pi}\sigma)$, so either scale tending to infinity forces likelihood to zero, including joint scale divergence. With bounded scales, location escaping to infinity also forces likelihood to zero. Joint scale collapse is unbounded only for constant data; for nonconstant data the normal and uniform faces exhaust the remaining nonzero limits. These are direct arguments using the [extraDistr density definition](https://search.r-project.org/CRAN/refmans/extraDistr/html/Bhattacharjee.html).

The normal member uses the conditional normal maximum, respecting held mu and sigma. The unrestricted uniform face uses the midrange and half-range; it is approached with endpoints expanded by an amount tending to zero more slowly than sigma. With a held and range less than 2a the face likelihood is $(2a)^{-n}$; with range greater than 2a it is zero. At equality, with endpoint multiplicities k and l, multiply $(2a)^{-n}$ by $(k/(k+l))^k(l/(k+l))^l$. This follows by shifting the centre by order sigma and optimizing the two normal endpoint probabilities. A held subset therefore changes the boundary comparison. Held mu conditions both normal and uniform face fits on that location.

With mu free, positive a held and sigma free, range at most 2a always selects the uniform face: each interior density is at most $1/(2a)$, and at exact range 2a the endpoint probabilities are bounded by complementary normal probabilities. These pointwise bounds give precisely the face value above. Range greater than 2a instead guarantees an attained positive-sigma maximum, since both scale endpoints then have zero likelihood. With both scales held, location is always attained, including for constant samples.

The conditional uniform comparison respects held coefficients. The normal boundary is returned as a=0, preserving held mu and sigma.

Evaluation and fitting at $a=0$ give the attained normal member.

The uniform face is always a local maximum of the likelihood closure. If r is the sample half-range, the peak-density bound rules out improvement for $a\ge r$, while the extreme-pair CDF bound rules it out for $r4^{-1/n}\le a<r$. It need not be global: for a sample of twenty zeros and one observation at each of $-10$ and $10$, the fitted normal face has higher likelihood. Thus boundary multimodality is genuine even though location is conditionally unique; an exhaustive classification of multiple finite scale modes is not claimed.

Holding mu conditions both face comparisons on that exact centre. For held positive a, sigma can tend to zero with nonzero likelihood precisely when every distance from the held mu is at most a; observations exactly at an endpoint contribute a factor 1/2 each. Holding sigma removes this uniform escape. The proper boundary a=0 is returned as the attained normal member. Constant samples have unbounded likelihood when normal scale is free, location can coincide with the common observation, and uniform width is free or held at zero. Holding a positive width rules out this divergence.

With $\sigma>0$ held, an MLE is attained for every nonempty finite sample,
with location held or free. The width cannot diverge on a positive likelihood
level set because $f\le1/(2a)$. With bounded width and fixed positive
normal scale, escaping location loses likelihood. The remaining width domain
includes $a=0$, so compactness and continuity give attainment. If location
and positive width are both held instead, a positive-scale MLE exists exactly
when some observed distance from that location exceeds $a$. Otherwise every
finite-scale density is strictly below its uniform-face value, including the
half-density factors at endpoints. Outside that case, both zero and infinite
normal scale lose likelihood, which gives attainment. These arguments do not
prove uniqueness of the fitted scales.

## R notes

The suggested `mlunif` alternative fits unrestricted endpoints and does not
preserve held `mu` or `a`. The normal member is returned with `a = 0`,
preserving held `mu` and `sigma`.

## Decision history

2026-10-31 (fixed-shape profile search):

For a fully free fit, fix the dimensionless shape $k=a/\sigma$ and optimize the
location and inverse scale jointly. The fixed-shape objective is strictly
concave, and the inner solve uses analytic derivatives with damped Newton
steps and a scaled score check. A grid in log shape is traversed outward
with warm starts; local grid maxima are refined and the best candidate is
polished by the existing local optimizer. One central multistart seed is also
polished and retained when its likelihood is at least as high, preserving the
established fit precision. If the grid has no interior local
maximum, the grid-end likelihoods are compared with the normal and uniform faces using
the existing face rules. This outer search and face comparison are numerical;
they do not certify that no mode lies beyond or between grid points. Failed
inner solves, unsuccessful polishing and unresolved face comparisons fall
back to the existing multistart; held-parameter fits retain that path. A test
hook counts fallbacks when enabled. See the [round-four evidence](../../../../../dev/research/evaluations/2026-10-31-logconcave-4-report.md).
