---
id: bhatt
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
    when: '$\sigma \to 0$'
    detect: conditional uniform face within 1e-8*n of the best interior likelihood
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
  method: multistart
  certified: partial
  note: 'bhatt/README.md, Details; bhatt/mle.hpp (public fitter and held overloads); bhatt/test.cpp: Location is
    strictly concave with scales held; joint scales use multistart/grid optimization without a global certificate.'
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
---

The Bhattacharjee distribution is the convolution of a normal distribution and a symmetric uniform distribution.

## Details

For fixed dimensionless shape $k=a/\sigma$, the log-likelihood is strictly
concave in $(u,b)=(\mu/\sigma,1/\sigma)$. For nonconstant observations
it has a unique finite maximum in these coordinates; constant observations
collapse when location and scale are free. Holding the physical half-width fixed is a different constraint. This is a mathematical property,
not a certificate of the current search over shape. See the [proof and scope](../../../../../dev/research/evaluations/2026-10-31-logconcave-report.md).

A constant sample has unbounded likelihood when both scales are free. The normal and uniform faces are fitted conditionally on held scales and compared with the interior within $10^{-8}n$. Holding sigma removes the uniform face; holding a removes the normal face. With a held and sample range exactly 2a, endpoint masses contribute an optimized binomial penalty on the uniform face. For fixed positive scales the location likelihood is strictly log concave; scale optimization uses a grid of simplex starts and is a numerical global search, without a certificate excluding other scale modes.

Identification follows from $E[X]=\mu$, $\kappa_4=-2a^4/15$ and $\operatorname{Var}(X)=\sigma^2+a^2/3$. The density is bounded above by both $1/(2a)$ and $1/(\sqrt{2\pi}\sigma)$, so either scale tending to infinity forces likelihood to zero, including joint scale divergence. With bounded scales, location escaping to infinity also forces likelihood to zero. Joint scale collapse is unbounded only for constant data; for nonconstant data the normal and uniform faces exhaust the remaining nonzero limits. These are direct arguments using the [extraDistr density definition](https://search.r-project.org/CRAN/refmans/extraDistr/html/Bhattacharjee.html).

The normal member uses the conditional normal maximum, respecting held mu and sigma. The unrestricted uniform face uses the midrange and half-range; it is approached with endpoints expanded by an amount tending to zero more slowly than sigma. With a held and range less than 2a the face likelihood is $(2a)^{-n}$; with range greater than 2a it is zero. At equality, with endpoint multiplicities k and l, multiply $(2a)^{-n}$ by $(k/(k+l))^k(l/(k+l))^l$. This follows by shifting the centre by order sigma and optimizing the two normal endpoint probabilities. A held subset therefore changes the boundary comparison. Held mu conditions both normal and uniform face fits on that location.

With mu free, positive a held and sigma free, range at most 2a always selects the uniform face: each interior density is at most $1/(2a)$, and at exact range 2a the endpoint probabilities are bounded by complementary normal probabilities. These pointwise bounds give precisely the face value above. Range greater than 2a instead guarantees an attained positive-sigma maximum, since both scale endpoints then have zero likelihood. With both scales held, location is always attained, including for constant samples.

The conditional uniform comparison respects held coefficients. The suggested `mlunif` alternative fits unrestricted endpoints; it does not preserve held mu or a. The normal boundary is returned as a=0, preserving held mu and sigma.

As in the reference package, evaluation at a zero is allowed and gives the normal limit; fitting also includes this attained normal member.

The uniform face is always a local maximum of the likelihood closure. If r is the sample half-range, the peak-density bound rules out improvement for $a\ge r$, while the extreme-pair CDF bound rules it out for $r4^{-1/n}\le a<r$. It need not be global: for `c(-10, rep(0, 20), 10)`, the fitted normal face has higher likelihood. Thus boundary multimodality is genuine even though location is conditionally unique; an exhaustive classification of multiple finite scale modes is not claimed.


Holding mu conditions both face comparisons on that exact centre. For held positive a, sigma can tend to zero with nonzero likelihood precisely when every distance from the held mu is at most a; observations exactly at an endpoint contribute a factor 1/2 each. Holding sigma removes this uniform escape. The proper boundary a=0 is returned as the attained normal member. Constant samples collapse only when both scales are free and mu can coincide with the data.
