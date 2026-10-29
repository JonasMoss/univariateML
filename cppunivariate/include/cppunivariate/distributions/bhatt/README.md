---
id: bhatt
name: Bhattacharjee
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
- name: a
  role: half-width
  domain: (0, inf)
  simulate: 2
  kind: regular
r:
  density: extraDistr::dbhatt
  sampler: extraDistr::rbhatt
  control:
    sigma: Optional positive held sigma; NULL estimates it.
    a: Optional positive held a; NULL estimates it.
    reltol: Relative search tolerance in standardized coordinates.
    iterlim: Maximum refinement iterations per search start.
log_density: log((erf((x-mu+a)/(sqrt(2)*sigma))-erf((x-mu-a)/(sqrt(2)*sigma)))/(4*a))
limits:
  - model: norm
    when: '$a \to 0$'
    detect: conditional normal face within 1e-8*n of the best interior likelihood
    status: NormalLimit
  - model: unif
    when: '$\sigma \to 0$'
    detect: conditional uniform face within 1e-8*n of the best interior likelihood
    status: UniformLimit
examples: |
  set.seed(12876)
  mlbhatt(extraDistr::rbhatt(500, 0, 1, 2))
holds:
- held:
  - mu
  status: not-implemented
  reason: The adapter does not accept `mu`; this hold is not implemented (P2).
---

The Bhattacharjee distribution is the convolution of a normal distribution and a symmetric uniform distribution.

## Details

A constant sample has unbounded likelihood when both scales are free. The normal and uniform faces are fitted conditionally on held scales and compared with the interior within $10^{-8}n$. Holding sigma removes the uniform face; holding a removes the normal face. With a held and sample range exactly 2a, endpoint masses contribute an optimized binomial penalty on the uniform face. For fixed positive scales the location likelihood is strictly log concave; scale optimization uses a grid of simplex starts and is a numerical global search, without a certificate excluding other scale modes.

Identification follows from $E[X]=\mu$, $\kappa_4=-2a^4/15$ and $\operatorname{Var}(X)=\sigma^2+a^2/3$. The density is bounded above by both $1/(2a)$ and $1/(\sqrt{2\pi}\sigma)$, so either scale tending to infinity forces likelihood to zero, including joint scale divergence. With bounded scales, location escaping to infinity also forces likelihood to zero. Joint scale collapse is unbounded only for constant data; for nonconstant data the normal and uniform faces exhaust the remaining nonzero limits. These are direct arguments using the [extraDistr density definition](https://search.r-project.org/CRAN/refmans/extraDistr/html/Bhattacharjee.html).

The normal face uses the sample mean and root mean squared residual (or held sigma). The unrestricted uniform face uses the midrange and half-range; it is approached with endpoints expanded by an amount tending to zero more slowly than sigma. With a held and range less than 2a the face likelihood is $(2a)^{-n}$; with range greater than 2a it is zero. At equality, with endpoint multiplicities k and l, multiply $(2a)^{-n}$ by $(k/(k+l))^k(l/(k+l))^l$. This follows by shifting the centre by order sigma and optimizing the two normal endpoint probabilities. A held subset therefore changes the boundary comparison. A requested held mu is rejected as an unused argument; only the two scale parameters are supported as held coordinates.

With a held and sigma free, range at most 2a always selects the uniform face: each interior density is at most $1/(2a)$, and at exact range 2a the endpoint probabilities are bounded by complementary normal probabilities. These pointwise bounds give precisely the face value above. Range greater than 2a instead guarantees an attained positive-sigma maximum, since both scale endpoints then have zero likelihood. With both scales held, location is always attained, including for constant samples.

The conditional face comparisons respect held scales. The suggested `mlnorm` and `mlunif` alternatives currently fit unrestricted scales/endpoints; they do not preserve a held sigma or a held a. The error message identifies that distinction.

As in the reference package, evaluation at a zero is allowed and gives the normal limit; fitting still requires positive a and reports the normal face with the D1 message.

The uniform face is always a local maximum of the likelihood closure. If r is the sample half-range, the peak-density bound rules out improvement for $a\ge r$, while the extreme-pair CDF bound rules it out for $r4^{-1/n}\le a<r$. It need not be global: for `c(-10, rep(0, 20), 10)`, the fitted normal face has higher likelihood. Thus boundary multimodality is genuine even though location is conditionally unique; an exhaustive classification of multiple finite scale modes is not claimed.
