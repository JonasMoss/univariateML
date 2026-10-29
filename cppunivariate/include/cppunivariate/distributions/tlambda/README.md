---
id: tlambda
name: Tukey lambda
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
- name: lambda
  role: shape
  domain: '(-inf, inf)'
  simulate: 0.14
  fit_domain:
  - lower: -1.0
    restriction:
      mechanism: collapse
      exempt_when_held:
      - - mu
      reason: lambda >= -1 prevents free-location scale collapse; ties at held mu
        need a separate diagnostic.
  - upper: 1.0
    restriction:
      mechanism: branch
      exempt_when_held: []
      reason: lambda <= 1 selects the principal branch; no exemption is proved.
  kind: regular
r:
  density: dtlambda
  sampler: rtlambda
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    lambda: "Optional finite held shape; NULL estimates it under the applicable fitting bounds."
    reltol: "Tolerance on the standardized search simplex."
    iterlim: "Maximum simplex iterations per search start."
log_density: "-log(sigma)-log(u**(lambda-1)+(1-u)**(lambda-1))"
references:
  - "Joiner, B. L. and Rosenblatt, J. R. (1971). Some properties of the range in samples from Tukey's symmetric lambda distributions. Journal of the American Statistical Association 66, 394-399."
examples: |
  set.seed(1)
  mltlambda(rtlambda(50, lambda = 0.14))
holds: []
limits:
  - name: scale collapse
    when: Scale tends to zero at the fitted or held location.
    detect: Excessive mass at that location or vanishing residual spread.
    status: CollapseLimit
    message: "collapse"
  - name: infeasible held support
    fixed: [sigma, lambda]
    when: Held scale and positive shape cannot contain the sample.
    status: InvalidSupport
    message: support cannot contain
---

The Tukey lambda family is defined by its quantile function and spans symmetric heavy-tailed, logistic, approximately normal and uniform shapes.

## Details

Evaluation permits all finite shapes. With location free, estimated shape uses the principal unimodal branch, -1 <= lambda <= 1, including both attained bounds. Held location exempts the lower bound. The default fit requires at least three finite observations with every tie multiplicity strictly less than half the sample size. Numerical fitting also requires a finite sample range and standardization that preserves distinct values. Every coefficient may be held; a held finite shape may lie outside the fitted branch, including lambda = 2 as an exact uniform alias. The two uniform representations have scales differing by a factor of two, so unrestricted shape is not identifiable.

The unrestricted likelihood is unbounded. Set location at one observation and sigma = lambda times A, where A exceeds every distance to that observation. At the centre the density is 2^(lambda-2)/(lambda A). At each nonzero distance d, the density is asymptotic to 1/(lambda abs(d)). Thus, with k observations at the centre, the log likelihood grows as k lambda log(2) - n log(lambda) plus a bounded term. No unrestricted MLE is returned.

The principal branch excludes the U-shaped interval 1 < lambda < 2 and the subsequent increasingly concentrated branch. Merely capping shape at 2 leaves a density discontinuity at lambda = 1: at a support endpoint, the density as lambda decreases to 1 from above is twice the uniform density. On the chosen branch the endpoint density is zero below 1 and the uniform endpoint density at 1 makes the likelihood upper semicontinuous.

For lambda = -a < 0, scale collapse at a location with multiplicity k has likelihood proportional to sigma^((n-k)/a-k). Bounding a by 1 and requiring 2k < n excludes collapse. More generally, uniformly over the fitted branch the standardized density is bounded and z^2 f(z) is bounded. Consequently, near a collapsing location the likelihood is bounded by a constant times sigma^(n-2k), while diverging scale or location gives likelihood tending to zero. Compactness and upper semicontinuity then give an attained maximum. When location and scale are both free, the existing tie guard is sufficient, not necessary; unsupported tie configurations are rejected also with fixed shape.

The fitter standardizes the data, searches multiple location, scale and shape starts and separately optimizes the lower shape face. The exact uniform fit is compared with all candidates and wins ties within 1e-8 times sample size. This is a numerical global search, not a certificate that every mode has been found. The uniform is an attained member, so it is returned rather than an escape error. There are no missing limit models within the declared fitted branch. The log density uses u = F(x), implicitly defined by the quantile function. The quality specification differentiates this inverse implicitly and uses independent SciPy and mpmath evaluations.


Any location or scale may be held. Holding sigma removes scale collapse and permits small or tied samples. With mu held and sigma free, only ties at that mu enter the collapse exponent. Held positive lambda requires the sample to fit inside its support; endpoints with positive limiting density are allowed, including lambda>=1. Conditional searches apply the README bounds and hold exemptions, and explicitly compare the attained uniform member. Searches nominate modes numerically.

With free shape, any observation equal to held mu permits scale collapse
through sufficiently negative lambda when scale is free. With negative
lambda held, equality of the collapse exponent selects an unattained
fixed-location supremum.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.

With `mu` held, the lower shape restriction is exempt. The fitter searches
the wider shape domain and diagnoses collapse separately when observations
equal the held location and scale is free.

A held finite `lambda` may lie outside the principal branch; the bound
applies to estimated shape. Fixed negative shape uses its actual collapse
exponent, and held positive-shape support must contain the observations.
