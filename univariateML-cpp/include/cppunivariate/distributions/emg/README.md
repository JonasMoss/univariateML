---
id: emg
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mu, sigma > 0 and lambda > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Exponentially Modified Gaussian
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
  role: normal scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: lambda
  symbol: '\lambda'
  role: exponential rate
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: demg
  sampler: remg
  control:
    mu: Optional held normal-component mean; NULL estimates it.
    sigma: Optional held normal-component standard deviation; NULL estimates it.
    lambda: Optional held exponential rate; NULL estimates it.
    reltol: Tolerance on the log-scale search simplex diameter.
    iterlim: Maximum iterations per search start.
log_density: log(lambda/2) + lambda*(mu-x) + lambda**2*sigma**2/2 + log(erfc((mu+lambda*sigma**2-x)/(sqrt(2)*sigma)))
limits:
  - model: norm
    firmness: proved
    when: 'The rate satisfies $\lambda\to\infty$ with $\mu$ and $\sigma>0$ fixed.'
    detect: 'With rate free, the conditional normal log likelihood is at least the best numerical interior log likelihood minus $10^{-8}n$.'
    status: NormalLimit
    numerical: true
  - name: translated exponential
    firmness: proved
    when: 'The normal scale satisfies $\sigma\to0$ with location and positive rate fixed.'
    detect: 'With scale free and location free or held at or below the sample minimum, the conditional translated-exponential log likelihood is at least the best numerical interior log likelihood minus $10^{-8}n$.'
    status: TranslatedExponentialLimit
    numerical: true
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlemg())
  mlemg(x)

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
    - family: exp
      at:
        rate: lambda
fit:
  method: profile-search
  certified: partial
  note: 'emg/README.md, Details: fixed-shape location-scale objective is strictly concave; the shape grid and face comparison are numerical.'
areas:
- measurement and errors
classification: specialist
classification_reason: Used to model chromatographic peaks combining Gaussian broadening and exponential delay.
summary: Exponentially Modified Gaussian provides a model for chromatographic peaks combining Gaussian broadening
  and exponential delay.
aliases:
- "exponentially modified normal"
- "ex-Gaussian"
- "EMG"
references:
- Grushka, E. (1972). Characterization of exponentially modified Gaussian peaks in chromatography. Analytical Chemistry,
  44(11), 1733–1738. doi:10.1021/ac60319a011.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant samples have unbounded likelihood.
    For nonconstant samples a finite maximum need not exist, because a
    normal or translated-exponential limit can compete with interior fits.
    A complete existence criterion and joint uniqueness result are not
    established. Holding $\lambda$ removes the normal escape; holding
    $\sigma$ removes the translated-exponential escape. With both held,
    free location has a unique finite maximum. Other held subsets are
    compared with their compatible limits numerically.
  reason: >-
    The normal and exponential limits follow from the independent-sum
    representation. Fixed dimensionless shape $\lambda\sigma$ gives a
    strictly concave location and inverse-scale objective with a unique
    finite maximum for nonconstant samples. This does not prove uniqueness
    when shape is free or when the physical rate alone is held.
---

The exponentially modified Gaussian is the sum of independent normal and exponential variables, used for asymmetric response times and chromatographic peaks.

## Details

When every coefficient is held, the likelihood is evaluated at those values
without optimization.

Write $X=\mu+\sigma Z+E_\lambda$, with $Z$ standard normal and
$E_\lambda$ independent exponential. For fixed finite $\mu$ and $\sigma>0$,
$\lambda\to\infty$ makes $E_\lambda\to0$ in probability; the convolution
density tends pointwise to the normal density by bounded convergence. For
fixed finite $\mu$ and $\lambda>0$, $\sigma\to0$ gives the translated exponential:
the convolution density converges at every $x\ne\mu$ by the approximate
identity property. Both limits also hold in distribution. At $x=\mu$ the
latter density tends to half its one-sided value, a distinction relevant
when an observation lies exactly at the endpoint. These statements do not
certify the numerical face comparison.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-skewreal.md) describes the limiting paths and the scope of the boundary comparisons.

For fixed dimensionless shape $k=\lambda\sigma$, the log-likelihood is strictly
concave in $(u,b)=(\mu/\sigma,1/\sigma)$. For nonconstant observations
it has a unique finite maximum in these coordinates; constant observations
collapse when location and scale are free. Holding the physical rate fixed is a different constraint. This is a mathematical property,
not a certificate of the current search over shape. See the [proof and scope](../../../../../dev/research/evaluations/2026-10-31-logconcave-report.md).

$\mu$ is the normal component mean, $\sigma$ its standard deviation and
$\lambda$ the exponential rate. The mean delay is $1/\lambda$.
Constant observations can have unbounded likelihood. A finite interior maximum
need not exist. Normal and translated exponential faces are compared with
interior candidates, respecting held parameters. The translated exponential face has endpoint $\min_i x_i$ and rate
$1/\overline{(x-\min_i x_i)}$ when both are free.
With a held endpoint exactly at the minimum the limiting density there is
half the exponential density. Repeated minima count separately.

The multistart fallback profiles location by a bracketed score solve and searches the remaining log scales. Neither numerical search certifies global optimality. Empirical skewness sign describes local behaviour near the normal
face and is not used as a global existence criterion.

## R notes

`mlnorm` fits an unrestricted normal and does not retain held EMG
coefficients automatically. There is no translated-exponential fitter.
For a free endpoint, `mlexp(x - min(x))` fits distances from the fitted
endpoint. Held endpoints or rates require their conditional comparison.

## Decision history

The 2026-10-31 fit-speed revision returns location scores in a fixed-size array
instead of allocating a vector. Starts, density evaluations, location
convergence, boundary comparisons and failed-optimizer score checks are
unchanged. Full-profile caching and likelihood-invariant hoisting were tried
without a useful default-fit timing improvement. See the [paired evidence and
rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

2026-10-31 (fixed-shape profile search):

For a fully free fit, fix the dimensionless shape $k=\lambda\sigma$ and optimize the
location and inverse scale jointly. The fixed-shape objective is strictly
concave, and the inner solve uses analytic derivatives with damped Newton
steps and a scaled score check. A grid in log shape is traversed outward
with warm starts; local grid maxima are refined and the best candidate is
polished by the existing local optimizer. One existing multistart seed is also
polished and retained when its likelihood is at least as high, preserving the
established fit precision. If the grid has no interior local
maximum, the grid-end likelihoods are compared with the normal and translated-exponential faces using
the existing face rules. This outer search and face comparison are numerical;
they do not certify that no mode lies beyond or between grid points. Failed
inner solves, unsuccessful polishing and unresolved face comparisons fall
back to the existing multistart; held-parameter fits retain that path. A test
hook counts fallbacks when enabled. See the [round-four evidence](../../../../../dev/research/evaluations/2026-10-31-logconcave-4-report.md).
