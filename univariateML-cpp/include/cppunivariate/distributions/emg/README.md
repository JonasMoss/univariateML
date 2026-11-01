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
    firmness: argued
    when: '$\lambda\to\infty$'
    detect: Conditional normal likelihood wins within 1e-8*n.
    status: NormalLimit
  - name: translated exponential
    firmness: argued
    when: '$\sigma\to0$'
    detect: Conditional translated-exponential likelihood wins within 1e-8*n.
    status: TranslatedExponentialLimit
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
---

The exponentially modified Gaussian is the sum of independent normal and exponential variables, used for asymmetric response times and chromatographic peaks.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-skewreal.md) describes the limiting paths and the scope of the boundary comparisons.

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

For fixed dimensionless shape $k=\lambda\sigma$, the log-likelihood is strictly
concave in $(u,b)=(\mu/\sigma,1/\sigma)$. For nonconstant observations
it has a unique finite maximum in these coordinates; constant observations
collapse when location and scale are free. Holding the physical rate fixed is a different constraint. This is a mathematical property,
not a certificate of the current search over shape. See the [proof and scope](../../../../../dev/research/evaluations/2026-10-31-logconcave-report.md).

The 2026-10-31 fit-speed revision returns location scores in a fixed-size array
instead of allocating a vector. Starts, density evaluations, location
convergence, boundary comparisons and failed-optimizer score checks are
unchanged. Full-profile caching and likelihood-invariant hoisting were tried
without a useful default-fit timing improvement. See the [paired evidence and
rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

`mu` is the normal component mean, rather than the mean of the sum.
Constant observations can have unbounded likelihood. A finite interior maximum
need not exist. Normal and translated exponential faces are compared with
interior candidates, respecting held parameters. `mlnorm` fits the unrestricted
normal; its interface does not retain held EMG parameters. The translated
exponential face has endpoint `min(x)` and rate
`1 / mean(x - min(x))` when both parameters are free. There is no package
fitter for the translated exponential: `mlexp(x - min(x))` fits its distances.
With a held endpoint exactly at the minimum the limiting density there is
half the exponential density. Repeated minima count separately.

The multistart fallback profiles location by a bracketed score solve and searches the remaining log scales. Neither numerical search certifies global optimality. Empirical skewness sign describes local behaviour near the normal
face and is not used as a global existence criterion.

## d/p/q/r

This is the sum of an independent normal and exponential variate.
