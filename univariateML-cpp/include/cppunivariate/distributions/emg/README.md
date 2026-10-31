---
id: emg
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
    when: '$\lambda\to\infty$'
    detect: Conditional normal likelihood wins within 1e-8*n.
    status: NormalLimit
  - name: translated exponential
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
relations: {}
fit:
  method: multistart
  certified: partial
  note: 'emg/README.md, Details; emg/mle.hpp (public fitter and held overloads); emg/test.cpp: Location is strictly
    concave with scales held; joint scales use multistart/grid optimization without a global certificate.'
areas:
- measurement and errors
classification: specialist
classification_reason: Used to model chromatographic peaks combining Gaussian broadening and exponential delay.
summary: Exponentially Modified Gaussian provides a model for chromatographic peaks combining Gaussian broadening
  and exponential delay.
aliases: []
references:
- Grushka, E. (1972). Characterization of exponentially modified Gaussian peaks in chromatography. Analytical Chemistry,
  44(11), 1733–1738. doi:10.1021/ac60319a011.
key_references:
- 1
---

The exponentially modified Gaussian is the sum of independent normal and exponential variables, used for asymmetric response times and chromatographic peaks.

## Details

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

The location likelihood is strictly concave for fixed positive scales and is
profiled by a bracketed score solve. Multiple starts in the remaining log
scales allow competing modes. This numerical search does not certify global
optimality. Empirical skewness sign describes local behaviour near the normal
face and is not used as a global existence criterion.

## d/p/q/r

This is the sum of an independent normal and exponential variate.
