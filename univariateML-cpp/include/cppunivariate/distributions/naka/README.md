---
id: naka
name: Nakagami
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: 'm'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\Omega'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dnaka
  sampler: rnaka
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    abstol: Absolute step tolerance in shape (the gamma shape for x^2).
    iterlim: Maximum number of iterations.
log_density: log(2) + shape*log(shape) - loggamma(shape) - shape*log(scale) + (2*shape - 1)*log(x) - shape*x**2/scale
references:
- 'Choi, S. C, and R. Wette. "Maximum likelihood estimation of the parameters of the gamma distribution and their
  bias." Technometrics 11.4 (1969): 683-690.'
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 17.
  Wiley, New York.
- Nakagami, M. (1960). The m-Distribution—A General Formula of Intensity Distribution of Rapid Fading. In Statistical
  Methods in Radio Wave Propagation. Pergamon Press. doi:10.1016/B978-0-08-009306-2.50005-4.
limits:
- name: unbounded likelihood
  fixed: [scale]
  when: Every squared observation equals held scale.
  status: Degenerate
  message: unbounded
examples: |
  set.seed(1)
  x <- rml(200, mlnaka())
  mlnaka(x)

group: gamma and Weibull
construction:
- transformation
relations:
  transform_of:
  - family: gamma
    transform: power
    at:
      shape: shape
      scale: scale/shape
    power: 2
fit:
  method: delegation
  certified: 'yes'
  note: 'naka/README.md, Details; naka/mle.hpp (public fitter and held overloads); naka/test.cpp: Free scale is
    the empirical second moment; shape solves the decreasing gamma log-shape/digamma equation. Held scale has the
    same monotone shape equation or an explicit collapse.'
areas:
- reliability and survival
- waiting times and queues
classification: specialist
classification_reason: Used to model radio-fading amplitudes whose squared magnitude is gamma distributed.
summary: Nakagami provides a model for radio-fading amplitudes whose squared magnitude is gamma distributed.
aliases: []
key_references:
- 3
---

The Nakagami distribution models positive amplitudes, including radio-fading measurements. Its square follows a gamma distribution.

## Details

The shape $m$ may be any positive value; $m < 1/2$ is not excluded. The scale is the mean of the squared variable, rather than the scale of that gamma variable.

With scale held and shape free, the likelihood is unbounded if every squared observation equals scale. Holding shape permits constant data.

## d/p/q/r

Scale is the mean squared amplitude; sampling takes the square root of a gamma variate.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
