---
id: naka
inference_numeric:
  - regime: interior
    available: true
    condition: "finite shape > 0 and scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
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
    abstol: Positive relative accuracy tolerance for the gamma shape bracket.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
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
  firmness: proved
  fixed: [scale]
  when: 'The free shape $m$ tends to infinity with $\Omega$ held at the common squared observation value.'
  detect: 'Every squared observation equals the held scale $\Omega$.'
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
  special_case_of:
  - family: trgamma
    at:
      shape1: shape
      shape2: 2
      scale: sqrt(scale/shape)
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
aliases:
- "Nakagami-m"
- "m-distribution"
key_references:
- 3
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists for every
    nonconstant positive sample. Constant samples have unbounded likelihood.
    Holding shape gives a unique finite scale estimate, including for constants.
    Holding scale gives a unique finite shape estimate unless every squared
    observation equals that scale, when the likelihood is unbounded.
    Holding both leaves only likelihood evaluation.
  reason: >-
    Put $y_i=x_i^2$. With scale free, its maximizer is $\hat\Omega=\bar y$.
    The remaining shape equation is $\log m-\psi(m)=s$, where
    $s=\log\bar y-\overline{\log y}$ is positive exactly for nonconstant data.
    With scale held, replace $s$ by the average of
    $y_i/\Omega-1-\log(y_i/\Omega)$, which is positive unless every
    $y_i=\Omega$. The function $\log m-\psi(m)$ decreases strictly from
    infinity to zero because $\psi'(m)>1/m$. If $s=0$, the likelihood
    grows without bound as $m$ tends to infinity.
---

The Nakagami distribution models positive amplitudes, including radio-fading measurements. Its square follows a gamma distribution.

## Details

The shape $m$ may be any positive value, including values below $1/2$. The shape controls concentration around the root mean squared amplitude. The scale $\Omega$ is the mean squared amplitude, rather than the scale of the gamma law of the squared amplitude. Sampling takes the square root of a gamma variable.

With scale free, its estimate is the sample second moment. The remaining shape equation is the decreasing gamma log-shape/digamma equation. Holding scale gives the same equation with a different nonnegative right-hand side. It has a unique finite solution unless every squared observation equals the held scale. Holding shape permits constant data. Holding both coefficients evaluates the likelihood.

The gamma reduction forms relative squared differences as $r_i(2+r_i)$, where $r_i=(x_i-x_0)/x_0$, and accumulates the second moment relative to its anchor. This preserves concentrated samples without first rounding their squares. The likelihood includes the square-transform Jacobian and is evaluated at the returned coefficients. Binary64 spacing of the second moment can prevent transfer of the latent likelihood certificate on ulp-wide samples; these finite fits retain an explicit uncertified indication. An unrepresentable free second moment is a numerical failure; a fully held likelihood can remain finite even when individual squares underflow.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and their held-parameter conditions.

## Decision history

2026-10-31 (diagnostic contract): Empty or invalid input carries a non-success status. Degeneracy depends on which coefficients are free, and delegate failures survive transformations. See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md) and the TASK-132 cases in `test.cpp` for the validation scope.
