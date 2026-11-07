---
id: gamma
inference_numeric:
  - regime: interior
    available: true
    condition: "shape > 0 and scale > 0 finite; shape and/or scale free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "shape or scale at a boundary or limit"
    reason: "gamma boundary and limit models are outside the interior derivative regime"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Gamma
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: '\theta'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: stats::dgamma
  sampler: stats::rgamma
  control:
    shape: Optional held shape; NULL estimates it.
    scale: Optional held scale; NULL estimates it.
    abstol: Absolute step tolerance in shape.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: -shape*log(scale) - loggamma(shape) + (shape - 1)*log(x) - x/scale
references:
  - "Choi, S. C, and R. Wette. \"Maximum likelihood estimation of the parameters of the gamma distribution and their bias.\" Technometrics 11.4 (1969): 683-690. doi:10.1080/00401706.1969.10490731."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York."
examples: |
  x <- datasets::precip
  mlgamma(x)

summary: The gamma distribution models positive measurements with separate shape and scale coefficients.
classification: classical
classification_reason: A standard model for positive waiting times and skewed measurements with a power-law onset.
aliases: []
key_references:
- 1
- 2
limits:
- name: unbounded likelihood
  firmness: proved
  when: The free shape tends to infinity while scale tends to zero at fixed mean
  detect: all observations are equal and both shape and scale are free
  status: Degenerate
  message: does not exist
  kind: nonexistence

group: gamma and Weibull
construction:
- base
relations:
  special_case_of:
  - family: trgamma
    at:
      shape1: shape
      shape2: 1
      scale: scale
  transform_of:
  - family: naka
    at:
      shape: shape
      scale: shape*scale
    transform: power
    power: 0.5
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'gamma/README.md, Details; gamma/mle.hpp (public fitter and held overloads); gamma/test.cpp: Gamma shape
    has a unique monotone digamma equation; scale is analytic, with bracketed fallback and held-coordinate solves.
    Transformation preserves the maximum.'
areas:
- reliability and survival
- waiting times and queues
existence:
  uniqueness: proved
  statement: >-
    With $k$ and $\theta$ free, the MLE exists if and only if the
    observations are not all equal, and it is then unique. With $k$ or
    $\theta$ held, it exists and is unique for every positive sample.
  reason: >-
    The gamma family is a regular exponential family with sufficient
    statistic $(\log x, x)$ and natural parameter $(k, -1/\theta)$, a
    one-to-one reparameterization. The log-likelihood is strictly concave in
    the natural parameter, so it has at most one maximizer. A maximizer
    exists exactly when the mean sufficient statistic lies in the interior of
    its convex support, here $\overline{\log x} < \log \bar x$, which holds by
    Jensen's inequality unless all observations are equal. Holding one
    coefficient leaves a concave one-dimensional slice whose score changes
    sign for every positive sample.
---

The gamma distribution models positive measurements and waiting times.

## Details

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and their held-parameter conditions.

2026-10-30 (TASK-125): when the held-shape total overflows, the scale is computed from the mean accumulated as `sum(x/n)`, and the likelihood penalty as `sum(x/scale)`. The existing held-shape overflow test checks the result; it is not an error bound for arbitrary sample sizes.

Holding either shape or scale gives a unique finite fit for any positive sample, including constant data. With both free, constant data have unbounded likelihood.


With both coefficients free, profiling scale gives the shape score
$\log(k)-\psi(k)=\log(\bar x)-\overline{\log x}$. Its unique positive
root is sought by Newton iteration; near ties or after Newton failure the
implementation uses a bracketed log-shape bisection. The bracket follows
digamma bounds, and an exhausted bracket search reports failure. With
shape held, scale is analytic; with scale held, the monotone digamma
equation is bisected. This establishes uniqueness of the mathematical
solution, subject to floating-point and solver tolerances.

## Decision history

The historical Newton initialization and absolute step tolerance were kept
for compatibility with dependent fits and goldens. The near-tie fallback
addresses cancellation in the score and likelihood; see `mle.hpp`, family
tests and `dev/research/evaluations/2026-10-29-p0-positive.md`. On 2026-10-30
commit `cd3993d` changed the reported coefficient from rate to scale;
the rate remains an internal calculation. The original reason for the
empirical initialization constants is unknown; historical source or an
independent comparison of starts would settle it.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
