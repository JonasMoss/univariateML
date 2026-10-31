---
id: gamma
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
    iterlim: Maximum number of iterations.
log_density: -shape*log(scale) - loggamma(shape) + (shape - 1)*log(x) - x/scale
references:
  - "Choi, S. C, and R. Wette. \"Maximum likelihood estimation of the parameters of the gamma distribution and their bias.\" Technometrics 11.4 (1969): 683-690. doi:10.1080/00401706.1969.10490731."
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York."
examples: |
  x <- datasets::precip
  mlgamma(x)

summary: The gamma distribution models positive measurements with separate shape and scale coefficients.
classification: classical
classification_reason: A standard model for waiting times and positive skewed data.
aliases: []
key_references:
- 1
- 2
limits:
- name: unbounded likelihood
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
fit:
  method: scalar-solve
  certified: partial
  note: Unique scalar solution and bracketed fallback documented in Details; subject to floating-point solver
    tolerances.
areas:
- reliability and survival
- waiting times and queues
---

The gamma distribution models positive measurements and waiting times.

## Details

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
