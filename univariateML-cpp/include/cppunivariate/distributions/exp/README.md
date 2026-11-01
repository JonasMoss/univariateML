---
id: exp
inference_numeric:
  - regime: interior
    available: true
    condition: "finite rate > 0; rate free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "rate zero or nonfinite"
    reason: "rate is on a boundary or outside the proper finite model"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Exponential
support: {type: real, lower: 0.0, upper: inf, closed: [true, false]}
params:
- name: rate
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: stats::dexp
  sampler: stats::rexp
  control:
    rate: Optional held rate; NULL estimates it.
log_density: log(rate) - rate*x
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 19. Wiley, New York."
examples: |
  set.seed(1)
  x <- rml(200, mlexp())
  mlexp(x)

summary: The exponential distribution models nonnegative waiting times with a constant hazard rate.
classification: classical
classification_reason: A standard model for waiting times under a constant event hazard.
aliases:
- "negative exponential"
- "one-parameter exponential"
- "antilogarithmic"
key_references:
- 1
limits:
- name: unbounded likelihood
  when: The rate grows without bound, $\lambda \to \infty$
  detect: All observations are zero and the rate is free
  status: Unbounded
  message: unbounded
  kind: nonexistence

group: gamma and Weibull
construction:
- base
relations:
  special_case_of:
  - family: gamma
    at:
      shape: 1
      scale: 1/rate
  - family: weibull
    at:
      shape: 1
      scale: 1/rate
  - family: gpd
    at:
      mu: 0
      sigma: 1/rate
      xi: 0
  - family: sgomp
    at:
      b: rate
      eta: 0
  transform_of:
  - family: laplace
    at:
      mu: 0
      sigma: 1/rate
    transform: abs
fit:
  method: closed-form
  certified: 'yes'
  note: 'exp/README.md, Details; exp/mle.hpp (public fitter and held overloads); exp/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- reliability and survival
- waiting times and queues
existence:
  uniqueness: proved
  statement: >-
    For a nonempty sample of finite nonnegative observations, the free-rate
    MLE exists and is unique if and only if at least one observation is positive.
    Holding a finite positive rate gives a finite likelihood for every such sample.
  reason: >-
    For sample size $m$ and sum $S$, the log likelihood is
    $m\log\lambda-\lambda S$. Its second derivative is $-m/\lambda^2<0$.
    If $S>0$, its unique maximum is $\hat\lambda=m/S$.
    If $S=0$, it increases without bound as $\lambda\to\infty$.

---

The exponential distribution models nonnegative waiting times with a constant hazard rate.

## Details

The rate $\lambda$ controls how quickly events occur. Larger rates describe
shorter waiting times. The free-rate estimate is the inverse sample mean.
A held finite positive rate gives a finite likelihood even for all-zero data.
With the rate free, all-zero data have unbounded likelihood.

## Decision history

On 2026-05-08 the exponential dossier retained the closed-form rate fit
(commit `e5e33a1`); the
choice follows directly from the concave log likelihood, so no numerical
search or competing finite boundary is needed. The positive-rate estimate
is unique when the sum is positive; all-zero data have unbounded
likelihood as the rate increases. The held-rate case is evaluated at the supplied value.
See the family tests and the existence analysis in
`dev/research/evaluations/2026-10-29-p0-positive.md`.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.

### Range portability (2026-11-01)

If the ordinary sum overflows, the free-rate calculation scales each datum by
its largest value before summing. This evaluates the same closed form without
relying on an extended exponent range. If a held-rate sum overflows,
the likelihood instead sums the individually rate-weighted observations.
The R extreme-value test and appended C++ held-rate case use two
observations of `1e308`.
