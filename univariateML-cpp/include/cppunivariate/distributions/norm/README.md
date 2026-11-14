---
id: norm
inference_numeric:
  - regime: interior
    available: true
    condition: "finite mean and sd > 0; mean and/or sd free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "sd = 0 or nonfinite coefficients"
    reason: "zero scale is outside the interior continuous model"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: core_dnorm
  sampler: stats::rnorm
  control:
    sd: Optional held sd; NULL estimates it.
    mean: Optional held mean; NULL estimates it.
log_density: -log(sd) - log(2*pi)/2 - (x-mean)**2/(2*sd**2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York."
holds: []
limits:
  - name: scale collapse
    firmness: proved
    when: Scale tends to zero at the fitted or held location
    detect: sd is free and every observation equals the fitted or held location
    status: Unbounded
    message: "unbounded"
examples: |
  x <- datasets::trees$Height
  mlnorm(x)

summary: The normal distribution models symmetric continuous variation through a location and a standard deviation.
classification: classical
classification_reason: A standard model for additive measurement errors and approximately Gaussian fluctuations.
aliases:
- "Gaussian"
- "Gauss"
- "Laplace–Gauss"
- "law of error"
- "law of facility of errors"
- "Laplace's second law"
key_references:
- 1

group: normal and Student
construction:
- location-scale
relations:
  special_case_of:
  - family: sn
    at:
      xi: mean
      omega: sd
      alpha: 0
  - family: snorm
    at:
      mean: mean
      sd: sd
      xi: 1
  - family: ged
    at:
      mean: mean
      sd: sd
      nu: 2
  - family: bhatt
    at:
      mu: mean
      sigma: sd
      a: 0
  - family: norm
    at:
      mean: mean
      sd: sd
    configuration:
      kernel: tnorm
      truncate:
      - -Inf
      - Inf
    coefficient_map:
      mean: mean
      sd: sd
      a: -Inf
      b: Inf
  truncation_of:
  - family: norm
    at:
      mean: mean
      sd: sd
    source_configuration:
      kernel: tnorm
      truncate:
      - a
      - b
      coefficient_map:
        mean: mean
        sd: sd
fit:
  method: closed-form
  certified: 'yes'
  note: 'norm/README.md, Details; norm/mle.hpp (public fitter and held overloads); norm/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- measurement and errors
existence:
  uniqueness: proved
  statement: >-
    With $\mu$ and $\sigma$ free, the MLE exists if and only if the
    observations are not all equal; it is then unique, with $\hat\mu = \bar x$
    and $\hat\sigma^2 = n^{-1}\sum_i (x_i - \bar x)^2$. With $\sigma$ held,
    $\hat\mu = \bar x$ always. With $\mu$ held, the MLE exists unless every
    observation equals $\mu$.
  reason: >-
    The normal family is a regular exponential family with sufficient
    statistic $(x, x^2)$ and natural parameter
    $(\mu/\sigma^2, -1/(2\sigma^2))$. The log-likelihood is strictly concave
    in the natural parameter, so it has at most one maximizer, and one exists
    exactly when $(\bar x, \overline{x^2})$ lies inside the convex hull of the
    parabola $\{(x, x^2)\}$, that is when $\overline{x^2} > \bar x^2$; this
    fails only for constant samples.
modifiers:
  truncate:
    status: implemented
    certified: 'yes'
    domain_bounded: parent parameter domain
    note: Concave conditional exponential family.
    limits:
    - name: scale collapse
      firmness: proved
      when: As $\sigma\to0$, mass concentrates at the projection of $\mu$ onto $[a,b]$.
      detect: Scale is free and the sample is constant; mean is free or every observation equals the projection
        of the held mean onto the nonempty profiled interval.
      status: DegenerateData
      message: unbounded
      kind: nonexistence
    - name: endpoint location escape
      firmness: proved
      fixed:
      - sd
      when: With positive scale held, $\mu\to-\infty$ at a finite lower endpoint or $\mu\to\infty$ at a finite upper
        endpoint concentrates mass there.
      detect: Mean is free and every observation equals the same finite endpoint of the nonempty profiled interval.
      status: UnboundedLocation
      message: unbounded
      kind: nonexistence
    - model: exp
      firmness: proved
      when: As $\sigma\to\infty$ and $\mu/\sigma^2\to-1/(\bar x-a)$ with $a$ finite and $b=\infty$, the distribution
        tends to $a+Y$, where $Y$ is exponential.
      detect: The sample is nonconstant, both parent coefficients are free, $a$ is finite, $b=\infty$, and the empirical
        variance is at least $(\bar x-a)^2$.
      status: ExponentialBoundary
      numerical: true
    - model: exp
      firmness: proved
      when: As $\sigma\to\infty$ and $\mu/\sigma^2\to1/(b-\bar x)$ with $a=-\infty$ and $b$ finite, the distribution
        tends to $b-Y$, where $Y$ is exponential.
      detect: The sample is nonconstant, both parent coefficients are free, $a=-\infty$, $b$ is finite, and the
        empirical variance is at least $(b-\bar x)^2$.
      status: ReflectedExponentialBoundary
      numerical: true
    - name: truncated exponential
      firmness: proved
      when: As $\sigma\to\infty$ and $\mu/\sigma^2\to r$ with both bounds finite, the distribution tends to the
        law with density proportional to $e^{rx}$ on $[a,b]$. It is uniform when $r=0$.
      detect: The sample is nonconstant, both parent coefficients are free, both bounds are finite, and the empirical
        variance is at least that of the exponential tilt with the same empirical mean.
      status: TruncatedExponentialBoundary
      numerical: true
    - name: uniform
      firmness: proved
      fixed:
      - mean
      when: With $\mu$ held and both bounds finite, $\sigma\to\infty$ gives the uniform distribution on $[a,b]$.
      detect: The parent mean $\mu$ is held, scale is free, both bounds are finite, and the empirical squared distance
        from $\mu$ is at least $((b-\mu)^3-(a-\mu)^3)/(3(b-a))$.
      status: UniformBoundary
      numerical: true

---

The normal (Gaussian) distribution is a symmetric distribution on the real line, used to model continuous measurements and errors.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives these boundary conditions, including the relevant held parameters.

The estimated `sd` uses the variance with divisor $n$, rather than the unbiased divisor $n-1$. It is the residual root mean square about the exact sample mean, which near ties can differ from the returned, rounded mean by a large fraction of the spread: on $(1,1,1+2^{-52})$ the mean rounds to 1, residuals about 1 give $2^{-52}/\sqrt3$, and the MLE is $2^{-52}\sqrt2/3$. The reported log likelihood is the value at the returned coefficients, so on that sample it lies $n\delta^2/(2\,\mathrm{sd}^2) = 3/4$ below the supremum, $\delta = -2^{-52}/3$ being the rounding of the mean (accuracy contract, "Log likelihood").


With mean held, scale is the root mean square residual about that value. Scale collapses only when every observation equals the held mean. Holding sd removes collapse, including for constant samples.

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.

For residuals $r_i=x_i-\mu$, the scale profile is $b\sqrt{\sum_i(r_i/b)^2/n}$, where $b=\max_i|r_i|$. Likelihood evaluation uses residual-to-scale ratios rather than squared physical scales. A nonzero residual whose fitted scale cannot be represented is a numerical failure, not collapse.
