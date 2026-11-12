---
id: geom
inference_numeric:
  - regime: interior
    available: true
    condition: "0 < prob < 1; prob free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "prob = 0 or prob = 1"
    reason: "probability is on a boundary; the count distribution is improper or degenerate"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Geometric
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: stats::dgeom
  sampler: stats::rgeom
  control:
    prob: "Optional held prob; NULL estimates it."
log_density: "log(prob) + x*log(1 - prob)"
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  set.seed(1)
  x <- rml(200, mlgeom())
  mlgeom(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations:
  special_case_of:
  - family: nbinom
    at:
      size: 1
      prob: prob
  - family: cmp
    at:
      lambda: 1-prob
      nu: 0
  - family: zmgeom
    at:
      prob: prob
      p0: prob
  - family: dgamma
    at:
      shape: 1
      rate: -log(1-prob)
    when: prob < 1
  - family: dweibull
    at:
      shape1: 1-prob
      shape2: 1
    when: prob < 1
  truncation_of:
  - family: dlaplace
    at:
      location: 0
      scale: 1-prob
    when: prob < 1
fit:
  method: closed-form
  certified: 'yes'
  note: 'geom/README.md, Details; geom/mle.hpp (public fitter and held overloads); geom/test.cpp: Closed-form maximum
    for every supported free/held subset; degenerate or incompatible samples stop as documented.'
areas:
- counts and ecology
classification: classical
classification_reason: A standard model for failures before the first success in repeated independent trials.
summary: Geometric provides a model for failures before the first success in repeated independent trials.
aliases:
- "Furry"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    For every nonempty sample of nonnegative integer counts, the free-probability
    MLE exists and is unique. All-zero data give $\hat p=1$.
    A held probability $0<p<1$ gives positive likelihood for every such sample.
    Holding $p=1$ is compatible only with all-zero data.
  reason: >-
    For sample size $m$ and sum $S$, the log likelihood is
    $m\log p+S\log(1-p)$. If $S>0$, strict concavity gives the unique
    maximum $\hat p=m/(m+S)$. If $S=0$, the likelihood increases strictly
    up to the included endpoint $p=1$.

modifiers:
  zero:
    truncated:
      status: implemented
      certified: 'yes'
      note: Existing conditional/hurdle/mixture fitter; every hold is forwarded.
    modified:
      status: implemented
      certified: 'yes'
      note: Existing conditional/hurdle/mixture fitter; every hold is forwarded.
    inflated:
      status: implemented
      certified: 'yes'
      note: Existing conditional/hurdle/mixture fitter; every hold is forwarded.
      limits:
      - model: zip
        firmness: proved
        when: The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed and positive inflation.
        detect: Size and probability are free; a signed Poisson-tail bound and complete finite-rectangle coverage
          select the same-inflation ZIP face, or certified positive-count and NB comparisons select the free-inflation
          face. Unresolved comparisons stop without selecting a limit.
        status: ZipLimit
        numerical: true
      - model: pois
        firmness: proved
        when: The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed, while inflation tends to
          zero.
        detect: A certified ordinary-NB reduction or complete global comparison selects the zero-inflation Poisson
          face. An unresolved global comparison is a numerical stop, not a non-existence report.
        status: PoissonLimit
        numerical: true
  truncate:
    status: implemented
    certified: 'yes'
    domain_bounded: parent parameter domain
    note: Concave conditional exponential family.
    limits:
    - when: prob tends to zero on a finite retained window
      detect: sample mean is at or above the limiting conditional mean
      name: uniform on the retained integer window
      status: UpperLimit
    - when: all observations equal the lowest retained integer above zero, outside the declared positive-truncation
        extension
      detect: sample is constant at the retained lower bound
      name: point mass at the lowest retained integer
      status: LowerLimit

---

The geometric distribution models the number of failures before the first success in independent Bernoulli trials.

## Details

The success probability $p$ controls how quickly the first success occurs.
The count starts at zero. The estimate is $\hat p=1/(1+\bar x)$.
All-zero data give the included boundary estimate $\hat p=1$, a point mass at zero.

Holding the probability evaluates the likelihood at that value. Every held
$0<p<1$ is compatible with every nonnegative integer sample. Holding $p=1$
requires every observation to be zero. Values outside the parameter domain
or incompatible with the counts are rejected.

## R notes

The count of failures starting at zero follows the convention of `stats::dgeom`.

## Decision history

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.
