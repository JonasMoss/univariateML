---
id: binom
inference_numeric:
  - regime: interior
    available: true
    condition: "known integer size >= 1, 0 < prob < 1; prob free and size held"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "size = 0, prob = 0 or prob = 1"
    reason: "the law is degenerate, so the probability information is zero; unknown size is discrete"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  simulate: 10.0
  integer: yes
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: stats::dbinom
  sampler: stats::rbinom
  control:
    size: "Optional fixed number of trials; NULL estimates size."
    abstol: "Unused by the free-size search (an exact integer bracket, not a continuous one); kept for the held-probability integer search's tail bound."
    iterlim: "Iteration budget for the free-size bracket-and-bisect search (no fixed magnitude limit: raise this to search further) and for the held-probability integer search."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + x*log(prob) + (size - x)*log(1 - prob)"
limits:
  - model: pois
    firmness: numerical
    when: The trial count tends to infinity and the probability tends to zero with $np=\bar x>0$ fixed
    detect: Both $n$ and $p$ are free and $0<\bar x\le m^{-1}\sum_i(x_i-\bar x)^2$, where $m$ is sample size
    status: Underdispersed
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley. doi:10.1002/0471715816."
  - "Olkin, I., Petkau, A. J., and Zidek, J. V. (1981). A comparison of n estimators for the binomial distribution. Journal of the American Statistical Association, 76(375), 637-642. doi:10.1080/01621459.1981.10477697."
  - "Carroll, R. J. and Lombard, F. (1985). A Note on N Estimators for the Binomial Distribution. Journal of the American Statistical Association, 80(390), 423-426."
  - "DasGupta, A. and Rubin, H. (2005). Estimation of binomial parameters when both n,p are unknown. Journal of Statistical Planning and Inference, 130(1-2), 391-404."
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlbinom())
  mlbinom(x, size = 10)

summary: The binomial distribution counts successes in a fixed number of independent trials with a common success probability.
classification: classical
classification_reason: A standard model for success counts from independent trials with a common success probability.
aliases: []
key_references:
- 1
- 2

group: Poisson, binomial and negative binomial
construction:
- base
relations:
  special_case_of:
  - family: zib
    at:
      size: size
      prob: prob
      pi: 0
  - family: tbinom
    at:
      size: size
      prob: prob
      a: -1
      b: size
  - family: cmb
    at:
      size: size
      prob: prob
      nu: 1
    when: size <= 10000000
fit:
  method: integer-search
  certified: 'yes'
  note: 'binom/README.md, Details; binom/mle.hpp (public fitter and held overloads); binom/test.cpp: Free-size profile
    is unimodal (DeRiggi result cited in Details); held-probability search excludes the entire integer tail. Fixed-size
    probability is analytic; work-budget exhaustion stops.'
areas:
- counts and ecology
existence:
  uniqueness: unknown
  statement: >-
    For a nonempty sample of nonnegative integer counts with positive mean,
    a finite MLE with both $n$ and $p$ free exists exactly when
    $\bar x>v$, where $v=m^{-1}\sum_i(x_i-\bar x)^2$ and $m$ is sample size.
    All-zero data also attain a maximum, but do not identify the parameters.
    Integer maximizers need not be unique. With $n>0$ held at an integer
    at least $\max_i x_i$, $\hat p=\bar x/n$ is unique.
    Holding $n=0$ requires all-zero data and leaves $p$ unidentified.
    With $0<p<1$ held, a finite integer maximum exists, possibly at two
    adjacent sizes. Holding $p=0$ requires all-zero data and leaves $n$
    unidentified. Holding $p=1$ requires constant counts and fixes $n$
    to that count. With both coefficients held, the observations must
    have positive probability under the supplied model.
  reason: >-
    The positive-mean existence criterion and profile unimodality are given
    by DeRiggi (1983), cited in Details. Unimodality does not exclude a tie
    between adjacent integers. For a held interior probability, the adjacent
    log-likelihood difference is
    $m\log(1-p)-\sum_i\log(1-x_i/(n+1))$.
    It is strictly decreasing when some count is positive and tends to
    $m\log(1-p)<0$, certifying a finite maximum and at most two adjacent
    maximizers. For example, counts all equal to one and held $p=1/2$
    give equal maxima at $n=1$ and $n=2$. All-zero counts give a decreasing
    likelihood in $n$ for held $0<p<1$. The held-size result follows from
    the Bernoulli log likelihood. The uniqueness label does not assert a
    unique joint integer estimate for every sample.

---

The binomial distribution models the number of successes in a fixed number of independent Bernoulli trials.

## Details

The trial count $n$ is a nonnegative integer and $p$ is the common success
probability. Holding the trial count estimates only the probability and
requires $n\ge\max_i x_i$. With $n>0$, the estimate is $\bar x/n$.
At $n=0$, only zero counts are possible and the probability is unidentified.

When both coefficients are free, the fit profiles the probability at
$\bar x/n$ and searches the integer trial counts. For positive-mean data,
a finite maximum exists exactly when the sample mean exceeds the variance
computed with divisor equal to sample size. Otherwise the supremum is the
Poisson limit. The free-size profile is unimodal by DeRiggi, D. F. (1983),
“Unimodality of Likelihood Functions for the Binomial Distribution”,
Journal of the American Statistical Association, 78(381), 181–183,
[doi:10.1080/01621459.1983.10477949](https://doi.org/10.1080/01621459.1983.10477949).
Bracketing and bisection locate the first nonpositive adjacent profile
increment. This selects the smaller size if two adjacent sizes tie.
All-zero data have many maximizers, represented by $n=0,p=0$.
Constant positive counts give $n=\bar x,p=1$.

A held probability is preserved exactly. For $0<p<1$, adjacent likelihood
ratios bound the remaining integer tail before the fit returns. Boundary
probabilities require positive likelihood for the observations. Holding
$p=0$ requires all-zero data and leaves the trial count unidentified,
represented by zero. Holding $p=1$ requires constant counts and sets the
trial count to that value. With a held zero trial count and free probability,
the reported probability is zero by convention.

The estimated trial count can be unstable. Alternatives are discussed by
Carroll and Lombard (1985) and DasGupta and Rubin (2005). These references
concern estimation choices rather than a guarantee of numerical accuracy.

## R notes

Supply `size` explicitly. A numeric value holds the trial count, while
`size = NULL` requests its estimation. Supplying `prob` holds the success
probability. When both are free and the Poisson supremum is not attained,
the fit stops with advice to use `mlpois` or supply `size`.

The integer searches stop on exhaustion of `iterlim`. The free-size search
has no fixed magnitude cap, but finite arithmetic can limit resolvable
integer sizes. Raising `iterlim` addresses a work-budget failure and does
not guarantee resolution of every mathematical maximum. The held-probability
search also stops if its integer-resolution guard is reached.

## Decision history

### Diagnostic status contract (2026-10-31)

Invalid and empty samples carry non-success statuses, including delegate paths.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp`.
