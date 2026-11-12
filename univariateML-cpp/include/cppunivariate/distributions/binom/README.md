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
  density: core_dbinom
  sampler: stats::rbinom
  control:
    size: "Optional fixed number of trials; NULL estimates size."
    abstol: "Unused by the free-size search (an exact integer bracket, not a continuous one); kept for the held-probability integer search's tail bound."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + x*log(prob) + (size - x)*log(1 - prob)"
limits:
  - model: pois
    firmness: proved
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

modifiers:
  zero:
    truncated:
      status: implemented
      certified: partial
      note: Existing conditional/hurdle/mixture fitter; every hold is forwarded.
      limits:
      - model: ztpois
        firmness: proved
        when: As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated
          Poisson law.
        detect: A strict positive factorial-moment gap at the mean-matched positive-Poisson face supplies a signed
          tail cutoff, and complete finite-prefix coverage excludes every smaller size. Comparisons not passing
          both checks return a finite search result with `certified = FALSE`.
        status: PoissonLimit
    modified:
      status: implemented
      certified: partial
      note: Existing conditional/hurdle/mixture fitter; every hold is forwarded.
      limits:
      - model: zmpois
        firmness: proved
        when: As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated
          Poisson law. The zero probability $p_0$ remains fixed, giving a zero-modified Poisson law.
        detect: The positive-count fit passes the signed-tail and complete finite-prefix face certificate in ztbinom;
          the Bernoulli zero-indicator likelihood is added with the same held or fitted zero probability.
        status: PoissonLimit
      - model: zmpois
        firmness: proved
        when: As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated
          Poisson law. The zero probability $p_0$ remains fixed, giving a zero-modified Poisson law.
        detect: The positive-count fit passes the signed-tail and complete finite-prefix face certificate in ztbinom;
          the Bernoulli zero-indicator likelihood is added with the same held or fitted zero probability.
        status: PoissonLimit
        fixed:
        - p0
    inflated:
      status: implemented
      certified: partial
      note: Existing conditional/hurdle/mixture fitter; every hold is forwarded.
      limits:
      - model: zip
        firmness: proved
        when: The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive
          rate.
        detect: The relaxed zero-modified signed-tail and finite-prefix certificate proves an admissible ZIP face
          wins; samples without zeros delegate to the ordinary binomial certificate. Noninflated faces also use
          a compact rate certificate that proves inflation is inactive near the Poisson optimum. Other comparisons
          remain unresolved.
        status: ZipLimit
      - model: zip
        firmness: proved
        when: The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive
          rate.
        detect: Zero inflation or a sample without zeros delegates to the binomial face decision, retaining held
          inflation. With observed zeros and nonzero held inflation, an inflation-uniform or localized signed rate
          bound and complete prefix exclusion prove the held ZIP face wins; otherwise the comparison remains unresolved.
        status: ZipLimit
        fixed:
        - pi
      - model: zip
        firmness: proved
        when: As trial count tends to infinity and success probability tends to zero with their product fixed, the
          law tends to zero-inflated Poisson with the same inflation.
        detect: Signed tail and complete finite-prefix exclusion prove escape, while the held-inflation ZIP certificate
          cannot order multiple face-rate maxima at its comparison margin.
        status: FaceTie
        fixed:
        - pi
  truncate:
    status: implemented
    certified: 'yes'
    requires_held:
    - size
    domain_bounded: parent parameter domain
    note: Concave conditional exponential family; size held only.
    holds:
    - held: []
      status: not-implemented
      reason: General truncation requires size held; zero truncation also supports size free.
    limits:
    - name: point mass at lowest support point
      firmness: proved
      when: With size fixed, $p\to0$ on a nonsingleton window whose lowest retained count is positive.
      detect: Size is held, probability is free, and every observation equals the strictly positive lowest count
        of a nonsingleton retained window.
      status: LowerPointMass
    - name: point mass at highest support point
      firmness: proved
      when: With size fixed, $p\to1$ on a nonsingleton window whose highest retained count is below $n$.
      detect: Size is held, probability is free, and every observation equals the highest count of a nonsingleton
        retained window below $n$.
      status: UpperPointMass
    - name: point mass at highest support point
      firmness: proved
      when: The size satisfies $n\to\infty$ with $0<p<1$ held and a finite upper truncation bound.
      detect: Size is free, probability is held strictly between zero and one, and constant data equal the finite
        upper truncation edge and no feasible size can make the retained support singleton.
      fixed:
      - prob
      status: UpperPointMass
    - model: tpois
      firmness: proved
      when: The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same
        truncation window.
      detect: A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment
        certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the
        comparison remains unresolved.
      fixed: []
      status: TpoisLimit
    - model: tpois
      firmness: proved
      when: The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same
        truncation window.
      detect: A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment
        certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the
        comparison remains unresolved.
      fixed:
      - a
      status: TpoisLimit
    - model: tpois
      firmness: proved
      when: The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same
        truncation window.
      detect: A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment
        certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the
        comparison remains unresolved.
      fixed:
      - b
      status: TpoisLimit
    - model: tpois
      firmness: proved
      when: The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same
        truncation window.
      detect: A signed factorial-moment tail with complete finite-prefix exclusion, or an exact finite-window moment
        certificate, proves the truncated-Poisson face wins on the held or profiled integer window; otherwise the
        comparison remains unresolved.
      fixed:
      - a
      - b
      status: TpoisLimit

---

The binomial distribution models the number of successes in a fixed number of independent Bernoulli trials.

## d/p/q/r

### Concentrated count density

The identity mass(k;n,p) = beta_density(p;k+1,n-k+1)/(n+1) replaces three large log-gamma terms with the beta density's compensated physical mean displacement. All three count increments must be exact in binary64 before this branch applies.

Counts below 16 use the finite binomial product directly, including Poisson limits with arbitrarily large represented trial counts. Once adding one ceases to be exact, two nonnegative binomial deviances replace the shifted-beta identity. FMA retains np-k; Stirling remainders at counts at least 16 bound the normalizer truncation.

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

### Double-width intermediate likelihood (2026-11-01)

When the maximum observed count is at most 100000 and intermediates are
double-width, the likelihood
forms the falling-factorial correction as a sum of `log1p(-j/size)` and
combines the size powers with the fitted probability. This avoids subtracting
large log-gamma values. Prefix sums make this O(maximum count + number of
distinct observations) in work and O(maximum count) in temporary memory.
Larger counts retain the existing calculation; its accuracy on double-width
targets is an open validation gap. The large-size
profile comparison in `test.cpp` exercises the moderate-count path.
