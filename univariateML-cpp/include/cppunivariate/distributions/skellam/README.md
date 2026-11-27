---
id: skellam
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mu1 > 0 and mu2 > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Skellam
support: {type: integer, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu1
  symbol: '\mu_1'
  role: positive-count rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: mu2
  symbol: '\mu_2'
  role: negative-count rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dskellam
  sampler: rskellam
  control:
    mu1: "Optional fixed nonnegative finite positive-count rate; NULL estimates it."
    mu2: "Optional fixed nonnegative finite negative-count rate; NULL estimates it."
    tol: "Relative coefficient accuracy (default 1e-10)."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: "-mu1-mu2 + x*log(mu1/mu2)/2 + log(besseli(Abs(x),2*sqrt(mu1*mu2)))"
references:
- Skellam, J. G. (1946). The frequency distribution of the difference between two Poisson variates belonging to
  different populations. Journal of the Royal Statistical Society 109, 296.
- Johnson, N. L. (1959). On an extension of the connexion between Poisson and chi-squared distributions. Biometrika
  46, 352–362.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlskellam())
  mlskellam(x)

group: Poisson, binomial and negative binomial
construction:
- base
relations:
  convolution_of:
  - family: pois
    operation: difference
    components:
    - family: pois
      at:
        lambda: mu1
    - family: pois
      at:
        lambda: mu2
    when: mu1 > 0 && mu2 > 0
fit:
  method: profile-search
  certified: 'partial'
  note: 'skellam/README.md, Details. Conditional fits use strict concavity. Joint certification isolates profile maxima and strictly compares competing candidates, including attainable zero-rate faces.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model differences between independent Poisson event counts.
summary: Skellam provides a model for differences between independent Poisson event counts.
aliases: []
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With both rates free, every nonempty finite integer sample has an
    attained finite MLE in the closed nonnegative quadrant. General joint
    uniqueness remains open for integer samples. A mean-zero sample with a single positive magnitude has a unique fit. A constant sample $k$ has the maximum
    $(\mu_1,\mu_2)=(\max(k,0),\max(-k,0))$. With one rate held,
    the remaining rate has a unique finite MLE whenever the held model
    permits the observations. A held zero rate restricts the observations
    to the appropriate signed Poisson support. Holding both rates leaves
    only likelihood evaluation.
  reason: >-
    A convolution mass is at most the maximal mass of either Poisson
    factor. That bound tends to zero when either rate diverges. Continuity
    on the closed quadrant and a positive likelihood at some finite point
    therefore give compact positive likelihood level sets and attainment.
    With one positive rate held, the conditional paired-count variance
    is strictly less than its mean, giving strict concavity in the free
    rate. With a zero rate held, the problem is an ordinary signed Poisson
    fit. These arguments prove existence and conditional uniqueness,
    while general joint uniqueness remains open. Lean proves closed-quadrant
    attainment and the mean-zero single-magnitude case in
    `UnivariateML.Discrete3Skellam.joint_attained` and
    `UnivariateML.Discrete3Skellam.joint_unique_one_magnitude`, respectively.
---

The Skellam distribution describes the difference between two independent Poisson counts. The rates $\mu_1$ and $\mu_2$ describe the positive and subtracted counts, respectively. Their difference is the mean of the observed difference.

## Details

Exact real weights on integer support can give two global winners, even at mean zero. Integer-sample ties remain open. [Proof](../../../../../dev/research/evaluations/2026-11-25-skellam2-math-report.md#weighted-coexistence-and-integer-near-ties).

Zero rates are attained members, including the point mass at zero when both vanish. A positive or negative Poisson boundary may therefore be returned directly. Constant nonzero data need not give a point mass. With one positive rate fixed, the log likelihood is strictly concave in the other rate; a bracketed score solve finds its unique maximum, including the zero endpoint. Holding either or both rates fixed is supported; data outside a fixed zero-rate member's support are rejected.

The mean and variance identify both rates. Likelihoods are bounded by one per observation. Both-sided samples exclude zero-rate faces. A convolution mass is at most the maximal mass of either Poisson factor, which vanishes when that rate diverges. Continuity on the closed quadrant therefore gives a finite maximizer for every finite integer sample, also with fixed rates whenever the support permits the sample. Constant data $k$ have the joint MLE $(k,0)$ for $k>0$, $(0,-k)$ for $k<0$, and $(0,0)$ for zero: condition on the opposite Poisson count and use the decreasing maximal Poisson mass as its integer mode increases. For nonnegative data with positive mean, the inward profile derivative per observation at the Poisson face is $\bar x\,n^{-1}\sum_i 1/(x_i+1)-1$; reflect the data for the other face. This is a local boundary diagnostic, not a sufficient global rule; the fitter compares likelihoods.

## R notes

Supply `mu1` or `mu2` to hold the corresponding rate. Holding both evaluates the likelihood. Numerical failure stops fitting, and increasing `iterlim` raises the search budget.

## Decision history

2026-10-30 (TASK-125): the large-order expression divides before multiplying and uses log coordinates when the order/argument ratio overflows. The independent truth rows at count 1e300 exercise the change.

### Joint profile certificate

Strict maximization over the rate ratio at fixed product leaves the profile
with rate difference equal to the sample mean. With $t$ the smaller rate
and $d$ the absolute sample mean, the moderate-rate score and curvature are
bounded by continued fractions for the paired-count series. For
$z=2\sqrt{t(t+d)}$ above the transition, centered Riccati bounds on
$z I_k'(z)/I_k(z)$ and its derivative cover large rates without enumerating
paired counts. The upper score bound also excludes the infinite tail.

A cell with strict score sign contains no stationary point. Strict
curvature and endpoint signs distinguish a local minimum from an isolated
maximum. The maximum's bracket must meet the relative tolerance in both
rates. Strict likelihood comparisons between the remaining candidates use
the positive paired-count series and a geometric remainder. A feasible
Poisson face with exactly zero inward score can be classified locally by
the curvature on a cell reaching zero. Both faces are attained members.
[Score, curvature and strict comparisons](../../../../../dev/research/evaluations/2026-11-25-skellam-math-report.md#2-an-interior-certificate).

If the profile bounds cannot settle a comparison, a logarithmic grid and
local refinements can still return an unrefuted candidate. A joint interior
candidate on this route must pass the local Newton accuracy check and has
unknown global cardinality.

### Probability evaluation

Zero rates include the signed Poisson limits; sampling subtracts two independent Poisson variates.

### Boundary log tails (TASK-134)

When one rate is zero, CDF evaluation delegates to the Poisson log-tail
implementation, reflecting the count and reversing the tail for a negative
Poisson law. With both rates positive, probabilities below `1e-280` are summed
in log mass coordinates rather than taking a logarithm of a subnormal CDF.
Skellam masses are log-concave (convolution of a Poisson sequence and a
reflected Poisson sequence). In a descending tail, the next mass ratio thus
bounds subsequent ratios, giving a geometric remainder bound. Summation stops
when that bound is at most `2e-16` times the sum; inability to advance the count
returns NaN. This is a numerical stopping certificate using computed masses,
not a directed-rounding bound on the Bessel approximations.

For a negative count `v`, the positive Bessel series also gives
`I_(n+1)(z)/I_n(z) <= z/(2*(n+1))`, hence the next descending mass ratio
is at most `mu2/(1-v)`. This analytic geometric bound can certify a
single-term tail at enormous counts where the next integer is not
representable. It avoids treating representational stagnation as convergence.

The joint-rate cover counts curvature-bounded intervals, rather than local optimizer steps. On exhaustion it retains the best feasible pair of rates and a practical likelihood rounding margin; the global upper bound remains infinite.

Differences of adjacent rounded log masses can leave rate-score errors
independent of the scalar search width. The positive Bessel series retains the
conditional-count information needed to convert both rate scores into
coefficient errors.

On a one-sided Poisson face the free rate is the compensated sample mean.
For constant integer observations $k>0$, its exact value is $\mu_1=k$,
$\mu_2=0$, including when the unnormalized integer sum exceeds $2^{53}$.

## d/p/q/r

When one rate is at most 16 and the positive count exceeds $10^6$ with dominant rate/count below two, a positive Poisson convolution evaluates the mass without cancellation of large Debye exponents. Its successive term ratios decrease, giving a geometric remainder below $2\times10^{-16}$. Large-rate CDFs in this regime use a scaled, compensated positive sum for the smaller tail, using log concavity of the Skellam masses and a geometric remainder below $2\times10^{-14}$. A nonfinite seed log mass, or exhaustion of the million-cell guard without closing the bound, returns NaN. Near the mean, the Debye exponent also has the cancellation-free identity $-d^2\int_0^1(1-t)/\operatorname{hypot}(m+td,z)\,dt$, where $m=\mu_1-\mu_2$, $d=k-m$ and $z=2\sqrt{\mu_1\mu_2}$; a bounded Simpson rule evaluates it for $|d|/(\mu_1+\mu_2)<0.01$. Remainder certification is conditional on the computed floating-point comparisons.
