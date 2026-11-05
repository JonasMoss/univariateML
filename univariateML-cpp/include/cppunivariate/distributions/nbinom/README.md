---
id: nbinom
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite size > 0 and 0 < prob < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  symbol: 'r'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
r:
  density: stats::dnbinom
  sampler: stats::rnbinom
  control:
    reltol: "Relative tolerance on the Newton proposal; the certificate uses its own fixed accuracy."
    iterlim: "Maximum proposer iterations; certificate evaluation budget is 16 * min(iterlim, 100000)."
    size: "Optional fixed nonnegative size (including fractional values); NULL estimates size."
    prob: "Optional held prob; NULL estimates it."
log_density: "loggamma(x + size) - loggamma(size) - loggamma(x + 1) + size*log(prob) + x*log(1 - prob)"
limits:
  - model: pois
    firmness: proved
    when: 'the size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p=\bar x>0$ fixed'
    detect: 'both coefficients are free, $\bar x>0$, and $S^2=n^{-1}\sum_i(x_i-\bar x)^2\le\bar x$'
    status: Underdispersed
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  set.seed(1)
  x <- rml(200, mlnbinom())
  mlnbinom(x)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  special_case_of:
  - family: zinb
    at:
      size: size
      prob: prob
      pi: 0
  mixture_of:
  - family: pois
    at:
      lambda: mix
    mixed: lambda
    mixing:
      family: gamma
      at:
        shape: size
        scale: (1-prob)/prob
    when: size > 0 && prob < 1
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'Unique-root certificate with padded opposite score signs, analytic held-size probability, and a structured Poisson-limit stop. Exact-arithmetic certificate conditional on computed comparisons; unresolved arithmetic or budget stops.'
areas:
- counts and ecology
classification: classical
classification_reason: A standard model for overdispersed event counts with gamma-mixed Poisson intensity.
summary: Negative binomial provides a model for overdispersed event counts with gamma-mixed Poisson intensity.
aliases:
- "Pólya"
- "gamma–Poisson"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free and at least one positive count, a finite
    MLE exists and is unique if and only if $S^2>\bar x$, where
    $S^2=n^{-1}\sum_i(x_i-\bar x)^2$. Otherwise the supremum is the
    Poisson limit. For all-zero data, the maximum is attained by every
    pair with $r=0$ or $p=1$, so the coefficients are not unique.
    With $r>0$ held, the free probability has a unique MLE, including
    $p=1$ for all-zero data. With $0<p<1$ held, the free size has a unique
    MLE, including $r=0$ for all-zero data. Held $r=0$ or $p=1$ permits
    only zeros and leaves the other free coefficient unidentified.
    Holding both coefficients leaves only likelihood evaluation.
  reason: >-
    The free-fit criterion and uniqueness for positive-mean samples are
    proved by Aragón, J., Eberly, D. and Eberly, S. (1992),
    "Existence and uniqueness of the maximum likelihood estimator for the
    two-parameter negative binomial distribution", Statistics & Probability
    Letters, 15(5), 375–379, doi:10.1016/0167-7152(92)90157-Z.
    For held positive size, maximizing in probability gives
    $p=r/(r+\bar x)$. For held $0<p<1$, the size score is
    $n\log p+\sum_i\sum_{j=0}^{x_i-1}(r+j)^{-1}$.
    If some count is positive, it decreases strictly from infinity to
    $n\log p<0$, so it has one root. If all counts vanish, the likelihood
    is $p^{nr}$ and its maximum is attained at $r=0$.
---

The negative binomial distribution models nonnegative counts with overdispersion relative to a Poisson distribution. The size $r$ need not be an integer. The probability $p$ determines the mean together with $r$, through $r(1-p)/p$.

## Details

With both coefficients free and a positive sample mean, the probability profiles out as $p=r/(r+\bar x)$. The size estimate solves a scalar score equation. A finite solution exists exactly when the sample variance, computed with divisor $n$, exceeds the sample mean. Equality also gives an unattained Poisson supremum. Holding a positive size removes this escape. Holding a probability strictly below one leaves a strictly decreasing size score and a unique finite solution when a count is positive.

Size zero and probability one each describe the point mass at zero. These are attained members of the family, not escapes. All-zero samples therefore have a maximum, but the free pair of coefficients is not identified. Held size zero or held probability one is incompatible with positive observations. Holding both coefficients evaluates the likelihood without estimation.

## R notes

Supplying `size` holds any finite nonnegative size, including fractional values. `size = NULL` estimates it, and `prob = NULL` estimates the probability. All-zero data with free size return `size = 0`, preserving a held probability or choosing `prob = 0.5` as a representative when both are free. With positive size held, all-zero data estimate `prob = 1`. These attained boundary fits are silent. For positive-mean data whose variance is no greater than their mean, the free fit stops and recommends `mlpois`.

## Decision history

2026-10-30 (adapter independence): the held-size/free-probability precision guard now lives in the core. A positive count total with `size/(size+mean(x))` rounding to one returns `PrecisionFailure`, preserving the former adapter rejection.

The profile score is positive near zero for nonzero, overdispersed data,
and negative for sufficiently large size: its leading large-size term is
proportional to the mean minus the population-form sample variance. The
existence and uniqueness result of Aragon, Eberly and Eberly (1992) therefore
places the finite maximizer between any two sizes with opposite score signs.
The computed signs are floating-point approximations. The 2026-11-05 certificate below adds a practical sign margin and mandatory root enclosure; no outward-rounded interval arithmetic is claimed. Failure to find a usable bracket is a precision failure, not evidence of a Poisson escape.

2026-11-01 (variance cancellation): the raw-moment variance can fall below
its true value for large, tightly clustered counts. Before classifying a
sample as underdispersed the fitter recomputes its variance around the sample
mean. For example, the two counts $m-k,m+k$, with $m=k^2-1$, have mean $m$
and population variance $k^2$ exactly. This reduces cancellation in the
classification; it is not an exact-arithmetic certificate for every possible
input or platform.

## Scalar certificate (2026-11-05)

The uniqueness theorem quoted above means that any positive and negative
profile-score endpoints enclose the global maximizer. The held-probability
score is strictly decreasing and has the same property. `certificate.hpp`
uses the old solver only as a proposer; it doubles/halves until the signs
are resolved and bisects to relative width $5\times10^{-7}$. A small
residual alone cannot terminate the search: two nearby signed endpoints
are required. Historical coefficients are retained only inside the enclosure.
The profiled probability $r/(r+\bar x)$ increases with $r$ and has relative
sensitivity at most one, so the size enclosure also controls probability.

For integer counts, the score is evaluated through
$\psi(r+x)-\psi(r)=\sum_{j=0}^{x-1}(r+j)^{-1}$.
The implementation uses $r$ times the mean score to avoid small magnitudes.
Counts through 4096 use recurrence; larger counts use digamma at $r<16$,
and the Euler--Maclaurin expansion through the $r^{-10}$ term at $r\ge16$.
Near the Poisson face the leading mean terms are canceled algebraically: the recurrence sums $-j/(r+j)$ and the logarithmic term uses a degree-20 series for $\log(1+u)-u$ at $u<0.01$. Thus the arithmetic pad scales with the remaining terms rather than the canceled mean. The omitted term is bounded by $691r^{-11}/32760$ for this positive-real
asymptotic expansion. The positive-real remainder and its sign are given in
[NIST DLMF 5.11(ii)](https://dlmf.nist.gov/5.11.ii). The two digamma
remainders have the same sign, so the error of their difference is bounded
by the larger first-neglected-term bound. The practical sign pad adds
$128\epsilon(1+\text{absolute terms})(1+m\epsilon)$ (with the constant one omitted on the residual route) for $m$ distinct
counts; the small-$r$ digamma route adds an explicit pad depending on both
digamma magnitudes. This certificate is exact in arithmetic conditional on
the comparisons, with this practical floating-point margin; it does not
prove a universal libm error bound or use outward-rounded intervals.

The dispersion decision is guarded by a centered-moment comparison.
Ordinary integer-moment products within $2^{53}$ permit exact equality;
otherwise a comparison within a $256\epsilon$ scale allowance stops.
Unresolved root signs, nonfinite arithmetic, a probability rounding to one,
or exhaustion of $16\min(\mathrm{maxit},100000)$ certificate evaluations
return `PrecisionFailure`. A numerical search cap never diagnoses a
Poisson limit. The genuine `Underdispersed` stop includes the Poisson mean
and log likelihood in `StopDetail`; the binding exposes these on the error.

The certificate transfers to an actual call of `nbinom_mle` made by another
family. It does not certify a truncated/mixture likelihood, an outer search,
or a caller that discards failure statuses. No other family's certification
claim is changed here.
