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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
- Brøns, H. and Jensen, S. T. (1988). Maximum Likelihood Estimation in the Negative Binomial Distribution. University of Copenhagen, Institute of Mathematical Statistics, Preprint 1988 No. 8.
- 'Levin, B. and Reeds, J. (1977). Compound Multinomial Likelihood Functions are Unimodal: Proof of a Conjecture of I. J. Good. Annals of Statistics, 5(1), 79–87. doi:10.1214/aos/1176343741.'
- Wang, Y. (1996). Estimation problems for the two-parameter negative binomial distribution. Statistics & Probability Letters, 26(2), 113–114. doi:10.1016/0167-7152(94)00259-2.
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
    proved below by expanding the convexity argument of Brøns and Jensen
    (1988), including strictness and the equality case. Levin and Reeds
    (1977) is the classical published reference; its proof was not accessible
    in this check. Aragón, Eberly and Eberly (1992) is not used as a proof
    authority because Wang (1996) reports an error in its argument.
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

## Existence and uniqueness proof

Let $x_1,\ldots,x_n$ be nonnegative integers, $n\ge1$, with
$m=\bar x>0$ and $v=n^{-1}\sum_i(x_i-m)^2$. This proof expands the
convexity construction of [Brøns and Jensen (1988), §§2–3](https://www.math.ku.dk/bibliotek/arkivet/preprints-fra-ims/1988/preprint_1988_-_no._8_br_ns__hans_and_jensen__s_ren_tolver_-_maximum_likelihood_estimation_in_the---.pdf).
It does not depend on the disputed Aragón–Eberly–Eberly proof. The
[source-access report](../../../../../dev/research/evaluations/2026-11-07-nb-proof-report.md)
distinguishes the inspected preprint from the published proofs and Wang's note.

For each $r>0$, strict concavity in $p$ gives the unique conditional
maximum $p(r)=r/(r+m)$. Write $f(r)=-\ell(r,p(r))$ and let
$\sum_{i,j}$ denote $\sum_{i=1}^n\sum_{j=0}^{x_i-1}$, with empty sums zero.
Then

$$
 f'(r)=n\log(1+m/r)-\sum_{i,j}\frac1{r+j},\qquad
 f''(r)=-\frac{nm}{r(r+m)}+\sum_{i,j}\frac1{(r+j)^2}.
$$

Use the increasing coordinate $b(r)=r\log(1+m/r)/m$, from $(0,\infty)$
onto $(0,1)$. Direct differentiation gives

$$
 b'(r)=\frac{\log(1+m/r)-m/(r+m)}m>0,\qquad
 b''(r)=-\frac{m}{r(r+m)^2}<0.
$$

We first prove a curvature inequality. For every integer $q\ge1$, set

$$T_q(r)=r(r+m)^2\sum_{k=0}^{q-1}(r+k)^{-2}.$$

Choose $d=\min(q-1,\lfloor m\rfloor)$. The telescoping identity

$$
 r\sum_{k=0}^d\frac1{(r+k)^2}
 =\frac{d+1}{r+d}+\sum_{k=1}^d\frac{k}{(r+k)^2(r+k-1)}
$$

holds by subtraction of consecutive $d$ values (and directly for $d=0$).
Consequently

$$
 T_q(r)=(d+1)\frac{(r+m)^2}{r+d}
 +\sum_{k=1}^d\frac{k(r+m)^2}{(r+k)^2(r+k-1)}
 +\sum_{k=d+1}^{q-1}\frac{r(r+m)^2}{(r+k)^2}.
$$

Every term has nonnegative second derivative. For the first term use
$(r+m)^2/(r+d)=r+2m-d+(m-d)^2/(r+d)$.
For $1\le k\le d\le m$, the logarithm of the positive middle factor has
second derivative

$$-\frac2{(r+m)^2}+\frac2{(r+k)^2}+\frac1{(r+k-1)^2}>0,$$

which implies strict convexity of that factor. Every final-sum index
satisfies $k>m$, and direct differentiation gives

$$
 \left[\frac{r(r+m)^2}{(r+k)^2}\right]''
 =\frac{2(m-k)(m-3k)r+4mk(k-m)}{(r+k)^4}>0.
$$

Strictness always holds: if $d=0$ the first term has positive curvature
because $m>0$; if $d\ge1$ a middle term does. Thus $T_q''(r)>0$.

Now define

$$
 h(r)=\frac{f''(r)}{b''(r)}
      =n(r+m)-\frac1m\sum_{i:x_i>0}T_{x_i}(r).
$$

At least one count is positive, so $h''<0$. Expanding the finite sums at
infinity gives the finite limit

$$
 h(\infty)=\frac{\sum_i x_i(x_i-1)-nm^2}{m}
          =\frac{n(v-m)}m.
$$

A strictly concave twice differentiable function with a finite limit at
infinity must have $h'(r)>0$ everywhere: if $h'$ were nonpositive at any
point, its strict decrease would make it bounded above by a negative
constant thereafter, contradicting the finite limit.

Since $f'(\infty)=b'(\infty)=0$ and their second derivatives are
integrable on $[r,\infty)$, their ratio is a weighted average:

$$
 G(r):=\frac{f'(r)}{b'(r)}
 =\frac{\int_r^\infty h(t)[-b''(t)]\,dt}
        {\int_r^\infty[-b''(t)]\,dt}>h(r).
$$

Both integrals converge because $h$ is bounded on this interval.
It follows that

$$G'(r)=\frac{b''(r)}{b'(r)}[h(r)-G(r)]>0.$$

Thus $f$ is strictly convex in $b$, although the original profile score
need not be monotone in $r$. If $a=\#\{i:x_i>0\}$, then
$f'(r)=-a/r+O(\log(1/r))$ near zero, so $G(0+)=-\infty$.
The same weighted-average formula gives $G(\infty)=n(v-m)/m$.
Continuity and strict increase therefore give exactly one root if
$v>m$, and none if $v\le m$. In the latter case $G(r)<0$ at every finite
$r$, including when $v=m$.

The profile score is $\ell_{\rm prof}'=-f'$. When $v>m$ it is strictly
positive before its unique root and strictly negative after it. The root
is therefore the unique global size maximum, and its unique profiled
probability completes the joint MLE. When $v\le m$ the profile increases
strictly to its unattained Poisson supremum. Indeed, for fixed integer $x$,

$$
 P_{r,p(r)}(x)=\frac{m^x}{x!}
 \left[\prod_{j=0}^{x-1}(1+j/r)\right](1+m/r)^{-(r+x)}
 \longrightarrow e^{-m}m^x/x!.
$$

Every unprofiled likelihood is bounded by its profile. With positive
observations the finite boundary members $r=0$ and $p=1$ have zero
likelihood, so none adds a maximizer. The size-zero endpoint of the
profile also has zero likelihood. If all observations are zero, instead,
$L=p^{nr}$ on the interior and its extension has maximum one precisely
on the union $r=0$ or $p=1$ in the declared parameter domain.

## R notes

Supplying `size` holds any finite nonnegative size, including fractional values. `size = NULL` estimates it, and `prob = NULL` estimates the probability. All-zero data with free size return `size = 0`, preserving a held probability or choosing `prob = 0.5` as a representative when both are free. With positive size held, all-zero data estimate `prob = 1`. These attained boundary fits are silent. For positive-mean data whose variance is no greater than their mean, the free fit stops and recommends `mlpois`.

## Decision history

2026-10-30 (adapter independence): the held-size/free-probability precision guard now lives in the core. A positive count total with `size/(size+mean(x))` rounding to one returns `PrecisionFailure`, preserving the former adapter rejection.

The profile score is positive near zero for nonzero, overdispersed data,
and negative for sufficiently large size: its leading large-size term is
proportional to the mean minus the population-form sample variance. The
existence and uniqueness proof below therefore places the finite maximizer
between any two sizes with opposite score signs.
The computed signs are floating-point approximations. The 2026-11-05 certificate below adds a practical sign margin and mandatory root enclosure; no outward-rounded interval arithmetic is claimed. Failure to find a usable bracket is a precision failure, not evidence of a Poisson escape.

2026-11-01 (variance cancellation): the raw-moment variance can fall below
its true value for large, tightly clustered counts. Before classifying a
sample as underdispersed the fitter recomputes its variance around the sample
mean. For example, the two counts $m-k,m+k$, with $m=k^2-1$, have mean $m$
and population variance $k^2$ exactly. This reduces cancellation in the
classification; it is not an exact-arithmetic certificate for every possible
input or platform.

## Scalar certificate

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

### Probability evaluation near the Poisson limit

At fixed mean and large size, subtracting `logGamma(size)` from `logGamma(size+x)` loses the finite rising-factorial correction. The PMF uses the shared shifted gamma-ratio expansion; the exact size and count enter before the nearly equal gamma terms are combined. Log-tail continuation starts from this PMF, so its relative error also controls the probability on the Poisson face.

### Concentrated count density

The identity mass(k;r,p) = p*beta_density(p;r,k+1)/(r+k) retains the beta mean displacement without subtracting a large gamma normalizer. The branch requires an exact k+1 and a finite r+k; the zero-count mass is evaluated directly as p^r.

For larger counts whose unit shift rounds away, the mass instead uses a gamma-binomial split with two nonnegative deviances. A scaled product expansion retains p*(r+k)-r without forming an overflowing sum. The log normalizer uses log(r+k) through a ratio and a sum; all Stirling arguments are at least 16.
