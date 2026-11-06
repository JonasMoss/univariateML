---
id: ztnbinom
inference_numeric:
  - regime: "known zero negative-binomial size"
    available: true
    condition: "size = 0 held; 0 < prob < 1 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite size > 0 and 0 < prob < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-truncated negative binomial
support:
  type: integer
  lower: 1
  upper: inf
  closed:
  - true
  - false
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
  density: dztnbinom
  sampler: rztnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for the coefficient proposer; the certificate uses a separate signed enclosure.
    iterlim: Proposer iterations; certificate budget is 16 times min(iterlim, 100000).
    prob: "Optional held prob; NULL estimates it."
log_density: loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)
limits:
- model: ztpois
  firmness: proved
  when: As $r\to\infty$ and $p\to1$ with $r(1-p)/p\to\lambda>0$, the law tends to a zero-truncated Poisson law.
  detect: With both coefficients free and some count above one, the certificate proves that the finite-size profile increases throughout its domain and approaches the fitted zero-truncated Poisson law.
  status: PoissonLimit
examples: |
  set.seed(1)
  x <- rml(200, mlztnbinom())
  mlztnbinom(x)

group: modified counts
construction:
- truncation
relations:
  modification_of:
  - family: nbinom
    at:
      size: size
      prob: prob
    type: zero-truncated
  special_case_of:
  - family: zmnbinom
    at:
      size: size
      prob: prob
      p0: 0
fit:
  method: profile-search
  certified: 'yes'
  note: "ztnbinom/certificate.hpp: Conditional exact-arithmetic score and derivative bounds cover the full size domain, including analytic tail bounds; unresolved signs, extra extrema and exhausted budgets stop. Historical coefficients are retained only within a signed accuracy enclosure."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model negative-binomial counts sampled only when positive.
summary: Zero-truncated negative binomial provides a model for negative-binomial counts sampled only when positive.
aliases:
- "positive negative binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a general finite-MLE criterion and uniqueness result are not established here. The supremum can occur at the zero-truncated Poisson limit. All-one samples attain $p=1$ and leave $r$ unidentified. With finite $r>0$ held, the probability MLE exists and is unique, including $p=1$ for all-one data. Held $r=0$ gives a logarithmic-series model with a unique probability estimate. With $0<p<1$ held, there is a unique finite size maximum, possibly at $r=0$. Held $p=1$ requires only ones and leaves free size unidentified. Holding both coefficients leaves only likelihood evaluation.
  reason: >-
    For fixed positive size, the positive-count law is an exponential family in $\log(1-p)$. Its conditional mean ranges strictly from one to infinity, and its log likelihood is strictly concave in that natural parameter. At size zero the same argument applies to the logarithmic-series member. With interior probability held, likelihood is continuous at size zero and tends to zero as size tends to infinity, so a maximum is attained on $[0,\infty)$. At held interior probability, the size log likelihood is strictly concave: its second derivative is $-n/r^2-\sum_i\sum_{j=1}^{x_i-1}(r+j)^{-2}+nt^2/(4\sinh^2(rt/2))<0$, where $t=-\log p$. This does not establish joint uniqueness; joint fits instead require a sample-specific global certificate.
---

The zero-truncated negative binomial distribution models negative-binomial counts observed only when positive. The size $r$ is a nonnegative shape coefficient that can be fractional, and $p$ is the underlying negative-binomial probability.

## Details

Holding a positive size leaves a unique probability fit obtained by conditional-mean matching. Size zero is an attained logarithmic-series member with logarithmic parameter $1-p$. Probability one is the attained point mass at one. All-one samples therefore fail to identify a free size. Holding any finite size removes the infinite-size Poisson escape.

Holding an interior probability also removes that escape. Its strictly decreasing size score has a unique zero, or is already negative at the attained logarithmic endpoint. The certificate encloses that zero with padded signed comparisons. At a zero endpoint score too close to resolve, it stops.

With both coefficients free and some count above one, let $m$ be the positive sample mean, $t(r)=-\log p(r)$ its unique conditional-mean solution, and $w_j=n^{-1}\#\{i:x_i>j\}$. Write

$$A(r)=\sum_{j\ge1}\frac{w_j}{r+j},\quad H(r)=\sum_{j\ge1}\frac{j w_j}{r+j},\quad J(t)=1-\frac{t}{e^t-1}.$$

The mean equation is $m=r(e^t-1)/(1-e^{-rt})$, with its continuous $r=0$ extension $(e^t-1)/t$. The profile derivative satisfies

$$r Q'(r)/n=F(r)=mJ(t(r))-H(r).$$

The certificate uses the monotone mean equation to bracket $t(r)$. Since $t(r)$ decreases and $J$ increases, for $a\le r\le b$ it bounds $F$ between $mJ(t(b))-H(a)$ and $mJ(t(a))-H(b)$. It also bounds $F'$ to enclose a unique decreasing root or exclude a monotone segment. Put $E(u)=1/(e^u-1)-1/u$. Then

$$t'(r)=\frac{tE(rt)}{1+E(t)-rE(rt)},\quad
F'(r)=mJ'(t)t'(r)+\sum_{j\ge1}\frac{j w_j}{(r+j)^2}.$$

Here $E$ increases from $-1/2$ to zero, $J'$ is positive and decreasing from $1/2$, and the denominator is positive. These monotonicities give interval bounds for both derivatives. Root isolation subdivides geometrically, keeping every unresolved segment. It accepts only a profile with one certified decreasing root and positive/negative initial/final signs, or a profile with no roots and one sign throughout. A detected minimum or additional extrema cause a numerical stop; this is not a joint-unimodality assumption.

The initial tail uses $Q'(0)/n=A(0)-t(0)/2$ and bounds $Q'/n=A(r)-t[1+E(rt)]$ over $[0,a]$. The last factor increases in both arguments. For the infinite tail let $\lambda/(1-e^{-\lambda})=m$, $K=\sum j w_j$, $D=\sum j^2 w_j$ and $C=m\lambda/2-K$. The certificate uses

$$\frac{C}{r}-\frac{m^2\lambda+m\lambda^2/12}{r^2}
\ \le F(r)\le\ \frac{C}{r}+\frac{D}{r^2}.$$

To see this, put $u=rt(r)$. Then $u\le\lambda$ and $u\ge\lambda-2m\lambda/r$: the conditional Poisson mean has derivative at least $1/2$, and $(e^t-1)/t\le e^t$. Combine $t/2-t^2/12\le J(t)\le t/2$ with $K/r-D/r^2\le H(r)\le K/r$. These inequalities hold for every positive argument. The bounds prove a nonzero final sign once $r$ exceeds the corresponding ratio. No likelihood-near-tie comparison diagnoses the Poisson face. An unresolved $C$ stops instead.

A finite root is first isolated to relative size width at most $5\times10^{-7}$ and a log-probability span at most $5\times10^{-7}$. The historical grid/refinement fit is only a proposer. It is retained inside that enclosure, or after a separate padded sign check at size times $1\pm10^{-6}$ with log-probability span below $2\times10^{-6}$; otherwise the narrow-bracket midpoint is used. Probability is checked against a monotone mean bracket at the selected size. Finite maxima and the attained logarithmic member are accepted only after the global sign coverage succeeds. A globally increasing profile stops with the Poisson rate and log likelihood in `StopDetail`. Its supremum bounds evaluate each likelihood term over the signed rate bracket and pad the raw absolute terms; they do not assume that the rounded rate is exact. Numerical and budget stops return no coefficient estimate, with available global likelihood bounds (the universal upper bound is zero).

The certificate is exact in arithmetic conditional on computed comparisons. Its practical binary64 margin is $1024\epsilon(1+s)(1+M\epsilon)$ for a sum of absolute component scale $s$ and $M$ recurrence terms; conditional-mean comparisons use $256\epsilon(1+m)$. Recurrence sums are compensated. Small-argument $E$, $J$ and $J'$ use Bernoulli expansions below $0.1$, with omitted terms below $2.1\times10^{-16}$. The Poisson rate gets an independently checked padded bracket. Likelihood bounds additionally use $1024\epsilon$ times their raw absolute term scale; the feasible-candidate lower bound uses a conservative count/log-count scale. These allowances are practical guards, not outward-rounded arithmetic or a universal libm error proof. Counts above 100000, integer totals above $2^{52}$, unrepresentable probabilities, ambiguous signs and exhausted subdivision budgets stop. The budget is `16 * min(iterlim, 100000)` outer score/segment evaluations; conditional-mean bisections and the proposer have their own bounded loops.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) gives the original existence characterization. The [certificate report](../../../../../dev/research/evaluations/2026-11-06-nbfam-cert-report.md) records validation and the separate unresolved ZINB problem.

## R notes

Supplying `size` or `prob` holds that coefficient, while `NULL` estimates it. A held size may be zero. The attained members at `size = 0` and `prob = 1` follow actuar's conventions. Detected Poisson escapes stop with advice to use `mlztpois` and carry the limiting rate and likelihood. Invalid controls, incompatible held values, unresolved comparisons and certificate budget exhaustion stop. Increasing `iterlim` raises the subdivision budget; it does not make an unresolved comparison exact.

## Decision history

On 2026-11-06 the former log-size grid and $10^{-8}n$ face comparison were demoted to a coefficient proposer. That comparison could reject a finite maximum near a face and supplied no global guarantee. The certificate keeps the existing golden coefficients only after a signed accuracy enclosure. The first narrow-bracket prototype moved the historical truncated golden; the independent 60-digit size root differs from it by about $2.71\times10^{-7}$ relatively. The separate signed proposer enclosure preserves that regression without changing its tolerance. Joint uniqueness remains unproved in general; the implemented sample-specific sign and derivative coverage replaces that missing theorem for each successful fit.
