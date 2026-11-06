---
id: bnbinom
inference_numeric:
  - regime: "identifiable interior submodel"
    available: true
    condition: "finite size > 0, alpha > 0 and beta > 0; any nonempty supported free subset except size = beta with both size and beta free"
    classification: "dev/research/evaluations/2026-11-04-dqm-c-report.md#b-beta-negative-binomial-exact-singular-set"
  - regime: "singular information"
    available: false
    condition: "finite size > 0, alpha > 0 and beta > 0; size = beta with both size and beta free"
    reason: "DQM holds but the free information has a null direction; ordinary observed or sandwich covariance is unavailable"
    classification: "dev/research/evaluations/2026-11-04-dqm-c-report.md#b-beta-negative-binomial-exact-singular-set"
name: Beta negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  symbol: 'r'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: beta
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dbnbinom
  sampler: rbnbinom
  control:
    gradtol: "Scalar log-parameter bracket width (at most 1e-8); otherwise tolerance on the average log likelihood score in log parameters."
    iterlim: "Scalar certificate refinement iterations and evaluation budget; otherwise maximum BFGS iterations per deterministic start."
log_density: "loggamma(size+x)-loggamma(size)-loggamma(x+1)+loggamma(alpha+size)+loggamma(beta+x)-loggamma(alpha+size+beta+x)-loggamma(alpha)-loggamma(beta)+loggamma(alpha+beta)"
limits:
  - model: nbinom
    firmness: proved
    when: 'The shapes satisfy $\alpha,\beta\to\infty$ with their ratio and $r>0$ fixed. Exchanging $r$ and $\beta$ gives the corresponding path with $\beta>0$ fixed.'
    detect: 'The shape $\alpha$ and at least one of $r$ and $\beta$ are free. A reachable negative binomial limit matches or exceeds the best attempted finite log likelihood within $10^{-8}$ times the sample size. This numerical comparison is not a global non-existence proof.'
    status: NegativeBinomialLimit
    numerical: true
  - model: pois
    firmness: proved
    when: 'All three shapes tend to infinity with $r\beta/\alpha$ tending to a finite positive mean.'
    detect: 'All coefficients are free. The Poisson limit matches or exceeds the best attempted finite log likelihood within $10^{-8}$ times the sample size. This numerical comparison is not a global non-existence proof.'
    status: PoissonLimit
    numerical: true
  - model: pois
    firmness: proved
    when: 'A free shape satisfies $r\to0$, $\beta\to0$, or $\alpha\to\infty$, while the other two coefficients stay fixed and positive.'
    detect: 'Every observation is zero and at least one coefficient is free.'
    status: PointMass
references:
- Irwin, J. O. (1975). The Generalized Waring Distribution. Part I. JRSS A 138(1), 18-31. doi:10.2307/2345247.
- Xekalaki, E. (1984). The Bivariate Generalized Waring Distribution and its Application to Accident Theory. JRSS
  A 147(3), 488-498. doi:10.2307/2981580.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlbnbinom())
  mlbnbinom(x)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  mixture_of:
  - family: nbinom
    at:
      size: size
      prob: mix
    mixed: prob
    mixing:
      family: beta
      at:
        shape1: alpha
        shape2: beta
fit:
  method: multistart
  certified: 'partial'
  note: 'bnbinom/README.md, Scalar certificates; bnbinom/mle.hpp: Every one-free-shape fit has a single-crossing global certificate. Fits with two or three free shapes retain the uncertified multistart search; all-held evaluation is not counted as certification.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model clustered event counts with power-law tails from beta mixing.
summary: Beta negative binomial provides a model for clustered event counts with power-law tails from beta mixing.
aliases:
- "generalized Waring"
- "inverse Markov–Pólya"
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, no general sample criterion for a finite
    maximum is asserted here. Negative binomial and Poisson limits can
    compete with finite fits. Exchanging $r$ and $\beta$ leaves the law
    unchanged, so an off-diagonal maximizer cannot give a unique labeled
    coefficient vector. All-zero samples have no finite MLE whenever any
    coefficient is free. Holding coefficients removes some limiting paths,
    and every one-free-shape fit has a unique finite maximum if at least one
    observation is positive. With alpha held and both other shapes free, a
    finite maximum exists for such samples, but labeled uniqueness need not
    hold. The remaining multivariate searches have no global certificate. Holding all coefficients leaves only
    likelihood evaluation, including for all-zero data.
  reason: >-
    The probability mass is symmetric in $r$ and $\beta$ after expanding
    the beta and gamma factors. Its mass at zero is strictly below one
    at every finite positive parameter vector. It tends to one as a free
    $r$ or $\beta$ tends to zero, or as a free $\alpha$ tends to infinity,
    with the other coefficients fixed and positive. Comparing numerical
    stationary points with reachable limiting models does not establish
    that all finite maxima have been found. The scalar score and the
    conditional existence proofs appear below.
---

The beta negative binomial mixes a negative binomial success probability with a beta distribution. Also called the generalized Waring distribution, it models counts with power-law tails. The shape $r$ is the underlying negative binomial size. The shapes $\alpha$ and $\beta$ govern the beta mixing distribution for its success probability.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) describes the limiting paths and the scope of the boundary comparisons.

Any subset of $r$, $\alpha$ and $\beta$ may be held fixed. When $r$ and $\beta$ are both free, swapping them gives exactly the same distribution. The returned representative satisfies $r\le\beta$. No ordering is imposed when either is held.

On the diagonal $r=\beta$ the swap fixes the law, so the score along $(1,0,-1)$ in $(r,\alpha,\beta)$ vanishes and the Fisher information has rank 2 of 3. The diagonal is therefore a singular boundary of the identified set $r\le\beta$: Wald intervals for $r$ and $\beta$ separately are unreliable near it. Elsewhere in the interior the information is nonsingular ([classification](../../../../../dev/research/evaluations/2026-11-04-dqm-c-report.md)).

With two or three shapes free, a deterministic multistart search compares finite stationary points with every reachable negative binomial and Poisson face. Limiting models win computed likelihood ties within $10^{-8}$ times the sample size. This search does not certify global optimality or assume unimodality. An unresolved score or range endpoint stops the fit. With all coefficients supplied, the likelihood is evaluated directly. For all-zero data, any free coefficient allows the likelihood to approach one only at an excluded boundary. With every coefficient held, the all-zero likelihood is evaluated at the supplied positive values.


## Scalar certificates

Hold two shapes at any finite positive values. For a sample containing a
positive count, the remaining shape has a unique finite global maximizer.
This is a single-crossing result for the score, not a claim of joint concavity.
Let $A(t)=n^{-1}\sum_i(1-e^{-x_i t})$. It is increasing and concave, with
$A(0)=0$ and $A'(0)>0$. The ordinary alpha score divided by $n$ is

$$
\int_0^\infty \frac{e^{-(\alpha+r+\beta)t}}{1-e^{-t}}
\left[(e^{rt}-1)(e^{\beta t}-1)-A(t)\right]dt.
$$
The bracket is strictly convex, starts at zero with negative derivative,
and tends to infinity. It changes sign exactly once, from negative to positive.
For the size score the corresponding integral is

$$
\int_0^\infty \frac{e^{-(r+\alpha)t}}{1-e^{-t}}
\left[(e^{\alpha t}-e^{-\beta t})A(t)-(1-e^{-\beta t})\right]dt.
$$
Its bracket has the sign of
$[1+(e^{\alpha t}-1)/(1-e^{-\beta t})]A(t)-1$.
The ratio is strictly increasing: its numerator is positive and strictly
convex from zero, and its denominator positive and concave from zero.
Thus this bracket also changes sign once from negative to positive.
The beta score follows by exchanging $r$ and $\beta$.

For any of these integrals, at a zero of the score, increasing the free shape
multiplies the integrand by a strictly decreasing exponential. Relative to
its value at the bracket's sign change, this increases the negative part
and decreases the positive part, so the score becomes strictly negative.
Decreasing the shape makes it strictly positive. There can be only one root.
The alpha score is asymptotic to $n/\alpha$ at zero and
$-\sum_i x_i/\alpha$ at infinity. The size score is asymptotic to
$\#\{i:x_i>0\}/r$ at zero and $-n\alpha/r$ at infinity; beta is symmetric.
These endpoint signs prove root existence and global optimality. All-zero
samples instead have only the excluded point-mass supremum described above.

The implementation expands a bracket in log shape, then bisects while
preserving positive and negative score endpoints. It accepts only a bracket
of width at most $\min(\mathtt{gradtol},10^{-8})$ and returns its midpoint.
A sign is used only beyond $128\epsilon$ times the average sum of absolute
digamma terms. An unresolved midpoint triggers quarter-point comparisons;
unresolved progress or an unrepresentable shape stops numerically.
Expansion and refinement share at most
$128\min(\mathtt{iterlim},100000)$ score evaluations; refinement also has
$\min(\mathtt{iterlim},100000)$ steps. Exhaustion stops with a budget condition.
No small-score or optimizer fallback returns an unchecked estimate.

The certificate is exact-arithmetic conditional on these computed comparisons,
with the stated practical floating-point margin; it is not outward-rounded
interval arithmetic. Diagnostic witness likelihood bounds use
$10^{-10}n(1+|\ell|/n)$ around the evaluated value and leave the global upper
bound infinite. They do not certify a stopped candidate. Extreme cancellation
can produce a numerical stop even though a unique mathematical maximizer exists.

With alpha held and both other shapes free, a positive count also ensures
existence. If either shape tends to zero, its positive-count masses tend to
zero uniformly in the other shape, since
$p(x)\le (r)_x/x!$ and $p(x)\le(\beta)_x/x!$ for $x>0$.
If a shape tends to infinity while the other stays bounded below, the count
law stochastically escapes to infinity (use its beta-mixture representation
and symmetry). Every observed finite-count probability then tends to zero.
These alternatives exhaust escapes from compact subsets of the positive
quadrant, so every positive likelihood superlevel set is compact.
This proves attainment, without certifying the current two-dimensional search
or establishing uniqueness. On the golden sample with $\alpha=2$, the
finite witness $(r,\alpha,\beta)=(1,2,5)$ has log likelihood
$-1276.3456581776477$, while every diagonal point has log likelihood below
$-1276.4458273656892$. Compactness and symmetry therefore give at least two
distinct labeled global maximizers. The
[diagonal-exclusion reproducer](../../../../../dev/research/evaluations/2026-11-06-bnbinom-cert-data/nonuniqueness.py)
uses monotone mass-ratio bounds on 861 log-shape segments and bounds both
exterior tails. Its comparisons use 60-digit arithmetic with a $10^{-20}$
absolute log-likelihood margin, conditional on computed comparisons rather
than outward rounding. The witness need not itself be a maximizer. The
[certification report](../../../../../dev/research/evaluations/2026-11-06-bnbinom-cert-report.md)
records validation and the multivariate certification gaps.

## R notes

Supply `size`, `alpha` or `beta` to hold that coefficient. A detected negative binomial limit recommends `mlnbinom`, holding its size at the supplied `size` or `beta` when either was held. A Poisson limit or all-zero free fit recommends `mlpois`. The multivariate search and limit comparisons do not certify a global maximum. One-free-shape fits use the scalar certificate below; its budget and arithmetic stops carry finite diagnostic witnesses when available.
