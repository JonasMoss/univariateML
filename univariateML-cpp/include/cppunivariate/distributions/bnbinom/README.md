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
    gradtol: "Tolerance on the average log likelihood score in log parameters."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per negative-binomial face certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
  certified: 'no'
  note: 'bnbinom/README.md, Details; bnbinom/mle.hpp (public fitter and held overloads); bnbinom/test.cpp: Default
    and nontrivial conditional fits use multistart, finite grids or local profile refinement without a global certificate;
    all-held density evaluation is not counted as certification.'
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
    but the nonzero-sample conditional fits are numerical searches without
    a general uniqueness guarantee. Holding all coefficients leaves only
    likelihood evaluation, including for all-zero data.
  reason: >-
    The probability mass is symmetric in $r$ and $\beta$ after expanding
    the beta and gamma factors. Its mass at zero is strictly below one
    at every finite positive parameter vector. It tends to one as a free
    $r$ or $\beta$ tends to zero, or as a free $\alpha$ tends to infinity,
    with the other coefficients fixed and positive. Comparing numerical
    stationary points with reachable limiting models does not establish
    that all finite maxima have been found.
---

The beta negative binomial mixes a negative binomial success probability with a beta distribution. Also called the generalized Waring distribution, it models counts with power-law tails. The shape $r$ is the underlying negative binomial size. The shapes $\alpha$ and $\beta$ govern the beta mixing distribution for its success probability.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) describes the limiting paths and the scope of the boundary comparisons.

Any subset of $r$, $\alpha$ and $\beta$ may be held fixed. When $r$ and $\beta$ are both free, swapping them gives exactly the same distribution. The returned representative satisfies $r\le\beta$. No ordering is imposed when either is held.

On the diagonal $r=\beta$ the swap fixes the law, so the score along $(1,0,-1)$ in $(r,\alpha,\beta)$ vanishes and the Fisher information has rank 2 of 3. The diagonal is therefore a singular boundary of the identified set $r\le\beta$: Wald intervals for $r$ and $\beta$ separately are unreliable near it. Elsewhere in the interior the information is nonsingular ([classification](../../../../../dev/research/evaluations/2026-11-04-dqm-c-report.md)).

A deterministic multistart search compares finite stationary points with every reachable negative binomial and Poisson face. Limiting models win computed likelihood ties within $10^{-8}$ times the sample size. This search does not certify global optimality or assume unimodality. An unresolved score or range endpoint stops the fit. With all coefficients supplied, the likelihood is evaluated directly. For all-zero data, any free coefficient allows the likelihood to approach one only at an excluded boundary. With every coefficient held, the all-zero likelihood is evaluated at the supplied positive values.

## R notes

Supply `size`, `alpha` or `beta` to hold that coefficient. A detected negative binomial limit recommends `mlnbinom`, holding its size at the supplied `size` or `beta` when either was held. A Poisson limit or all-zero free fit recommends `mlpois`. The finite search and limit comparisons do not certify a global maximum.

An unresolved negative-binomial face is not replaced by the Poisson face:
its certificate failure and any retained face witnesses propagate to the
parent fit. Both face solvers use the caller's iteration allowance.
