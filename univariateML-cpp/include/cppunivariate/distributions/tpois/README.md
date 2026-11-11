---
id: tpois
inference_numeric:
  - regime: "singleton retained support"
    available: false
    condition: "known truncation bounds with exactly one retained integer; finite lambda > 0 free; the conditional law is a point mass, so information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "finite lambda > 0 free; known a and b held; retained support has at least two integers"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Truncated Poisson
support: {type: integer, lower: "max(0, floor(a)+1)", upper: "floor(b)", closed: [true, true]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: a
  symbol: 'a'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 0
  kind: design
- name: b
  symbol: 'b'
  role: known design quantity
  domain: (-inf, inf)
  simulate: 10
  kind: design
r:
  density: dtpois
  sampler: rtpois
  control:
    lambda: Optional fixed parameter; NULL estimates it.
    a: "Strict lower bound; NULL estimates it as min(x) - 1."
    b: "Inclusive upper bound; NULL estimates it as max(x)."

log_density: "x*log(lambda)-loggamma(x+1)-log(Sum(exp(k*log(lambda)-loggamma(k+1)), (k, Max(0, floor(a)+1), floor(b))))"
limits:
- name: point mass at lowest support point
  firmness: proved
  when: 'The rate satisfies $\lambda\to0$ on a fixed nonsingleton window whose lowest retained count is positive.'
  detect: 'The rate is free and every observation equals the strictly positive lowest count of a nonsingleton retained window.'
  status: LowerPointMass
  fixed: [b]
- name: point mass at highest support point
  firmness: proved
  when: 'The rate satisfies $\lambda\to\infty$ on a fixed nonsingleton window with a finite upper edge.'
  detect: 'The rate is free and every observation equals the finite highest count of a nonsingleton retained window.'
  status: UpperPointMass
  fixed: [a]
holds: []
fit_args:
  a: 0
  b: 10
examples: |
  set.seed(1)
  x <- rml(200, mltpois())
  mltpois(x, a = 0L, b = 10L)

group: modified counts
construction:
- truncation
relations:
  truncation_of:
  - family: pois
    at:
      lambda: lambda
fit:
  method: scalar-solve
  certified: 'yes'
  note: "tpois/mle.hpp and profile.hpp: Bounds profile to the minimal observed integer window; a nonsingleton window has a strictly increasing conditional mean and a unique bracketed rate root or the documented excluded endpoint. Zero and singleton-support members are attained; singleton rate is unidentified."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model Poisson counts recorded only within a specified conditioning window.
summary: Truncated Poisson provides a model for Poisson counts recorded only within a specified conditioning window.
aliases: []
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With all coefficients free, every nonempty nonnegative integer sample
    has a finite maximum. A nonconstant sample identifies the rate on the
    smallest retained integer window, but the real truncation bounds are
    not unique. A constant sample permits singleton support and does not
    identify the rate. With data-compatible bounds held, tighten each free bound to
    the corresponding sample extreme. On a nonsingleton window a unique
    positive finite rate exists exactly when the sample mean lies strictly
    between its edges. All-zero data attain rate zero if zero is retained.
    Other edge-only samples have an unattained point-mass supremum.
    With the rate held, tightening free bounds maximizes the likelihood
    whenever the conditioning event and observations have positive probability.
    Holding all coefficients leaves only likelihood evaluation.
  reason: >-
    Removing unobserved retained counts decreases the conditioning
    probability and increases the likelihood. Bounds giving the same
    integer window are equivalent. On a fixed nonsingleton window,
    the second derivative in $\log\lambda$ is minus the sample size
    times the conditional variance, which is positive. The conditional
    mean increases from the lower edge to the upper edge (or infinity),
    giving the stated root criterion. Singleton support has likelihood one
    for every positive rate. Rate zero is an attained member only when
    the window contains zero. The uniqueness claim concerns the rate on
    nonsingleton support, not the complete coefficient vector.
---

The truncated Poisson distribution models Poisson counts observed only within a conditioning window $a<X\le b$. The rate $\lambda$ describes the underlying Poisson law. The bounds describe which counts can be recorded, rather than censoring observations outside the window.

## d/p/q/r

### Relative conditional masses

A rare conditioning event can give a moderate conditional log density while both parent kernel terms are large. Positive Poisson ratios lambda/(k+1), anchored at the retained mode, remove that common kernel before normalizing; monotone ratios give a geometric bound on the omitted tails. Conditional evaluation returns NaN when its unit lattice cannot advance, its two-million-cell side guard cannot close the remainder, or a separated parent-log comparison exceeds its stated practical margin.

## Details

Bounds should reflect the sampling design. With both bounds estimated, the retained integer window tightens to the sample extremes. Removing unobserved counts from the window cannot decrease the conditional likelihood. Real bounds giving the same integer window are equivalent. The strict lower bound has representatives in $[\min_i x_i-1,\min_i x_i)$, and an estimated finite upper bound has representatives in $[\max_i x_i,\max_i x_i+1)$.

On a fixed nonsingleton window the conditional mean increases strictly with $\log\lambda$, because its derivative is the conditional variance. A sample mean strictly between the edges therefore gives a unique finite positive rate. An edge-only sample instead approaches a point mass. The atom at zero is attained at $\lambda=0$ when zero is retained. At a strictly positive lower edge, or at the finite upper edge of nonsingleton support, the point mass is only a limit. Singleton support has likelihood one at every positive rate and does not identify the rate.

Holding the rate leaves only selection of any free bounds. Holding every coefficient evaluates the conditional likelihood. Untruncated bounds recover the ordinary Poisson fit. The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) gives the boundary arguments.

## R notes

Supply `a` and `b` explicitly. Use `a = NULL` or `b = NULL` to estimate the corresponding bound. The returned representatives are `min(x) - 1` and `max(x)`. On singleton support a free rate is represented by one. A point-mass escape stops fitting.

For `a = 0, b = Inf`, interior fits agree with `mlztpois`. The zero-rate conditioning limit is an attained member only in the separately defined zero-truncated family. Thus `mlztpois` retains its attained boundary while `mltpois` reports an escape.

Scalar d/p/q/r share the positive-probability conditioning contract. At `lambda = 0`, a nonempty window containing zero returns its atom at zero. A window excluding zero returns NaN, rather than its weak-limit atom at the first allowed count.

## Decision history

The normalizer uses mode-centred recurrences with a two-million-term budget per tail. Unresolved normalization stops with numerical failure.
