---
id: zmnbinom
inference_numeric:
  - regime: "known zero negative-binomial size"
    available: true
    condition: "size = 0 held; 0 < prob < 1 and 0 < p0 < 1; prob and/or p0 free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite size > 0, 0 < prob < 1 and 0 < p0 < 1; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-modified negative binomial
support:
  type: integer
  lower: 0
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
- name: p0
  symbol: 'p_0'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzmnbinom
  sampler: rzmnbinom
  control:
    size: Optional fixed size; NULL estimates size.
    reltol: Tolerance for the positive-count coefficient proposer.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
    prob: "Optional held prob; NULL estimates it."
    p0: "Optional held p0; NULL estimates it."
log_density: Piecewise((log(p0), Eq(x, 0)), (log(1-p0)+(loggamma(size+x)-loggamma(size)-loggamma(x+1)+size*log(prob)+x*log(1-prob)-log(1-prob**size)), True))
limits:
- model: zmpois
  firmness: proved
  when: 'The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ tending to a finite positive rate and $p_0$ unchanged.'
  detect: 'Size and probability are free and the delegated positive-count certificate proves that its finite-size profile increases throughout the domain to the zero-truncated Poisson limit.'
  status: PoissonLimit
examples: |
  set.seed(1)
  x <- rml(200, mlzmnbinom())
  mlzmnbinom(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: nbinom
    at:
      size: size
      prob: prob
    type: zero-modified
    mass: p0
fit:
  method: delegation
  certified: 'yes'
  note: "zmnbinom/mle.hpp: Exact zero-indicator separation delegates positive counts to the global ztnbinom certificate and transports its limit and numerical/budget stops, including likelihood bounds."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model overdispersed positive counts with a separately fitted zero probability.
summary: Zero-modified negative binomial provides a model for overdispersed positive counts with a separately fitted
  zero probability.
aliases:
- "hurdle negative binomial"
references:
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, all-zero data attain a maximum at $p_0=1$ with
    unidentified base coefficients. Positive observations all equal to one attain
    a maximum at $p=1$ with size unidentified. For other samples, finite-size
    attainment depends on comparison with the infinite-size zero-modified Poisson
    boundary; general uniqueness is unknown. Holding any finite nonnegative size,
    or holding $0<p<1$, gives an attained maximum for compatible data. Holding
    only $p_0$ leaves the same positive-count existence problem. Further holds
    preserve these conclusions. Holding all coefficients leaves likelihood
    evaluation. Held zero-probability endpoints must match the observed zero
    indicators, and held $p=1$ permits positives only at one.
  reason: >-
    The zero-indicator factor is independent of the zero-truncated negative
    binomial likelihood of the positives. When positives occur, at fixed size it has a unique
    probability fit because its conditional mean is strictly monotone in the
    natural parameter; size zero is the continuous logarithmic-series member. At
    held $0<p<1$, its likelihood vanishes at infinite size and extends
    continuously to size zero, establishing attainment. With size and probability
    free, let $Q(r)$ be the maximized positive log likelihood at finite $r\ge0$
    and let $Q_\infty$ be the fitted zero-truncated Poisson value. A finite
    maximum exists if some finite $r$ has $Q(r)\ge Q_\infty$; otherwise only the
    infinite-size boundary attains the supremum. The positive-count certificate establishes the required global ordering on each successful call; it stops on unresolved signs or extra extrema. At held interior probability the positive-count size fit is unique by strict concavity, as proved in the ztnbinom dossier.
---

The zero-modified negative binomial distribution assigns probability $p_0$ directly to zero and models positive counts by a zero-truncated negative binomial law. The size $r$ need not be an integer. Together with $p$, it controls the shape of the positive-count distribution.

## Details

The likelihood factors into a zero-indicator part and a positive conditional part. Holding the zero probability leaves the positive conditional fit unchanged. A free zero probability is the empirical zero fraction, including zero or one when the corresponding count category is absent.

Size zero is the attained logarithmic-series member, with logarithmic-series parameter $1-p$, following the actuar convention. It remains available when size is held. Probability one gives a point mass at one in the positive conditional law for every finite size. Thus positive counts all equal to one give an attained two-point law, while no positive observations leave the base coefficients unidentified.

Holding finite nonnegative size removes the infinite-size escape. Holding $p<1$ also removes it. With only the zero probability held, the positive conditional profile can still approach a zero-truncated Poisson law, producing a zero-modified Poisson boundary with the same zero probability.

The positive-count fit delegates to the [ztnbinom certificate](../ztnbinom/README.md), including its attained logarithmic member, signed root enclosure, full-domain score bounds, practical floating-point margins and workload limits. Joint uniqueness remains unknown in general. Each successful free-size fit either has one globally certified maximum or the certified attained endpoint; unresolved signs or additional extrema stop. The Poisson report requires a globally increasing positive-count profile. Its limit coefficients are the zero-truncated Poisson rate and the held or fitted zero probability, and its likelihood includes the zero-indicator contribution. The [count-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) gives the global profile characterization and held-subset arguments.

## R notes

Supply any coefficient to hold it; `NULL` estimates it. Fractional nonnegative sizes, including `size = 0`, are allowed. All-zero samples warn that base parameters are unidentified. Supplied values are preserved, and free base coefficients use the representatives `size = 1`, `prob = 0.5`. With free zero probability they return `p0 = 1`. A free fit with positives all equal to one warns that `mlbinom(x, size = 1)` fits the attained two-point law exactly.

The infinite-size report stops with advice to use `mlzmpois`. A search that exhausts floating-point resolution or its certificate budget also stops, preserving the delegated diagnostic and likelihood bounds.

## Decision history

On 2026-11-06 the dedicated delegate path replaced the shared result alias so that limit coefficients, likelihood bounds and budget diagnostics survive every hold subset. The zero-indicator factor shifts both bounds by its log likelihood, with a practical $1024\epsilon(1+n+|\ell_0|)$ arithmetic allowance on each side. No density, golden fixture or zero-indicator maximizer changed. See the [certificate report](../../../../../dev/research/evaluations/2026-11-06-nbfam-cert-report.md).
