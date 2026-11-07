---
id: zip
inference_numeric:
  - regime: "Poisson member with inflation known"
    available: true
    condition: "finite lambda > 0 free; pi = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: false
    condition: "finite lambda > 0 free; pi = 1 held; information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "primary interior"
    available: true
    condition: "finite lambda > 0 and 0 < pi < 1; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-inflated Poisson
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzip
  sampler: rzip
  control:
    lambda: "Optional held lambda; NULL estimates it."
    pi: "Optional held pi; NULL estimates it."
    iterlim: "Iteration budget per certified rate root; raise to resolve budget stops."
log_density: "Piecewise((log(pi + (1 - pi)*exp(-lambda)), Eq(x, 0)), (log(1 - pi) - lambda + x*log(lambda) - loggamma(x + 1), True))"
limits:
  - model: pois
    firmness: proved
    when: 'The rate $\lambda$ tends to zero with inflation $\pi<1$ held.'
    detect: 'All observations are zero, the rate is free and inflation is held below one.'
    fixed: [pi]
    status: ZeroLimit
  - name: undefined zero-rate reference
    firmness: proved
    when: 'The rate $\lambda$ is held at zero, outside the declared domain.'
    fixed: [lambda]
    status: InvalidInput
    message: lambda positive
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  set.seed(1)
  x <- rml(200, mlzip())
  mlzip(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: pois
    at:
      lambda: lambda
    type: zero-inflated
    mass: pi
fit:
  method: closed-form
  certified: 'yes'
  note: 'Free inflation is an exact clipped profile; the free fit reduces to the unique zero-truncated
    rate equation. Held inflation uses finite root isolation and bounded comparison of every rate maximum;
    unresolved arithmetic, competing peaks and exhausted work stop with structured details. See Details.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model Poisson event counts with an additional structural-zero component.
summary: Zero-inflated Poisson provides a model for Poisson event counts with an additional structural-zero component.
aliases:
- "ZIP"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, every nonempty count sample has a finite maximum.
    If a count is positive, the maximizing pair is unique. All-zero data are
    maximized by $\pi=1$ with any $\lambda>0$. With $\lambda>0$ held, free
    inflation has a unique maximum. With $\pi<1$ held, a finite rate maximum
    exists whenever a count is positive, but two global maxima can tie. For
    all-zero data and held $\pi<1$, the supremum requires $\lambda\downarrow0$ and
    is not attained in the declared domain. Held $\pi=1$ permits only zeros and
    leaves a free rate unidentified. Holding both coefficients leaves likelihood
    evaluation, subject to compatibility.
  reason: >-
    Write $z$ for the empirical zero fraction. At a held rate, the inflation
    maximum is $\max\{0,(z-e^{-\lambda})/(1-e^{-\lambda})\}$, with value one for
    all-zero data. In the positive-inflation branch, the rate solves
    $\lambda/(1-e^{-\lambda})=\bar x/(1-z)$, whose left side is strictly
    increasing. If this branch is infeasible, the unique maximum is the Poisson
    member $\pi=0,\lambda=\bar x$. For held $\pi<1$ and a positive count, the
    likelihood tends to zero at both ends of the rate domain, so a finite maximum
    exists. Its score has at most three roots; two separated maxima can tie.
---

The zero-inflated Poisson distribution mixes a Poisson count of rate $\lambda$ with a structural-zero component of probability $\pi$. The total zero probability also includes zeros from the Poisson component.

## Details

When the empirical zero fraction is no greater than $e^{-\bar x}$, the free fit is the attained Poisson member $\pi=0$. Otherwise inflation is positive and the rate is determined by the mean of the positive observations. All-zero data identify only the point mass at zero, leaving the rate arbitrary.

Holding the rate estimates only inflation by a clipped zero-fraction adjustment. Holding inflation below one leaves a finite rate maximum for any sample containing a positive count. This conditional likelihood can have separated peaks, which the rate certificate isolates and compares. With all observations zero, the rate instead tends to zero, outside the declared domain $\lambda>0$. A held zero rate is rejected. A held unit inflation is compatible only with zeros.

The [count-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) records the boundary arguments and held subsets.

## R notes

Supply either coefficient to hold it, or use `NULL` to estimate it. The all-zero fit warns and returns `lambda = 1, pi = 1` when both are free. With unit inflation held, the free rate also uses one as a representative. All-zero data with `pi < 1` held stop with advice to use `mlpois`.

## Held-inflation certificate

For $0<\pi<1$, let $n$ be sample size, $k>0$ the positive count,
$S>0$ the count sum, $u=S/n$ and $v=S/k$. When zeros occur, every
stationary rate is in $(u,v)$: the score

$$s(t)=S/t-k-(n-k)/(1+\pi e^t/(1-\pi))$$

is positive below $u$ and negative above $v$. Both likelihood tails tend
to minus infinity. Rewriting $s(t)=0$ gives

$$h(t)=-t+\log(nt-S)-\log(S-kt)-\log(\pi/(1-\pi))=0.$$

On $(u,v)$, the score has the opposite sign to $h$, and
$h'(t)=-1+1/(t-u)+1/(v-t)$. Put $d=v-u$.
For $d<4$ this derivative is positive. For $d>4$ its only zeros are
$(u+v\pm\sqrt{d(d-4)})/2$. Thus at most three stationary rates exist,
and only increasing branches of $h$ can contain a maximum. Each such
branch has at most one root, isolated by bisection. This excludes both
tails and all unsearched interior points; no grid or initial rate is needed.
The shared helper in `certificate.hpp` also supplies ZINB's held-inflation
ZIP face, retaining every peak enclosure for its later face comparisons.

A root bracket $[a,b]$ gives the likelihood upper bound
$\ell((a+b)/2)+(b-a)(n+S/a)+E$, since $|s(t)|\le n+S/a$ there.
The lower bound is the evaluated midpoint likelihood minus $E$.
ZIP uses $E=4096\epsilon(1+n+|\ell|+kt+S|\log t|)$ and compensated
summation of stable log probabilities. Rate brackets must have relative
width at most $10^{-13}$. A winner is returned only when its lower bound
exceeds every other peak's upper bound. Overlapping bounds stop as `tie`
and retain both candidates; exhaustion of the per-root `iterlim` bisections
stops as `budget` with candidates and a global upper bound. Raising `iterlim`
can resolve a budget stop. The default is 200 and the cap follows
`min(iterlim, 100000)`.

Computed $d$ within $4096\epsilon(1+n+d)$ of four, or a turning-point
$h$ value within $4096\epsilon(1+n+v+|\log\pi|)$ of zero, stops as
`numerical`. This includes potentially tangent stationary roots, rather than
silently discarding them. Such a stop retains a finite likelihood witness
and an unclosed infinite upper bound. The certificate is global in exact
arithmetic conditional on the computed comparisons, with these practical
floating-point margins; it does not use outward-rounded interval arithmetic.
ZINB retains its own arithmetic margin when calling the shared helper.

Uniqueness fails even with positive counts and held interior inflation.
For 99 zeros and one count of ten there is a value
$\pi_*\simeq0.7254971052291191$ at which the global rates are approximately
$0.5223041246623353$ and $9.982727928366345$. To see existence of an exact
tie, on $\pi\in[0.7,0.75]$ the two turning-point signs remain strictly
positive and negative, so both strict maxima vary continuously. The lower
rate wins at 0.7 and the higher rate at 0.75. The intermediate value theorem
therefore gives an exact tied value between them. The rounded value above
is a numerical locator, not an assertion of exact equality at a binary64 input.
Independent high-precision values, the sign checks and reproduction commands
are retained in the [certificate report](../../../../../dev/research/evaluations/2026-11-07-zip-cert-report.md).
