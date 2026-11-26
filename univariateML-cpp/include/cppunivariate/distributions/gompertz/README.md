---
id: gompertz
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite a > 0 and b > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Gompertz
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: a
  symbol: 'a'
  role: rate
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: b
  symbol: 'b'
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dgompertz
  sampler: rgompertz
  control:
    a: Optional held a; NULL estimates it.
    b: Optional held b; NULL estimates it.
    tol: "Requested relative accuracy of `b` (default 1e-10): near the exponential face, require score signs within `tol * b`; otherwise the free fit uses its relative Newton-step criterion."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: log(a) + b*x - (a/b)*(exp(b*x)-1)
limits:
- name: unbounded likelihood
  firmness: proved
  fixed: [b]
  when: 'The free initial hazard $a$ tends to infinity with $b$ held fixed.'
  detect: 'All observations are zero.'
  status: Unbounded
  message: unbounded
- name: exponential face
  firmness: proved
  model: exp
  fixed: [a]
  when: 'The free growth coefficient $b$ decreases to zero with $a$ held fixed.'
  detect: 'The sample satisfies $0<\sum_i x_i\le a\sum_i x_i^2/2$.'
  status: Overdispersed
  numerical: true
  message: mlexp
- model: 'exp'
  firmness: proved
  when: 'The growth coefficient $b$ decreases to zero and $a$ tends to $1/\bar x$.'
  detect: 'Both coefficients are free, the sample is nonconstant, and $\overline{x^2}\ge 2\bar x^2$, decided in double-double arithmetic with a rounding pad; a sample inside the pad, including every exact tie, returns the limit uncertified.'
  status: 'Overdispersed'
references:
- Lenart, A. (2012). The Gompertz distribution and Maximum Likelihood Estimation of its parameters - a revision.
  MPIDR Working Paper WP 2012-008.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
examples: |
  set.seed(1)
  x <- rml(200, mlgompertz())
  mlgompertz(x)

group: other positive
construction:
- base
relations: {}
fit:
  method: profile-search
  certified: 'yes'
  note: 'gompertz/README.md, Details; gompertz/mle.hpp (public fitter and held overloads); gompertz/test.cpp: Held
    b gives analytic a; held a has a strictly concave log likelihood in b; profiling a leaves a strictly decreasing shape
    score. Exponential boundary comparisons precede return.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model adult mortality and lifetimes with an exponentially increasing hazard.
summary: Gompertz provides a model for adult mortality and lifetimes with an exponentially increasing hazard.
aliases:
- "truncated Gumbel"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists for a nonconstant
    nonnegative sample exactly when $\overline{x^2}<2\bar x^2$.
    Otherwise a nonconstant sample selects the excluded exponential face.
    Constant samples have unbounded likelihood. With $a$ held and a positive
    sample sum, a unique finite estimate of $b$ exists exactly when
    $\sum_i x_i>a\sum_i x_i^2/2$. Otherwise the supremum is at $b=0$.
    With $a$ held and all observations zero, every positive $b$ is a maximizer.
    With $b$ held, a unique finite estimate of $a$ exists exactly when the
    sample sum is positive. Holding both leaves only likelihood evaluation.
  reason: >-
    Write $I(b)=\sum_i\int_0^{x_i}e^{bt}\,dt$. Profiling $a$ gives
    $\hat a=n/I(b)$ and, up to constants, $b\sum_i x_i-n\log I(b)$.
    The logarithm of this moment-generating integral is strictly convex for
    positive sample sum. The profile derivative at zero is
    $n(\bar x-\overline{x^2}/(2\bar x))$, and its limit at infinity is
    $n(\bar x-\max_i x_i)<0$ for nonconstant data.
    With $a$ held, the second derivative in $b$ is
    $-a\sum_i\int_0^{x_i}t^2e^{bt}\,dt<0$ and the score at zero is
    $\sum_i x_i-a\sum_i x_i^2/2$. All-zero data instead give likelihood
    $a^n$, independent of $b$.
  strict_exceptions:
  - class: near-constant
    outcome: failure(evaluation)
    argument: >-
      The strictly concave rate profile has positive score at
      $b_0=2048/\min x$ on this sample. The profiled initial hazard decreases
      with rate and is already below binary64 at $b_0$.
---

The Gompertz distribution models lifetimes with an exponentially increasing hazard. The coefficient $a$ is the initial hazard, and $b$ controls how quickly it increases with time.

## Details

The hazard is $a\exp(bx)$, with both coefficients positive. The exponential law is an excluded limit as $b$ decreases to zero. For a positive sample sum, profiling the initial hazard leaves a strictly concave scalar log likelihood in $b$. Its derivative at zero determines whether a finite maximum exists or the exponential face has the supremum.

The free-fit decision is the sign of $T=nQ-2S^2$ with $S=\sum x_i$, $Q=\sum x_i^2$. The data are scaled by an exact power of two, $S$ and $Q$ are accumulated as unevaluated pairs with TwoSum and FMA products, and $T$ is formed without rounding the cancelling leading terms. With $u=2^{-53}$, $\gamma_k=ku/(1-ku)$ and $nQ$ computed upward as $N$, the pad is $(6\gamma_{2n}^2N+16uW+16n^22^{-1074})(1+10^{-6})$, where $W$ is the sum of the magnitudes of the correction terms. The two Sum2 bounds are the cited ones (Ogita, Rump and Oishi, 2005); the factor 6, the evaluation bound $16uW$ and the underflow term are derived in `mle.hpp` and checked on constructed samples, not proved independently. Relative to $nQ$ the pad is about $5\times10^{-30}$ for $n=3$ and $3\times10^{-25}$ for $n=1000$, against binary64 comparisons that fail within about $10u\approx10^{-15}$. $T>$ pad is a certified limit, $T<-$ pad a finite fit, and anything between is the limit with `numerical: true` semantics (uncertified). With $a$ held, the corresponding score is $S-\alpha Q/2$ in the scaled data, where $\alpha$ is the initial hazard in those units. It uses the same moment pairs and a product residual for $\alpha Q/2$. The pad accounts for both moment errors and the roundoff in the low-word calculation. A negative score outside the pad certifies the exponential face. The pad itself selects an uncertified limit.

Holding $a$ also leaves a strictly concave log likelihood in $b$ whenever the sample sum is positive. For all-zero data with $a$ held the likelihood is flat in $b$. Holding $b$ gives an explicit estimate of $a$ when the sample sum is positive, and unbounded likelihood otherwise. Holding both evaluates the likelihood. With both free, a constant positive sample has unbounded likelihood as $b$ increases and $a$ decreases to concentrate the density at its common value.

Near the exponential face, write $z_i=x_i/2^e$, $\beta=b2^e$, and $H(\beta)=\sum_i z_i\int_0^1\exp(\beta z_i t)\,dt$. The free profile score has the sign of $SH/n-H'$, while the held-hazard score is $S-\alpha H'$. The boundary comparison supplies the constant term and a Taylor polynomial supplies the remaining terms. The fitter requires opposite padded signs within half the requested relative tolerance of the represented rate. Strict concavity makes this a global bracket. The free hazard follows as $n/(2^e H)$, and $0<H'/H\le1$ bounds the propagation of the rate error into it.

Away from the face, the free fit runs on data divided by their mean, and $a$ and $b$ scale with its reciprocal. On subnormal data they can exceed binary64 ($(1,2,3)$ times the smallest subnormal gives $a=1.5\times10^{322}$, $b=2.5\times10^{323}$); a coefficient that overflows stops with `NumericalFailure`, and one below binary64 resolution with `NumericalFailure` and a tolerance detail.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) gives the boundary arguments and their held-parameter conditions.

## R notes

When the exponential face wins, fitting stops with advice to use `mlexp`. If `a` is held and all observations are zero, the fitter returns `b = 1` as one of the equally good values.

## Decision history

2026-10-30 (adapter independence): The fully free constant-sample rejection moved into the core fitter with status `Unbounded`, preserving the former adapter decision. Conditional all-zero handling also remains in the core.

With $a$ held and positive sample sum, the strictly decreasing $b$ score is evaluated relative to $\sum_i x_i$ using logarithms of the integral derivative. A log-$b$ bracket expands within the representable positive range until both signs are verified; it has no cap on $b\max_i x_i$. Bisection must pass an absolute total log-$b$ score gate of $10^{-3}$ at the represented result. Logarithmic integral accumulation also permits finite likelihoods when $\exp(bx)$ overflows. A missing bracket or failed gate returns `IterationFailure`. These are exact-arithmetic certificates conditional on computed comparisons, with a practical floating-point score margin; they do not use outward-rounded intervals.
