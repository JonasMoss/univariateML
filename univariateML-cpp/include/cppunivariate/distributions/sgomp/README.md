---
id: sgomp
inference_numeric:
  - regime: "exponential member with shape known"
    available: true
    condition: "finite b > 0 free; eta = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite b > 0 and eta > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Shifted Gompertz
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- name: b
  symbol: 'b'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: eta
  symbol: '\eta'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dsgomp
  sampler: rsgomp
  control:
    b: Optional fixed positive rate; `NULL` estimates it.
    eta: Optional fixed nonnegative shape; `NULL` estimates it.
    reltol: "Relative accuracy of every free coefficient (default 1e-10)."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: log(b) - b*x - eta*exp(-b*x) + log(1 + eta*(1-exp(-b*x)))
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlsgomp())
  mlsgomp(x)

group: other positive
construction:
- base
relations: {}
fit:
  method: profile-search
  certified: 'yes'
  note: 'sgomp/README.md, Details; sgomp/mle.hpp; sgomp/test.cpp: Monotone conditional score roots;
    compact profile curvature subdivision certifies the joint proposal or stops with numerical,
    comparison or budget details. Joint uniqueness is not assumed.'
areas:
- reliability and survival
classification: specialist
classification_reason: Used to model waiting times with flexible onset in diffusion and adoption models.
summary: Shifted Gompertz provides a model for waiting times with flexible onset in diffusion and adoption models.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a finite MLE exists for every nonconstant
    nonnegative sample. Constant samples have unbounded likelihood.
    Holding $b$ gives a unique finite estimate of $\eta$, possibly zero.
    Holding $\eta$ gives a unique finite estimate of $b$ exactly when the sample sum
    is positive. An all-zero sample has unbounded likelihood when $b$ is free.
    Joint uniqueness is not established. With $\eta=0$ held and positive
    sample sum, the unique rate estimate is the exponential estimate. Holding both leaves only likelihood evaluation.
  reason: >-
    For held $b$, put $v_i=e^{-bx_i}$. The shape score is
    $\sum_i[-v_i+(1-v_i)/(1+\eta(1-v_i))]$.
    It decreases strictly if any observation is positive and tends to
    $-\sum_i v_i<0$. A nonpositive score at zero selects $\eta=0$.
    For all-zero data it is the negative constant $-n$, again selecting zero.
    With $\eta$ held, the second rate derivative is
    $-n/b^2-\eta\sum_i x_i^2v_i-\eta(1+\eta)\sum_i x_i^2v_i/[1+\eta(1-v_i)]^2<0$.
    For positive sample sum the log likelihood tends to minus infinity at
    both ends of the rate domain, proving a unique rate maximum.
    With both free and nonconstant data, likelihood envelopes bound the rate
    away from zero and infinity on every positive likelihood level set.
    Bounded rate then bounds the maximizing shape. These bounds prove
    attainment but do not prove uniqueness. Profile subdivision certifies
    completeness conditional on the computed interval bounds.
---

The shifted Gompertz distribution models nonnegative waiting times, including diffusion and adoption times. The coefficient $b$ sets the time scale through a rate, and $\eta$ changes the shape; there is no location coefficient.

## Details

At a fixed rate, increasing $\eta$ shifts probability toward longer waiting times.

The domain includes $\eta=0$, an attained exponential member with rate $b$. Zeros are valid observations. A maximum can occur at this member rather than at a positive shape. Holding the rate gives a unique shape estimate from a decreasing score, or the endpoint zero. Holding the shape leaves a strictly concave scalar rate likelihood, with an explicit exponential estimate when the held shape is zero.

For a nonconstant sample, the joint likelihood attains a maximum. This existence result follows from likelihood envelopes, independently of whether the numerical search finds it. In particular, writing $m=\min_i x_i$, an upper envelope is
$\ell\le n\log b+n\log n-b(\sum_i x_i-nm)$.
The coefficient of $-b$ is positive for nonconstant data, so this excludes arbitrarily large rates. For $b\max_i x_i\le1$, maximizing $n\log(1+\eta)-n\eta/e$ bounds the remaining shape contribution by $n$, giving $\ell\le n\log b+n$. This excludes rates near zero. At bounded rates the likelihood decreases without bound as the shape grows. Constant positive samples instead allow $b$ to increase and $\eta=\exp(bx_1)$ to concentrate the density at their common value. All-zero data have unbounded likelihood whenever the rate is free.

The joint grid only proposes a candidate. A complete profile subdivision then checks
the rate interval supplied by the envelopes. The conditional shape optimum
$\hat\eta(b)$ is nondecreasing: the mixed derivative is
$C=\sum_i x_i e^{-bx_i}\{1+(1+\eta(1-e^{-bx_i}))^{-2}\}>0$.
On each rate interval $[l,h]$, the endpoint shape optima bound all intervening
optima. Writing $D=-\ell_{\eta\eta}>0$, the profile curvature is
$\ell_{bb}+C^2/D$ in the interior and $\ell_{bb}$ on the zero-shape face.
Bounds on these three derivatives give a quadratic upper envelope about the
midpoint, including the residual inner-root tangent error. Losing intervals
are excluded. Every unresolved small region is retained until the final
lower likelihood witness is known; changing the proposal cannot invalidate
previously retained regions. A successful certificate encloses every possible
global maximizer within relative rate error $10^{-7}$ and shape error
$10^{-7}\max(1,\eta)$ of the returned coefficients. Joint uniqueness is not
presumed. Separated surviving regions stop as a comparison tie, and exhaustion
of $128\min(\mathrm{iterlim},100000)$ certificate evaluations (including the
proposal mesh) stops on budget. Overflow or an invalid curvature bound stops
numerically. Endpoint shape solutions are reused when subdividing.

The rate root with eta held uses its strictly negative second derivative,
$-n/b^2-\eta\sum x_i^2e^{-bx_i}-\eta(1+\eta)\sum
x_i^2e^{-bx_i}/(1+\eta-\eta e^{-bx_i})^2$, and a signed expanding bracket.
It is refined to relative width $2\cdot10^{-13}$. In the joint certificate,
conditional shape solves use $\min(\mathrm{reltol},10^{-13})$. The log-shape
brackets are inflated by twice this tolerance plus $10^{-12}$. Each retained
region has relative rate width at most $10^{-9}$, shape width at most
$10^{-8}\max(1,\eta)$, and its likelihood upper bound exceeds its center
value by at most twice the practical margin
$512\epsilon(n+|\ell|)$. The final global upper bound must be within eight
such margins of the returned value. A historical proposal is retained only
when all surviving regions satisfy the coefficient enclosure around it.
The certificate is in exact arithmetic conditional on those computed bounds
and comparisons; it does not use outward-rounded interval arithmetic.

The [positive-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-positive.md) records the held-parameter and boundary arguments.

## R notes

The fitter compares candidates by directly subtracting the rate and shape contributions before adding their common log likelihood. To preserve ordinary-fit rounding on numerical plateaus, candidate differences within $8\epsilon n$ retain the historical ordering only when the full-likelihood floating-point spacing is also at most $8\epsilon n$, where $\epsilon$ is double precision machine epsilon. This tie margin stays absolute as the common likelihood grows. It prefers the attained `eta = 0` member only when its right shape score is nonpositive and its directly computed likelihood difference is at least $-10^{-8}n$. This absolute per-observation margin does not grow with the magnitude of the common log likelihood. Strict concavity proves the held-rate shape decision in exact arithmetic, conditional on the computed score comparison; floating-point comparisons use double precision. The compact profile subdivision checks joint search completeness using the bounds above. Constant samples with both coefficients free, and all-zero samples with `b` free, stop with an unbounded-likelihood error.

Strict concavity in eta alone does not control joint coefficient accuracy:
a rate error changes the conditional eta optimum through the mixed derivative
of $\log(1+\eta(1-e^{-bx}))-\eta e^{-bx}$. A small conditional eta score
therefore cannot by itself justify the reported rate and eta errors.

With $\eta=0$ held, the rate has the closed form $n/\sum x_i$.
The sum is compensated after scaling by the largest observation; repeated
observations therefore do not accumulate a first-order summation bias.
