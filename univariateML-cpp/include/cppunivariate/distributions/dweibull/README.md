---
id: dweibull
inference_numeric:
  - regime: primary interior
    available: true
    condition: "0 < shape1 < 1 and finite shape2 > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Discrete Weibull
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: shape1
  symbol: 'q'
  role: probability
  domain: (0, 1)
  simulate: 0.5
  kind: regular
- name: shape2
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: ddweibull
  sampler: rdweibull
  control:
    shape1: "Optional value in (0, 1) at which to hold `shape1`; `NULL` (the default) estimates it."
    shape2: "Optional positive value at which to hold `shape2`; `NULL` (the default) estimates it. `shape2 = 1` is the geometric distribution."
    reltol: "Absolute tolerance on `log(shape2)` in the root search for `shape2`; unused when `shape2` is held."
    iterlim: "Maximum number of iterations of each scalar root search."
log_density: "Piecewise((log(1 - shape1), Eq(x, 0)), (log(shape1**(x**shape2) - shape1**((x + 1)**shape2)), True))"
limits:
  - name: degenerate
    firmness: proved
    when: 'The shape $\beta$ tends to infinity with $(-\log q)^{-1/\beta}$ fixed between the common positive count and the next integer. For a common count of zero, $q$ tends to zero.'
    detect: 'Both coefficients are free and all observations are equal.'
    status: PointMass
  - name: degenerate
    firmness: proved
    when: 'The free probability $q$ tends to zero with $\beta$ held fixed.'
    detect: 'All observations are zero.'
    status: PointMass
    fixed: [shape2]
  - model: binom
    firmness: proved
    when: 'The shape $\beta$ tends to infinity and $q$ tends to the sample fraction of ones, giving a binomial law with size one.'
    detect: 'Both coefficients are free and the sample contains both zero and one, with no other counts.'
    status: BernoulliLimit
  - name: adjacent two-point
    firmness: proved
    when: 'The shape $\beta$ tends to infinity while $q^{(k+1)^\beta}$ tends to the sample fraction of observations equal to $k+1$.'
    detect: 'Both coefficients are free and the sample takes exactly two adjacent values $k$ and $k+1$, with $k\ge1$.'
    status: TwoPointLimit
  - name: Bernoulli(shape1)
    firmness: proved
    when: 'The free shape $\beta$ tends to infinity with $q$ held fixed, giving a Bernoulli law with success probability $q$.'
    detect: 'All observations are zero or one, and at least one is one.'
    status: HeldBernoulliLimit
    fixed: [shape1]
references:
- Nakagawa, T. and Osaki, S. (1975). The discrete Weibull distribution. IEEE Transactions on Reliability, R-24(5),
  300–301. doi:10.1109/TR.1975.5214915.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mldweibull())
  mldweibull(x)

group: gamma and Weibull
construction:
- discretisation
relations:
  discretisation_of:
  - family: weibull
    at:
      shape: shape2
      scale: (-log(shape1))**(-1/shape2)
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'dweibull/README.md, Details; dweibull/mle.hpp (public fitter and held overloads); dweibull/test.cpp: The
    likelihood is strictly concave in log(-log(shape1)) and shape2. Both holds are affine restrictions; safeguarded
    inner Newton and bracketed decreasing profile scores find the unique maximum.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model discrete lifetimes with duration-dependent failure probabilities.
summary: Discrete Weibull provides a model for discrete lifetimes with duration-dependent failure probabilities.
aliases:
- "discrete Weibull type I"
key_references:
- 2
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a unique finite MLE exists exactly when
    $\max_i x_i-\min_i x_i\ge2$. Constant samples and samples on two
    adjacent counts have only an excluded limiting maximum.
    Holding $\beta$ gives a unique finite estimate of $q$ exactly when at
    least one observation is positive. Holding $q$ and observing at least
    one positive count gives a unique finite estimate of $\beta$ exactly
    when some observation is at least two. If $q$ is held and all observations
    are zero, every positive $\beta$ maximizes the likelihood.
    Holding both leaves only likelihood evaluation.
  reason: >-
    The log likelihood is strictly concave in $(\alpha,\beta)$, where
    $\alpha=\log(-\log q)$, whenever a count is positive. The Hessian
    argument in Details proves this directly. Either hold is an affine
    restriction in these coordinates. As $\beta$ decreases to zero,
    positive-count masses vanish uniformly over $q$. At the other parameter
    boundaries the only proper limits are point masses or adjacent two-point
    laws. A sample range of at least two therefore forces attainment on a
    compact likelihood level set. The declared limits cover the remaining
    cases. With $q$ held, all-zero data have likelihood $(1-q)^n$,
    independent of $\beta$.
---

The type I discrete Weibull distribution of Nakagawa and Osaki (1975) models nonnegative counts such as the number of cycles survived before failure. A count is the integer part of a continuous Weibull variable with shape $\beta$.

## Details

The coefficient $q$ is the probability that the count is positive.

The [existence analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) records the limit and sample conditions. The concavity and boundary arguments below establish existence and uniqueness separately from numerical checks.

The Weibull scale is $(-\log q)^{-1/\beta}$. The member $\beta=1$ is an attained geometric law with success probability $1-q$. Shapes below one give a decreasing probability of failure at the next count, conditional on survival. Shapes above one give an increasing probability. The log likelihood is strictly concave in $(\log(-\log q),\beta)$ once an observation is positive. The fitter solves the unique inner probability equation and brackets the decreasing profile shape score. Either held coefficient leaves a concave scalar problem. Unresolved roots stop fitting.

With $q$ held and all observations zero, the likelihood does not depend on $\beta$. There are then infinitely many maximizers.

### Why a single profile bracket is global

Put $t=-\log q$. With $a_k=k^\beta$ and $d_k=(k+1)^\beta-k^\beta$, the inner score is
$\ell_t=\sum_k n_k[-a_k+d_k/\operatorname{expm1}(td_k)]$ and

$$\ell_{tt}=-\sum_k n_k\frac{d_k^2e^{-td_k}}{(1-e^{-td_k})^2}<0.$$

For a sample containing a positive count, this score goes from positive
infinity to a negative limit, giving exactly one root.

Put $\alpha=\log t$. For a positive count $k$, write
$a=\exp(\alpha+\beta\log k)$, $b=\exp(\alpha+\beta\log(k+1))$,
$E=\exp(a-b)$ and $D=1-E$. In the endpoint coordinates
$v=\log a$, $u=\log b$, the Hessian of
$h(u,v)=\log(e^{-a}-e^{-b})$ satisfies

$$h_{uu}=\frac{bE(D-b)}{D^2}<0,\qquad
\det\nabla^2h=\frac{abE}{D^3}\{b-1+(1-a)E\}>0.$$

Indeed $D<b-a<b$. The expression in braces is zero at $b=a$
and has derivative $1-(1-a)e^{a-b}>0$ for $b>a>0$.
The affine map from $(\alpha,\beta)$ to $(u,v)$ is invertible,
so each positive-count log probability is strictly concave in
$(\alpha,\beta)$. The zero-count term $\log(1-e^{-e^\alpha})$
is concave and independent of $\beta$. Thus a sample containing a
positive count has a strictly concave log likelihood. The inner
$\alpha$ score decreases strictly from $n$ to a negative limit;
its zero is also the unique inner $t$ score zero. The profile curvature
is the strictly negative Schur complement of the joint Hessian. Its beta score
therefore decreases strictly, allowing a single sign bracket in log beta
rather than a scan that might miss local maxima. The same argument
applies with either parameter held.

For completeness, as $\beta\downarrow0$, every positive-count mass
vanishes uniformly over $t$, since
$p_k\le ((1+1/k)^\beta-1)/e$. At bounded positive beta, $t\downarrow0$
loses mass to infinity and $t\uparrow\infty$ gives a point mass at zero.
As $\beta\uparrow\infty$, the threshold $t^{-1/\beta}$ permits only
point masses or adjacent two-point proper limits. A range of at least
two therefore forces a positive likelihood level set to be compact.
For range one, its adjacent two-point limit attains the empirical
saturated likelihood, strictly exceeding every finite fit; constants
similarly escape to their point mass. These establish the stated
existence criterion, separately from numerical checks.

## R notes

With `shape1` held and all observations zero, the fitter warns that the estimate is not unique and returns `shape2 = 1`.

Doubles close to 1 resolve $-\log \mathrm{shape1}$ only coarsely. When the fitted `shape1` is within `1e-6` of 1, `shape2` is refitted with that representable `shape1` held; when it rounds to 1 (the Weibull scale to the power `shape2` exceeds about $2^{53}$, e.g. counts near 40 with `shape2 = 10`) the fit stops with an error.

## Stop details

Adjacent faces preserve their original integer support. On the held-shape1 Bernoulli face the success probability is the held value, rather than the empirical proportion.
