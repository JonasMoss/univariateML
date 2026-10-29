---
id: dweibull
name: Discrete Weibull
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
  - {name: shape1, role: probability, domain: "(0, 1)", default: 0.8, fixable: true}
  - {name: shape2, role: shape, domain: "(0, inf)", default: 1.5, fixable: true}
r:
  density: extraDistr::ddweibull
  sampler: extraDistr::rdweibull
  control:
    shape1: "Optional value in (0, 1) at which to hold `shape1`; `NULL` (the default) estimates it."
    shape2: "Optional positive value at which to hold `shape2`; `NULL` (the default) estimates it. `shape2 = 1` is the geometric distribution."
    reltol: "Absolute tolerance on `log(shape2)` in the root search for `shape2`; unused when `shape2` is held."
    iterlim: "Maximum number of iterations of each scalar root search."
log_density: "Piecewise((log(1 - shape1), Eq(x, 0)), (log(shape1**(x**shape2) - shape1**((x + 1)**shape2)), True))"
limits:
  - name: degenerate
    when: '$\mathrm{shape2}\to\infty$ with $(-\log \mathrm{shape1})^{-1/\mathrm{shape2}}$ fixed between the observed value and the next integer (at 0: $\mathrm{shape1} \to 0$)'
    detect: shape1 and shape2 are free and all observations are equal
    status: PointMass
  - name: degenerate
    when: '$\mathrm{shape1}\to 0$'
    detect: shape2 is held and all observations are 0
    status: PointMass
    fixed: [shape2]
  - model: binom
    when: '$\mathrm{shape2}\to\infty$ with $\mathrm{shape1}$ at the fraction of ones (a binomial with size 1)'
    detect: shape1 and shape2 are free and the observations are 0 and 1, both present
    status: BernoulliLimit
  - name: adjacent two-point
    when: '$\mathrm{shape2}\to\infty$ with $\mathrm{shape1}^{(k+1)^{\mathrm{shape2}}}$ at the fraction of observations equal to $k+1$'
    detect: shape1 and shape2 are free and the observations take two adjacent values k and k + 1, k at least 1, both present
    status: TwoPointLimit
  - name: Bernoulli(shape1)
    when: '$\mathrm{shape2}\to\infty$'
    detect: shape1 is held and the observations are 0 and 1, with at least one 1
    status: HeldBernoulliLimit
    fixed: [shape1]
references:
  - "Nakagawa, T. and Osaki, S. (1975). The discrete Weibull distribution. IEEE Transactions on Reliability, R-24(5), 300–301. doi:10.1109/TR.1975.5214915."
examples: |
  AIC(mldweibull(corbet), mlgeom(corbet))
  mldweibull(corbet, shape2 = 1)
---

The type I discrete Weibull distribution of Nakagawa and Osaki (1975) models nonnegative counts such as the number of cycles survived before failure. A count is the integer part of a continuous Weibull variable with shape `shape2`; `shape1` is the probability that the count is positive.

## Details

The Weibull scale is $(-\log \mathrm{shape1})^{-1/\mathrm{shape2}}$, and `shape2 = 1` gives the geometric distribution with `prob = 1 - shape1`, an attained member.
The log-likelihood is strictly concave in $(\log(-\log \mathrm{shape1}), \mathrm{shape2})$ once an observation is positive: each count is an interval-censored observation of a log-Weibull variable.
The maximum likelihood estimator is therefore unique when it exists. With both parameters free it exists exactly when `max(x) - min(x) >= 2`; with `shape2` held, when some observation is positive; with `shape1` held and some positive observation, when some observation is at least 2.
With `shape1` held and all observations 0 the likelihood does not depend on `shape2`: the fitter warns and returns `shape2 = 1`.

Doubles close to 1 resolve $-\log \mathrm{shape1}$ only coarsely. When the fitted `shape1` is within `1e-6` of 1, `shape2` is refitted with that representable `shape1` held; when it rounds to 1 (the Weibull scale to the power `shape2` exceeds about $2^{53}$, e.g. counts near 40 with `shape2 = 10`) the fit stops with an error.
`extraDistr::pdweibull` does not floor non-integer arguments, so `pml` floors them while that defect persists; `extraDistr::ddweibull(log = TRUE)` underflows to `-Inf` far in the tail, and `qdweibull` and `rdweibull` can return `-1` for very small `shape2`. `logLik` is unaffected.


## Why a single profile bracket is global

With $a_k=k^\beta$ and $d_k=(k+1)^\beta-k^\beta$, the inner score is
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
