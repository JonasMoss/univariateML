---
id: lomax
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite lambda > 0 and kappa > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Lomax
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: kappa
  symbol: '\kappa'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dlomax
  sampler: rlomax
  control:
    lambda: Optional held lambda; NULL estimates it.
    kappa: Optional held kappa; NULL estimates it.
    lambda0: Optional starting value for `lambda`; `NULL` uses a method-of-moments start.
    reltol: Relative tolerance on the Newton step for `lambda * mean(x)`, with an absolute floor of the same size in these dimensionless units.
    iterlim: Maximum number of Newton iterations.
log_density: log(lambda) + log(kappa) - (kappa + 1)*log(1 + lambda*x)
limits:
  - model: exp
    firmness: proved
    when: '$\kappa$ tends to infinity while $\lambda\kappa$ tends to a positive exponential rate'
    detect: 'both coefficients are free and the numerical profile search finds no improvement over the best exponential larger than $10^{-8}n$ log-likelihood units'
    status: ExponentialLimit
    numerical: true
references:
- del Castillo, J. and Daoudi, J. (2009). Estimation of the generalized Pareto distribution. Statistics & Probability
  Letters 79(5), 684–688. doi:10.1016/j.spl.2008.10.021.
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics
  35(2), 185–191. doi:10.1080/00401706.1993.10485040.
- Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley, p.
  60.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mllomax())
  mllomax(x)

group: Pareto and Lomax
construction:
- base
relations:
  special_case_of:
  - family: burr
    at:
      shape1: kappa
      shape2: 1
      scale: 1/lambda
  - family: pareto2
    at:
      shape: kappa
      scale: 1/lambda
      min: 0
  - family: betapr
    at:
      shape1: 1
      shape2: kappa
      scale: 1/lambda
fit:
  method: profile-search
  certified: partial
  note: 'lomax/README.md, Details; lomax/mle.hpp (public fitter and held overloads); lomax/test.cpp: Either held
    coefficient gives a unique finite fit; joint scale profile and exponential-face comparison remain numerical
    rather than a global certificate.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model positive losses and waiting times with a Pareto tail from zero.
summary: Lomax provides a model for positive losses and waiting times with a Pareto tail from zero.
aliases:
- "Pareto type II (zero threshold)"
- "Pareto of the second kind (zero threshold)"
key_references:
- 4
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a finite MLE exists exactly when
    $\psi(u) \le 0$ for some $0 < u < \bar u$, with $\psi$ and $\bar u$
    defined below. Otherwise the supremum is the exponential limit.
    Joint uniqueness is not established. Holding either coefficient gives
    a unique finite MLE for every nonempty positive sample, including constant data.
  reason: >-
    Put $y_i=x_i/\bar x$, $q=\lambda\bar x$ and
    $A(q)=n^{-1}\sum_i\log(1+qy_i)$. Profiling gives $\hat\kappa=1/A(q)$
    and a log-likelihood gap from the best exponential of
    $n\{\log q-\log A(q)-A(q)\}$. This tends to zero as $q\to0$ and
    to $-\infty$ as $q\to\infty$. At $q=ue^u$ its sign is that of
    $-\psi(u)$. For $u\ge\bar u$, $\psi(u)>\log(u/\bar u)\ge0$.
    With $\lambda$ held the likelihood is strictly concave in $\kappa$.
    With $\kappa$ held the score in $\log\lambda$ strictly decreases
    from $n$ to $-n\kappa$.
---

The Lomax (Pareto type II) distribution models positive incomes, losses and waiting times with a heavy right tail.

## Details

The rate $\lambda$ sets the inverse scale, and the shape $\kappa$ controls the tail. Smaller $\kappa$ gives a heavier tail.

For the existence criterion, put $y_i=x_i/\bar x$, $\psi(u)=n^{-1}\sum_i\log(e^{-u}+uy_i)$ and $\bar u=\bar x/(\prod_i x_i)^{1/n}$. The finite MLE exists if and only if $\psi(u)\le0$ somewhere in $0<u<\bar u$. This follows by comparing the profiled likelihood with the best exponential likelihood, as shown above. It does not establish uniqueness. The fit searches this bounded interval numerically and refines candidate maxima. The search is not a global certificate.

The moment condition $\overline{x^2}<2\bar x^2$ decides only local behaviour at the exponential limit and does not rule out a finite MLE. For example, the sample $(1,100)$ has an MLE despite satisfying that condition. For two observations, the transition occurs at a larger-to-smaller ratio of approximately 28.81, obtained numerically; the criterion above determines whether the transition value itself is included.

Holding either coefficient removes the exponential escape and gives a unique finite fit, even for constant data. Holding both evaluates the likelihood at the supplied coefficients.

## R notes

The implementation prefers the exponential limit when the largest profile improvement it finds is at most $10^{-8}n$ log-likelihood units and stops with a recommendation to use `mlexp`. This tolerance can reject a mathematical MLE that ties or only slightly improves on the limit.

With $\kappa$ held, the total log-rate score is $n-(\kappa+1)\sum_i\operatorname{logistic}(\log\lambda+\log x_i)$. It strictly decreases from $n$ to $-n\kappa$, so its unique root is the global conditional maximum. The solver expands a log-rate bracket within the representable positive range, verifies both signs, and requires absolute total score below $10^{-3}$ at the represented rate. Log probabilities avoid overflow and underflow in the weighted logistic terms. If the root cannot be represented or certified numerically, fitting returns `IterationFailure`.
