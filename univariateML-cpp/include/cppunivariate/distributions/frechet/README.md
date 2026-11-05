---
id: frechet
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite lambda > 0, sigma > 0 and mu; mu held; lambda and/or sigma free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Frechet
support: {type: real, lower: "mu", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "-Inf"}}
params:
- name: lambda
  symbol: '\lambda'
  role: shape
  domain: '(0, inf)'
  simulate: 1
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held:
      - - mu
      reason: >-
        The bound lambda >= 1 excludes scale collapse at a freely fitted
        threshold when the minimum occurs fewer than half the time. Exceptional
        ties still require a separate check. It also excludes a shape tending to
        zero while mu approaches the minimum, which can make the likelihood
        unbounded even with sigma held. Holding mu gives an inverse Weibull
        model on x - mu, for which every positive lambda is admissible.
  kind: regular
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: threshold
  advice: >-
    Holding mu fits an inverse Weibull model on x - mu. Use mu = NULL to
    estimate the threshold with free lambda restricted to lambda >= 1.
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dfrechet
  sampler: rfrechet
  control:
    mu: Threshold to hold fixed; NULL estimates it.
    lambda: Positive shape to hold fixed, or NULL to estimate.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    reltol: Profile score and root bracket tolerance.
    iterlim: Maximum iterations per scalar solve.
log_density: log(lambda)-log(sigma)-(lambda+1)*log((x-mu)/sigma)-((x-mu)/sigma)**(-lambda)
limits:
  - firmness: proved
    name: fixed-shape collapse
    fixed: [lambda]
    status: InvalidData
    message: "unattained collapse face"
    when: 'With shape $\lambda$ held, $\mu=m-\varepsilon$ approaches the sample minimum $m$ and $\sigma=c\varepsilon$ decreases to zero for a positive constant $c$.'
    detect: 'Location and scale are free, and the minimum occurs $k$ times with $\lambda(n-k)\le k$. The strict inequality gives divergence. Equality gives a finite, unattained collapse face.'
  - firmness: proved
    name: shape divergence
    fixed: [sigma]
    status: InvalidData
    message: "unbounded shape likelihood"
    when: 'With scale $\sigma$ held, the free shape $\lambda$ tends to infinity and $\mu$ equals the common observation minus $\sigma$.'
    detect: 'The sample is constant and location is free, or every observation minus the held location equals $\sigma$.'
  - firmness: proved
    model: gumbel
    when: 'The shape $\lambda$ tends to infinity with $\sigma/\lambda=a>0$ and $\mu+\sigma=b$ fixed.'
    detect: 'A numerical comparison selects the Gumbel face over the finite candidates when location, scale and shape are free.'
    status: GumbelLimit
    numerical: true
  - firmness: numerical
    name: free collapse
    status: NoMLE
    message: "minimum multiplicity permits scale collapse"
    when: 'The location $\mu$ approaches the sample minimum from below, $\lambda$ tends to one, and $\sigma$ decreases proportionally to the location gap.'
    detect: 'All coefficients are free, the minimum occurs exactly $n/2$ times, and no numerically found finite candidate beats the collapse face. This comparison does not certify that every finite competitor has been found.'
    numerical: true
fit_args:
  mu: ~
examples: |
  set.seed(1)
  x <- rml(200, mlfrechet())
  mlfrechet(x, mu = NULL)

group: extreme value
construction:
- base
relations:
  transform_of:
  - family: invweibull
    transform: shift
    at:
      shape: lambda
      scale: sigma
    shift: mu
  special_case_of:
  - family: gev
    at:
      mu: mu+sigma
      sigma: sigma/lambda
      xi: 1/lambda
fit:
  method: delegation
  certified: 'partial'
  note: 'frechet/README.md, Details; frechet/mle.hpp (public fitter and held overloads); frechet/test.cpp: Every subset with mu held is an inverse-Weibull fit (joint concave log profile, held-shape closed form or held-scale monotone shape score). Free mu retains numerical location/profile searches: all regular coefficients free, lambda held, sigma held, or both held.'
areas:
- extremes and environment
classification: specialist
classification_reason: Used to model block maxima with polynomially decaying upper tails.
summary: Frechet provides a model for block maxima with polynomially decaying upper tails.
aliases:
- "type II extreme value"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free and unrestricted shape, the likelihood is
    unbounded for every sample. The fitted model therefore restricts $\lambda\ge1$.
    A minimum repeated more than $n/2$ times gives unbounded likelihood.
    At exactly $n/2$ repeats, a finite collapse face must be compared with
    finite candidates. A Gumbel limit can also prevent attainment.
    A complete existence criterion and joint uniqueness are not established.
    With $\mu$ held and observations strictly above it, both remaining
    coefficients have a unique finite MLE exactly for nonconstant data.
    Holding $\mu$ and $\lambda$ gives a unique finite scale estimate even
    for constants. Holding $\mu$ and $\sigma$ gives a unique finite shape
    estimate unless every $x_i-\mu=\sigma$, when likelihood is unbounded.
    With only $\lambda$ held, a finite MLE exists exactly when
    $\lambda(n-k)>k$, where $k$ is the minimum multiplicity.
    Otherwise the supremum is an unattained collapse face or is unbounded. With only $\sigma$ held,
    constant data have unbounded shape likelihood.
    Holding $\lambda$ and $\sigma$ gives a unique finite location maximum.
    Uniqueness for the remaining free-location fits is not established.
    Holding all coefficients leaves only likelihood evaluation.
  reason: >-
    Holding location reduces the model to inverse Weibull on $x_i-\mu$,
    whose monotone shape equation proves the stated conditional results.
    For free location, put $\mu=m-\varepsilon$ and
    $\sigma=c\varepsilon$. The likelihood is proportional asymptotically
    to $\varepsilon^{\lambda(n-k)-k}$ at fixed shape.
    The Gumbel path and the half-tie collapse face are derived in Details.
    These paths prove obstructions and construct limiting competitors.
    They do not prove that the numerical profile search exhausts all maxima
    or all joint escape paths.
---

The Fréchet distribution models extreme maxima with heavy right tails. The threshold $\mu$ sets the lower endpoint, $\sigma$ sets the scale above it, and $\lambda$ controls the tail decay.

## Details

For the Gumbel face fix $b\in\mathbb R$ and $a>0$, set
$\sigma=\lambda a$ and $\mu=b-\lambda a$, and let $\lambda\to\infty$.
For fixed $x$, $((x-\mu)/\sigma)^{-\lambda}\to
\exp(-(x-b)/a)$, and differentiating the Fréchet CDF gives the Gumbel
density limit. Scheffé's lemma gives total-variation convergence. This
is a density limit, not a certificate of the face comparison.

For the free collapse path put $m=\min x_i$, $\mu=m-\varepsilon$,
$\lambda=1$, and $\sigma=(n/k)\varepsilon$, where $k$ observations equal
$m$. The minimum density is $(n/k)e^{-n/k}/\varepsilon$, and each
nonminimum density is asymptotic to $(n/k)\varepsilon/(x_i-m)^2$.
Thus the likelihood is asymptotic to a positive constant times
$\varepsilon^{n-2k}$; at $k=n/2$ it has a finite positive limit. This
constructs the face but does not prove that a numerical search has found
the best finite competitor or that all joint escape paths are exhausted.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-extreme.md) describes the limiting paths and the scope of the boundary comparisons.

When the location is free, the minimum multiplicity controls the positive-shape collapse; ties above the minimum do not cause it.

With shape and scale free, a held-location fit requires a nonconstant sample strictly above the threshold. Its existence and identification are exactly those of inverse Weibull after subtracting the threshold. Holding location permits every positive shape. Holding location and shape gives the explicit scale estimate
$\hat\sigma=(n/\sum_i(x_i-\mu)^{-\lambda})^{1/\lambda}$.
Holding location and scale leaves a strictly concave shape likelihood, solved by bisection. These conditional cases allow a single observation and constants unless the stated shape divergence applies.

When location and shape are free, the fitted shape is restricted to $\lambda\ge1$. The attained member $\lambda=1$ is included. The joint fitter requires at least three observations. A minimum repeated more than half the time always gives unbounded likelihood under this restriction. At an exact half tie, the fitter compares the numerically found finite candidates against the finite collapse face derived above. No interior candidate beat this face in the extensive tests recorded in the [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-extreme.md). This is numerical evidence, not a proof that the face always wins.

For a minimum repeated $k$ times, the fixed-shape collapse exponent is $\lambda(n-k)-k$. Strictly negative values give divergence, including every positive shape for constant data. With only shape held, the fitter requires $\lambda(n-k)>k$ and rejects equality as an unattained collapse face. In this fixed-shape case the rejection also follows from a global derivative bound. Put $r_i=\varepsilon/(x_i-m+\varepsilon)$ and profile out scale. Then
$\varepsilon\ell'(\varepsilon)=n\lambda\sum_i r_i^{\lambda+1}/\sum_i r_i^\lambda-(\lambda+1)\sum_i r_i$.
Since $r_i\le1$, the ratio is at most one, while $\sum_i r_i>k$ for nonconstant data. Thus the derivative is negative whenever $n\lambda\le(\lambda+1)k$, including equality. The supremum is confined to the unattained collapse face. If $\lambda(n-k)>k$, the profiled likelihood tends to zero both as the location gap decreases to zero and as it grows without bound. It therefore attains a finite maximum. This fixed-shape argument does not certify the search with shape free.

Holding shape and scale leaves a scalar location fit and permits every positive shape, including values below one. Its likelihood tends to zero at both ends of the location domain, so a finite maximum exists. Uniqueness follows by writing $d_i=x_i-\mu$ and $R=\sum_i d_i^{-\lambda-1}/\sum_i d_i^{-1}$. As the location gap increases, $R$ decreases strictly from infinity to zero. Indeed, under weights proportional to $d_i^{-1}$, its derivative is
$-(\lambda+1)E_w(d^{-\lambda-1})+E_w(d^{-\lambda})E_w(d^{-1})<0$,
since the covariance of the two decreasing powers of $d$ is nonnegative. The gap score changes sign exactly when $R=(\lambda+1)/(\lambda\sigma^\lambda)$. Holding only scale restricts the free shape to $\lambda\ge1$ and rejects constant samples because of shape divergence. The lower shape bound also excludes a different spike that is possible even with scale held. Put $\mu=m-\sigma e^{-L}$ and $\lambda=c/L$ for fixed $c>0$. As $L$ increases, each minimum density is proportional to $e^L/L$, while every other observation contributes a density proportional to $1/L$. Thus the unrestricted likelihood grows as $e^{kL}/L^n$ for any sample with minimum multiplicity $k$. The location search profiles the exact scale or the concave shape solve and refines sampled local maxima. Its global coverage remains numerical.

The joint fit uses the generalized extreme-value coordinates: location $\mu+\sigma$, scale $\sigma/\lambda$, and shape $1/\lambda$. It scans profile score roots and compares finite candidates with the Gumbel and collapse faces. Root discovery and the face comparison have no global certificate. Unresolved scalar solves stop fitting.

## R notes

All hold combinations are supported. Supply `mu` explicitly: a value holds the threshold, and `mu = NULL` estimates it. With `mu` free and `lambda` free, the fit restricts `lambda >= 1`. With `mu` held, `mlinvweibull` on `x - mu` gives the corresponding two-parameter fit.

The Gumbel face wins ties within $10^{-8}n$ log-likelihood units, and fitting stops with advice to use `mlgumbel`. Estimated coefficients on an applicable `fit_domain` bound carry a `restricted` attribute and emit a warning of class `univariateML_restricted`. Held coefficients are not flagged. Restriction exemptions apply to free coefficients. Failed optimizers and unresolved scalar searches stop. A small score alone does not establish global optimality.
