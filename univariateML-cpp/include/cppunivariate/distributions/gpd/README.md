---
id: gpd
inference_numeric:
  - regime: "positive shape fixed support"
    available: true
    condition: "finite mu held, sigma > 0 and xi > 0; sigma and/or xi free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels"
  - regime: "exponential member with shape held"
    available: true
    condition: "finite mu held, sigma > 0 free and xi = 0 held"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels"
  - regime: unreviewed or unavailable
    available: false
    condition: "joint moving-support regime with xi <= 0 and xi free"
    reason: "the moving-support C2 and fitted-subset classification for this numerical procedure is not yet completed"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Generalized Pareto
support: {type: real, lower: "mu", upper: "if (xi < 0) mu - sigma/xi else Inf", closed: [true, true], endpoint_log_density: {lower: "-log(sigma)", upper: "if (xi == -1) -log(sigma) else if (xi < -1) Inf else -Inf"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: design
  advice: 'Supply a finite held mu. With sigma and xi free, threshold ties give unbounded likelihood, so choose mu strictly below the data. Estimating mu is not supported, including when other coefficients are held.'
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: xi
  symbol: '\xi'
  role: shape
  domain: '(-inf, inf)'
  simulate: 0
  fit_domain:
    lower: -1.0
    restriction:
      mechanism: endpoint
      exempt_when_held: []
      reason: For xi below -1, moving the upper endpoint onto the sample maximum makes the likelihood unbounded when scale is free. The bound xi >= -1 removes this singularity.
  kind: regular
r:
  density: dgpd
  sampler: rgpd
  control:
    mu: Threshold, held fixed; every observation must be at or above it.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    xi: Shape to hold fixed, or NULL to estimate (lower bound -1).
    reltol: Relative tolerance for scale bisection and transformed profile root brackets, with a unit absolute floor for the latter.
    iterlim: Maximum number of bisection iterations per root.
log_density: Piecewise((-log(sigma)-(x-mu)/sigma, Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma), True))
limits:
  - firmness: proved
    name: threshold scale collapse
    fixed: [mu]
    status: InvalidData
    message: "scale collapse at the threshold"
    when: 'Scale $\sigma$ tends to zero with $\mu$ held. For held $\xi>0$ the likelihood has order $\sigma^{(n-k)/\xi-k}$, where $k$ counts threshold ties.'
    detect: 'Scale is free and either all observations equal $\mu$, or $k>0$ and shape is free, or held $\xi>0$ satisfies $\xi k\ge n-k$. Equality gives a finite unattained supremum for the fixed-shape scale fit.'
references:
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics,
  35(2), 185–191. doi:10.1080/00401706.1993.10485040.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds:
- held: []
  status: no-mle
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
- held:
  - xi
  status: not-implemented
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
- held: [sigma]
  status: not-implemented
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
- held: [sigma, xi]
  status: not-implemented
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
fit_args:
  mu: 0.0
examples: |
  x <- datasets::precip[datasets::precip > 40] - 40
  mlgpd(x, mu = 0)

group: extreme value
construction:
- base
- generalisation
relations: {}
fit:
  method: profile-search
  certified: partial
  note: 'gpd/README.md, Details; gpd/mle.hpp (public fitter and held overloads); gpd/test.cpp: Exponential shape-zero
    and fixed-shape endpoint members have exact subsets; general scale/shape profile search is numerical. The required
    threshold is held.'
areas:
- extremes and environment
classification: specialist
classification_reason: Used to model threshold exceedances in extreme-value analysis.
summary: Generalized Pareto provides a model for threshold exceedances in extreme-value analysis.
aliases:
- "GPD"
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free the likelihood is unbounded. Fits hold $\mu$
    and estimate shape within $\xi\ge-1$. With scale and shape free,
    any observation at $\mu$ still makes the likelihood unbounded.
    For strictly positive excesses a finite maximum exists within this
    shape range. Joint uniqueness is not established. The fit compares
    numerical profile candidates with the attained uniform boundary.
    With shape held at $\xi>-1$, a unique scale maximum exists unless threshold
    ties permit scale collapse. At $\xi=-1$, the scale maximum is the largest
    positive excess. Holding scale gives a finite shape maximum for every
    nonempty finite sample at or above the held threshold. It need not be
    unique, since all-zero excesses give a flat shape likelihood.
    With scale held and threshold free, a finite maximum exists within
    $\xi\ge-1$ for every nonempty finite sample, but these fits are not supported.
    Free-threshold subsets are not supported even when finite maxima exist.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    With $\xi<-1$, moving the upper endpoint onto the sample maximum creates
    a density singularity. For $\xi\ge-1$ and positive excesses, each $y>0$
    has density at most $g(\xi)/y$, where $g(\xi)=(1+\xi)^{-1-1/\xi}$,
    $g(0)=e^{-1}$ and $g(-1)=1$. This excludes $\xi\to\infty$ from
    maximizing sequences. On bounded shape intervals the likelihood tends
    to zero at both scale ends. The closed uniform member gives upper
    semicontinuity at $\xi=-1$, so a maximum is attained on a compact set.
    For $k$ threshold ties and held $\xi>0$,
    scale collapse gives likelihood of order $\sigma^{(n-k)/\xi-k}$.
    At equality the scaled score is strictly negative for every positive scale,
    so the finite path limit is unattained. For $\xi>-1$, the scale score
    multiplied by $\sigma$ is
    $(1+\xi)\sum_i y_i/(\sigma+\xi y_i)-n$, where $y_i=x_i-\mu$.
    It strictly decreases if any excess is positive and crosses zero except
    in the stated collapse cases. For $\xi=-1$, likelihood is $\sigma^{-n}$
    on $\sigma\ge\max_i y_i$.
    With scale held and threshold free within $\xi\ge-1$, either a uniform member attains
    the density bound $\sigma^{-n}$, or threshold profiles to the sample
    minimum and the shape likelihood vanishes at both ends of its feasible interval.
---

The generalized Pareto distribution models exceedances above a chosen threshold $\mu$. Positive shape $\xi$ gives a heavy right tail, zero shape gives an exponential distribution with scale $\sigma$, and negative shape gives a finite upper endpoint $\mu-\sigma/\xi$.

## Details

Observations must be finite and at or above the held threshold. For negative shape they must also satisfy the upper support bound. At $\xi=-1$ the model is uniform on the closed interval $[\mu,\mu+\sigma]$, so the largest observation can equal the upper endpoint with positive density.

Without a shape restriction, approaching the sample maximum with an upper endpoint at $\xi<-1$ makes the likelihood unbounded when scale is free. Fitting therefore estimates shape within $\xi\ge-1$. A fully held model can still be evaluated outside this fitting range. Data generated below the bound can give constrained fits at $\xi=-1$. This existence restriction does not imply regular inference; the usual regular MLE asymptotics do not apply on $[-1,-1/2)$.

For strictly positive excesses, a finite MLE exists within $\xi\ge-1$. Maximizing the density of one observation $y>0$ over scale gives the envelope $g(\xi)/y$, with $g$ defined above. Since $g(\xi)\to0$ as $\xi\to\infty$, a maximizing sequence has bounded shape. The density is also at most $1/\sigma$, excluding arbitrarily large scales. For bounded shapes it tends uniformly to zero as scale vanishes. This follows from exponential decay for feasible nonpositive shapes, and power bounds on positive-shape intervals, including the exponential limit at zero. The closed uniform endpoint makes the likelihood upper semicontinuous at the shape floor, so the remaining compact set contains a maximum. This is an existence argument, not a certificate for the numerical profile search.

For $k$ observations at the held threshold, each density factor equals $1/\sigma$. For a fixed $\xi>0$, every positive excess $d$ contributes asymptotically $\xi^{-1-1/\xi}d^{-1-1/\xi}\sigma^{1/\xi}$. Thus the likelihood has order $\sigma^{(n-k)/\xi-k}$. It diverges if $\xi k>n-k$. At equality its positive finite limit is the supremum for that fixed-shape scale problem: the scaled scale score is negative everywhere. If every observation equals the threshold, likelihood diverges as $\sigma^{-n}$. With both shape and scale free, any threshold tie allows a strictly negative exponent by choosing $\xi>(n-k)/k$.

For held shape $\xi>-1$ and at least one positive excess, a strictly decreasing scaled scale score gives a unique root whenever collapse is excluded. At shape zero this is the mean excess. At $\xi=-1$ the uniform maximum is attained at the largest excess, provided it is positive. Constant positive excesses and single positive observations are allowed. Holding scale instead gives a scalar shape likelihood over its support-feasible interval within $\xi\ge-1$. The fit compares sampled and refined interior candidates with the attained floor. A finite maximum exists because any positive excess excludes an infinite shape through the density envelope, while the feasible lower edge is either the attained uniform member or has zero likelihood. All-zero excesses with scale held give the same likelihood for every shape and hence no unique estimate.

Joint fitting uses the transformed scale/shape profile and compares its candidate roots with the exponential member and uniform boundary. Bounds restrict the profile search, but sampling and refinement do not certify that every stationary point has been found. Global coverage remains numerical. Unresolved scalar searches stop.

A free threshold is excluded by interface policy for every held subset. This is stronger than a mathematical assertion of non-existence. For example, with exponential shape and scale held, the likelihood increases to a finite maximum at the sample minimum. With uniform shape and scale held, compatible thresholds form an attained plateau. The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-extreme.md) records the limiting paths; its older universal free-threshold claim does not cover these closed-support submodels.

With scale held, estimating the threshold still permits a finite maximum within $\xi\ge-1$. Let $R=\max_i x_i-\min_i x_i$. If $\sigma\ge R$, a uniform member covers the sample and attains the joint density bound $\sigma^{-n}$. If $0<\sigma<R$, support feasibility requires $\xi\ge-\sigma/R>-1$. The density then increases as the threshold rises to the sample minimum. At that threshold the shape likelihood is continuous and positive on $(-\sigma/R,\infty)$ and tends to zero at both ends, so a finite maximum exists. This argument does not prove uniqueness, and the interface still requires a held threshold.

## R notes

Supply `mu` explicitly. With shape and scale free, choose it strictly below the data to avoid threshold collapse. All three coefficients may be held; incompatible support is rejected. Estimated coefficients on an applicable `fit_domain` bound carry `restricted` and emit `univariateML_restricted` warnings. Held coefficients are not flagged. Failed optimizers return failure unless the total score norm in fitting coordinates is below `1e-3`; unresolved scalar searches stop.
