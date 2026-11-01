---
id: sged
inference_numeric:
  - regime: symmetric member with held location
    available: true
    condition: "finite mean held, sd > 0 and nu > 0; xi = 1 held; if nu is free require nu > 1"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels"
  - regime: symmetric normal member
    available: true
    condition: "finite mean and sd > 0; nu = 2 and xi = 1 held; mean and/or sd free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels"
  - regime: "cusp moving with smooth exponent"
    available: true
    condition: "finite mean, sd > 0, xi > 0 and nu > 2; any regular subset free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels"
  - regime: unreviewed or unavailable
    available: false
    condition: "joint cusp-moving regime with 0 < nu <= 2, excluding the stated symmetric held-parameter scopes"
    reason: "the C2 and held-subset classification at the moving cusp has not yet been completed"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Skew Generalized Error
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: nu
  symbol: '\nu'
  role: shape
  domain: '(0, inf)'
  simulate: 2
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: spike
      exempt_when_held: []
      reason: nu >= 1 selects the Laplace member; holding sd does not remove spikes
        (P0 locscale/skewreal).
  kind: regular
- name: xi
  symbol: '\xi'
  role: skew
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dsged
  sampler: rsged
  control:
    mean: "Optional held mean; NULL estimates it where the hold subset is supported."
    sd: "Optional held sd; NULL estimates it where the hold subset is supported."
    nu: "Optional held nu; NULL estimates it where the hold subset is supported."
    xi: "Optional held xi; NULL estimates it where the hold subset is supported."
    gradtol: "Tolerance on the Euclidean norm of the total negative log-likelihood gradient in transformed coordinates; fixed step and objective-decrease criteria can also stop the fit."
    iterlim: "Maximum number of optimisation iterations."
log_density: "log(sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)) - log(sd) + log(2/(xi + 1/xi)) + log(nu/(sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu))*2**(1 + 1/nu)*gamma(1/nu))) - Abs(Piecewise(((sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)*(x - mean)/sd + (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))*(xi - 1/xi))/xi, (sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)*(x - mean)/sd + (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))*(xi - 1/xi)) > 0), ((sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)*(x - mean)/sd + (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))*(xi - 1/xi))*xi, True)))**nu/(2*(sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu)))**nu)"
limits:
  - {firmness: numerical, name: unresolved constrained uniform endpoint rates, fixed: [sd], when: "fixed uniform width equals the sample range and its endpoint face would win", status: ScaleBoundary}
  - {firmness: numerical, name: unresolved constrained uniform endpoint rates, fixed: [sd, xi], when: "fixed uniform width equals the sample range and its endpoint face would win", status: ScaleBoundary}
  - {firmness: argued, model: unif, fixed: [mean], when: "nu tends to infinity with the held moment constraints", status: UniformBoundary}
  - {firmness: argued, name: right translated half-family, fixed: [mean], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfGedBoundary}
  - {firmness: argued, name: left translated half-family, fixed: [mean], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfGedBoundary}
  - {firmness: proved, name: constant sample scale collapse, fixed: [mean], when: "All observations equal the free or held mean and sd tends to zero", status: Unbounded}
  - {firmness: argued, model: unif, fixed: [sd], when: "nu tends to infinity with the held moment constraints", status: UniformBoundary}
  - {firmness: argued, name: right translated half-family, fixed: [sd], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfGedBoundary}
  - {firmness: argued, name: left translated half-family, fixed: [sd], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfGedBoundary}
  - {firmness: argued, name: right translated half-family, fixed: [nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfGedBoundary}
  - {firmness: argued, name: left translated half-family, fixed: [nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfGedBoundary}
  - {firmness: proved, name: constant sample scale collapse, fixed: [nu], when: "All observations equal the free or held mean and sd tends to zero", status: Unbounded}
  - {firmness: argued, model: unif, fixed: [xi], when: "nu tends to infinity with the held moment constraints", status: UniformBoundary}
  - {firmness: proved, name: constant sample scale collapse, fixed: [xi], when: "All observations equal the free or held mean and sd tends to zero", status: Unbounded}
  - {firmness: argued, model: unif, fixed: [mean, sd], when: "nu tends to infinity with the held moment constraints", status: UniformBoundary}
  - {firmness: argued, name: right translated half-family, fixed: [mean, sd], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfGedBoundary}
  - {firmness: argued, name: left translated half-family, fixed: [mean, sd], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfGedBoundary}
  - {firmness: argued, name: right translated half-family, fixed: [mean, nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfGedBoundary}
  - {firmness: argued, name: left translated half-family, fixed: [mean, nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfGedBoundary}
  - {firmness: proved, name: constant sample scale collapse, fixed: [mean, nu], when: "All observations equal the free or held mean and sd tends to zero", status: Unbounded}
  - {firmness: argued, model: unif, fixed: [mean, xi], when: "nu tends to infinity with the held moment constraints", status: UniformBoundary}
  - {firmness: proved, name: constant sample scale collapse, fixed: [mean, xi], when: "All observations equal the free or held mean and sd tends to zero", status: Unbounded}
  - {firmness: argued, name: right translated half-family, fixed: [sd, nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfGedBoundary}
  - {firmness: argued, name: left translated half-family, fixed: [sd, nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfGedBoundary}
  - {firmness: argued, model: unif, fixed: [sd, xi], when: "nu tends to infinity with the held moment constraints", status: UniformBoundary}
  - {firmness: proved, name: constant sample scale collapse, fixed: [nu, xi], when: "All observations equal the free or held mean and sd tends to zero", status: Unbounded}
  - {firmness: argued, name: right translated half-family, fixed: [mean, sd, nu], when: "xi tends to infinity with endpoint and scale constrained by held mean and sd", status: RightHalfGedBoundary}
  - {firmness: argued, name: left translated half-family, fixed: [mean, sd, nu], when: "xi tends to zero with endpoint and scale constrained by held mean and sd", status: LeftHalfGedBoundary}
  - {firmness: argued, model: unif, fixed: [mean, sd, xi], when: "nu tends to infinity with the held moment constraints", status: UniformBoundary}
  - {firmness: proved, name: scale collapse, fixed: [mean, nu, xi], when: "all observations equal held mean and sd tends to zero", status: Unbounded, message: "Scale collapse"}
  - {firmness: numerical, name: uncertified scale boundary, fixed: [mean, nu, xi], when: "conditional scale search reaches its numerical tail guard", status: ScaleBoundary, message: "Scale-collapse boundary"}
  - model: unif
    firmness: argued
    when: '$\nu \to \infty$'
    detect: Uniform log-likelihood exceeds all finite candidates.
    status: UniformBoundary
  - name: right translated half-GED
    firmness: argued
    when: '$\xi \to \infty$'
    detect: Right translated half-GED profile wins.
    status: RightHalfGedBoundary
  - name: left translated half-GED
    firmness: argued
    when: '$\xi \to 0$'
    detect: Left translated half-GED profile wins.
    status: LeftHalfGedBoundary
references:
- Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American
  Statistical Association, 93(441), 359-371.
- 'Nelson, D. B. (1991). Conditional Heteroskedasticity in Asset Returns: A New Approach. Econometrica, 59, 347-370.'
- Tumlinson, S. E. (2015). On the non-existence of maximum likelihood estimates for the extended exponential power
  distribution and its generalizations. Statistics & Probability Letters, 107, 111-114. doi:10.1016/j.spl.2015.08.009.
- 'fGarch documentation (accessed 2026-10-31). fGarch: Rmetrics - Autoregressive Conditional Heteroskedastic Modelling. R package reference
  manual. https://cran.r-project.org/web/packages/fGarch/fGarch.pdf.'
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mlsged(x)

group: skewed and robust location-scale
construction:
- skewing
- location-scale
relations: {}
fit:
  method: multistart
  certified: 'no'
  note: 'sged/README.md, Details; sged/mle.hpp (public fitter and held overloads); sged/test.cpp: Default and nontrivial
    conditional fits use multistart, finite grids or local profile refinement without a global certificate; all-held
    density evaluation is not counted as certification.'
areas:
- finance
- measurement and errors
classification: specialist
classification_reason: Used to model asymmetric financial errors with separate skewness and exponential-power tails.
summary: Skew Generalized Error provides a model for asymmetric financial errors with separate skewness and exponential-power
  tails.
aliases: []
key_references:
- 4
---

The skew generalized error distribution extends the exponential-power family with Fernandez-Steel asymmetry. Its location and scale parameters are the mean and standard deviation.

## Details

The [conditional skew analysis](../../../../../dev/research/evaluations/2026-10-29-p2b-skew-report.md) derives the collapse exponents and face constraints; its equality cases and global comparisons remain uncertified.

The 2026-10-31 fit-speed revision computes the fixed shape/skew density
normalizers and kernel scale once in the conditional location/scale search. Each
density contribution retains the previous arithmetic grouping and observation
order. Search grids, observation-kink refinements and failure handling are
unchanged. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

Fitting restricts the shape to $\nu \ge 1$, the log-concave GED range,
including the attained Laplace member at $\nu=1$. Without this restriction
the likelihood is unbounded as $\nu \to 0$ for nonconstant samples
(Tumlinson, 2015). Estimated shapes below one are not fitted.
fGarch's own `gedFit` allows positive shapes below one and returns local
solutions. Density, probability, quantile and random generation functions
retain the full $\nu>0$ domain. Coefficients follow fGarch's mean and
standard deviation parameterisation.

The fitter compares several interior starts with the exact Laplace fit
and the uniform limit. Total log-likelihood differences of at most
$10^{-8}n$ are ties, with an attained member preferred to a limit.
A winning uniform limit is reported with a recommendation to use `mlunif`.

For the skew-Laplace member with mode $c$, left scale $a$ and right scale
$b$, write $A=\sum_{x<c}(c-x)$ and $B=\sum_{x>c}(x-c)$.
Profiling gives $a=\sqrt A(\sqrt A+\sqrt B)/n$ and the analogous
formula for $b$. The remaining objective $(\sqrt A+\sqrt B)^2$ is
concave between observations, so a minimum is at an observation. All are
checked with prefix sums. The mapping is mean $c+b-a$, standard deviation
$\sqrt{a^2+b^2}$ and skew $\sqrt{b/a}$.

Both translated half-GED faces are also compared. For distances $y$ from
the appropriate sample endpoint, the scale satisfies
$a^\nu=\nu\operatorname{mean}(y^\nu)$. The one-dimensional profile
is scanned and refined in log shape from 0 to 20; the infinite-shape
endpoint is the uniform. A winning half face has no MLE in the skewed
family and is reported explicitly.


Every coefficient may be held, singly or jointly; held values are returned
exactly. Conditional fits compare multistart interior searches with every
reachable half-family and shape boundary, retaining the held mean and sd
constraints on those faces. The fixed-shape, fixed-skew location-scale fit
profiles scale and scans location, including observation kinks. Scalar
searches scan before refining competing brackets. Numerical multistart and
finite scans provide evidence rather than a global optimality certificate.
A search reaching its numerical guard stops instead of returning a fit.

For fixed finite $\xi>0$ and $\nu\ge1$, the standardized log-density is
a constant minus a positive, side-dependent multiple of $|z-c|^\nu$.
This piecewise power is convex: the one-sided slopes match at the join for
$\nu>1$, and are ordered there for $\nu=1$. Hence the log-likelihood is
concave in $(\mu/\sigma,1/\sigma)$ after the fixed-shape affine
standardization. At $\nu=1$ the kink requires subgradient treatment; for
$0<\nu<1$ the kernel is not log-concave. Infinite shape and vanishing or
infinite skew lead to uniform or half-family limiting faces, which must be
compared separately. The default free fit still uses numerical multistart
optimization over both shapes and location-scale parameters; fixed-shape
concavity alone does not certify that search.

For a possible further reduction, parameterize the kernel by its mode $c$
and positive left and right scales $a,b$. For fixed $c,\nu$, put
$A=\sum_{x_i<c}(c-x_i)^\nu$, $B=\sum_{x_i>c}(x_i-c)^\nu$ and
$p=1/(\nu+1)$. When $A,B>0$, the scale score equations give
$a=(\nu/n)^{1/\nu}A^p(A^p+B^p)^{1/\nu}$ and the analogous expression
for $b$. The profiled likelihood therefore decreases with $A^p+B^p$.
This removes the side scales, but it does not by itself prove unimodality
or provide a stopping rule for the remaining mode and shape searches;
zero-side and uniform limits also require separate treatment.

| TASK-116 property | Evidence |
|---|---|
| Fixed finite $\xi>0$, $\nu\ge1$ | Concavity in inverse-scale coordinates, proved as above. |
| Conditional fits | Numerical scans and refinements; not an interval-certified inner solve. |
| Default free shapes and limiting faces | Multistart and face comparisons; numerical, with no global shape certificate. |

With mean and shapes held, scale maxima exist unless all observations equal
the held mean, when scale collapse makes the likelihood unbounded. Held
positive shapes below one are accepted for these conditional fits; the
$\nu\ge1$ restriction applies to estimated shape. With scale and both shapes held,
location maxima exist even with ties.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.

When both mean and sd are held, data exactly at a limiting uniform
endpoint retain the GED endpoint density penalty. If only sd is held
and its limiting uniform width equals the sample range exactly, a winning
endpoint face is reported as uncertified rather than returning a uniform
proxy for its constrained likelihood.

## d/p/q/r

Uses the fGarch mean and standard deviation parameterisation.
