---
id: sged
name: Skew Generalized Error
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: nu
  role: shape
  domain: '(0, inf)'
  simulate: 3.0
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: spike
      exempt_when_held: []
      reason: nu >= 1 selects the Laplace member; holding sd does not remove spikes
        (P0 locscale/skewreal).
  kind: regular
- name: xi
  role: skew
  domain: (0, inf)
  simulate: 3.0
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
  - {name: scale collapse, fixed: [mean, nu, xi], when: "all observations equal held mean and sd tends to zero", status: Unbounded, message: "Scale collapse"}
  - {name: uncertified scale boundary, fixed: [mean, nu, xi], when: "conditional scale search reaches its numerical tail guard", status: ScaleBoundary, message: "Scale-collapse boundary"}
  - model: unif
    when: '$\nu \to \infty$'
    detect: Uniform log-likelihood exceeds all finite candidates.
    status: UniformBoundary
  - when: '$\xi \to \infty$'
    detect: Right translated half-GED profile wins.
    status: RightHalfGedBoundary
  - when: '$\xi \to 0$'
    detect: Left translated half-GED profile wins.
    status: LeftHalfGedBoundary
references:
  - "Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American Statistical Association, 93(441), 359-371."
  - "Nelson, D. B. (1991). Conditional Heteroskedasticity in Asset Returns: A New Approach. Econometrica, 59, 347-370."
  - "Tumlinson, S. E. (2015). On the non-existence of maximum likelihood estimates for the extended exponential power distribution and its generalizations. Statistics & Probability Letters, 107, 111-114. doi:10.1016/j.spl.2015.08.009."
examples: |
  mlsged(precip)
holds:
- {held: [mean], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [sd], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, sd], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [sd, nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [sd, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [nu, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, sd, nu], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
- {held: [mean, sd, xi], status: not-implemented, reason: "Hold subset existence remains unresolved (P0)"}
---

The skew generalized error distribution extends the exponential-power family with Fernandez-Steel asymmetry. Its location and scale parameters are the mean and standard deviation.

## Details

Fitting restricts the shape to $\nu \ge 1$, the log-concave GED range,
including the attained Laplace member at $\nu=1$. Without this restriction
the likelihood is unbounded as $\nu \to 0$ for nonconstant samples
(Tumlinson, 2015). Very sharp-peaked shapes below one are not fitted.
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


Holding `nu` and `xi` together with `sd` supports location estimation;
holding them together with `mean` supports scale estimation. All coefficients
may also be held for likelihood evaluation. The remaining partial holds
retain explicit implementation exceptions because their existence criteria
were unresolved in P0. Conditional searches scan and refine a one-dimensional
likelihood, including density kinks in location; numerical comparisons do
not certify global optimality.

With mean and shapes held, scale maxima exist unless all observations equal
the held mean, when scale collapse makes the likelihood unbounded. Held
positive shapes below one are accepted for these conditional fits; the
$\nu\ge1$ restriction applies to estimated shape. With scale held,
location maxima exist even with ties.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.
