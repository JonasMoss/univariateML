---
id: sged
inference_numeric:
  - regime: "symmetric member with held location"
    available: true
    condition: "finite mean held, sd > 0 and nu > 0; xi = 1 held; if nu is free require nu > 1"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels"
  - regime: "symmetric normal member"
    available: true
    condition: "finite mean and sd > 0; nu = 2 and xi = 1 held; mean and/or sd free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels"
  - regime: "cusp moving with smooth exponent"
    available: true
    condition: "finite mean, sd > 0, xi > 0 and nu > 2; any regular subset free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels"
  - regime: "unreviewed or unavailable"
    available: false
    condition: "joint cusp-moving regime with 0 < nu <= 2, excluding the stated symmetric held-parameter scopes"
    reason: "DQM and finite full-rank information hold for nu > 1/2, but moving-mode ordinary Hessians are invalid for nu <= 1; joint fitted cusp Hessians and numerical join handling in the remaining excluded subsets are not yet validated"
    classification: "dev/research/evaluations/2026-11-04-dqm-b-report.md#two-piece-models-snorm-sstd-and-sged"
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
      reason: Estimating nu is restricted to nu >= 1 to exclude the small-shape spike.
        Holding sd alone does not exclude that mechanism.
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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: "log(sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)) - log(sd) + log(2/(xi + 1/xi)) + log(nu/(sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu))*2**(1 + 1/nu)*gamma(1/nu))) - Abs(Piecewise(((sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)*(x - mean)/sd + (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))*(xi - 1/xi))/xi, (sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)*(x - mean)/sd + (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))*(xi - 1/xi)) > 0), ((sqrt((1 - (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2)*(xi**2 + xi**(-2)) + 2*(gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))**2 - 1)*(x - mean)/sd + (gamma(2/nu)/sqrt(gamma(1/nu)*gamma(3/nu)))*(xi - 1/xi))*xi, True)))**nu/(2*(sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu)))**nu)"
limits:
  - {firmness: numerical, name: unresolved constrained uniform endpoint rates, fixed: [sd], when: "The shape $\\nu$ tends to infinity at fixed standard deviation, with a limiting uniform endpoint on an observation.", detect: "The constrained uniform width equals the sample range and its numerically compared endpoint face would win; its approach rate is unresolved.", status: ScaleBoundary, numerical: true}
  - {firmness: numerical, name: unresolved constrained uniform endpoint rates, fixed: [sd, xi], when: "The shape $\\nu$ tends to infinity at fixed standard deviation, with a limiting uniform endpoint on an observation.", detect: "The constrained uniform width equals the sample range and its numerically compared endpoint face would win; its approach rate is unresolved.", status: ScaleBoundary, numerical: true}
  - {firmness: proved, model: unif, fixed: [mean], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: UniformBoundary, numerical: true}
  - {firmness: proved, name: right translated half-family, fixed: [mean], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfGedBoundary, numerical: true}
  - {firmness: proved, name: left translated half-family, fixed: [mean], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfGedBoundary, numerical: true}
  - {firmness: proved, name: constant sample scale collapse, fixed: [mean], when: "The standard deviation $\\sigma$ tends to zero at the free or held mean.", detect: "Every observation equals the free or held mean and standard deviation is free.", status: Unbounded}
  - {firmness: proved, model: unif, fixed: [sd], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: UniformBoundary, numerical: true}
  - {firmness: proved, name: right translated half-family, fixed: [sd], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfGedBoundary, numerical: true}
  - {firmness: proved, name: left translated half-family, fixed: [sd], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfGedBoundary, numerical: true}
  - {firmness: proved, name: right translated half-family, fixed: [nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfGedBoundary, numerical: true}
  - {firmness: proved, name: left translated half-family, fixed: [nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfGedBoundary, numerical: true}
  - {firmness: proved, name: constant sample scale collapse, fixed: [nu], when: "The standard deviation $\\sigma$ tends to zero at the free or held mean.", detect: "Every observation equals the free or held mean and standard deviation is free.", status: Unbounded}
  - {firmness: proved, model: unif, fixed: [xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: UniformBoundary, numerical: true}
  - {firmness: proved, name: constant sample scale collapse, fixed: [xi], when: "The standard deviation $\\sigma$ tends to zero at the free or held mean.", detect: "Every observation equals the free or held mean and standard deviation is free.", status: Unbounded}
  - {firmness: proved, model: unif, fixed: [mean, sd], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: UniformBoundary, numerical: true}
  - {firmness: proved, name: right translated half-family, fixed: [mean, sd], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfGedBoundary, numerical: true}
  - {firmness: proved, name: left translated half-family, fixed: [mean, sd], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfGedBoundary, numerical: true}
  - {firmness: proved, name: right translated half-family, fixed: [mean, nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfGedBoundary, numerical: true}
  - {firmness: proved, name: left translated half-family, fixed: [mean, nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfGedBoundary, numerical: true}
  - {firmness: proved, name: constant sample scale collapse, fixed: [mean, nu], when: "The standard deviation $\\sigma$ tends to zero at the free or held mean.", detect: "Every observation equals the free or held mean and standard deviation is free.", status: Unbounded}
  - {firmness: proved, model: unif, fixed: [mean, xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: UniformBoundary, numerical: true}
  - {firmness: proved, name: constant sample scale collapse, fixed: [mean, xi], when: "The standard deviation $\\sigma$ tends to zero at the free or held mean.", detect: "Every observation equals the free or held mean and standard deviation is free.", status: Unbounded}
  - {firmness: proved, name: right translated half-family, fixed: [sd, nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfGedBoundary, numerical: true}
  - {firmness: proved, name: left translated half-family, fixed: [sd, nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfGedBoundary, numerical: true}
  - {firmness: proved, model: unif, fixed: [sd, xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: UniformBoundary, numerical: true}
  - {firmness: proved, name: constant sample scale collapse, fixed: [nu, xi], when: "The standard deviation $\\sigma$ tends to zero at the free or held mean.", detect: "Every observation equals the free or held mean and standard deviation is free.", status: Unbounded}
  - {firmness: proved, name: right translated half-family, fixed: [mean, sd, nu], when: "The skew $\\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: RightHalfGedBoundary, numerical: true}
  - {firmness: proved, name: left translated half-family, fixed: [mean, sd, nu], when: "The skew $\\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation.", detect: "The numerically compared half-family face wins among retained candidates while satisfying held constraints.", status: LeftHalfGedBoundary, numerical: true}
  - {firmness: proved, model: unif, fixed: [mean, sd, xi], when: "The shape $\\nu$ tends to infinity while any held mean and standard deviation are retained.", detect: "The numerically compared limiting face wins among retained candidates while satisfying held constraints.", status: UniformBoundary, numerical: true}
  - {firmness: proved, name: scale collapse, fixed: [mean, nu, xi], when: "The standard deviation $\\sigma$ tends to zero at the held mean.", detect: "Every observation equals the free or held mean and standard deviation is free.", status: Unbounded, message: "Scale collapse"}
  - model: unif
    firmness: proved
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: The numerically compared uniform log likelihood exceeds all retained finite candidates; this comparison does not certify the global supremum.
    status: UniformBoundary
    numerical: true
  - name: right translated half-GED
    firmness: proved
    when: 'The skew $\xi$ tends to infinity with finite endpoint and right-side scale.'
    detect: The numerically fitted right translated half-GED face beats the retained finite candidates.
    status: RightHalfGedBoundary
    numerical: true
  - name: left translated half-GED
    firmness: proved
    when: 'The skew $\xi$ tends to zero with finite endpoint and left-side scale.'
    detect: The numerically fitted left translated half-GED face beats the retained finite candidates.
    status: LeftHalfGedBoundary
    numerical: true
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
  certified: 'partial'
  note: 'sged/certificate.hpp certifies free location with both shapes held (scale held or free), and free scale with mean and both positive shapes held. sged/skew_certificate.hpp additionally certifies free skew at held nu=2 with either moment held or free, and at 0<nu<=1 with both moments free. With xi=1 held, sged/free_shape_certificate.hpp delegates all four free-nu subsets to the GED global certificate in identical public moment coordinates. Balanced two-point data with free moments have a certified uniform supremum for every held or free skew. sged/weighted_shape_certificate.hpp certifies asymmetric held xi with nu and both moments free. sged/mode_skew_certificate.hpp completes fixed positive nu with free skew and both moments free. sged/held_moment_certificate.hpp covers fixed positive nu other than 2 with both moments held by compact skew subdivision; finite winners require a full coefficient enclosure, strict-support half-GED faces are compared globally, and unresolved endpoint contacts or arithmetic stop numerically. Free nu with a held moment at asymmetric skew, free nu and xi jointly, and nonnormal free-skew fits with exactly one moment held retain uncertified numerical searches; the unchanged default is among these open cases.'
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
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, estimated shape is restricted to $\nu\ge1$; constant data permit scale collapse, and uniform or translated half-GED limits can prevent a finite maximum. Without the fitting restriction, the all-free likelihood is unbounded for every sample. Global existence and uniqueness in the restricted domain outside the stated cases are unresolved. Holding coefficients retains only reachable limiting faces and the corresponding moment constraints. With both shapes held and location free, the location/scale maximum is unique for nu > 1 unless free scale collapses on constant data; nu <= 1 permits ties. These subsets now use a global certificate. With $\mu$ and both shapes held, a scale maximum exists unless every observation equals that location; the scale maximum is unique for nu >= 1 by strict convexity in inverse scale, and for every positive nu when xi=1; the asymmetric scale search below one uses a compact-domain certificate and can have separated ties. With $\sigma$ and both shapes held, a location maximum exists, including for tied samples. Holding all coefficients leaves likelihood evaluation.
---

The skew generalized error distribution extends the exponential-power family with Fernandez-Steel asymmetry. Its location and scale parameters are the mean and standard deviation.

## Details

The coefficients $\mu$ and $\sigma$ are the mean and standard deviation. Shape $\nu$ controls exponential-power tail thickness, and $\xi$ controls asymmetry. At $\xi=1$ the family reduces to the symmetric generalized error distribution; values above one lengthen the right-hand side, and reciprocal values reflect the standardized law. Holding moments constrains the join point and side scales as shapes vary.

### Density limits

Let $g_\nu(y)=\nu\exp(-|y|^\nu)/(2\Gamma(1/\nu))$ and
$m_\nu=\Gamma(2/\nu)/\Gamma(1/\nu)$,
$v_\nu=\Gamma(3/\nu)/\Gamma(1/\nu)$. With mode $c$, ordinary
kernel scale $a$ and skew $r=\xi$, the two-piece density is
$2g_\nu((x-c)/(ar))/(a(r+r^{-1}))$ on the right and uses
$(x-c)r/a$ on the left. Its mean is $c+a m_\nu(r-r^{-1})$
and its variance is
$a^2\{v_\nu(r^2+r^{-2}-1)-m_\nu^2(r-r^{-1})^2\}$, by integration.
These equations enforce any held mean and standard deviation along each path.

For fixed finite $r$, $g_\nu\to\mathbf1_{(-1,1)}/2$ almost everywhere,
$m_\nu\to1/2$ and $v_\nu\to1/3$ as $\nu\to\infty$ by
$z\Gamma(z)\to1$. The two pieces therefore join to a uniform density
on $(c-a/r,c+ar)$, whose mean and standard deviation agree with the limiting moment
formulas. For $r\to\infty$ with $ar\to b>0$ and $c\to c_0$,
the left probability $1/(1+r^2)$ vanishes and the density on the right
converges to $2g_\nu((x-c_0)/b)/b$. Its mean and standard deviation are
$c_0+bm_\nu$ and $b\sqrt{v_\nu-m_\nu^2}$; reflection gives the
left face. These limiting moment equations also cover the stated held
subsets. Scheffé's lemma gives total-variation convergence in each case.
Endpoint density values and coupled endpoint rates require separate
analysis for likelihoods; these density proofs do not certify the
numerical face comparison.

The [conditional skew analysis](../../../../../dev/research/evaluations/2026-10-29-p2b-skew-report.md) derives the collapse exponents and face constraints; its equality cases and global comparisons remain uncertified.

Fitting restricts the shape to $\nu \ge 1$, the log-concave GED range,
including the attained Laplace member at $\nu=1$. Without this restriction
the likelihood is unbounded as $\nu \to 0$ for nonconstant samples
(Tumlinson, 2015). Estimated shapes below one are not fitted.
Density, probability, quantile and random generation functions
retain the full $\nu>0$ domain. Coefficients follow fGarch's mean and
standard deviation parameterisation.

The fitter compares several interior starts with the exact Laplace fit
and the uniform limit. Total log-likelihood differences of at most
$10^{-8}n$ are ties, with an attained member preferred to a limit.
A winning uniform limit is reported rather than returned as a finite shape estimate.

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
is scanned and refined in log shape from zero to 20; the infinite-shape
endpoint is the uniform. If the supremum lies on a half face, it is not attained in the finite skewed
family. A numerically winning half face is reported explicitly; the search
does not prove that no larger finite maximum was missed.


Every coefficient may be held, singly or jointly; held values are returned
exactly. Other conditional fits compare multistart interior searches with every
reachable half-family and shape boundary, retaining the held mean and standard deviation
constraints on those faces. Fits with either shape free
scan before refining competing brackets. Numerical multistart and
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
| Held shapes, free location; scale-only with positive held shapes | Global certificates described below. |
| Held skew, free shape and both moments; held positive shape, free skew and both moments | Global weighted-shape and mode-profile certificates below. |
| Held positive shape, both moments held, free skew | Compact skew cover; enclosed finite winner, strict-support half-GED face, tie, or explicit numerical/budget stop. |
| Remaining held-moment and joint free-shape/skew fits | Numerical scans and refinements; no global certificate. |
| Default free shapes and limiting faces | Multistart and face comparisons; numerical, with no global shape certificate. |

With mean and shapes held, scale maxima exist unless all observations equal
the held mean, when scale collapse makes the likelihood unbounded. Held
positive shapes below one are accepted for these conditional fits; the
$\nu\ge1$ restriction applies to estimated shape. With scale and both shapes held,
location maxima exist even with ties.

When both mean and standard deviation are held, data exactly at a limiting uniform
endpoint retain the GED endpoint density penalty. If only the standard deviation is held
and its limiting uniform width equals the sample range exactly, a winning
endpoint face is reported as uncertified rather than returning a uniform
proxy for its constrained likelihood.

### Held-shape certificate

Write $c=\mu-\sigma\mu_u/\sigma_u$ for the mode and
$h=\sigma\lambda 2^{1/\nu}/\sigma_u$ for ordinary scale, where
$\lambda$ is the variance-standardized GED scale, and $\mu_u,\sigma_u$
are its Fernandez–Steel shift and scale. For fixed $\nu,\xi$,
$$
 Q(c)=\sum_{x_i<c}\xi^\nu(c-x_i)^\nu+
       \sum_{x_i>c}\xi^{-\nu}(x_i-c)^\nu,\qquad
 \ell=C-n\log h-Q(c)/h^\nu.
$$
With scale held, maximizing likelihood minimizes $Q$. With scale free,
$h^\nu=\nu Q(c)/n$ is the unique scale maximum whenever $Q>0$, and
profiling again minimizes $Q$. The public mean is recovered from the mode
only after solving this problem; holding the mean instead does not fix the mode
when $\xi\ne1$.

For $\nu>1$, $Q$ is strictly convex, has derivative negative below the
sample minimum and positive above the maximum, and has one global minimum.
The certificate bisects that derivative on the whole sample range, evaluating
compensated signed sums after logarithmic rescaling. An uncertain derivative sign must be
replaced by two signed endpoints enclosing the root; it is never treated as a
zero score. The normalized root enclosure has width at most $10^{-9}$.
The stored mean must recover the normalized mode within another $10^{-9}$;
otherwise the fitter stops. Cancellation in mean recovery is also rejected
when a $32\epsilon$ allowance on its summands exceeds $10^{-7}(1+|\mu|)$.
The likelihood evaluated at the stored coefficients must agree with the
mode-coordinate evaluation within the candidate allowance below; this guards
against cusp amplification of reconstruction errors. This supplies an absolute coefficient enclosure
in units of the largest observation distance from the sample midpoint,
rather than a stopping test based only on likelihood decrease. Termwise
minimum/maximum distances over the root interval also enclose the profiled
scale and recovered mean; both public-coordinate widths must be at most
$10^{-7}(1+|\theta|)$ before acceptance.

For $\nu=1$, the minimizers are weighted medians with quantile
$1/(1+\xi^2)$. Sorted observations and cumulative left/right slopes locate
one knot or a flat interval. For $0<\nu<1$, $Q$ is strictly concave on
every interval between distinct observations. No such interior point can
minimize it, and outside the sample range it is monotone towards the data.
Evaluating every distinct observation therefore enumerates all possible
global minima. The latter result does **not** assert uniqueness. For example,
$x=(-1,1)$ with $\xi=1$ has two maximizing modes for every held
$0<\nu<1$. At $\nu=1$ the entire interval $[-1,1]$ maximizes likelihood.
These examples apply with scale held or profiled; ties stop with candidate
coefficients rather than returning one arbitrary fit.

Constant data with free location and scale have $Q=0$ and unbounded
likelihood as scale vanishes. With positive scale held they have a unique
maximizing mode at that observation. With mean and $\xi=1$ held, the
same scale profile gives a unique maximum unless all observations equal
the held mean. These statements cover all positive held shapes, independently
of the restriction on estimated shape.

With mean and both shapes held and $\nu\ge1$, set
$t=u/\sigma>0$, with $u=\max_i|x_i-\mu|>0$. The negative log likelihood
is, up to constants, $F(t)=-n\log t+\sum_i V(a_i t+\mu_u)$,
where $a_i=(x_i-\mu)\sigma_u/u$ and
$V(z)=|z/(\lambda\xi^{\operatorname{sign}z})|^\nu/2$.
Each $V$ is convex, and $-n\log t$ is strictly convex. Thus $F$ has
one minimum: it diverges at zero, and at infinity a nonzero $a_i$ gives
power growth dominating the logarithm. The scale certificate brackets the
monotone derivative $F'(t)$ with expanding log-$t$ endpoints and then
bisects, using compensated signed logarithmic sums for $tF'(t)$. A derivative
uncertainty, including a $\nu=1$ join, must be resolved by two signed
endpoints or it stops numerically. A log-scale bracket of width $10^{-9}$
is required, and the stored scale must preserve its log coordinate within
$10^{-8}$. Tail bracketing and bisection share the solver budget; the
certificate imposes no finite inverse-scale cutoff. When $u=0$, collapse
is unbounded. For asymmetric held shapes below one, the separate certificate below replaces
the convexity argument.

For $0<\nu<1$, $\xi\ne1$ and held mean, put
$u=\max|x_i-\mu|$, $d=\mu_u\ne0$,
$r=u\sigma_u/(|d|\sigma)$, $b_i=\operatorname{sign}(d)(x_i-\mu)/u$
and $k=|d/\lambda|^\nu/2$. Up to the known additive constant,
$$
 F(r)=-n\log r+P(r),\qquad
 P(r)=\sum_i k\xi^{-\nu\operatorname{sign}(d(1+b_i r))}|1+b_i r|^\nu.
$$
Positive knots $r=-1/b_i$ partition its sign cells. Within each open cell,
$P$ is concave and $P'$ decreases. The certificate evaluates all knots and
bounds every intervening cell; it assumes neither unimodality nor uniqueness.
An incumbent cost $B$ excludes $\log r<-B/n-1$, since $P\ge0$.
For $r\ge2$, some $|b_i|=1$, so
$F(r)\ge C r^\nu-n\log r$, with
$C=k\exp(-|\nu\log\xi|)/2^\nu>0$.
This envelope increases once $C\nu r^\nu>n$ and diverges. Expanding
its log endpoint until its lower bound exceeds $B$ closes the upper tail.
Thus every global minimum lies in a finite, computed interval, and exists
unless $u=0$, the unbounded collapse case.

On a cell $[l,h]$, the chord of $P$ is a lower bound. Its sum with
$-n\log r$ is minimized explicitly at $r=n/\text{slope}$, clamped to
the cell (at $h$ if the slope is nonpositive). A cell is discarded if
this bound exceeds the incumbent, or if
$P'(h)-n/l>0$ or $P'(l)-n/h<0$ certifies monotonicity; its endpoints
have already been evaluated. Remaining cells are bisected geometrically.
Terminal cells have log width at most $10^{-10}$. Retained knots and
cells must together enclose all possible winners within a public scale
width $10^{-7}(1+|\sigma|)$ and log-scale width $10^{-7}$. Separated surviving regions stop as a
comparison tie, retaining finite witnesses. A finite global likelihood upper
bound is reported after every cell and both tails have been closed; before
that, budget and numerical stops retain an infinite upper bound.

Uniqueness can fail here. Take any repeated sample $x_i=-1$,
held mean zero and $\nu=1/2$. Let $R>1$ solve
$\log R=2(1-1/R)$, set $K=2\sqrt{R-1}/R$ and
$\xi=\sqrt{1+K^2/6}$. On $r>1$ the cost relative to the cusp is
$n[-\log r+K\sqrt{r-1}]$. Its stationary equation is quadratic in
$\sqrt{r-1}$: the first root is a maximum, the second is the minimum
at $R\simeq4.92155$, tied with the cusp $r=1$. On $0<r<1$ the cost
strictly decreases towards the cusp. These are two separated global scale
maxima, despite every observation differing from the held mean.

The subunit certificate uses compensated sums, fused residual evaluation and
a $4096\epsilon(1+n+|F|)(1+|\nu\log\xi|+1/\nu)$ cost allowance;
derivative comparisons use the same conditioning factor times their absolute
term sum. Algebraic knot residuals are set to zero. Recovery tries three algebraically equivalent scale expressions at a cusp and
their adjacent representable values. It is accepted only
if the density at the stored public coefficients agrees with its ideal value
within $10^{-7}(n+|\ell|)$. Nonfinite arithmetic, lost distinct normalized
observations or knots, and underflowed scales stop numerically. These checks address practical cancellation and cusp
sensitivity; they are not directed-rounding bounds. Tail expansions, point
and cell evaluations share $128\min(\mathtt{iterlim},100000)$ work units.
A partition whose endpoint evaluations cannot fit the remaining budget stops
before evaluating its knots, retaining the non-cusp initial witness. This
avoids quadratic work before a predictable large-sample budget stop.
Each evaluation is $O(n)$; evaluating $k$ distinct knots costs $O(nk)$,
knot sorting is $O(n\log n)$, and retained
search storage is bounded by the certificate budget. Raising `iterlim`
raises the work cap without introducing a fixed scale cutoff.

The certificate is exact-arithmetic conditional on computed comparisons.
Its practical floating-point allowances are $512\epsilon$ times the
rescaled score magnitude and its exponent conditioning factor, and
$10^{-10}(n+|\ell|)$ per evaluated candidate ($10^{-7}(n+|\ell|)$ for
$\nu<1$, allowing for the cusp's sensitivity to mode reconstruction).
Overlapping likelihood allowances or an uncertain weighted-median slope
produce a comparison-tie stop. These are numerical allowances, not
outward-rounded interval bounds. Nonfinite normalizers, powers, scales or
failed coordinate reconstruction produce a numerical stop. The settled
log-gamma moment helper used by d/p/q/r supplies the certificate's shift
and scale; the historical all-free objective is unchanged.

The certificate uses these fixed coefficient enclosures independently of
`gradtol`, which continues to control the legacy numerical optimizers.
Bisection uses at most $\min(\mathtt{iterlim},100000)$ solver steps;
exhaustion is a numerical stop. Location bisection retains its finite
witness when available; scale bracketing can stop before evaluating a
candidate. Knot enumeration
uses at most $128\min(\mathtt{iterlim},100000)$ distinct knots and stops
as `budget` with surviving candidates if exhausted. Each knot evaluation
costs $O(n)$; sorting costs $O(n\log n)$, the weighted-median path costs
$O(n)$ after sorting, and the smooth solve costs $O(n)$ per iteration.
No finite shape or skew cutoff is part of these held-shape proofs.

The remaining free subsets are described below. Fixed-skew convexity alone
does not control a free shape or free skew profile, close its infinite
tails, or settle the moment-constrained uniform endpoint rates.
See the [certification evidence and open cases](../../../../../dev/research/evaluations/2026-11-06-sged-cert-report.md).

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.

The fGarch mean and standard deviation convention is used. Its `gedFit` permits positive shapes below one and returns local solutions. A winning uniform limit here stops with advice to use `mlunif`.

## Decision history

The 2026-10-31 fit-speed revision computes the fixed shape/skew density
normalizers and kernel scale once in the conditional location/scale search. Each
density contribution retains the previous arithmetic grouping and observation
order. Search grids, observation-kink refinements and failure handling are
unchanged. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

The finite scan is not used when the public mean and both shapes are held;
that fit uses the subunit certificate above. Its bounds and the analytic
nonuniqueness example are checked in the [implementation and validation
evidence](../../../../../dev/research/evaluations/2026-11-07-sged-subunit-report.md).

## Fixed-shape free-skew certificates

At held $\nu=2$, the public mean, standard deviation and skew coordinates
are exactly those of the two-piece normal. Its public fitter dispatches
to the joint polynomial certificate when both moments are free, and the
conditional certificate otherwise. Its
half-normal scales are multiplied by $\sqrt{2}$ to express the same density
as a half-GED with kernel $\exp[-(x/b)^2]$. Candidate coordinates, face
models, ties, budgets and arithmetic stops are translated together. If the
converted half-GED scale overflows or loses more than $10^{-10}$ in its
logarithmic conversion, delegation stops numerically rather than retaining
an unrepresentable or inaccurate face coefficient.

For $0<\nu\le1$ with both moments free, write the density as
$\nu\exp[-|(x-c)/s|^\nu]/[\Gamma(1/\nu)(a+b)]$, with
$s=a$ left of the join $c$ and $s=b$ right of it. Put
$A=\sum_{x_i<c}(c-x_i)^\nu$, $B=\sum_{x_i>c}(x_i-c)^\nu$,
$r=1/(1+\nu)$ and $S=A^r+B^r$. Profiling gives
$a=tA^r$, $b=tB^r$, $t=(\nu S/n)^{1/\nu}$ and
$\ell=n[\log\nu-\log\Gamma(1/\nu)-\log(tS)-1/\nu]$.
On every interval between observations, $A$ and $B$ are nonnegative concave
functions. Composition with the increasing concave power $r$ shows that
$S$ is concave. Its minimum is therefore at a sample knot. A join outside
the sample range can be moved towards the nearest observation, reducing
every residual at fixed side scales. Thus all finite maximizers and both
half-GED faces are covered by enumerating the distinct knots. The face
scale is $(\nu\operatorname{mean}|x-c|^\nu)^{1/\nu}$.

The public moment map uses $k=\Gamma(2/\nu)/\Gamma(1/\nu)$ and
$v=\Gamma(3/\nu)/\Gamma(1/\nu)$:
$\mu=c+k(b-a)$,
$\sigma^2=v(a^2-ab+b^2)-k^2(b-a)^2$, $\xi=\sqrt{b/a}$.
The power sums use compensated accumulation. Comparisons allow $4096\epsilon(1+n+|\ell|)(1+1/\nu)$ per candidate.
Stored coefficients must reconstruct the knot within $10^{-10}(1+|c|)$
and agree with the public density within $10^{-7}(n+|\ell|)$. Candidate
bounds include the discrepancy between ideal and stored-coordinate
likelihoods in addition to the comparison allowance. Recovered finite and face
scales must agree with their logarithmic ideal values within $10^{-10}$;
face likelihoods are also reevaluated at the stored coefficients. Nonfinite
moments, underflow of a positive side scale, merged normalized knots or failed
reconstruction stop numerically. A $256\epsilon$ absolute-term allowance
also checks cancellation in the recovered public mean against its
$10^{-7}(1+|\mu|)$ coefficient margin.
These are exact-arithmetic certificates conditional on computed comparisons
with practical floating-point margins, not outward-rounded intervals.
Enumeration costs $O(nk)$ for $k$ distinct knots and is limited to
$128\min(\mathtt{iterlim},100000)$ evaluations. A partition with more
knots than the remaining budget stops after one linear-cost face witness,
before incurring predictable quadratic work. Overlapping winning
likelihood bounds stop as a tie; for the two-point sample $(-1,1)$ the
reflected half-GED faces tie, disproving general closure uniqueness.
Finite uniqueness also fails: at $\nu=1/2$, the symmetric sample consisting
of $-100$, five copies each of $-1$ and $1$, and $100$ has two winning
interior knots $c=\pm1$. At either interior knot $S<12$; at either
endpoint $S>20$. These inequalities separate the only four distinct
knots, and reflection gives the exact tie.

For held shapes other than two with exactly one held moment, the existing numerical
searches remain uncertified. With both moments free, the certificate below
completes the smooth fixed-shape cases.
In particular their finite skew scan is not a bound on either infinite
tail. These open regions and the missing bounds are recorded in the
[free-skew evidence](../../../../../dev/research/evaluations/2026-11-07-sged-skew-report.md).


## Free-shape symmetric submodel

Holding $\xi=1$ gives $\mu_u=0$, $\sigma_u=1$, and equal side
scales for every $\nu$. The density and its public moment coordinates
are exactly GED's, including when either moment is held. All four free-shape
subsets therefore use the public GED certificate at the same controls and
restriction $\nu\ge1$. Its profile-score bounds, location enclosures,
curvature contraction and uniform-tail bound supply the global comparison;
no SGED shape scan participates in these fits. The same conditional-comparison
certificate and practical floating-point margins apply, including GED's
coefficient enclosures. This reduction applies only to skew held at exactly
one, never to an estimated skew near one.

Finite witnesses append the held skew to GED's mean, sd and shape. Uniform
witnesses retain their two endpoint coordinates, and every likelihood bound,
tie, work count, budget and numerical stop is preserved. A finite result is
accepted only if GED certifies it. In particular rounded held-sd endpoint
comparisons remain numerical stops. Free-shape uniqueness is false: at
$x=(-1,1)$, held $\sigma=0.1$ and $\xi=1$, the entire Laplace
median interval maximizes likelihood; the fitter retains its two endpoints
in a tie stop.

For asymmetric held skew with a held public moment, a free shape changes
the mode-to-mean and scale-to-sd conversions. Those holds cannot be replaced
by held kernel coordinates. With both shapes free the uniform face also
intersects both half-GED faces. These held-moment and joint-shape subsets
retain the existing uncertified numerical searches, including the default fit. The [free-shape evidence and exact open
scope](../../../../../dev/research/evaluations/2026-11-08-sged-free-report.md)
give the constrained objective, tail reductions and missing bounds.


With both moments free and equal numbers of observations at two distinct
values $l<h$, a global face comparison is analytic even for asymmetric held
or free skew. Log-concavity for $\nu\ge1$ gives
$1>\int_l^h f(x)\,dx\ge(h-l)\sqrt{f(l)f(h)}$ for every
finite SGED. Hence $\ell<-n\log(h-l)$. This bound is reachable:
choose an interior join $c$ with $(h-c)/(c-l)=\xi^2$, and side scales
$a=(c-l)(1+\delta)$, $b=(h-c)(1+\delta)$, where
$\delta=\log\nu/\nu$. Both endpoint penalties then vanish and
the normalizer tends to $1/(h-l)$. Free skew may use $\xi=1$.
The fitter reports the uniform face without a shape or skew scan.
This result requires both public moments free; a held moment changes the
reachable paths. The face likelihood uses the half-width to avoid width
overflow, with a $4096\epsilon(1+n+|\ell|)$ comparison allowance;
a subnormal half-width stops numerically.


## Asymmetric held-skew free-shape certificate

With both moments free, write $a=h/\xi$, $b=h\xi$ for the two
kernel side scales. For mode $c$, put $r_i=\xi(c-x_i)$ on the left
and $r_i=(x_i-c)/\xi$ on the right, and $T=\sum r_i^p$, where
$p=\nu\ge1$. Scale profiling gives $h^p=pT/n$. Above one the
mode objective is strictly convex; at one its minimizers are the entire
weighted-median face with quantile $1/(1+\xi^2)$.

Let $U=\sum r_i^p\log r_i$, $V=\sum r_i^p(\log r_i)^2$,
$G=\sum\operatorname{sign}(c-x_i)w_i r_i^{p-1}\log r_i$ and
$H=\sum w_i^2r_i^{p-2}$, with $w_i=\xi$ left and $1/\xi$ right.
The envelope score and its curvature are
$$
 A=p^2\ell'/n=\psi(1+1/p)+\log p-pU/T+\log(T/n),
$$
$$
 A'=1/p-\psi_1(1+1/p)/p^2-p[V/T-(U/T)^2]
       +p^2G^2/[(p-1)TH].
$$
The mode is enclosed over every proposed shape interval using its monotone
weighted score, without assuming a monotone mode path. Shared positive
rescaling of residuals prevents high-shape score underflow. Termwise power,
power-log and power-log-squared ranges enclose these formulas; crossing
knots include both side weights. An unavailable finite Hessian enclosure
disables curvature contraction rather than discarding a possible root.
Definite score signs exclude stationary points, negative upper curvature
contracts roots, and other cells are subdivided or excluded by likelihood.

For $C=\xi+1/\xi$ and sample range $R>0$, at least one weighted
extreme residual is at least $R/C$, whatever the mode. Convexity of log gamma
therefore gives the uniform-in-skew tail bound
$$
 \ell(p)/n\le-\log R-
 [\log p+1-\gamma-\log n]/p.
$$
For $p\ge\max(2,n)$ the signed correction is strictly negative.
The certificate thus covers the entire remaining finite interval, compares
all retained roots and the weighted-median face at one, and compares the
reachable uniform face. It does not select a uniform limit from a finite
shape scan.

## Fixed-shape free-skew mode certificate

For fixed $p>1$, set $A=\sum_{x_i<c}(c-x_i)^p$,
$B=\sum_{x_i>c}(x_i-c)^p$, $r=1/(p+1)$ and $S=A^r+B^r$.
The side-scale profile is $a=tA^r$, $b=tB^r$,
$t=(pS/n)^{1/p}$, and the profile likelihood decreases with $S$.
Every maximizing mode is in the closed sample range. Its endpoints represent
the right and reflected half-GED faces.

Writing $C_L=\sum_{x_i<c}(c-x_i)^{p-1}$ and
$C_R=\sum_{x_i>c}(x_i-c)^{p-1}$, the stationary equation is
$F=C_L/A^{p/(p+1)}-C_R/B^{p/(p+1)}=0$.
Monotone side sums enclose $F$ on every mode cell; differentiating these
sums encloses $F'$. Positive lower curvature permits root contraction.
The likelihood bound $S(c)\ge A(l)^r+B(h)^r$ controls a whole cell
$[l,h]$. Before the first distinct knot the left score term is exactly
$k^r(c-x_{\min})^{-r}$ for its multiplicity $k$; the reflected
formula closes the other face neighbourhood. Thus neither infinite skew
tail is represented by a finite skew cutoff.

All distinct knots are evaluated. Above one an interior knot with definite
nonzero first derivative cannot maximize; stationary or uncertain knots are
retained along with every surviving root enclosure. Both half-GED faces
are compared after the complete cover closes. No unimodality is assumed.
For the two-point sample $(-1,1)$, $S(c)=(c+1)^{p/(p+1)}+
(1-c)^{p/(p+1)}$ is strictly concave, so its two endpoint minima tie.
This is an exact nonuniqueness example for every such held shape.

Both certificates recover moments using
$k=\Gamma(2/p)/\Gamma(1/p)$, $v=\Gamma(3/p)/\Gamma(1/p)$,
$\mu=c+k(b-a)$ and $\sigma^2=(v-k^2)(b-a)^2+vab$.
Gamma-ratio enclosures use digamma derivative bounds in $1/p$; these moment
ratios are not assumed monotone. All possible winners must fit in public
mean intervals of width $2\times10^{-7}(1+|\mu|)$ and log-sd and
log-skew intervals of width $2\times10^{-7}$, with shape root width
$2\times10^{-7}(1+p)$ when free. Representatives are refined further.
The stored public coefficients must reproduce the profile likelihood within
the arithmetic allowance; failed recovery stops numerically.
Separated overlapping candidates stop as ties. A weighted finite tie occurs
at held $\xi=2$ for $(-100,-.01,-.01,-.01,15\text{ copies of }.01,100)$:
the winning shape-one weighted-median interval is retained, not one selected
location.

These are exact-arithmetic certificates conditional on computed comparisons.
Likelihood and score margins use $4096\epsilon(1+\text{magnitude})
(1+n\epsilon)(1+p_{\max})$, including normalizer magnitudes.
Positive side-power sums use relative margins with the same factor and an
$n$-subnormal allowance, so a rare side is not erased by an absolute margin
from the opposite side. Positive side sums lost to underflow never become
half-GED faces. This is not outward-rounded interval arithmetic.
Both searches meter point, cell and root evaluations against
$128\min(\mathtt{iterlim},100000)$, with inner solves capped at
$\min(\mathtt{iterlim},100000)$. Knots cost $O(nk)$ for $k$ distinct
values; an unfundable knot partition stops after a linear-cost face witness.
Budget and numerical stops retain feasible witnesses and an infinite upper
bound until the full cover is closed. The [free-shape report](../../../../../dev/research/evaluations/2026-11-08-sged-free-report.md)
records independent bounds, coefficient checks, timings and remaining holds.


## Fixed shape with both moments held

Writing $z=(b-a)/(a+b)$, $k=\Gamma(2/p)/\Gamma(1/p)$,
$v=\Gamma(3/p)/\Gamma(1/p)$ and $D=3v-4k^2$, the held moments force
$s=a+b=2\sigma/\sqrt{v+Dz^2}$ and $c=\mu-kzs$.
For $d_i=(x_i-\mu)/\sigma$ put $q_i=d_i\sqrt{v+Dz^2}/2+kz$.
The constrained likelihood on $-1<z<1$ is
$$
\ell=n[\log p-\log\Gamma(1/p)-\log(2\sigma)
       +\tfrac12\log(v+Dz^2)]
-\sum_i\left(\frac{2|q_i|}{1+\operatorname{sign}(q_i)z}\right)^p.
$$
Interval residual lower bounds give a likelihood upper bound even on cells
that touch either infinite-skew endpoint. On smooth interior cells, interval
first derivatives exclude stationary maxima. Cells crossing a join retain
both signs and receive no derivative exclusion. No finite skew cutoff or
uniqueness assumption is used. For a finite result, all surviving cells must fit
in a log-skew interval of width at most $\min(\mathtt{tol},2\times10^{-7})$; the returned representative
is reevaluated in the public coordinates and retains the exact held values.
If a connected union is wider than the coefficient target, its surviving cells
are refined again; individually narrow cells never suffice by themselves.


At $z=\mathrm{side}\in\{-1,1\}$, the held moments determine the
half-GED scale $b=\sigma/\sqrt{v-k^2}$ and endpoint
$c=\mu-\mathrm{side}\,kb$. When every observation is strictly on the
supported side beyond a $128\epsilon(1+p+1/p)$ componentwise margin, the profile and
score extend continuously to that endpoint;
all denominators in the occupied-side penalty stay positive. A definite
outward score therefore excludes an entire endpoint cell in favor of its
recovered face. Both faces and every interior region are compared globally.
The stored face must preserve the held moments within the coefficient margin
and reproduce its ideal likelihood within the comparison allowance.
A winning face carries its endpoint, kernel scale, shape and finite global
supremum bounds. Exact contacts receive no endpoint substitution: their
vanishing-side penalties remain an unresolved numerical comparison.

This is an exact-arithmetic certificate conditional on computed comparisons,
with a practical $4096\epsilon(1+\text{magnitude})(1+n\epsilon)
(1+p+1/p)$ allowance for score and likelihood comparisons. Residuals use $64\epsilon$
times the sum of their absolute component magnitudes plus one, with the same
$1+p+1/p$ conditioning factor.
It does not use directed rounding. An unresolved endpoint contact, lost moment representation or failed public likelihood
reconstruction returns a numerical stop with an infinite unresolved upper bound,
retaining feasible witnesses when available. After a closed cover, separated possible winners return
a tie with their witnesses and a finite global upper bound. Work exhaustion returns a budget stop; point and cell evaluations
share $128\min(\mathtt{iterlim},100000)$ units. Strict-support half-GED faces extend the same profile to its endpoints and are
compared against all retained interior regions after the cover closes.
The [independent checker](../../../../../dev/research/evaluations/2026-11-08-sged-held-data/check_math.py)
compares the density, smooth scores and selected coefficient references at 70 digits.


Finite uniqueness fails even with both moments held. For $p=1$, $\mu=0$,
$\sigma=1$ and observations $(-d,d)$ with $1<d<5/(3\sqrt2)$, the
likelihood is even in $z$ and has precisely two maximizing skew values.
With $t=z^2$, its derivative vanishes when
$$
d=\frac{t^2+2t+5}{(t+3)\sqrt{2(1+t)}}.
$$
The right side decreases strictly from $5/(3\sqrt2)$ to $1$ on $(0,1)$:
its derivative has the sign of $(t-1)(t^2+10t+13)$.
The unique root in $t$ gives two reciprocal skews; $z=0$ is a strict local
minimum and both skew tails have likelihood tending to negative infinity.
The tests use $d=1.1$ and retain both finite witnesses.
