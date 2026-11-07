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
  - {firmness: numerical, name: uncertified scale boundary, fixed: [mean, nu, xi], when: "The conditional scale search approaches its numerical tail guard.", detect: "The numerical scale search cannot certify a finite interior answer.", status: ScaleBoundary, message: "Scale-collapse boundary", numerical: true}
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
  note: 'sged/certificate.hpp certifies free location with both shapes held (scale held or free), and free scale with mean held when xi=1 or nu>=1 is held. Other subsets, including the unchanged default, retain numerical searches without a global certificate; all-held evaluation is not counted.'
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
    With all coefficients free, estimated shape is restricted to $\nu\ge1$; constant data permit scale collapse, and uniform or translated half-GED limits can prevent a finite maximum. Without the fitting restriction, the all-free likelihood is unbounded for every sample. Global existence and uniqueness in the restricted domain outside the stated cases are unresolved. Holding coefficients retains only reachable limiting faces and the corresponding moment constraints. With both shapes held and location free, the location/scale maximum is unique for nu > 1 unless free scale collapses on constant data; nu <= 1 permits ties. These subsets now use a global certificate. With $\mu$ and both shapes held, a scale maximum exists unless every observation equals that location; the scale maximum is unique for nu >= 1 by strict convexity in inverse scale, and for every positive nu when xi=1; the asymmetric scale search below one remains uncertified. With $\sigma$ and both shapes held, a location maximum exists, including for tied samples. Holding all coefficients leaves likelihood evaluation.
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
constraints on those faces. Fits with either shape free, and the asymmetric scale-only fit,
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
| Held shapes, free location; scale-only with $\nu\ge1$ or $\xi=1$ | Global certificates described below. |
| Remaining conditional fits | Numerical scans and refinements; no global certificate. |
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
is unbounded. For asymmetric held shapes below one, this convexity argument
is unavailable and the legacy scale-only search is retained.

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

The other free subsets remain uncertified, including asymmetric scale-only
fits with held $0<\nu<1$. In particular, fixed-shape
convexity does not control a free shape or free skew profile, close its
infinite tails, or settle the moment-constrained uniform endpoint rates.
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
