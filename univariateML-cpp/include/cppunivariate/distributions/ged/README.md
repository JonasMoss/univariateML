---
id: ged
inference_numeric:
  - regime: "cusp moving with smooth exponent"
    available: true
    condition: "finite mean, sd > 0 and nu > 2; any regular subset free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "held location"
    available: true
    condition: "finite mean held and sd > 0, nu > 0; if nu is free require nu > 1"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "normal member with shape held"
    available: true
    condition: "finite mean and sd > 0, nu = 2 held; mean and/or sd free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "moving location with smooth or held shape"
    available: false
    condition: "finite mean free, sd > 0; either nu > 2, or nu > 1 is held; sd may be held or fitted; excluding the preceding available submodels"
    reason: "DQM and full-rank information are established, but support/join-aware numerical derivatives and fitted covariance validation for this expanded scope are not yet implemented"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "joint fitted cusp Hessian open"
    available: false
    condition: "finite mean free, sd > 0 and 1 < nu <= 2 with nu free"
    reason: "the model is DQM with full-rank information and an integrable correct a.e. Hessian at the truth; consistency of the joint fitted Hessian and cusp-crossing numerical derivatives remains unvalidated"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "regular location with unsuitable ordinary Hessian"
    available: false
    condition: "finite mean free, sd > 0 and 1/2 < nu <= 1 held"
    reason: "the model is DQM with full rank, but the ordinary location Hessian omits the Laplace kink at nu = 1 and is nonintegrable below 1; below 1 the location MLE also makes raw fitted OPG singular"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "critical or fractional cusp location"
    available: false
    condition: "finite mean free, sd > 0 and 0 < nu <= 1/2 held"
    reason: "location uses sqrt(n log n) at nu = 1/2 or an fBm likelihood at rate n^(1/(1+2*nu)) below it; ordinary Hessian inference is invalid and raw critical OPG has the wrong information constant"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
  - regime: "active fitted Laplace shape bound"
    available: false
    condition: "nu = 1 free, finite mean held or fitted, sd > 0"
    reason: "the regular underlying model has a constrained Gaussian shape tangent; moving-location ordinary Hessian inference additionally misses the cusp contribution"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#ged"
name: Generalized Error
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
r:
  density: dged
  sampler: rged
  control:
    nu: Optional held nu; NULL estimates it.
    sd: Optional held sd; NULL estimates it.
    mean: Optional held mean; NULL estimates it.
    gradtol: Positive numerical tolerance; finite free-shape acceptance additionally requires global coefficient enclosures within the frozen-golden accuracy.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; 128 * min(iterlim, 100000) shape-certificate evaluations; held subunit shapes use 100000 * min(iterlim, 100000) enumeration cells."
log_density: -log(sd) - Abs(x-mean)**nu/(2*(sd*sqrt(2**(-2/nu)*gamma(1/nu)/gamma(3/nu)))**nu) + log(nu) - 3*loggamma(1/nu)/2 + loggamma(3/nu)/2 - log(2)
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale tends to zero at the fitted or held location.
    detect: Every observation equals the fitted or held location.
    status: InvalidStart
    message: "unbounded"
  - model: unif
    firmness: proved
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: With free sd, finite-interval subdivision and a signed tail bound exclude every finite shape. With held sd, unresolved uniform comparisons stop numerically.
    status: UniformBoundary
    numerical: true
references:
- Nelson, D. B. (1991). Conditional Heteroscedasticity in Asset Returns, A New Approach. Econometrica, 59, 347–370.
- Tumlinson, S. E. (2015). On the non-existence of maximum likelihood estimates for the extended exponential power
  distribution and its generalizations. Statistics & Probability Letters, 107, 111–114. doi:10.1016/j.spl.2015.08.009.
- 'fGarch documentation (accessed 2026-10-31). fGarch: Rmetrics - Autoregressive Conditional Heteroskedastic Modelling. R package reference
  manual. https://cran.r-project.org/web/packages/fGarch/fGarch.pdf.'
holds: []
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mlged(x)

group: skewed and robust location-scale
construction:
- location-scale
- generalisation
relations:
  special_case_of:
  - family: sged
    at:
      mean: mean
      sd: sd
      nu: nu
      xi: 1
fit:
  method: profile-search
  certified: 'yes'
  note: 'Free shape uses interval profile scores, monotone location enclosures, curvature-based root contraction and global likelihood/tail comparisons for every mean/sd hold subset. Finite successes enclose every potentially winning coefficient to golden accuracy. Separated unresolved maxima, median intervals and observation-knot ties stop with witnesses; exhausted work and unresolved arithmetic stop with budget or numerical details. Held positive shapes use convex or exhaustive observation-knot profiling. No capped numerical proposal is accepted.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model symmetric financial errors with exponential-power rather than Gaussian tails.
summary: Generalized Error provides a model for symmetric financial errors with exponential-power rather than Gaussian
  tails.
aliases:
- "exponential power"
- "symmetric generalized normal"
- "Subbotin"
- "generalized Gaussian"
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, fitting restricts $\nu\ge1$; constant data have unbounded scale likelihood, and nonconstant data may select the unattained uniform limit. Without the fitting restriction, the all-free likelihood is unbounded for every sample. Global uniqueness with free shape in the restricted domain is unknown. For held $\nu>1$, nonconstant data have a unique location-scale maximum; at $\nu=1$, median intervals can give nonunique locations. With shape held, holding $\mu$ gives a unique scale maximum unless all residuals vanish. Holding $\sigma$ removes scale collapse; with shape held above one its location maximum is unique, while shape one permits median intervals. Free shape retains its fitting restriction and conditional uniform comparison. Held positive shapes below one also admit a location maximum at an observation and an explicit scale profile when residual spread is positive, but locations can tie. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    At fixed shape, location minimizes $\sum_i|x_i-\mu|^\nu$, strictly convex for $\nu>1$ and minimized by medians for $\nu=1$. The free scale is an explicit positive multiple of the residual power mean. For nonconstant data this yields a unique location and scale when $\nu>1$. It does not prove uniqueness in shape.
---

The generalized error (exponential power) distribution is a symmetric location-scale family whose shape controls tail thickness.

## Details

The coefficients $\mu$ and $\sigma$ are the mean and standard deviation. Shape $\nu=1$ gives Laplace tails and $\nu=2$ gives the normal distribution. Larger shape gives lighter tails and approaches a uniform distribution. Holding a coefficient preserves its value while the remaining coefficients are estimated.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) describes the limiting paths and the scope of the boundary comparisons.

Fitting restricts the shape to $\nu \ge 1$, the log-concave GED range,
including the attained Laplace member at $\nu=1$. Without this restriction
the likelihood is unbounded as $\nu \to 0$ for nonconstant samples
(Tumlinson, 2015). Estimated shapes below one are excluded; held positive shapes below one are accepted in conditional fits.
Density, probability, quantile and random generation functions
retain the full $\nu>0$ domain. Coefficients follow fGarch's mean and
standard deviation parameterisation.

The fitter profiles location and scale, covers the finite shape range with score enclosures, and closes the entire tail analytically. Total log-likelihood differences unresolved within $10^{-8}n$ stop as comparisons with witnesses. A winning uniform limit is reported with advice to use `mlunif`. An exhausted certificate stops on budget.

With location and scale free, at the Laplace face $\mu$ is a sample median and $\sigma$ is $\sqrt{2}$ times
the mean absolute deviation from that median.


For each held or profiled shape, location minimizes the sum of absolute residuals to that power (a median at shape one, or an observation for held shape below one); the free scale has an exact power-mean profile. A held location collapses only when all residuals vanish. Held standard deviation removes scale collapse. Free shape retains the existing restriction $\nu\ge1$ and is compared with the conditional uniform face. With both location and standard deviation held, observations at a uniform endpoint contribute their limiting GED penalty. Held-sd tails that cannot be separated from the finite candidates stop numerically.

For fixed $\nu\ge1$, the standardized log-density is a constant minus a
positive multiple of $|z|^\nu$, hence is concave. Consequently the
location-scale log-likelihood is concave in $(u,b)=(\mu/\sigma,1/\sigma)$:
it is $n\log b$ plus concave terms evaluated at $bx_i-u$. At $\nu=1$
the kernel has a kink and a median need not be unique; as $\nu\to\infty$
the uniform is a separate limiting face. For $0<\nu<1$ the kernel is
not log-concave. For free shape, the interval score certificate below covers every finite competitor rather than assuming unimodality.

| TASK-116 property | Evidence |
|---|---|
| Fixed $\nu\ge1$ | Concavity in inverse-scale coordinates, proved as above. |
| Conditional location and scale | Median or numerical monotone-score root and analytic scale profile. |
| Finite free shape | Profile-score enclosures, curvature bounds, root coefficient boxes and global comparisons. |
| Free-sd uniform face | Signed tail theorem and finite-interval subdivision exclude all finite shapes. |

With standard deviation held and a limiting uniform endpoint forced onto an observation, each such observation has limiting log-density penalty $e^{-\gamma}$, where $\gamma$ is Euler's constant. If mean is free and both endpoints are forced, with endpoint counts k and l the total penalty is $2e^{-\gamma}\sqrt{kl}$ after profiling the location shift of order $1/\nu$.

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
certificates stop; an optimizer score never rescues an incomplete global cover.

The fGarch mean and standard deviation convention is used. Its `gedFit` permits positive shapes below one and returns local solutions. A winning uniform limit here stops with advice to use `mlunif`.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).


## Free-shape bounds

Write the density as
$[2b\Gamma(1+1/\nu)]^{-1}\exp(-|x-\mu|^\nu/b^\nu)$, where
$b=\sigma\sqrt{\Gamma(1/\nu)/\Gamma(3/\nu)}$.
For $p\ge1$, let $R_p=(n^{-1}\min_\mu\sum_i|x_i-\mu|^p)^{1/p}$,
omitting minimization when mean is held. The power-mean inequality makes
$R_p$ nondecreasing even after minimization. When scale is free,
$\ell(p)/n=h(p)-\log R_p$, with
$h(p)=-\log2-\log\Gamma(1+1/p)-(1+\log p)/p$.
The identity
$\psi(1+t)-\log t=\int_0^\infty e^{-tu}(1/u-1/(e^u-1))\,du>0$
shows that $h$ increases to $-\log2$. Thus an enclosure
$L_p\le\log R_p$ gives upper bounds
$n[h(q)-L_p]$ on $[p,q]$ and $n[-\log2-L_p]$ on $[p,\infty)$.
These bounds allow location and scale to vary throughout each interval.
If $\rho$ is the maximum held-mean residual, or half the sample range with
mean free, $R_p\ge\rho n^{-1/p}$ also gives the explicit uniform-tail
remainder $n\log n/p$ above $-n\log(2\rho)$.
The convex tangent $\log\Gamma(1+t)\ge-\gamma t$ strengthens this to
$\ell(\nu)/n\le-\log(2\rho)
-[\log\nu+1-\gamma-\log n]/\nu$.
Thus every $\nu\ge n\exp(\gamma-1)$ lies below the corresponding uniform
face, including when location is profiled. Unlike the unsigned remainder,
this also closes a uniform-winning tail in exact arithmetic.
The location lower bound uses convex supporting lines and residual-distance
enclosures; a failed or interrupted location solve does not invalidate it.

For held standard deviation put $t=1/\nu$, $r=\sqrt3\sigma$,
$B(t)=[\log\Gamma(1+t)-\log\Gamma(1+3t)]/2$, and
$C(t)=-3\log\Gamma(1+t)/2+\log\Gamma(1+3t)/2$.
Then $\log b=\log r+B(t)$ and the log density at the mode is
$-\log(2r)+C(t)$. The trigamma series proves $B''<0$, so
$B(t)\le\gamma t$; also $C(0)=C'(0)=0$ and
$C''(t)\le(9/2)\zeta(2)$. Uniformly for $\nu\ge p\ge1$,

$$
\ell(\mu,\sigma,\nu)\le
-n\log(2r)+\frac{9n\zeta(2)}{4p^2}
-e^{-\gamma}\sum_i\left(\frac{|x_i-\mu|}{r}\right)^\nu.
$$

When mean is held, residuals exceeding $r$ give a tail penalty bounded
below by their powers at $p$. An exact endpoint contributes $e^{-\gamma}$.
When mean is free, retain the two extreme groups, with multiplicities $k,l$
and half-range $d$. Their minimum penalty is
$(d/r)^\nu F_\nu(k,l)$, where

$$
F_p(k,l)=\min_u[k|1+u|^p+l|1-u|^p],\qquad
\log F_p=\log2+\tfrac12\log(kl)
-(p-1)\log\cosh\frac{\log(k/l)}{2(p-1)}.
$$

Here $F_1=2\min(k,l)$ and $F_\infty=2\sqrt{kl}$.
Differentiation gives $d\log F_p/dp=\log2-H(w,1-w)\ge0$.
For $d\ge r$, the tail lower penalty is therefore $(d/r)^pF_p$.
At $d=r$ its limiting value recovers the optimized endpoint penalty
$2e^{-\gamma}\sqrt{kl}$, including location paths of order $1/\nu$.
For $d<r$ the tail bound drops the penalty, since every observation can
be strictly inside a limiting uniform support.

Finite intervals with held sd use $|B'(t)|\le3$ for $0\le t\le1$,
the decreasing peak height, and
$z^v\ge z^p-(q-p)/(ep)$ for $z\ge0$, $p\le v\le q$.
The latter follows by bounding $-z^v\log z$ on $0<z<1$;
for $z\ge1$ powers increase. These bounds also include $p=1$.

The inequalities are exact-arithmetic results conditional on the computed
comparisons. The implementation uses a practical margin of
$4096\epsilon(1+\text{magnitude})(1+n\epsilon)$, enlarged by $1+p$
(or $1+q$) for power-dependent arithmetic. Likelihood margins include $n(5+2|\log\sigma|)$ in the magnitude to cover cancellation of scale and normalizer terms; held subunit shapes also include their log-gamma magnitudes. Endpoint log-distance comparisons
within this margin omit the endpoint penalty and set `endpoint_uncertain`:
the limiting penalty is discontinuous there, so adding a small likelihood
margin would not suffice. Overflow in a penalty computation or a subnormal normalization radius returns an unavailable bound. These are
not outward-rounded interval bounds. Held-sd tails that remain unresolved stop with `UniformUnresolved` (a numerical binding condition): strictly interior observations admit sufficiently
large finite shapes above the unpenalized uniform height, whereas rounded
endpoint equalities cannot justify the discontinuous penalty. For example,
double-precision observations $\pm\operatorname{sqrt}(3)$ with mean zero and
sd one are strictly interior to the exact endpoint. At shape $10^{19}$ their
likelihood exceeds the previously selected penalized face by more than one
log-likelihood unit for just two observations. The independent high-precision
check retains this counterexample. Numerical checks and reproducible
independent mpmath comparisons accompany the
[free-shape report](../../../../../dev/research/evaluations/2026-11-07-ged-free-report.md).

For a constant sample $x_i=a$ with mean held at $m\ne a$ and sd free,
$R_p=|a-m|$ is independent of $p$. Strict increase of $h(p)$ proves the
uniform escape, on the interval with endpoints $a$ and $2m-a$.
This route sets `GedResult::certified` on a `UniformBoundary` stop and retains
the exact face parameters rather than an arbitrary large shape.

For constant data at the held or free mean with sd held, the optimal residual
penalty is zero. With $t=1/\nu$, the derivative of the log peak is
$\tfrac32[\psi(3t)-\psi(t)]-1/t>0$, since
$\psi(3t)-\psi(t)=2/(3t)+\sum_{j=1}^\infty
2t/[(j+t)(j+3t)]$.
Thus the unique free-shape maximum is $\nu=1$; a free mean equals the
common observation. A second analytic certificate applies to held sd when
$\log R_1-\log\sigma>K=9/4-\gamma-(\log2)/2$.
Indeed $D(t)=B(t)-tB'(t)$ increases on $[0,1]$, since
$D'(t)=-tB''(t)>0$, and $\tfrac12\log3+D(1)=K$.
Consequently $d\log[(R_1/b(\nu))^\nu]/d\nu
=\log R_1-\log\sigma-\tfrac12\log3-D(1/\nu)>0$.
The power-mean inequality gives a penalty lower bound
$n(R_1/b(\nu))^\nu$, exact at shape one, while the peak height decreases.
Therefore the global shape maximum is uniquely $\nu=1$. Mean, when free,
can be any median. A nontrivial median interval stops with `Nonunique` and
two endpoint witnesses. For example, observations $-1,1$, sd $0.1$ held,
and free mean and shape have every $(\mu,\nu)\in[-1,1]\times\{1\}$
as global maximizers. This is a counterexample to free-shape uniqueness.

## Finite-shape score certificate

Let $p=\nu>1$, $r_i=|x_i-m(p)|$, $T=\sum_i r_i^p$,
$U=\sum_i r_i^p\log r_i$, $V=\sum_i r_i^p(\log r_i)^2$,
$H=\sum_i r_i^{p-2}$ and
$G=\sum_i\operatorname{sign}(m-x_i)r_i^{p-1}\log r_i$.
Terms with a removable zero are evaluated by continuity. With mean free,
$m(p)$ is the unique root of the strictly increasing location score
$\sum_i\operatorname{sign}(m-x_i)|m-x_i|^{p-1}=0$.
The implicit derivative is $m'=-G/[(p-1)H]$. For held mean take $m'=0$.

With sd free, the envelope score is

$$
A(p)=\frac{p^2\ell'(p)}n
=\psi(1+1/p)+\log p-D(p),\qquad
D=pU/T-\log(T/n).
$$

Writing $W=V/T-(U/T)^2$, differentiation at the optimal location gives

$$
A'=\frac1p-\frac{\psi_1(1+1/p)}{p^2}-pW
+\frac{p^2G^2}{(p-1)TH}.
$$

The last term is omitted when mean is held. In particular no generic
monotonicity of $D$, of the location path, or of the profile score is assumed.

For held sd put $z_i=r_i/b(p)$,
$\beta=[3\psi(1+3/p)-\psi(1+1/p)]/2$ and
$E=[\psi_1(1+1/p)-9\psi_1(1+3/p)]/2$.
Then $(\log b)'=\beta/p^2$ and $\beta'=E/p^2$. With $T,U,V,H,G$
now computed from $z_i$, the corresponding score is

$$
A=\psi(1+1/p)-\beta+\frac{p\beta T-p^2U}{n}.
$$

Its derivative is enclosed using $A'=2A/p+p^2\ell''/n$, where

$$
\frac{\ell''}n=
-\frac{\psi_1(1+1/p)+E}{p^4}
-\frac{2[\psi(1+1/p)-\beta]}{p^3}
-\frac{V-2\beta U/p+(\beta^2/p^2-E/p^3)T}{n}
+\frac{pG^2}{n(p-1)H}.
$$

Again omit the final term when mean is held. The formulas follow by
differentiating the raw likelihood and taking its location Schur complement.
If $H$ is infinite at an observation knot below shape two, its inverse is zero;
an unavailable finite bound simply disables curvature contraction there.

On a shape interval, monotonicity of digamma and trigamma encloses their
values analytically. The location equation is enclosed for every shape in
that interval, with safeguarded interval Newton steps or sign brackets.
For a location bracket $[a,b]$, each residual ranges from its distance to
$[a,b]$ to its greatest endpoint distance. Joint ranges of $r^p$,
$r^p\log r$ and $r^p(\log r)^2$ use the endpoint values and the sole
interior extrema $r=e^{-1/p}$ and $r=e^{-2/p}$. Negative residual powers
retain infinite upper bounds when needed. These elementary bounds enclose
$A$ and $A'$ without differentiating a numerical location fit.

A location enclosure on a parent shape interval also encloses the minimizer
on every child interval and at its endpoint shapes. Child score boxes start
from that enclosure; likelihood bounds use it in the convex supporting-line
moment bound instead of solving the same location problem again. Budgets
shorter than the binary64 mantissa retain the cold-start enclosure route,
preserving short-budget stops. Residual
endpoint powers and logarithms are shared between the value, score and
curvature moments. Lower powers in the location Hessian are evaluated
directly: an underflowed $r^p$ does not justify a zero $r^{p-2}$.

Every finite shape is either excluded by a likelihood upper bound, excluded
from the stationary set by a definite score sign, or retained in a root
box. A strictly negative upper bound for $A'$ permits monotone contraction;
otherwise the interval is subdivided. This covers knots and shape one;
shape one is also retained explicitly with its full median face. The tail
bounds in the preceding section close the remaining half-line. Thus every
global maximizer belongs to the explicit boundary candidates or retained
coefficient boxes. This is a compactness/stationary-point argument, not an
assumption of a single root. Multiple roots inside one sufficiently small
box need not be distinguished to obtain coefficient accuracy.

Acceptance requires a winning shape box of width at most
$2\times10^{-7}(1+\nu_{\rm lower})$, a normalized location width at most $2\times10^{-7}$
(the normalization unit is the maximum endpoint residual about the held
mean or sample midrange), and log-sd width at most $2\times10^{-7}$.
The representative is further refined, but its accuracy claim comes from
the entire box. Every other root or boundary candidate must be separated
by likelihood bounds; otherwise the fitter stops with comparison witnesses.
Nontrivial median intervals and subunit-shape knot ties also stop explicitly.
No general uniqueness theorem is claimed.

All finite `Ok` results and selected uniform limits set `GedResult::certified`.
Failed certificates never return a numerical proposal as `Ok`. The default
`iterlim=200` is unchanged. Each shape certificate has at most
$128\min(\mathrm{iterlim},100000)$ evaluations, with inner brackets and
root-refinement solves capped by $\min(\mathrm{iterlim},100000)$ iterations.
Exhaustion retains evaluated witnesses and stops on budget; floating-point
or unresolved-tail failures stop numerically. Held subunit-shape location
fits enumerate the observation knots and use the standard enumeration cap.
The stated practical floating-point margins apply to score and derivative
comparisons as well as likelihood bounds.

Inverse normalization is checked separately from the root cover. The returned
mean and log-sd must round-trip within $2\times10^{-8}$ in normalized
coordinates, and the likelihood evaluated at the actual original-coordinate
coefficients must agree with the normalized candidate within the practical
margin. Witness likelihoods are evaluated at those actual coefficients too.
A large offset with small spread can make a certified normalized location
unrepresentable; this stops with `TransformFailure`. Fixed convex-shape fits
also check the returned location's monotone score, including the normal mean
shortcut. No normalization failure is converted into a finite success.


Rounded equality of endpoint residuals does not imply containment by a
reflected uniform support. With $u=2^{-53}$, observations $-1+u,1+2u$ and
held mean $u$, both computed residual radii equal one, but reflecting the
left observation gives the right endpoint one and excludes the other
observation. Such a face stops numerically. Original observations also
define the Laplace median interval: normalizing $1,\ldots,19,10^{308}$
merges its central observations, but the original medians still span $[10,11]$.

Held-shape fits independently check the analytic log-scale profile against the
returned scale, with tolerance $2\times10^{-8}$ plus its arithmetic margin.
An intermediate subnormal product can fail this check even when a positive
scale is returned: for $x=(0,0,10^{-320})$ and held $\nu=1/2$, the ideal scale
is $10^{-320}\sqrt{120}/36$. Such losses stop numerically, as do positive
residuals lost to underflow before a held subunit power is applied. They do
not imply unbounded likelihood. Density evaluation retains the logarithm of
nonzero residuals even when their standardized quotient underflows; this does not relax the fitter's
normalization certificate.
