---
id: snorm
inference_numeric:
  - regime: "regular finite interior"
    available: true
    condition: "finite mean, sd > 0 and finite xi > 0; any free subset, including xi = 1 with xi free; analytic piecewise natural-coordinate derivatives; successful interior fit and nonsingular information required"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#two-piece-normal-snorm"
name: Skew Normal
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
- name: xi
  symbol: '\xi'
  role: skew
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dsnorm
  sampler: rsnorm
  control:
    mean: "Optional held mean; NULL estimates it where the hold subset is supported."
    sd: "Optional held sd; NULL estimates it where the hold subset is supported."
    xi: "Optional held xi; NULL estimates it where the hold subset is supported."
    iterlim: "Work limit for root isolation and candidate enumeration; increase after a budget or root-enclosure stop."
log_density: log(sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)) - log(sd) + log(2/(xi+1/xi)) - log(2*pi)/2 - Piecewise(((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2/xi**2, (sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))>0), ((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2*xi**2, True))/2
limits:
  - firmness: proved
    name: scale collapse
    when: 'The free standard deviation $\sigma$ tends to zero at the fitted or held mean.'
    detect: 'Every observation equals the fitted or held mean and scale is free.'
    status: Unbounded
    message: "Scale collapse"
    kind: nonexistence
  - model: hnorm
    firmness: proved
    when: 'As $\xi\to\infty$, the distribution tends to a translated right half-normal with the limiting mean and standard deviation.'
    status: 'RightHalfNormalBoundary'
    numerical: true
  - name: 'reflected translated half-normal'
    firmness: proved
    when: 'As $\xi\to0$, the distribution tends to a reflected translated half-normal with the limiting mean and standard deviation.'
    status: 'LeftHalfNormalBoundary'
    numerical: true
references:
- Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American
  Statistical Association, 93(441), 359–371.
- 'fGarch documentation (accessed 2026-10-31). fGarch: Rmetrics - Autoregressive Conditional Heteroskedastic Modelling. R package reference
  manual. https://cran.r-project.org/web/packages/fGarch/fGarch.pdf.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlsnorm())
  mlsnorm(x)

group: skewed and robust location-scale
construction:
- skewing
- location-scale
relations:
  special_case_of:
  - family: sged
    at:
      mean: mean
      sd: sd
      nu: 2
      xi: xi
fit:
  method: profile-search
  certified: 'yes'
  note: 'snorm/README.md, Details; snorm/certificate.hpp; snorm/test.cpp: all-free algebraic mode comparison, exact weighted held-skewness solves, and Bernstein isolation of rational held-moment stationary equations with constrained half-normal comparisons. Exact-arithmetic certificates are conditional on computed comparisons and the stated floating-point margins.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model two-piece Gaussian errors with unequal scales on the two sides.
summary: Skew Normal provides a model for two-piece Gaussian errors with unequal scales on the two sides.
aliases:
- "two-piece normal"
- "split normal"
- "Fernández–Steel skew-normal"
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, nonconstant samples have a finite MLE exactly
    when the two-piece profile described below attains its global minimum at
    an interior mode. Otherwise the supremum is a translated half-normal
    limit. Constant samples have unbounded likelihood. Joint uniqueness is
    not established. Holding any finite $\xi>0$ gives a unique conditional
    finite maximum unless free scale collapses on constant data at the fitted
    or held mean. With skewness free under moment holds, attainment is decided
    by comparing the finite stationary candidates and constrained half-normal
    limits. Neither attainment nor uniqueness holds for every sample.
  reason: >-
    For a proposed mode $c$, let $A(c)=\sum_{x_i<c}(x_i-c)^2$ and
    $B(c)=\sum_{x_i\ge c}(x_i-c)^2$. Profiling the two side scales
    makes maximization equivalent to minimizing
    $Q(c)=(A(c)^{1/3}+B(c)^{1/3})^3$ on the interval between the sample
    extremes. An interior minimizer gives positive side scales. Endpoint
    minimizers give half-normal limits. This compact profile establishes
    attainment in the closure, but does not establish uniqueness.
---

The Fernández–Steel skew-normal distribution models skewness through different Gaussian scales on the two sides of a mode.

## Details

Fully free constant finite samples, including a singleton, report unbounded likelihood before profile construction: the normal member with mean at the observation has density proportional to $1/\sigma$. Empty or nonfinite samples remain invalid.

The coefficients $\mu$ and $\sigma$ are the distribution's mean and standard deviation. The positive coefficient $\xi$ controls the side-scale ratio, and $\xi=1$ gives a normal distribution. The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-skewreal.md) describes the limiting paths and boundary comparisons.

With all coefficients free, the fitter profiles the two side scales analytically and searches the mode between the sample extremes. Between consecutive observations the stationary candidates satisfy a polynomial equation of degree at most seven. Comparing these candidates and the endpoints gives a global comparison in exact arithmetic, conditional on the computed root comparisons. It does not imply that a finite maximizer is unique. The right and left half-normal limits have their endpoints at the sample minimum and maximum respectively.

Any subset of the three coefficients can be held. For fixed skewness, write
$a=A\sigma$, $b=B\sigma$, and $c=\mu-m(b-a)$ for the two side scales and
mode, with $m=\sqrt{2/\pi}$. On each observation cell the location score is
linear and the inverse-standard-deviation score is quadratic. Enumerating
consistent cells gives the global solution of the strictly concave problem
in inverse scale and location divided by scale. Weighted means are anchored
at the corresponding sample extreme. The profiled residual sum uses centered
within-group sums and the between-group term, avoiding subtraction of large
weighted raw moments. Its analytically profiled likelihood is checked against
the recovered fit within $10^{-10}(n+|\ell|)$. With both moments held the
likelihood is evaluated directly. No finite scale or skewness guard is used.

For free skewness under a moment hold, put $d=3-4m^2>0$ and
$T=(1+\sqrt{1+d})^{-1}$. The rational parametrization

$$
a=\sigma\frac{1-2t-dt^2}{1+dt^2},\qquad
b=\sigma\frac{1+2t-dt^2}{1+dt^2},\qquad -T<t<T
$$

covers every positive pair of side scales with variance $\sigma^2$ once.
The variance identity gives $\max(a,b)\le\sigma/h$ and
$a+b\ge\sigma/h$, where $h^2=1-m^2$. Thus free scale tending to infinity
forces the likelihood to zero uniformly in skewness. Free scale tending to
zero concentrates at the held mean and cannot maximize a nonconstant sample
or a constant sample away from that mean. With sd held, location escaping
to infinity also forces the likelihood to zero. Apart from scale collapse,
the two half-normal faces therefore exhaust the possible maximizing escapes.
Holding the mean sets $c=\mu-m(b-a)$. Otherwise the optimal mode is the
weighted sample mean with weights $a^{-2}$ and $b^{-2}$ in its observation
cell. If standard deviation is free, inverse scale solves
$C u^2+E u-n=0$, where
$C=\sum (x_i-\mu)^2/A_i^2$,
$E=m(B-A)\sum(x_i-\mu)/A_i^2$, and $A_i=A$ or $B$ on the corresponding
side. Its positive root is unique. Eliminating this quadratic from the
remaining skewness score yields a polynomial. The fitter forms these
polynomials directly in Bernstein form, whose convex-hull sign exclusion
and bisection retain every stationary root, including multiple roots.
Clearing denominators introduces known factors at the two excluded endpoints.
For a nonempty side, the factor order is one for held sd with free mean,
three for held mean with free sd when that side is constant, and six for
the latter case when its observations equal the held mean. With held sd
and free mean a constant side instead has order four. An empty side gives
orders eight, twelve and three for held sd alone, held mean alone and both
moments held respectively. The fitter removes only these proved factors;
Bernstein division checks its endpoint remainder against
$8192\,\epsilon_{\mathrm{double}}(d_p+1)$ times the coefficient norm,
where $d_p$ is the polynomial degree.
A failed check stops. This prevents denominator roots from being mistaken
for poorly resolved finite maximizers.

Resultant roots not satisfying the original equations can add candidates;
comparing additional feasible points does not exclude a global maximum.
The squared-residual likelihood is continuously differentiable at moving
joins, so a finite maximum at a join is included among stationary points.

Both half-normal endpoints are compared under the held moment constraints.
With both mean and standard deviation held, each observation at the limiting
endpoint retains the shrinking-side penalty $m^2/(8h^4)$, where
$h^2=1-m^2$. If either moment is free, a vanishing adjustment removes this
penalty. A held mean with free standard deviation gives a quadratic scale
score on the support-constrained half-normal face; a held standard deviation
fixes its half-scale at $\sigma/h$. Free location places its endpoint at the
corresponding sample extreme. These are limits of the sample likelihood,
which need not equal the closed-support half-normal density at an endpoint.
With both moments held, support comparisons within
$128\epsilon_{\mathrm{double}}(1+|c|+|y_{\min}|+|y_{\max}|)$ in normalized
coordinates leave contact unresolved. The unpenalized face, allowing the
endpoint to vary across that margin, is an upper bound on all three
possibilities: contact, strict support and infeasibility. A potentially
winning unresolved face causes a numerical stop. Applying the contact penalty
to a slightly interior face could otherwise discard the true supremum.

The held certificates are global in exact arithmetic, conditional on computed
polynomial signs and likelihood comparisons. A sign-definite Bernstein hull
near a possible turning point is subdivided when its smallest coefficient
magnitude is at most $8\epsilon_w(d_p+1)\|p\|_\infty$, where $\epsilon_w$
is the working-precision epsilon. If this remains unresolved at rational-parameter
width $\sqrt{8\epsilon_w(d_p+1)}$, the interval remains unresolved rather
than being declared root-free. It can be discarded only by a likelihood
upper bound; otherwise the fit stops numerically. This guard prevents a
rounded even root from being silently excluded. For an unresolved shape interval in a given observation cell, relaxing both
moment constraints and the cell constraint gives an upper bound. Let $V_L,V_R$ be the within-group sums of squares and let
$\Delta$ be the difference of group means. For the unscaled side factors
$A,B$ above, profiling the assigned quadratic over unrestricted location and scale gives

$$
Q_* = \frac{V_L}{A^2}+\frac{V_R}{B^2}
 +\frac{k(n-k)\Delta^2}{kB^2+(n-k)A^2},\qquad
\ell_* = n\left[\log m-\log(A+B)+\frac12\log\frac{n}{Q_*}-\frac12\right].
$$

Empty-group terms are omitted. Lower bounds on $Q_*$ and $A+B$ yield an
upper bound on every constrained likelihood in that cell and shape interval, with
the affine data Jacobian restored. Lower moment bounds subtract
$8\epsilon_{\mathrm{double}}(n+1)$ times their absolute-term scale before
clipping at zero. An unresolved interval is excluded only when this relaxed
upper bound is below the best lower witness by the likelihood comparison
margin. This avoids imposing a root-resolution requirement on irrelevant
turning points. All remaining uncertainty is a numerical stop.

Root intervals have width at
most $2\times10^{-14}$ in the normalized rational parameter. Before returning,
the root interval must intersect its observation cell, the enclosed total
likelihood must have width at most $10^{-8}(n+|\ell|)$, and the recovered
public coordinates must vary by at most
$10^{-7}(1+|\theta|)$ across the root interval, and the public-coordinate
likelihood must agree with the side-scale evaluation within
$10^{-10}(1+|\ell|)$. The root-width choice also closes the likelihood
enclosures for strongly skewed held-mean samples, which a parameter-width
check alone does not ([golden hold sweep counterexample](../../../../../dev/research/evaluations/2026-11-07-snorm-held-data/initial-conformance-failure.txt)). Distinct finite or opposite-face candidates within
$10^{-9}(n+|\ell|)$ are reported as a comparison tie. Face decisions use
a $10^{-8}n$ allowance. The public density's moment normalizer is also checked for finiteness.
For its standardized residual $z=L+M$, the recovery check propagates
$16\epsilon_{\mathrm{double}}(1+|L|+|M|)$ through the Gaussian quadratic.
If this can cross the join, the shorter side controls the error bound.
The resulting likelihood uncertainty must be at most
$10^{-10}(n+|\ell|)$. This rejects severe join cancellation instead of
returning an encoded parameter vector whose likelihood cannot be verified.
These are practical floating-point margins, not outward-rounded interval bounds. Failure to isolate, recover
coordinates or evaluate consistently is terminal; there is no optimizer
fallback. Each observation split is a certificate evaluation, each Bernstein
subdivision is an enumeration cell, and subdivision depth is a solver limit
under the [shared work-budget policy](../../../../../dev/design/work-budgets.md).
The half-normal faces are evaluated before interior isolation, so numerical
and budget stops retain their computed witnesses. Exhaustion retains an
unclosed infinite global upper bound.

Uniqueness cannot be asserted for the closure comparison. For constant data
with standard deviation held and mean free, both the right and reflected
half-normal faces have the same maximal density $mh/\sigma$ per observation;
these are distinct equal maximizers and are reported as a tie. Fixed skewness
has a unique finite maximum unless the scale-collapse condition applies.
Finite non-uniqueness also occurs: for $x=(-1,0,1)$ with $\mu=0$ and
$\sigma=151/200$, both half-normal endpoints are infeasible,
$\ell(6)>\ell(1)$, and reflection gives $\ell(\xi)=\ell(1/\xi)$.
The two tails tend to minus infinity, so at least two distinct finite
maximizers exist. An [exact rational check](../../../../../dev/research/evaluations/2026-11-07-snorm-held-data/nonunique.py)
verifies the strict inequalities. The certificate compares all candidates
without assuming unimodality.


For a constant sample $x$ with mean held at $\mu\ne x$ and standard deviation
free, the sharp bound is $f(x)\le m^2/|x-\mu|$. It is approached only by the
half-normal ending at $x$, with half-scale $|x-\mu|/m$, reflected when
$x>\mu$. To see the bound, reflect so $x>\mu$, set $s=a+b$ and
$z=(x-\mu)/s$. For $z\le m$ the normalization alone suffices. For $z>m$
the Gaussian residual is at least $z+m$, and
$z\exp(-(z+m)^2/2)<m$ because $m^2>1/2$. Thus this constant-data case has
no finite maximum; it is not the scale collapse that occurs when the mean
is free or held at $x$.

## R notes

This is the two-piece normal family. `mlsn` fits the Azzalini skew-normal family. Any subset of `mean`, `sd`, and `xi` can be held. Evaluation uses the fGarch mean and standard deviation parameterization.

Use `mlhnorm(x, mu = NULL)` for the right half-normal boundary. For the left boundary, fit `mlhnorm(-x, mu = NULL)` and reflect the resulting distribution back to the original axis.

The free-profile conversion to standard deviation forms the side-scale variance in normalized coordinates before restoring the physical scale.
