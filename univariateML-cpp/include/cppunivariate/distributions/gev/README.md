---
id: gev
inference_numeric:
  - regime: "gumbel member with shape held"
    available: true
    condition: "finite mu and sigma > 0; xi = 0 held; mu and/or sigma free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "regular interior model"
    available: true
    condition: "finite mu, sigma > 0 and xi > -1/2; any nonempty supported free subset; if xi is free require xi < 1; excluding the preceding available submodels"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "critical upper endpoint"
    available: false
    condition: "finite mu, sigma > 0 and xi = -1/2; any nonempty supported free subset"
    reason: "one endpoint direction has a sqrt(n log n) Gaussian experiment and the complement has root-n rate; ordinary finite Fisher inference is unavailable"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "Poisson upper endpoint"
    available: false
    condition: "finite mu, sigma > 0 and -1 <= xi < -1/2; any nonempty supported free subset"
    reason: "one endpoint direction has a non-Gaussian Poisson experiment at rate n^(-xi), with a root-n complement; free xi = -1 additionally constrains shape"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "active upper fitted shape bound"
    available: false
    condition: "finite mu, sigma > 0 and xi = 1 free"
    reason: "the model is DQM with full rank but the fitted shape bound gives a Gaussian cone experiment rather than ordinary joint Wald calibration"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "held shape below fitted endpoint range"
    available: false
    condition: "xi < -1 held and at least one coefficient free"
    reason: "the mathematical endpoint experiment is nonregular; nontrivial accepted free-scale exceptions occur only when all observations are below held mu, an event of probability exp(-n) under this model"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
name: Generalized extreme value
support: {type: real, lower: "if (xi > 0) mu - sigma/xi else -Inf", upper: "if (xi < 0) mu - sigma/xi else Inf", closed: [true, true], endpoint_log_density: {lower: "-Inf", upper: "if (xi == -1) -log(sigma) else if (xi < -1) Inf else -Inf"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
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
  - lower: -1.0
    restriction:
      mechanism: endpoint
      exempt_when_held: []
      reason: For xi below -1 the density is unbounded at the upper end of its support, so placing that end at the sample maximum makes the likelihood unbounded.
  - upper: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held: []
      reason: Without an upper bound on xi, shrinking the scale while the lower end of the support moves to the sample minimum makes the likelihood unbounded for every sample, also with mu or sigma held. The bound xi <= 1 prevents this whenever fewer than half of the observations are tied at the minimum.
  kind: regular
r:
  density: dgev
  sampler: rgev
  control:
    mu: Location to hold fixed, or NULL to estimate.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    xi: Shape to hold fixed, or NULL to estimate within `[-1, 1]`. Held shapes above one are accepted when the collapse criterion permits.
    reltol: Requested relative coefficient accuracy; conditional point-profile solves also use dimensionless scores.
    iterlim: "Work limit per solve: min(iterlim, 100000) solver iterations; certificates allow 128 * min(iterlim, 100000) slab, box or profile evaluations."
log_density: Piecewise((-log(sigma)-(x-mu)/sigma-exp(-(x-mu)/sigma), Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma)-(1+xi*(x-mu)/sigma)**(-1/xi), True))
limits:
- {firmness: proved, name: held-minimum half tie, fixed: [mu], status: NoMLE, message: "minimum multiplicity permits scale collapse", when: '$\sigma \to 0$ with $\mu$ held at the sample minimum and $\xi=1$', detect: 'Shape and scale are free and exactly half the observations equal held mu, except two distinct values with equal counts: that case attains the supremum at xi=-1.'}
- {firmness: proved, name: scale collapse, status: NoMLE, message: "minimum multiplicity permits scale collapse", when: '$\xi \to 1$ and $\sigma \to 0$, with the lower end of the support at the sample minimum', detect: 'All coefficients are free and exactly half of the observations equal the minimum, except two distinct values with equal counts: that case attains the supremum at xi=-1.'}
examples: |
  set.seed(1)
  x <- rml(200, mlgev())
  mlgev(x)

summary: The generalized extreme-value distribution unifies the limiting laws for normalized block maxima.
classification: specialist
classification_reason: Used to model block maxima spanning the three extreme-value tail regimes.
aliases:
- "GEV"
- "Fisher–Tippett"
key_references:
- 1
references:
  - "Coles, S. (2001). An Introduction to Statistical Modeling of Extreme Values. Springer. doi:10.1007/978-1-4471-3675-0."

group: extreme value
construction:
- base
- generalisation
relations: {}
fit:
  method: profile-search
  certified: 'partial'
  note: "gev/mle.hpp, gev_held_profile and gev_profile: Held xi=-1 has closed-form shifted reverse-exponential fits; held xi=0 and sigma give analytic location; held sigma and -1<xi<0 give a strictly concave, bracketed location solve. These nontrivial submodels are certified. Shape-only fits with mu and sigma held use derivative interval coverage and explicit support-edge envelopes, with numerical or budget stops if unresolved. All-free two-point samples with minimum fraction at most one half have a unique reverse-exponential maximum, computed from counts with coefficient precision guards; exact public location or scale holds containing that member inherit its certificate. Exact half-minimum ties have proved global bounds both with all coefficients free and with mu held at the minimum; attained members are retained. The general all-free fit uses joint_certificate.hpp: a shape-slab cover, monotone endpoint-gap brackets, a minimum-pivot Gumbel seam and a one-sided reverse-exponential face envelope. Interior maxima require joint coefficient inclusion and global candidate comparison; unresolved coverage or coefficient inclusion retains a successful numerical search with certified=false. Free-shape fits with one nuisance held attempt a constrained profile/box cover in held_certificate.hpp, including the Gumbel seam and moving-support value envelopes; successful search results remain uncertified if the cover cannot close. Other conditional fits retain numerical root discovery without global coverage."
areas:
- extremes and environment
existence:
  uniqueness: unknown
  statement: >-
    Without restrictions the likelihood is unbounded for every sample, so
    fits keep the shape in $-1 \le \xi \le 1$. Within that range the only
    known obstacle is a tie at the sample minimum. If more than half of the
    observations equal the minimum, no MLE exists. If exactly half do, the
    supremum is approached as $\xi \to 1$ and the scale shrinks. It is
    attained only for two distinct values with equal counts, at $\xi=-1$.
  reason: >-
    Near an end point of the support the density behaves like a power of the
    distance to it. For $\xi < -1$ that power is negative at the upper end,
    so moving the end point onto the sample maximum makes the likelihood
    unbounded. At the lower end, shrinking the scale onto a
    minimum tied $k$ times makes the likelihood behave like
    $\sigma^{(n-k)/\xi - k}$, which is unbounded when $\xi k > n - k$; for
    $\xi \le 1$ this needs $k > n/2$, and $k = n/2$ at $\xi = 1$ is the
    boundary case.
---

The generalized extreme value distribution models maxima. A positive shape $\xi$ gives a heavy right tail, a negative one a finite upper end point, and $\xi = 0$ is the Gumbel distribution.

## Details

For the held-location collapse, put $\mu=m=\min x_i$, fix $\xi>0$
and let $\sigma\downarrow0$. Each of the $k$ minimum terms is
$e^{-1}/\sigma$; for $d_i=x_i-m>0$ each other term is asymptotic to
$\xi^{-1-1/\xi}d_i^{-1-1/\xi}\sigma^{1/\xi}$. Hence the product is
asymptotic to a positive constant times
$\sigma^{(n-k)/\xi-k}$. Equality has a finite positive path limit,
not a proof that the global likelihood is maximized there. With location
free and $\xi=1$, set $\mu=m+\sigma/2$; the minimum terms become
$4e^{-2}/\sigma$ and the remaining terms are asymptotic to
$\sigma/d_i^2$, again giving a finite limit at $k=n/2$. The pair bound below proves the global supremum for all-free half ties.
The held-minimum half-tie bound below resolves the corresponding public hold separately.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-extreme.md) describes the limiting paths and the scope of the boundary comparisons.

Fits restrict the shape to $-1 \le \xi \le 1$, because outside that range the likelihood can be made unbounded: at the upper end of the support for $\xi < -1$, and at a tied sample minimum for large $\xi$. At $\xi = -1$ the upper end point belongs to the support, so the boundary fit, the reversed exponential distribution, is a proper member of the family. Fits with $-1 \le \xi < -1/2$ are non-regular: the usual asymptotic theory for the MLE does not apply there.

Only ties at the sample minimum matter. Let $k$ observations equal the minimum. With $\xi > 0$ held and $\mu$ and $\sigma$ free, a finite maximum requires $\xi k < n - k$. With all three coefficients free, more than $n/2$ ties always make it unbounded. At exactly $k=n/2$, the global log-likelihood supremum is
$A=n\log2-n-2\sum_{x_i>m}\log(x_i-m)$, by the pair bound below.
It is attained only when all nonminimum observations equal a common $M$:
then $\xi=-1$, $\sigma=(M-m)/2$ and $\mu=(M+m)/2$ maximize globally.
Otherwise the fitter stops with the finite unattained supremum in its details.
With $\mu=m$ held, the distinct supremum is
$A_m=-n/2-2\sum_{x_i>m}\log(x_i-m)$. It is attained precisely for the
two-point sample, now at $\sigma=M-m$, $\xi=-1$. This is a direct
held-coordinate bound, not an unconstrained Fréchet transport.

Held shapes above 1 are accepted when the tie condition allows them. A held shape below $-1$ is accepted only when $\mu$ is held at or above the sample maximum, or when every coefficient is held.

With the shape held, holding the scale leaves a one-dimensional fit for the location (certified for nonpositive shape), and holding the location leaves one for the scale. With the shape free, the fit profiles over $\xi$ and compares interior maxima with both shape bounds. The shape-only search is certified as described below. The general all-free fit uses the global cover below. Conditional searches with a free shape and one held nuisance coefficient attempt the constrained cover described below; unresolved cases retain the numerical search result. The default fit needs three finite, non-constant observations.


At fixed scale and $-1<\xi<0$, set
$t_i=1+\xi(x_i-\mu)/\sigma$. The location score and curvature are
$\ell_\mu=\sigma^{-1}\sum t_i^{-1}(1+\xi-t_i^{-1/\xi})$
and
$\ell_{\mu\mu}=(1+\xi)\sigma^{-2}\sum t_i^{-2}(\xi-t_i^{-1/\xi})<0$.
Each individual score changes sign at
$x_i+\sigma[1-(1+\xi)^{-\xi}]/\xi$. Their extremes, intersected
with $\mu>\max x+\sigma/\xi$, bracket the unique global location
maximum. The score tends to positive infinity at the support boundary
and is negative beyond the last individual crossing. Bisection stops at
width `reltol*sigma` and a total dimensionless location-score residual
below $10^{-3}$, using `min(iterlim,100000)` iterations; exhaustion
is non-convergence with a structured `numerical` stop. The score evaluates its near-zero difference
with `expm1`, also for shapes close to zero. The exact-arithmetic
certificate is conditional on computed signs; the practical location
margin is the stated bracket width. Nonfinite likelihoods or an
unrepresentable interior solution stop numerically. At $\xi=-1$ the
linear likelihood selects $\mu=\max x-\sigma$; at zero the analytic
Gumbel location applies. Positive held shape can have changing curvature
and retains the numerical scalar search.


### Shape-only coverage

With location and scale held, put $z_i=(x_i-\mu)/\sigma$ and
$L_i(\xi)=\log(1+\xi z_i)/\xi$. The support interval is the intersection
of $[-1,1]$ and $1+\xi z_i>0$, with the reverse-exponential endpoint
included. On compact interior segments, interval automatic differentiation
bounds $\ell$, $\ell'$ and $\ell''$. Across zero shape the series
$L_i=\sum_{j\ge0}(-\xi z_i)^j z_i/(j+1)$ removes the singularity.
After degree $N$, absolute remainder bounds for value, first and second
derivatives are respectively
$|z|r^{N+1}/[(N+2)(1-r)]$, $z^2r^N/(1-r)$ and
$2|z|^3(N+1)r^{N-1}/(1-r)^3$, where $r=\max|\xi z|<1$.
Segments separated from zero use the logarithmic formula directly.

At an open support edge, write $y=\exp(-L)$ and $c=1+\xi$.
Each term $c\log y-y$ is at most $c\log c-c$, whose maximum over
a shape interval is at an endpoint. For the observation defining a negative
shape edge $a=-1/z$, its term is at most
$-(1+1/a)\log(1+bz)$ throughout $(a,b]$. For a positive edge $b=-1/z$,
put $Y=(1+az)^{-1/b}$; when $Y>1+b$ its term is at most
$(1+b)\log Y-Y$ throughout $[a,b)$. Adding the universal bounds for the
other observations excludes an edge neighbourhood only if its upper bound
is below a finite incumbent. No endpoint neighbourhood is omitted by a nudge.
The critical edge $a=-1,z=1$ can remain unresolved and then stops numerically.

The remaining boxes are discarded by a likelihood bound or a strictly signed
score. Negative curvature plus opposite endpoint scores isolates exactly one
maximum. Bisection bounds its coefficient to relative `reltol`; all isolated candidates
and attained endpoints are compared. Unresolved comparisons stop numerically.
For $x_i=\mu$ throughout, the likelihood is identically
$-n(1+\log\sigma)$: every shape is a maximizer and the fitter returns zero.
This is a counterexample to unconditional uniqueness. If every standardized
observation is one, the uniform density bound $h_\xi(z)\le1$ proves that
$\xi=-1$ is the global maximizer, returned without differentiating the endpoint.

These are exact-arithmetic bounds conditional on computed comparisons,
with practical margin $4096\epsilon_{\rm mach}n$ in comparisons and
$32\epsilon_{\rm mach}n$ in the curvature-based root enclosure. They are
not outward-rounded intervals. The series and root loops each have
`min(iterlim,100000)` iterations; segment and edge evaluations share
`128*min(iterlim,100000)`. Exhaustion gives a structured `budget` stop;
an unresolved inner solve or arithmetic comparison gives `numerical`.
Successful original-coordinate likelihoods must agree with standardized
values within $10^{-7}n$. Stopped witnesses are never estimates.

### Half-minimum pair bound

For two observations distance $d>0$ apart, optimize the scale of a Weibull
or Fréchet density with shape $a\ge1$ and endpoint distances having ratio
$r\in(0,1)$. The maximum product is

$$\frac{4e^{-2}}{d^2}
 \left[\frac{a r^{(a-1)/2}(1-r)}{1+r^a}\right]^2
 =\frac{4e^{-2}}{d^2}
 \left[\frac{a\sinh u}{\cosh(au)}\right]^2,
 \qquad r=e^{-2u}.$$

Convexity gives $\sinh(au)\ge a\sinh u$ and
$\cosh(au)>\sinh(au)$, so the bracket is strictly less than one.
The only attained endpoint equality is Weibull shape one, one observation
at its endpoint, and scale $d/2$. GEV positive shapes correspond to Fréchet
with $a=1/\xi\ge1$; negative shapes correspond to reflected Weibull with
$a=-1/\xi\ge1$. At zero shape, optimizing Gumbel location gives the same
bound times $[t/\cosh t]^2<1$, $t=d/(2\sigma)$.

Pair each of the $k=n/2$ minima with one nonminimum and multiply the bounds.
This gives $A$ above for every finite GEV fit, irrespective of joint paths.
The positive-shape collapse approaches $A$. Equality at finite parameters
requires every pair to have the same distance and the same reverse-exponential
scale; this is precisely the two-point exception. These pair bounds do not
establish uniqueness for general samples.

Explicit uniform location and scale bounds for samples whose largest value
multiplicity is below half are proved in the
[nuisance-bound derivation](../../../../../dev/research/evaluations/2026-11-07-evt-joint-data/nuisance-bounds.md).
They also bound the constrained held-scale Fréchet problem. Combined with
upper semicontinuity, they prove finite GEV attainment in that multiplicity
regime. These uniform bounds alone do not locate the joint maxima.



For any all-free two-point sample, let $p=k/n\le1/2$ be the fraction at its
minimum and $d=M-m$. The unique global maximum is
$\xi=-1$, $\sigma=pd$, $\mu=M-pd$, with log likelihood
$-n[1+\log(pd)]$. To see this, profile scale in the reflected Weibull and
Fréchet representations. Relative to that member, the likelihood's $n$th
root is respectively

$$R_W=\frac{ap(1-r)r^{(a-1)(1-p)}}{p+(1-p)r^a},\qquad
R_F=R_W r^{1-2p}.$$

For $p\le1/2$, each is at most the strict pair ratio
$a r^{(a-1)/2}(1-r)/(1+r^a)<1$. The zero-shape limit is strictly below
one as well, since the pair ratio tends to $t/\cosh t<1$ for finite Gumbel
scale. Equality occurs only at reflected Weibull shape one and its attained
endpoint. The code uses $pd$ directly, instead of subtracting the sample
mean from $M$.

For a half tie, opposite-sign endpoints use $(m+M)/2$ to preserve a small
location; subnormal halving residuals are checked with `fma`. Endpoint
feasibility is enforced using a TwoDiff rounding residual, with any scale
adjustment limited to `reltol/4` relative to the initial scale. Outside the
half tie, a $64\epsilon_{\rm mach}(|M|+\sigma)$ location-error allowance
must be below `reltol*abs(mu)/4`; subnormal scale or unresolved public
representation stops numerically. A near-zero location with normal
scale and a resolved feasible face likelihood is instead returned uncertified.
The analytic precision control is at most
$10^{-2}$ and requires at least $1024\epsilon_{\rm mach}$; successful
likelihoods agree with the analytic supremum within $10^{-7}n$. These
safeguards can reject a known finite maximum whose relative coefficient
accuracy cannot be justified by the implemented double-arithmetic guard.

### Sharp density bound and the held-minimum half tie

The standardized density obeys the sharp uniform bounds
$h_\xi(z)\le1$ and $z^2h_\xi(z)\le1$ for $-1\le\xi\le1$.
For the second, put $L=\log(1+\xi z)/\xi$, $y=e^{-L}$. Then

$$z^2h_\xi(z)=4\left[\frac{\sinh(\xi L/2)}{\xi}\right]^2
 e^{-L-e^{-L}}\le(1-y)^2e^{-y}\le1.$$

The first inequality follows from convexity of $\sinh$ and $|\xi|\le1$,
including zero shape by continuity. For $0<y\le1$ the last expression is
strictly below one; for $y\ge1$ its maximum is $4e^{-3}<1$. Finite equality
occurs only at the closed reverse-exponential endpoint $\xi=-1,z=1$.

If public location is held at $m=\min x$, the $k=n/2$ minimum densities
are exactly $e^{-1}/\sigma$ and every nonminimum density is at most
$\sigma/(x_i-m)^2$. Multiplying proves the global bound $A_m$ above.
Equality requires every nonminimum to equal $m+\sigma$ and $\xi=-1$.
Otherwise the $\xi=1$, $\sigma\downarrow0$ path approaches $A_m$ without
attaining it. The fitter resolves this before the numerical scale/shape
search and reports the finite supremum. If scale is also held at the
attaining value, the same finite member remains a certified maximum.


### General all-free shape cover

For a nonconstant sample with fewer than half its observations at the minimum,
the all-free fit covers the fitted shape interval $[-1,1]$. At each nonzero
shape, scale profiling leaves a unique monotone endpoint-gap root. Its gap
increases with reciprocal absolute shape, so brackets at two shape endpoints
enclose all nuisance optima between them. The cover uses reflected-Weibull
gap coordinates near the upper endpoint and minimum-pivot coordinates around
zero and on the positive branch. The pivot form keeps all exponential terms
at most one and permits power-of-radius Taylor bounds through the Gumbel seam.

The pivot jets retain a common corner dependency: for $z\ge0$,
$L$, $-L_\xi$ and $L_{\xi\xi}$ increase in $z$ and decrease in $\xi$.
This follows by differentiating
$L=z\int_0^1(1+\xi zt)^{-1}\,dt$ under the integral.
Scalar series at the two corners therefore bound all three quantities,
including slabs crossing zero.

A slab is excluded by a likelihood upper bound, a definite profile-score sign,
or strict profile convexity. Strict negative profile curvature and opposite
endpoint scores isolate a stationary maximum. A joint interval-Newton step
then bounds all three public coefficients. The closed reverse-exponential
face is compared through a one-sided envelope that avoids derivatives at its
moving support endpoint. Every surviving maximum is compared globally;
unordered comparisons within the arithmetic margin, exhausted coverage and
coefficient inclusion failures retain a successful profile search with
`certified=false`. Failed searches and proved nonexistence still stop. An
interior fallback must pass the total shape-score gate; it supplies no global
or coefficient certificate.

These are exact-arithmetic certificates conditional on computed comparisons,
with compensated sums and explicit practical floating-point margins, rather
than outward-rounded arithmetic. The certificate budget is
$128\min(\mathrm{iterlim},100000)$; inner sign brackets and contractions have
solver budgets $\min(\mathrm{iterlim},100000)$. A point-profile Newton solve
may improve a feasible lower witness but never certifies a returned fit.
The [derivation and independent checks](../../../../../dev/research/evaluations/2026-11-08-gev-allfree-data/joint-proof.md)
state the nuisance, face, Taylor and coefficient-inclusion arguments.
General public nuisance holds require their own constrained cover and remain
outside this all-free certificate.


Public-coordinate restoration checks compensated support residuals against
every original observation. At the closed reverse-exponential endpoint an
error-free sum comparison precedes division, so underflow to negative zero
cannot admit an excluded observation.
An inward rounding error can be repaired only through a free location or scale,
within its coefficient tolerance, followed by a fresh support and raw-likelihood
check. The restoration comparison allows the larger of $10^{-7}n$ and a
256-double-epsilon margin on the likelihood operands. Held coefficients remain exact. This restoration check also applies to
fixed-shape and held-nuisance fits; it adds no global certificate to their search.

## R notes

Estimated coefficients on a `fit_domain` bound carry `restricted` and emit a warning of class `univariateML_restricted`; held coefficients are not flagged. Failed optimizers return a failure status unless the score norm in fitting coordinates is below `1e-3`; unresolved scalar searches stop.


The unique all-free two-point member also solves any held subset containing
it: holding public location at $M-p(M-m)$ or scale at $p(M-m)$ preserves the
global maximum and uniqueness. Exact range, integer-product and subtraction residuals verify that the held
value contains the real member; equality to a rounded free estimate alone
does not qualify. General
held values use the constrained cover below.

Recognized exact holds preserve the two-point certificate's precision stop;
failure does not fall through to a numerical conditional search.


### One public nuisance coefficient held

For free shape with either location or scale held, the constrained likelihood
uses a constrained stationary/value cover in shape and the free nuisance
coefficient. The bound
$f(x)\le\min(1/\sigma,\sigma/(x-\mu)^2)$ confines every point above a
feasible likelihood level to a finite nuisance interval. With location held,
the universal bound requires $2j>n$, where $j$ counts observations different
from the held location. The sharper negative-residual bound below fills the
remaining compactness cases; collapse equalities retain their separate
classifications.
With scale held, the location interval is always finite for a nonconstant
sample and finite witness.

The cover includes the Gumbel member under the same hold, both attained shape
bounds, and value envelopes across moving support. An interior isolated
maximum needs an interval negative definite constrained Hessian, root
existence and joint coefficient inclusion. Support corners use the one-sided
bounds below. Completion proves global dominance by a stationary and
value box cover. This uses the joint certificate's interval machinery without
assuming the constrained profile is unimodal. Arithmetic margins and the
compactness proof are given in the
[constrained-cover proof](../../../../../dev/research/evaluations/2026-11-10-gev-held-data/proof.md).

A successful search remains available when coefficient inclusion, face
comparison or coverage cannot close within the work allowance. It is then
uncertified; incomplete coverage creates no stop. A strictly better feasible
point, verified again in public coordinates, instead disproves the search and
stops with both point witnesses. The C++ result's
`certified` field records completion for all-free and held-nuisance covers;
older conditional routes do not generally populate that field. The catalogue flag remains partial.

For all-free and these held-nuisance fits, R attaches the logical `certified` attribute to the fit;
the common binding result carries the same marker as zero or one.

The [profile and support-face extension](../../../../../dev/research/evaluations/2026-11-10-gev-held2-data/proof.md)
contracts the unique fixed-scale location root on nonnegative shape slabs.
Negative-shape and held-location fits retain the two-coordinate cover.
For nonpositive shape, a one-observation lower-tail value bound also excludes
remote location boxes where likelihood derivatives overflow at the Gumbel seam.
Their root enclosures can remain broad near moving support; uniqueness
alone does not make the profile cover cheaper. An active reverse-exponential support corner has a one-sided value isolator on
the feasible portion of an extended corner rectangle, avoiding derivatives
at zero residual. Its public coordinates use the original sample maximum
and the hold. Data with an observation below a held
location have a compact scale superlevel region even when at least half the observations equal that
location. Together with the minimum-tie classifiers this closes the
$2j\le n$ compactness gap in exact arithmetic; general inner scale-root
uniqueness is not assumed.
