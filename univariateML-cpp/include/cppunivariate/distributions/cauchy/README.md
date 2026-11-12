---
id: cauchy
inference_numeric:
  - regime: interior
    available: true
    condition: "finite location and finite scale > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
  - regime: boundary or limit
    available: false
    condition: "one or more coefficients at an excluded boundary or nonfinite limit"
    reason: "this scope requires the stated finite interior parameters; boundary behavior is separate"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Cauchy
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: location
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: scale
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: core_dcauchy
  sampler: stats::rcauchy
  control:
    scale: Optional held scale; NULL estimates it.
    location: Optional held location; NULL estimates it.
    abstol: Joint-fit absolute step tolerance in (location, log(scale)); conditional certificates use fixed tolerances.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: -log(pi) - log(scale) - log(1 + ((x-location)/scale)**2)
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994) Continuous Univariate Distributions, Volume 1, Chapter 16.
  Wiley, New York.
holds: []
limits:
  - name: scale collapse
    firmness: proved
    when: 'The scale $\sigma$ tends to zero at the estimated or held location $\mu$.'
    detect: >-
      Scale is free. With location free, an atom occurs at least $n/2$ times,
      except in a sample of two equally frequent distinct values. With location
      held, at least $n/2$ observations equal that location.
    status: NoMLE
    message: "does not exist"
examples: |
  set.seed(1)
  x <- rml(200, mlcauchy())
  mlcauchy(x)

group: skewed and robust location-scale
construction:
- location-scale
relations:
  special_case_of:
  - family: lst
    at:
      mu: location
      sigma: scale
      df: 1
fit:
  method: convex-solve
  certified: "yes"
  note: "cauchy/mle.hpp, fit_cauchy; Details: the free fit is certified by strict geodesic convexity under the strict half-mass condition, conditional on computed comparisons and the final dimensionless total-score norm < 1e-3. Balanced two-point maxima are explicit. Held location is certified by a verified monotone-score bracket and bisection. Held scale uses exhaustive interval bounds, strictly concave root isolation and separated likelihood comparisons; unresolved ties or numerical certificates stop."
areas:
- robust modelling
classification: classical
classification_reason: A standard model for symmetric errors whose extreme values require tails without finite variance.
summary: Cauchy provides a model for symmetric errors whose extreme values require tails without finite variance.
aliases:
- "Lorentz"
- "Cauchy–Lorentz"
- "Breit–Wigner"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    For a nonempty finite sample with both coefficients free, a finite MLE
    exists uniquely if every distinct value occurs fewer than $n/2$ times.
    Two distinct values occurring equally often also attain a maximum,
    with a continuum of maximizing pairs. In every other sample with an
    atom of multiplicity at least $n/2$, no finite MLE exists.
    With location held, a unique finite scale MLE exists exactly when
    fewer than $n/2$ observations equal that location.
    With positive scale held, a location maximum always exists but need
    not be unique. Holding both coefficients leaves likelihood evaluation.
  reason: >-
    At an atom of multiplicity $k$, collapse has likelihood proportional
    to $\sigma^{n-2k}$. When all $2k<n$, the likelihood tends to zero
    at every boundary, so a maximum is attained in the interior.
    If $2k=n$ at $a$, put $w=\mu+i\sigma$ and $v=-1/(w-a)$.
    Writing $t=\operatorname{Im}v$, the likelihood is
    $\pi^{-n}t^n/\prod_{x_i\ne a}|1+(x_i-a)v|^2$.
    Each denominator factor is at least $(x_i-a)^2t^2$.
    The resulting upper bound is attained only if all observations other
    than $a$ have the same value $b$, with
    $\operatorname{Re}v=-1/(b-a)$. Otherwise it is approached only at collapse.
    With location held, the log-scale score
    $n-2\sum_i\sigma^2/(\sigma^2+(x_i-\mu)^2)$ decreases strictly
    from $n-2k$ to $-n$ when some residual is nonzero.
    With scale held, continuity and decay at location infinity give
    attainment, without strict concavity in location. For observations
    $-a,a$ and held $0<\sigma<a$, the location maxima are
    $\pm\sqrt{a^2-\sigma^2}$.
    For joint uniqueness, join any two upper-half-plane points $(\mu,\sigma)$
    by a vertical line or a circle orthogonal to the real axis.
    On a vertical line, put $\sigma=e^t$. Each negative log-density term,
    apart from a constant, is $\log(e^t+(x_i-\mu)^2e^{-t})$,
    strictly convex unless $x_i=\mu$.
    On a circle with centre $c$ and radius $r>0$, put
    $\mu=c+r\tanh t$ and $\sigma=r/\cosh t$.
    Each term becomes $\log(A_i\cosh t-B_i\sinh t)$, where
    $A_i=((x_i-c)^2+r^2)/r$ and $B_i=2(x_i-c)$.
    Its second derivative is
    $(A_i^2-B_i^2)/(A_i\cosh t-B_i\sinh t)^2\ge0$,
    with equality only at $x_i=c-r$ or $x_i=c+r$.
    At least three distinct observations therefore make the sum strictly
    convex along every such connecting path, excluding two joint maxima.
    The strict sub-half-mass condition requires at least three distinct values.
---

The Cauchy distribution models symmetric errors with heavy tails. Its location $\mu$ is the centre of symmetry, and its positive scale $\sigma$ sets the width. Its tails have neither a finite mean nor a finite variance.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) records the boundary analysis and its historical qualifications. The half-mass equality argument in the existence statement resolves that case explicitly.

Two equally frequent distinct values have a continuum of joint maxima. The fit selects their midpoint as location and half their distance as scale. An atom with more than half the sample mass instead gives unbounded likelihood. An atom with exactly half the mass gives a finite unattained supremum unless the sample has just those two equally frequent values.

Holding location leaves a strictly decreasing scale score with one root exactly below the half-mass threshold. Holding positive scale prevents collapse, but the location likelihood can have several modes and several global maxima. The conditional location fit uses the interval certificate below. The joint fit uses safeguarded Newton steps with its separate geometric certificate.

For held location, the log-scale score is
`g(t) = -k + sum(tanh(log(abs(x-location))-t))`, summing over nonzero
residuals. Its derivative is strictly negative and its limits are `n-2k`
and `-n`, proving uniqueness and global maximality when `2k < n`.
The solver starts at the smallest and largest positive log residual and expands
until strict positive/negative endpoint scores are verified, bounded by the
smallest positive and largest finite double scales. It then bisects to log-width
`1e-12` (or a computed zero score), within `iterlim`. A missing bracket,
iteration exhaustion, nonfinite result or rounded-scale score exceeding
`1e-10*n` reports `IterationFailure`; no unchecked endpoint is returned.
Log residuals and a log-sum-exp likelihood avoid residual-ratio overflow.
This certificate holds in exact arithmetic conditional on the computed
comparisons; the score gate states the practical floating-point margin,
not an outward-rounded interval guarantee. The held-scale certificate is described separately below.
The prior radius-minus-350 bracket failed for `c(0,1e-300,1)` at location zero;
see the [regression report](../../../../../dev/research/evaluations/2026-11-02-cauchy-bracket-report.md).

## R notes

Supplying `location` or `scale` holds that coefficient. When scale is free and the applicable existence condition fails, the fit stops with a non-existence diagnostic rather than returning a collapsing scale.

## Free-fit certificate

Write $F=-\ell$ on the upper half-plane with metric
$ds^2=(d\mu^2+d\sigma^2)/\sigma^2$. The vertical and circular
parameterizations in the existence proof have unit hyperbolic speed.
For a vertical path the second derivative of each term is
$4(x_i-\mu)^2/(e^t+(x_i-\mu)^2e^{-t})^2$; for a circle it is the
nonnegative expression given above. With at least three distinct observations
the summed second derivative is positive along every geodesic.
The hyperbolic plane is a Hadamard manifold: any two points have a connecting
geodesic. If $dF=0$ at one endpoint, strict convexity along that geodesic
makes $F$ strictly larger at the other endpoint. Thus an exact stationary
point is the unique global likelihood maximum, not merely a local maximum.
The strict sub-half-mass condition ensures interior attainment and at least
three distinct values. The balanced two-point exception instead has an
explicit nonunique maximum; the other half-mass and super-half-mass cases
are rejected as described above.

For $t=\log\sigma$ and $z_i=(x_i-\mu)/\sigma$, the total
log-likelihood score is
$U_\mu=\sigma^{-1}\sum_i2z_i/(1+z_i^2)$ and
$U_t=\sum_i(z_i^2-1)/(1+z_i^2)$.
The final free-fit gate is
$\sqrt{(\sigma U_\mu)^2+U_t^2}<10^{-3}$, with finite parameters,
positive scale, finite likelihood and iteration convergence also required.
This is the hyperbolic gradient norm, using the score in $(\mu,\log\sigma)$
coordinates with the location component multiplied by scale; it is a total,
not per-observation, score and is invariant under affine changes of data units.
A small step alone is insufficient. Failure of the gate gives
`IterationFailure`; the binding stops on that status.

This certifies the exact stationary solution in exact arithmetic, conditional
on computed comparisons, with the stated practical floating-point score
margin. It does not prove that a nonzero residual is an exact maximizer.
There is no uniform positive strong-convexity constant over the whole plane:
curvature can tend to zero toward its boundary. Consequently the gate alone
does not supply a numerical likelihood-gap bound. If a lower geodesic Hessian
bound $m>0$ were independently established along the path to the optimum,
convexity would give $F-F_*\leq\|\nabla F\|^2/(2m)$.
No such computed bound is claimed here. Safeguards, median/MAD initialization
and iteration limits find a candidate; the geometric theorem justifies its
global interpretation when stationary. Neither convergence from every start
nor outward-rounded floating-point certification is asserted.


## Held-scale location certificate

For fixed positive scale, standardize with the sample median $c$:
$y_i=(x_i-c)/\sigma$ and $t=(\mu-c)/\sigma$. Ignore the constant
$-n\log(\pi\sigma)$ and write

$$
 L(t)=-\sum_i\log(1+(y_i-t)^2),\qquad
 G(t)=L'(t)=\sum_i q(y_i-t),\qquad q(z)=\frac{2z}{1+z^2}.
$$

Outside $[\min y_i,\max y_i]$ every score term has the same strict
sign pointing inward, so all maxima are inside that compact interval.
For a box $[a,b]$, each residual lies in $[y_i-b,y_i-a]$.
The extrema of $q$ on such an interval occur at its endpoints or at
$z=-1,1$, where $q=-1,1$. Summing these termwise extrema encloses $G$.
Also

$$
 L''(t)=\sum_i h(y_i-t),\qquad
 h(z)=\frac{2(z^2-1)}{(1+z^2)^2}.
$$

The maximum of $h$ on a residual interval occurs at an endpoint or at
$z=\pm\sqrt3$, where $h=1/4$: its other stationary point $0$ is a
minimum. Thus the sum of termwise upper bounds proves strict concavity
where it is negative. Finally, if $d_i$ is the distance from $y_i$ to
$[a,b]$, then $U(a,b)=-\sum_i\log(1+d_i^2)$ is an upper bound for
$L$ throughout that box.

Start with the full data range and the likelihood at $t=0$ as an incumbent
lower bound. Discard a box only when its likelihood upper bound is strictly
below the incumbent, its score has a definite sign, or its strictly decreasing
score places its root outside the box. On a strictly concave box, slightly
expand the endpoints and recheck concavity and strict positive/negative scores.
Bisection then encloses its unique stationary point, a strict local maximum.
Unresolved boxes are bisected, visiting the side nearer the median first.
Overlapping root discoveries are merged only if the hull of their brackets
is itself strictly concave: two distinct roots there would contradict the
strict decrease of $G$. The finite partition therefore either excludes every
possible competitor or encloses it in a certified local-maximum bracket.
This argument uses rational-function bounds directly, without forming the
potentially degree-$2n-1$ score polynomial.

Root bisection requires bracket width at most $10^{-10}$ in scale units and
total dimensionless score $|G|<10^{-7}$. Because $|L''|\le2n$, a root
in a bracket of width $w$, with midpoint score below $10^{-7}$, has likelihood
at most $L(t)+10^{-7}w+nw^2$. Success requires one candidate's evaluated
likelihood to exceed every other candidate's upper bound by the comparison
margin below. Hence there is a unique winning exact root conditional on the
computed comparisons. The returned estimate approximates that root; it is
not asserted to be an exact floating-point stationary point. Conversion back
to the original location must retain the score gate and stay within $10^{-10}$
of the root bracket. A corresponding conservative returned-point likelihood
gap is $10^{-7}D+nD^2$, with $D=w+10^{-10}$.

**Ties:** separated global maxima can be real, for example at
$\mu=\pm\sqrt{a^2-\sigma^2}$ for observations $-a,a$ and
$\sigma<a$. The fitter returns `AmbiguousMaximum`, with no location estimate,
when the competing candidate likelihood intervals cannot be strictly ordered.
The binding stops with "global maxima are tied or numerically indistinguishable".
This reports ambiguity rather than claiming an exact tie from rounded numbers.
No arbitrary maximizer is selected. A degenerate maximum, such as the quartic
maximum for $(-1,1)$ at scale one, can instead exhaust subdivision and gives
`IterationFailure`. Neither status means the mathematical MLE fails to exist.

Compensated sums are used throughout this conditional search. With machine
epsilon $\epsilon$, the practical score and curvature comparison margin is
$128\epsilon n$; the likelihood comparison margin is
$512\epsilon n[1+2\log\operatorname{hypot}(1,\max y-\min y)]$.
The elementary-function evaluations and standardized coordinates are floating
point, not outward-rounded intervals. As elsewhere in the package, the proof
is in exact arithmetic conditional on the computed comparisons, with these
stated practical margins. Bisection sign updates are conditional computed
comparisons too, not rigorous interval-arithmetic evaluations near zero.

`iterlim` bounds subdivision depth and each root bisection; at most
`64*iterlim` boxes may be processed. Returned `iterations` counts processed
boxes for this held subset. Nonfinite transformations, standardized magnitudes
above $10^{150}$, lost location accuracy, missing strict brackets, exhausted
budgets or unresolved curvature cause numerical failure, never an unchecked
last iterate. Work is linear in sample size per box/score evaluation and memory
is linear in sample size plus stored boxes and roots. No uniform bound on the
number of boxes or successful certification for every input is claimed;
adversarially flat or crowded modes may correctly stop. Median-centred search
and likelihood pruning avoid the old observation-gap scan's quadratic growth.
The [validation report](../../../../../dev/research/evaluations/2026-11-04-cauchy-sturm-report.md)
records independent small-sample polynomial checks and timings through $n=10^5$.
