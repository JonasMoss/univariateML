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
  density: stats::dcauchy
  sampler: stats::rcauchy
  control:
    scale: Optional held scale; NULL estimates it.
    location: Optional held location; NULL estimates it.
    abstol: Absolute step tolerance in (location, log(scale)).
    iterlim: Maximum number of iterations.
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
  certified: "partial"
  note: "cauchy/mle.hpp, fit_cauchy; Details: the free fit is certified by strict geodesic convexity under the strict half-mass condition, conditional on computed comparisons and the final dimensionless total-score norm < 1e-3. Balanced two-point maxima are explicit. Held location is certified by a verified monotone-score bracket and bisection. Held-scale location scans remain uncertified; hence the aggregate tag is partial."
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

Holding location leaves a strictly decreasing scale score with one root exactly below the half-mass threshold. Holding positive scale prevents collapse, but the location likelihood can have several modes and several global maxima. The conditional fit searches across observation gaps. The joint fit uses safeguarded Newton steps with the certificate below. The conditional location search has no general global certificate.

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
not an outward-rounded interval guarantee. The held-scale search is unchanged.
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
