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
  method: profile-search
  certified: partial
  note: "cauchy/mle.hpp, fit_cauchy: The free fit uses safeguarded Newton in location and log scale, not a scalar profile search. Held location has a unique monotone scale equation, but the fixed lower bracket is unchecked and can miss its root; held-scale location scans have no root-isolation certificate. The balanced two-point joint maximum is explicit; general Newton step convergence is not a global certificate."
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

Holding location leaves a strictly decreasing scale score with one root exactly below the half-mass threshold. For extremely different nonzero residual magnitudes, the current numerical scale bracket can miss that root. Holding positive scale prevents collapse, but the location likelihood can have several modes and several global maxima. The conditional fit searches across observation gaps. The joint fit uses safeguarded Newton steps. Neither that joint iteration nor the conditional location search supplies a general global certificate.

## R notes

Supplying `location` or `scale` holds that coefficient. When scale is free and the applicable existence condition fails, the fit stops with a non-existence diagnostic rather than returning a collapsing scale.
