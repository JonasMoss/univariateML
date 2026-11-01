---
id: burr
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Burr
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: shape2
  symbol: 'c'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: 's'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dburr
  sampler: rburr
  control:
    shape2: Known positive finite coefficient; NULL estimates it.
    shape1: Known positive finite coefficient; NULL estimates it.
    abstol: Absolute Newton step tolerance in `log(shape2)` when scale is fixed.
    gradtol: Gradient tolerance for the mean profiled negative log likelihood.
    iterlim: Maximum BFGS iterations per start.
    shape10: Optional positive starting shape1; `NULL` uses automatic starts.
    shape20: Optional positive starting shape2; `NULL` uses automatic starts.
    scale0: Optional positive starting scale; `NULL` uses automatic starts.
    reltol: Relative objective tolerance for BFGS.
log_density: log(shape1) + log(shape2) - log(x) - shape2*log(scale/x) - (shape1+1)*log(1+(x/scale)**shape2)
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The free power $c$ tends to infinity while the free scale follows a concentrating path at the common observation. With scale held, concentration is also possible at a common observation below $s$ by letting $k$ grow, or at $s$ itself.
    detect: All observations are equal and $c$ and $s$ are free. With $s$ held, $c$ is free and either every observation equals $s$, or both shapes are free and the common observation is below $s$.
    status: Unbounded
    message: unbounded
  - model: weibull
    firmness: proved
    fixed: [shape2]
    when: With $c$ held, $k$ tends to infinity and $s k^{-1/c}$ stays at a positive Weibull scale.
    detect: With $c$ held, the numerical comparison for the transformed observations selects the exponential limit of the Lomax likelihood. This is not an exact sample criterion.
    status: WeibullBoundary
  - model: 'weibull'
    firmness: proved
    when: The shape $k$ tends to infinity while $s k^{-1/c}$ tends to a positive Weibull scale.
    detect: A numerical comparison finds no candidate finite fit improving on this limit by more than $10^{-8}n$ log-likelihood units. This comparison is not a global certificate.
    status: 'WeibullBoundary'
  - model: 'pareto'
    firmness: proved
    when: The power $c$ tends to infinity while $kc$ tends to a positive Pareto shape and $s$ stays at its endpoint.
    detect: A numerical comparison finds no candidate finite fit improving on this limit by more than $10^{-8}n$ log-likelihood units. This comparison is not a global certificate.
    status: 'ParetoBoundary'
  - model: 'pareto'
    firmness: proved
    fixed: [scale]
    when: The power $c$ tends to infinity while $kc$ tends to a positive Pareto shape and $s$ stays at its endpoint.
    detect: Only scale is held and all observations are at least $s$. The all-equal-to-$s$ case has unbounded likelihood instead.
    status: 'AllGeOne'
references:
- Shao, Q. (2004). Notes on maximum likelihood estimation for the three-parameter Burr XII distribution. Computational
  Statistics & Data Analysis 45, 675-687. doi:10.1016/S0167-9473(02)00367-5.
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, Chapter 20.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlburr())
  mlburr(x)

group: beta and generalised beta
construction:
- base
- generalisation
relations:
  special_case_of:
  - family: gb2
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: scale
  - family: pareto4
    at:
      shape1: shape1
      shape2: shape2
      scale: scale
      min: 0
  transform_of:
  - family: lomax
    at:
      lambda: 1/scale**shape2
      kappa: shape1
    transform: power
    power: shape2
fit:
  method: multistart
  certified: partial
  note: 'burr/README.md, Details; burr/mle.hpp (public fitter and held overloads); burr/test.cpp: One-free-coordinate
    and transformed Lomax subsets have monotone scores; free shapes/scale use numerical profiles or BFGS. Reciprocal
    delegation inherits these qualifications.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance severity and size data needing a flexible polynomial upper tail.
summary: Burr provides a model for insurance severity and size data needing a flexible polynomial upper tail.
aliases:
- "Burr type XII"
- "Singh–Maddala"
- "beta-P"
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant positive data have unbounded likelihood.
    For nonconstant data, the Weibull and Pareto limits can compete with finite
    fits. A complete joint sample criterion and joint uniqueness are not established.
    With only scale $s$ held, a finite maximum exists for nonconstant data if some observation is
    below $s$. Constant data below $s$ have unbounded likelihood.
    If all observations are at least $s$, the supremum is on the
    Pareto path, except that all observations equal to $s$ give unbounded likelihood.
    Holding $k$ gives a unique finite fit for nonconstant data, but constant data
    remain unbounded when $c$ and $s$ are free. Holding $c$ reduces existence to
    the Lomax criterion on $x_i^{c}$, with a possible Weibull limit.
    Holding both shapes gives a unique scale estimate for any positive sample.
    Holding $k$ and $s$ gives a unique finite $c$ estimate unless all observations
    equal $s$. Holding $c$ and $s$ gives a unique finite $k$ estimate.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    With $k$ held, the log-data likelihood is strictly concave in
    $c$ and $c\log s$, with coercive profiles for nonconstant samples.
    With $c$ held, a power transformation gives a Lomax likelihood.
    With $s$ held, profiling $k$ gives a one-dimensional likelihood whose
    endpoints yield the stated existence and Pareto conditions.
    The single-coordinate scores are strictly decreasing in suitable coordinates.
    These conditional arguments do not certify the joint numerical search.
---

The Burr type XII distribution is a heavy-tailed model for positive size and loss data.
The scale $s$ sets the measurement scale. The shapes $k$ and $c$ control the
lower and upper tails. The upper-tail exponent is $kc$,
and the lower-tail power is $c$.

## Details

Fitting requires a nonempty positive finite sample. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md)
describes the limiting paths and the scope of the boundary comparisons.
The joint fit uses several numerical starts and compares candidate fits with Weibull and Pareto limits. These comparisons do not constitute a global certificate.

With only scale held at $s$ and nonconstant observations all at least $s$,
the supremum is the endpoint-$s$ Pareto log-likelihood minus $Z\log 2$,
where $Z$ counts observations equal to $s$. This endpoint penalty must be
retained when comparing likelihoods. If every observation equals $s$, the
likelihood is unbounded. For nonconstant data, if any observation is below $s$, a finite maximum
exists, although uniqueness is not established here. Constant data below $s$
instead have unbounded likelihood: $k$ can grow as $c$ tends to infinity,
concentrating the distribution at that common value even with scale held.

For constant observations $x_0<s$, put $z=(x_0/s)^c$ and
profile $k=1/\log(1+z)$. The density at $x_0$ is
$\{c/(e x_0)\}z/\{(1+z)\log(1+z)\}$, asymptotic to $c/(e x_0)$.
Thus holding scale alone does not remove constant-sample collapse below it.

Holding $c$ gives a Lomax likelihood on $x^{c}$, whose exponential
limit becomes Weibull with that power held. Its existence criterion is the
one in the [Lomax dossier](../lomax/README.md), applied to the transformed
observations. Holding $k$ instead gives a strictly concave log-data likelihood
in inverse spread and standardized location for nonconstant samples.
With $k$ and $s$ held, a free $c$ is unbounded only when every observation
equals $s$. Holding $c$ and $s$ leaves an explicit estimate of $k$.

## R notes

Supply `scale = s` to estimate only the shapes. This fits the historical
scale-1 family to `x / s` and subtracts `length(x) * log(s)` from its
log-likelihood. The fixed-scale path profiles `shape1` and refines `shape2`
by Newton iteration, with a profile-likelihood fallback start. Starting
controls `shape10`, `shape20`, `scale0`, and BFGS tolerances apply only to
the free-scale path. Detected limits stop with a recommendation to fit
`mlweibull` or `mlpareto`.
A limit recommendation from a numerical comparison is not an exact sample criterion.
The free-scale comparisons prefer a limit within $10^{-8}n$ log-likelihood
units, so a tied or very slightly better finite maximum can be rejected.

## Decision history

univariateML 1.x estimated only the shapes, with scale fixed at 1.
The current default also estimates scale. The fixed-scale limit retains
endpoint $s$, including the likelihood penalty at observations equal to it.
