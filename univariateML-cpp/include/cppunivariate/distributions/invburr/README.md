---
id: invburr
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Inverse Burr
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
  density: dinvburr
  sampler: rinvburr
  control:
    shape2: Known positive finite coefficient; NULL estimates it.
    shape1: Known positive finite coefficient; NULL estimates it.
    abstol: Absolute Newton step tolerance in `log(shape2)` when scale is fixed.
    gradtol: Positive delegate tolerance; the all-free certificate uses deterministic enclosure widths.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
    shape10: Optional positive starting shape1; `NULL` uses automatic starts.
    shape20: Optional positive starting shape2; `NULL` uses automatic starts.
    scale0: Optional positive starting scale; `NULL` uses automatic starts.
    reltol: Positive legacy relative tolerance, validated for compatibility by the all-free certificate.
log_density: log(shape1) + log(shape2) - log(x) + shape2*log(scale/x) - (shape1+1)*log(1+(scale/x)**shape2)
limits:
  - name: unbounded likelihood
    firmness: proved
    when: The free power $c$ tends to infinity while the free scale follows a concentrating path at the common observation. With scale held, concentration is also possible at a common observation above $s$ by letting $k$ grow, or at $s$ itself.
    detect: All observations are equal and $c$ and $s$ are free. With $s$ held, $c$ is free and either every observation equals $s$, or both shapes are free and the common observation is above $s$.
    status: Unbounded
    message: unbounded
  - model: invweibull
    firmness: proved
    fixed: [shape2]
    when: With $c$ held, $k$ tends to infinity and $s k^{1/c}$ stays at a positive inverse Weibull scale.
    detect: With $c$ held, the certified Lomax comparison for the transformed observations selects the exponential limit of the Lomax likelihood. This is not an exact sample criterion.
    status: InvWeibullBoundary
    numerical: true
  - model: 'invweibull'
    firmness: proved
    when: The shape $k$ tends to infinity while $s k^{1/c}$ tends to a positive inverse Weibull scale.
    detect: With all coefficients free, the reciprocal Burr certificate strictly excludes every finite competitor and the other face; unresolved comparisons retain a successful finite search as uncertified.
    status: 'InvWeibullBoundary'
    numerical: true
  - model: 'power'
    firmness: proved
    when: The power $c$ tends to infinity while $kc$ tends to a positive power shape and $s$ stays at its endpoint.
    detect: With all coefficients free, the reciprocal Burr certificate strictly excludes every finite competitor and the other face; unresolved comparisons retain a successful finite search as uncertified.
    status: 'PowerBoundary'
    numerical: true
  - model: 'power'
    firmness: proved
    fixed: [scale]
    when: The power $c$ tends to infinity while $kc$ tends to a positive power shape and $s$ stays at its endpoint.
    detect: Only scale is held and all observations are at most $s$. The all-equal-to-$s$ case has unbounded likelihood instead.
    status: 'AllLeOne'
references:
- Shao, Q. (2004). Notes on maximum likelihood estimation for the three-parameter Burr XII distribution. Computational
  Statistics & Data Analysis 45, 675-687. doi:10.1016/S0167-9473(02)00367-5.
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, Chapter 20.
  Wiley, New York.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlinvburr())
  mlinvburr(x)

group: beta and generalised beta
construction:
- transformation
- generalisation
relations:
  transform_of:
  - family: burr
    transform: inverse
    at:
      shape1: shape1
      shape2: shape2
      scale: 1/scale
  - family: invpareto
    at:
      shape: shape1
      scale: scale**shape2
    transform: power
    power: shape2
  special_case_of:
  - family: gb2
    at:
      shape1: 1
      shape2: shape2
      shape3: shape1
      scale: scale
fit:
  method: delegation
  certified: partial
  note: 'Reciprocal Burr and Lomax fits transport the likelihood Jacobian, inverse-Weibull/power faces and per-fit certification flag. Raw log-ratio refinement checks the physical held-scale root; the held-first-shape location fit has its own flag.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model insurance loss severity with the reciprocal Burr tail pattern.
summary: Inverse Burr provides a model for insurance loss severity with the reciprocal Burr tail pattern.
aliases:
- "Burr type III"
- "Dagum"
- "Dagum type I"
- "beta-K"
- "three-parameter kappa"
key_references:
- 3
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant positive data have unbounded likelihood.
    For nonconstant data, the inverse Weibull and power limits can compete with finite
    fits. Each certified finite fit is unique and global for that sample;
    each accepted limit is certified globally optimal. A universal joint sample
    criterion and universal joint uniqueness theorem are not established.
    With only scale $s$ held, a unique finite maximum exists for nonconstant data if some observation is
    above $s$. Constant data above $s$ have unbounded likelihood.
    If all observations are at most $s$, the supremum is on the
    power path, except that all observations equal to $s$ give unbounded likelihood.
    Holding $k$ gives a unique finite fit for nonconstant data, but constant data
    remain unbounded when $c$ and $s$ are free. Holding $c$ reduces existence to
    the Lomax criterion on $x_i^{-c}$, with a possible inverse Weibull limit.
    Holding both shapes gives a unique scale estimate for any positive sample.
    Holding $k$ and $s$ gives a unique finite $c$ estimate unless all observations
    equal $s$. Holding $c$ and $s$ gives a unique finite $k$ estimate.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    Reciprocation transports the Burr arguments and changes likelihood only by a data-dependent Jacobian. With $k$ held, the log-data likelihood is strictly concave in
    $c$ and $c\log s$, with coercive profiles for nonconstant samples.
    With $c$ held, a power transformation gives a Lomax likelihood.
    With $s$ held, profiling $k$ gives a one-dimensional likelihood whose
    endpoints yield the stated existence and power conditions.
    The single-coordinate scores are strictly decreasing in suitable coordinates.
    The all-free search additionally certifies each accepted outcome through the reciprocal Burr certificate; a universal joint sample criterion remains unknown.
---

The inverse Burr distribution is the distribution of the reciprocal of a Burr type XII variable. It models positive size and loss data.
The scale $s$ sets the measurement scale. The shapes $k$ and $c$ control the
lower and upper tails. The upper-tail exponent is $c$,
and the lower-tail power is $kc$.

## Details

Fitting requires a nonempty positive finite sample. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md)
describes the limiting paths and the scope of the boundary comparisons.
The fit delegates to Burr on reciprocal observations, mapping a held scale to its reciprocal.

With only scale held at $s$ and nonconstant observations all at most $s$,
the supremum is the endpoint-$s$ power log-likelihood minus $Z\log 2$,
where $Z$ counts observations equal to $s$. This endpoint penalty must be
retained when comparing likelihoods. If every observation equals $s$, the
likelihood is unbounded. For nonconstant data, if any observation is above $s$, a finite maximum
exists and is unique by the reciprocal strict-concavity profile argument. Constant data above $s$
instead have unbounded likelihood: $k$ can grow as $c$ tends to infinity,
concentrating the distribution at that common value even with scale held.

The constant-sample concentrating path above a held scale is the reciprocal
of the path below a held Burr scale. The explicit profiled-density calculation
in the [Burr dossier](../burr/README.md) proves that its likelihood is unbounded.

Holding $c$ gives a Lomax likelihood on $x^{-c}$, whose exponential
limit becomes inverse Weibull with that power held. Its existence criterion is the
one in the [Lomax dossier](../lomax/README.md), applied to the transformed
observations. Holding $k$ instead gives a strictly concave log-data likelihood
in inverse spread and standardized location for nonconstant samples.
With $k$ and $s$ held, a free $c$ is unbounded only when every observation
equals $s$. Holding $c$ and $s$ leaves an explicit estimate of $k$.

## R notes

Supply `scale = s` to estimate only the shapes. The fixed-scale path reciprocates observations and scale, then delegates
to the certified Burr scalar profile, including its reciprocal Jacobian.
The Newton proposal becomes certified only after the independent
root-bracket and coefficient checks; an unclosed check can retain it uncertified. Starting
controls `shape10`, `shape20`, `scale0` and positive tolerances remain
accepted and validated for compatibility. The all-free certificate uses its
own deterministic proposals and enclosure widths. Detected limits stop with
`mlinvweibull` or `mlpower`; unresolved Burr comparisons retain the converged finite search and transport
its `certified=false` flag through the reciprocal map.

## Decision history

univariateML 1.x estimated only the shapes, with scale fixed at 1.
The current default also estimates scale. The fixed-scale limit retains
endpoint $s$, including the likelihood penalty at observations equal to it.

## Certified joint delegation (2026-11-04, TASK-116)

For $y_i=1/x_i$, $k$ and $c$ are unchanged and $s_x=1/s_y$.
The log likelihood is $\ell_x=\ell_y-2\sum_i\log x_i$; this additive,
parameter-independent Jacobian preserves strict comparisons and maximizers.
The Weibull face becomes inverse Weibull with shape $c$ and scale $1/s_y$;
the Pareto face becomes power with endpoint $1/\min y_i=\max x_i$ and
shape equal to the Pareto tail exponent. Boundary result fields carry the
transformed limiting scale and checked likelihood, with $k=\infty$ or
$(k,c)=(0,\infty)$ as sentinels, rather than finite Burr coefficients.
No secondary face optimizer can override the certificate.

The certificate is exact-arithmetic conditional on its computed comparisons,
with the practical floating-point margins and libm assumptions in the
[Burr dossier](../burr/README.md#details) and
[production certificate report](../../../../../dev/research/evaluations/2026-11-04-burr-port-report.md).
It is not an outward-rounded interval guarantee. Reciprocal or excess
observations must be representable in binary64; nonfinite transformed
coefficients or likelihoods cannot become successful fits. Ties (including
sign-uncertain seams), the `128 * min(iterlim, 100000)` profile budget and arithmetic failures have
separate diagnostics and never return unchecked estimates. Joint and held routes
preserve the delegate's per-fit certification flag. Held power inherits the Lomax certificate; held scale with both shapes free inherits the strict-concavity scalar certificate.
The general joint uniqueness theorem remains open; the certificate proves
uniqueness only for each certified finite sample.

This replaces the former multi-start/near-face comparison for the joint fit.
The [dependency report](../../../../../dev/research/evaluations/2026-11-04-burr-deps-report.md)
records transformation checks, unchanged goldens, timings and final validation.

The certified transformation rejects arithmetic overflow and any merging of
distinct observations into equal transformed values with `NumericalFailure`.
This prevents rounded transformations from supplying artificial constant-sample
or tied-endpoint certificates. Genuine data ties remain allowed.
The power-limit endpoint is taken from the original sample maximum, avoiding
a one-ulp support error from a reciprocal round trip. Its likelihood still
transports the certificate value with the data-only Jacobian.


The held-scale two-shape reduction now inherits the
[Burr scalar concavity certificate](../burr/README.md#held-scale-certificate-task-1164),
including strict endpoint score comparisons, coefficient accuracy checks,
practical floating-point margins and the delegate's uncertified fallback.
Reciprocation maps the held-scale Pareto face to a power face and adds the reciprocal Jacobian to likelihood bounds. Direct fixed-scale and held-subset entrypoints share this path.

Arithmetic that rounds a distinct observation onto a held scale stops numerically: reciprocation checks equality against the original scale, and threshold subtraction checks its error-free sum residual. This protects the exact endpoint classification and its tied-observation penalty.

With first shape held, the fitter instead works directly on negative centered
log ratios of the original observations. Strict concavity in power and power
times log scale gives a unique root without rounding reciprocals. Scale
restoration uses expm1, and its represented likelihood is checked against the
latent optimum. With scale held and both shapes free, successful reciprocal
certificates are refined against the original log-ratio score, removing the
historical proposal's looser coefficient tolerance.

The held-first-shape route retains finite estimates with an explicit uncertified
flag when the binary64 scale grid prevents transfer of its latent likelihood
certificate. The reported likelihood still evaluates the returned coefficients.
