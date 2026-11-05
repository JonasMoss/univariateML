---
id: betapr
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Beta prime
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: shape1
  symbol: 'p'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: 'q'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: scale
  symbol: 'b'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
r:
  density: dbetapr
  sampler: rbetapr
  control:
    shape1: Held positive finite coefficient; NULL estimates it.
    shape2: Held positive finite coefficient; NULL estimates it.
    gradtol: Tolerance on the total gradient norm in log coordinates after dividing data by their geometric mean; with fixed scale, relative Newton step tolerance on log(shape2).
    reltol: Relative objective tolerance.
    abstol: Step tolerance in log coordinates after dividing data by their geometric mean.
    shape10: Optional positive starting value for shape1 (zero selects automatic starts).
    shape20: Optional positive starting value for shape2 (zero selects automatic starts).
    scale0: Optional positive starting value for scale (zero selects automatic starts).
    iterlim: Maximum number of iterations.
log_density: (shape1-1)*log(x/scale) - log(scale) - (shape1+shape2)*log(1+x/scale) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
limits:
  - {firmness: proved, model: exp, when: 'With shape1 held at one, shape2 and scale diverge at fixed ratio.', status: ExpBoundary, fixed: [shape1], numerical: true}
  - {firmness: proved, model: invexp, when: 'With shape2 held at one, shape1 diverges and scale vanishes at fixed product.', status: InvExpBoundary, fixed: [shape2], numerical: true}
  - firmness: proved
    name: positive log-variance
    when: Both free shapes $p$ and $q$ tend to infinity with their ratio and scale chosen to concentrate at the common observation.
    detect: All observations are equal and both shapes are free.
    status: InvalidStart
  - firmness: proved
    model: gamma
    fixed: [shape1]
    when: With $p$ held, $q$ and $b$ tend to infinity while $b/q$ tends to a positive gamma scale.
    detect: A numerical comparison selects the gamma limit within $10^{-8}n$ log-likelihood units. Constant data with only $p$ held select this limit analytically.
    status: GammaBoundary
    numerical: true
  - firmness: proved
    model: invgamma
    fixed: [shape2]
    when: With $q$ held, $p$ tends to infinity and $b$ tends to zero while $bp$ tends to a positive inverse-gamma scale.
    detect: A numerical comparison selects the inverse-gamma limit within $10^{-8}n$ log-likelihood units. Constant data with only $q$ held select this limit analytically.
    status: InvGammaBoundary
    numerical: true
  - model: gamma
    firmness: proved
    when: The shape $q$ tends to infinity while $p$ tends to a positive finite shape and $b/q$ tends to a positive gamma scale.
    detect: A numerical comparison selects the gamma limit within $10^{-8}n$ log-likelihood units. This comparison is not a global certificate.
    status: GammaBoundary
    numerical: true
  - model: invgamma
    firmness: proved
    when: The shape $p$ tends to infinity while $q$ tends to a positive finite shape and $bp$ tends to a positive inverse-gamma scale.
    detect: A numerical comparison selects the inverse-gamma limit within $10^{-8}n$ log-likelihood units. This comparison is not a global certificate.
    status: InvGammaBoundary
    numerical: true
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25.
  Wiley, New York.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
examples: |
  set.seed(1)
  x <- rml(200, mlbetapr())
  mlbetapr(x)

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: gb2
    at:
      shape1: shape2
      shape2: 1
      shape3: shape1
      scale: scale
fit:
  method: multistart
  certified: partial
  note: 'betapr/README.md, Details; betapr/mle.hpp (public fitter and held overloads); betapr/test.cpp: Held shape1=1 reduces exactly to certified Lomax and held shape2=1 to inverse Pareto, including all supported further holds and transformed stop bounds. Fixed-scale beta fitting is concave. The joint three-parameter fit and one-held non-unit shape profiles remain numerical.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model positive ratios with separate lower-tail and upper-tail shapes.
summary: Beta prime provides a model for positive ratios with separate lower-tail and upper-tail shapes.
aliases:
- "beta of the second kind"
- "inverted beta"
- "scaled beta prime"
- "scaled F"
- "Pearson type VI"
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant positive data have unbounded likelihood.
    For nonconstant data the gamma and inverse-gamma limits can compete with
    finite fits. A complete joint sample criterion and joint uniqueness are
    not established. With scale held, a unique finite MLE exists exactly when
    the sample is nonconstant if both shapes are free. With scale and either
    shape held, the remaining shape has a unique finite maximum even for
    constant data. With both shapes held, scale has a unique finite maximum
    for every positive sample. With just $p$ held, the gamma limit remains;
    with just $q$ held, the inverse-gamma limit remains. Constant samples
    select these respective limits, and a general sample criterion and
    uniqueness for these two conditional problems are not established.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    With scale $b$ held, $y_i=x_i/(b+x_i)$ transforms the likelihood to a
    beta likelihood with a data-only Jacobian. Strict concavity in the beta
    natural parameters gives uniqueness, and nonconstant interior data give
    existence. A single beta shape has a strictly decreasing digamma score
    that changes sign. With both shapes held, the log-scale score strictly
    decreases from $nq$ to $-np$, so it has one root. Gamma-ratio asymptotics
    establish the two limiting densities, but do not decide whether either
    dominates all finite fits.
---

The beta prime distribution models positive ratios. The scale $b$ sets the
measurement scale. The shape $p$ controls the lower-tail power and $q$
controls the polynomial upper tail.

## Details

Fitting requires a nonempty positive finite sample. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-halfthreshold.md)
describes the limiting paths and boundary comparisons.
With $p$ fixed, $q\to\infty$ and $b/q\to\theta>0$ give a gamma density
with shape $p$ and scale $\theta$. With $q$ fixed, $p\to\infty$ and
$pb\to\theta>0$ give an inverse-gamma density with shape $q$ and scale
$\theta$. These follow from $\Gamma(p+q)/\Gamma(q)\sim q^p$ in the first
case and the corresponding ratio with $p$ and $q$ interchanged in the second.
The pointwise density limits for $x>0$ imply total-variation convergence by
Scheffé's lemma. They do not certify which limit wins a likelihood comparison.

Holding scale excludes these nondegenerate escapes. The transformation
$y=x/(b+x)$ gives a beta likelihood, with a unique finite maximum for
nonconstant data. Constant data instead have unbounded likelihood as both
shapes grow with their ratio chosen to concentrate at the observation.
At held scale, sending only $q$ to infinity concentrates mass at zero;
sending only $p$ to infinity sends mass to infinity. Sending both to infinity
with finite positive ratio gives a point mass, rather than gamma or inverse gamma.

At held scale, holding one shape leaves a strictly decreasing digamma score.
Holding both shapes gives a strictly decreasing log-scale score with a unique
root, even for constant data. Holding just one shape leaves the corresponding
gamma or inverse-gamma escape. The limit comparison retains that shape and
optimizes the limit scale. For constant data, profiling scale gives a likelihood
strictly increasing in the remaining shape, so the supremum is the limit.
For example, with $p$ held its derivative per observation is
$\psi(p+q)-\psi(q)-\log((p+q)/q)>0$.
The inequality follows by integrating $\psi'(t)>1/t$ from $q$ to $q+p$.
Reciprocation proves the other case.

The joint fit compares numerical candidate maxima against gamma and
inverse-gamma limits. With one shape held, the remaining shape is profiled
at each scale and a log-scale grid refines sampled maxima. This search is
numerical and does not certify global optimality. An unresolved numerical
edge produces failure rather than an unchecked estimate.

## R notes

Supply `scale` to hold it at a positive finite value. Either shape may also
be held with `shape1` or `shape2`. Limit comparisons within $10^{-8}n$
log-likelihood units select the escaping model and stop with a recommendation
to use `mlgamma` or `mlinvgamma`. This numerical preference can reject a finite
maximum tied with or only slightly better than a limit.

## Decision history

univariateML 1.x estimated only the shapes, with scale fixed at 1.
On 2026-10-30 (TASK-125), identical observations with exactly one held shape
were assigned directly to the appropriate gamma or inverse-gamma limit using
the digamma inequality above. This avoids cancellation in the finite grid comparison.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).
