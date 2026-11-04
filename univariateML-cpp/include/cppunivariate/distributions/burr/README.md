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
    gradtol: Positive legacy gradient tolerance; all-free certification uses fixed conservative enclosures.
    iterlim: Maximum proposal iterations (at most 120 per bracketed solve); the global certificate has a separate 4096-profile budget.
    shape10: Optional positive historical starting shape1; checked for compatibility.
    shape20: Optional positive historical starting shape2; checked for compatibility.
    scale0: Optional positive historical starting scale; checked for compatibility.
    reltol: Positive legacy relative objective tolerance; retained for compatibility with held and dependent fits.
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
    numerical: true
  - model: 'weibull'
    firmness: proved
    when: The shape $k$ tends to infinity while $s k^{-1/c}$ tends to a positive Weibull scale.
    detect: All-free search strictly excludes every finite profile interval using dual/chord bounds, small-k exclusion and a signed seam bound relative to the exact Weibull optimum. Computed enclosures use the stated practical arithmetic margin.
    status: 'WeibullBoundary'
  - model: 'pareto'
    firmness: proved
    when: The power $c$ tends to infinity while $kc$ tends to a positive Pareto shape and $s$ stays at its endpoint.
    detect: All-free search strictly excludes every finite profile interval using dual/chord bounds, small-k exclusion and a signed or unsigned Weibull tail bound. Computed enclosures use the stated practical arithmetic margin.
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
  method: profile-search
  certified: partial
  note: 'The all-free fit is certified in exact arithmetic conditional on the computed comparisons, with the practical floating-point margin described below. Root boxes and negative profile curvature isolate the winner; dual/chord envelopes and both tail exclusions establish globality. Unresolved searches stop. Held subsets keep their separate algorithms; the held-power Weibull comparison is numerical, hence the aggregate tag is partial.'
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
    For nonconstant data, a finite MLE exists if and only if some finite point
    reaches the larger of the optimized Pareto and Weibull log likelihoods
    (equality is included). These are the only likelihood-retaining escape
    faces. At least half the sample at its minimum precludes a finite MLE.
    General joint uniqueness is not established; a successful certificate
    proves uniqueness for that sample. Exact finite/face ties can stop unresolved.
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
    The joint certificate separately combines compact exclusion, root isolation
    and the two face bounds; conditional arguments alone do not establish it.
---

The Burr type XII distribution is a heavy-tailed model for positive size and loss data.
The scale $s$ sets the measurement scale. The shapes $k$ and $c$ control the
lower and upper tails. The upper-tail exponent is $kc$,
and the lower-tail power is $c$.

## Details

The all-free certificate requires at least two positive finite observations;
constant samples have unbounded likelihood. Held subsets retain their
existing sample-size contracts. The
[family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-genbeta.md)
describes the limiting paths and the scope of the boundary comparisons.
The all-free fit uses the certificate in
[the root-box derivation](../../../../../dev/research/evaluations/2026-11-04-burr-root-data/derivation.md)
and [the closure analysis](../../../../../dev/research/evaluations/2026-11-03-burr-closure-report.md).
Its implementation is [certificate.hpp](certificate.hpp). All likelihoods
inside the certificate are total sample log likelihoods with a common,
explicitly restored log-data normalization constant. The Hessian is a
likelihood Jacobian used for optimization, not Fisher information.

For fixed $k$, the likelihood is strictly concave in $(c,b)$, where
$b=-c\log s$. A bracketed intercept solve and a bracketed, decreasing
profile-power score supply proposals. Safeguarded Newton steps remain inside
those brackets. A 41-point logarithmic reconnaissance proposes stationary
maxima, refined by analytic three-score Newton steps. Reconnaissance need
not find every root: a missed competing root prevents global exclusion.
User starts are validated positive finite values; the certificate's mandatory
reconnaissance does not depend on the historical BFGS starting controls.

A preconditioned fixed-point map encloses a unique three-score root in a
small box. A larger product box contains the unique conditional nuisance
maximum throughout a $k$ interval. A congruence enclosure of the negative
Hessian proves negative profile curvature there; fixed-$k$ concavity alone
would not suffice. Root boxes translate original log observations by the proposed log scale,
propagating that exact affine change through the enclosures. Standardized
coordinates or a large log offset can make the rectangular curvature extension
unnecessarily loose.
Outside the winning neighborhood, an entropy dual with one weight defined
exactly through the sum constraint bounds each fixed-$k$ optimum. Convex
chord envelopes bound intervals. Every remaining interval must be strictly
below a feasible lower bound, or is subdivided; there is no likelihood tie
preference or local-score rescue.

If $m$ observations equal the sample minimum, the small-$k$ theorem excludes
$0<k\le m/(n-m)$ against Pareto. The unsigned Weibull tail bound is
$F(k)\le W+n^2(k+1)/[2(k+1-n)^2]$, for $k>n-1$.
When the enclosed seam slope $A=\sum v_i^2/2-n$ is strictly negative,
the signed seam theorem instead excludes all $k\ge1/h$ against the same
exact Weibull optimum $W$. Its power root is bracketed and propagated into
all fitted exponentials and seam constants. A slope enclosure containing
zero supplies no signed-tail decision. A finite improvement can still use
the unsigned bound. Without either route the fit stops unresolved.

Certification is **in exact arithmetic, conditional on the computed
comparisons, with a stated practical floating-point margin**. Each interval
operation is enlarged by $64\epsilon(1+\max(|l|,|u|))$;
an $n$-term sum uses $64\epsilon(n+1)(1+\sum_i\max(|l_i|,|u_i|))$,
where $\epsilon=2^{-52}$. The elementary libm assumption is absolute error
at most $8\epsilon\max(1,|f|)$ for exp, log, softplus, sigmoid and entropy.
Root containment and curvature require a further 0.1 norm margin. These
allowances are practical assumptions, not a universal libm theorem or
outward-rounded interval proof. The golden root has independent exact
rational evidence in the linked research work; that evidence does not make
every production run an exact rational proof.

Invalid domains, failed brackets, nonfinite evaluations and failed linear
solves stop with explicit failure. The global budget is 4096 cached profiles;
tail searches, root refinements and bracket solves are also bounded. Exhaustion
returns `CertificateBudget`; a sign-uncertain seam or unresolved tie returns
`CertificateTie`; other arithmetic/proposal failures return `NumericalFailure`.
No capped estimate is returned. Constant positive data return `Unbounded`.
The C++ `burr_mle_certified` entrypoint additionally accepts an explicit final
`certificate_budget` for reproducible stops. The public default `burr_mle`
uses this certified path. `burr_mle_legacy` remains for InvBurr, Pareto IV and
GB2 starts, preserving those families' historical results; it is not certified.

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
controls `shape10`, `shape20`, `scale0`, and historical BFGS tolerances
are retained for compatibility and validated on the all-free path. Detected
limits stop with a recommendation to fit `mlweibull` or `mlpareto`.
Certificate tie, budget and numerical failures have distinct error messages
and return no coefficient vector. The returned iteration count on the
all-free path is the number of attempted cached conditional profiles.
Held-$c$ and held-scale paths keep their historical limit diagnostics; their
numerical comparisons do not acquire the all-free certificate.

## Decision history

univariateML 1.x estimated only the shapes, with scale fixed at 1.
The current default also estimates scale. The fixed-scale limit retains
endpoint $s$, including the likelihood penalty at observations equal to it.

On 2026-11-04 (TASK-138), the all-free BFGS comparison was replaced by root
boxes plus global exclusion. The previous $10^{-8}n$ face preference could
reject a better finite maximum and is deliberately absent here. The signed
seam bound avoids the large-$k$ cost of an unsigned bound near Weibull.
The earlier quadrature curvature diagnostic had incorrect signs on two
retained workloads and is not used. See the linked derivation and
[port validation](../../../../../dev/research/evaluations/2026-11-04-burr-port-report.md)
for the retained decisions, workload outcomes and timing evidence.
