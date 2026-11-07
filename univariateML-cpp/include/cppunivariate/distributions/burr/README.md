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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
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
  certified: 'yes'
  note: 'burr/README.md, Details; burr/mle.hpp (public fitter and held overloads); burr/test.cpp: All-free fits use the global Burr certificate; held shape2 with free shape1 and scale uses certified Lomax on exp(shape2*(log(x)-max(log(x)))), with its exact Jacobian and Weibull face map. Held scale with both shapes free uses the strictly concave scalar-profile certificate, conditional on computed comparisons and the stated practical margin. Other shape holds use monotone scores or closed forms.'
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
    With only scale $s$ held, a unique finite maximum exists for nonconstant data if some observation is
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
solves stop with explicit failure. The global budget is `128 * min(iterlim, 100000)` cached profiles;
tail searches, root refinements and bracket solves are also bounded. Exhaustion
returns `CertificateBudget`; a sign-uncertain seam or unresolved tie returns
`CertificateTie`; other arithmetic/proposal failures return `NumericalFailure`.
No capped estimate is returned. Constant positive data return `Unbounded`.
The C++ `burr_mle_certified` entrypoint additionally accepts an explicit final
`certificate_budget` for reproducible stops. The public default `burr_mle`
uses this certified path. `burr_mle_legacy` remains for GB2 starts; it is not certified.

With only scale held at $s$ and nonconstant observations all at least $s$,
the supremum is the endpoint-$s$ Pareto log-likelihood minus $Z\log 2$,
where $Z$ counts observations equal to $s$. This endpoint penalty must be
retained when comparing likelihoods. If every observation equals $s$, the
likelihood is unbounded. For nonconstant data, if any observation is below $s$, a finite maximum
exists and is unique by strict concavity of the profiled power likelihood. Constant data below $s$
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

Supply `scale = s` to estimate only the shapes. The fixed-scale path profiles `shape1` exactly and certifies the unique
`shape2` maximum using strict scalar concavity and signed endpoint bounds. Starting
controls `shape10`, `shape20`, `scale0`, and historical BFGS tolerances
are retained for compatibility and validated on the all-free path. Detected
limits stop with a recommendation to fit `mlweibull` or `mlpareto`.
Certificate tie, budget and numerical failures have distinct error messages
and return no coefficient vector. The returned iteration count on the
all-free path is the number of attempted cached conditional profiles.
Held-$c$ inherits the separate Lomax certificate, with the Jacobian
$n\log c+(c-1)\sum\log x_i-cn\max\log x_i$ for normalized powers.
Every likelihood bound and candidate receives this shift. Underflow of a
positive power stops as a numerical failure. The held-scale two-shape profile has its own strict-concavity certificate below.

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


### Held-scale certificate (TASK-116.4)

Write $z_i=\log(x_i/s)$, $G(c)=\sum_i\log(1+e^{cz_i})$ and
$R(c)=\sum_i\log(1+e^{-cz_i})$. Profiling $k=n/G(c)$ gives

$$\ell_p(c)=n\log n-n-\sum_i\log x_i-n\log(G(c)/c)-R(c),\qquad c>0.$$

This profile is **strictly concave**. Here is an elementary proof rather than
an assumption of unimodality. For $u>0$, set $A(u)=\log(1+e^{-u})$ and
$p=(1+e^u)^{-1}$. The inequalities $p\le A\le e^{-u}$ imply
$1-p\le p/A\le1$ and

$$(\log A)''=(p/A)(1-p-p/A)\ge-p\ge-e^{-u}.$$

Consequently $(\log(A(u)/u))''\ge u^{-2}-e^{-u}>0$, since
$u^2e^{-u}\le4/e^2<1$. Thus $A(u)/u$ is strictly log-convex.
For positive arguments, $\log(1+e^u)/u=1+A(u)/u$ is also strictly
log-convex (sums of positive log-convex functions are log-convex, with
strictness here). For $z_i=0$, the summand is $\log2/c$.
It follows that $G(c)/c$ is strictly log-convex. As $R$ is convex,
$\ell_p''(c)<0$ for every finite $c>0$.

As $c\downarrow0$, $\ell_p\to-\infty$. For nonconstant data with a negative
$z_i$, it also tends to $-\infty$ as $c\to\infty$: if any $z_i\ge0$,
the negative observations supply the diverging negative linear term;
if every $z_i<0$, its linear coefficient is
$\sum_i z_i-n\max_i z_i<0$. There is therefore exactly one finite
maximizer. Constant samples at or below $s$ instead have unbounded
likelihood. If every observation is at least $s$ and some exceeds it,
put $S=\sum_i z_i>0$ and $Z=\#\{x_i=s\}$. The profile increases to
$n\log(n/S)-n-\sum_i\log x_i-Z\log2$, strictly below which every finite
fit lies. This is the endpoint-$s$ Pareto path; the $Z\log2$ deficit is
retained in stop likelihoods. Its endpoint must stay held. A Weibull face
requires changing scale, so cannot compete in this subset.

`scale_certificate.hpp` rescales logs by $U=\max_i|z_i|$ and solves in
$t=cU$. Doubling and bisection only propose a root. Acceptance requires
strictly positive and negative enclosed scores at two positive endpoints,
and their coefficient images for both $k$ and $c$ must have relative width
at most $10^{-6}$. The root proposal is refined to relative bracket width
$10^{-13}$ independently of the user tolerance. To preserve historical
held-scale coefficients without changing goldens, a successful historical
Newton proposal within $10^{-8}$ of that root may instead supply the returned
center. It must pass the same independent signed bracket and coefficient
width checks; its optimizer status never establishes globality. Strict concavity proves
that this bracket contains the unique global maximum. Concave tangents
supply an upper likelihood bound and the center a lower bound. `iterlim`
bounds proposal evaluations; exhaustion stops with `CertificateBudget`.
Unresolved endpoint signs or coefficient width stop with `CertificateTie`;
nonfinite arithmetic or invalid bounds stop with `NumericalFailure`.
No capped iterate is returned. The historical moment/Newton path is retained solely as a compatibility
proposal; alone it could stop at an unchecked iterate and supplied no global
certificate. Invalid or distant proposals are discarded.

The certificate is in exact arithmetic conditional on the computed
comparisons. Practical bounds reuse the all-free certificate allowance:
$64\epsilon(1+\max|a|)$ on elementary operations and transcendental
values, and $64\epsilon(m+1)(1+\sum|a_i|)$ on sums of $m$ terms.
They are practical floating-point margins, not outward-rounded interval
arithmetic or a universal libm error theorem. Close ratios use `log1p`;
otherwise differences of logs avoid ratio overflow. Very ill-conditioned
or extreme data may correctly stop when the margin prevents a decision.
The [validation report](../../../../../dev/research/evaluations/2026-11-06-burr-scale-report.md)
records independent comparisons, regression tolerances, and timings.
