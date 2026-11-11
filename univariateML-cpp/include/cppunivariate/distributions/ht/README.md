---
id: ht
inference_numeric:
  - regime: "known threshold"
    available: true
    condition: "finite mu held, nu > 0 and sigma > 0; nu and/or sigma free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels"
  - regime: unreviewed or unavailable
    available: false
    condition: "threshold free"
    reason: "a free threshold changes support with positive endpoint density and is non-DQM"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support"
name: Half-t
support: {type: real, lower: "mu", upper: inf, closed: [true, false], endpoint_log_density: {lower: "log(2) + lgamma((nu+1)/2) - lgamma(nu/2) - log(nu*pi)/2 - log(sigma)"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: threshold
  advice: Holding mu = 0 gives the ordinary half-t. Supply mu = NULL to estimate the translated endpoint, with estimated nu restricted to nu >= 1.
- name: nu
  symbol: '\nu'
  role: shape
  domain: (0, inf)
  simulate: 5
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held: [[mu]]
      reason: Estimating nu with a free endpoint is restricted to nu >= 1 to exclude
        small-shape collapse for samples whose minimum occurs less than half the time.
        Holding mu removes this fitting bound; endpoint ties still require collapse diagnostics.
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dht
  sampler: rht
  control:
    mu: Required endpoint; a value holds it and explicit `NULL` estimates the translation.
log_density: log(2) + loggamma((nu+1)/2) - loggamma(nu/2) - log(nu*pi)/2 - log(sigma) - (nu+1)*log(1+(x-mu)**2/(nu*sigma**2))/2
limits:
  - model: hnorm
    firmness: proved
    name: half-normal with the same endpoint; use mlhnorm(x, mu = 0) after a held
      translation, or mlhnorm(x, mu = NULL) with a free one
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: NormalLimit
    numerical: true
  - name: unbounded scale collapse
    firmness: proved
    when: 'The free scale $\sigma$ tends to zero at the free or held endpoint.'
    detect: 'With held shape, $k>\nu(n-k)$ endpoint observations cause collapse. With free shape and a held endpoint, any endpoint tie suffices. With both endpoint and shape free, $2k>n$ suffices under the fitting restriction.'
    status: Unbounded
  - name: finite unattained scale-zero supremum
    firmness: proved
    fixed: [nu]
    when: 'The free scale $\sigma$ tends to zero at fixed positive shape.'
    detect: 'The endpoint multiplicity satisfies $k=\nu(n-k)$ in a sample of size $n$. The scale supremum is finite and unattained.'
    status: ScaleBoundary
references:
- extraDistr package, HalfT documentation (density and parameterization).
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
fit_args:
  mu: 0
examples: |
  set.seed(1)
  x <- rml(200, mlht())
  mlht(x, mu = 0L)

group: normal and Student
construction:
- folding
relations:
  transform_of:
  - family: lst
    at:
      mu: 0
      sigma: sigma
      df: nu
    transform: abs
    shift: mu
  special_case_of:
  - family: fpareto
    at:
      shape1: nu/2
      shape2: 2
      shape3: 0.5
      scale: sigma*sqrt(nu)
      min: mu
fit:
  method: profile-search
  certified: 'yes'
  note: 'ht/README.md, Details; ht/cert.hpp and ht/test.cpp: endpoint and scale profiling with a bounded global
    df search; certificate conditional on computed comparisons with a 1e-8 n practical log-likelihood margin.'
areas:
- measurement and errors
classification: specialist
classification_reason: Used to model one-sided heavy-tailed deviations measured from a lower endpoint.
summary: Half-t provides a model for one-sided heavy-tailed deviations measured from a lower endpoint.
aliases: []
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, the endpoint is the sample minimum and estimated $\nu$ is restricted to $\nu\ge1$. A finite MLE is not guaranteed: excessive endpoint ties cause collapse and a half-normal limit can prevent attainment. Global shape uniqueness is unknown. Holding the endpoint permits any estimated $\nu>0$; with scale free, any observation at that held endpoint then makes the free-shape likelihood unbounded. With $\nu$ held and scale free, a unique scale maximum exists exactly when $\nu(n-k)>k$, where $k$ counts endpoint observations; equality gives a finite unattained supremum and the reverse inequality gives unbounded likelihood. Holding scale removes collapse, but free shape still requires boundary comparisons. With both shape and scale held, a free endpoint is uniquely the sample minimum. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    The density decreases strictly with distance from the endpoint, so a free endpoint is the sample minimum. At fixed shape the log-scale score decreases strictly from $\nu(n-k)-k$ to $-n$ when a positive residual exists. The displayed two-mode example in Details rules out general shape-profile unimodality.
---

The half-t models nonnegative Student t magnitudes with an endpoint, translated from zero.

## Details

The endpoint $\mu$ is the smallest possible observation. Scale $\sigma$ is the ordinary Student scale, and $\nu$ controls tail weight. Holding the endpoint fits magnitudes measured from a known threshold; estimating it fits a translation as well.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-halfthreshold.md) derives these boundary conditions, including the relevant held parameters.

The finite family is identifiable: its tail exponent determines $\nu$, and its density at the endpoint then determines $\sigma$. Any coefficient may be held fixed.

With free scale and held $\nu$, the log-scale score $\sum_i(\nu+1)w_i^2/(\nu+w_i^2)-n$, $w_i=(x_i-\mu)/\sigma$, is strictly decreasing. A unique root exists iff $\nu(n-k)>k$, where $k$ counts observations at the free or held endpoint. Equality gives a finite unattained scale-zero supremum; a strict reverse inequality gives unbounded likelihood.

With both $\nu$ and $\sigma$ free and $\mu$ held, any tie at that held value permits a small positive $\nu$ and scale collapse with unbounded likelihood; this is a measure-zero event for continuous data away from an exact coincidence. Holding scale removes collapse regardless of $\mu$, so ties are allowed in that subset; with a free $\mu$, free $\nu$ still uses the declared $\nu\ge1$ fitting domain. All held coefficients must be finite, $\mu$ of any sign, $\nu$ and $\sigma$ positive. With shape and scale held, a held endpoint leaves likelihood evaluation, including on data entirely at that endpoint; a free endpoint is estimated by the sample minimum.

For positive $x-\mu$, $f(x;\nu,\sigma,\mu)\leq 2t_\nu(0)(1+1/\nu)^{-(\nu+1)/2}/(x-\mu)$, by maximizing each individual density over scale at $\sigma=x-\mu$. This bound tends to zero as $\nu\to0$ uniformly in scale, ruling out a small-shape escape away from the endpoint. With scale held, densities at the endpoint also tend to zero as $\nu \to 0$ (the half-t mode there is $O(\sqrt\nu)$ at fixed $\sigma$). The other escaping face is the half-normal, compared at its exact root mean square scale (or at the held scale). A normal face within $10^{-8}n/4$ of the retained incumbent is selected and stops; the bounds reserve the rest of the $10^{-8}n$ margin for the unresolved profile gap and floating-point allowance.

The shape profile need not be unimodal: $(0.12773,12.5979,3.91782,2.58658,0.138212)$ has local maxima near $\nu=0.41508$ and $\nu=1.17074$ (independently checked with the reference density), the former higher. The original grid in $\log\nu$ and its local refinements initialize the incumbent. A subsequent branch-and-bound search uses the interval score bounds below to cover all shapes between explicit small- and large-shape bounds, and compares the normal face and, when applicable, the scale-zero face. A one-sided score enclosure allows the interval maximum to be checked at an endpoint. Exhausting the interval budget or encountering a numerical guard returns `IterationFailure`. Scale is the ordinary Student t scale, not a standard deviation. The example holds $\mu=0$; the same certificate applies to a free endpoint on the restricted domain $\nu\ge1$.

With $\mu$ held, the translated endpoint reduces to this fixed-zero-endpoint model on $x-\mu$, exactly as above (the measure-zero exemption noted there).

With $\mu$ free, the likelihood increases in $\mu$ up to the sample minimum (the density is strictly decreasing away from the endpoint for every finite $\nu$, $\sigma$), so the free endpoint is exactly $\mu=\min_i x_i$, including tied minima; a held $\mu$ above any observation is invalid. Writing $k$ for that tie multiplicity: holding $\nu$ reduces to the fixed-endpoint criteria above, with $k$ now counting ties at the minimum. Free $\nu$ is restricted to $\nu\ge1$ whenever $\mu$ is free, including when scale is held. With $\nu$ and $\sigma$ both free, any tie ($k\ge1$, which a free endpoint always produces) would make the unrestricted model collapse as above; restricting $\nu\ge1$ bounds the collapse exponent at the break-even point $\nu(n-k)=k$, i.e. $2k=n$.

For $2k>n$ the restriction does not help (the break-even $\nu_*=k/(n-k)$ itself exceeds 1, so part of the allowed range still collapses): unbounded. For $2k<n$, $\nu_*<1$, so every allowed $\nu\ge1$ is in the existence region, and the restricted profile search (compared against the half-normal escape, as always) is data-dependent. For $2k=n$ exactly, $\nu_*=1$: the restricted search explores $\nu>1$ and additionally compares the explicit finite $\nu=1$, $\sigma\to0$ limit
$$
n\log(2) + n\,\mathrm{logmode}(1) - 2\sum_{x_i>\min(x)}\log(x_i-\min(x)) = n\log(2/\pi) - 2\sum_{x_i>\min(x)}\log(x_i-\min(x)),
$$
an unattained supremum (a scale-boundary diagnostic) when it beats both the interior search and the normal escape, derived by taking the exact $\sigma\to0$ limit of each observation's log density at the break-even $\nu$, where the leading $\log\sigma$ terms cancel by construction. ($\mathrm{logmode}(\nu)$ is the ordinary Student t log density at zero, scale 1; at $\nu=1$ it is $-\log\pi$. The factor 2 for the half density is included separately.) See the [held-endpoint report](../../../../../dev/research/evaluations/2026-10-29-p4-report.md) for the full derivation.

Evaluation uses the internal d/p/q/r functions.

### Outer search evidence

| Question | Evidence |
|---|---|
| Endpoint and inner scale | Proved as above: the endpoint is the minimum when free, and the log-scale score is strictly decreasing with the stated tie criterion. The implemented bisection is numerical. |
| Shape on bounded intervals | The two-mode sample disproves general unimodality. The interval score and inner-solve bounds below bound missed maxima without assuming unimodality. |
| Shape faces | Uniform small-shape, large-shape and tied scale-zero bounds below cover the omitted tails and boundary neighbourhood. |
| Global certificate | In exact arithmetic, conditional on the computed comparisons, the returned finite candidate or selected face is within $10^{-8}n$ log-likelihood units of the supremum. This is a practical floating-point margin, not an outward-rounded interval guarantee. Unresolved bounds or a spent budget stop with `IterationFailure`. |

For a finite bounded $\nu$ interval strictly inside the scale-existence
region, a data-dependent scale bracket is available. Let $p=n-k$ be the
number of strictly positive residuals, $d_{\min}$ and $d_{\max}$ their
smallest and largest values, and $q=n/[p(\nu+1)]<1$. At the scale root,
$\sum_{d_i>0}d_i^2/(\nu\sigma^2+d_i^2)=n/(\nu+1)$, so monotonicity
in $d_i$ gives
$$\frac{d_{\min}}{\sqrt{\nu q/(1-q)}}\leq\sigma_*(\nu)
\leq\frac{d_{\max}}{\sqrt{\nu q/(1-q)}}.$$
The bounds extend uniformly over a compact shape interval separated from
$\nu p=k$. Combined with the analytic shape score and envelope theorem,
they imply a finite profile derivative bound there. The evaluated interval
and tied-boundary bounds used by the search are given below.

More explicitly, with $t=\log\nu$, $w_i=d_i^2/(\nu\sigma^2)$ and
$A(\nu)=\log\Gamma((\nu+1)/2)-\log\Gamma(\nu/2)-\frac12\log(\pi\nu)$,
the fixed-scale score is
$$\partial_t\ell=n\nu A'(\nu)-\frac{\nu}{2}\sum_i\log(1+w_i)
 +\frac{\nu+1}{2}\sum_i\frac{w_i}{1+w_i}.$$
For $\nu\in[a,b]$ with $ap>k$, the bracket above implies
$\sigma_*\ge s_0=d_{\min}\sqrt{(ap-k)/(an)}$. Hence
$w_i\le W=d_{\max}^2/(a s_0^2)$ and the profiled score magnitude is at
most $n[\sup_{a\le\nu\le b}|\nu A'(\nu)|+b\log(1+W)/2+(b+1)/2]$.
The envelope theorem applies because the scale maximum is unique and
interior. The analogous held-scale bound uses the held scale for $s_0$.
For an explicit digamma-free bound, the digamma function is increasing and
$\psi(z+1)-\psi(z)=1/z$, so
$0<\psi((\nu+1)/2)-\psi(\nu/2)<2/\nu$ and
$|\nu A'(\nu)|\le1/2$. Thus the displayed derivative bound is at most
$n[1/2+b\log(1+W)/2+(b+1)/2]$. This can be conservative and does not by
itself make a practical global search.

#### Explicit tail and inner-solve bounds

The following bounds are in exact arithmetic. The implementation uses them
conditional on computed comparisons and reserves a practical floating-point
margin; it does not use outward-rounded interval arithmetic. Write $d_i=x_i-\mu$,
$p=\#\{i:d_i>0\}$, and $k=n-p$. Jensen's inequality for a gamma random
variable gives $\Gamma(z+1/2)/\Gamma(z)\le\sqrt z$, hence
$A(\nu)\le-\tfrac12\log(2\pi)$ and also
$2e^{A(\nu)}\le\sqrt{2\nu/\pi}$. Maximizing each positive-observation
density over scale, and bounding its remaining power factor by one, gives,
for every $0<\nu\le a$,
$$
\ell(\nu,\sigma)\le
\frac n2\log(2a/\pi)-\sum_{d_i>0}\log d_i-k\log\sigma_{\rm held},
$$
where the last term is omitted when $k=0$. When $k>0$ this formula
requires a held scale; free scale and free shape with a held endpoint and
an endpoint tie instead have the unboundedness described above. Thus a
strict comparison of this bound with a finite candidate excludes the whole
small-$\nu$ tail, not only its endpoint.

For the other tail, fix $a>0$ such that $ap>k$ when scale is free. Take
$s_0=\sigma_{\rm held}$ for held scale, or
$s_0=d_{\min}\sqrt{(ap-k)/(an)}$ for free scale. The scale-root bracket
above implies $\sigma_*(\nu)\ge s_0$ for every $\nu\ge a$. The inequality
$\log(1+u)\ge u-u^2/2$ for $u\ge0$, together with the gamma-ratio bound,
then gives the uniform profile bound
$$
\sup_{\sigma}\ell(\nu,\sigma)\le
\ell_{\rm HN}^{\sup}+
\frac{1+1/a}{4\nu}\sum_i\left(\frac{d_i}{s_0}\right)^4,
\qquad \nu\ge a,
$$
where the supremum is over the free scale, or evaluation at the held scale,
as applicable, and $\ell_{\rm HN}^{\sup}$ is the corresponding half-normal
maximum. For free scale this comparison is made at the half-t scale root,
not at arbitrary scales below $s_0$. Consequently any finite $D\ge a$
whose correction is below a chosen likelihood margin excludes the entire
tail $\nu\ge D$ up to that margin. The bound can be very conservative for
samples with widely separated positive residuals.

For a finite shape interval $[a,b]$ in the scale-existence region, suppose
a numerical scale solve retains a genuine log-scale root bracket $[L,U]$
and evaluates the likelihood at its midpoint. Its log-scale score lies
between $-n$ and $p(b+1)-n$, so the error in the profiled log likelihood is
at most $\max\{n,\,p(b+1)-n\}(U-L)/2$ in exact arithmetic. This term can
be added to an outer interval upper bound. It does not validate a computed
bracket or provide a floating-point margin.

The implementation uses the sharper endpoint scale-score residuals in place
of the global score bound here, and includes a sample-size-dependent
floating-point allowance. Scale-score bisection retains a log-scale bracket
of width at most $10^{-8}$ (or stops at adjacent representable numbers);
the retained bracket and its endpoint residuals bound the lost likelihood.
The ordinary-range path caches squared distances scaled by the largest
positive residual and evaluates the rational scale score using one
exponential per df and scale, rather than one per observation. It falls
back to log-distance evaluation when the scaled values or scale factors
would have an extreme range. The finite-precision certificate remains
conditional on computed comparisons with the stated practical margin.
For the large-shape bound it may improve $s_0$
using the $c$ largest positive residuals: if each is at least $d_0$ and
$c(a+1)>n$, their scale-score contributions imply
$\sigma_*\ge d_0\sqrt{[c(a+1)-n]/(an)}$ for all $\nu\ge a$.

At the exact tie $2k=n$, the scale-root bracket degenerates as
$\nu\downarrow1$, but the boundary neighbourhood can be bounded directly.
Put $S=\sum_{d_i>0}\log d_i$, $M=\max_{d_i>0}d_i$, $h=p(\nu-1)$, and
$$
B(\nu)=n(\log2+A(\nu))+\tfrac12p(\nu+1)\log\nu-(\nu+1)S.
$$
Factoring the positive residual terms shows that the log likelihood is at
most $B(\nu)+h\log\sigma-\tfrac12(n+h)\log(1+\nu\sigma^2/M^2)$.
Maximizing this bound in $\sigma$ gives, for $h>0$,
$$
B(\nu)+\frac h2\log\frac{M^2h}{\nu n}
-\frac{n+h}{2}\log(1+h/n).
$$
The correction has limit zero at $h=0$, where $B(1)$ is the stated
scale-zero face. For an explicit uniform bound over $1\le\nu\le1+\delta$
with $p\delta\le n$, use
$$
B(1)+\frac n2\log(1+\delta)
+\frac{p(2+\delta)}2\log(1+\delta)
+\delta|S|+p\delta\max(\log M,0).
$$
Indeed, $|A'(\nu)|\le1/(2\nu)$ follows from the monotonicity and recurrence
of digamma; the other terms follow by bounding $B(\nu)-B(1)$ and discarding
the nonpositive terms in the optimized correction. This conservative
bound tends to the boundary value with $\delta$ and closes the gap left
by an interval certificate away from the boundary.

#### A computable interval score enclosure

For $a\le\nu\le b$, the scale roots can be enclosed more tightly than by
the global $d_{\min},d_{\max}$ bracket. At a fixed scale, each positive
residual contributes $(\nu+1)d_i^2/(\nu\sigma^2+d_i^2)$ to the scale
score. Its derivative in $\nu$ has the sign of $d_i^2-\sigma^2$; its
minimum and maximum on $[a,b]$ are consequently its two endpoint values.
Summing the minima and maxima yields decreasing functions of $\sigma$
that bracket the actual score for every $\nu$ in the interval. Their
respective zeros bracket every scale root. An implementation must verify
the root brackets and include their numerical solve errors.

Given such a bracket, bound $w_i=d_i^2/(\nu\sigma^2)$ between $w_i^-$ and
$w_i^+$. Set $r(w)=w/(1+w)$ and $h(w)=\log(1+w)-r(w)$; both are
nondecreasing. The contribution of observation $i$ to the shape score,
apart from the normalizer, is exactly $[r(w_i)-\nu h(w_i)]/2$. It therefore
lies between $[r(w_i^-)-b h(w_i^+)]/2$ and
$[r(w_i^+)-a h(w_i^-)]/2$. This form avoids subtracting two terms of order
$\nu$ near the normal face; the implementation bounds the small-$w$
series for $h$ on both sides by successive partial sums.

The normalizer term $\nu A'(\nu)$ can be enclosed without assuming it is
monotone. Increasing digamma gives the lower and upper bounds
$$
\frac a2\max\!\left\{0,\psi((a+1)/2)-\psi(b/2)\right\}-\frac12,
\quad
\frac b2\left[\psi((b+1)/2)-\psi(a/2)\right]-\frac12.
$$
At large $\nu$ this subtraction is poorly conditioned. The integral
representation $A'(\nu)=\tfrac14\int_0^\infty
e^{-\nu u/2}\tanh(u/4)\,du$, and
$x-x^3/3\le\tanh x\le x$ for $x\ge0$, instead imply
$$
\frac{1}{4b}-\frac{1}{8a^3}\le \nu A'(\nu)\le\frac{1}{4a}.
$$
The upper bound can be sharpened to
$1/(4a)-1/(8b^3)+1/(4a^5)$ using
$\tanh u\le u-u^3/3+2u^5/15$. This inequality follows by differentiating
the difference: for $u^2\le3/2$, use
$\tanh u\ge u-u^3/3\ge0$ to bound its derivative below; for
$u^2\ge3/2$, its remaining polynomial terms are nonnegative.
Combining these componentwise intervals gives a profile-score enclosure.
Integrating its bounds from an evaluated point, with the inner-solve error
above, bounds the profile throughout the interval. The implementation checks
an interval endpoint when the score has one sign, and otherwise bisects. It
stops after 100,000 intervals or when its numerical checks fail. The
$10^{-8}n$ margin is practical rather than an outward-rounded enclosure.

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients.

Supply `mu` explicitly. Holding `mu = 0` gives the ordinary half-t, another numeric value holds a translation, and `mu = NULL` estimates the endpoint. A half-normal escape recommends `mlhnorm` with the same endpoint and any held scale.

Degrees of freedom can be weakly identified even when the profiled scale
and likelihood are accurate. At the normal face, the common half-step gamma
and Gaussian data terms must cancel before differentiation. The remaining
score requires the joint shape/scale information to bound coefficient error. The free endpoint remains the exact
sample minimum.

For held $\nu=1$, $\mu=0$ and observations $(0,d,2d)$, the scale
maximizer is $d\sqrt{(\sqrt{73}-5)/6}$. At the smallest positive
double $d$, rounding this scale cannot satisfy the coefficient contract;
the fitter reports a numerical accuracy failure.
