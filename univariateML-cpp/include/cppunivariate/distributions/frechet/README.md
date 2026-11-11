---
id: frechet
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite lambda > 0, sigma > 0 and mu; mu held; lambda and/or sigma free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Frechet
support: {type: real, lower: "mu", upper: "inf", closed: [true, false], endpoint_log_density: {lower: "-Inf"}}
params:
- name: lambda
  symbol: '\lambda'
  role: shape
  domain: '(0, inf)'
  simulate: 1
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held:
      - - mu
      reason: >-
        The bound lambda >= 1 excludes scale collapse at a freely fitted
        threshold when the minimum occurs fewer than half the time. Exceptional
        ties still require a separate check. It also excludes a shape tending to
        zero while mu approaches the minimum, which can make the likelihood
        unbounded even with sigma held. Holding mu gives an inverse Weibull
        model on x - mu, for which every positive lambda is admissible.
  kind: regular
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: threshold
  advice: >-
    Holding mu fits an inverse Weibull model on x - mu. Use mu = NULL to
    estimate the threshold with free lambda restricted to lambda >= 1.
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dfrechet
  sampler: rfrechet
  control:
    mu: Threshold to hold fixed; NULL estimates it.
    lambda: Positive shape to hold fixed, or NULL to estimate.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    reltol: Requested relative coefficient accuracy; conditional point-profile solves also use dimensionless scores.
    iterlim: "Work limit per solve: min(iterlim, 100000) solver iterations; certificates allow 128 * min(iterlim, 100000) slab, box or profile evaluations."
log_density: log(lambda)-log(sigma)-(lambda+1)*log((x-mu)/sigma)-((x-mu)/sigma)**(-lambda)
limits:
  - firmness: proved
    name: fixed-shape collapse
    fixed: [lambda]
    status: InvalidData
    message: "unattained collapse face"
    when: 'With shape $\lambda$ held, $\mu=m-\varepsilon$ approaches the sample minimum $m$ and $\sigma=c\varepsilon$ decreases to zero for a positive constant $c$.'
    detect: 'Location and scale are free, and the minimum occurs $k$ times with $\lambda(n-k)\le k$. The strict inequality gives divergence. Equality gives a finite, unattained collapse face.'
  - firmness: proved
    name: shape divergence
    fixed: [sigma]
    status: InvalidData
    message: "unbounded shape likelihood"
    when: 'With scale $\sigma$ held, the free shape $\lambda$ tends to infinity and $\mu$ equals the common observation minus $\sigma$.'
    detect: 'The sample is constant and location is free, or every observation minus the held location equals $\sigma$.'
  - firmness: proved
    model: gumbel
    when: 'The shape $\lambda$ tends to infinity with $\sigma/\lambda=a>0$ and $\mu+\sigma=b$ fixed.'
    detect: 'A certified global comparison selects the Gumbel face over the finite candidates when location, scale and shape are free.'
    status: GumbelLimit
  - firmness: proved
    name: free collapse
    status: NoMLE
    message: "minimum multiplicity permits scale collapse"
    when: 'The location $\mu$ approaches the sample minimum from below, $\lambda$ tends to one, and $\sigma$ decreases proportionally to the location gap.'
    detect: 'All coefficients are free and the minimum occurs exactly $n/2$ times. A global pair bound proves that no finite competitor attains the collapse supremum.'
fit_args:
  mu: ~
examples: |
  set.seed(1)
  x <- rml(200, mlfrechet())
  mlfrechet(x, mu = NULL)

group: extreme value
construction:
- base
relations:
  transform_of:
  - family: invweibull
    transform: shift
    at:
      shape: lambda
      scale: sigma
    shift: mu
  special_case_of:
  - family: gev
    at:
      mu: mu+sigma
      sigma: sigma/lambda
      xi: 1/lambda
fit:
  method: delegation
  certified: 'partial'
  note: 'frechet/README.md, Details; frechet/mle.hpp (public fitter and held overloads); frechet/test.cpp: Every subset with mu held is an inverse-Weibull fit (joint concave log profile, held-shape closed form or held-scale monotone shape score). Held lambda with free mu has a monotone-location certificate when sigma is held and a monotone profiled-scale certificate when sigma is free. All-free exact half-minimum ties have a proved finite unattained global supremum by the pair bound. All-free two-point minimum minorities compare the unique shape-one member against the Gumbel face by inverse-shape convexity and a bounded concave scalar solve. An exact public scale hold containing a certified finite two-point member inherits its certificate. General all-free fits use the positive-shape GEV slab cover in gev/joint_certificate.hpp, with direct Frechet coefficient enclosures and global Gumbel comparison. Free-lambda/free-mu fits with public sigma held now attempt a constrained two-coordinate global cover with a uniform location-root enclosure and an explicit infinite-shape tail bound; a successful search remains uncertified if the cover cannot close.'
areas:
- extremes and environment
classification: specialist
classification_reason: Used to model block maxima with polynomially decaying upper tails.
summary: Frechet provides a model for block maxima with polynomially decaying upper tails.
aliases:
- "type II extreme value"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free and unrestricted shape, the likelihood is
    unbounded for every sample. The fitted model therefore restricts $\lambda\ge1$.
    A minimum repeated more than $n/2$ times gives unbounded likelihood.
    At exactly $n/2$ repeats, a finite collapse face must be compared with
    finite candidates. A Gumbel limit can also prevent attainment.
    A complete existence criterion and joint uniqueness are not established.
    With $\mu$ held and observations strictly above it, both remaining
    coefficients have a unique finite MLE exactly for nonconstant data.
    Holding $\mu$ and $\lambda$ gives a unique finite scale estimate even
    for constants. Holding $\mu$ and $\sigma$ gives a unique finite shape
    estimate unless every $x_i-\mu=\sigma$, when likelihood is unbounded.
    With only $\lambda$ held, a unique finite MLE exists exactly when
    $\lambda(n-k)>k$, where $k$ is the minimum multiplicity.
    Otherwise the supremum is an unattained collapse face or is unbounded. With only $\sigma$ held,
    constant data have unbounded shape likelihood.
    Holding $\lambda$ and $\sigma$ gives a unique finite location maximum.
    Uniqueness with shape free and location free is not established.
    Holding all coefficients leaves only likelihood evaluation.
  reason: >-
    Holding location reduces the model to inverse Weibull on $x_i-\mu$,
    whose monotone shape equation proves the stated conditional results.
    For free location, put $\mu=m-\varepsilon$ and
    $\sigma=c\varepsilon$. The likelihood is proportional asymptotically
    to $\varepsilon^{\lambda(n-k)-k}$ at fixed shape.
    The Gumbel path and the half-tie collapse face are derived in Details.
    These paths prove obstructions and construct limiting competitors.
    The all-free profile cover compares every surviving finite maximum with
    the Gumbel face. Held-scale joint fits instead use a constrained
    location-root rectangle and an infinite-shape likelihood envelope.
---

The Fréchet distribution models extreme maxima with heavy right tails. The threshold $\mu$ sets the lower endpoint, $\sigma$ sets the scale above it, and $\lambda$ controls the tail decay.

## Details

For the Gumbel face fix $b\in\mathbb R$ and $a>0$, set
$\sigma=\lambda a$ and $\mu=b-\lambda a$, and let $\lambda\to\infty$.
For fixed $x$, $((x-\mu)/\sigma)^{-\lambda}\to
\exp(-(x-b)/a)$, and differentiating the Fréchet CDF gives the Gumbel
density limit. Scheffé's lemma gives total-variation convergence. This
is a density limit, not a certificate of the face comparison.

For the free collapse path put $m=\min x_i$, $\mu=m-\varepsilon$,
$\lambda=1$, and $\sigma=(n/k)\varepsilon$, where $k$ observations equal
$m$. The minimum density is $(n/k)e^{-n/k}/\varepsilon$, and each
nonminimum density is asymptotic to $(n/k)\varepsilon/(x_i-m)^2$.
Thus the likelihood is asymptotic to a positive constant times
$\varepsilon^{n-2k}$; at $k=n/2$ it has a finite positive limit. The pair bound below proves that this is the global supremum at an exact
half tie, and that no finite Fréchet fit attains it.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-extreme.md) describes the limiting paths and the scope of the boundary comparisons.

When the location is free, the minimum multiplicity controls the positive-shape collapse; ties above the minimum do not cause it.

With shape and scale free, a held-location fit requires a nonconstant sample strictly above the threshold. Its existence and identification are exactly those of inverse Weibull after subtracting the threshold. Holding location permits every positive shape. Holding location and shape gives the explicit scale estimate
$\hat\sigma=(n/\sum_i(x_i-\mu)^{-\lambda})^{1/\lambda}$.
Holding location and scale leaves a strictly concave shape likelihood, solved by bisection. These conditional cases allow a single observation and constants unless the stated shape divergence applies.

When location and shape are free, the fitted shape is restricted to $\lambda\ge1$. The attained member $\lambda=1$ is included. The joint fitter requires at least three observations. A minimum repeated more than half the time always gives unbounded likelihood under this restriction. At an exact half tie, the fitter stops with the proved finite unattained
supremum $A=n\log2-n-2\sum_{x_i>m}\log(x_i-m)$ in its stop details.

For a minimum repeated $k$ times, the fixed-shape collapse exponent is $\lambda(n-k)-k$. Strictly negative values give divergence, including every positive shape for constant data. With only shape held, the fitter requires $\lambda(n-k)>k$ and rejects equality as an unattained collapse face. In this fixed-shape case the rejection also follows from a global derivative bound. Put $r_i=\varepsilon/(x_i-m+\varepsilon)$ and profile out scale. Then
$\varepsilon\ell'(\varepsilon)=n\lambda\sum_i r_i^{\lambda+1}/\sum_i r_i^\lambda-(\lambda+1)\sum_i r_i$.
Since $r_i\le1$, the ratio is at most one, while $\sum_i r_i>k$ for nonconstant data. Thus the derivative is negative whenever $n\lambda\le(\lambda+1)k$, including equality. The supremum is confined to the unattained collapse face. If $\lambda(n-k)>k$, the profiled likelihood tends to zero both as the location gap decreases to zero and as it grows without bound. It therefore attains a finite maximum. Global shape coverage also needs the outer certificate below.

Holding shape and scale leaves a scalar location fit and permits every positive shape, including values below one. Its likelihood tends to zero at both ends of the location domain, so a finite maximum exists. Uniqueness follows by writing $d_i=x_i-\mu$ and $R=\sum_i d_i^{-\lambda-1}/\sum_i d_i^{-1}$. As the location gap increases, $R$ decreases strictly from infinity to zero. Indeed, under weights proportional to $d_i^{-1}$, its derivative is
$-(\lambda+1)E_w(d^{-\lambda-1})+E_w(d^{-\lambda})E_w(d^{-1})<0$,
since the covariance of the two decreasing powers of $d$ is nonnegative. The gap score changes sign exactly when $R=(\lambda+1)/(\lambda\sigma^\lambda)$. Holding only scale restricts the free shape to $\lambda\ge1$ and rejects constant samples because of shape divergence. The lower shape bound also excludes a different spike that is possible even with scale held. Put $\mu=m-\sigma e^{-L}$ and $\lambda=c/L$ for fixed $c>0$. As $L$ increases, each minimum density is proportional to $e^L/L$, while every other observation contributes a density proportional to $1/L$. Thus the unrestricted likelihood grows as $e^{kL}/L^n$ for any sample with minimum multiplicity $k$. With shape free, the location search profiles the concave shape solve and refines sampled local maxima. These are proposals for the constrained global cover below; failure to close that cover leaves the successful proposal uncertified.


With $\lambda$ and $\sigma$ held, the monotone ratio above is solved in
log gap by expanding a bracket and bisecting. There is no fixed search
window. Failure to close a bracket or root within `iterlim` stops.

With only $\lambda$ held, scale is profiled analytically. Put
$S_p(\varepsilon)=\sum_i(g_i+\varepsilon)^{-p}$, $g_i=x_i-\min x$,
and $R=S_{\lambda+1}/(S_\lambda S_1)$. The gap score has the sign of
$n\lambda R-(\lambda+1)$. For $q_i=(g_i+\varepsilon)^{-1}$ define
$E_p(q)=\sum q_i^{p+1}/\sum q_i^p$. This weighted mean is strictly
increasing in $p$ for nonconstant data: its derivative with respect to $p$
is the positive weighted covariance of $q$ and $\log q$. Consequently
$\frac{d}{d\varepsilon}\log R
=-(\lambda+1)E_{\lambda+1}(q)+\lambda E_\lambda(q)+E_1(q)<0$.
The two endpoint limits are $R(0+)=1/k$ and $R(\infty)=1/n$.
Thus when $\lambda(n-k)>k$ there is exactly one score root, and it is
the unique global maximum. This also proves that no distinct equal maxima
occur in the fixed-shape profile. The earlier collapse criterion handles
all other samples before searching.
At equality the finite, unattained supremum is
$n\log(n\lambda)-n-n\log k-(\lambda+1)\sum_{g_i>0}\log g_i$;
a negative exponent gives an infinite supremum. These bounds accompany
the structured non-existence stop. Overflowing differences of finite
observations are evaluated on half-sized operands before taking the logarithm.

The solver evaluates these ratios with $r_i=\varepsilon/(g_i+\varepsilon)$,
so each sum has at least one unit minimum term and cannot underflow as a
whole. It expands a bracket in log gap and bisects; there is no fixed
location window. Bracket and root loops each use
`min(iterlim,100000)` iterations. The exact-arithmetic certificate is
conditional on computed score signs, with practical log-gap width
`reltol*(1+abs(log_gap))`. Equality of the collapse exponent is tested
with a fused multiply-add to avoid a rounded false equality. Diagnostic
likelihood bounds use $256\epsilon_{\rm mach}n(1+\lambda)^2$;
these are floating-point margins, not outward-rounded intervals.
Arithmetic failure, an unrepresentable gap or restored coefficients,
or a restored likelihood discrepancy above $10^{-7}n$ stops numerically.
A returned normalized point also requires
$256\epsilon_{\rm mach}\lambda[1+|\log\sigma|+\max_i|\log(g_i+\varepsilon)|]\le10^{-7}$.
This conservative precision check prevents enormous held powers from turning
rounded log distances into a spurious point fit; it is a floating-point
safeguard, not an additional parameter restriction.
An exhausted bracket or root solve stops as non-convergence, with a structured
`numerical` condition,
retaining its finite witness in original units. These monotone certificates
establish no joint free-shape result.


For the half-tie bound, pair each minimum with one nonminimum distance $d$
away. After optimizing each pair's scale separately, its density product
is at most $4e^{-2}/d^2$ times
$[a\sinh u/\cosh(au)]^2$, where $a=\lambda\ge1$ and
$e^{-2u}=(m-\mu)/(m+d-\mu)$. Convexity of $\sinh$ gives
$a\sinh u\le\sinh(au)<\cosh(au)$, so the bound is strict for every
finite fit. Multiplying over all pairs bounds the global likelihood
strictly by $\exp(A)$. The shape-one collapse path attains $A$ in the limit,
proving the finite unattained supremum without a search or nuisance cutoff.
This proof covers all joint paths, including diverging shape. It makes no
claim about free-shape fits without an exact half-minimum tie, or about the
held-scale problem. The GEV map used below preserves the all-free likelihood;
it does not preserve public location or scale holds.

The all-free fit uses generalized extreme-value coordinates: location $\mu+\sigma$, scale $\sigma/\lambda$, and shape $1/\lambda$. A global slab cover on $0\le\xi\le1$ compares isolated finite maxima with the Gumbel face. Exact half-minimum ties and two-point minimum minorities retain their separate analytic certificates. Unresolved coverage, coefficient precision or candidate comparisons stop fitting.


### General all-free public-coordinate certificate

At minimum pivot $m$, let $v=\xi(m-\mu)$ and
$g=-\log\operatorname{mean}\exp[-\log(1+\xi(x_i-m)/v)/\xi]$.
The direct public map is

$$\lambda=1/\xi,\qquad \mu=m-v/\xi,\qquad
  \sigma=v e^{\xi g}/\xi.$$

The positive-shape GEV cover isolates roots until enclosures of these
Fréchet coefficients meet the requested accuracy. Accuracy of GEV location
and scale alone would be insufficient near the singular map at zero shape.
The $\xi=0$ candidate is a proper Gumbel limit, with finite location and
scale, and never a capped finite Fréchet estimate. Candidate and limit
witnesses use their own public coordinates in both bindings.

The [profile-cover derivation](../../../../../dev/research/evaluations/2026-11-08-gev-allfree-data/joint-proof.md)
provides the monotone inner roots, minimum-pivot Taylor bounds and global
comparisons. Holding public Fréchet scale constrains the GEV orbit and
changes the inner equation; this all-free cover is not reused for that
hold. Its existing compatible-member shortcuts remain separate.


## R notes

All hold combinations are supported. Supply `mu` explicitly: a value holds the threshold, and `mu = NULL` estimates it. With `mu` free and `lambda` free, the fit restricts `lambda >= 1`. With `mu` held, `mlinvweibull` on `x - mu` gives the corresponding two-parameter fit.

A strictly winning Gumbel face produces a limit stop with proper Gumbel coefficients and advice to use `mlgumbel`. Separated candidates that cannot be ordered within the arithmetic margin produce a tie stop. Estimated coefficients on an applicable `fit_domain` bound carry a `restricted` attribute and emit a warning of class `univariateML_restricted`. Held coefficients are not flagged. Restriction exemptions apply to free coefficients. Failed optimizers and unresolved scalar searches stop. A small score alone does not establish global optimality.


### All-free two-point comparison

Let the data take values $m<M$, with minimum fraction $p<1/2$,
$q=1-p$, and $d=M-m$. Write $b=1/\lambda\in(0,1]$ and
$t=\lambda\log((M-\mu)/(m-\mu))>0$. After profiling scale,

$$\ell/n=\log\frac{e^{bt}-1}{b}-q(1+b)t
 -\log(p+qe^{-t})-1-\log d.$$

For fixed $t$ its second derivative in $b$ is
$1/b^2-t^2/[4\sinh^2(bt/2)]>0$. Consequently only $b=1$ and
$b=0$ can maximize the closure likelihood. On $b=1$ the unique maximum is

$$h=\frac{p(1-2p)}{\sqrt{pq}+p},\quad
 \lambda=1,\quad\mu=m-dh,\quad
 \sigma=d\frac{h(1+h)}{p+h}.$$

On the Gumbel face the strictly concave scalar objective is
$\log t-qt-\log(p+qe^{-t})-1-\log d$.
Its decreasing score brackets its unique root. Tangent upper bounds on that
bracket separate the face from the finite candidate; unresolved comparisons
stop, rather than applying the ordinary face tolerance. Strict convexity
excludes every finite interior shape, so a winning Gumbel face is unattained.
A finite winner is unique. The comparison margin is
$4096\epsilon n(1+|\log d|+|\log h|)$; a separate propagated arithmetic
guard checks the relative public-coefficient accuracy. These certificates
are exact-arithmetic statements conditional on computed comparisons with
practical floating-point margins. The Gumbel stop retains its coefficients
and finite supremum bounds in both bindings. An exact scale hold containing the finite winner preserves its maximum;
range and fused integer-product residuals verify this identity, so equality
to a rounded free estimate alone does not qualify.
Other held-scale values require a constrained search; the Gumbel face does
not satisfy a finite public scale hold.
The [derivation and independent checks](../../../../../dev/research/evaluations/2026-11-08-gev-allfree-data/two-point-proof.md)
also explain why the all-free orbit does not establish a held-scale certificate.

For a recognized exact scale hold, a failed free certificate retains a numerical
stop and only candidates satisfying that hold contribute to the likelihood
lower bound. An unconstrained Gumbel winner cannot be transported to a finite
held scale; that case uses the constrained search and cover below.

### Free shape and location with public scale held

In units of the held scale, write $x_i\ge0$ relative to the sample minimum
and $\mu=-e$. Every fixed-shape location maximum satisfies
$[k/(2n)]^{1/\lambda}\le e<1$, where $k$ is the minimum multiplicity.
This encloses the constrained inner roots on every shape slab. A separate
range-dependent likelihood envelope tends to minus infinity as
$\lambda\to\infty$, giving a finite outer shape bound above a feasible
likelihood level. The free-scale Gumbel face is not feasible under this hold:
$\sigma_G=\sigma_F/\lambda$ tends to zero.

The constrained cover uses $(\lambda,\log e)$ jets, interval Newton and
strict concavity on an isolating rectangle, followed by a complete stationary
and value box cover. The shape-one boundary remains attained and restricted.
Once the root is enclosed, the excluded rectangle is enlarged: a negative
definite interval Hessian on every piece of a subdivision makes that root the
unique maximizer of the larger rectangle (at the shape-one edge, an outward
shape score with negative location curvature). Without this step the cover
excluded a rectangle of relative radius $2\times10^{-6}$ and spent about
three quarters of its work approaching it, which left ordinary samples of
size 500 uncertified at the default `iterlim`. The
[measurements](../../../../../dev/research/evaluations/2026-11-10-held-concave-report.md)
record this and the remaining limit: the work still grows with the sample
size, and samples of size 1000 and 2000 need `iterlim` 200 and 400.
The [proof and arithmetic margins](../../../../../dev/research/evaluations/2026-11-10-gev-held-data/proof.md)
state the tail bound and root-existence conditions. A completed cover sets the
C++ result's `certified` field for this route. If coverage, coefficient inclusion or a comparison remains unresolved,
the successful search is returned unchanged and uncertified. A strictly better
feasible point, rechecked in public coordinates, instead stops with both point
witnesses because it disproves the search. Existing search failures retain their
status. The catalogue flag remains partial.

R attaches a logical `certified` attribute whenever the result carries the
marker (every uncertified fit, and every held-scale fit); JavaScript carries
it as zero or one.

With location held, the inverse-Weibull reduction preserves numerical iteration
failures and the per-fit uncertified flag for a scale limited by binary64
spacing. Such a flag does not assert nonexistence.
