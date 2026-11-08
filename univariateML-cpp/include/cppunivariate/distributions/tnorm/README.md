---
id: tnorm
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mean and sd > 0; a < b known and held with positive truncation probability; mean and/or sd free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Truncated Normal
support: {type: real, lower: "a", upper: "b", closed: [true, true]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: a
  symbol: 'a'
  role: lower
  domain: '[-inf, inf)'
  simulate: -inf
  kind: design
  advice: Supply NULL to estimate this bound at the sample extreme, or an infinite value for an unbounded side.
- name: b
  symbol: 'b'
  role: upper
  domain: (-inf, inf]
  simulate: inf
  kind: design
  advice: Supply NULL to estimate this bound at the sample extreme, or an infinite value for an unbounded side.
r:
  density: dtnorm
  sampler: rtnorm
  control:
    a: Required lower bound; NULL estimates min(x); -Inf leaves the lower side unbounded.
    b: Required upper bound; NULL estimates max(x); Inf leaves the upper side unbounded.
    mean: "Optional held parent-normal mean; NULL estimates it."
    sd: "Optional held parent-normal standard deviation; NULL estimates it."
    reltol: Relative tolerance on the Newton step in the natural parameters `mean/sd^2` and `-1/(2*sd^2)` of standardized data.
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: -log(sd) - log(2*pi)/2 - (x-mean)**2/(2*sd**2) - log((erf((b-mean)/(sd*sqrt(2))) - erf((a-mean)/(sd*sqrt(2))))/2)
limits:
  - name: scale collapse
    firmness: proved
    when: 'As $\sigma\to0$, mass concentrates at the projection of $\mu$ onto $[a,b]$.'
    detect: 'Scale is free and the sample is constant; mean is free or every observation equals the projection of the held mean onto the nonempty profiled interval.'
    status: DegenerateData
    message: unbounded
    kind: nonexistence
  - name: endpoint location escape
    firmness: proved
    fixed: [sd]
    when: 'With positive scale held, $\mu\to-\infty$ at a finite lower endpoint or $\mu\to\infty$ at a finite upper endpoint concentrates mass there.'
    detect: 'Mean is free and every observation equals the same finite endpoint of the nonempty profiled interval.'
    status: UnboundedLocation
    message: unbounded
    kind: nonexistence
  - model: exp
    firmness: proved
    when: 'As $\sigma\to\infty$ and $\mu/\sigma^2\to-1/(\bar x-a)$ with $a$ finite and $b=\infty$, the distribution tends to $a+Y$, where $Y$ is exponential.'
    detect: 'The sample is nonconstant, both parent coefficients are free, $a$ is finite, $b=\infty$, and the empirical variance is at least $(\bar x-a)^2$.'
    status: ExponentialBoundary
    numerical: true
  - model: exp
    firmness: proved
    when: 'As $\sigma\to\infty$ and $\mu/\sigma^2\to1/(b-\bar x)$ with $a=-\infty$ and $b$ finite, the distribution tends to $b-Y$, where $Y$ is exponential.'
    detect: 'The sample is nonconstant, both parent coefficients are free, $a=-\infty$, $b$ is finite, and the empirical variance is at least $(b-\bar x)^2$.'
    status: ReflectedExponentialBoundary
    numerical: true
  - name: truncated exponential
    firmness: proved
    when: 'As $\sigma\to\infty$ and $\mu/\sigma^2\to r$ with both bounds finite, the distribution tends to the law with density proportional to $e^{rx}$ on $[a,b]$. It is uniform when $r=0$.'
    detect: 'The sample is nonconstant, both parent coefficients are free, both bounds are finite, and the empirical variance is at least that of the exponential tilt with the same empirical mean.'
    status: TruncatedExponentialBoundary
    numerical: true
  - name: uniform
    firmness: proved
    fixed: [mean]
    when: 'With $\mu$ held and both bounds finite, $\sigma\to\infty$ gives the uniform distribution on $[a,b]$.'
    detect: 'The parent mean $\mu$ is held, scale is free, both bounds are finite, and the empirical squared distance from $\mu$ is at least $((b-\mu)^3-(a-\mu)^3)/(3(b-a))$.'
    status: UniformBoundary
    numerical: true
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, second edition,
  Chapter 13.10. Wiley.
- Barndorff-Nielsen, O. (1978). Information and Exponential Families in Statistical Theory. Wiley.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
fit_args:
  a: -.inf
  b: .inf
examples: |
  set.seed(1)
  x <- rml(200, mltnorm())
  mltnorm(x, a = -Inf, b = Inf)

group: normal and Student
construction:
- truncation
relations:
  truncation_of:
  - family: norm
    at:
      mean: mean
      sd: sd
fit:
  method: convex-solve
  certified: 'yes'
  note: 'tnorm/README.md, Numerical acceptance; tnorm/mle.hpp, tnorm_mle; cmb/acceptance.hpp, certify: exact bound profiling and collapse tests; affine natural-parameter subsets use score half-space coefficient enclosures and a concave tangent likelihood bound. Untruncated fits are closed form. Numerical face ties and failed certificates stop; comparisons use the documented practical floating-point margins.'
areas:
- measurement and errors
classification: specialist
classification_reason: Used to model Gaussian measurements recorded only within a supplied interval.
summary: Truncated Normal provides a model for Gaussian measurements recorded only within a supplied interval.
aliases: []
key_references:
- 3
existence:
  uniqueness: proved
  statement: >-
    With all coefficients free, the bounds profile to the sample extremes.
    For a nonconstant sample, a finite parent-normal MLE exists exactly when
    its empirical variance is smaller than that of the exponential tilt on
    those bounds having the same empirical mean. It is then unique.
    The same criterion applies with bounds held, with the one-sided and
    untruncated cases described below. Constant samples have unbounded
    likelihood when both parent coefficients are free and the bounds form
    a nonempty interval. With $\sigma$ held, a unique finite location estimate
    exists exactly when the empirical mean is strictly inside the bounds.
    With $\mu$ held, the squared-distance criterion below characterizes a
    unique finite scale estimate. With both parent coefficients held,
    compatible observations on a nonempty interval require only likelihood
    evaluation after profiling any free bounds. In every case, coincident
    profiled bounds give no continuous-density MLE.
  reason: >-
    Tightening a free bound to its sample extreme strictly increases likelihood.
    At fixed bounds the log likelihood is strictly concave in
    $(\mu/\sigma^2,-1/(2\sigma^2))$, so any finite maximum is unique.
    Holding either parent coefficient restricts these natural parameters
    to an affine line. The scores compare the empirical sufficient statistic
    with its model expectation. The endpoint expectations give the stated
    existence tests, including the exponential or uniform limit when the
    quadratic coefficient reaches zero.
---

The truncated normal distribution models Gaussian measurements recorded or possible only within an interval. Its coefficients $\mu$ and $\sigma$ describe the parent normal distribution, rather than the mean and standard deviation after truncation.

## Details

Finite bounds are closed and use the density limit from within the interval. A bound may be held or estimated. Tightening a free bound increases the likelihood for every parent location and scale, so its estimate is exactly the corresponding sample extreme. Coincident bounds do not define a continuous density and are rejected.

For fixed bounds the likelihood is strictly concave in the natural parameters $\mu/\sigma^2$ and $-1/(2\sigma^2)$. This establishes uniqueness when a finite maximum exists. A sample can be too dispersed for a finite parent-normal fit, with the supremum instead attained in an exponential limit. Such samples have positive probability under the truncated normal, especially near this limit. With positive scale held, a finite location estimate exists when the sample mean is strictly inside the interval. If every observation is at one finite endpoint, location escapes towards that endpoint and the likelihood is unbounded.

For nonconstant data, let $m$ and $v$ be the empirical mean and variance
with divisor $n$. On finite bounds, the full natural parameter space is
all of $\mathbb R^2$, including positive quadratic coefficients (convex
exponents); the normal subfamily requires $\theta_2<0$. The interior
moment set is $a<m<b$ and $0<v<(m-a)(b-m)$. Within it, fit the unique
exponential tilt $e^{r x}/\int_a^b e^{r u}du$ with mean $m$, and denote
its variance by $v_r$. A finite normal MLE exists exactly when
$0<v<v_r$; $v\ge v_r$ gives the boundary supremum, rather than an
attained normal fit. For $r=0$, $m=(a+b)/2$ and $v_r=(b-a)^2/12$.
Equivalently, for $r\ne0$, write $w=b-a$:
$m=a+w/(1-e^{-rw})-1/r$ and
$v_r=1/r^2-w^2 e^{-rw}/(1-e^{-rw})^2$.

On $(a,\infty)$, the natural parameter space consists of
$\theta_2<0$ with arbitrary $\theta_1$, together with
$\theta_2=0,\theta_1<0$; no positive quadratic coefficient is
integrable. The moment interior is $m>a,v>0$, and the normal MLE
exists exactly when $v<(m-a)^2$. Reflect this condition to
$v<(b-m)^2$ on $(-\infty,b)$. On the whole line only
$\theta_2<0$ is integrable and every nonconstant sample has a normal
MLE. Constant samples with both parent parameters free have unbounded
likelihood as the scale collapses. With a held mean, the sufficient
statistic is the empirical squared distance from that mean; on a finite
interval its upper boundary test compares it with the uniform's squared
distance. On an unbounded interval that expectation diverges and there is
no uniform escape. A finite scale estimate also requires the empirical squared
distance to exceed its infimum over the support. Equality means that every
observation is at the closest support point to the held mean, and scale
collapse makes the likelihood unbounded. These conditions follow by profiling the strictly
concave natural likelihood and testing its derivative at zero quadratic
coefficient. Numerical comparisons use a likelihood tolerance, as described in the R notes.


With parent mean held, write $q=n^{-1}\sum_i(x_i-\mu)^2$ and $q_0=\inf_{a\le x\le b}(x-\mu)^2$. A finite scale estimate exists exactly when $q>q_0$ and, for finite bounds, $q$ is smaller than the uniform distribution's expected squared distance. On unbounded intervals there is no upper restriction. These strict inequalities also cover compatible constant samples away from the closest support point to the held mean. Every statement assumes a nonempty interval and observations in its closed support.

The collapse tests require no tolerance. At an interior projected mean, the density grows as $1/\sigma$; at an endpoint equal to the mean it grows as $2/(\sqrt{2\pi}\sigma)$. At a finite lower endpoint $a>\mu$, writing $d=a-\mu>0$ gives normalization relative to the endpoint exponent
$\int_0^{b-a}\exp(-(2du+u^2)/(2\sigma^2))\,du\sim\sigma^2/d$.
Thus the endpoint density is asymptotic to $d/\sigma^2$. Reflection proves the upper-endpoint case. With scale held and $\mu\to-\infty$, the same integral is asymptotic to $\sigma^2/(a-\mu)$, proving the location escape; reflect for the upper endpoint. These arguments also hold with an infinite opposite endpoint. Profiling a free bound first preserves the argument unless both profiled bounds coincide, which is invalid.

## R notes

Supply both `a` and `b`. A numeric value holds a bound, `NULL` estimates it at the corresponding sample extreme, and `-Inf` or `Inf` leaves that side unbounded. Any subset of the four coefficients can be held. The same collapse and exponential boundary tests apply after profiling free bounds. Constant data with coincident estimated bounds are rejected.

Boundary comparisons favour the limiting model within `1e-8` times sample size in log likelihood. Exponential, truncated exponential and uniform escapes stop rather than returning a finite parent-normal estimate. `mlunif` estimates its own endpoints and is a different model from the uniform limit on held bounds. See `mlhnorm` for a half-normal with an estimated endpoint and `mlnorm` for the untruncated fit.

Scale collapse is checked by exact equality before searching: constant observations must equal the projection of a held mean onto the profiled interval, or mean must be free. With scale held and mean free, constant observations at a finite endpoint instead report an unbounded location escape. Both-held fits evaluate the likelihood. Coincident profiled bounds remain invalid. The [existence review](../../../../../dev/research/evaluations/2026-11-02-existence-6-report.md) preserves the original reproducer.

## Numerical acceptance

The acceptance calculation centers joint and held-scale fits at the empirical
mean and scales them by the empirical standard deviation; held-mean fits use
the empirical root mean squared distance from that mean. This avoids the
nearly singular covariance obtained by centering at a distant parent mean
near an exponential face. In these coordinates, the score components are
empirical minus model means of $U$ and $U^2$. With mean held only the quadratic
coordinate is free; with scale held only the linear coordinate is free.
The inverse sufficient-statistic covariance proposes surrounding points for
the [concavity certificate](../cmb/acceptance.hpp). Its half-space enclosure
is transformed back through
$\mu=c-s\theta_1/(2\theta_2)$ and
$\sigma=s/\sqrt{-2\theta_2}$. Every enclosed corner must agree with the
reported mean within $10^{-8}(s+|\hat\mu|)$ and scale within
$10^{-8}\hat\sigma$. Enclosures crossing zero quadratic coefficient fail.

The certificate uses the likelihood and coordinate margins stated in
[acceptance.hpp](../cmb/acceptance.hpp); quadrature and face comparisons remain
conditional floating-point calculations. Each refinement uses two or three
quadratures and has the shared solver allowance. A failed enclosure stops
numerically with its feasible witness and any established likelihood upper bound.

At the likelihood roundoff floor, Newton backtracking may accept an objective
decrease of at most $64\epsilon(1+|f|)$ only when the free moment residual
strictly decreases. This replaces premature acceptance of a stalled score
below $10^{-7}$ in near-face examples; the final quantitative certificate
still decides acceptance. A finite solve within the face likelihood tolerance
is reported as a numerical tie with both witnesses, rather than asserting
that a finite MLE does not exist.

The data normalization uses the residual root mean square directly, including with a held parent mean. Neither the Newton equations nor the final score enclosure require a variance in physical units.
