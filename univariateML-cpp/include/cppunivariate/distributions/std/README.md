---
id: std
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mean, sd > 0 and nu > 2; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Student-t
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mean
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sd
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: nu
  symbol: '\nu'
  role: df
  domain: (2, inf)
  simulate: 5
  kind: regular
r:
  density: dstd
  sampler: rstd
  control:
    nu: Optional held nu; NULL estimates it.
    sd: Optional held sd; NULL estimates it.
    mean: Optional held mean; NULL estimates it.
    gradtol: All-free coefficient enclosure width target, clamped between 1e-7 and 1e-6 in normalized location and relative scale/shape units, including the complete sd enclosure. Held fits retain their existing score tolerance.
    iterlim: Maximum iterations in each nuisance EM and Newton proposal. The all-free global certificate separately allows at most 4096 profile evaluations.
log_density: loggamma((nu+1)/2) - loggamma(nu/2) - log((nu-2)*pi)/2 - (nu+1)*log(1+((x-mean)/sd)**2/(nu-2))/2 - log(sd)
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale tends to zero at the fitted or held location.
    detect: The collapse exponent is negative at an attainable location; equality is treated separately as an uncertified boundary.
    status: Unbounded
    message: "collapse"
  - name: scale collapse
    firmness: proved
    fixed: [nu]
    when: "The ordinary t scale tends to zero at the free or held location."
    detect: 'The location has multiplicity $k>\nu(n-k)$ in a sample of size $n$.'
    status: Unbounded
  - name: tie equality boundary
    firmness: argued
    fixed: [nu]
    when: "The ordinary t scale tends to zero at the free or held location."
    detect: 'The location has multiplicity $k=\nu(n-k)$; a regular maximum is not certified.'
    status: ScaleBoundary
  - model: 'norm'
    firmness: proved
    when: 'The shape $\nu$ tends to infinity with mean and sd fixed.'
    detect: For all-free fits, exhaustive profile and tail bounds select this face. Held subsets retain their numerical face comparisons.
    status: 'NormalBoundary'
    numerical: true
  - model: lst
    firmness: proved
    when: 'The shape $\nu$ decreases to two with mean and ordinary t scale fixed; sd diverges.'
    detect: For all-free fits, exhaustive profile and tail bounds select this face. Held subsets retain their numerical face comparisons.
    status: 'DfTwoBoundary'
    numerical: true
references:
- Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, Chapter 13.
  Wiley, New York.
- 'fGarch documentation (accessed 2026-10-31). fGarch: Rmetrics - Autoregressive Conditional Heteroskedastic Modelling.
  R package reference manual. https://cran.r-project.org/web/packages/fGarch/fGarch.pdf.'
holds: []
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mlstd(x)

group: normal and Student
construction:
- location-scale
relations:
  special_case_of:
  - family: sstd
    at:
      mean: mean
      sd: sd
      nu: nu
      xi: 1
  - family: lst
    at:
      mu: mean
      sigma: sd*sqrt((nu-2)/nu)
      df: nu
fit:
  method: profile-search
  certified: partial
  note: 'std/README.md, Details; lst/certificate.hpp; std/mle.hpp: All-free fits use exhaustive profile envelopes, root inclusion and uniform curvature neighborhoods, with normal and lower-face certificates conditional on computed practical arithmetic bounds. Held subsets retain their existing exact or numerical routes; general held location/shape profiles remain uncertified.'
areas:
- finance
- measurement and errors
classification: classical
classification_reason: A standard model for finite-variance Student errors parameterised by mean and standard deviation.
summary: Student-t provides a model for finite-variance Student errors parameterised by mean and standard deviation.
aliases: []
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, a finite MLE is not guaranteed: scale collapse, a normal limit or a location-scale $t_2$ limit can prevent attainment. Global uniqueness is unknown. Holding $\nu>2$ removes both shape limits; a tie of multiplicity $k$ gives unbounded likelihood if $k>\nu(n-k)$, while equality requires separate boundary analysis. With $\mu$ and shape held, a unique positive scale maximum exists exactly when $\nu(n-k)>k$, where $k$ counts observations at that location. Holding $\sigma$ removes scale collapse only when shape is also held; free shape can still collapse as $\nu\downarrow2$. Other held subsets use numerical profiles and reachable boundary comparisons. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    For ordinary t scale $a$, collapse contributes $a^{\nu(n-k)-k}$. With location and shape held, the log-scale score $\sum_i(\nu+1)d_i^2/(\nu a^2+d_i^2)-n$ decreases strictly from $\nu(n-k)-k$ to $-n$ when any residual is nonzero. The density limits in Details establish the shape faces, not global profile optimality.
---

The Student-t distribution is a symmetric, heavy-tailed alternative to the normal distribution.

## Details

The location $\mu$ is the mean and $\sigma$ is the standard deviation. Smaller $\nu>2$ gives heavier tails. Holding either moment preserves it during estimation of the remaining coefficients.

For fixed mean and standard deviation, the ordinary t scale is
$\sigma\sqrt{(\nu-2)/\nu}$; the gamma ratio in the t normalizer
and $(1+z^2/\nu)^{-(\nu+1)/2}\to e^{-z^2/2}$ give the normal
density as $\nu\to\infty$. For the $t_2$ face, instead fix ordinary
t scale $a>0$ and let $\sigma=a\sqrt{\nu/(\nu-2)}$ diverge as
$\nu\downarrow2$; continuity of the ordinary t density gives the
location-scale $t_2$ density. Scheffé's lemma upgrades each to
total-variation convergence. Neither limit certifies the fitter's
numerical boundary comparison.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) describes the limiting paths and the scope of the boundary comparisons.

With free degrees of freedom, a tie of multiplicity $k>2(n-k)$ makes the likelihood unbounded; equality enters the global comparison through its exact collapse supremum. A selected bounded collapse path stops separately from unboundedness; an unresolved comparison stops with a certificate failure.

This is the fGarch parameterisation: $\mu$ and $\sigma$ are the mean and standard deviation, and $\nu$ is greater than 2. A location-scale t distribution with 2 degrees of freedom has infinite variance and cannot be represented by finite coefficients in this parameterisation.

With $\nu$ held fixed, only location and scale are estimated by monotone
EM, stopping when the norm of the scale-standardized location and log-scale
scores is below the requested score tolerance. Failure to reach that tolerance is an error.
The normal and two-degree-of-freedom escapes do not apply to this subset.
Fixed $\nu$ must exceed 2.
For multiplicity $k$ in a sample of size $n$, collapse of ordinary t scale $a$ has likelihood
proportional to $a^{\nu(n-k)-k}$. A negative exponent is reported as
unbounded; equality is reported separately as a scale-collapse boundary,
without claiming unboundedness or certifying a regular MLE (Cauchy equality
can have nonunique attained fits). Strictly positive exponents for every tie
permit the regular fixed-parameter fit.


Holds use the ordinary t scale $\sigma\sqrt{(\nu-2)/\nu}$. With mean held, only its tie multiplicity drives scale collapse. With standard deviation held, the $\nu\downarrow2$ limit forces ordinary scale to zero, rather than giving a finite-scale t2 law. The conditional normal face remains eligible. When the standard deviation is free, the conditional ordinary t2 face is compared separately. Profile searches are numerical, not global certificates.

The [2026-11-04 certificate study](../../../../../dev/research/evaluations/2026-11-04-t-cert-report.md)
proves geodesic convexity of the fixed-shape nuisance objective for
$\nu\ge1$, gives explicit nuisance containment and finite-interval profile
upper bounds, and derives the refitted normal-seam expansion. With $z_i=(x_i-\bar x)/s_N$ and
$s_N^2=n^{-1}\sum_i(x_i-\bar x)^2$, its first coefficient in
$\xi=1/\nu$ is $n(\overline{z^4}-3)/4$. A negative coefficient certifies
only local preference for the normal limit: the study retains a sample with
negative slope and a strictly better finite fit. The subsequent root inclusion and uniform-neighborhood arguments supply coefficient and global exclusion checks in the native certificate described below.

When the largest tie multiplicity satisfies $k<2n/3$, a finite all-free
maximum exists if and only if some point with $\nu>2$ reaches or exceeds
both the optimized normal likelihood and the optimized ordinary-scale
$t_2$ likelihood. Equality is included. The linked study proves this
criterion using uniform nuisance containment and profile continuity; it
does not establish general shape unimodality or uniqueness. The all-free native certificate now makes exhaustive comparisons conditional on its computed enclosures.


The 2026-11-06 native port makes the certificate the all-free default.
The [arithmetic and acceptance note](../../../../../dev/research/evaluations/2026-11-06-t-port-data/arithmetic.md)
records formulas, margins, rejected arithmetic choices and guarded failures.
The grid supplies proposals only. Root acceptance requires a strict
contraction self-map, a uniform nuisance-ball inward bound and a positive
Hessian congruence throughout a larger product neighborhood. Geodesic
convexity identifies every conditional nuisance optimum there; the Schur
complement makes the retained profile neighborhood strictly concave.
Every exterior finite shape interval receives a chord upper bound, and the
entire normal tail receives a signed or unsigned bound. At the lower face,
the relative endpoint chord compares the same exact endpoint on both sides.
A positive-scale endpoint additionally needs nuisance root inclusion.

Arithmetic is binary64, with operation margins of 64 epsilon times endpoint
magnitude plus the minimum subnormal; cancellation branches use the operand
magnitudes. Sums have a separate depth-dependent margin.
Normalization uncertainty is propagated from the original observations.
Gamma-ratio recurrences and integrated alternating remainders avoid large
gamma subtraction. Relative normal losses and log1p remainders avoid
subtracting full likelihoods at the normal seam. Certification is in exact
arithmetic conditional on the computed bounds and strict comparisons, with
these practical floating-point margins; it is not outward-rounded interval
arithmetic.

Binary-carry pairwise sums limit accumulation depth. The stable point-score
combination in the linked note preserves shape/nuisance cancellation near
normal. Exact common replication is reduced before fitting and all likelihood
bounds are multiplied back.

All proposal, line-search and neighborhood loops are bounded. The default
cache permits 4096 profiles; shapes beyond 1e14 are a numerical-domain
refusal, never a fitted cap. Unresolved comparisons, singular solves,
nonfinite/domain errors and insufficient coefficient widths stop with
`CertificateTie`, `CertificateBudget` or `NumericalFailure`. No certificate
failure can use the local optimizer-score exception. Location width is
measured in empirical normal-scale units; scale and shape widths are relative.
The all-free width target is gradtol clamped to [1e-7,1e-6]; std checks the
entire transformed standard-deviation interval as well. Held subsets retain
their previous algorithms and convergence rules.

Normal stops carry `(mean,sd)`; positive ordinary-scale t2 stops carry
`(mu,sigma,df=2)`. A bounded collapse stop carries the diagnostic path
`(mu,df)` under the descriptive label `unavailable:t-scale-collapse`, with its limiting likelihood bounds.
That path is not a public density family and does not assign a continuous
density to an atomic weak limit. Budget and numerical stops preserve finite
witnesses where available; unknown global upper bounds remain infinite.

## Decision history

The 2026-11-06 TASK-140 port replaces only the all-free defaults with the native certificate. Held subsets and sstd remain on their existing routes.

The 2026-11-04 TASK-140 study retains the existing default: its whole-profile
value enclosures do not yet establish a certified global coefficient vector.
See the linked certificate study for proofs, counterexamples and reproducible
checks.

The 2026-10-31 fit-speed revision hoists the fixed-df normalizer in the shared
lst conditional search used by held-scale fits. It preserves scale
reconstruction, likelihood grouping, observation order and all nominated/refined
points. See the lst dossier for the shared method; this does not certify the
existing numerical grid globally. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

The shared profile search caches grid-point values, avoiding repeated nested
location profiles without changing the grid, refinement or convergence checks.
This reuse does not certify that the grid finds every mode.

Historical held-subset face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).

At a critical atom b with multiplicity $k=2(n-k)$, the fixed-shape
ordinary t2 supremum is
$-n\log2-3\sum_{x_i\ne b}\log|x_i-b|$, approached by ordinary scale
collapse. It is unattained at positive scale. The endpoint value alone
does not decide the all-free fit. Balanced two-point data instead have a
strictly increasing optimized likelihood throughout $\nu>2$ and select
the normal supremum.

For the symmetric three-point sample with mass $2/3$ at b and $1/6$ each
at $b-d$ and $b+d$, $d>0$, the entire profile is strictly decreasing as
$\nu$ increases over $(2,\infty)$. Its collapse supremum is approached
as $\nu\downarrow2$. Its normal
seam slope is exactly zero, with positive second coefficient $n/4$;
the zero slope does not make the normal the winner. The ordinary fitted
scale satisfies $s^2=d^2(\nu-2)/(3\nu)$ and standard deviation stays
$d/\sqrt3$ along this path. This is a collapse limit, with supremum
$-n\log(2d)$, rather than a positive ordinary-scale t2 fit.

The [root and lower-face derivation](../../../../../dev/research/evaluations/2026-11-05-t-root-data/derivation.md)
proves these endpoint statements and gives a relative chord inequality
that can exclude a whole punctured endpoint neighborhood. It also derives
the analytic Hessian in upper-half-plane coordinates, root inclusion and
uniform nuisance/profile-curvature checks. These arguments underlie the all-free native certificate; held profile searches keep their existing scope.
