---
id: lst
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mu, sigma > 0 and df > 0; when df and mu are both free require df > 1; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Location-scale Student-t
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0.0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1.0
  kind: regular
- name: df
  symbol: '\nu'
  role: df
  domain: '(0, inf)'
  simulate: 5
  fit_domain:
    lower: 1.0
    restriction:
      mechanism: collapse
      exempt_when_held:
      - - mu
      reason: Estimating df with free mu is restricted to df >= 1 to exclude
        small-shape scale collapse. Holding mu removes this fitting bound,
        but observations equal to that mu still require collapse diagnostics.
  kind: regular
r:
  density: dlst
  sampler: rlst
  control:
    df: Optional held df; NULL estimates it.
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    gradtol: All-free coefficient enclosure width target, clamped between 1e-7 and 1e-6 in normalized location and relative scale/shape units. Held joint fits use the score tolerance for EM nominations and require root inclusion; scalar held fits require coefficient brackets. Shape-only brackets target relative width min(gradtol, 1e-10) and require relative width at most min(gradtol, 1e-7).
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: loggamma((df+1)/2) - loggamma(df/2) - log(df*pi)/2 - log(sigma) - (df+1)*log(1+((x-mu)/sigma)**2/df)/2
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale tends to zero at the fitted or held location.
    detect: The collapse exponent is negative at an attainable location; critical equality uses the separate fixed-shape analysis.
    status: Unbounded
    message: "scale"
  - name: scale collapse
    firmness: proved
    fixed: [df]
    when: "The scale tends to zero at the free or held location."
    detect: 'The location has multiplicity $k>\nu(n-k)$ in a sample of size $n$.'
    status: Unbounded
  - name: tie equality boundary
    firmness: proved
    fixed: [df]
    when: "The scale tends to zero at the free or held location."
    detect: 'At $k=\nu(n-k)$, held location gives an unattained supremum. With free location and shape at least one, equality is unattained except for the balanced two-point Cauchy maximum curve; joint equality below shape one stops numerically.'
    status: ScaleBoundary
  - model: norm
    firmness: proved
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: All-free and shape-only held fits use exhaustive finite-interval and tail certificates. Joint held profiles can certify normal with a strictly negative first-order term, a uniform tail remainder and a complete compact cover; unresolved profiles retain legacy finite or limit decisions marked uncertified.
    status: NormalBoundary
holds: []
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mllst(x)

group: normal and Student
construction:
- location-scale
relations: {}
fit:
  method: profile-search
  certified: partial
  note: 'lst/certificate.hpp; lst/shape_certificate.hpp; lst/signed_face_certificate.hpp: All-free, fixed-shape scalar and closed shape-only fits use global certificates conditional on practical arithmetic bounds. Joint held-shape fits at shape >= 1 and a held-scale Cauchy regime retain their certificates. Constrained normal limits use signed profile tails and compact covers. Unresolved finite or limit nominations retain converged legacy decisions marked uncertified. Symmetric subunit samples can prove global nonuniqueness; generic subunit joint fits remain unresolved.'
areas:
- measurement and errors
classification: classical
classification_reason: A standard model for robust location-scale errors allowing Student degrees of freedom below
  two.
summary: Location-scale Student-t provides a model for robust location-scale errors allowing Student degrees of
  freedom below two.
aliases:
- "location-scale t"
- "Student t"
- "Pearson type VII"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, fitting is restricted to $\nu\ge1$; a finite MLE is not guaranteed because scale collapse or the normal limit can intervene. Global uniqueness is unknown. Holding positive $\nu$ removes the normal escape; collapse has exponent $\nu(n-k)-k$ at a location with $k$ ties. With $\mu$ and $\nu$ held, a unique positive scale maximum exists exactly when $\nu(n-k)>k$; equality gives a finite unattained scale-zero supremum. Holding $\mu$ exempts estimated $\nu$ from the lower fitting bound, and any observation at that location then makes the free-scale, free-$\nu$ likelihood unbounded. Holding $\sigma$ removes scale collapse. Remaining location and shape searches have no general uniqueness proof. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    The scale score at fixed location and shape is $\sum_i(\nu+1)d_i^2/(\nu\sigma^2+d_i^2)-n$. It decreases strictly from $\nu(n-k)-k$ to $-n$ when any residual is nonzero. Its equality case is strictly negative at every positive scale. Free location or shape lacks this monotonicity argument.
---

The location-scale Student t includes heavy-tailed distributions with infinite variance.

## Details

The location $\mu$ is the centre of symmetry, $\sigma$ is the ordinary Student scale, and $\nu$ controls tail weight. The mean exists only for $\nu>1$ and the variance only for $\nu>2$. Holding a coefficient preserves its value while the others are fitted.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) describes the limiting paths and the scope of the boundary comparisons.

$\sigma$ is a scale, not a standard deviation. With location free, estimating shape restricts $\nu$ to be at least 1 and includes the attained Cauchy member at $\nu=1$. Unrestricted positive degrees of freedom allow unbounded likelihood through scale collapse. Strictly more than half the observations at one value makes the all-free likelihood unbounded. At exact half mass the finite Cauchy collapse supremum enters the global comparison; it is not an automatic all-free failure. Balanced two-point data select the normal supremum by an exact monotonicity theorem.

The all-free fitter uses the certificate described below. A grid nominates roots; acceptance requires root inclusion, a uniform profile neighborhood and exclusion of every other shape interval and the normal tail. No fitted shape cap, local residual exception or numerical tie is accepted as a maximum.

With $\nu$ held fixed, only location and scale are estimated by monotone
EM, stopping when the norm of the scale-standardized location and log-scale
scores is below the requested score tolerance. For held shape at least one, this nominates a point for the root inclusion below. Failure to reach the score tolerance is an error; a small residual alone is not the certificate.
The normal escape does not apply to this subset.
Fixed $\nu$ may be any positive finite value when its collapse condition
permits a finite MLE.
For multiplicity $k$ in a sample of size $n$, scale collapse has likelihood
proportional to $\sigma^{\nu(n-k)-k}$. A negative exponent is reported as
unbounded; equality is reported separately as a scale-collapse boundary,
without claiming unboundedness or certifying a regular MLE (Cauchy equality
can have nonunique attained fits). Strictly positive exponents for every tie
permit the regular fixed-parameter fit.


Conditional fits preserve held location $\mu$ and scale $\sigma$. With location held and scale free, only ties at that location contribute to collapse: the exponent is $\nu(n-k)-k$. A negative exponent is unbounded; at equality and fixed shape the collapse supremum is not attained. Free shape has lower bound one with location free; held location exempts that bound. The conditional normal face requires an actual-hold tail certificate. Holding scale removes scale collapse. Held-scale/free-location/free-shape fits can certify the Cauchy face using a uniform shape-score bound. Shape-only fits use exhaustive scalar certificates. Other two-dimensional held profiles retain successful numerical results marked uncertified. At fixed shape, held-scale location fits use exhaustive stationary-point isolation.

With shape free, any observation equal to the held location permits scale collapse
through sufficiently small positive shape when scale is free, and fitting stops.

With $\mu$ held, the lower shape restriction is exempt. Observations equal to that location make the free-scale/free-shape likelihood unbounded; unresolved two-dimensional free-shape cases retain converged numerical fits marked uncertified.

The [2026-11-04 certificate study](../../../../../dev/research/evaluations/2026-11-04-t-cert-report.md)
proves geodesic convexity of the fixed-shape nuisance objective for
$\nu\ge1$, gives explicit nuisance containment and finite-interval profile
upper bounds, and derives the refitted normal-seam expansion. With $z_i=(x_i-\bar x)/s_N$ and
$s_N^2=n^{-1}\sum_i(x_i-\bar x)^2$, its first coefficient in
$\xi=1/\nu$ is $n(\overline{z^4}-3)/4$. A negative coefficient certifies
only local preference for the normal limit: the study retains a sample with
negative slope and a strictly better finite fit. The subsequent root inclusion and uniform-neighborhood arguments supply coefficient and global exclusion checks in the native certificate described below.

When the largest tie multiplicity satisfies $k<n/2$, a finite all-free
maximum exists if and only if some finite admissible point reaches or exceeds
the optimized normal likelihood. Equality is included. The Cauchy member is
attained within the fitting domain; it remains eligible as a finite maximum.
The linked study proves this criterion using uniform nuisance containment
and convergence of the profile to the normal likelihood.

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.


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
cache permits `128 * min(iterlim, 100000)` profiles; shapes beyond 1e14 are a numerical-domain
refusal, never a fitted cap. Unresolved comparisons, singular solves,
nonfinite/domain errors and insufficient coefficient widths stop with
`CertificateTie`, `CertificateBudget` or `NumericalFailure`. No certificate
failure can use the local optimizer-score exception. Location width is
measured in empirical normal-scale units; scale and shape widths are relative.
The all-free width target is gradtol clamped to [1e-7,1e-6]; std checks the
entire transformed standard-deviation interval as well. Shape-only and fixed-shape held subsets use the certificates below; unresolved two-dimensional free-shape profiles retain successful numerical results marked uncertified.

Normal stops carry `(mean,sd)`; positive ordinary-scale t2 stops carry
`(mu,sigma,df=2)`. A bounded collapse stop carries the diagnostic path
`(mu,df)` under the descriptive label `unavailable:t-scale-collapse`, with its limiting likelihood bounds.
That path is not a public density family and does not assign a continuous
density to an atomic weak limit. Budget and numerical stops preserve finite
witnesses where available; unknown global upper bounds remain infinite.

## Decision history

The all-free default uses the native certificate. Held subsets use the fixed-shape and scalar shape certificates below; unresolved joint free-shape profiles retain converged numerical fits marked uncertified.

The 2026-11-04 TASK-140 study retains the existing default: its whole-profile
value enclosures do not yet establish a certified global coefficient vector.
See the linked certificate study for proofs, counterexamples and reproducible
checks.

The 2026-10-31 fit-speed revision computes the fixed-df likelihood normalizer
once per conditional nuisance search. It retains the scale reconstruction
exp(log(s)), per-observation grouping and input-order reduction of LstCost, so
repeated evaluations have the same represented values. The shared routine is
also used by held-scale standardized Student-t fits. No grid or stopping rule
changes. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

The shared profile search caches each grid-point value within a search. This
avoids repeating nested location profiles while preserving the grid, refinement,
comparisons and convergence checks; it does not strengthen global guarantees.

The held-location scale route retains residual log magnitudes to avoid squared-residual overflow; the strengthened bracket acceptance is described below.

At a critical half-mass atom b, the fixed-shape Cauchy supremum is
$-n\log\pi-2\sum_{x_i\ne b}\log|x_i-b|$. It is attained only for
balanced two-point data, with nonunique nuisance fits. That exceptional
Cauchy member is strictly dominated in the all-free problem: for balanced
two-point data the optimized likelihood is strictly increasing in shape
toward the normal supremum. With more support points the Cauchy collapse
endpoint is unattained; its value alone does not decide the all-free fit.

The [root and lower-face derivation](../../../../../dev/research/evaluations/2026-11-05-t-root-data/derivation.md)
proves these endpoint statements and gives a relative chord inequality
that can exclude a whole punctured endpoint neighborhood. It also derives
the analytic Hessian in upper-half-plane coordinates, root inclusion and
uniform nuisance/profile-curvature checks. These arguments underlie the all-free native certificate; held profiles require their own constrained bounds.

## Fixed-shape held certificates

With both location and shape held, the strictly decreasing log-scale score
brackets the unique maximum when the collapse exponent is positive. The
returned log-scale bracket must have width at most $10^{-10}$; score signs
at offsets $5\times10^{-9}$ must additionally exceed a roundoff allowance
$256\epsilon n\max(4+\log(1+\nu)+n,\max_i|w_i|)$, where
$w_i=2(\log|x_i-\mu|-\log s)-\log\nu$ and $s$ is ordinary t scale.
Equality yields the unattained scale-zero supremum with its likelihood.

With ordinary scale and shape held, every stationary location lies in the
sample range. `lst/location_certificate.hpp` covers that range with score
and curvature enclosures, isolates all possible maxima, and compares their
likelihood intervals. Two observations at $\pm b$ have two equal maxima
$\pm\sqrt{b^2-\nu s^2}$ when $b^2>\nu s^2$. Such comparisons stop as
`CertificateTie` with both candidates; no uniqueness is assumed. Degenerate
curvature, insufficient coefficient accuracy and exhausted work stop.
The arithmetic uses the existing practical $64\epsilon$ enclosure margins;
it does not claim outward-rounded intervals.

For joint location/scale with held shape at least one and the strict atom
inequality, geodesic convexity makes an included stationary root global.
EM nominates the point; Newton refinement and a two-dimensional contraction
inclusion verify it. The complete location and relative scale widths must
be below $10^{-6}$. The balanced two-point Cauchy equality keeps its existing
attained, nonunique route after verifying its maximizing curve in physical coordinates. An endpoint-rounded midpoint stops numerically. Subunit-shape joint fits use the symmetry exclusion below when it proves nonuniqueness; other cases stop numerically rather than returning an unchecked EM result.

The [held-subset derivation and validation](../../../../../dev/research/evaluations/2026-11-07-std-held-report.md)
records the proofs, actual-hold normal-seam coefficients, unresolved
free-shape bounds and regression evidence. These gaps keep certification
partial; unresolved two-dimensional free-shape profiles retain converged numerical fits marked uncertified.

With location and ordinary scale held, `shape_certificate.hpp` covers the
whole positive shape axis. The bound $f_\nu(x;\mu,\sigma)\le
\sqrt{\nu}/(2\sigma)$ excludes a neighborhood of zero using a finite
witness. A closed scalar root cover and actual-hold normal-tail bounds
then compare all maxima and the normal supremum. Holding location exempts
shape from the lower fitting restriction; no artificial shape floor is used.

Outside the decreasing-shape Cauchy and certified normal regimes,
converged two-dimensional nominations retain their finite or limit decisions
with `certified=false`. A failed solve still stops; a strictly better
likelihood witness rejects a proposed face.

With scale held and location free, a sufficiently large sample range permits
a uniform-in-location proof that the likelihood strictly decreases in shape
on $\nu\ge1$. The fitter bounds the shape derivative on a finite interval
and closes the infinite tail analytically, then certifies the location fit
at the attained Cauchy endpoint. The [derivation](../../../../../dev/research/evaluations/2026-11-07-std-held-data/derivation.md#held-scale-decreasing-shape-regime)
gives the sufficient bound; failure of that bound implies no shape decision.
Both stages share the certificate evaluation budget. This regime needs
ordinary scale held and does not transfer to held standard deviation.

Collapse equality is evaluated at the supplied binary64 shape, using fused
multiply-add for $\nu(n-k)-k$. A rounded product can otherwise manufacture
equality; for example, stored `8/3` is slightly below the exact 8-to-3
critical ratio and gives strict unboundedness.

Location accuracy with scale held is measured against empirical data spread,
so a large held scale cannot mask an inaccurate location. Scale estimates
must survive logarithmic round-trip checks after conversion to physical
coordinates; an unrepresentable accurate estimate causes a numerical stop.
The [constrained-shape derivation](../../../../../dev/research/evaluations/2026-11-08-std-free-data/derivation.md)
gives the exempt lower-shape tail and actual-hold normal-tail bounds.

At held shape below one, strict atom inequalities imply existence but do not
imply uniqueness. For exactly symmetric data, the fitter bounds the entire
centered scale profile using its monotone scale score. If reflected finite
witnesses both strictly beat that bound, symmetry proves at least two distinct
global maximizers. For example, $(-100,-99,0,99,100)$ at shape $1/2$ passes this
test. Candidate coefficients in this stop are likelihood witnesses, not
maximizer enclosures. Unresolved symmetric and asymmetric cases retain a
numerical stop. Exact symmetry is verified on binary64 input sums.

For held location and free scale, the first normal-profile coefficient is
$n(b_2-3)/4$ about the supplied centre. For held ordinary scale it is
$\sum_i(z_i^4-2z_i^2-1)/4$ about the fitted normal location. Uniform
remainders, nuisance root brackets and the finite shape/location cover
can certify the normal limit despite location modes on the Cauchy face.
The [signed-face derivation](../../../../../dev/research/evaluations/2026-11-08-std-free-data/signed-derivation.md)
explains the actual-hold bounds. Zero or uncertain first-order signs retain
uncertified legacy decisions when the search converges.

## d/p/q/r

At large degrees of freedom, direct Student tails avoid a beta argument whose
complement has rounded away. A normal-tail bridge covers the region where
its relative correction is below binary64 accuracy; unsupported tail
calculations return NaN. The
[extreme-shape checks](../../../../../dev/research/evaluations/2026-11-09-tail-extreme-data/run-targeted.sh)
cover the half-t, standardized-t and skew-t consumers as well.
