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
    gradtol: Tolerance on the analytic gradient norm of the total negative log-likelihood in standardized location, log scale and log(df - 1) coordinates; fixed step and objective backstops also apply.
    iterlim: Maximum iterations for each EM start and each BFGS refinement.
log_density: loggamma((df+1)/2) - loggamma(df/2) - log(df*pi)/2 - log(sigma) - (df+1)*log(1+((x-mu)/sigma)**2/df)/2
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale tends to zero at the fitted or held location.
    detect: The collapse exponent is negative at an attainable location; equality is treated separately as an uncertified boundary.
    status: Unbounded
    message: "scale"
  - name: scale collapse
    firmness: proved
    fixed: [df]
    when: "The scale tends to zero at the free or held location."
    detect: 'The location has multiplicity $k>\nu(n-k)$ in a sample of size $n$.'
    status: Unbounded
  - name: tie equality boundary
    firmness: argued
    fixed: [df]
    when: "The scale tends to zero at the free or held location."
    detect: 'A location has multiplicity $k=\nu(n-k)$. Equality needs separate analysis. Balanced two-point Cauchy data can have attained, nonunique fits; the remaining diagnosed equality cases are not certified as regular maxima.'
    status: ScaleBoundary
  - model: norm
    firmness: proved
    when: 'The shape $\nu$ tends to infinity, retaining finite location and scale.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: NormalBoundary
    numerical: true
holds: []
examples: |
  x <- diff(log(datasets::EuStockMarkets[, "DAX"]))
  mllst(x)

group: normal and Student
construction:
- location-scale
relations: {}
fit:
  method: multistart
  certified: partial
  note: 'lst/README.md, Details; lst/mle.hpp (public fitter and held overloads); lst/test.cpp: With mu and df held,
    scale has a strictly monotone score with the documented tie criterion; free location/df uses EM or multistart
    profiles without a global certificate.'
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

$\sigma$ is a scale, not a standard deviation. With location free, estimating shape restricts $\nu$ to be at least 1 and includes the attained Cauchy member at $\nu=1$. Unrestricted positive degrees of freedom allow unbounded likelihood through scale collapse. The all-free fitter requires at least three observations and rejects samples in which any value occurs at least half the time, including equality. This equality rejection is not a proof of mathematical non-existence; Cauchy equality can admit nonunique attained fits.

A multistart profile grid and joint refinement compare finite fits with Cauchy and normal fits. The joint problem need not be unimodal; the search does not certify a global maximum. Log-likelihood differences within `1e-8` per observation are treated as ties, preferring Cauchy and then other attained finite members. On the unresolved normal approach ($\nu\ge10^5$) a numerical tie selects the normal boundary.

With $\nu$ held fixed, only location and scale are estimated by monotone
EM, stopping when the norm of the scale-standardized location and log-scale
scores is below the requested score tolerance. Failure to reach that tolerance is an error.
The normal escape does not apply to this subset.
Fixed $\nu$ may be any positive finite value when its collapse condition
permits a finite MLE.
For multiplicity $k$ in a sample of size $n$, scale collapse has likelihood
proportional to $\sigma^{\nu(n-k)-k}$. A negative exponent is reported as
unbounded; equality is reported separately as a scale-collapse boundary,
without claiming unboundedness or certifying a regular MLE (Cauchy equality
can have nonunique attained fits). Strictly positive exponents for every tie
permit the regular fixed-parameter fit.


Conditional fits preserve held location $\mu$ and scale $\sigma$. With location held and scale free, only ties at that location contribute to collapse: the exponent is $\nu(n-k)-k$. A negative exponent is unbounded; at equality and fixed shape the collapse supremum is not attained. Free shape has lower bound one with location free; held location exempts that bound. The conditional normal face is compared in either case. Holding scale removes scale collapse. Shape and held-scale location profiles use numerical multistart searches; they do not certify isolation of every mode.

With shape free, any observation equal to the held location permits scale collapse
through sufficiently small positive shape when scale is free, and fitting stops.

With $\mu$ held, the lower shape restriction is exempt. The fitter searches
the wider shape domain and diagnoses collapse separately when observations
equal the held location and scale is free.

The [2026-11-04 certificate study](../../../../../dev/research/evaluations/2026-11-04-t-cert-report.md)
proves geodesic convexity of the fixed-shape nuisance objective for
$\nu\ge1$, gives explicit nuisance containment and finite-interval profile
upper bounds, and derives the refitted normal-seam expansion. With $z_i=(x_i-\bar x)/s_N$ and
$s_N^2=n^{-1}\sum_i(x_i-\bar x)^2$, its first coefficient in
$\xi=1/\nu$ is $n(\overline{z^4}-3)/4$. A negative coefficient certifies
only local preference for the normal limit: the study retains a sample with
negative slope and a strictly better finite fit. The research value bounds
do not certify coefficient accuracy or replace the current fitter. A production
global certificate still requires bounded arithmetic near the normal seam,
isolation of stationary roots and comparisons with equality faces.

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

## Decision history

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

With location and degrees of freedom held, the log-scale score is strictly decreasing from $\nu(n-k)-k$ to $-n$, where $k$ observations equal the held location. Under the strict existence inequality the unique root is the global scale maximum. The solver expands until both score signs are verified within the representable scale range, bisects, and requires absolute total log-scale score below $10^{-3}$ at the represented result. Residual log magnitudes avoid squared-residual overflow. Missing brackets and failed score checks return `IterationFailure`; no search endpoint is returned as an estimate.

Face comparisons retain the family tolerance but stop with the existing numerical-failure status when `core/face_compare.hpp` cannot resolve the decision after its floating-point rounding allowance. This allowance concerns computed likelihood comparisons, not optimizer convergence or global optimality; see the [comparison analysis](../../../../../dev/research/evaluations/2026-11-02-face-compare-report.md).

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
uniform nuisance/profile-curvature checks. These statements do not
certify the current production multistart dispatch.
