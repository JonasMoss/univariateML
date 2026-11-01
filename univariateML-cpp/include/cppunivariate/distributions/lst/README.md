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
      reason: df >= 1 prevents free-location scale collapse; ties at a held location
        need a separate diagnostic.
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
    when: Scale tends to zero at the fitted or held location.
    detect: Excessive mass at that location or vanishing residual spread.
    status: Unbounded
    message: "scale"
  - name: scale collapse
    firmness: proved
    fixed: [df]
    when: "tie multiplicity k > df*(n-k)"
    status: Unbounded
  - name: tie equality boundary
    firmness: argued
    fixed: [df]
    when: "tie multiplicity k = df*(n-k)"
    status: ScaleBoundary
  - model: norm
    firmness: proved
    when: '$df \to \infty$'
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
---

The location-scale Student t includes heavy-tailed distributions with infinite variance.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) describes the limiting paths and the scope of the boundary comparisons.

The 2026-10-31 fit-speed revision computes the fixed-df likelihood normalizer
once per conditional nuisance search. It retains the scale reconstruction
exp(log(s)), per-observation grouping and input-order reduction of LstCost, so
repeated evaluations have the same represented values. The shared routine is
also used by held-scale standardized Student-t fits. No grid or stopping rule
changes. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

`sigma` is a scale, not a standard deviation. With location free, estimating shape restricts `df` to be at least 1 and includes the attained Cauchy member at `df = 1`. Unrestricted positive degrees of freedom allow unbounded likelihood through scale collapse. The fitter requires at least three observations and rejects samples in which any value occurs at least half the time, including equality.

A multistart profile grid and joint refinement compare finite fits with Cauchy and normal fits. The joint problem need not be unimodal; the search does not certify a global maximum. Log-likelihood differences within `1e-8` per observation are treated as ties, preferring Cauchy and then other attained finite members. On the unresolved normal approach (`df >= 1e5`) a numerical tie selects the normal boundary.

With `df` held fixed, only location and scale are estimated by monotone
EM, stopping when the norm of the scale-standardized location and log-scale
scores is below `gradtol`. Failure to reach that tolerance is an error.
The normal escape does not apply to this subset.
Fixed `df` may be any positive finite value when its collapse condition
permits a finite MLE.
For multiplicity `k` in a sample of size `n`, scale collapse has likelihood
proportional to `scale^(df*(n-k)-k)`. A negative exponent is reported as
unbounded; equality is reported separately as a scale-collapse boundary,
without claiming unboundedness or certifying a regular MLE (Cauchy equality
can have nonunique attained fits). Strictly positive exponents for every tie
permit the regular fixed-parameter fit.


Conditional fits preserve held mu and sigma. With mu held and sigma free, only ties at that mu contribute to collapse: the exponent is df(n-k)-k. A negative exponent is unbounded; at equality and fixed df the collapse supremum is not attained. Free df has lower bound 1 with mu free; held mu exempts that bound. The conditional normal face is compared in either case. Held sigma removes scale collapse. Shape and held-scale location profiles use numerical multistart searches; they do not certify isolation of every mode.

The shared profile search caches each grid-point value within a search. This
avoids repeating nested location profiles while preserving the grid, refinement,
comparisons and convergence checks; it does not strengthen global guarantees.

With df free, any observation equal to held mu permits scale collapse
through sufficiently small positive df when scale is free, and fitting stops.

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. README hold exemptions apply to free coefficients. Failed
optimizers return a failure status unless the score norm in fitting
coordinates is below `1e-3`; unresolved scalar searches stop.

With `mu` held, the lower shape restriction is exempt. The fitter searches
the wider shape domain and diagnoses collapse separately when observations
equal the held location and scale is free.
