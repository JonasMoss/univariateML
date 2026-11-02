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
    gradtol: Tolerance on the norm of the finite-difference gradient of the total negative log-likelihood in transformed coordinates after centering and scaling the sample to unit standard deviation.
    iterlim: Maximum number of iterations.
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
    detect: The numerically compared limiting face is selected over the retained finite candidates.
    status: 'NormalBoundary'
    numerical: true
  - model: lst
    firmness: proved
    when: 'The shape $\nu$ decreases to two with mean and ordinary t scale fixed; sd diverges.'
    detect: The numerically compared limiting face is selected over the retained finite candidates.
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
  method: multistart
  certified: partial
  note: 'std/README.md, Details; std/mle.hpp (public fitter and held overloads); std/test.cpp: With mean and nu
    held, ordinary-t scale has a strictly monotone score (collapse exponent decides existence); free location or
    nu uses EM or profile searches without a global certificate.'
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

With free degrees of freedom, a tie of multiplicity $k>2(n-k)$ makes the likelihood unbounded; equality is reported as an uncertified scale-collapse boundary rather than as unbounded.

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

## Decision history

The 2026-10-31 fit-speed revision hoists the fixed-df normalizer in the shared
lst conditional search used by held-scale fits. It preserves scale
reconstruction, likelihood grouping, observation order and all nominated/refined
points. See the lst dossier for the shared method; this does not certify the
existing numerical grid globally. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

The shared profile search caches grid-point values, avoiding repeated nested
location profiles without changing the grid, refinement or convergence checks.
This reuse does not certify that the grid finds every mode.
