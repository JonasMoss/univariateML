# Migration to 2.0: zero and truncation modifiers

The following fitters have been removed, with no aliases or redirects. Use the
base fitter with named modifier arguments (all calls below take data `x`):

- `mltbinom`: `mlbinom(x, truncate = c(max(0, floor(a) + 1), floor(b)), size = ...)`.
- `mltnorm`: `mlnorm(x, truncate = c(a, b))`.
- `mltpois`: `mlpois(x, truncate = c(max(0, floor(a) + 1), floor(b)))`.
- `mlzipf`: `mlzeta(x, truncate = c(1, N))`.
- `mlzib`: `mlbinom(x, zero = "inflated", size = ...)`.
- `mlzinb`: `mlnbinom(x, zero = "inflated")`.
- `mlzip`: `mlpois(x, zero = "inflated")`.
- `mlzmbinom`: `mlbinom(x, zero = "modified", size = ...)`.
- `mlzmgeom`: `mlgeom(x, zero = "modified")`.
- `mlzmlogarithmic`: `mllgser(x, zero = "modified")`.
- `mlzmnbinom`: `mlnbinom(x, zero = "modified")`.
- `mlzmpois`: `mlpois(x, zero = "modified")`.
- `mlztbinom`: `mlbinom(x, zero = "truncated", size = ...)`.
- `mlztgeom`: `mlgeom(x, zero = "truncated")`.
- `mlztnbinom`: `mlnbinom(x, zero = "truncated")`.
- `mlztpois`: `mlpois(x, zero = "truncated")`.

The retained window is closed. Old strict integer bounds `(a,b]` map to
`[max(0,floor(a)+1),floor(b)]`; infinite endpoints remain unbounded. Free
old bounds map to `NA` endpoints, and free binomial size requires explicit
`size = NULL`. Normal bounds keep their values. Old Zipf `N` becomes the upper
window endpoint (`NA` when estimated); bounds are attributes, not coefficients.
For zero-modified logarithmic fits, old `prob` is now `theta`; all other parent
and zero-atom coefficient names are retained.

Finite zeta windows allow every finite real exponent, including negative `s`.
Uniformity is `s = 0`; endpoint-only samples can have an infinite-exponent
point-mass supremum. The old nonnegative-shape Zipf boundary claim does not
apply to this wider domain. To reproduce the old data-free Zipf default use
`mlzeta(s = 1, truncate = c(1, 10))`.

# Development version

* Fitters now build fixed distributions when data are omitted, using README defaults. Fully held models with data evaluate the likelihood with zero estimated parameters. Use `rml(n, mlX())` for simulation.

## Bug fixes

* `confint.univariateML()` now honours `Nreps` for bootstrap draws; the named `reps` alias takes precedence if both are supplied.

# univariateML 1.5.0

## Architecture

* extraDistr and actuar are no longer dependencies. `dml`, `pml`, `qml`
  and `rml` use the package's own C++ d/p/q/r for those families, checked
  against independent high-precision truth tables. The fourteen stats
  families continue to use stats. Random streams from `rml` change for
  the switched families.

* All family-specific d/p/q/r are internal. This includes the previously
  exported functions for eighteen own families, which were never released.
  Use `dml`, `pml`, `qml` and `rml` with a fitted object. Imports is now Rcpp.

* Suggests is down to jsonlite, testthat, knitr and rmarkdown. fGarch,
  logitnorm, nakagami and sads are no longer used: the tests compare the
  package's own functions with values frozen from them. The copula
  vignette and the `abalone` data set it used are removed, and with them
  copula and dplyr.

## Parameter rules

* Design quantities and thresholds have no default. `mlbinom`, `mlzib`,
  `mlzmbinom` and `mlztbinom` require `size`; `mltnorm` requires `a` and
  `b`; `mlgpd`, `mlhnorm` and `mlfrechet` require `mu`; `mlhuber`
  requires `epsilon` (1.345 is the conventional choice). Pass a value to
  hold it or `NULL` to estimate it where that is supported. `mlfatigue`
  accepts a held threshold `mu`. A missing value stops with the reason
  and both ways out. `model_select()` records the families it skipped in
  a `skipped` attribute.

* `mlbinom(x, size = NULL)` searches the size without a cap, and a
  cancellation that misplaced very large sizes is fixed. With a tie of
  exactly half the sample at the minimum, `mlgev()` and `mlfrechet()` now
  compare the interior fit with the collapse face instead of refusing
  outright.

* Restrictions, closure and domains:
  - Fits on a fitting restriction warn (class `univariateML_restricted`)
    and record it in a `restricted` attribute: lst `df = 1`, frechet
    `lambda = 1`, gev and gpd shape bounds, Tukey lambda bounds, ged and
    sged `nu = 1`. Holding `mu` exempts the collapse bound for lst,
    frechet, ht and Tukey lambda; sample-specific collapse can still stop
    a fit, and Tukey lambda retains its upper branch bound.
  - `mlgev()` and `mlgpd()` accept shapes down to -1 (was -1/2). At -1 the
    fit is attained (the GPD is uniform); fits in [-1, -1/2) are not
    regular.
  - Support endpoints that depend on a coefficient are closed, and the
    density there is its limit from inside:
    - `mltnorm(x, a = NULL, b = NULL)` estimates the bounds as the sample
      extremes;
    - `mlpower()` puts `alpha` at `max(x)` exactly (`epsilon` is ignored);
    - `dml()` at an estimated endpoint agrees with the fitted likelihood.
  - All-zero samples return the attained point mass instead of an escape:
    nbinom and zinb (`size = 0`), pig (`shape = 0`), cmp (`lambda = 0`),
    cmb (`prob` 0 or 1).
  - A failed optimizer stops with its status unless the score check
    passes at the returned point. The iteration-limit warnings of
    `mlgumbel()`, `mlweibull()` and `mlinvweibull()` are now errors.

* Every coefficient can be held (`p = value`) or estimated (`p = NULL`):
  e.g. `mlgamma(x, shape = 2)`, `mlnorm(x, mean = 0)`,
  `mlweibull(x, shape = 2)`, `mlsstd(x, mean = 0, xi = 1.5)`,
  `mltbinom(x, a = 0, b = 10, size = NULL)`. Each family's README lists
  the remaining exceptions under `holds:`: combinations with no MLE
  (`no-mle`, 60) and combinations not yet supported (`not-implemented`,
  11: beta-binomial and Conway-Maxwell-binomial free sizes with a free
  shape or dispersion, and the hypergeometric designs whose population can
  grow without bound). With the population `N` held, or with `m` held for
  `mlnhyper`, the hypergeometric designs are estimated by exact
  enumeration.
  Existing hold examples include `mlburr(x, scale = 1)` (the 1.x fit),
  `mlstd(x, nu = 5)` and `mllst(x, df = 3)`; scale holds also include
  `mlinvburr`, `mlbetapr` and `mlparalogis`, and size holds include
  `mlbinom` and `mlnbinom`. Held parameters appear in the `fixed` attribute and do not count toward
  the `logLik()` degrees of freedom, AIC or BIC.
  Data-dependent non-existence stops with the reason; numerical searches
  that run out of budget stop instead of returning a capped estimate.

* Parametrization:
  - `mlgamma()`, `mlinvexp()`, `mlinvweibull()` and `mlllogis()` report
    `scale` (= 1/rate) instead of `rate`, and hold it as `scale = `.
    Families fitted through them (inverse gamma, Nakagami, log-gamma,
    Frechet, Pareto III, inverse Burr) changed with them; exponential,
    Lomax, Gompertz and log-gamma keep their rates.
  - `mlhcauchy()` and `mlht()` have a threshold `mu`, like `mlhnorm()`:
    `mu` is required (`mu = 0` is the former fit, `NULL` estimates it as
    the sample minimum). With `mu` free, half-Cauchy has an MLE iff fewer
    than half the observations sit at the minimum, and half-t uses the
    restriction `nu >= 1`.
  - `mlhyper()` and `mlnhyper()` report the population size `N` instead
    of `n = N - m`; `N` is required, and `mlhyper(x, k = , m = , N = NULL)`
    estimates it.
  - Display names: "Inverse Weibull", "Log-logistic", "Log-gamma",
    "Logit-normal", "Beta prime" and "Power" (were "InverseWeibull",
    "Loglogistic", "Loggamma", "LogitNormal", "BetaPrime", "PowerDist").

* Arguments to the `ml***` fitters must match exactly. An unsupported hold
  stops instead of being matched partially to a start value (such as
  `shape0`) and estimated.

## New models

* `mlhnorm()` (translated half-normal), `mllst()` (location-scale t),
  `mlzinb()` (zero-inflated negative binomial), `mlgpd()` (generalized
  Pareto over a user-set threshold, shape at least -1), `mlinvexp()`,
  `mlinvpareto()` and `mlinvparalogis()` (inverse exponential, inverse
  Pareto, inverse paralogistic).

* `mltriang()` (triangular; `a`, `b`, `c` can each be held fixed) and
  `mlhuber()` (Huber density; `epsilon` is held at 1.345 by default and
  estimated with `mlhuber(x, epsilon = NULL)`, which stops with "use
  `mlnorm`" or "use `mllaplace`" when a limit wins).

* `mlgev()` (generalized extreme value, shape in [-1, 1]),
  `mltrgamma()` and `mlinvtrgamma()` (generalized gamma and its
  reciprocal), `mltnorm()` (truncated normal with known bounds),
  `mlhyper()` (hypergeometric with known draws, `k` required) and
  `mlpig()` (Poisson-inverse Gaussian).

* `mldweibull()` (discrete Weibull type I) and `mlbbinom()` (beta-binomial
  with known `size`; evaluation uses the internal d/p/q/r).

* `mlsgomp()` (shifted Gompertz; the exponential is the attained member
  `eta = 0`) with its own `dsgomp()`, `psgomp()`, `qsgomp()`, `rsgomp()`
  (extraDistr has no quantile function and underflows in the tails).

* `mlgb2()` (generalized beta of the second kind, actuar's transformed beta;
  contains the Burr, inverse Burr, beta prime, paralogistic and Lomax
  fitters as members and stops with the winning limit: generalized gamma,
  its inverse, lognormal, Pareto, power, or double Pareto).

* `mlsn()` (Azzalini skew-normal) with its own `dsn()`, `psn()`, `qsn()`,
  `rsn()`. The shape estimate is infinite with positive probability (often:
  about a third of samples of size 50 when the shape is 5); the fitter then
  stops with "use `mlhnorm`" (on `-x` for left skew).

* `mlemg()` (exponentially modified Gaussian) with its own d/p/q/r; stops
  with "use `mlnorm`" or names the translated exponential limit.

* `mlfoldnorm()` (folded normal, `mu >= 0`) with its own d/p/q/r;
  previously available only in VGAM.

* `mlzeta()` and `mlyulesimon()` (discrete power laws on 1, 2, ...), with
  their own d/p/q/r; previously available only in VGAM.

* `mlcmp()` (Conway–Maxwell–Poisson) with its own `dcmp()`, `pcmp()`,
  `qcmp()`, `rcmp()` on a normalizing constant with certified truncation
  bounds; the fitter decides existence exactly and names the geometric or
  adjacent two-point limit otherwise.

* `mlcmb()` (Conway–Maxwell–binomial with known `size`; `nu` may be any
  real number, `nu = 1` is the binomial).

* `mlbnbinom()` (beta negative binomial; Irwin's generalized Waring
  distribution, which includes the Waring and a shifted Yule–Simon law).
  `size` and `beta` are interchangeable, so with both free the fit reports
  the representative with `size <= beta`.

* `mlhcauchy()` and `mlht()` (half-Cauchy and half-t; translated by a
  threshold `mu` since P4),
  and `mlhnorm(x, mu = 0)`: `mu` can now be held, giving extraDistr's
  half-normal. The half-t stops with "use `mlhnorm(x, mu = 0)`" at its
  normal limit; with free df any zero in the data makes its likelihood
  unbounded, and its df profile can have two maxima.

* `mltpois()` and `mltbinom()` (Poisson and binomial truncated to known
  bounds `a < x <= b`; `mltbinom` with known `size`) and `mldlaplace()`
  (discrete Laplace: integer median location, closed-form scale).

* `mlnhyper()` (negative hypergeometric: total draws until the `r`-th
  white ball, with known population `N`; estimates the number of white
  balls `m`).

* `mldnorm()` and `mldgamma()` (discretized normal and gamma, `floor` of
  the continuous variable). Their MLEs exist iff the sample range is at
  least two; data on two adjacent integers stop with "use
  `mlbinom(x - k, size = 1)`". Rounded data with a small spread are such
  samples almost always.

* `mlskellam()` (Skellam, the difference of two Poisson counts) with its
  own `dskellam()`, `pskellam()`, `qskellam()`, `rskellam()`; extraDistr has
  no Skellam CDF or quantile, and its density overflows. The MLE always
  exists; a rate of zero (a Poisson member) is returned as attained.

* `mlpareto2()`, `mlpareto3()`, `mlpareto4()`, `mlfpareto()` (Pareto types
  II–IV and Feller–Pareto) and `mlnsbeta()` (beta on known bounds). With a
  free lower endpoint (or free bounds) their likelihood is unbounded for
  every sample, so `min` (and `max`) must be supplied; the error says why
  and names the equivalent call, e.g. `mlpareto4(x, min = 0)` is `mlburr(x)`.
  With the endpoint held the fit is the corresponding existing fitter on the
  shifted data, escapes included.

* `mlfrechet()` (Fréchet). With `mu` estimated this is the GEV with
  $\xi = 1/\lambda \in (0, 1]$ and stops with "use `mlgumbel`" when the
  GEV shape estimate is not positive; with `mu` held it is the inverse
  Weibull on `x - mu`.

* `mltlambda()` (Tukey lambda with location and scale) with its own
  `dtlambda()`, `ptlambda()`, `qtlambda()`, `rtlambda()` (extraDistr has only
  a standard quantile and sampler). The unrestricted likelihood is unbounded
  as $\lambda \to \infty$, so the fit restricts $\lambda$ to $[-1, 1]$,
  from Cauchy-like tails to the uniform.

* `mlgenbeta()` (generalized beta of the first kind on `(0, scale)`, with
  `scale` held: free, the likelihood is unbounded for every sample). It
  contains the beta, Kumaraswamy and power families, and stops with "use
  `mllgamma`" (on `scale / x`, the unit-gamma limit) or "use `mlpower`" (a
  power law ending below `scale`).

* `mlslash()` (slash distribution) and `mlbhatt()` (Bhattacharjee:
  normal plus uniform). The slash MLE exists when no value holds half the
  sample; `mlbhatt` stops with "use `mlnorm`" or "use `mlunif`" at its two
  limits, which normal and uniform samples reach often (the normal limit in
  about half of normal samples, at any sample size). Evaluation uses the
  internal d/p/q/r, including direct logarithmic tails.

* Zero-truncated `mlztpois()`, `mlztgeom()`, `mlztnbinom()`, `mlztbinom()`;
  zero-modified `mlzmpois()`, `mlzmgeom()`, `mlzmnbinom()`, `mlzmbinom()`,
  `mlzmlogarithmic()`; zero-inflated binomial `mlzib()`.

## User-visible changes

* `mlparalogis()` now fits two parameters (`shape`, `scale`) rather
  than one (`shape`, with `rate` fixed at 1). This is a deliberate
  generalisation that exposes the full paralogistic family. Code
  that read `coef(mlparalogis(x))["shape"]` continues to work; an
  additional `scale` element now appears.

* `mlburr()`, `mlinvburr()` and `mlbetapr()` now also estimate `scale`
  (previously fixed at 1), like `mlparalogis()`. They stop with a pointer
  to the limit model (e.g. `mlweibull`, `mlpareto`, `mlgamma`) when the
  three-parameter MLE does not exist.

* The package now defines its own d/p/q/r where external references are
  broken, heavy or missing: `dzipf()`, `pzipf()`, `qzipf()`, `rzipf()`
  (replacing sads, whose `qzipf` was off by one and which rejected the
  uniform member `s = 0`; sads is no longer imported) and `dcmb()`,
  `pcmb()`, `qcmb()`, `rcmb()` for `mlcmb()`; `dlogitnorm()` and `dnaka()`
  families with the former references' names and arguments (logitnorm and
  nakagami are no longer imported; `pnaka` was positive below zero).
* Own `dstd()`, `dsstd()`, `dged()`, `dsged()`, `dsnorm()` families (and
  their p/q/r) with fGarch's names and arguments; fGarch is no longer
  imported, and far-tail log densities no longer underflow to `-Inf`.

* Runtime reference repairs and missing-reference quantile inversion are
  removed; the internal C++ implementations cover these families.

* `mllomax()` and `mlinvpareto()` decide existence exactly: the moment
  condition `mean(x^2) < 2*mean(x)^2` they used to stop on is only a local
  test, and samples such as `c(1, 100)` have a finite MLE. The fitters now
  search the profile likelihood over a bounded interval that provably
  contains every maximiser, and stop with "use `mlexp`" (`mlinvexp`) only
  when the exponential limit wins.

* `mlstd()` and `mlsstd()` stop when a repeated value makes the likelihood
  unbounded (multiplicity `k > 2 * (n - k)`), instead of returning a finite
  fit; `mlburr()`/`mlinvburr()` with the scale held stop when every
  observation equals that scale.

* An edge-case audit of the fitters: constant data, near ties, extreme
  scales (1e-300 to 1e300), counts near `.Machine$integer.max`, empty
  input and n = 1e6 now give either a correct fit or an honest error
  instead of `NaN` estimates or silently wrong fits. Visible changes:
  `mlzipf()` returns the attained uniform member `s = 0` instead of an
  error; `mlcauchy()` returns an attained (non-unique) maximum for a
  balanced pair of values; `mlgompertz()` and `mlnbinom()` name the limit
  model at the equality case too; large-sample `mlstd()`, `mlsstd()`,
  `mlged()`, `mlsged()` and `mlparalogis()` fits are more accurate.

* `mlinvgamma()`'s coefficient names are unchanged (`alpha`, `beta`)
  and continue to match `extraDistr::dinvgamma`.

* All other `coef(ml<id>(x))` names are preserved exactly. See
  `CANONICAL_NAMES.md` for the authoritative reference table.
