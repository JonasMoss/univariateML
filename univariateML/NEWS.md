# univariateML 1.5.0

## Architecture

* The package is now scoped to maximum-likelihood estimation only.
  The 116 per-distribution `d`/`p`/`q`/`r` functions previously
  re-exported from the package have been removed; users should call
  the canonical implementations in `extraDistr`, `actuar`, `fGarch`,
  `logitnorm` or `nakagami` directly (since replaced by the package's own
  functions with the same names; see below). Imports are now only Rcpp,
  extraDistr and actuar. These packages are
  now in `Imports:` and resolved at runtime by metadata-driven
  dispatch.

* Suggests is down to jsonlite, testthat, knitr and rmarkdown. fGarch,
  logitnorm, nakagami and sads are no longer used: the tests compare the
  package's own functions with values frozen from them. The copula
  vignette and the `abalone` data set it used are removed, and with them
  copula and dplyr.

## New models

* `mlhnorm()` (translated half-normal), `mllst()` (location-scale t),
  `mlzinb()` (zero-inflated negative binomial), `mlgpd()` (generalized
  Pareto over a user-set threshold, shape at least -1/2), `mlinvexp()`,
  `mlinvpareto()` and `mlinvparalogis()` (inverse exponential, inverse
  Pareto, inverse paralogistic).

* `mltriang()` (triangular; `a`, `b`, `c` can each be held fixed) and
  `mlhuber()` (Huber density; `epsilon` is held at 1.345 by default and
  estimated with `mlhuber(x, epsilon = NULL)`, which stops with "use
  `mlnorm`" or "use `mllaplace`" when a limit wins).

* `mlgev()` (generalized extreme value, shape in [-1/2, 1]),
  `mltrgamma()` and `mlinvtrgamma()` (generalized gamma and its
  reciprocal), `mltnorm()` (truncated normal with known bounds),
  `mlhyper()` (hypergeometric with known draws, `k` required) and
  `mlpig()` (Poisson-inverse Gaussian).

* `mldweibull()` (discrete Weibull type I) and `mlbbinom()` (beta-binomial
  with known `size`; its `qml()` inverts the reference CDF because
  extraDistr has no `qbbinom`).

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

* `mlhcauchy()` and `mlht()` (half-Cauchy and half-t with endpoint 0),
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
  about half of normal samples, at any sample size). Their quantiles invert
  the reference CDFs, which extraDistr provides without a quantile function.

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

* Some parameters can be held fixed by passing them by name, e.g.
  `mlburr(x, scale = 1)` (the 1.x fit), `mlstd(x, nu = 5)`,
  `mllst(x, df = 3)`; also `scale` in `mlinvburr()`, `mlbetapr()`,
  `mlparalogis()` and `size` in `mlbinom()`, `mlnbinom()`. Held parameters
  are listed in the `fixed` attribute and are not counted in `logLik()`
  degrees of freedom, AIC or BIC.

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

* Known defects in reference packages that fitted models can reach are
  repaired at runtime, each switching itself off once the reference is
  fixed: `extraDistr` `ptriang`, `pdweibull`, truncated normal d/p/q
  (also used by `mlhnorm()`), and `qgev`/`rgev`/upper-tail `pgev`, and
  `actuar` generalized gamma d/p/q/r. The list is in the package help page
  (`?univariateML`); fitting and `logLik()` never use these functions.

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
