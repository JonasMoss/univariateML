# univariateML 1.5.0

## Architecture

* The package is now scoped to maximum-likelihood estimation only.
  The 116 per-distribution `d`/`p`/`q`/`r` functions previously
  re-exported from the package have been removed; users should call
  the canonical implementations in `extraDistr`, `actuar`, `fGarch`,
  `logitnorm`, `nakagami`, or `sads` directly. These packages are
  now in `Imports:` and resolved at runtime by metadata-driven
  dispatch.

## New models

* `mlhnorm()` (translated half-normal), `mllst()` (location-scale t),
  `mlzinb()` (zero-inflated negative binomial), `mlinvexp()`,
  `mlinvpareto()` and `mlinvparalogis()` (inverse exponential, inverse
  Pareto, inverse paralogistic).

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

* `qml()` for `mlzipf()` fits no longer returns one less than the true
  quantile: `sads::qzipf()` (0.6.5) is off by one, so the package inverts
  the CDF itself while that defect persists.

* `mlinvgamma()`'s coefficient names are unchanged (`alpha`, `beta`)
  and continue to match `extraDistr::dinvgamma`.

* All other `coef(ml<id>(x))` names are preserved exactly. See
  `CANONICAL_NAMES.md` for the authoritative reference table.
