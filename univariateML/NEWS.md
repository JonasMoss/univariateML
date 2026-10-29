# univariateML 1.5.0

* Adopt runtime-checked reference repairs for actuar trgamma/invtrgamma and extraDistr truncated normal (including half-normal) and GEV evaluation; document all active repairs.

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
  `mlzinb()` (zero-inflated negative binomial), `mlgpd()` (generalized
  Pareto over a user-set threshold, shape at least -1/2), `mlinvexp()`,
  `mlinvpareto()` and `mlinvparalogis()` (inverse exponential, inverse
  Pareto, inverse paralogistic).

* `mltriang()` (triangular; `a`, `b`, `c` can each be held fixed) and
  `mlhuber()` (Huber density with `epsilon` held fixed, default 1.345).

* `mlgev()` (generalized extreme value, shape in [-1/2, 1]),
  `mltrgamma()` and `mlinvtrgamma()` (generalized gamma and its
  reciprocal), `mltnorm()` (truncated normal with known bounds),
  `mlhyper()` (hypergeometric with known draws, `k` required) and
  `mlpig()` (Poisson-inverse Gaussian).

* `mldweibull()` (discrete Weibull type I) and `mlbbinom()` (beta-binomial
  with known `size`; its `qml()` inverts the reference CDF because
  extraDistr has no `qbbinom`).

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

* `qml()` for `mlzipf()` fits no longer returns one less than the true
  quantile: `sads::qzipf()` (0.6.5) is off by one, so the package inverts
  the CDF itself while that defect persists.

* `mlinvgamma()`'s coefficient names are unchanged (`alpha`, `beta`)
  and continue to match `extraDistr::dinvgamma`.

* All other `coef(ml<id>(x))` names are preserved exactly. See
  `CANONICAL_NAMES.md` for the authoritative reference table.
