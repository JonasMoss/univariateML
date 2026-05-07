# Math backend: Boost.Math

mlcore needs a small set of special functions and dpqr primitives:

- **Special functions**: `digamma`, `trigamma`, `tetragamma`, `psigamma(n, z)`,
  `lgamma`, `lbeta`.
- **Densities** (used inline by some estimator likelihoods):
  `dnorm`, `dpois`, `dgeom`, `dnbinom`.
- **dpqr primitives** (used by `src/distributions/*.cpp` to expose
  `dged`/`pged`/etc. to R users): `pnorm`/`qnorm`, `plogis`/`qlogis`,
  `dgamma`/`pgamma`/`qgamma`, `dt`/`pt`/`qt`.

Everything routes through `src/ml/special/special.hpp` — the single seam
between mlcore and the math backend. Estimator code never imports the
backend directly. Swap this one file to retarget another backend.

## Decision: Boost.Math (via BH)

`mlcore::special::*` calls into `boost::math::*`. Boost.Math is shipped on
CRAN via the [`BH`](https://cran.r-project.org/package=BH) package
(currently 1.87.0.1), pulled in by `LinkingTo: BH` in DESCRIPTION. The
standalone C++ test harness (`tests/cpp/`) finds Boost.Math via:

1. `find_package(Boost CONFIG)` — system Boost (apt: `libboost-dev`,
   dnf: `boost-devel`), preferred for CI.
2. Fallback: `Rscript -e "system.file('include', package='BH')"` — picks
   up BH headers from a local R install, useful for the dev workflow.

Either way: header-only, no shared lib to link.

## Why Boost.Math

- **Portable.** The same C++ core works under R bindings (Rcpp + BH),
  Python bindings (nanobind + system Boost or vendored subset), and
  any other host language. No reliance on R's runtime.
- **Comprehensive.** All special functions and ~half our distribution
  primitives ship directly. The other ~half are 10–30 LoC closed-form
  compositions over Boost primitives (skewed-density family, transform
  family, Burr family, etc.).
- **Well-tested.** SciPy and Stan both build on Boost.Math; ULP-level
  agreement with R is plausible across most call sites. Algorithms are
  well-documented (Boost.Math docs include accuracy tables).
- **Clean API.** Distribution objects (`normal_distribution<>{mu,
  sigma}`), generic `pdf` / `cdf` / `quantile` / `complement` —
  composable, no macro pollution like R's `R_D__0` / `R_DT_val` /
  `Rf_*`.
- **License compatible.** [BSL-1.0](https://www.boost.org/users/license.html)
  is permissive (MIT-style); no attribution-in-binary requirement.

## Trade-offs vs alternatives

| Backend                  | Code we own | Build deps        | Notes                                              |
|--------------------------|-------------|-------------------|----------------------------------------------------|
| **Boost.Math (current)** | ~50 LoC seam | BH (header-only) | Portable to R + Python + standalone.               |
| Rmath via `Rf_*`         | 0           | libR (via LinkTo) | Couples to R runtime; macro pollution.             |
| Vendored R nmath         | ~1500 LoC   | none              | Walked back: heavy maintenance, no win for Python. |
| Cephes vendor            | ~3k LoC     | none              | Older, weaker accuracy; we'd own the math.         |

## What's NOT in Boost.Math

- **Multi-dimensional optimizers** (BFGS, L-BFGS, Nelder-Mead). Boost has
  evolutionary methods (cma_es, differential_evolution, jso) and 1D
  minimization (`brent_find_minima`) only. We keep our hand-rolled
  BFGS + Newton in `src/ml/core/{bfgs,newton}.hpp` (~300 LoC, golden-tested).
- **RNG sampling that respects R's `set.seed()`.** Delegated to R's
  `R::r*` wrappers on R-shell calls (`rsnorm`, `rged`, etc.). The future
  Python shell will use `std::mt19937` with NumPy seed pass-through.

## Distributions: who provides what

Boost ships directly: normal, logistic, gamma, students_t, uniform,
exponential, weibull, beta, lognormal, cauchy, pareto, laplace, rayleigh,
inverse_gamma, inverse_gaussian, skew_normal, extreme_value (=Gumbel),
poisson, geometric, negative_binomial, binomial, Bernoulli, hypergeometric,
triangular, F.

We layer atop Boost primitives (in `src/distributions/`): snorm, sged,
sstd, std, ged, nakagami, logitnorm, zipf, zip — and (planned, replaces
extraDistr/actuar Imports) betapr, fatigue, gompertz, kumar, lgser,
log_logistic, frechet/inverse_weibull, burr, inverse_burr, paralogis,
power, lomax, beta_prime, discrete_uniform.

## Revisit if

- A numerical issue surfaces that Boost upstream has fixed in a later
  release (track BH version on CRAN; the BH update lands the fix
  automatically).
- A user wants to drop the BH `LinkingTo` dependency entirely (e.g.,
  to ship via a CRAN package size budget). Boost.Math has a standalone
  mode (`BOOST_MATH_STANDALONE`) that compiles without Boost.Config et
  al.; vendoring the math headers via `bcp` would shave size at the
  cost of update tracking.
- Cross-language reproducibility (R/Python results matching to ULP)
  becomes a hard requirement. Today, Python users will see scipy.stats
  results when they call into NumPy, Boost.Math results when they call
  into mlcore — those don't agree to ULP, but both are correct to
  within accuracy specs.
