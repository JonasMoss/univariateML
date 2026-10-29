# Canonical R-package names

This file is the **authoritative reference** for the user-facing R
interface of every estimator in `univariateML`. It captures the
historic param names, model display names, and external-package
density references that downstream code may rely on.

## Source of truth

Commit **c0b9949** (`Replace \() with function.`, 2025-03-03) — the
last state of the R package before the cppunivariate codegen pivot.
Each `R/ml<id>.R` there carries a
`univariateML_metadata$ml<id> <- list(model=..., density=..., names=c(...))`
block. The `names = c(...)` vector is the canonical R-coef name; users
have downstream code referencing those names via `coef(mlfoo(x))`.

## Cross-check rule

For each distribution, three things must agree with the `coef names`
column below:

1. The corresponding `cppunivariate/include/cppunivariate/distributions/<id>/metadata.hpp`
   `params[]` first-string entries.
2. The `Rcpp::_["..."] = res....` keys emitted by `mlfoo_` in
   `univariateML/src/ml_exports.cpp`.
3. The formal arguments of the external `pkg::dfoo` referenced in
   `density` (so the resolver in `R/code_helpers.R` can pre-fill
   formals via `formals(fun)[[name]] <- value`).

C++ result-struct field names (e.g. `CauchyResult.mu`) are an
**internal** concern and are explicitly allowed to diverge from the
R-canonical names. The R-side bridge happens at ml_exports.cpp,
where each estimator's emit list aliases struct fields to canonical
R names — e.g. `Rcpp::_["location"] = res.mu`.

## Distributions (42)

| id | model display | coef names | density (external) | support |
|----|---------------|-----------|---------------------|---------|
| bbinom     | Beta-binomial           | size, alpha, beta  | extraDistr::dbbinom       | [0, size] integer (size required) |
| bnbinom    | Beta negative binomial | size, alpha, beta | extraDistr::dbnbinom | [0, ∞) integer |
| beta       | Beta                    | shape1, shape2     | stats::dbeta              | (0, 1) open      |
| betapr     | BetaPrime               | shape1, shape2, scale | extraDistr::dbetapr       | [0, ∞)           |
| binom      | Binomial                | size, prob         | stats::dbinom             | [0, ∞) integer   |
| burr       | Burr                    | shape1, shape2, scale | actuar::dburr             | (0, ∞) open      |
| cmb | Conway--Maxwell--binomial | size, prob, nu | dcmb (package-defined) | [0, size] integer (size required) |
| cauchy     | Cauchy                  | location, scale    | stats::dcauchy            | (-∞, ∞) open     |
| dunif      | Discrete uniform        | min, max           | extraDistr::ddunif        | (-∞, ∞) integer  |
| dweibull   | Discrete Weibull        | shape1, shape2     | extraDistr::ddweibull     | [0, ∞) integer   |
| exp        | Exponential             | rate               | stats::dexp               | [0, ∞)           |
| fatigue    | Birnbaum-Saunders       | alpha, beta, mu    | extraDistr::dfatigue      | [0, ∞)           |
| gamma      | Gamma                   | shape, rate        | stats::dgamma             | (0, ∞) open      |
| ged        | Generalized Error       | mean, sd, nu       | fGarch::dged              | (-∞, ∞) open     |
| geom       | Geometric               | prob               | stats::dgeom              | [0, ∞) integer   |
| gompertz   | Gompertz                | a, b               | extraDistr::dgompertz     | [0, ∞)           |
| gpd        | Generalized Pareto      | mu, sigma, xi     | extraDistr::dgpd          | (mu, mu-sigma/xi) if xi < 0; (mu, ∞) otherwise |
| gev | Generalized extreme value | mu, sigma, xi | extraDistr::dgev | parameter dependent |
| gumbel     | Gumbel                  | mu, sigma          | extraDistr::dgumbel       | (-∞, ∞) open     |
| huber      | Huber                   | mu, sigma, epsilon | extraDistr::dhuber       | (-∞, ∞) open     |
| hyper | Hypergeometric | m, n, k | stats::dhyper | [0, ∞) integer |
| nhyper | Negative hypergeometric | n, m, r | extraDistr::dnhyper | [0, ∞) integer |
| invburr    | Inverse Burr            | shape1, shape2, scale | actuar::dinvburr          | (0, ∞) open      |
| invgamma   | InvGamma                | alpha, beta        | extraDistr::dinvgamma     | (0, ∞) open      |
| invgauss   | Inverse Gaussian        | mean, shape        | actuar::dinvgauss         | (0, ∞) open      |
| invexp | Inverse exponential | rate | actuar::dinvexp | (0, ∞) open |
| invpareto | Inverse Pareto | shape, scale | actuar::dinvpareto | (0, ∞) open |
| invparalogis | Inverse paralogistic | shape, scale | actuar::dinvparalogis | (0, ∞) open |
| invweibull | InverseWeibull          | shape, rate        | actuar::dinvweibull       | (0, ∞) open      |
| invtrgamma | Inverse transformed gamma | shape1, shape2, scale | actuar::dinvtrgamma | (0, ∞) open |
| kumar      | Kumaraswamy             | a, b               | extraDistr::dkumar        | (0, 1) open      |
| laplace    | Laplace                 | mu, sigma          | extraDistr::dlaplace      | (-∞, ∞) open     |
| lgamma     | Loggamma                | shapelog, ratelog  | actuar::dlgamma           | (1, ∞) open      |
| lgser      | Logarithmic series      | theta              | extraDistr::dlgser        | [1, ∞) integer   |
| llogis     | Loglogistic             | shape, rate        | actuar::dllogis           | [0, ∞)           |
| lnorm      | Lognormal               | meanlog, sdlog     | stats::dlnorm             | (0, ∞) open      |
| logis      | Logistic                | location, scale    | stats::dlogis             | (-∞, ∞) open     |
| logitnorm  | LogitNormal             | mu, sigma          | logitnorm::dlogitnorm     | (0, 1) open      |
| lomax      | Lomax                   | lambda, kappa      | extraDistr::dlomax        | (0, ∞) open      |
| naka       | Nakagami                | shape, scale       | nakagami::dnaka           | (0, ∞) open      |
| nbinom     | Negative binomial       | size, prob         | stats::dnbinom            | [0, ∞) integer   |
| hcauchy    | Half-Cauchy             | sigma            | extraDistr::dhcauchy       | [0, ∞) |
| ht         | Half-t                  | nu, sigma        | extraDistr::dht            | [0, ∞) |
| tnorm      | Truncated Normal        | mean, sd, a, b    | extraDistr::dtnorm         | (a, b) open, known bounds |
| norm       | Normal                  | mean, sd           | stats::dnorm              | (-∞, ∞) open     |
| paralogis  | Paralogistic            | shape, scale [†]   | actuar::dparalogis        | (0, ∞) open      |
| pareto     | Pareto                  | a, b               | extraDistr::dpareto       | [0, ∞)           |
| pig        | Poisson-inverse Gaussian | mean, shape       | actuar::dpoisinvgauss      | [0, ∞) integer   |
| pois       | Poisson                 | lambda             | stats::dpois              | [0, ∞) integer   |
| power      | PowerDist               | alpha, beta        | extraDistr::dpower        | [0, ∞)           |
| rayleigh   | Rayleigh                | sigma              | extraDistr::drayleigh     | [0, ∞)           |
| sged       | Skew Generalized Error  | mean, sd, nu, xi   | fGarch::dsged             | (-∞, ∞) open     |
| sn | Azzalini skew-normal | xi, omega, alpha | dsn (package-defined) | real |
| snorm      | Skew Normal             | mean, sd, xi       | fGarch::dsnorm            | (-∞, ∞) open     |
| sstd       | Skew Student-t          | mean, sd, nu, xi   | fGarch::dsstd             | (-∞, ∞) open     |
| std       | Student-t                | mean, sd, nu       | fGarch::dstd              | (-∞, ∞) open     |
| triang | Triangular | a, b, c | extraDistr::dtriang | [a, b] |
| trgamma    | Transformed gamma       | shape1, shape2, scale | actuar::dtrgamma       | (0, ∞) open      |
| unif       | Uniform                 | min, max           | stats::dunif              | (-∞, ∞) open     |
| weibull    | Weibull                 | shape, scale       | stats::dweibull           | (0, ∞) open      |
| zip        | Zero-inflated Poisson   | lambda, pi         | extraDistr::dzip          | [0, ∞) integer   |
| zinb       | Zero-inflated negative binomial | size, prob, pi | extraDistr::dzinb | [0, Inf) integer |
| zipf       | Zipf                    | N, s               | dzipf (package-defined)               | [1, ∞) integer   |
| ztpois | Zero-truncated Poisson | lambda | actuar::dztpois | [1, ∞) integer |
| ztgeom | Zero-truncated geometric | prob | actuar::dztgeom | [1, ∞) integer |
| ztnbinom | Zero-truncated negative binomial | size, prob | actuar::dztnbinom | [1, ∞) integer |
| ztbinom | Zero-truncated binomial | size, prob | actuar::dztbinom | [1, ∞) integer |
| zmpois | Zero-modified Poisson | lambda, p0 | actuar::dzmpois | [0, ∞) integer |
| zmgeom | Zero-modified geometric | prob, p0 | actuar::dzmgeom | [0, ∞) integer |
| zmnbinom | Zero-modified negative binomial | size, prob, p0 | actuar::dzmnbinom | [0, ∞) integer |
| gb2 | Transformed beta (GB2) | shape1, shape2, shape3, scale | actuar::dtrbeta | (0, ∞) |
| zmbinom | Zero-modified binomial | size, prob, p0 | actuar::dzmbinom | [0, ∞) integer |
| zmlogarithmic | Zero-modified logarithmic | prob, p0 | actuar::dzmlogarithmic | [0, ∞) integer |
| zib | Zero-inflated binomial | size, prob, pi | extraDistr::dzib | [0, ∞) integer |
| tpois | Truncated Poisson | lambda, a, b | extraDistr::dtpois | [0, ∞) integer |
| tbinom | Truncated binomial | prob, a, b, size | extraDistr::dtbinom | [0, ∞) integer |
| dlaplace | Discrete Laplace | location, scale | extraDistr::ddlaplace | (-∞, ∞) integer |

[†] **Drift**: historic c0b9949 fitted only `shape` with rate fixed at
1; the current cppunivariate-backed `mlparalogis` fits both `shape`
and `scale`. Accepted as a deliberate generalisation; documented in
`NEWS.md` for the 1.5.0 release.

## C++ ↔ R name mapping (where they differ)

For most distributions, the cppunivariate result-struct field names
match the R-canonical coef names (e.g. `LomaxResult.lambda, .kappa`).
For seven distributions the C++ struct uses mathematical-convention
names while the R interface uses historic ones; the bridge lives in
`univariateML/src/ml_exports.cpp`:

| id        | C++ struct fields  | R coef names      |
|-----------|--------------------|-------------------|
| beta      | alpha, beta        | shape1, shape2    |
| betapr    | alpha, beta, scale | shape1, shape2, scale |
| cauchy    | mu, scale          | location, scale   |
| invgamma  | shape, scale       | alpha, beta       |
| invgauss  | mu, lambda         | mean, shape       |
| logis     | mu, scale          | location, scale   |
| pareto    | shape, min         | a, b              |
| zipf      | N, shape           | N, s              |

These divergences are **intentional**: the C++ struct names follow
the natural-mathematics convention for that distribution, while the
R names follow what users have written downstream code against. If
later we want to unify, the lever is the C++ struct field rename
inside cppunivariate; the R-side names are frozen.
