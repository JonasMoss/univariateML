# Canonical R-package names

This table records user-facing coefficient names, model display names and
reference densities. The executable source of truth is each distribution's
`README.md` and `adapter.cpp` under
`univariateML-cpp/include/cppunivariate/distributions/`.

The historical baseline was commit **c0b9949** (2025-03-03). The parameter
rules adopted on 2026-10-29 extend that baseline: gamma, inverse exponential,
inverse Weibull and log-logistic use scale; the half families add a translated
threshold `mu`; the hypergeometric families replace black-ball count `n` with
population size `N`.

Coefficient names and order must agree between README `params`, adapter
`estimates`, generated metadata and R wrappers. Reference argument names agree
except for declared `r.args` expressions and `r.shift`: hypergeometric references
receive `n = N - m`, and half-family references operate at `x - mu`. C++ result
field names remain internal.

## Distributions

| id | model display | coef names | density (selected) | support |
|----|---------------|-----------|---------------------|---------|
| pareto2 | Pareto type II | shape, scale, min | dpareto2 | (min, ∞), min required |
| pareto3 | Pareto type III | shape, scale, min | dpareto3 | (min, ∞), min required |
| pareto4 | Pareto type IV | shape1, shape2, scale, min | dpareto4 | (min, ∞), min required |
| fpareto | Feller-Pareto | shape1, shape2, shape3, scale, min | dfpareto | (min, ∞), min required |
| nsbeta | Nonstandard beta | shape1, shape2, min, max | dnsbeta | (min, max), both bounds required |
| frechet | Frechet | lambda, mu, sigma | dfrechet | (mu, ∞), lambda >= 1 if mu free |
| bbinom     | Beta-binomial           | size, alpha, beta  | dbbinom       | [0, size] integer (size required) |
| bnbinom    | Beta negative binomial | size, alpha, beta | dbnbinom | [0, ∞) integer |
| beta       | Beta                    | shape1, shape2     | stats::dbeta              | (0, 1) open      |
| betapr     | Beta prime              | shape1, shape2, scale | dbetapr       | [0, ∞)           |
| binom      | Binomial                | size, prob         | stats::dbinom             | [0, ∞) integer   |
| burr       | Burr                    | shape1, shape2, scale | dburr             | (0, ∞) open      |
| cmb | Conway--Maxwell--binomial | size, prob, nu | dcmb (package-defined) | [0, size] integer (size required) |
| cauchy     | Cauchy                  | location, scale    | stats::dcauchy            | (-∞, ∞) open     |
| dunif      | Discrete uniform        | min, max           | ddunif        | (-∞, ∞) integer  |
| dnorm | Discretized normal | mean, sd | ddnorm | integer |
| dgamma | Discretized gamma | shape, rate | ddgamma | nonnegative integer |
| dweibull   | Discrete Weibull        | shape1, shape2     | ddweibull     | [0, ∞) integer   |
| exp        | Exponential             | rate               | stats::dexp               | [0, ∞)           |
| fatigue    | Birnbaum-Saunders       | alpha, beta, mu    | dfatigue      | [0, ∞)           |
| gamma      | Gamma                   | shape, scale        | stats::dgamma             | (0, ∞) open      |
| ged        | Generalized Error       | mean, sd, nu       | dged              | (-∞, ∞) open     |
| geom       | Geometric               | prob               | stats::dgeom              | [0, ∞) integer   |
| gompertz   | Gompertz                | a, b               | dgompertz     | [0, ∞)           |
| gpd        | Generalized Pareto      | mu, sigma, xi     | dgpd          | (mu, mu-sigma/xi) if xi < 0; (mu, ∞) otherwise |
| gev | Generalized extreme value | mu, sigma, xi | dgev | parameter dependent |
| gumbel     | Gumbel                  | mu, sigma          | dgumbel       | (-∞, ∞) open     |
| huber      | Huber                   | mu, sigma, epsilon | dhuber       | (-∞, ∞) open     |
| hyper | Hypergeometric | m, N, k | stats::dhyper | [0, ∞) integer |
| nhyper | Negative hypergeometric | m, N, r | dnhyper | [0, ∞) integer |
| invburr    | Inverse Burr            | shape1, shape2, scale | dinvburr          | (0, ∞) open      |
| invgamma   | InvGamma                | alpha, beta        | dinvgamma     | (0, ∞) open      |
| invgauss   | Inverse Gaussian        | mean, shape        | dinvgauss         | (0, ∞) open      |
| invexp | Inverse exponential | scale | dinvexp | (0, ∞) open |
| invpareto | Inverse Pareto | shape, scale | dinvpareto | (0, ∞) open |
| invparalogis | Inverse paralogistic | shape, scale | dinvparalogis | (0, ∞) open |
| invweibull | Inverse Weibull         | shape, scale        | dinvweibull       | (0, ∞) open      |
| invtrgamma | Inverse transformed gamma | shape1, shape2, scale | dinvtrgamma | (0, ∞) open |
| kumar      | Kumaraswamy             | a, b               | dkumar        | (0, 1) open      |
| laplace    | Laplace                 | mu, sigma          | dlaplace      | (-∞, ∞) open     |
| lgamma     | Log-gamma               | shapelog, ratelog  | dlgamma           | (1, ∞) open      |
| lgser      | Logarithmic series      | theta              | dlgser        | [1, ∞) integer   |
| llogis     | Log-logistic            | shape, scale        | dllogis           | [0, ∞)           |
| lnorm      | Lognormal               | meanlog, sdlog     | stats::dlnorm             | (0, ∞) open      |
| logis      | Logistic                | location, scale    | stats::dlogis             | (-∞, ∞) open     |
| logitnorm  | Logit-normal            | mu, sigma          | dlogitnorm     | (0, 1) open      |
| lomax      | Lomax                   | lambda, kappa      | dlomax        | (0, ∞) open      |
| naka       | Nakagami                | shape, scale       | dnaka           | (0, ∞) open      |
| nbinom     | Negative binomial       | size, prob         | stats::dnbinom            | [0, ∞) integer   |
| hnorm      | Translated Half-Normal | mu, sigma | dhnorm | [mu, ∞), mu required |
| hcauchy    | Half-Cauchy             | mu, sigma        | dhcauchy       | [mu, ∞), mu required |
| ht         | Half-t                  | mu, nu, sigma    | dht            | [mu, ∞), mu required |
| norm with `truncate=c(a,b)` | Configured norm | mean, sd | `dml` | Base support intersected with the configured law |
| norm       | Normal                  | mean, sd           | stats::dnorm              | (-∞, ∞) open     |
| paralogis  | Paralogistic            | shape, scale [†]   | dparalogis        | (0, ∞) open      |
| pareto     | Pareto                  | a, b               | dpareto       | [0, ∞)           |
| pig        | Poisson-inverse Gaussian | mean, shape       | dpig      | [0, ∞) integer   |
| pois       | Poisson                 | lambda             | stats::dpois              | [0, ∞) integer   |
| power      | Power                   | alpha, beta        | dpower        | [0, ∞)           |
| rayleigh   | Rayleigh                | sigma              | drayleigh     | [0, ∞)           |
| sged       | Skew Generalized Error  | mean, sd, nu, xi   | dsged             | (-∞, ∞) open     |
| skellam | Skellam | mu1, mu2 | dskellam (package-defined) | (-∞, ∞) integer |
| slash | Slash | mu, sigma | dslash | real |
| bhatt | Bhattacharjee | mu, sigma, a | dbhatt | real |
| sn | Azzalini skew-normal | xi, omega, alpha | dsn (package-defined) | real |
| snorm      | Skew Normal             | mean, sd, xi       | dsnorm            | (-∞, ∞) open     |
| sstd       | Skew Student-t          | mean, sd, nu, xi   | dsstd             | (-∞, ∞) open     |
| std       | Student-t                | mean, sd, nu       | dstd              | (-∞, ∞) open     |
| tlambda | Tukey lambda | mu, sigma, lambda | dtlambda (package-defined) | real; bounded when lambda > 0 |
| triang | Triangular | a, b, c | dtriang | [a, b] |
| trgamma    | Transformed gamma       | shape1, shape2, scale | dtrgamma       | (0, ∞) open      |
| unif       | Uniform                 | min, max           | stats::dunif              | (-∞, ∞) open     |
| weibull    | Weibull                 | shape, scale       | stats::dweibull           | (0, ∞) open      |
| pois with `zero="inflated"` | Configured pois | lambda, pi | `dml` | Base support intersected with the configured law |
| nbinom with `zero="inflated"` | Configured nbinom | size, prob, pi | `dml` | Base support intersected with the configured law |
| zeta with `truncate=c(1,N)` | Configured zeta | s | `dml` | Base support intersected with the configured law |
| pois with `zero="truncated"` | Configured pois | lambda | `dml` | Base support intersected with the configured law |
| geom with `zero="truncated"` | Configured geom | prob | `dml` | Base support intersected with the configured law |
| nbinom with `zero="truncated"` | Configured nbinom | size, prob | `dml` | Base support intersected with the configured law |
| binom with `zero="truncated"` | Configured binom | size, prob | `dml` | Base support intersected with the configured law |
| pois with `zero="modified"` | Configured pois | lambda, p0 | `dml` | Base support intersected with the configured law |
| geom with `zero="modified"` | Configured geom | prob, p0 | `dml` | Base support intersected with the configured law |
| nbinom with `zero="modified"` | Configured nbinom | size, prob, p0 | `dml` | Base support intersected with the configured law |
| genbeta | Generalized beta of the first kind | shape1, shape2, shape3, scale | dgenbeta | (0, scale), scale required |
| gb2 | Transformed beta (GB2) | shape1, shape2, shape3, scale | dgb2 | (0, ∞) |
| binom with `zero="modified"` | Configured binom | size, prob, p0 | `dml` | Base support intersected with the configured law |
| lgser with `zero="modified"` | Configured lgser | theta, p0 | `dml` | Base support intersected with the configured law |
| binom with `zero="inflated"` | Configured binom | size, prob, pi | `dml` | Base support intersected with the configured law |
| pois with `truncate=c(max(0,floor(a)+1),floor(b))` | Configured pois | lambda | `dml` | Base support intersected with the configured law |
| binom with `truncate=c(max(0,floor(a)+1),floor(b))` | Configured binom | prob, size | `dml` | Base support intersected with the configured law |
| dlaplace | Discrete Laplace | location, scale | ddlaplace | (-∞, ∞) integer |

[†] **Drift**: historic c0b9949 fitted only `shape` with rate fixed at
1; the current cppunivariate-backed `mlparalogis` fits both `shape`
and `scale`. Accepted as a deliberate generalisation; documented in
`NEWS.md` for the 1.5.0 release.

## C++ ↔ R name mapping (where they differ)

For most distributions, the cppunivariate result-struct field names
match the R-canonical coef names (e.g. `LomaxResult.lambda, .kappa`).
For seven distributions the C++ struct uses mathematical-convention
names while the R interface uses historic ones; the bridge lives in
`bindings/r/src/ml_exports.cpp`:

| id        | C++ struct fields  | R coef names      |
|-----------|--------------------|-------------------|
| beta      | alpha, beta        | shape1, shape2    |
| betapr    | alpha, beta, scale | shape1, shape2, scale |
| cauchy    | mu, scale          | location, scale   |
| invgamma  | shape, scale       | alpha, beta       |
| invgauss  | mu, lambda         | mean, shape       |
| logis     | mu, scale          | location, scale   |
| pareto    | shape, min         | a, b              |
| zeta with `truncate=c(1,N)`      | N, shape           | N, s              |

These divergences are **intentional**: the C++ struct names follow
the natural-mathematics convention for that distribution, while the
R names follow what users have written downstream code against. If
later we want to unify, the lever is the C++ struct field rename
inside cppunivariate; the R-side names are frozen.
