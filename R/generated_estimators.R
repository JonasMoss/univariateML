# This file is auto-generated from r-src via scripts/gen_estimators_from_rsrc.R
# Do not edit by hand.

#' Beta distribution maximum likelihood estimation
#'
#' Uses `stat::nlm` to estimate the parameters of the Beta distribution.
#'
#' For the density function of the Beta distribution see [Beta][stats::Beta].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Ignored.
#' @return `mlbeta` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum
#'    likelihood estimates for `shape1` and `shape2` and the
#'    following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @details For `type`, the option `none` is fastest.
#' @seealso [Beta][stats::Beta] for the Beta density, [nlm][stats::nlm] for the
#'   optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York.
#'
#' @examples
#' AIC(mlbeta(USArrests$Rape / 100))
#' @export
mlbeta <- function(x, na.rm = FALSE, ...) {}


mlbeta_ <- function(x, ...) {
  n <- length(x)
  s <- sum(log(x)) / n
  r <- sum(log1p(-x)) / n

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100

  m <- sum(x) / n
  v <- sum(x^2) / n - m^2
  nu <- m * (1 - m) / v - 1
  b <- log((1 - m) * nu)

  for (i in seq(iterlim)) {
    exp_b <- exp(b)
    digamma_b_r <- digamma(exp_b) - r
    trigamma_b <- trigamma(exp_b)

    # Inverts digamma(s-r+digamma(exp(b)))
    y <- s + digamma_b_r
    a <- 1 / log(1 + exp(-y))
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)

    # Construct derivative and hessian
    f <- exp_b * (digamma_b_r - digamma(a + exp_b))
    df <- f + exp_b^2 * (trigamma_b - trigamma(a + exp_b) * (trigamma_b / trigamma(a) + 1))

    b0 <- b - f / df
    if (abs((b0 - b) / b0) < reltol) break
    b <- b0
  }

  list(
    estimates = c(a, exp_b),
    logLik = n * ((a - 1) * s + (exp_b - 1) * r - lbeta(a, exp_b)),
    i = i
  )
}


#' Beta prime distribution maximum likelihood estimation
#'
#' This function does not estimate the scale parameter for the `BetaPrime`
#'     distribution. Transforms the data and uses `stat::nlm` to estimate
#'     the parameters of the Beta distribution.
#'
#' For the density function of the Beta prime distribution see
#'     [BetaPrime][extraDistr::BetaPrime].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlbeta`][mlbeta].
#' @return `mlbetapr` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape1` and `shape2` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @details For `type`, the option `none` is fastest.
#' @seealso [BetaPrime][extraDistr::BetaPrime] for the Beta prime density,
#'   [nlm][stats::nlm] for the optimizer this function uses, [mlbeta] for
#'   the Beta distribution maximum likelihood estimator.
#' @examples
#' AIC(mlbetapr(USArrests$Rape))
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York.
#' @export
mlbetapr <- function(x, na.rm = FALSE, ...) {}


mlbetapr_ <- function(x, ...) {
  estimates <- mlbeta(x / (x + 1), na.rm = FALSE, ...)
  alpha <- estimates[1]
  beta <- estimates[2]
  logLik <- unname(-length(x) * (lbeta(alpha, beta) -
    (alpha - 1) * mean(log(x)) +
    (alpha + beta) * mean(log(1 + x))))
  list(estimates = estimates, logLik = logLik)
}

#' Binomial distribution maximum likelihood estimation
#'
#' For the density function of the Binomial distribution see
#' [Binomial][stats::dbinom].
#'
#' The estimator computes both the `size` and `prob` parameter by default. Be aware
#'   that the likelihood will often be unbounded. According to Olkin et al. (1981),
#'   the likelihood is unbounded when \eqn{\hat{\mu}/\hat{\sigma}^2 \leq 1},
#'   where \eqn{\hat{\sigma}^2} is the biased sample variance. When the likelihood
#'   is unbounded,the maximum likelihood estimator can be regarded as a [Poisson][stats::dpois]
#'   with `lambda` parameter equal to the mean of the observation.
#'
#' When \eqn{\hat{\mu}/\hat{\sigma}^2 \leq 1} and `size` is not supplied by the user,
#'   an error is cast. If `size` is provided and `size < max(x)`, an error is cast.
#'
#' The maximum likelihood estimator of `size` is unstable, and improvements exist.
#'   See, e.g., Carroll and Lomard (1985) and DasGupta and Rubin (2005).
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... The arguments `size` can be specified to only return the ml of `prob`.
#'    `reltol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#' @return `mlbinom` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `size` and `prob` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' # The likelihood will often be unbounded.
#' \dontrun{
#' mlbinom(ChickWeight$weight)
#' }
#' # Provide a size
#' mlbinom(ChickWeight$weight, size = 400)
#'
#' # Or use mlpoiss, the limiting likelihood of the binomial.
#' mlpois(ChickWeight$weight)
#' @seealso [Binomial][stats::dbinom] for the density.
#' @export
#' @references
#' Olkin, I., Petkau, A. J., & Zidek, J. V. (1981). A comparison of n Estimators for the binomial distribution. Journal of the American Statistical Association, 76(375), 637-642. https://doi.org/10.1080/01621459.1981.10477697
#'
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#'
#' Carroll, R. J., & Lombard, F. (1985). A Note on N Estimators for the Binomial Distribution. Journal of the American Statistical Association, 80(390), 423-426. https://doi.org/10.1080/01621459.1985.10478134
#'
#' DasGupta, A., & Rubin, H. (2005). Estimation of binomial parameters when both n,p are unknown. Journal of Statistical Planning and Inference, 130(1-2), 391-404. https://doi.org/10.1016/j.jspi.2004.02.019


mlbinom <- function(x, na.rm = FALSE, ...) {}


mlbinom_ <- function(x, ...) {
  dots <- list(...)

  n <- length(x)
  x_bar <- mean(x)
  n <- length(x)
  counts <- Rfast::Table(x)
  uniques <- as.numeric(names(counts))

  log_likelihood <- function(size) {
    n * x_bar * log(x_bar) +
      n * (size - x_bar) * log(size - x_bar) -
      n * size * log(size) +
      sum(counts * lchoose(size, uniques))
  }

  if (!is.null(dots$size)) {
    size <- dots$size
    if (size < max(x)) stop("`size` is smaller than `max(x)`.")
    prob <- x_bar / size
    return(list(estimates = c(size, prob), logLik = log_likelihood(size)))
  }

  if (mean(x) / (stats::var(x) * (n - 1) / n) <= 1) {
    stop("The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument.")
  }

  f_over_df <- function(size) {
    size_m_x <- size - x_bar
    f <- n * (log(size_m_x) - log(size)) +
      n * digamma(size + 1) -
      sum(counts * digamma(size - uniques + 1))

    df <- n * x_bar / (size * size_m_x) +
      n * trigamma(size + 1) -
      sum(counts * trigamma(size - uniques + 1))

    f / df
  }

  size0 <- max(x)
  size_cands <- size0
  if (size0 != 1) {
    if (f_over_df(size0) > 0) {
      size_cand <- newton_raphson_1d(f_over_df, size0)
      size_cands <- c(size0, floor(size_cand), ceiling(size_cand))
    }
  }

  log_liks <- sapply(size_cands, log_likelihood)
  size <- size_cands[which.max(log_liks)]

  list(
    estimates = c(size = size, prob = x_bar / size),
    logLik = max(log_liks)
  )
}

#' Burr distribution maximum likelihood estimation
#'
#' The maximum likelihood estimator fails to exist when the data contains no values
#' strictly smaller than 1. Then the likelihood converges
#' to the likelihood of the [Pareto distribution][mlpareto] in this case.
#'
#' This function estimates the only the shape parameters of the Burr distribution.
#' The shape is set to 1.
#'
#' For the density function of the Burr distribution see [Burr][actuar::dburr].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlburr` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape1` and `shape2` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlburr(abalone$length)
#' @seealso [Burr][actuar::dburr] for the Burr density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 20. Wiley, New York.
#' @export
mlburr <- function(x, na.rm = FALSE, ...) {}



mlburr_ <- function(x, ...) {
  if (all(x >= 1)) {
    stop("The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlpareto` to fit a Pareto distribution.")
  }

  n <- length(x)
  log_x <- log(x)
  lx_sum <- sum(log_x)
  f_over_df <- function(c) {
    x_c <- x^c
    x_c_div <- x_c / (x_c + 1)
    g <- sum(log(1 + x_c))
    dg <- sum(log_x * x_c_div)
    d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
    f <- n / c - n * dg / g + lx_sum - dg
    df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
    f / df
  }

  mu <- lx_sum / n
  sigma2 <- sum(log_x^2) / n - mu^2
  c0 <- burr_lmom(mu, sigma2)[2]
  c <- newton_raphson_1d(f_over_df, c0, ...)
  k <- n / sum(log(1 + x^c))

  list(
    estimates = c(k, c),
    logLik = sum(actuar::dburr(x, k, c, log = TRUE))
  )
}

#' Cauchy distribution maximum likelihood estimation
#'
#' Calculates the estimates using `nlm` and an exponential transform of the
#'     location parameter. If `n < 5`, an exact solution is reported. In
#'     the edge case where no maximum likelihood estimator exists and error is
#'     thrown.
#'
#' For the density function of the Cauchy distribution see
#'     [Cauchy][stats::Cauchy].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlcauchy` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `location` and `scale` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Cauchy][stats::Cauchy] for the Cauchy density, [nlm][stats::nlm]
#'   for the optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 16. Wiley, New York.
#' @examples
#' mlcauchy(airquality$Temp)
#' @export
mlcauchy <- function(x, na.rm = FALSE, ...) {}


mlcauchy_ <- function(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  start <- c(m, mad)

  f <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = start))

  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum
  )
}

#' Discrete uniform distribution maximum likelihood estimation
#'
#' For the density function of the Discrete uniform distribution see
#' [DiscreteUniform][extraDistr::ddunif].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... not in use.
#' @return `mldunif` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `min` and `max` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mldunif(corbet)
#' @seealso [DiscreteUniform][extraDistr::ddunif] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mldunif <- function(x, na.rm = FALSE, ...) {}


mldunif_ <- function(x, ...) {
  n <- length(x)
  estimates <- c(min(x), max(x))
  list(
    estimates = estimates,
    logLik = -n * log(estimates[2] - estimates[1] + 1)
  )
}

#' Exponential distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `rate` is the inverse sample mean.
#'
#' For the density function of the exponential distribution see
#'    [Exponential][stats::Exponential].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed? If `FALSE`,
#'   the function fails when `x` contains missing values.
#' @param ... currently affects nothing.
#' @return `mlexp` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured by `match.call`}
#'     \item{`continuous`}{Is the density continuous or discrete?}
#' @examples
#' mlexp(precip)
#' @seealso [Exponential][stats::Exponential] for the exponential density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 19. Wiley, New York.
#' @export
mlexp <- function(x, na.rm = TRUE, ...) {}


mlexp_ <- function(x, ...) {
  estimates <- 1 / mean(x)
  logLik <- length(x) * (log(estimates) - 1)
  list(estimates = estimates, logLik = logLik)
}

#' Gamma distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Birnbaum--Saunders distribution.
#' The parameter `mu` is set to `0`, hence only `alpha` and `beta` are estmated.
#'
#' For the density function of the Birnbaum--Saunders distribution see
#' [BirnbaumSaunders][extraDistr::dfatigue].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `reltol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#' @return `mlfatigue` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `alpha` and `beta`, with `mu=0`, and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlfatigue(precip)
#' @references From, S. G., & Li, L. (2006). Estimation of the parameters of the Birnbaum<U+2013>Saunders distribution. Communications in Statistics: Theory and Methods, 35(12), 2157<U+2013>2169. https://doi.org/10.1080/03610920600853563
#' @export

mlfatigue <- function(x, na.rm = FALSE, ...) {}


mlfatigue_ <- function(x, ...) {
  n <- length(x)
  x_bar <- sum(x) / n
  x_inv_bar <- n / sum(1 / x)

  f_over_df <- function(beta) {
    xi <- n / sum(1 / (x + beta))
    dxi <- sum(1 / (x + beta)^2) * xi^2 / n
    f <- beta^2 - beta * (2 * x_inv_bar + xi) + x_inv_bar * (x_bar + xi)
    df <- 2 * beta - (2 * x_inv_bar + xi) - beta * dxi + x_inv_bar * dxi
    f / df
  }

  # Starting parameter: Method of moments solution.
  sqrt_x <- sqrt(x)
  beta0 <- sum(sqrt_x) / sum(1 / sqrt_x)
  beta <- newton_raphson_1d(f_over_df, beta0, ...)

  alpha <- sqrt(x_bar / beta + beta / x_inv_bar)

  estimates <- c(alpha = alpha, beta = beta, mu = 0)
  logLik <- sum(extraDistr::dfatigue(x, alpha, beta, 0, log = TRUE))
  list(estimates = estimates, logLik = logLik)
}

#' Gamma distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Gamma distribution.
#'
#' For the density function of the Gamma distribution see
#' [GammaDist][stats::GammaDist].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `reltol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#' @return `mlgamma` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape` and `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlgamma(precip)
#' @seealso [GammaDist][stats::GammaDist] for the Gamma density.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation
#' of the parameters of the gamma distribution and their bias."
#' Technometrics 11.4 (1969): 683-690.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 17. Wiley, New York.
#' @export
mlgamma <- function(x, na.rm = FALSE, ...) {}


mlgamma_ <- function(x, ...) {
  n <- length(x)
  mean_hat <- sum(x) / n
  lx_bar <- sum(log(x)) / n
  s <- log(mean_hat) - lx_bar

  f_over_df <- function(shape0) {
    (log(shape0) - digamma(shape0) - s) / (1 / shape0 - trigamma(shape0))
  }

  shape0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s)) * 0.9989 + 0.0102
  shape <- newton_raphson_1d(f_over_df, shape0, ...)
  rate <- shape / mean_hat

  estimates <- c(shape = shape, rate = rate)
  logLik <- n * (shape * log(rate) - lgamma(shape) + (shape - 1) * lx_bar - rate * mean_hat)

  list(estimates = estimates, logLik = logLik)
}

#' Generalized error distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::gedFit].
#'
#' For the density function of the generalized error distribution see
#' [ged][fGarch::ged].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlged` returns an object of [class][base::class] `univariateML`.
#'   This is a named numeric vector with maximum likelihood estimates for the
#'   parameters `mean`, `sd`, `nu` and the following attributes:
#'   \item{`model`}{The name of the model.}
#'   \item{`density`}{The density associated with the estimates.}
#'   \item{`logLik`}{The loglikelihood at the maximum.}
#'   \item{`support`}{The support of the density.}
#'   \item{`n`}{The number of observations.}
#'   \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlged(precip)
#' @seealso [ged][fGarch::ged] for the Student t-density.
#' @references Nelson D.B. (1991); Conditional Heteroscedasticity in Asset
#'   Returns: A New Approach, Econometrica, 59, 347<U+2013>370.
#'
#'   Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat Tails and
#'   Skewness, Preprint.
#' @export
mlged <- function(x, na.rm = FALSE, ...) {}


mlged_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::gedFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

#' Geometric distribution maximum likelihood estimation
#'
#' For the density function of the Geometric distribution see
#' [Geometric][stats::dgeom].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... not in use.
#' @return `mlgeom` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `prob` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlgeom(corbet)
#' @seealso [Geometric][stats::dgeom] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlgeom <- function(x, na.rm = FALSE, ...) {}


mlgeom_ <- function(x, ...) {
  n <- length(x)
  prob <- 1 / (mean(x) + 1)
  logLik <- sum(x * log(1 - prob)) + n * log(prob)
  list(estimates = prob, logLik = logLik)
}

#' Gompertz distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Gompertz distribution.
#'
#' For the density function of the Gompertz distribution see
#' [Gompertz][extraDistr::Gompertz].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `reltol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#' @return `mlgompertz` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `a` and `b` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlgompertz(precip)
#' @seealso [Gompertz][extraDistr::Gompertz] for the Gompertz density.
#' @references Lenart, A. (2012). The Gompertz distribution and Maximum Likelihood Estimation of its parameters - a revision. MPIDR WORKING PAPER WP 2012-008. https://www.demogr.mpg.de/papers/working/wp-2012-008.pdf
#' @export
mlgompertz <- function(x, na.rm = FALSE, ...) {}


mlgompertz_ <- function(x, ...) {
  n <- length(x)
  x_sum <- sum(x)
  x_var <- stats::var(x) * (n - 1) / n
  if (x_sum / n < sqrt(x_var)) {
    stop("The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed. The exponential distribution will yield a better fit.")
  }

  f_over_df <- function(b) {
    exp_bx <- exp(b * x)
    sum_bx <- sum(exp_bx)
    sum_bx_x <- sum(x * exp_bx)
    sum_bx_x2 <- sum(x^2 * exp_bx)
    s <- sum_bx - n
    r <- b * sum_bx_x - s

    a <- b * n / s
    da <- -n * r / s^2
    d2a <- n / s^2 * (b * (2 * sum_bx_x^2 / s - sum_bx_x2) - 2 * sum_bx_x)

    f <- n * da / a + x_sum
    df <- n * (a * d2a - da^2) / a^2
    f / df
  }

  b <- newton_raphson_1d(f_over_df, 10e-2, ...)

  a <- b * n / (sum(exp(b * x)) - n)
  a <- max(a, 1e-09)

  list(
    estimates = c(a, b),
    logLik = sum(extraDistr::dgompertz(x, a, b, log = TRUE))
  )
}

#' Gumbel distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Gumbel distribution.
#'
#' For the density function of the Gumbel distribution see
#'    [Gumbel][extraDistr::Gumbel].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `reltol` is the relative accuracy requested, defaults to
#'     `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#' @return `mlgumbel` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for `mu`
#'    and `s` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' `shape` and `sigma`.
#' @examples
#' mlgumbel(precip)
#' @seealso [Gumbel][extraDistr::Gumbel] for the Gumbel density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 22. Wiley, New York.
#' @export
mlgumbel <- function(x, na.rm = FALSE, ...) {}


mlgumbel_ <- function(x, ...) {
  x_bar <- sum(x) / length(x)
  estimates <- mlgumbel_estimate(x - x_bar, ...)
  mu <- estimates[1] + x_bar
  sigma <- estimates[2]
  logLik <- sum(extraDistr::dgumbel(x, mu, sigma, log = TRUE))
  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik, iterations = estimates[3])
}

mlgumbel_estimate <- function(x, ...) {
  x2 <- x^2
  dots <- list(...)
  reltol <- if (!is.null(dots$reltol)) dots$reltol else .Machine$double.eps^0.25
  iterlim <- if (!is.null(dots$iterlim)) dots$iterlim else 100

  f_over_df <- function(sigma0) {
    neg_sigma_inv <- -1 / sigma0
    exps <- exp(x * neg_sigma_inv)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x2 * exps) / psi0 - psi1^2
    f <- sigma0 + psi1
    df <- 1 + neg_sigma_inv^2 * psi2
    f / df
  }

  sigma0 <- sqrt(sum(x2) / length(x)) * 2.45 / pi
  sigma <- sigma0

  for (i in seq_len(iterlim)) {
    sigma_new <- sigma - f_over_df(sigma)
    if (is.nan(sigma_new)) stop("NaN in Newton--Raphson iteration.")
    if (abs((sigma_new - sigma) / sigma_new) < reltol) {
      sigma <- sigma_new
      break
    }
    sigma <- sigma_new
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }

  neg_sigma_inv <- -1 / sigma
  psi0 <- sum(exp(x * neg_sigma_inv))
  mu <- -sigma * log(psi0 / length(x))
  c(mu, sigma, iterations = i)
}

#' Inverse Burr distribution maximum likelihood estimation
#'
#' The maximum likelihood estimator fails to exist when the data contains no values
#' strictly greater than 1. Then the likelihood converges
#' to the likelihood of the [Pareto distribution][mlpareto] in this case.
#'
#' `mlinvburr(x)` calls `mlburr(1/x)` internally.
#'
#' For the density function of the Inverse Burr distribution see [Inverse Burr][actuar::dinvburr].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlburr` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape1` and `shape2` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlburr(abalone$length)
#' @seealso [Inverse Burr][actuar::dinvburr] for the Inverse Burr density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 20. Wiley, New York.
#' @export
mlinvburr <- function(x, na.rm = FALSE, ...) {}


mlinvburr_ <- function(x, ...) {
  params <- mlburr_(1/x)$estimates
  list(
    estimates = params,
    logLik = sum(actuar::dinvburr(x, params[1], params[2], log = TRUE))
  )
}

#' Inverse Gamma distribution maximum likelihood estimation
#'
#' Transforms the data and uses Newton-Raphson to estimate the parameters of
#' the Gamma distribution.
#'
#' For the density function of the inverse Gamma distribution see
#' [InvGamma][extraDistr::InvGamma].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlgamma`][mlgamma].
#' @return A named numeric vector with maximum likelihood estimates for
#' `alpha` and `beta`.
#' @examples
#' mlinvgamma(precip)
#' @seealso [InvGamma][extraDistr::InvGamma] for the Inverse Gamma density.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation of
#' the parameters of the gamma distribution and their bias."
#' Technometrics 11.4 (1969): 683-690.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' Witkovsky, V. (2001). "Computing the Distribution of a Linear Combination
#' of Inverted Gamma Variables". Kybernetika. 37 (1): 79<U+2013>90
#'
#' @export
mlinvgamma <- function(x, na.rm = FALSE, ...) {}


mlinvgamma_ <- function(x, ...) {
  gamma_ml <- mlgamma_(1 / x, ...)
  alpha <- gamma_ml$estimates[1]
  beta <- gamma_ml$estimates[2]

  L <- mean(log(x))
  M <- mean(1 / x)
  logLik <- length(x) * (alpha * log(beta) - log(gamma(alpha)) - (alpha + 1) * L - beta * M)

  list(estimates = gamma_ml$estimates, logLik = unname(logLik))
}

#' Inverse Gaussian (Wald) maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mean` is the empirical mean and the
#'     maximum likelihood estimate of `1/shape` is the difference between
#'     the mean of reciprocals and the reciprocal of the mean.
#'
#' For the density function of the Inverse Gamma distribution see
#'     [InverseGaussian][actuar::InverseGaussian].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlinvgauss` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `mean` and `shape` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlinvgauss(precip)
#' @seealso [InverseGaussian][actuar::InverseGaussian] for the
#' Inverse Gaussian density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 15. Wiley, New York.
#' @export
mlinvgauss <- function(x, na.rm = FALSE, ...) {}


mlinvgauss_ <- function(x, ...) {
  n <- length(x)
  mu <- mean(x)
  lambda <- 1 / (mean(1 / x) - 1 / mu)
  estimates <- c(mu, lambda)

  L <- mean(log(x))
  S <- mean((x - mean(x))^2 / x)

  logLik <- -n / 2 * (3 * L - log(lambda) + log(2 * pi) + lambda / mu^2 * S)
  list(estimates = estimates, logLik = logLik)
}

#' Inverse Weibull distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `shape` and `rate` are calculated
#'   by calling [`mlweibull`][mlweibull] on the transformed data.
#'
#' For the density function of the log normal distribution see
#' [InverseWeibull][actuar::InverseWeibull].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlweibull`][mlweibull].
#' @return `mlinvweibull` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `shape` and `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlinvweibull(precip)
#' @seealso [InverseWeibull][actuar::InverseWeibull] for the Inverse Weibull
#'    density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions
#' in Economics and Actuarial Sciences, Wiley.
#'
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models,
#' From Data to Decisions, Fourth Edition, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for
#' actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export
mlinvweibull <- function(x, na.rm = FALSE, ...) {}


mlinvweibull_ <- function(x, ...) {

  log_x <- log(x)
  n <- length(x)
  s <- -sum(log_x) / n

  estimates <- mlgumbel_estimate(log_x + s, ...)
  rate_log <- -estimates[1] + s

  rate <- exp(rate_log)
  shape <- 1 / estimates[2]

  #r <- sum(exp(-log_x * shape)) / n
  #scale <- 1/rate
  #r <- (scale^shape)^2
  #r <- scale^shape * r
  # r <- 1

  logLik <- n * (log(shape) + shape * (-rate_log + s) - 1 + s)
  list(estimates = c(shape, rate), logLik = logLik)
}

#' Kumaraswamy distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Kumaraswamy
#'     distribution.
#'
#' For the density function of the Kumaraswamy distribution see
#'     [Kumaraswamy][extraDistr::Kumaraswamy].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `a0` is an optional starting value for the `a` parameter.
#'     `reltol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#'
#' @return `mlkumar` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for `a`
#'    and `b` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Kumaraswamy][extraDistr::Kumaraswamy] for the Kumaraswamy density.
#' @examples
#' AIC(mlkumar(USArrests$Rape / 100))
#' @references Jones, M. C. "Kumaraswamy's distribution: A beta-type
#' distribution with some tractability advantages." Statistical Methodology
#' 6.1 (2009): 70-81.
#'
#' Kumaraswamy, Ponnambalam. "A generalized probability density function
#' for double-bounded random processes." Journal of Hydrology 46.1-2 (1980):
#' 79-88.
#' @export
mlkumar <- function(x, na.rm = FALSE, ...) {}


kumar_lmom <- function(mu, sigma2) {
  neuler <- digamma(1) # Negative Euler-Mascheroni constant
  basel <- pi^2 / 6 # trigamma(1)

  f_over_df <- function(a) {
    b <- inverse_digamma(neuler - a * mu)
    tri <- trigamma(b)
    tetra <- psigamma(b, deriv = 2)

    f <- a * tri * (basel - tri - a^2 * sigma2)
    df <- a * tetra * mu - 2 * tri * (basel + tri)
    f / df
  }

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100
  a0 <-  sqrt(basel / sigma2)
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0) * reltol) break
    a0 <- a
  }

  a <- a0
  # b is inverse_digamma(neuler - a * mu) - 1
  a
}

mlkumar_ <- function(x, ...) {
  n <- length(x)
  log_x <- log(x)
  n_div <- 1 / n
  mu <- sum(log_x) * n_div
  sigma2 <- sum(log_x^2) * n_div - mu^2

  f_over_df <- function(a) {
    xa <- exp(a * log_x)
    z <- log_x / (1 - xa)

    alpha <- sum(log1p(-xa)) * n_div # Corresponds to s1a
    beta <- sum(xa * z) * n_div
    gamma <- sum(xa * z^2) * n_div

    f <- 1/a + mu + (1/alpha + 1) * beta
    df <- -1/(a^2) + (gamma * alpha + beta^2) / (alpha^2) + gamma

    f / df
  }

  a0 <- kumar_lmom(mu, sigma2)
  a0 <- if (a0 > 0) a0 else sqrt(pi^2 / (6 * sigma2))

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0) * reltol) break
    a0 <- a
  }

  a <- a0
  b <- -n / sum(log1p(-x^a))

  list(
    estimates = c(a = a, b = b),
    logLik = n * (log(a) + log(b) + (a - 1) * mu + 1 / b - 1),
    iterations = i
  )
}

#' Laplace distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mu` is the sample median while the
#'    maximum likelihood estimate of `sigma` is mean absolute deviation
#'    from the median.
#'
#' For the density function of the Laplace distribution see
#' [Laplace][extraDistr::Laplace].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllaplace` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `mu` and `sigma` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllaplace(precip)
#' @seealso [Laplace][extraDistr::Laplace] for the Laplace density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 24. Wiley, New York.
#' @export
mllaplace <- function(x, na.rm = FALSE, ...) {}


mllaplace_ <- function(x, ...) {
  mu <- stats::median(x)
  sigma <- mean(abs(x - mu))
  estimates <- c(mu = mu, sigma = sigma)
  logLik <- -length(x) * (1 + log(2 * sigma))
  list(estimates = estimates, logLik = logLik)
}

#' Log-gamma distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `shapelog` and `ratelog` are calculated
#'   by calling [mlgamma()] on the transformed data.
#'
#' For the density function of the log normal distribution see
#'   [Loggamma][actuar::Loggamma].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlgamma`][mlgamma].
#' @return `mllgamma` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shapelog` and `ratelog` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllgamma(precip)
#' @seealso [Loggamma][actuar::Loggamma] for the log normal density.
#' @references Hogg, R. V. and Klugman, S. A. (1984), Loss Distributions, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for
#' actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export
mllgamma <- function(x, na.rm = FALSE, ...) {}


mllgamma_ <- function(x, ...) {
  y <- log(x)
  n <- length(x)
  estimates <- mlgamma_(y, ...)$estimates
  shapelog <- estimates[1]
  ratelog <- estimates[2]
  L <- mean(y)
  K <- mean(log(y))
  logLik <-
    unname(n * (shapelog * log(ratelog) - log(gamma(shapelog)) +
      (shapelog - 1) * K - (ratelog + 1) * L))

  list(estimates = estimates, logLik = logLik)
}

#' Logarithmic series distribution maximum likelihood estimation
#'
#' For the density function of the Logarithmic series distribution see
#' [Logarithmic series][extraDistr::dlgser]. For an example data set, see [corbet].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Not in use.
#' @return `mllgser` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `theta`.
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' theta_hat <- mllgser(corbet)
#'
#' # The corbert data contains observations from 1 to 24.
#' observed <- table(corbet)
#'
#' # The chi square test evaluated at the maximum likelihood is highly significant.
#' expected <- extraDistr::dlgser(1:24, theta_hat)
#' chisq.test(observed, p = expected / sum(expected))
#'
#' # But chi square test evaluated at 0.997 (used in Corbet) is not.
#' expected <- extraDistr::dlgser(1:24, 0.997)
#' chisq.test(observed, p = expected / sum(expected))
#'
#' # The chi square for `dzipf` is similar.
#' expected <- sads::dzipf(1:24, mlzipf(corbet)[1], mlzipf(corbet)[2]) * length(corbet)
#' chisq.test(observed, p = expected / sum(expected))
#' @references
#' Fisher, R. A., Corbet, A. S., & Williams, C. B. (1943). The relation between the number of species and the number of individuals in a random sample of an animal population. The Journal of Animal Ecology, 12(1), 42. https://doi.org/10.2307/1411
#'
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @seealso [Logarithmic series][extraDistr::dlgser] for the density.
#' @export

mllgser <- function(x, na.rm = FALSE, ...) {}


mllgser_ <- function(x, ...) {
  if (sum(x) == length(x)) {
    stop("All observations equals 1; the maximum likelihood estimator does not exist.")
  }
  x_bar <- mean(x)
  xi <- -x_bar * pracma::lambertWn(-exp(-1 / x_bar) / x_bar) - 1
  theta <- xi / (xi + 1)
  logLik <- sum(extraDistr::dlgser(x, theta, log = TRUE))

  list(estimates = theta, logLik = logLik)
}

#' Log-logistic distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `shape` and `rate` are calculated
#'    by transforming the data back to the logistic model and applying
#'    [`mllogis`][mllogis].
#'
#' For the density function of the log-logistic distribution see
#'    [Loglogistic][actuar::Loglogistic]
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mllogis`][mllogis].
#' @return `mllogis` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape` and `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllnorm(precip)
#' @seealso [Loglogistic][actuar::Loglogistic] for the log-logistic density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions
#' in Economics and Actuarial Sciences, Wiley.
#'
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models,
#' From Data to Decisions, Fourth Edition, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for
#' actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export
mlllogis <- function(x, na.rm = FALSE, ...) {}


mlllogis_ <- function(x, ...) {
  y <- log(x)
  estimates <- mllogis_(y)$estimates
  estimates[1] <- exp(-estimates[1])
  estimates[2] <- 1 / estimates[2]
  estimates <- rev(estimates)
  logLik <- sum(actuar::dllogis(x, estimates[1], estimates[2], log = TRUE))
  list(estimates = estimates, logLik = logLik)
}

#' Log-normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `meanlog` is the empirical mean of the
#'     log-transformed data and the maximum likelihood estimate of `sdlog`
#'     is the square root of the biased sample variance based on the
#'     log-transformed data.
#'
#' For the density function of the log normal distribution see
#'     [Lognormal][stats::Lognormal].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllnorm` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `meanlog` and `sdlog` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllnorm(precip)
#' @seealso [Lognormal][stats::Lognormal] for the log normal density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 14. Wiley, New York.
#' @export
mllnorm <- function(x, na.rm = FALSE, ...) {}


mllnorm_ <- function(x, ...) {
  y <- log(x)
  n <- length(x)
  meanlog <- mean(y)
  sdlog <- sqrt(stats::var(y) * (n - 1) / n)
  estimates <- c(meanlog = meanlog, sdlog = sdlog)
  logLik <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sdlog) + 2 * meanlog)
  list(estimates = estimates, logLik = logLik)
}

#' Logistic distribution maximum likelihood estimation
#'
#' Calculates the estimates using `nlm` with an exponential transform of the
#'     location parameter.
#'
#' For the density function of the logistic distribution see
#'     [Logistic][stats::Logistic].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllogis` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `location` and `scale` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllogis(precip)
#' @seealso [Logistic][stats::Logistic] for the Logistic density,
#'   [nlm][stats::nlm] for the optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 23. Wiley, New York.
#' @export
mllogis <- function(x, na.rm = FALSE, ...) {}


mllogis_ <- function(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  f <- function(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = c(m, log(mad))))
  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum,
    i = values$iterations
  )
}

#' Logit-Normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mu` is the empirical mean of the
#'     logit transformed data and the maximum likelihood estimate of
#'     `sigma` is the square root of the logit transformed
#'     biased sample variance.
#'
#' For the density function of the logit-normal distribution see
#'    [dlogitnorm][logitnorm::dlogitnorm].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllogitnorm` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `mu` and `sigma` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' AIC(mllogitnorm(USArrests$Rape / 100))
#' @seealso [Normal][stats::dnorm] for the normal density.
#' @references Atchison, J., & Shen, S. M. (1980). Logistic-normal
#' distributions: Some properties and uses. Biometrika, 67(2), 261-272.
#' @export
mllogitnorm <- function(x, na.rm = FALSE, ...) {}


mllogitnorm_ <- function(x, ...) {
  n <- length(x)
  y <- stats::qlogis(x)
  mu <- mean(y)
  sigma <- sqrt(stats::var(y) * (n - 1) / n)

  H <- mean(log(x))
  G <- mean(log(1 - x))

  estimates <- c(mu = mu, sigma = sigma)
  logLik <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sigma) + 2 * H + 2 * G)
  list(estimates = estimates, logLik = logLik)
}

#' Lomax distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Lomax distribution.
#'
#' For the density function of the Lomax distribution see
#' [Lomax][extraDistr::Lomax].
#'
#' The likelihood estimator of the Lomax distribution is unbounded when `mean(x^2) < 2*mean(x)^2`. When this
#'    happens, the likelihood converges to an exponential distribution with parameter
#'    equal to the mean of the data. This is the natural limiting case for the Lomax
#'    distribution, and it is reasonable to use `mlexp` in this case.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `lambda0` an optional starting value for the `lambda` parameter.
#'    `reltol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#'
#' @return `mllomax` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `lambda` and `kappa` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Lomax][extraDistr::Lomax] for the Lomax density.
#' @references Kleiber, Christian; Kotz, Samuel (2003), Statistical Size
#' Distributions in Economics and Actuarial Sciences, Wiley Series in
#' Probability and Statistics, 470, John Wiley & Sons, p. 60
#' @examples
#' set.seed(3)
#' mllomax(extraDistr::rlomax(100, 2, 4))
#'
#' # The maximum likelihood estimator may fail if the data is exponential.
#' \dontrun{
#' set.seed(5)
#' mllomax(rexp(10))
#' }
#' @export
mllomax <- function(x, na.rm = FALSE, ...) {}


mllomax_ <- function(x, ...) {
  n <- length(x)
  m <- sum(x) / n
  s <- sum(x^2) / n
  m2 <- m^2

  if (s < 2 * m2) {
    stop("The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution.")
  }
  f_over_df <- function(lambda0) {
    lambda0_x1p <- 1 + lambda0 * x
    x_div <- x / lambda0_x1p
    S <- sum(log(lambda0_x1p)) / n
    S1 <- sum(x_div) / n
    S2 <- sum(x_div^2) / n

    psi <- S1 / S
    f <- 1 / lambda0 - psi - S1
    df <- psi^2 + S2 / S + S2 - 1 / lambda0^2
    f / df
  }

  alpha0 <- 2 * (s - m2) / (s - 2 * m2)
  lambda0 <- 1 / (m * (alpha0 - 1))

  lambda <- newton_raphson_1d(f_over_df, lambda0, ...)
  kappa <- n / sum(log1p(lambda * x))
  loglik <- n * (log(kappa) + log(lambda) - 1 - 1/kappa)

  list(estimates = c(lambda, kappa), logLik = loglik)
}

#' Nakagami distribution maximum likelihood estimation
#'
#' The maximum likelihood estimates of `shape` and `scale` are calculated by
#'     calling `mlgamma` on the transformed data.
#'
#' For the density function of the Nakagami distribution see
#'     [Nakagami][nakagami::Nakagami].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlgamma`][mlgamma].
#' @return `mlgamma` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum
#'    likelihood estimates for `shape` and `rate` and the following
#'    attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured by `match.call`}
#'     \item{`continuous`}{Is the density continuous or discrete?}
#' @examples
#' mlgamma(precip)
#' @seealso [Nakagami][nakagami::Nakagami] for the Nakagami distribution.
#'    [GammaDist][stats::GammaDist] for the closely related Gamma density.
#'    See [`mlgamma`][mlgamma] for the machinery underlying this function.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation of the
#' parameters of the gamma distribution and their bias." Technometrics 11.4
#' (1969): 683-690.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate
#' Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' @export
mlnaka <- function(x, na.rm = TRUE, ...) {}


mlnaka_ <- function(x, ...) {
  estimates <- mlgamma_(x^2, ...)$estimates
  estimates["rate"] <- 1 / estimates["rate"] * estimates["shape"]

  n <- length(x)
  shape <- estimates[1]
  scale <- estimates[2]

  logLik <-
    unname(n * (shape * log(shape) + log(2) -
      lgamma(shape) - shape * log(scale)) +
      (2 * shape - 1) * sum(log(x)) - shape / scale * sum(x^2))

  list(estimates = estimates, logLik = logLik)
}

#' Negative binomial distribution maximum likelihood estimation
#'
#' For the density function of the Negative binomial distribution see
#' [Negative binomial][stats::dnbinom].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... The arguments `size` can be specified to only return the ml of `prob`.
#'    `reltol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#' @return `mlnbinom` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `size` and `prob` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlnbinom(corbet)
#' @seealso [Negative binomial][stats::dnbinom] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlnbinom <- function(x, na.rm = FALSE, ...) {}


mlnbinom_ <- function(x, ...) {
  dots <- list(...)
  n <- length(x)
  x_bar <- mean(x)

  if (is.null(dots$size)) {
    if (x_bar == 0) {
      warning("All observations are 0; the maximum likelihood estimator is not unique.")
      size <- 1
    } else {
      x_var <- stats::var(x)

      if (x_var * (n - 1) / n < x_bar) {
        stop("The maximum likelihood estimator does not exists for underdispersed data, but converges to a Poisson. Use `mlpois` instead.")
      }

      f_over_df <- function(size) {
        prob <- size / (x_bar + size)
        dprob <- x_bar / (size^2 + x_bar * size)
        f <- sum(digamma(x + size)) - n * digamma(size) + n * log(prob)
        df <- sum(trigamma(x + size)) - n * trigamma(size) + n * dprob
        f / df
      }

      size0 <- x_bar^2 / (x_var - x_bar)
      size <- newton_raphson_1d(f_over_df, size0, ...)
    }
  } else {
    size <- dots$size
  }

  prob <- size / (x_bar + size)
  logLik <- sum(lgamma(x + size)) - sum(lfactorial(x)) - n * lgamma(size) +
    n * size * log(prob) + n * log(1 - prob) * x_bar

  list(estimates = c(size, prob), logLik = logLik)
}

#' Normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mean` is the empirical mean and the
#'     maximum likelihood estimate of `sd` is the square root of the
#'     biased sample variance.
#'
#' For the density function of the normal distribution see
#' [Normal][stats::Normal].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlnorm` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `mean` and `sd` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlnorm(precip)
#' @seealso [Normal][stats::Normal] for the normal density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York.
#' @export
mlnorm <- function(x, na.rm = FALSE, ...) {}


mlnorm_ <- function(x, ...) {
  n <- length(x)

  mu <- mean(x)
  sigma <- sqrt(stats::var(x) * (n - 1) / n)

  estimates <- c(mu, sigma)
  logLik <- -n / 2 * (1 + log(2 * pi) + 2 * log(sigma))
  list(estimates = estimates, logLik = logLik)
}

#' Paralogistic distribution maximum likelihood estimation
#'
#' This function estimates the only the shape parameters of the Paralogistic distribution.
#' The rate is set to 1.
#'
#' For the density function of the Paralogistic distribution see [Paralogistic][actuar::dparalogis].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlparalogis` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlparalogis(abalone$length)
#' @seealso [Paralogistic][actuar::dparalogis] for the paralogistic density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley.
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley.
#' @export
mlparalogis <- function(x, na.rm = FALSE, ...) {}


mlparalogis_ <- function(x, ...) {
  n <- length(x)
  log_x <- log(x)
  log_x2 <- log_x^2
  s <- sum(log_x) / n
  n <- length(x)

  f_over_df <- function(shape) {
    x_shape <- x^shape
    x_div <- x_shape / (1 + x_shape)
    r <- sum(log(1 + x_shape)) / n
    r_grad <- sum(x_div * log_x) / n
    r_hessian <- sum(x_div * log_x2) / n - sum(x_div * x_div * log_x2) / n

    f <- 2 / shape + s - r - (shape + 1) * r_grad
    df <- -2 / shape^2 - 2 * r_grad - (shape + 1) * r_hessian
    f / df
  }

  # Approximate method of moments solution
  shape0 <- max(3 / (1 - mean(x)), 0.1)
  dots <- list(...)
  if (!is.null(dots$shape0)) shape0 <- dots$shape0
  shape <- newton_raphson_1d(f_over_df, shape0)
  rn <- function(shape) sum(log(1 + x^shape))
  loglik <- 2 * n * log(shape) + (shape - 1) * n * s - (shape + 1) * rn(shape)

  list(estimates = c(shape), logLik = loglik)
}

#' Pareto distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `b` is the minimum of `x` and the
#'     maximum likelihood estimate of `a` is
#'     `1/(mean(log(x)) - log(b))`.
#'
#' For the density function of the Pareto distribution see
#'    [Pareto][extraDistr::Pareto].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlpareto` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `a` and `b` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpareto(precip)
#' @seealso [Pareto][extraDistr::Pareto] for the Pareto density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 20. Wiley, New York.
#' @export
mlpareto <- function(x, na.rm = FALSE, ...) {}


mlpareto_ <- function(x, ...) {
  m <- mean(log(x))
  b <- min(x)
  a <- 1 / (m - log(b))

  estimates <- c(a, b)
  logLik <- length(x) * (log(a) + a * log(b) - (a + 1) * m)
  list(estimates = estimates, logLik = logLik)
}

#' Poisson distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `lambda` is the empirical mean.
#'
#' For the density function of the Poisson distribution see
#' [Poisson][stats::Poisson].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlpois` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `lambda` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpois(ChickWeight$weight)
#' @seealso [Poisson][stats::Poisson] for the Poisson density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlpois <- function(x, na.rm = FALSE, ...) {}


mlpois_ <- function(x, ...) {
  n <- length(x)
  lambda <- mean(x)
  logLik <- -n * lambda + sum(x) * log(lambda) - sum(lgamma(x + 1))
  list(estimates = lambda, logLik = logLik)
}

#' Power distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `alpha` is the maximum of `x` +
#'     `epsilon` (see the details) and the maximum likelihood estimate of
#'     `beta` is `1/(log(alpha)-mean(log(x)))`.
#'
#' For the density function of the power distribution see
#'     [PowerDist][extraDistr::PowerDist]. The maximum likelihood estimator of
#'     `alpha` does not exist, strictly
#'     speaking. This is because `x` is supported `c(0, alpha)` with
#'     an open endpoint on alpha in the `extraDistr` implementation of
#'     `dpower`. If the endpoint was closed, `max(x)` would have been
#'     the maximum likelihood estimator. To overcome this problem, we add
#'     a possibly user specified `epsilon` to `max(x)`.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `epsilon` is a positive number added to `max(x)` as an to the
#'     maximum likelihood. Defaults to `.Machine$double.eps^0.5`.
#' @return `mlpower` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `alpha` and `beta` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpower(precip)
#' @seealso [PowerDist][extraDistr::PowerDist] for the power density. [extraDistr::Pareto]
#'    for the closely related Pareto distribution.
#' @references
#' Arslan, G. "A new characterization of the power distribution."
#' Journal of Computational and Applied Mathematics 260 (2014): 99-102.
#' @export
mlpower <- function(x, na.rm = FALSE, ...) {}


mlpower_ <- function(x, ...) {
  dots <- list(...)
  epsilon <- if (!is.null(dots$epsilon)) {
    dots$epsilon
  } else {
    .Machine$double.eps^0.5
  }

  m <- mean(log(x))
  alpha <- max(x) + epsilon
  beta <- 1 / (log(alpha) - m)

  estimates <- c(alpha, beta)
  logLik <-
    length(x) * (log(beta) - beta * log(alpha) + (beta - 1) * m)
  list(estimates = estimates, logLik = logLik)
}

#' Rayleigh distribution maximum likelihood estimation
#'
#' Calculates the `sigma` parameter as the square root of half the
#'    empirical second moment.
#'
#' For the density function of the Rayleigh distribution see
#' [Rayleigh][extraDistr::Rayleigh].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlrayleigh` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `sigma` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlrayleigh(precip)
#' @seealso [Rayleigh][extraDistr::Rayleigh] for the Rayleigh density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 18. Wiley, New York.
#' @export
mlrayleigh <- function(x, na.rm = FALSE, ...) {}


mlrayleigh_ <- function(x, ...) {
  sigma <- sqrt(1 / 2 * mean(x^2))
  logLik <- length(x) * (mean(log(x) - 2 * log(sigma) - 1))
  list(estimates = sigma, logLik = logLik)
}

#' Skew Generalized Error distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::sgedFit].
#'
#' For the density function of the Student t-distribution see
#' [sged][fGarch::sged].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlsged` returns an object of [class][base::class] `univariateML`.
#'   This is a named numeric vector with maximum likelihood estimates for the
#'   parameters `mean`, `sd`, `nu`, `xi`, and the following attributes:
#'   \item{`model`}{The name of the model.}
#'   \item{`density`}{The density associated with the estimates.}
#'   \item{`logLik`}{The loglikelihood at the maximum.}
#'   \item{`support`}{The support of the density.}
#'   \item{`n`}{The number of observations.}
#'   \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlsged(precip)
#' @seealso [sged][fGarch::sged] for the Student t-density.
#' @references Nelson D.B. (1991); Conditional Heteroscedasticity in Asset
#'   Returns: A New Approach, Econometrica, 59, 347<U+2013>370.
#'
#'   Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat Tails and
#'   Skewness, Preprint.
#' @export
mlsged <- function(x, na.rm = FALSE, ...) {}


mlsged_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::sgedFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

#' Skew Normal distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::snormFit].
#'
#' For the density function of the Student t distribution see
#' [dsnorm][fGarch::snorm].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlsnorm` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    the parameters `mean`, `sd`, `xi` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlsnorm(precip)
#' @seealso [dsnorm][fGarch::snorm] for the Student-t density.
#' @references Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat
#'     Tails and Skewness, Preprint.
#' @export
mlsnorm <- function(x, na.rm = FALSE, ...) {}


mlsnorm_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::snormFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

#' Skew Student t-distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::sstdFit].
#'
#' For the density function of the skew Student t-distribution see
#' [sstd][fGarch::sstd].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlsstd` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    the parameters `mean`, `sd`, `nu`, `xi` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlsstd(precip)
#' @seealso [sstd][fGarch::sstd] for the Skew Student t-density.
#' @references Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat
#'     Tails and Skewness, Preprint.
#' @export
mlsstd <- function(x, na.rm = FALSE, ...) {}


mlsstd_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::sstdFit(x))
  list(estimates = fit[["estimate"]], logLik = -fit$minimum)
}

#' Student-t distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::stdFit].
#'
#' For the density function of the Student t-distribution see
#' [std][fGarch::std].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlstd` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    the parameters `mean`, `sd`, `nu` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlstd(precip)
#' @seealso [std][fGarch::std] for the Student-t density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York.
#' @export
mlstd <- function(x, na.rm = FALSE, ...) {}


mlstd_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::stdFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

#' Uniform distribution maximum likelihood estimation
#'
#' The estimates are `min(x)` and `max(x)`.
#'
#' For the density function of the logistic distribution see
#'    [Uniform][stats::Uniform].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlunif` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for `min`
#'    and `max` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlunif(precip)
#' @seealso [Uniform][stats::Uniform] for the uniform density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 26. Wiley, New York.
#' @export
mlunif <- function(x, na.rm = FALSE, ...) {}


mlunif_ <- function(x, ...) {
  max_ <- max(x)
  min_ <- min(x)
  list(estimates = c(min_, max_), logLik = -length(x) * log(max_ - min_))
}

#' Weibull distribution maximum likelihood estimation
#'
#' For the density function of the Weibull distribution see
#'    [Weibull][stats::Weibull].
#'
#' Uses [mlgumbel] to estimate the parameters of the Weibull distribution.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `shape0` is an optional starting value for the `shape` parameter.
#'     `reltol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#'
#' @return `mlweibull` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape` and `scale` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Weibull][stats::Weibull] for the Weibull density.
#' @examples
#' BIC(mlweibull(precip))
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 21. Wiley, New York.
#' @export
mlweibull <- function(x, na.rm = FALSE, ...) {}


mlweibull_ <- function(x, ...) {
  log_x <- -log(x)
  s <- -sum(log_x) / length(x)
  estimates <- mlgumbel_estimate(log_x + s, ...)
  scale <- exp(-estimates[1] + s)[1]
  shape <- 1 / estimates[2]
  logLik <- length(x) * (log(shape / scale) + (shape - 1) * (s - log(scale)) - 1)
  list(estimates = c(shape, scale), logLik = logLik)
}

#' Zero-inflated Poisson distribution maximum likelihood estimation
#'
#' For the density function of the zeero-inflated Poisson distribution see
#' [Zero-inflated Poisson distribution][extraDistr::dzip].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Not currently in use.
#' @return `mlzip` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `lambda` and `pi` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlzip(corbet)
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @seealso [Zero-inflated Poisson distribution][extraDistr::dzip] for the density.
#' @export
mlzip <- function(x, na.rm = FALSE, ...) {}


mlzip_ <- function(x, ...) {
  r_0 <- mean(x == 0)
  x_bar <- mean(x)

  if (r_0 == 1) {
    pi <- 1
    lambda <- 1
    warning("All observations equal to 0; the maximum likelihood estimator is not unique.")
  } else if (r_0 > exp(-x_bar)) {
    x_bar <- mean(x)
    gamma <- x_bar / (1 - r_0)
    lambda <- pracma::lambertWp(-gamma * exp(-gamma)) + gamma
    pi <- (r_0 - exp(-lambda)) / (1 - exp(-lambda))
  } else {
    pi <- 0
    lambda <- x_bar
  }

  logLik <- sum(extraDistr::dzip(x, lambda, pi, log = TRUE))

  list(estimates = c(lambda, pi), logLik = logLik)
}

#' Zipf distribution maximum likelihood estimation
#'
#' For the density function of the Zipf distribution see
#' [Zipf][sads::dzipf].
#'
#' This function follows the same format as every other function in the package,
#' but most applications of Zipf's law use rank-abundance data. See, e.g., [sads::fitzipf]
#' for estimation of this sort of data.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Not currently in use.
#' @return `mlzipf` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `N` and `s` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' AIC(mlzipf(corbet)) # 2729.536
#' AIC(mllgser(corbet)) # 2835.883
#' @seealso [Zipf][sads::dzipf] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlzipf <- function(x, na.rm = FALSE, ...) {}


mlzipf_ <- function(x, ...) {
  N <- max(x)
  n <- length(x)
  sum_lx <- sum(log(x))
  log_seq <- log(seq(N))
  log_seq2 <- log(seq(N))^2
  div_seq <- 1 / seq(1, N)

  if (sum_lx == 0) {
    warning("All observations are equal to 1. The maximum likelihood estimator is not unique.")
    return(list(estimates = c(N, 1), logLik = 0))
  }

  f_over_df <- function(shape0) {
    div_seq_shape0 <- div_seq^shape0
    sum_shape <- sum(div_seq_shape0)
    sum_log_hs <- sum(div_seq_shape0 * log_seq)
    sum_log2_hs <- sum(div_seq_shape0 * log_seq2)
    top <- sum_log_hs / sum_shape
    bottom <- -n * sum_log2_hs / sum_shape + n * top^2
    (n * top - sum_lx) / bottom
  }

  shape0 <- 1
  shape <- newton_raphson_1d(f_over_df, shape0, ...)

  if (shape < 0) {
    stop("Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.")
  }

  logLik <- -shape * sum_lx - n * log(sum(1 / seq(1, N)^shape))
  list(estimates = c(N, shape), logLik = logLik)
}

