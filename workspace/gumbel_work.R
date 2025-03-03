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

univariateML_metadata$mlgumbel <- list(
  "model" = "Gumbel",
  "density" = "extraDistr::dgumbel",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mu", "sigma"),
  "default" = c(3, 3)
)

mlgumbel_ <- function(x, ...) {
  x_bar <- sum(x) / length(x)
  estimates <- mlgumbel_estimate(x - x_bar, ...)
  mu <- estimates[1] + x_bar
  sigma <- estimates[2]
  logLik <- -length(x) * (log(sigma) + (x_bar - mu) / sigma + 1)
  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik)
}

mlgumbel_estimate2 <- function(x, ...) {
  x2 <- x^2
  f_over_df <- function(sigma0) {
    exps <- exp(-x * sigma0)
    t0 <- sum(exps)
    t1 <- -sum(x * exps)
    t2 <- sum(x2 * exps)
    t3 <- -sum(x^3 * exps)
    t4 <- sum(x^4 * exps)
    t5 <- -sum(x^5 * exps)
    f <- t0 - t1 * sigma0
    df <- t1 - t2 * sigma0 - t1
    d2f <- -t3 * sigma0 - t2
    d3f <- -t4 * sigma0 - 2 * t3
    d4f <- -t5 * sigma0 - 3 * t4

    upper <- -d3f / f^2 - 6 * df^3 / f^4 + 6 * df * d2f / f^3
    lower <- -d4f / f^2 + 6 * d2f^2 / f^3 + 24 * df^4 / f^5 + 8 * d3f * df / f^3 - 36 * df^2 * d2f / f^4

    -4 * upper / lower

    # h <- -f/df
    # upper <- 1 + 0.5 * (d2f / df * h)
    # lower <- 1 + d2f / df * h + 1/6 * d3f / df * h^2
    #- h * upper / lower

    #-h * (1/(1+0.5 * (d2f / df) * h))
  }

  # sigma0 <- 1/ (sqrt(var(x) * (length(x) - 1) / (length(x) - 2.1)) * 2.44949) * pi
  sigma0 <- 1 / sqrt(var(x) * 2.44949) * pi
  sigma <- 1 / newton_raphson_1d(f_over_df, sigma0, ...)
  neg_sigma_inv <- -1 / sigma
  mu <- -sigma * log(sum(exp(x * neg_sigma_inv)) / length(x))
  c(mu, sigma)
}

mlgumbel_estimate <- function(x, ...) {
  x2 <- x^2
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

  # sigma0 <- sqrt(var(x) * (length(x) - 1) / (length(x) - 2.1)) * 2.44949 / pi
  sigma0 <- sqrt(var(x)) * 2.44949 / pi * (1 - 1 / length(x))
  # sigma0 <- sd(x) * 2.44949 / pi
  sigma <- newton_raphson_1d(f_over_df, sigma0, ...)
  neg_sigma_inv <- -1 / sigma
  mu <- -sigma * log(sum(exp(x * neg_sigma_inv)) / length(x))
  c(mu, sigma)
}


mlgumbel_estimate2 <- function(x, ...) {
  x2 <- x^2
  f_over_df <- function(sigma0) {
    neg_sigma_inv <- -1 / sigma0
    exps <- exp(x * neg_sigma_inv)
    xi0 <- sum(exps)
    xi1 <- sum(x * exps)
    xi2 <- sum(x2 * exps)
    xi3 <- sum(x^3 * exps)
    phi1 <- xi1 * neg_sigma_inv^2
    phi2 <- xi2 * neg_sigma_inv^4 + 2 * xi1 * neg_sigma_inv^3
    phi3 <- xi3 * neg_sigma_inv^6 + 6 * (xi2 * neg_sigma_inv^5 + xi2 * neg_sigma_inv^4)
    psi1 <- phi1 / xi0
    psi2 <- phi2 / xi0 - psi1^2
    psi3 <- phi3 / xi0 - 3 * psi2 * psi1 - psi1^3
    f <- -neg_sigma_inv + psi1
    df <- -neg_sigma_inv^2 + psi2
    d2f <- -2 * neg_sigma_inv^3 + psi3
    h <- f / df
    # h * 1/(1 - 0.5*d2f/df*h)
    # l <- f * d2f / df^2
    # (1 + l / 2 * (1 - l)) * h
  }

  sigma0 <- sqrt(var(x) * (length(x) - 1) / (length(x) - 2.1)) * 2.44949 / pi
  sigma0 <- sqrt(var(x)) * 2.44949 / pi * (1 - 1 / length(x))
  # sigma0 <- sd(x) * 2.44949 / pi
  sigma <- newton_raphson_1d(f_over_df, sigma0, ...)
  neg_sigma_inv <- -1 / sigma
  mu <- -sigma * log(sum(exp(x * neg_sigma_inv)) / length(x))
  c(mu, sigma)
}
