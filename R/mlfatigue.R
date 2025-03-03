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

univariateML_metadata$mlfatigue <- list(
  "model" = "Birnbaum-Saunders",
  "density" = "extraDistr::dfatigue",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE)),
  "names" = c("alpha", "beta", "mu"),
  "default" = c(3, 2, 0)
)

mlfatigue_ <- function(x, ...) {
  n <- length(x)
  x_bar <- mean(x)
  x_inv_bar <- n / sum(1 / x)

  f_over_df <- function(beta) {
    xi <- n / sum(1 / (x + beta))
    dxi <- sum(1 / (x + beta)^2) * xi^2 / n
    f <- beta^2 - beta * (2 * x_inv_bar + xi) + x_inv_bar * (x_bar + xi)
    df <- 2 * beta - (2 * x_inv_bar + xi) - beta * dxi + x_inv_bar * dxi
    f / df
  }

  # Starting parameter: Method of moments solution.
  beta0 <- sum(sqrt(x)) / sum(1 / sqrt(x))
  beta <- newton_raphson_1d(f_over_df, beta0, ...)

  alpha <- sqrt(x_bar / beta + beta / x_inv_bar)

  estimates <- c(alpha = alpha, beta = beta, mu = 0)
  logLik <- sum(extraDistr::dfatigue(x, alpha, beta, 0, log = TRUE))
  list(estimates = estimates, logLik = logLik)
}
