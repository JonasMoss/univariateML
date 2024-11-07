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
mlgamma <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlgamma <- list(
  "model" = "Gamma",
  "density" = "stats::dgamma",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape", "rate"),
  "default" = c(2, 2)
)

mlgamma_ <- \(x, ...) {

  n <- length(x)
  mean_hat <- mean(x)
  lx_bar <- mean(log(x))
  s <- log(mean_hat) - lx_bar

  f_over_df <- \(shape0) {
    (log(shape0) - digamma(shape0) - s) / (1 / shape0 - trigamma(shape0))
  }

  ## Starting estimator is close to the ML estimator of shape.
  shape0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s))
  shape <- newton_raphson_1d(f_over_df, shape0, ...)

  rate <- shape / mean_hat

  estimates <- c(shape = shape, rate = rate)
  logLik <- n * (shape * log(rate) - log(gamma(shape)) + (shape - 1) * lx_bar - rate * mean_hat)

  list(estimates = estimates, logLik = logLik)
}
