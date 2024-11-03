#' Weibull distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Weibull distribution.
#'
#' For the density function of the Weibull distribution see
#'    [Weibull][stats::Weibull].
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
mlweibull <- \(x, na.rm = FALSE, ...) {}

metadata$mlweibull <- list(
  "model" = "Weibull",
  "density" = "stats::dweibull",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE)),
  "names" = c("shape", "scale"),
  "default" = c(3, 4)
)

mlweibull_ <- \(x, ...) {
  n <- length(x)
  log_x <- log(x)
  l_hat <- mean(log_x)
  log_xsq <- log_x^2

  f_over_df <- \(shape0) {
    shape0_lsum <- mean(x^shape0 * log_x)
    shape0_lsum_sqr <- mean(x^shape0 * log_xsq)
    shape0_sum <- mean(x^shape0)
    a <- shape0_lsum / shape0_sum
    b <- shape0_lsum_sqr / shape0_sum
    f <- 1 / shape0 + l_hat - a
    df <- -1 / shape0^2 + a^2 - b
    f / df
  }

  dots <- list(...)
  shape0 <- if (!is.null(dots$sigma0)) dots$shape0 else 1

  shape <- newton_raphson_1d(f_over_df, shape0, ...)

  shape_mean <- mean(x^shape)
  scale <- shape_mean^(1 / shape)

  estimates <- c(shape, scale)
  logLik <- n * (log(shape) - log(scale) +
    (shape - 1) * (l_hat - log(scale)) - scale^-shape * shape_mean)
  list(estimates = estimates, logLik = logLik)
}
