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

univariateML_metadata$mlweibull <- list(
  "model" = "Weibull",
  "density" = "stats::dweibull",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape", "scale"),
  "default" = c(3, 4)
)

mlweibull_ <- function(x, ...) {
  log_x <- -log(x)
  s <- -sum(log_x) / length(x)
  estimates <- mlgumbel_estimate(-log(x) + s, ...)
  scale <- exp(-estimates[1] + s)[1]
  shape <- 1 / estimates[2]
  logLik <- length(x) * (log(shape / scale) + (shape - 1) * (s - log(scale)) - 1)
  list(estimates = c(shape, scale), logLik = logLik)
}
