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
mlinvweibull <- \(x, na.rm = FALSE, ...) {}

metadata$mlinvweibull <- list(
  "model" = "InverseWeibull",
  "density" = "actuar::dinvweibull",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape", "rate"),
  "default" = c(2, 3)
)

mlinvweibull_ <- \(x, ...) {
  y <- 1 / x
  n <- length(x)

  estimates <- mlweibull_(y, ...)$estimates

  shape <- estimates[1]
  scale <- 1 / estimates[2]
  G <- mean(log(x))
  Ma <- mean(x^-shape)

  logLik <- unname(n * (log(shape) + shape * (log(scale) - G) -
    scale^shape * Ma - G))
  list(estimates = estimates, logLik = logLik)
}
