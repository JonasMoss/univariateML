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

univariateML_metadata$mlinvweibull <- list(
  "model" = "InverseWeibull",
  "density" = "actuar::dinvweibull",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape", "rate"),
  "default" = c(2, 3)
)

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
