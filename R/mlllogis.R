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
mlllogis <- \(x, na.rm = FALSE, ...) {}

metadata$mlllogis <- list(
  "model" = "Loglogistic",
  "density" = "actuar::dllogis",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE)),
  "names" = c("shape", "rate")
)

mlllogis_ <- \(x, ...) {
  y <- log(x)
  estimates <- mllogis_(y)$estimates
  estimates[1] <- exp(-estimates[1])
  estimates[2] <- 1 / estimates[2]
  estimates <- rev(estimates)
  logLik <- sum(actuar::dllogis(x, estimates[1], estimates[2], log = TRUE))
  list(estimates = estimates, logLik = logLik)
}
