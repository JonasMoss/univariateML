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
mlexp <- \(x, na.rm = TRUE, ...) {}

metadata$mlexp <- list(
  "model" = "Exponential",
  "density" = "stats::dexp",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE)),
  "names" = c("rate")
)

mlexp_ <- \(x, ...) {
  estimates <- 1 / mean(x)
  logLik <- length(x) * (log(estimates) - 1)
  list(estimates = estimates, logLik = logLik)
}
