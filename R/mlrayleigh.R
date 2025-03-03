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

univariateML_metadata$mlrayleigh <- list(
  "model" = "Rayleigh",
  "density" = "extraDistr::drayleigh",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE)),
  "names" = c("sigma"),
  "default" = 2
)

mlrayleigh_ <- function(x, ...) {
  sigma <- sqrt(1 / 2 * mean(x^2))
  logLik <- length(x) * (mean(log(x) - 2 * log(sigma) - 1))
  list(estimates = sigma, logLik = logLik)
}
