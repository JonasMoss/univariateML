#' Poisson distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `lambda` is the empirical mean.
#'
#' For the density function of the Poisson distribution see
#' [Poisson][stats::Poisson].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlpois` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `lambda` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpois(ChickWeight$weight)
#' @seealso [Poisson][stats::Poisson] for the Poisson density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlpois <- \(x, na.rm = FALSE, ...) {}

metadata$mlpois <- list(
  "model" = "Poisson",
  "density" = "stats::dpois",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("lambda")
)

mlpois_ <- \(x, ...) {
  n <- length(x)
  lambda <- mean(x)
  logLik <- -n * lambda + sum(x) * log(lambda) - sum(lgamma(x + 1))
  list(estimates = lambda, logLik = logLik)
}
