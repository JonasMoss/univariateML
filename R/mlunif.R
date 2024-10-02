#' Uniform distribution maximum likelihood estimation
#'
#' The estimates are `min(x)` and `max(x)`.
#'
#' For the density function of the logistic distribution see
#'    [Uniform][stats::Uniform].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlunif` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for `min`
#'    and `max` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlunif(precip)
#' @seealso [Uniform][stats::Uniform] for the uniform density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 26. Wiley, New York.
#' @export
mlunif <- \(x, na.rm = FALSE, ...) {}

metadata$mlunif <- list(
  "model" = "Uniform",
  "density" = "stats::dunif",
  "support" = stats::setNames(intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)), c("min", "max")),
  "names" = c("min", "max")
)

mlunif_ <- \(x, ...) {
  max_ <- max(x)
  min_ <- min(x)
  list(estimates = c(min_, max_), logLik = -length(x) * log(max_ - min_))
}
