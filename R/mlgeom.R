#' Geometric distribution maximum likelihood estimation
#'
#' For the density function of the Geometric distribution see
#' [Geometric][stats::dgeom].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... not in use.
#' @return `mlgeom` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `prob` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpois(precip)
#' @seealso [Geometric][stats::dgeom] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlgeom <- \(x, na.rm = FALSE, ...) {}

metadata$mlgeom <- list(
  "model" = "Geometric",
  "density" = "stats::dgeom",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("prob"),
  "default" = 0.5
)

mlgeom_ <- \(x, ...) {
  n <- length(x)
  prob <- 1 / (mean(x) + 1)
  logLik <- sum(x * log(1 - prob)) + n * log(prob)
  list(estimates = prob, logLik = logLik)
}
