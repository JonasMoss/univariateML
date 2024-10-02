#' Student-t distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::stdFit].
#'
#' For the density function of the Student t-distribution see
#' [std][fGarch::std].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlstd` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    the parameters `mean`, `sd`, `nu` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlstd(precip)
#' @seealso [std][fGarch::std] for the Student-t density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York.
#' @export
mlstd <- \(x, na.rm = FALSE, ...) {}

metadata$mlstd <- list(
  "model" = "Student-t",
  "density" = "fGarch::dstd",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mean", "sd", "nu")
)

mlstd_ <- \(x, ...) {
  fit <- suppressWarnings(fGarch::stdFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
