#' Inverse Burr distribution maximum likelihood estimation
#'
#' The maximum likelihood estimator fails to exist when the data contains no values
#' strictly greater than 1. Then the likelihood converges
#' to the likelihood of the [Pareto distribution][mlpareto] in this case.
#'
#' `mlinvburr(x)` calls `mlburr(1/x)` internally.
#'
#' For the density function of the Inverse Burr distribution see [Inverse Burr][actuar::dinvburr].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlburr` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape1` and `shape2` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlburr(abalone$length)
#' @seealso [Inverse Burr][actuar::dinvburr] for the Inverse Burr density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 20. Wiley, New York.
#' @export
mlinvburr <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlinvburr <- list(
  "model" = "Inverse Burr",
  "density" = "actuar::dinvburr",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape1", "shape2"),
  "default" = c(1, 2)
)

mlinvburr_ <- function(x, ...) {
  params <- mlburr_(x)$estimates
  list(
    estimates = params,
    logLik = sum(actuar::dinvburr(x, params[1], params[2], log = TRUE))
  )
}
