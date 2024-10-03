#' Binomial distribution maximum likelihood estimation
#'
#' For the density function of the Binomial distribution see
#' [Binomial][stats::dbinom].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... The arguments `size` can be specified to only return the ml of `prob`.
#'    `rel.tol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#' @return `mlbinom` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `mean` and `sd` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpois(precip)
#' @seealso [Binomial][stats::dbinom] for the density.
#' @export
mlbinom <- \(x, na.rm = FALSE, ...) {}

metadata$mlbinom <- list(
  "model" = "Binomial",
  "density" = "stats::dbinom",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("size", "prob")
)

mlbinom_ <- \(x, ...) {
  n <- length(x)
  x_bar <- mean(x)



  list(estimates = c(size, prob), logLik = logLik)
}
