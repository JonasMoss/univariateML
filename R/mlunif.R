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


mlunif <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  n <- length(x)
  max_ <- max(x)
  min_ <- min(x)
  object <- c(min = min_, max = max_)
  class(object) <- "univariateML"
  attr(object, "model") <- "Uniform"
  attr(object, "density") <- "stats::dunif"
  attr(object, "logLik") <- -n * log(max_ - min_)
  attr(object, "support") <- c(min, max)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
