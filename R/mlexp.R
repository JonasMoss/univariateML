#' Exponential distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `rate` is the inverse sample mean.
#'
#' For the density function of the exponential distribution see [Exponential][stats::Exponential].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed? If `FALSE`,
#'   the function fails when `x` contains missing values.
#' @return `mlexp` returns an object of [class][base::class] `univariateML`. This
#'    is a named numeric vector with maximum likelihood estimates for `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlexp(precip)
#' @seealso [Exponential][stats::Exponential] for the exponential density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 19. Wiley, New York.
#' @export

mlexp <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) >= 0)
  rate <- 1 / mean(x)
  object <- c(rate = rate)
  class(object) <- "univariateML"
  attr(object, "model") <- "Exponential"
  attr(object, "density") <- "stats::dexp"
  attr(object, "logLik") <- length(x) * (log(rate) - 1)
  attr(object, "support") <- c(0, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
