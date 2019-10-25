#' Estimates the parameter of the exponential distribution using maximum likelihood
#'
#' The maximum likelihood estimate of \code{rate} is the inverse sample mean.
#'
#' For the density function of the exponential distribution see \link[stats]{dexp}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{rate}.
#' @examples mlexp(precip)
#' @seealso \link[stats]{dexp} for the exponential density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 19. Wiley, New York.
#' @export

mlexp = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) >= 0)
  rate = 1/mean(x)
  object = c(rate = rate)
  class(object) = "univariateML"
  attr(object, "model") = "Exponential"
  attr(object, "density") = "stats::dexp"
  attr(object, "logLik") = length(x)*(log(rate) - 1)
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
