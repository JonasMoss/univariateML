#' Estimates the parameter of the Laplace distribution using maximum likelihood
#'
#' The maximum likelihood estimate of \code{mu} is the sample median while the
#'    maximum likelihood estimate of \code{sigma} is mean absolute deviation
#'    from the median.
#'
#' For the density function of the Laplace distribution see \link[extraDistr]{dlaplace}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{mu} and \code{sigma}.
#' @examples mllaplace(precip)
#' @seealso \link[extraDistr]{dlaplace} for the Laplace density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 24. Wiley, New York.
#' @export

mllaplace = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  mu = stats::median(x)
  sigma = mean(abs(x - mu))
  object = c(mu = mu,
             sigma = sigma)
  class(object) = "univariateML"
  attr(object, "model") = "Laplace"
  attr(object, "density") = "extraDistr::dlaplace"
  attr(object, "logLik") = -length(x)*(1 + log(2*sigma))
  attr(object, "support") = c(-Inf, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
