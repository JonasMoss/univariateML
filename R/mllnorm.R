#' Estimates the parameter of the log-normal distribution using maximum likelihood
#'
#' The maximum likelihood estimate of \code{meanlog} is the empirical mean of the
#'     log-transformed data and the maximum likelihood estimate of \code{sdlog}
#'     is the square root of the biased sample variance based on the
#'     log-transformed data.
#'
#' For the density function of the log normal distribution see \link[stats]{dlnorm}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{meanlog} and \code{sdlog}.
#' @examples mllnorm(precip)
#' @seealso \link[stats]{dlnorm} for the log normal density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 14. Wiley, New York.
#' @export

mllnorm = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) >= 0)
  y = log(x)
  n = length(x)
  meanlog = mean(y)
  sdlog = sqrt(stats::var(y)*(n-1)/n)
  object = c(meanlog = meanlog, sdlog = sdlog)
  class(object) = "univariateML"
  attr(object, "model") = "Lognormal"
  attr(object, "density") = "stats::dlnorm"
  attr(object, "logLik") = -n/2*(1 + log(2*pi) + 2*log(sdlog) + 2*meanlog)
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
