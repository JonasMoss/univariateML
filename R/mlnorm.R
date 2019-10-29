#' Normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{mean} is the empirical mean and the
#'     maximum likelihood estimate of \code{sd} is the square root of the
#'     biased sample variance.
#'
#' For the density function of the normal distribution see \link[stats]{Normal}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{mean} and \code{sd}.
#' @examples mlnorm(precip)
#' @seealso \link[stats]{Normal} for the normal density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York.
#' @export

mlnorm = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  n = length(x)

  mu = mean(x)
  sigma = sqrt(stats::var(x)*(n-1)/n)

  object = c(mean = mu, sd = sigma)
  class(object) = "univariateML"
  attr(object, "model") = "Normal"
  attr(object, "density") = "stats::dnorm"
  attr(object, "logLik") = -n/2*(1 + log(2*pi) + 2*log(sigma))
  attr(object, "support") = c(-Inf, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
