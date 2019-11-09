#' Normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{mean} is the empirical mean and the
#'     maximum likelihood estimate of \code{sd} is the square root of the
#'     biased sample variance.
#'
#' For the density function of the normal distribution see \link[stats]{Normal}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mlnorm} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{mean} and \code{sd} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mlnorm(precip)
#' @seealso \link[stats]{Normal} for the normal density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York.
#' @export

mlnorm = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
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
