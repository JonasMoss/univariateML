#' Inverse Gaussian (Wald) maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{mean} is the empirical mean and the
#'     maximum likelihood estimate of \code{1/shape} is the difference between
#'     the mean of reciprocals and the reciprocal of the mean.
#'
#' For the density function of the Inverse Gamma distribution see \link[actuar]{InverseGaussian}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mlinvgauss} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{mean} and \code{shape} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mlinvgauss(precip)
#' @seealso \link[actuar]{InverseGaussian} for the Inverse Gaussian density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 15. Wiley, New York.
#' @export

mlinvgauss= function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 0)
  n = length(x)

  mu = mean(x)
  lambda = 1/(mean(1/x) - 1/mu)
  object = c(mean = mu, shape = lambda)
  L = mean(log(x))
  S = mean((x - mean(x))^2/x)

  class(object) = "univariateML"
  attr(object, "model") = "Inverse Gaussian"
  attr(object, "density") = "actuar::dinvgauss"
  attr(object, "logLik") = -n/2*(3*L - log(lambda) + log(2*pi) + lambda/mu^2*S)
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}


