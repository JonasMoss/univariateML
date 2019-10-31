#' Rayleigh distribution maximum likelihood estimation
#'
#' Calculates the \code{sigma} parameter as the square root of half the
#'    empirical second moment.
#'
#' For the density function of the Rayleigh distribution see \link[extraDistr]{Rayleigh}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mlrayleigh} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{sigma} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mlrayleigh(precip)
#' @seealso \link[extraDistr]{Rayleigh} for the Rayleigh density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 18. Wiley, New York.
#' @export

mlrayleigh = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  assertthat::assert_that(min(x) >= 0)

  sigma = sqrt(1/2*mean(x^2))
  object = c(sigma = sigma)
  class(object) = "univariateML"
  attr(object, "model") = "Rayleigh"
  attr(object, "density") = "extraDistr::drayleigh"
  attr(object, "logLik") = length(x)*(mean(log(x) - 2*log(sigma) - 1))
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object

}
