#' Pareto distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{b} is the minimum of \code{x} and the
#'     maximum likelihood estimate of \code{a} is
#'     \code{1/(mean(log(x)) - log(b))}.
#'
#' For the density function of the Pareto distribution see \link[extraDistr]{Pareto}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mlpareto} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{a} and \code{b} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mlpareto(precip)
#' @seealso \link[extraDistr]{Pareto} for the Pareto density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 20. Wiley, New York.
#' @export

mlpareto = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 0)

  M = mean(log(x))
  b = min(x)
  a = 1/(M - log(b))

  object = c(a = a,
             b = b)

  class(object) = "univariateML"
  attr(object, "model") = "Pareto"
  attr(object, "density") = "extraDistr::dpareto"
  attr(object, "logLik") = length(x)*(log(a) + a*log(b) - (a + 1)*M)
  attr(object, "support") = c(b, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object

}
