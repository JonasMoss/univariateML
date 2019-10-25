#' Estimates the parameter of the power distribution using maximum likelihood
#'
#' The maximum likelihood estimate of \code{alpha} is the maximum of \code{x} +
#'     \code{epsilon} (see the details) and the maximum likelihood estimate of
#'     \code{beta} is \code{1/(log(alpha)-mean(log(x)))}.
#'
#' For the density function of the power distribution see \link[extraDistr]{dpower}.
#'     The maximum likelihood estimator of \code{alpha} does not exist, strictly
#'     speaking. This is because \code{x} is supported \code{c(0, alpha)} with
#'     an open endpoint on alpha in the \code{extraDistr} implementation of
#'     \code{dpower}. If the endpoint was closed, \code{max(x)} would have been
#'     the maximum likelihood estimator. To overcome this problem, we add
#'     a possibly user specified \code{epsilon} to \code{max(x)}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param epsilon Positive number added to \code{max(x)} as an approximation
#'    to the maximum likelihood.
#' @return A named numeric vector with maximum likelihood estimates for
#'     \code{alpha} and \code{beta}.
#' @examples mlpower(precip)
#' @seealso \link[extraDistr]{dpower} for the power density. \link{mlpareto} for
#'    the closely related Pareto distribution.
#' @references
#' Arslan, G. "A new characterization of the power distribution." Journal of Computational and Applied Mathematics 260 (2014): 99-102.
#' @export

mlpower = function(x, na.rm = FALSE, epsilon = .Machine$double.eps^0.5) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) >= 0)

  M = mean(log(x))
  alpha = max(x) + epsilon
  beta = 1/(log(alpha) - M)

  object = c(alpha = alpha,
             beta = beta)

  class(object) = "univariateML"
  attr(object, "model") = "PowerDist"
  attr(object, "density") = "extraDistr::dpower"
  attr(object, "logLik") = length(x)*(log(beta) - beta*log(alpha) + (beta - 1)*M)
  attr(object, "support") = c(0, alpha)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
