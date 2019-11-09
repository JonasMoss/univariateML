#' Power distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{alpha} is the maximum of \code{x} +
#'     \code{epsilon} (see the details) and the maximum likelihood estimate of
#'     \code{beta} is \code{1/(log(alpha)-mean(log(x)))}.
#'
#' For the density function of the power distribution see \link[extraDistr]{PowerDist}.
#'     The maximum likelihood estimator of \code{alpha} does not exist, strictly
#'     speaking. This is because \code{x} is supported \code{c(0, alpha)} with
#'     an open endpoint on alpha in the \code{extraDistr} implementation of
#'     \code{dpower}. If the endpoint was closed, \code{max(x)} would have been
#'     the maximum likelihood estimator. To overcome this problem, we add
#'     a possibly user specified \code{epsilon} to \code{max(x)}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param epsilon Positive number added to \code{max(x)} as an approximation
#'    to the maximum likelihood.
#' @return \code{mlpower} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{alpha} and \code{beta} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mlpower(precip)
#' @seealso \link[extraDistr]{PowerDist} for the power density. \link{Pareto} for
#'    the closely related Pareto distribution.
#' @references
#' Arslan, G. "A new characterization of the power distribution." Journal of Computational and Applied Mathematics 260 (2014): 99-102.
#' @export

mlpower = function(x, na.rm = FALSE, epsilon = .Machine$double.eps^0.5) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
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
