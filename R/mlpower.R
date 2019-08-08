#' Estimates the parameter of the power distribution using maximum likelihood
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param epsilon Positive number added to \code{max(x)} as an approximation
#'    to the maximum likelihood.
#' @return A named numeric vector with maximum likelihood estimates for
#'     \code{alpha} and \code{beta}.
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
  attr(object, "logLik") = length(x)*(log(beta) - beta*log(alpha) + (beta - 1)*M)
  attr(object, "support") = c(0, alpha)
  object
}
