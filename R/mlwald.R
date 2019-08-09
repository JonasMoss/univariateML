#' Estimates the parameter of the Wald (inverse Gaussian) distribution using
#'     maximum likelihood
#'
#' The maximum likelihood estimate of \code{mu} is the empirical mean and the
#'     maximum likelihood estimate of \code{1/lambda} is the difference between
#'     the mean of reciprocals and the reciprocal of the mean.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{mu} and \code{lambda}.
#' @export

mlwald = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)]
  assertthat::assert_that(min(x) > 0)
  n = length(x)

  mu = mean(x)
  lambda = 1/(mean(1/x) - 1/mu)
  object = c(mu = mu, lambda = lambda)
  L = mean(log(x))
  S = mean((x - mean(x))^2/x)

  class(object) = "univariateML"
  attr(object, "model") = "Wald"
  attr(object, "density") = "extraDistr::dwald"
  attr(object, "logLik") = -n/2*(3*L - log(lambda) + log(2*pi) + lambda/mu^2*S)
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}


