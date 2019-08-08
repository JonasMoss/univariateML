#' Estimates the parameter of the Rayleigh distribution using maximum likelihood
#'
#' Calculates the \code{sigma} parameter as the square root of half the
#'    empirical second moment.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with the maximum likelihood estimate for
#'    \code{sigma}.
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
  object

}
