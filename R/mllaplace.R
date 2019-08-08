#' Estimates the parameter of the Laplace distribution using maximum likelihood
#'
#' The maximum likelihood estimate of \code{mu} is the sample median while the
#'    maximum likelihood estimate of \code{sigma} is mean absolute deviation
#'    from the median.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{mu} and \code{sigma}.
#' @export

mllaplace = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  mu = stats::median(x)
  sigma = mean(abs(x - mu))
  object = c(mu = mu,
             sigma = sigma)
  class(object) = "univariateML"
  attr(object, "model") = "Laplace"
  attr(object, "logLik") = -length(x)*(1 + log(2*sigma))
  attr(object, "support") = c(-Inf, Inf)
  object
}
