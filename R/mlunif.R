#' Estimates the parameters of uniform density using maximum likelihood
#'
#' The estimates are \code{min(x)} and \code{max(x)}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{min} and \code{max}.
#' @export


mlunif = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  n = length(x)
  max_ = max(x)
  min_ = min(x)
  object = c(min = min_, max = max_)
  class(object) = "univariateML"
  attr(object, "model") = "Uniform"
  attr(object, "density") = "stats::dunif"
  attr(object, "logLik") = -n*log(max_ - min_)
  attr(object, "support") = c(min, max)
  object
}
