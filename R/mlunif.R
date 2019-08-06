#' Estimates the parameters of uniform density using maximum likelihood
#'
#' The estimates are \code{min(x)} and \code{max(x)}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{min} and \code{max}.
#' @export


mlunif = function(x, na.rm = TRUE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  object = c(min(x), max(x))
  names(object) = c("alpha", "beta")
  class(object) = "univariateML"
  attr(object, "density") = "Uniform"
  object
}
