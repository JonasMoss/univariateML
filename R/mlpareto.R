#' Estimates the parameter of the Pareto distribution using maximum likelihood
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#'     \code{a} and \code{b}.
#' @export

mlpareto = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) >= 0)

  M = mean(log(x))
  b = min(x)
  a = 1/(M - log(b))

  object = c(a = a,
             b = b)

  class(object) = "univariateML"
  attr(object, "model") = "Pareto"
  attr(object, "logLik") = n*(log(a) + a*log(b) - (a + 1)*M)
  object

}
