#' Estimates the parameter of the Beta prime distribution using maximum likelihood
#'
#' Transforms the data and uses \code{stat::nlm} to estimate the parameters
#'     of the Beta distribution.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param start Optional starting parameter values for the minimization.
#' Passed to the \code{stats::nlm} function.
#' @param type Whether a dedicated \code{gradient} or \code{hessian} should be
#' passed to \code{stats::nlm}.
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{shape1} and \code{shap2}.
#' @details For \code{type}, the option \code{none} is fastest.
#' @export

mlbetapr = function(x, na.rm = TRUE, start = NULL, type = c("none", "gradient", "hessian")) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) > 0)

  object = mlbeta(x/(x + 1), start, type)
  class(object) = "univariateML"
  attr(object, "model") = "BetaPrime"
  object


}
