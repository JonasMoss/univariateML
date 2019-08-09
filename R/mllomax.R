#' Estimates the parameters of the Lomax distribution by maximum likelihood
#'
#' Uses Newton-Raphson to estimate the parameters of the Lomax distribution.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param start An optional starting value for the \code{lambda} parameter.
#'    Defaults to \code{median(x)}.
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated.
#'
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{lambda} and \code{kappa}.
#' @export

mllomax = function(x, na.rm = FALSE, start = stats::median(x),
                   rel.tol = .Machine$double.eps^0.25,
                   iterlim = 100) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) >= 0)

  rel.tol_str = deparse(substitute(rel.tol))
  n = length(x)

  # Check if

  lambda0 = start

  for(i in 1:iterlim) {
    S = mean(log(1 + lambda0*x))
    S1 = mean(x/(1 + lambda0*x))
    S2 = -mean(x^2/(1 + lambda0*x)^2)
    top = 1/lambda0 - S1/S - S1
    bottom = -1/lambda0^2 - S2/S + (S1/S)^2 - S2
    lambda = lambda0 - top/bottom

    if(abs((lambda0 - lambda)/lambda0) < rel.tol) break
    if(lambda < 0.00001) stop("The maximum likelihood estimator of the Lomax distribution does not exist. ")
    lambda0 = lambda
  }

  if(i == iterlim) {
      warning(paste0("The iteration limit (iterlim = ", iterlim, ") was reached",
                     " before the relative tolerance requirement (rel.tol = ",
                     rel.tol_str, ")."))
  }

  S = mean(log(1 + lambda*x))
  object = c(lambda = lambda,
             kappa = 1/mean(log(1 + lambda*x)))
  class(object) = c("univariateML")
  attr(object, "logLik") = n*(log(lambda) - log(S) - S - 1)
  attr(object, "model") = "Lomax"
  attr(object, "density") = "extraDistr::dlomax"
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object

}

