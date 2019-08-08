#' Estimates the parameter of the Inverse Gamma distribution using maximum likelihood
#'
#' Transforms the data and uses Newton-Raphson to estimate the parameters of the Gamma distribution.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated.
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{alpha} and \code{beta}.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation of the parameters of the gamma distribution and their bias." Technometrics 11.4 (1969): 683-690.
#' @export


mlinvgamma = function(x, na.rm = FALSE, rel.tol = .Machine$double.eps^0.25, iterlim = 100) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) > 0)

  object = mlgamma(1/x, na.rm = FALSE, rel.tol = rel.tol, iterlim = iterlim)
  L = mean(log(x))
  M = mean(1/x)
  names(object) = c("alpha", "beta")
  alpha = object[1]
  beta = object[2]
  class(object) = "univariateML"
  attr(object, "model") = "InvGamma"
  attr(object, "logLik") = unname(length(x)*(alpha*log(beta) - log(gamma(alpha)) +
                                -(alpha + 1)*L - beta*M))
  object
}
