#' Estimates the parameter of the Inverse Gamma distribution using maximum likelihood
#'
#' Transforms the data and uses Newton-Raphson to estimate the parameters of the Gamma distribution.
#'
#' @param x The data from which the estimate is to be computed.
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated.
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{shape} and \code{rate}.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation of the parameters of the gamma distribution and their bias." Technometrics 11.4 (1969): 683-690.
#' @export


mlinvgamma = function(x, rel.tol = .Machine$double.eps^0.25, iterlim = 100) {
  assertthat::assert_that(min(x) > 0)
  object = mlgamma(1/x, rel.tol, iterlim)
  names(object) = c("alpha", "beta")
  class(object) = "univariateML"
  attr(object, "density") = "stats::dinvgamma"
  object
}
