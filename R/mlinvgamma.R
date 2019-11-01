#' Inverse Gamma distribution maximum likelihood estimation
#'
#' Transforms the data and uses Newton-Raphson to estimate the parameters of the Gamma distribution.
#'
#' For the density function of the Gamma distribution see \link[extraDistr]{InvGamma}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated.
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{alpha} and \code{beta}.
#' @examples mlinvgamma(precip)
#' @seealso \link[extraDistr]{InvGamma} for the Inverse Gamma density.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation of the parameters of the gamma distribution and their bias." Technometrics 11.4 (1969): 683-690.
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York.
#' Witkovsky, V. (2001). "Computing the Distribution of a Linear Combination of Inverted Gamma Variables". Kybernetika. 37 (1): 79–90
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
  attr(object, "density") = "extraDistr::dinvgamma"
  attr(object, "logLik") = unname(length(x)*(alpha*log(beta) - log(gamma(alpha)) +
                                -(alpha + 1)*L - beta*M))
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
