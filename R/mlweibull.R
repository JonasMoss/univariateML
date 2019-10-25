#' Estimates the parameter of a Weibull distribution by maximum likelihood
#'
#' Uses Newton-Raphson to estimate the parameters of the Weibull distribution.
#'
#' For the density function of the Weibull distribution see \link[stats]{dweibull}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param shape0 An optional starting value for the \code{shape} parameter.
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated.
#'
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{shape} and \code{scale}.
#' @seealso \link[stats]{dweibull} for the Weibull density.
#' @examples BIC(mlweibull(precip))
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 21. Wiley, New York.
#' @export

mlweibull = function(x, na.rm = FALSE, shape0 = 2,
                     rel.tol = .Machine$double.eps^0.25,
                     iterlim = 100) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  assertthat::assert_that(min(x) > 0)

  rel.tol_str = deparse(substitute(rel.tol))
  log_x = log(x)
  l_hat = mean(log_x)
  log_xsq = log_x^2

  for(i in 1:iterlim) {
    shape0_lsum     = mean(x^shape0*log_x)
    shape0_lsum_sqr = mean(x^shape0*log_xsq)
    shape0_sum      = mean(x^shape0)
    A = shape0_lsum/shape0_sum
    B = shape0_lsum_sqr/shape0_sum
    top = 1/shape0 + l_hat - A
    bottom = -1/shape0^2 + A^2 - B
    shape = shape0 - top/bottom

    if(abs((shape0 - shape)/shape0) < rel.tol) break

    shape0 = shape
  }

  if(i == iterlim) {
    warning(paste0("The iteration limit (iterlim = ", iterlim, ") was reached",
                   " before the relative tolerance requirement (rel.tol = ",
                   rel.tol_str, ")."))
  }

  ## Given the shape, the scale is easy to compute.
  scale = (mean(x^shape))^(1/shape)
  shape_sum = mean(x^shape)
  n = length(x)
  object = c(shape = shape, scale = scale)
  class(object) = "univariateML"
  attr(object, "model") = "Weibull"
  attr(object, "density") = "stats::dweibull"
  attr(object, "logLik") = n*(log(shape) - log(scale) +
                (shape - 1)*(l_hat - log(scale)) - scale^-shape*shape_sum)
  attr(object, "support") = c(0, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object

}
