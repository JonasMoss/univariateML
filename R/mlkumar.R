#' Kumaraswamy distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Kumaraswamy distribution.
#'
#' For the density function of the Kumaraswamy distribution see \link[extraDistr]{Kumaraswamy}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param a0 An optional starting value for the \code{a} parameter.
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#'     iterations to be performed before the program is terminated.
#'
#' @return \code{mlkumar} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{a} and \code{b} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @seealso \link[extraDistr]{Kumaraswamy} for the Kumaraswamy density.
#' @examples AIC(mlkumar(USArrests$Rape/100))
#' @references Jones, M. C. "Kumaraswamy's distribution: A beta-type distribution with some tractability advantages." Statistical Methodology 6.1 (2009): 70-81.
#'
#'      Kumaraswamy, Ponnambalam. "A generalized probability density function for double-bounded random processes." Journal of Hydrology 46.1-2 (1980): 79-88.
#' @export

mlkumar = function(x, na.rm = FALSE, a0 = 1, rel.tol = .Machine$double.eps^0.25,
                    iterlim = 100) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) > 0)
  assertthat::assert_that(max(x) < 1)

  rel.tol_str = deparse(substitute(rel.tol))

  logs = log(x)

  for(i in 1:iterlim) {

    xa = x^a0
    T1 = a0*mean(logs/(1-xa))
    T2 = a0*mean(logs*(xa/(1-xa)))
    T3 = mean(log(1-xa))
    f = 1/a0*(1 + T1 + T2/T3)

    C = mean(logs^2*(xa/(1-xa)^2))
    D = mean(logs^2*(xa/(1-xa)))

    T1diff = 1/a0*T1 + a0*C
    T2diff = 1/a0*T2  + 1/a0*T2^2 + a0*D

    fdiff = -1/a0^2*f + 1/a0*(T1diff + T2diff/T3 + 1/a0*(T2/T3)^2)

    a = a0 - f/fdiff
    if(abs((a0 - a)/a0) < rel.tol) break
    a0 = a

  }

  if(i == iterlim) {
    warning(paste0("The iteration limit (iterlim = ", iterlim, ") was reached",
                   " before the relative tolerance requirement (rel.tol = ",
                   rel.tol_str, ")."))
  }

  ## Given the shape, the scale is easy to compute.
  b = -1/mean(log(1 - x^a))

  object = c(a = a, b = b)
  class(object) = "univariateML"
  attr(object, "model") = "Kumaraswamy"
  attr(object, "density") = "extraDistr::dkumar"
  attr(object, "logLik") = length(x)*(log(a) + log(b) + (a - 1)*mean(log(x)) +
                                        -1 + 1/b)
  attr(object, "support") = c(0, 1)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
