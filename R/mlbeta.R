#' Beta distribution maximum likelihood estimation
#'
#' Uses \code{stat::nlm} to estimate the parameters of the Beta distribution.
#'
#' For the density function of the Beta distribution see \link[stats]{Beta}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param start Optional starting parameter values for the minimization.
#'   Passed to the \code{stats::nlm} function.
#' @param type Whether a dedicated \code{"gradient"}, \code{"hessian"}, or
#'  \code{"none"} should be passed to \code{stats::nlm}.
#' @return A named numeric vector with maximum likelihood estimates for
#'  \code{shape1} and \code{shape2}.
#' @details For \code{type}, the option \code{none} is fastest.
#' @seealso \link[stats]{Beta} for the Beta density, \link[stats]{nlm} for the
#'   optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York.
#' @examples AIC(mlbeta(USArrests$Rape/100))
#' @export

mlbeta = function(x, na.rm = FALSE, start = NULL,
                  type = c("none", "gradient", "hessian")) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  assertthat::assert_that(min(x) > 0)
  assertthat::assert_that(max(x) < 1)

  type = match.arg(type)

  val1 = mean(log(x))
  val2 = mean(log(1-x))

  if(is.null(start)) {
    G1 = exp(val1)
    G2 = exp(val2)
    denom = 1/2*(1/(1-G1-G2))
    start = c(1/2 + G1*denom, 1/2 + G2*denom)
  }

  objective = function(p) {
    lbeta(p[1], p[2]) - (p[1] - 1)*val1 - (p[2] - 1)*val2
  }

  gradient = function(p) {
    digamma_alpha_beta = digamma(p[1] + p[2])
    c(digamma(p[1]) - digamma_alpha_beta - val1,
      digamma(p[2]) - digamma_alpha_beta - val2)
  }

  if(type == "gradient") {
    beta_objective = function(p) {
      result = objective(p)
      attr(result, "gradient") = gradient(p)
      result
    }
  } else if(type == "hessian") {
    hessian = function(p) {
      trigamma_alpha_beta = -trigamma(p[1] + p[2])
      matrix(trigamma_alpha_beta, nrow = 2, ncol = 2) +
        diag(c(trigamma(p[1]), trigamma(p[2])))
    }

    beta_objective = function(p) {
      result = objective(p)
      attr(result, "gradient") = gradient(p)
      attr(result, "hessian") = hessian(p)
      result
    }
  } else {
    beta_objective = objective
  }

  object = stats::nlm(beta_objective, p = start, typsize = start)$estimate
  names(object) = c("shape1", "shape2")
  class(object) = "univariateML"
  attr(object, "model") = "Beta"
  attr(object, "density") = "stats::dbeta"
  attr(object, "logLik") = -length(x)*stats::setNames(objective(object), NULL)
  attr(object, "support") = c(0, 1)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object

}
