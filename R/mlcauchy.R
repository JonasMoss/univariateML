#' Cauchy distribution maximum likelihood estimation
#'
#' Calculates the estimates using \code{nlm} and an exponential transform of the
#'     location parameter. If \code{n < 5}, an exact solution is reported. In
#'     the edge case where no maximum likelihood estimator exists and error is
#'     thrown.
#'
#' For the density function of the Cauchy distribution see \link[stats]{Cauchy}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mlcauchy} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{location} and \code{scale} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @seealso \link[stats]{Cauchy} for the Cauchy density, \link[stats]{nlm} for the
#'   optimizer this function uses.
#' @references  #' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 1, Chapter 16. Wiley, New York.
#' @examples mlcauchy(airquality$Temp)
#' @export

mlcauchy = function(x, na.rm = FALSE) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  m = stats::median(x)
  mad = stats::median(abs(x - m))

  start = c(m, mad)

  f = function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  values = suppressWarnings(stats::nlm(f = f,
                      p = start))

  object = c(location = values$estimate[1],
             scale = exp(values$estimate[2]))

  class(object) = "univariateML"
  attr(object, "model") = "Cauchy"
  attr(object, "density") = "stats::dcauchy"
  attr(object, "logLik") = -values$minimum
  attr(object, "support") = c(-Inf, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object

}
