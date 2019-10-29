#' Logistic distribution maximum likelihood estimation
#'
#' Calculates the estimates using \code{nlm} and an exponential transform of the
#'     location parameter. If \code{n < 5}, an exact solution is reported. In
#'     the edge case where no maximum likelihood estimator exists and error is
#'     thrown.
#'
#' For the density function of the logistic distribution see \link[stats]{Logistic}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{location} and \code{scale}.
#' @examples mllogis(precip)
#' @seealso \link[stats]{Logistic} for the Logistic density, \link[stats]{nlm} for the
#'   optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 23. Wiley, New York.
#' @export

mllogis = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  m = stats::median(x)
  mad = stats::median(abs(x - m))
  start = c(m, log(mad))

  f = function(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  values = suppressWarnings(stats::nlm(f = f,
                      p = start))

  object = c(location = values$estimate[1],
             scale = exp(values$estimate[2]))
  class(object) = "univariateML"
  attr(object, "model") = "Logistic"
  attr(object, "density") = "stats::dlogis"
  attr(object, "logLik") = -values$minimum
  attr(object, "support") = c(-Inf , Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object

}
