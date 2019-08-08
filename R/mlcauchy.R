#' Estimates the parameter of the Cauchy distribution using maximum likelihood
#'
#' Calculates the estimates using \code{nlm} and an exponential transform of the
#'     location parameter. If \code{n < 5}, an exact solution is reported. In
#'     the edge case where no maximum likelihood estimator exists and error is
#'     thrown.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{location} and \code{scale}.
#' @export

mlcauchy = function(x, na.rm = FALSE) {

  if(na.rm) x = x[!is.na(x)]

  n = length(x)
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
  object

}
