#' Laplace distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{mu} is the sample median while the
#'    maximum likelihood estimate of \code{sigma} is mean absolute deviation
#'    from the median.
#'
#' For the density function of the Laplace distribution see \link[extraDistr]{Laplace}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mllaplace} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{mu} and \code{sigma} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mllaplace(precip)
#' @seealso \link[extraDistr]{Laplace} for the Laplace density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 24. Wiley, New York.
#' @export

mllaplace = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  mu = stats::median(x)
  sigma = mean(abs(x - mu))
  object = c(mu = mu,
             sigma = sigma)
  class(object) = "univariateML"
  attr(object, "model") = "Laplace"
  attr(object, "density") = "extraDistr::dlaplace"
  attr(object, "logLik") = -length(x)*(1 + log(2*sigma))
  attr(object, "support") = c(-Inf, Inf)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
