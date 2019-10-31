#' Log-gamma distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{shapelog} and \code{ratelog} are calculated
#'   by calling \code{\link{mlgamma}} on the transformed data.
#'
#' For the density function of the log normal distribution see \link[actuar]{Loggamma}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param rel.tol Relative accuracy; passed to \code{mlgamma}.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated. Passed to
#' \code{mlgamma}.
#' @return \code{mllgamma} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{shapelog} and \code{ratelog} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mllgamma(precip)
#' @seealso \link[actuar]{Loggamma} for the log normal density.
#' @references Hogg, R. V. and Klugman, S. A. (1984), Loss Distributions, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export

mllgamma = function(x, na.rm = FALSE, rel.tol = .Machine$double.eps^0.25,
                   iterlim = 100) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) > 1)

  y = log(x)
  n = length(x)

  object = mlgamma(y, na.rm = na.rm, rel.tol = rel.tol, iterlim = iterlim)
  shapelog = object[1]
  ratelog = object[2]
  L = mean(y)
  K = mean(log(y))
  names(object) = c("shapelog", "ratelog")
  attr(object, "model") = "Loggamma"
  attr(object, "density") = "actuar::dlgamma"
  attr(object, "support") = c(1, Inf)
  attr(object, "logLik") = unname(n*(shapelog*log(ratelog) - log(gamma(shapelog)) +
                                  (shapelog - 1)*K - (ratelog + 1)*L))

  attr(object, "call") = match.call()
  object

}
