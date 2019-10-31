#' Log-logistic distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{shape} and \code{rate} are calculated
#'    by transforming the data back to the logistic model and applying \code{\link{mllogis}}
#'
#' For the density function of the log-logistic distribution see \link[actuar]{Loglogistic}
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mlllogis} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{shape} and \code{rate} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples mllnorm(precip)
#' @seealso \link[actuar]{Loglogistic} for the log-logistic density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley.
#'
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export

mlllogis = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) > 0)

  y = log(x)
  n = length(x)

  object = mllogis(y)
  class(object) = "univariateML"
  names(object) = c("shape", "rate")
  object[1] = exp(object[1])
  object[2] = 1/object[2]

  attr(object, "model") = "Loglogistic"
  attr(object, "density") = "actuar::dllogis"
  attr(object, "logLik") = sum(actuar::dllogis(x, object[1], object[2], log = TRUE))
  attr(object, "support") = c(0, Inf)
  attr(object, "call") = match.call()
  object
}
