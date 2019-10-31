#' Inverse Weibull distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{shape} and \code{rate} are calculated
#'   by calling \code{\link{mlweibull}} on the transformed data.
#'
#' For the density function of the log normal distribution see \link[actuar]{InverseWeibull}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{shape} and \code{rate}.
#' @examples mlinvweibull(precip)
#' @seealso \link[actuar]{InverseWeibull} for the Inverse Weibull density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley.
#'
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export

mlinvweibull = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  assertthat::assert_that(min(x) > 0)

  y = 1/x
  n = length(x)

  object = mlweibull(y)
  names(object) = c("shape", "rate")

  shape = object[1]
  scale = 1/object[2]
  G = mean(log(x))
  Ma = mean(x^-shape)

  class(object) = "univariateML"
  attr(object, "model") = "InverseWeibull"
  attr(object, "density") = "actuary::dinvweibull"
  attr(object, "logLik") =  unname(n*(log(shape) + shape*(log(scale) - G) -
                                        scale^shape*Ma - G))
  attr(object, "support") = c(0, Inf)
  attr(object, "call") = match.call()
  object
}
