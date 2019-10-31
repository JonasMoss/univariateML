#' Logit-Normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of \code{mu} is the empirical mean of the
#'     logit transformed data and the maximum likelihood estimate of
#'     \code{sigma} is the square root of the logit transformed
#'     biased sample variance.
#'
#' For the density function of the logit-normal distribution see \link[logitnorm]{dlogitnorm}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @return \code{mllogitnorm} returns an object of \link[base]{class} \code{univariateML}. This
#'    is a named numeric vector with maximum likelihood estimates for \code{mu} and \code{sigma} and the following attributes:
#'     \item{\code{model}}{The name of the model.}
#'     \item{\code{density}}{The density associated with the estimates.}
#'     \item{\code{logLik}}{The loglikelihood at the maximum.}
#'     \item{\code{support}}{The support of the density.}
#'     \item{\code{n}}{The number of observations.}
#'     \item{\code{call}}{The call as captured my \code{match.call}}
#' @examples AIC(mllogitnorm(USArrests$Rape/100))
#' @seealso link[dlogitnorm]{dlogitnorm}for the normal density.
#' @references Atchison, J., & Shen, S. M. (1980). Logistic-normal distributions: Some properties and uses. Biometrika, 67(2), 261-272.
#' @export

mllogitnorm = function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  assertthat::assert_that(min(x) > 0)
  assertthat::assert_that(max(x) < 1)

  n = length(x)
  y = stats::qlogis(x)
  mu = mean(y)
  sigma = sqrt(stats::var(y)*(n-1)/n)

  H = mean(log(x))
  G = mean(log(1 - x))
  object = c(mu = mu, sigma = sigma)
  class(object) = c("univariateML")
  attr(object, "model") = "LogitNormal"
  attr(object, "density") = "logitnorm::dlogitnorm"
  attr(object, "logLik") = -n/2*(1 + log(2*pi) + 2*log(sigma) + 2*H + 2*G)
  attr(object, "support") = c(0, 1)
  attr(object, "n") = length(x)
  attr(object, "call") = match.call()
  object
}
