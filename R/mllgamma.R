#' Log-gamma distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `shapelog` and `ratelog` are calculated
#'   by calling [mlgamma()] on the transformed data.
#'
#' For the density function of the log normal distribution see
#'   [Loggamma][actuar::Loggamma].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlgamma`][mlgamma].
#' @return `mllgamma` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shapelog` and `ratelog` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllgamma(precip)
#' @seealso [Loggamma][actuar::Loggamma] for the log normal density.
#' @references Hogg, R. V. and Klugman, S. A. (1984), Loss Distributions, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for
#' actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export

mllgamma <- \(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 1)

  y <- log(x)
  n <- length(x)

  object <- mlgamma(y, na.rm = na.rm, ...)
  shapelog <- object[1]
  ratelog <- object[2]
  L <- mean(y)
  K <- mean(log(y))
  names(object) <- c("shapelog", "ratelog")
  attr(object, "model") <- "Loggamma"
  attr(object, "density") <- "actuar::dlgamma"
  attr(object, "support") <- c(1, Inf)
  attr(object, "logLik") <-
    unname(n * (shapelog * log(ratelog) - log(gamma(shapelog)) +
      (shapelog - 1) * K - (ratelog + 1) * L))

  attr(object, "call") <- match.call()
  object
}
