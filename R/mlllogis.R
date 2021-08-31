#' Log-logistic distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `shape` and `rate` are calculated
#'    by transforming the data back to the logistic model and applying
#'    [`mllogis`][mllogis].
#'
#' For the density function of the log-logistic distribution see
#'    [Loglogistic][actuar::Loglogistic]
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mllogis`][mllogis].
#' @return `mlllogis` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape` and `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllnorm(precip)
#' @seealso [Loglogistic][actuar::Loglogistic] for the log-logistic density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions
#' in Economics and Actuarial Sciences, Wiley.
#'
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models,
#' From Data to Decisions, Fourth Edition, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for
#' actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export

mlllogis <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 0)

  y <- log(x)

  object <- mllogis(y)
  object[1] <- exp(-object[1])
  object[2] <- 1/object[2]
  object = rev(object)
  class(object) <- "univariateML"
  names(object) <- c("shape", "rate")

  attr(object, "model") <- "Loglogistic"
  attr(object, "density") <- "actuar::dllogis"
  attr(object, "logLik") <-
    sum(actuar::dllogis(x, object[1], object[2], log = TRUE))
  attr(object, "support") <- c(0, Inf)
  attr(object, "call") <- match.call()
  object
}
