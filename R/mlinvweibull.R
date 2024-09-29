#' Inverse Weibull distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `shape` and `rate` are calculated
#'   by calling [`mlweibull`][mlweibull] on the transformed data.
#'
#' For the density function of the log normal distribution see
#' [InverseWeibull][actuar::InverseWeibull].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlweibull`][mlweibull].
#' @return `mlinvweibull` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `shape` and `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlinvweibull(precip)
#' @seealso [InverseWeibull][actuar::InverseWeibull] for the Inverse Weibull
#'    density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions
#' in Economics and Actuarial Sciences, Wiley.
#'
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models,
#' From Data to Decisions, Fourth Edition, Wiley.
#'
#' Dutang, C., Goulet, V., & Pigeon, M. (2008). actuar: An R package for
#' actuarial science. Journal of Statistical Software, 25(7), 1-37.
#' @export
mlinvweibull <- \(x, na.rm = FALSE, ...) {}

mlinvweibull <- decorator("mlinvweibull")

metadata$mlinvweibull <- list(
  "model" = "invweibull",
  "density" = "extraDistr::dinvweibull",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "continuous" = TRUE,
  "names" = c("mu", "sigma"),
  "class" = "mlfun"
)

mlinvweibull_ <- \(x, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 0)

  y <- 1 / x
  n <- length(x)

  object <- mlweibull(y, ...)
  names(object) <- c("shape", "rate")

  shape <- object[1]
  scale <- 1 / object[2]
  G <- mean(log(x))
  Ma <- mean(x^-shape)

  class(object) <- "univariateML"
  attr(object, "model") <- "InverseWeibull"
  attr(object, "density") <- "actuar::dinvweibull"
  attr(object, "logLik") <- unname(n * (log(shape) + shape * (log(scale) - G) -
    scale^shape * Ma - G))
  attr(object, "support") <- c(0, Inf)
  attr(object, "call") <- match.call()
  object
}
