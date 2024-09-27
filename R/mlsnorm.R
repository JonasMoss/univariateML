#' Skew Normal distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::snormFit].
#'
#' For the density function of the Student t distribution see
#' [dsnorm][fGarch::snorm].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlsnorm` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    the parameters `mean`, `sd`, `xi` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlsnorm(precip)
#' @seealso [dsnorm][fGarch::snorm] for the Student-t density.
#' @references Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat
#'     Tails and Skewness, Preprint.
#' @export

mlsnorm <- \(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  fit <- suppressWarnings(fGarch::snormFit(x))
  object <- fit[["par"]]
  class(object) <- "univariateML"
  attr(object, "model") <- "Skew Normal"
  attr(object, "density") <- "fGarch::dsnorm"
  attr(object, "logLik") <- -fit$objective
  attr(object, "support") <- c(-Inf, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
