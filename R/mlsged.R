#' Skew Generalized Error distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::sgedFit].
#'
#' For the density function of the Student t distribution see
#' [dsged][fGarch::sged].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlsged` returns an object of [class][base::class] `univariateML`.
#'   This is a named numeric vector with maximum likelihood estimates for the
#'   parameters `mean`, `sd`, `nu`, `xi`, and the following attributes:
#'   \item{`model`}{The name of the model.} \item{`density`}{The density
#'   associated with the estimates.} \item{`logLik`}{The loglikelihood at the
#'   maximum.} \item{`support`}{The support of the density.} \item{`n`}{The
#'   number of observations.} \item{`call`}{The call as captured my
#'   `match.call`}
#' @examples
#' mlsged(precip)
#' @seealso [dsged][fGarch::sged] for the Student-t density.
#' @references Nelson D.B. (1991); Conditional Heteroscedasticity in Asset
#'   Returns: A New Approach, Econometrica, 59, 347–370.
#'
#'   Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat Tails and
#'   Skewness, Preprint.
#' @export

mlsged <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  fit <- suppressWarnings(fGarch::sgedFit(x))
  object <- fit[["par"]]
  class(object) <- "univariateML"
  attr(object, "model") <- "Skew Generalized Error"
  attr(object, "density") <- "fGarch::dsged"
  attr(object, "logLik") <- -fit$objective
  attr(object, "support") <- c(-Inf, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
