#' Generalized Error distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::gedFit].
#'
#' For the density function of the Student t-distribution see
#' [ged][fGarch::ged].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlged` returns an object of [class][base::class] `univariateML`.
#'   This is a named numeric vector with maximum likelihood estimates for the
#'   parameters `mean`, `sd`, `nu` and the following attributes:
#'   \item{`model`}{The name of the model.}
#'   \item{`density`}{The density associated with the estimates.}
#'   \item{`logLik`}{The loglikelihood at the maximum.}
#'   \item{`support`}{The support of the density.}
#'   \item{`n`}{The number of observations.}
#'   \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlged(precip)
#' @seealso [ged][fGarch::ged] for the Student t-density.
#' @references Nelson D.B. (1991); Conditional Heteroscedasticity in Asset
#'   Returns: A New Approach, Econometrica, 59, 347<U+2013>370.
#'
#'   Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat Tails and
#'   Skewness, Preprint.
#' @export
mlged <- \(x, na.rm = FALSE, ...) {}

metadata$mlged <- list(
  "model" = "Generalized Error",
  "density" = "fGarch::dged",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mean", "sd", "nu"),
  "default" = c(0, 1, 3)
)

mlged_ <- \(x, ...) {
  fit <- suppressWarnings(fGarch::gedFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
