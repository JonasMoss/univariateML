#' Skew Student t-distribution maximum likelihood estimation
#'
#' Joint maximum likelihood estimation as implemented by [fGarch::sstdFit].
#'
#' For the density function of the skew Student t-distribution see
#' [sstd][fGarch::sstd].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlsstd` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    the parameters `mean`, `sd`, `nu`, `xi` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlsstd(precip)
#' @seealso [sstd][fGarch::sstd] for the Skew Student t-density.
#' @references Fernandez C., Steel M.F.J. (2000); On Bayesian Modelling of Fat
#'     Tails and Skewness, Preprint.
#' @export
mlsstd <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlsstd <- list(
  "model" = "Skew Student-t",
  "density" = "fGarch::dsstd",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mean", "sd", "nu", "xi"),
  "default" = c(0, 1, 3, 2)
)

mlsstd_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::sstdFit(x))
  list(estimates = fit[["estimate"]], logLik = -fit$minimum)
}
