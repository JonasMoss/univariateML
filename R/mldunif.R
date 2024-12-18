#' Discrete uniform distribution maximum likelihood estimation
#'
#' For the density function of the Discrete uniform distribution see
#' [DiscreteUniform][extraDistr::ddunif].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... not in use.
#' @return `mldunif` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `min` and `max` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mldunif(corbet)
#' @seealso [DiscreteUniform][extraDistr::ddunif] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mldunif <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mldunif <- list(
  "model" = "Discrete uniform",
  "density" = "extraDistr::ddunif",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE), type = "Z"),
  "names" = c("min", "max"),
  "default" = c(2, 10)
)

mldunif_ <- \(x, ...) {
  n <- length(x)
  estimates <- c(min(x), max(x))
  list(
    estimates = estimates,
    logLik = -n * log(estimates[2] - estimates[1] + 1)
  )
}
