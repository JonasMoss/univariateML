#' Power distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `alpha` is the maximum of `x` +
#'     `epsilon` (see the details) and the maximum likelihood estimate of
#'     `beta` is `1/(log(alpha)-mean(log(x)))`.
#'
#' For the density function of the power distribution see
#'     [PowerDist][extraDistr::PowerDist]. The maximum likelihood estimator of
#'     `alpha` does not exist, strictly
#'     speaking. This is because `x` is supported `c(0, alpha)` with
#'     an open endpoint on alpha in the `extraDistr` implementation of
#'     `dpower`. If the endpoint was closed, `max(x)` would have been
#'     the maximum likelihood estimator. To overcome this problem, we add
#'     a possibly user specified `epsilon` to `max(x)`.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `epsilon` is a positive number added to `max(x)` as an to the
#'     maximum likelihood. Defaults to `.Machine$double.eps^0.5`.
#' @return `mlpower` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `alpha` and `beta` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpower(precip)
#' @seealso [PowerDist][extraDistr::PowerDist] for the power density. [extraDistr::Pareto]
#'    for the closely related Pareto distribution.
#' @references
#' Arslan, G. "A new characterization of the power distribution."
#' Journal of Computational and Applied Mathematics 260 (2014): 99-102.
#' @export
mlpower <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlpower <- list(
  "model" = "PowerDist",
  "density" = "extraDistr::dpower",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE)),
  "names" = c("alpha", "beta"),
  "default" = c(1, 2)
)

mlpower_ <- \(x, ...) {
  dots <- list(...)
  epsilon <- if (!is.null(dots$epsilon)) {
    dots$epsilon
  } else {
    .Machine$double.eps^0.5
  }

  m <- mean(log(x))
  alpha <- max(x) + epsilon
  beta <- 1 / (log(alpha) - m)

  estimates <- c(alpha, beta)
  logLik <-
    length(x) * (log(beta) - beta * log(alpha) + (beta - 1) * m)
  list(estimates = estimates, logLik = logLik)
}
