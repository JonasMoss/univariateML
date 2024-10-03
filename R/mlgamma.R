#' Gamma distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Gamma distribution.
#'
#' For the density function of the Gamma distribution see
#' [GammaDist][stats::GammaDist].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `rel.tol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#' @return `mlgamma` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape` and `rate` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlgamma(precip)
#' @seealso [GammaDist][stats::GammaDist] for the Gamma density.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation
#' of the parameters of the gamma distribution and their bias."
#' Technometrics 11.4 (1969): 683-690.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 17. Wiley, New York.
#' @export
mlgamma <- \(x, na.rm = FALSE, ...) {}

metadata$mlgamma <- list(
  "model" = "Gamma",
  "density" = "stats::dgamma",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape", "rate")
)

mlgamma_ <- \(x, ...) {
  dots <- list(...)

  rel.tol <- if (!is.null(dots$rel.tol)) {
    dots$rel.tol
  } else {
    .Machine$double.eps^0.25
  }

  iterlim <- if (!is.null(dots$iterlim)) {
    dots$iterlim
  } else {
    100
  }

  n <- length(x)
  mean_hat <- mean(x)
  L <- mean(log(x))
  s <- log(mean_hat) - L

  ## This start estimator is very close to the ML estimator already.
  shape0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s))

  ## The Newton-Raphson steps.
  for (i in 1:iterlim) {
    shape <- shape0 - (log(shape0) - digamma(shape0) - s) /
      (1 / shape0 - trigamma(shape0))
    if (abs((shape - shape0) / shape0) < rel.tol) break
    shape0 <- shape
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (rel.tol = ",
      rel.tol, ")."
    ))
  }

  ## Given the shape, the rate is easy to compute.
  rate <- shape / mean_hat

  estimates <- c(shape = shape, rate = rate)
  logLik <- n * (shape * log(rate) - log(gamma(shape)) +
    (shape - 1) * L - rate * mean_hat)

  list(estimates = estimates, logLik = logLik)
}
