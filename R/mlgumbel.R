#' Gumbel distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Gumbel distribution.
#'
#' For the density function of the Gumbel distribution see
#'    [Gumbel][extraDistr::Gumbel].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `sigma0` is an optional starting value defaulting to `1`.
#'     `reltol` is the relative accuracy requested, defaults to
#'     `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#' @return `mlgumbel` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for `mu`
#'    and `s` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' `shape` and `sigma`.
#' @examples
#' mlgumbel(precip)
#' @seealso [Gumbel][extraDistr::Gumbel] for the Gumbel density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 22. Wiley, New York.
#' @export
mlgumbel <- \(x, na.rm = FALSE, ...) {}

metadata$mlgumbel <- list(
  "model" = "Gumbel",
  "density" = "extraDistr::dgumbel",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mu", "sigma")
)

mlgumbel_ <- \(x, ...) {
  dots <- list(...)

  sigma0 <- if (!is.null(dots$sigma0)) {
    dots$sigma0
  } else {
    1
  }

  reltol <- if (!is.null(dots$reltol)) {
    dots$reltol
  } else {
    .Machine$double.eps^0.25
  }

  iterlim <- if (!is.null(dots$iterlim)) {
    dots$iterlim
  } else {
    100
  }

  mean_x <- mean(x)

  for (i in 1:iterlim) {
    A <- sum(x * exp(-x / sigma0))
    B <- sum(exp(-x / sigma0))
    C <- sum(x^2 * exp(-x / sigma0))

    top <- mean_x - sigma0 - A / B
    bottom <- -1 - 1 / sigma0^2 * (C / B - (A / B)^2)

    sigma <- sigma0 - top / bottom

    if (abs((sigma0 - sigma) / sigma0) < reltol) break

    sigma0 <- sigma
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }

  ## Given the sigma, the mu is easy to compute.
  mu <- -sigma * log(mean(exp(-x / sigma)))
  S <- mean(exp(-(x - mu) / sigma))

  estimates <- c(mu = mu, sigma = sigma)
  logLik <- -length(x) * (log(sigma) + 1 / sigma * (mean_x - mu) + S)
  list(estimates = estimates, logLik = logLik)
}
