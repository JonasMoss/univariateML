#' Gumbel distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Gumbel distribution.
#'
#' For the density function of the Gumbel distribution see
#'    [Gumbel][extraDistr::Gumbel].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `reltol` is the relative accuracy requested, defaults to
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

univariateML_metadata$mlgumbel <- list(
  "model" = "Gumbel",
  "density" = "extraDistr::dgumbel",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mu", "sigma"),
  "default" = c(3, 3)
)

mlgumbel_ <- \(x, ...) {
  x_bar <- sum(x) / length(x)
  estimates <- mlgumbel_estimate(x - x_bar, ...)
  mu <- estimates[1] + x_bar
  sigma <- estimates[2]
  logLik <- -length(x) * (log(sigma) + (x_bar - mu) / sigma + 1)
  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik)
}

mlgumbel_estimate <- \(x, ...) {
  x2 <- x^2
  f_over_df <- \(sigma0) {
    neg_sigma_inv <- -1 / sigma0
    exps <- exp(x * neg_sigma_inv)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x2 * exps) / psi0 - psi1^2
    f <- sigma0 + psi1
    df <- 1 + neg_sigma_inv^2 * psi2
    f / df
  }

  sigma0 <- sqrt(sum(x^2) / length(x)) * 2.45 / pi * (1 - 1 / length(x))
  sigma <- newton_raphson_1d(f_over_df, sigma0, ...)
  neg_sigma_inv <- -1 / sigma
  psi0 <- sum(exp(x * neg_sigma_inv))
  mu <- -sigma * log(psi0 / length(x))
  c(mu, sigma)
}
