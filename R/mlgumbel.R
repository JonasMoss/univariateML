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
  "names" = c("mu", "sigma"),
  "default" = c(3, 3)
)

mlgumbel_ <- \(x, ...) {
  x_bar <- mean(x)
  f_over_df <- \(sigma0) {
    a <- sum(x * exp(-x / sigma0))
    b <- sum(exp(-x / sigma0))
    c <- sum(x^2 * exp(-x / sigma0))

    f <- x_bar - sigma0 - a / b
    df <- -1 - 1 / sigma0^2 * (c / b - (a / b)^2)

    f / df
  }

  dots <- list(...)
  sigma0 <- if (!is.null(dots$sigma0)) dots$sigma0 else 1
  sigma <- newton_raphson_1d(f_over_df, sigma0, ...)

  mu <- -sigma * log(mean(exp(-x / sigma)))
  s <- mean(exp(-(x - mu) / sigma))
  logLik <- -length(x) * (log(sigma) + 1 / sigma * (x_bar - mu) + s)

  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik)
}
