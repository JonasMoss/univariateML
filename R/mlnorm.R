#' Normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mean` is the empirical mean and the
#'     maximum likelihood estimate of `sd` is the square root of the
#'     biased sample variance.
#'
#' For the density function of the normal distribution see
#' [Normal][stats::Normal].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlnorm` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `mean` and `sd` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlnorm(precip)
#' @seealso [Normal][stats::Normal] for the normal density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 13. Wiley, New York.
#' @export
mlnorm <- \(x, na.rm = FALSE, ...) {}

metadata$mlnorm <- list(
  "model" = "Normal",
  "density" = "stats::dnorm",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mean", "sd"),
  "default" = c(0, 1)
)

mlnorm_ <- \(x, ...) {
  n <- length(x)

  mu <- mean(x)
  sigma <- sqrt(stats::var(x) * (n - 1) / n)

  estimates <- c(mu, sigma)
  logLik <- -n / 2 * (1 + log(2 * pi) + 2 * log(sigma))
  list(estimates = estimates, logLik = logLik)
}
