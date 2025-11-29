#' Inverse Gaussian (Wald) maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mean` is the empirical mean and the
#'     maximum likelihood estimate of `1/shape` is the difference between
#'     the mean of reciprocals and the reciprocal of the mean.
#'
#' For the density function of the Inverse Gamma distribution see
#'     [InverseGaussian][actuar::InverseGaussian].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlinvgauss` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `mean` and `shape` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlinvgauss(precip)
#' @seealso [InverseGaussian][actuar::InverseGaussian] for the
#' Inverse Gaussian density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 15. Wiley, New York.
#' @export
mlinvgauss <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlinvgauss <- list(
  "model" = "Inverse Gaussian",
  "density" = "actuar::dinvgauss",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mean", "shape"),
  "default" = c(3, 4)
)

mlinvgauss_ <- function(x, ...) {
  n <- length(x)
  mu <- mean(x)
  lambda <- 1 / (mean(1 / x) - 1 / mu)
  estimates <- c(mu, lambda)

  L <- mean(log(x))
  S <- mean((x - mean(x))^2 / x)

  logLik <- -n / 2 * (3 * L - log(lambda) + log(2 * pi) + lambda / mu^2 * S)
  list(estimates = estimates, logLik = logLik)
}
