#' Laplace distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mu` is the sample median while the
#'    maximum likelihood estimate of `sigma` is mean absolute deviation
#'    from the median.
#'
#' For the density function of the Laplace distribution see
#' [Laplace][extraDistr::Laplace].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllaplace` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `mu` and `sigma` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllaplace(precip)
#' @seealso [Laplace][extraDistr::Laplace] for the Laplace density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 24. Wiley, New York.
#' @export
mllaplace <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mllaplace <- list(
  "model" = "Laplace",
  "density" = "extraDistr::dlaplace",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("mu", "sigma"),
  "default" = c(0, 1)
)

mllaplace_ <- function(x, ...) {
  mu <- stats::median(x)
  sigma <- mean(abs(x - mu))
  estimates <- c(mu = mu, sigma = sigma)
  logLik <- -length(x) * (1 + log(2 * sigma))
  list(estimates = estimates, logLik = logLik)
}
