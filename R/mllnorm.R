#' Log-normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `meanlog` is the empirical mean of the
#'     log-transformed data and the maximum likelihood estimate of `sdlog`
#'     is the square root of the biased sample variance based on the
#'     log-transformed data.
#'
#' For the density function of the log normal distribution see
#'     [Lognormal][stats::Lognormal].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllnorm` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `meanlog` and `sdlog` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllnorm(precip)
#' @seealso [Lognormal][stats::Lognormal] for the log normal density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 14. Wiley, New York.
#' @export
mllnorm <- \(x, na.rm = FALSE, ...) {}

mllnorm <- decorator("mllnorm")

metadata$mllnorm <- list(
  "model" = "Lognormal",
  "density" = "stats::dlnorm",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "continuous" = TRUE,
  "names" = c("meanlog", "sdlog"),
  "class" = "mlfun"
)

mllnorm_ <- \(x, na.rm = FALSE, ...) {
  y <- log(x)
  n <- length(x)
  meanlog <- mean(y)
  sdlog <- sqrt(stats::var(y) * (n - 1) / n)
  estimates <- c(meanlog = meanlog, sdlog = sdlog)
  logLik <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sdlog) + 2 * meanlog)
  list(estimates = estimates, logLik = logLik)
}
