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
mlgumbel <- \(x, na.rm = FALSE, ...) {}

mlgumbel <- decorator("mlgumbel")

metadata$mlgumbel <- list(
  "model" = "Gumbel",
  "density" = "extraDistr::dgumbel",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "continuous" = TRUE,
  "names" = c("mu", "sigma"),
  "class" = "mlfun"
)

mllnorm <- \(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 0)
  y <- log(x)
  n <- length(x)
  meanlog <- mean(y)
  sdlog <- sqrt(stats::var(y) * (n - 1) / n)
  object <- c(meanlog = meanlog, sdlog = sdlog)
  class(object) <- "univariateML"
  attr(object, "model") <- "Lognormal"
  attr(object, "density") <- "stats::dlnorm"
  attr(object, "logLik") <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sdlog) + 2 * meanlog)
  attr(object, "support") <- c(0, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
