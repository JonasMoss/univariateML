#' Rayleigh distribution maximum likelihood estimation
#'
#' Calculates the `sigma` parameter as the square root of half the
#'    empirical second moment.
#'
#' For the density function of the Rayleigh distribution see
#' [Rayleigh][extraDistr::Rayleigh].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlrayleigh` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `sigma` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlrayleigh(precip)
#' @seealso [Rayleigh][extraDistr::Rayleigh] for the Rayleigh density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 18. Wiley, New York.
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

mlrayleigh <- \(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  assertthat::assert_that(min(x) >= 0)

  sigma <- sqrt(1 / 2 * mean(x^2))
  object <- c(sigma = sigma)
  class(object) <- "univariateML"
  attr(object, "model") <- "Rayleigh"
  attr(object, "density") <- "extraDistr::drayleigh"
  attr(object, "logLik") <- length(x) * (mean(log(x) - 2 * log(sigma) - 1))
  attr(object, "support") <- c(0, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
