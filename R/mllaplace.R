#' Laplace distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mu` is the sample median while the
#'    maximum likelihood estimate of `sigma` is mean absolute deviation
#'    from the median.
#'
#' For the density function of the Laplace distribution see [Laplace][extraDistr::Laplace].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @return `mllaplace` returns an object of [class][base::class] `univariateML`. This
#'    is a named numeric vector with maximum likelihood estimates for `mu` and `sigma` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllaplace(precip)
#' @seealso [Laplace][extraDistr::Laplace] for the Laplace density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, Volume 2, Chapter 24. Wiley, New York.
#' @export

mllaplace <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  mu <- stats::median(x)
  sigma <- mean(abs(x - mu))
  object <- c(
    mu = mu,
    sigma = sigma
  )
  class(object) <- "univariateML"
  attr(object, "model") <- "Laplace"
  attr(object, "density") <- "extraDistr::dlaplace"
  attr(object, "logLik") <- -length(x) * (1 + log(2 * sigma))
  attr(object, "support") <- c(-Inf, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
