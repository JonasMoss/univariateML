#' Cauchy distribution maximum likelihood estimation
#'
#' Calculates the estimates using `nlm` and an exponential transform of the
#'     location parameter. If `n < 5`, an exact solution is reported. In
#'     the edge case where no maximum likelihood estimator exists and error is
#'     thrown.
#'
#' For the density function of the Cauchy distribution see
#'     [Cauchy][stats::Cauchy].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlcauchy` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `location` and `scale` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Cauchy][stats::Cauchy] for the Cauchy density, [nlm][stats::nlm]
#'   for the optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 16. Wiley, New York.
#' @examples
#' mlcauchy(airquality$Temp)
#' @export

mlcauchy <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  m <- stats::median(x)
  mad <- stats::median(abs(x - m))

  start <- c(m, mad)

  f <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(
    f = f,
    p = start
  ))

  object <- c(
    location = values$estimate[1],
    scale = exp(values$estimate[2])
  )

  class(object) <- "univariateML"
  attr(object, "model") <- "Cauchy"
  attr(object, "density") <- "stats::dcauchy"
  attr(object, "logLik") <- -values$minimum
  attr(object, "support") <- c(-Inf, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
