#' Logistic distribution maximum likelihood estimation
#'
#' Calculates the estimates using `nlm` with an exponential transform of the
#'     location parameter.
#'
#' For the density function of the logistic distribution see
#'     [Logistic][stats::Logistic].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllogis` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `location` and `scale` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllogis(precip)
#' @seealso [Logistic][stats::Logistic] for the Logistic density,
#'   [nlm][stats::nlm] for the optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 23. Wiley, New York.
#' @export

mllogis <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  start <- c(m, log(mad))

  f <- function(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(
    f = f,
    p = start
  ))

  object <- c(
    location = values$estimate[1],
    scale = exp(values$estimate[2])
  )

  class(object) <- "univariateML"
  attr(object, "model") <- "Logistic"
  attr(object, "density") <- "stats::dlogis"
  attr(object, "logLik") <- -values$minimum
  attr(object, "support") <- c(-Inf, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
