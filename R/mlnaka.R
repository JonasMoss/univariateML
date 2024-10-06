#' Nakagami distribution maximum likelihood estimation
#'
#' The maximum likelihood estimates of `shape` and `scale` are calculated by
#'     calling `mlgamma` on the transformed data.
#'
#' For the density function of the Nakagami distribution see
#'     [Nakagami][nakagami::Nakagami].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlgamma`][mlgamma].
#' @return `mlgamma` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum
#'    likelihood estimates for `shape` and `rate` and the following
#'    attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlgamma(precip)
#' @seealso [Nakagami][nakagami::Nakagami] for the Nakagami distribution.
#'    [GammaDist][stats::GammaDist] for the closely related Gamma density.
#'    See [`mlgamma`][mlgamma] for the machinery underlying this function.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation of the
#' parameters of the gamma distribution and their bias." Technometrics 11.4
#' (1969): 683-690.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate
#' Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' @export

mlnaka <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 0)

  # Make a minimal reference to nakagami package so that R-CMD-CHECK recognizes it
  invisible(nakagami::dnaka)

  n <- length(x)

  object <- mlgamma(x^2, na.rm = na.rm, ...)

  object["rate"] <- 1 / object["rate"] * object["shape"]
  names(object) <- c("shape", "scale")

  shape <- object["shape"]
  scale <- object["scale"]

  class(object) <- "univariateML"
  attr(object, "model") <- "Nakagami"
  attr(object, "density") <- "nakagami::dnaka"
  attr(object, "logLik") <-
    unname(n * (shape * log(shape) + log(2) -
      lgamma(shape) - shape * log(scale)) +
      (2 * shape - 1) * sum(log(x)) - shape / scale * sum(x^2))
  attr(object, "support") <- c(0, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
