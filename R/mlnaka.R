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
#'     \item{`call`}{The call as captured by `match.call`}
#'     \item{`continuous`}{Is the density continuous or discrete?}
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
mlnaka <- \(x, na.rm = TRUE, ...) {}

metadata$mlnaka <- list(
  "model" = "Nakagami",
  "density" = "nakagami::dnaka",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape", "scale"),
  "default" = c(2, 3)
)

mlnaka_ <- \(x, ...) {
  estimates <- mlgamma(x^2, na.rm = TRUE, ...)
  estimates["rate"] <- 1 / estimates["rate"] * estimates["shape"]
  n <- length(x)

  shape <- estimates[1]
  scale <- estimates[2]

  logLik <-
    unname(n * (shape * log(shape) + log(2) -
      lgamma(shape) - shape * log(scale)) +
      (2 * shape - 1) * sum(log(x)) - shape / scale * sum(x^2))

  list(estimates = estimates, logLik = logLik)
}
