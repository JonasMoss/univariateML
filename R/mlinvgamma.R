#' Inverse Gamma distribution maximum likelihood estimation
#'
#' Transforms the data and uses Newton-Raphson to estimate the parameters of
#' the Gamma distribution.
#'
#' For the density function of the inverse Gamma distribution see
#' [InvGamma][extraDistr::InvGamma].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlgamma`][mlgamma].
#' @return A named numeric vector with maximum likelihood estimates for
#' `alpha` and `beta`.
#' @examples
#' mlinvgamma(precip)
#' @seealso [InvGamma][extraDistr::InvGamma] for the Inverse Gamma density.
#' @references Choi, S. C, and R. Wette. "Maximum likelihood estimation of
#' the parameters of the gamma distribution and their bias."
#' Technometrics 11.4 (1969): 683-690.
#'
#' Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 17. Wiley, New York.
#'
#' Witkovsky, V. (2001). "Computing the Distribution of a Linear Combination
#' of Inverted Gamma Variables". Kybernetika. 37 (1): 79<U+2013>90
#'
#' @export
mlinvgamma <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlinvgamma <- list(
  "model" = "InvGamma",
  "density" = "extraDistr::dinvgamma",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("alpha", "beta"),
  "default" = c(3, 4)
)

mlinvgamma_ <- function(x, ...) {
  gamma_ml <- mlgamma_(1 / x, ...)
  alpha <- gamma_ml$estimates[1]
  beta <- gamma_ml$estimates[2]

  L <- mean(log(x))
  M <- mean(1 / x)
  logLik <- length(x) * (alpha * log(beta) - log(gamma(alpha)) - (alpha + 1) * L - beta * M)

  list(estimates = gamma_ml$estimates, logLik = unname(logLik))
}
