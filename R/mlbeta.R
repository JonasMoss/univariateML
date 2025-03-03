#' Beta distribution maximum likelihood estimation
#'
#' Uses `stat::nlm` to estimate the parameters of the Beta distribution.
#'
#' For the density function of the Beta distribution see [Beta][stats::Beta].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Ignored.
#' @return `mlbeta` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum
#'    likelihood estimates for `shape1` and `shape2` and the
#'    following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @details For `type`, the option `none` is fastest.
#' @seealso [Beta][stats::Beta] for the Beta density, [nlm][stats::nlm] for the
#'   optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York.
#'
#' @examples
#' AIC(mlbeta(USArrests$Rape / 100))
#' @export
mlbeta <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlbeta <- list(
  "model" = "Beta",
  "density" = "stats::dbeta",
  "support" = intervals::Intervals(c(0, 1), closed = c(FALSE, FALSE)),
  "names" = c("shape1", "shape2"),
  "defaults" = c(2, 3)
)

mlbeta_ <- function(x, ...) {
  s <- mean(log(x))
  r <- mean(log(1 - x))
  g1 <- exp(s)
  g2 <- exp(r)
  b <- log(0.5 + 0.5 * g2 / (1 - (g1 + g2)))
  n <- length(x)

  reltol <- .Machine$double.eps^0.25
  iterlim <- 100

  for (i in seq(iterlim)) {
    exp_b <- exp(b)
    digamma_b_r <- digamma(exp_b) - r
    trigamma_b <- trigamma(exp_b)

    # Inverts digamma(s-r+digamma(exp(b)))
    y <- s + digamma_b_r
    a <- 1 / log(1 + exp(-y))
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)

    # Construct derivative and hessian
    f <- exp_b * (digamma_b_r - digamma(a + exp_b))
    df <- f + exp_b^2 * (trigamma_b - trigamma(a + exp_b) * (trigamma_b / trigamma(a) + 1))

    b0 <- b - f / df
    if (abs((b0 - b) / b0) < reltol) break
    b <- b0
  }

  list(
    estimates = c(a, exp_b),
    logLik = n * ((a - 1) * s + (exp_b - 1) * r - lbeta(a, exp_b)),
    i = i
  )
}
