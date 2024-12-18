#' Gompertz distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Gompertz distribution.
#'
#' For some data sets the maximum likelihood estimator of `b` fails to exist
#'  since the root of the profile maximum likelihood equation is non-positive.
#'  The value `1e-06` is returned in this case, along with a warning.
#'
#' For the density function of the Gompertz distribution see
#' [Gompertz][extraDistr::Gompertz].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `reltol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#' @return `mlgompertz` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `a` and `b` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlgompertz(precip)
#' @seealso [Gompertz][extraDistr::Gompertz] for the Gompertz density.
#' @references Lenart, A. (2012). The Gompertz distribution and Maximum Likelihood Estimation of its parameters - a revision. MPIDR WORKING PAPER WP 2012-008. https://www.demogr.mpg.de/papers/working/wp-2012-008.pdf
#' @export
mlgompertz <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlgompertz <- list(
  "model" = "Gompertz",
  "density" = "extraDistr::dgompertz",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE)),
  "names" = c("a", "b"),
  "default" = c(1, 1)
)

mlgompertz_ <- \(x, ...) {
  n <- length(x)
  x_sum <- sum(x)
  x_var <- var(x) * (n - 1) / n
  if (x_sum / n < sqrt(x_var)) {
    stop("The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed. The exponential distribution will yield a better fit.")
  }

  f_over_df <- \(b) {
    exp_bx <- exp(b * x)
    sum_bx <- sum(exp_bx)
    sum_bx_x <- sum(x * exp_bx)
    sum_bx_x2 <- sum(x^2 * exp_bx)
    s <- sum_bx - n
    r <- b * sum_bx_x - s

    a <- b * n / s
    da <- -n * r / s^2
    d2a <- n / s^2 * (b * (2 * sum_bx_x^2 / s - sum_bx_x2) - 2 * sum_bx_x)

    f <- n * da / a + x_sum
    df <- n * (a * d2a - da^2) / a^2
    f / df
  }

  b <- newton_raphson_1d(f_over_df, 10e-2, ...)

  a <- b * n / (sum(exp(b * x)) - n)
  a <- max(a, 1e-09)

  list(
    estimates = c(a, b),
    logLik = sum(extraDistr::dgompertz(x, a, b, log = TRUE))
  )
}
