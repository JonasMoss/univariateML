#' Burr distribution maximum likelihood estimation
#'
#' The maximum likelihood estimator fails to exist when the data contains no values
#' strictly smaller than 1. Then the likelihood converges
#' to the likelihood of the [Pareto distribution][mlpareto] in this case.
#'
#' This function estimates the only the shape parameters of the Burr distribution.
#' The shape is set to 1.
#'
#' For the density function of the Burr distribution see [Burr][actuar::dburr].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlburr` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape1` and `shape2` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlburr(abalone$length)
#' @seealso [Burr][actuar::dburr] for the Burr density.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous
#' Univariate Distributions, Volume 1, Chapter 20. Wiley, New York.
#' @export
mlburr <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlburr <- list(
  "model" = "Burr",
  "density" = "actuar::dburr",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape1", "shape2"),
  "default" = c(1, 2)
)


mlburr_ <- function(x, ...) {
  if (all(x >= 1)) {
    stop("The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlburr` to fit a Pareto distribution.")
  }

  n <- length(x)
  log_x <- log(x)
  lx_sum <- sum(log_x)
  f_over_df <- function(c) {
    x_c <- x^c
    x_c_div <- x_c / (x_c + 1)
    g <- sum(log(1 + x_c))
    dg <- sum(log_x * x_c_div)
    d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
    f <- n / c - n * dg / g + lx_sum - dg
    df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
    f / df
  }

  # Moment approximation to c from linear regression.
  c0 <- 0.385 / (sum(log_x * log_x) / n)^(1 / 3)
  # c0 <- 4.953032*(sum(log_x * log_x) / n)^(-0.46)
  c <- newton_raphson_1d(f_over_df, c0, ...)
  k <- n / sum(log(1 + x^c))

  list(
    estimates = c(k, c),
    logLik = sum(actuar::dburr(x, k, c, log = TRUE))
  )
}
