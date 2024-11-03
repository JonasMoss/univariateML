#' Lomax distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Lomax distribution.
#'
#' For the density function of the Lomax distribution see
#' [Lomax][extraDistr::Lomax].
#'
#' The likelihood estimator of the Lomax distribution may be unbounded. When this
#'    happens, the likelihood converges to an exponential distribution with parameter
#'    equal to the mean of the data. This is the natural limiting case for the Lomax
#'    distribution, and it is reasonable to use `mlexp` in this case. See
#'    `vignette("Distribution Details", package = "univariateML")` for details.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `lambda0` an optional starting value for the `lambda` parameter.
#'    Defaults to `median(x)`. `reltol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#'
#' @return `mllomax` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `lambda` and `kappa` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Lomax][extraDistr::Lomax] for the Lomax density.
#' @references Kleiber, Christian; Kotz, Samuel (2003), Statistical Size
#' Distributions in Economics and Actuarial Sciences, Wiley Series in
#' Probability and Statistics, 470, John Wiley & Sons, p. 60
#' @examples
#' set.seed(3)
#' mllomax(extraDistr::rlomax(100, 2, 4))
#'
#' # The maximum likelihood estimator may fail if the data is exponential.
#' \dontrun{
#' set.seed(5)
#' mllomax(rexp(10))
#' }
#' @export
mllomax <- \(x, na.rm = FALSE, ...) {}

metadata$mllomax <- list(
  "model" = "Lomax",
  "density" = "extraDistr::dlomax",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("lambda", "kappa"),
  "default" = c(1, 2)
)

mllomax_ <- \(x, ...) {
  f_over_df <- \(lambda0) {
    S <- mean(log(1 + lambda0 * x))
    S1 <- mean(x / (1 + lambda0 * x))
    S2 <- -mean(x^2 / (1 + lambda0 * x)^2)
    f <- 1 / lambda0 - S1 / S - S1
    df <- -1 / lambda0^2 - S2 / S + (S1 / S)^2 - S2
    f / df
  }

  dots <- list(...)
  if (is.null(dots$lambda0)) {
    s <- mean(x^2)
    m <- mean(x)
    alpha <- (s - m^2) / (0.5 * s - m^2)
    lambda0 <- 1 / (m * (max(alpha, 1.1) - 1))
  } else {
    lambda0 <- dots$lambda0
  }

  lambda <- newton_raphson_1d(f_over_df, lambda0, ...)

  s <- \(lambda) mean(log(1 + lambda * x))

  loglik <- \(lambda) sum(extraDistr::dlomax(x, lambda, 1 / s(lambda), log = TRUE))
  logLik <- loglik(lambda)

  if (loglik(0.000001) > logLik) {
    stop("The maximum likelihood estimator does not exist. Use `mlexp` to fit an exponential distribution.")
  }

  list(estimates = c(lambda, 1 / s(lambda)), logLik = logLik)
}
