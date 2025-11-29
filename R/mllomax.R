#' Lomax distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Lomax distribution.
#'
#' For the density function of the Lomax distribution see
#' [Lomax][extraDistr::Lomax].
#'
#' The likelihood estimator of the Lomax distribution is unbounded when `mean(x^2) < 2*mean(x)^2`. When this
#'    happens, the likelihood converges to an exponential distribution with parameter
#'    equal to the mean of the data. This is the natural limiting case for the Lomax
#'    distribution, and it is reasonable to use `mlexp` in this case.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `lambda0` an optional starting value for the `lambda` parameter.
#'    `reltol` is the relative accuracy requested,
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
mllomax <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mllomax <- list(
  "model" = "Lomax",
  "density" = "extraDistr::dlomax",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("lambda", "kappa"),
  "default" = c(1, 2)
)

mllomax_ <- function(x, ...) {
  n <- length(x)
  m <- sum(x) / n
  s <- sum(x^2) / n
  m2 <- m^2

  if (s < 2 * m2) {
    stop("The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution.")
  }
  f_over_df <- function(lambda0) {
    lambda0_x1p <- 1 + lambda0 * x
    x_div <- x / lambda0_x1p
    S <- sum(log(lambda0_x1p)) / n
    S1 <- sum(x_div) / n
    S2 <- sum(x_div^2) / n

    psi <- S1 / S
    f <- 1 / lambda0 - psi - S1
    df <- psi^2 + S2 / S + S2 - 1 / lambda0^2
    f / df
  }

  alpha0 <- 2 * (s - m2) / (s - 2 * m2)
  lambda0 <- 1 / (m * (alpha0 - 1))

  lambda <- newton_raphson_1d(f_over_df, lambda0)
  kappa <- n / sum(log1p(lambda * x))
  loglik <- n * (log(kappa) + log(lambda) - 1 - 1/kappa)

  list(estimates = c(lambda, kappa), logLik = loglik)
}
