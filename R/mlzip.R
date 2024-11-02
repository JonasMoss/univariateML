#' Zero-inflated Poisson distribution maximum likelihood estimation
#'
#' For the density function of the zeero-inflated Poisson distribution see
#' [Zero-inflated Poisson distribution][extraDistr::dzip].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Not currently in use.
#' @return `mlzip` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `lambda` and `pi` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlzip(corbet)
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @seealso [Zero-inflated Poisson distribution][extraDistr::dzip] for the density.
#' @export
mlzip <- \(x, na.rm = FALSE, ...) {}

metadata$mlzip <- list(
  "model" = "Zero-inflated Poisson",
  "density" = "extraDistr::dzip",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("lambda", "pi"),
  "default" = c(1, 0.5)
)

mlzip_ <- \(x, ...) {
  r_0 <- mean(x == 0)
  x_bar <- mean(x)

  if (r_0 == 1) {
    pi <- 1
    lambda <- 1
    warning("All observations equal to 0; the maximum likelihood estimator is not unique.")
  } else if (r_0 > exp(-x_bar)) {
    x_bar <- mean(x)
    gamma <- x_bar / (1 - r_0)
    lambda <- pracma::lambertWp(-gamma * exp(-gamma)) + gamma
    pi <- (r_0 - exp(-lambda)) / (1 - exp(-lambda))
  } else {
    pi <- 0
    lambda <- x_bar
  }

  logLik <- sum(extraDistr::dzip(x, lambda, pi, log = TRUE))

  list(estimates = c(lambda, pi), logLik = logLik)
}
