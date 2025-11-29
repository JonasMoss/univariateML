#' Kumaraswamy distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Kumaraswamy
#'     distribution.
#'
#' For the density function of the Kumaraswamy distribution see
#'     [Kumaraswamy][extraDistr::Kumaraswamy].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `a0` is an optional starting value for the `a` parameter.
#'     `reltol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#'
#' @return `mlkumar` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for `a`
#'    and `b` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Kumaraswamy][extraDistr::Kumaraswamy] for the Kumaraswamy density.
#' @examples
#' AIC(mlkumar(USArrests$Rape / 100))
#' @references Jones, M. C. "Kumaraswamy's distribution: A beta-type
#' distribution with some tractability advantages." Statistical Methodology
#' 6.1 (2009): 70-81.
#'
#' Kumaraswamy, Ponnambalam. "A generalized probability density function
#' for double-bounded random processes." Journal of Hydrology 46.1-2 (1980):
#' 79-88.
#' @export
mlkumar <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlkumar <- list(
  "model" = "Kumaraswamy",
  "density" = "extraDistr::dkumar",
  "support" = intervals::Intervals(c(0, 1), closed = c(FALSE, FALSE)),
  "names" = c("a", "b"),
  "default" = c(2, 3)
)

kumar_lmom <- function(mu, sigma2) {
  neuler <- digamma(1) # Negative Euler-Mascheroni constant
  basel <- pi^2 / 6 # trigamma(1)

  f_over_df <- function(a) {
    b <- inverse_digamma(neuler - a * mu)
    tri <- trigamma(b)
    tetra <- psigamma(b, deriv = 2)

    f <- a * tri * (basel - tri - a^2 * sigma2)
    df <- a * tetra * mu - 2 * tri * (basel + tri)
    f / df
  }

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100
  a0 <-  sqrt(basel / sigma2)
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0) * reltol) break
    a0 <- a
  }

  a <- a0
  # b is inverse_digamma(neuler - a * mu) - 1
  a
}

mlkumar_ <- function(x, ...) {
  n <- length(x)
  log_x <- log(x)
  n_div <- 1 / n
  mu <- sum(log_x) * n_div
  sigma2 <- sum(log_x^2) * n_div - mu^2

  f_over_df <- function(a) {
    xa <- exp(a * log_x)
    z <- log_x / (1 - xa)

    alpha <- sum(log1p(-xa)) * n_div # Corresponds to s1a
    beta <- sum(xa * z) * n_div
    gamma <- sum(xa * z^2) * n_div

    f <- 1/a + mu + (1/alpha + 1) * beta
    df <- -1/(a^2) + (gamma * alpha + beta^2) / (alpha^2) + gamma

    f / df
  }

  a0 <- kumar_lmom(mu, sigma2)
  a0 <- if (a0 > 0) a0 else sqrt(pi^2 / (6 * sigma2))

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0) * reltol) break
    a0 <- a
  }

  a <- a0
  b <- -n / sum(log1p(-x^a))

  list(
    estimates = c(a = a, b = b),
    logLik = n * (log(a) + log(b) + (a - 1) * mu + 1 / b - 1),
    iterations = i
  )
}
