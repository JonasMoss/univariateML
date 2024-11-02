#' Negative binomial distribution maximum likelihood estimation
#'
#' For the density function of the Negative binomial distribution see
#' [Negative binomial][stats::dnbinom].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... The arguments `size` can be specified to only return the ml of `prob`.
#'    `reltol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#' @return `mlnbinom` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `size` and `prob` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlnbinom(corbet)
#' @seealso [Negative binomial][stats::dnbinom] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlnbinom <- \(x, na.rm = FALSE, ...) {}

metadata$mlnbinom <- list(
  "model" = "Negative binomial",
  "density" = "stats::dnbinom",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("size", "prob"),
  "default" = c(10, 0.3)
)

mlnbinom_ <- \(x, ...) {
  dots <- list(...)
  reltol <- get_reltol(dots)
  iterlim <- get_iterlim(dots)

  n <- length(x)
  x_bar <- mean(x)
  get_prob <- \(size) size / (x_bar + size)

  if (is.null(dots$size)) {
    if (mean(x) == 0) {
      warning("All observations are 0; the maximum likelihood estimator is not unique.")
      size <- 1
    } else {
      dget_prob <- \(size) x_bar / (size^2 + x_bar * size)
      f <- \(size) sum(digamma(x + size)) - n * digamma(size) + n * log(get_prob(size))
      df <- \(size) sum(trigamma(x + size)) - n * trigamma(size) + n * dget_prob(size)

      # Start at method of moments estimate.
      x_var <- stats::var(x)
      if (x_var * (n - 1) / n < x_bar) {
        stop("The maximum likelihood estimator does not exists for underdispersed data, but converges to a Poisson. Use `mlpois` instead.")
      }
      size0 <- x_bar^2 / (x_var - x_bar)

      for (i in seq(iterlim)) {
        size <- size0 - f(size0) / df(size0)
        if (abs((size0 - size) / size0) < reltol) break

        size0 <- size
      }
    }
  } else {
    size <- dots$size
  }

  prob <- get_prob(size)
  logLik <- sum(lgamma(x + size)) - sum(lfactorial(x)) - n * lgamma(size) +
    n * size * log(prob) + n * log(1 - prob) * x_bar

  list(estimates = c(size, prob), logLik = logLik)
}
