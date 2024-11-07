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

univariateML_metadata$mlnbinom <- list(
  "model" = "Negative binomial",
  "density" = "stats::dnbinom",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("size", "prob"),
  "default" = c(10, 0.3)
)

mlnbinom_ <- \(x, ...) {
  dots <- list(...)
  n <- length(x)
  x_bar <- mean(x)

  if (is.null(dots$size)) {
    if (x_bar == 0) {
      warning("All observations are 0; the maximum likelihood estimator is not unique.")
      size <- 1
    } else {
      x_var <- stats::var(x)

      if (x_var * (n - 1) / n < x_bar) {
        stop("The maximum likelihood estimator does not exists for underdispersed data, but converges to a Poisson. Use `mlpois` instead.")
      }

      f_over_df <- \(size) {
        prob <- size / (x_bar + size)
        dprob <- x_bar / (size^2 + x_bar * size)
        f <- sum(digamma(x + size)) - n * digamma(size) + n * log(prob)
        df <- sum(trigamma(x + size)) - n * trigamma(size) + n * dprob
        f / df
      }

      size0 <- x_bar^2 / (x_var - x_bar)
      size <- newton_raphson_1d(f_over_df, size0, ...)
    }
  } else {
    size <- dots$size
  }

  prob <- size / (x_bar + size)
  logLik <- sum(lgamma(x + size)) - sum(lfactorial(x)) - n * lgamma(size) +
    n * size * log(prob) + n * log(1 - prob) * x_bar

  list(estimates = c(size, prob), logLik = logLik)
}
