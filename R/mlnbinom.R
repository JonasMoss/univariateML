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
#'    `mean` and `sd` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpois(precip)
#' @seealso [Negative binomial][stats::dnbinom] for the density.
#' @export
mlnbinom <- \(x, na.rm = FALSE, ...) {}

metadata$mlnbinom <- list(
  "model" = "Negative binomial",
  "density" = "stats::dnbinom",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("size", "prob")
)

mlnbinom_ <- \(x, ...) {
  dots <- list(...)
  reltol <- get_reltol(dots)
  iterlim <- get_iterlim(dots)

  n <- length(x)
  x_bar <- mean(x)
  get_prob <- \(size) size / (x_bar + size)

  x_bar2 <- mean(x^2)
  p = 1 - (x_bar/(x_bar2-x_bar^2))
  size = (1-p)*x_bar/p

  if (is.null(dots$size)) {
    if(mean(x) == 0) {
      warning("All observations are 0; the maximum likelihood estimator is not unique.")
      size <- 1
    } else {
      dget_prob <- \(size) x_bar / (size^2 + x_bar * size)
      f <- \(size) sum(digamma(x + size)) - n * digamma(size) + n * log(get_prob(size))
      df <- \(size) sum(trigamma(x + size)) - n * trigamma(size) + n * dget_prob(size)

      # Start at method of moments estimates.
      x_bar2 <- mean(x^2)
      p = 1 - (x_bar/(x_bar2-x_bar^2))
      size0 = (1-p)*x_bar/p

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
