mlnbinom_ <- function(x, ...) {
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

      f_over_df <- function(size) {
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
