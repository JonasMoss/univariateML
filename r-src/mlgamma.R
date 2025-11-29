mlgamma <- function(x, na.rm = FALSE, ...) {}


mlgamma_ <- function(x, ...) {
  n <- length(x)
  mean_hat <- sum(x) / n
  lx_bar <- sum(log(x)) / n
  s <- log(mean_hat) - lx_bar

  f_over_df <- function(shape0) {
    (log(shape0) - digamma(shape0) - s) / (1 / shape0 - trigamma(shape0))
  }

  shape0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s)) * 0.9989 + 0.0102
  shape <- newton_raphson_1d(f_over_df, shape0, ...)
  rate <- shape / mean_hat

  estimates <- c(shape = shape, rate = rate)
  logLik <- n * (shape * log(rate) - lgamma(shape) + (shape - 1) * lx_bar - rate * mean_hat)

  list(estimates = estimates, logLik = logLik)
}
