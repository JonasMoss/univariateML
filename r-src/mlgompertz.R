mlgompertz_ <- function(x, ...) {
  n <- length(x)
  x_sum <- sum(x)
  x_var <- stats::var(x) * (n - 1) / n
  if (x_sum / n < sqrt(x_var)) {
    stop("The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed. The exponential distribution will yield a better fit.")
  }

  f_over_df <- function(b) {
    exp_bx <- exp(b * x)
    sum_bx <- sum(exp_bx)
    sum_bx_x <- sum(x * exp_bx)
    sum_bx_x2 <- sum(x^2 * exp_bx)
    s <- sum_bx - n
    r <- b * sum_bx_x - s

    a <- b * n / s
    da <- -n * r / s^2
    d2a <- n / s^2 * (b * (2 * sum_bx_x^2 / s - sum_bx_x2) - 2 * sum_bx_x)

    f <- n * da / a + x_sum
    df <- n * (a * d2a - da^2) / a^2
    f / df
  }

  b <- newton_raphson_1d(f_over_df, 10e-2, ...)

  a <- b * n / (sum(exp(b * x)) - n)
  a <- max(a, 1e-09)

  list(
    estimates = c(a, b),
    logLik = sum(extraDistr::dgompertz(x, a, b, log = TRUE))
  )
}
