mlfatigue_ <- function(x, ...) {
  n <- length(x)
  x_bar <- sum(x) / n
  x_inv_bar <- n / sum(1 / x)

  f_over_df <- function(beta) {
    xi <- n / sum(1 / (x + beta))
    dxi <- sum(1 / (x + beta)^2) * xi^2 / n
    f <- beta^2 - beta * (2 * x_inv_bar + xi) + x_inv_bar * (x_bar + xi)
    df <- 2 * beta - (2 * x_inv_bar + xi) - beta * dxi + x_inv_bar * dxi
    f / df
  }

  # Starting parameter: Method of moments solution.
  sqrt_x <- sqrt(x)
  beta0 <- sum(sqrt_x) / sum(1 / sqrt_x)
  beta <- newton_raphson_1d(f_over_df, beta0, ...)

  alpha <- sqrt(x_bar / beta + beta / x_inv_bar)

  estimates <- c(alpha = alpha, beta = beta, mu = 0)
  logLik <- sum(extraDistr::dfatigue(x, alpha, beta, 0, log = TRUE))
  list(estimates = estimates, logLik = logLik)
}
