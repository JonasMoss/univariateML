mlgumbel_estimate_2 <- function(x, ...) {
  x2 <- x^2
  f_over_df <- function(sigma0) {
    neg_sigma_inv <- -1 / sigma0
    exps <- exp(x * neg_sigma_inv)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x2 * exps) / psi0 - psi1^2
    f <- sigma0 + psi1
    df <- 1 + neg_sigma_inv^2 * psi2
    f / df
  }

  sigma0 <- sqrt(sum(x^2) / length(x)) * 2.45 / pi * (1 - 1 / length(x))
  sigma <- newton_raphson_1d(f_over_df, sigma0, ...)
  neg_sigma_inv <- -1 / sigma
  psi0 <- sum(exp(x * neg_sigma_inv))
  mu <- -sigma * log(psi0 / length(x))
  c(mu, sigma)
}

mlinvweibull_2 <- function(x, ...) {
  log_x <- log(x)
  n <- length(x)
  s <- -sum(log_x) / n
  estimates <- mlgumbel_estimate_2(log_x + s, ...)
  rate_log <- -estimates[1] + s
  rate <- exp(rate_log)
  shape <- 1 / estimates[2]
  logLik <- n * (log(shape) + shape * (-rate_log + s) - 1 + s)
  list(estimates = c(shape, rate), logLik = logLik)
}


x <- actuar::rinvweibull(1000, 2, 3)
mlinvweibull_2(x)