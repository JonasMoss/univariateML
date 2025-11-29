mllomax <- function(x, na.rm = FALSE, ...) {}


mllomax_ <- function(x, ...) {
  n <- length(x)
  m <- sum(x) / n
  s <- sum(x^2) / n
  m2 <- m^2

  if (s < 2 * m2) {
    stop("The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution.")
  }
  f_over_df <- function(lambda0) {
    lambda0_x1p <- 1 + lambda0 * x
    x_div <- x / lambda0_x1p
    S <- sum(log(lambda0_x1p)) / n
    S1 <- sum(x_div) / n
    S2 <- sum(x_div^2) / n

    psi <- S1 / S
    f <- 1 / lambda0 - psi - S1
    df <- psi^2 + S2 / S + S2 - 1 / lambda0^2
    f / df
  }

  alpha0 <- 2 * (s - m2) / (s - 2 * m2)
  lambda0 <- 1 / (m * (alpha0 - 1))

  lambda <- newton_raphson_1d(f_over_df, lambda0, ...)
  kappa <- n / sum(log1p(lambda * x))
  loglik <- n * (log(kappa) + log(lambda) - 1 - 1/kappa)

  list(estimates = c(lambda, kappa), logLik = loglik)
}
