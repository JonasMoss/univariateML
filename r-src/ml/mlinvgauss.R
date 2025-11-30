mlinvgauss_ <- function(x, ...) {
  n <- length(x)
  mu <- mean(x)
  lambda <- 1 / (mean(1 / x) - 1 / mu)
  estimates <- c(mu, lambda)

  L <- mean(log(x))
  S <- mean((x - mean(x))^2 / x)

  logLik <- -n / 2 * (3 * L - log(lambda) + log(2 * pi) + lambda / mu^2 * S)
  list(estimates = estimates, logLik = logLik)
}
