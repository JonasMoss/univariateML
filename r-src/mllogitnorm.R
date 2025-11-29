mllogitnorm_ <- function(x, ...) {
  n <- length(x)
  y <- stats::qlogis(x)
  mu <- mean(y)
  sigma <- sqrt(stats::var(y) * (n - 1) / n)

  H <- mean(log(x))
  G <- mean(log(1 - x))

  estimates <- c(mu = mu, sigma = sigma)
  logLik <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sigma) + 2 * H + 2 * G)
  list(estimates = estimates, logLik = logLik)
}
