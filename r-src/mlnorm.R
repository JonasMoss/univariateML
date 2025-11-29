mlnorm <- function(x, na.rm = FALSE, ...) {}


mlnorm_ <- function(x, ...) {
  n <- length(x)

  mu <- mean(x)
  sigma <- sqrt(stats::var(x) * (n - 1) / n)

  estimates <- c(mu, sigma)
  logLik <- -n / 2 * (1 + log(2 * pi) + 2 * log(sigma))
  list(estimates = estimates, logLik = logLik)
}
