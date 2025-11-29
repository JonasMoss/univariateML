mlrayleigh_ <- function(x, ...) {
  sigma <- sqrt(1 / 2 * mean(x^2))
  logLik <- length(x) * (mean(log(x) - 2 * log(sigma) - 1))
  list(estimates = sigma, logLik = logLik)
}
