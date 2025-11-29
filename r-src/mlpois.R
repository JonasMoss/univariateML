mlpois <- function(x, na.rm = FALSE, ...) {}


mlpois_ <- function(x, ...) {
  n <- length(x)
  lambda <- mean(x)
  logLik <- -n * lambda + sum(x) * log(lambda) - sum(lgamma(x + 1))
  list(estimates = lambda, logLik = logLik)
}
