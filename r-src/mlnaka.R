mlnaka_ <- function(x, ...) {
  estimates <- mlgamma_(x^2, ...)$estimates
  estimates["rate"] <- 1 / estimates["rate"] * estimates["shape"]

  n <- length(x)
  shape <- estimates[1]
  scale <- estimates[2]

  logLik <-
    unname(n * (shape * log(shape) + log(2) -
      lgamma(shape) - shape * log(scale)) +
      (2 * shape - 1) * sum(log(x)) - shape / scale * sum(x^2))

  list(estimates = estimates, logLik = logLik)
}
