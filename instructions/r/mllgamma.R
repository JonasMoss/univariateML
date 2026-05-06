mllgamma_ <- function(x, ...) {
  y <- log(x)
  n <- length(x)
  estimates <- mlgamma_(y, ...)$estimates
  shapelog <- estimates[1]
  ratelog <- estimates[2]
  L <- mean(y)
  K <- mean(log(y))
  logLik <-
    unname(n * (shapelog * log(ratelog) - log(gamma(shapelog)) +
      (shapelog - 1) * K - (ratelog + 1) * L))

  list(estimates = estimates, logLik = logLik)
}
