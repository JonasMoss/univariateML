mlexp_ <- function(x, ...) {
  estimates <- 1 / mean(x)
  logLik <- length(x) * (log(estimates) - 1)
  list(estimates = estimates, logLik = logLik)
}
