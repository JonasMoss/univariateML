mldunif_ <- function(x, ...) {
  n <- length(x)
  estimates <- c(min(x), max(x))
  list(
    estimates = estimates,
    logLik = -n * log(estimates[2] - estimates[1] + 1)
  )
}
