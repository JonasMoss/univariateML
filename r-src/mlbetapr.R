mlbetapr <- function(x, na.rm = FALSE, ...) {}


mlbetapr_ <- function(x, ...) {
  estimates <- mlbeta(x / (x + 1), na.rm = FALSE, ...)
  alpha <- estimates[1]
  beta <- estimates[2]
  logLik <- unname(-length(x) * (lbeta(alpha, beta) -
    (alpha - 1) * mean(log(x)) +
    (alpha + beta) * mean(log(1 + x))))
  list(estimates = estimates, logLik = logLik)
}
