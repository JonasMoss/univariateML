mlgeom <- function(x, na.rm = FALSE, ...) {}


mlgeom_ <- function(x, ...) {
  n <- length(x)
  prob <- 1 / (mean(x) + 1)
  logLik <- sum(x * log(1 - prob)) + n * log(prob)
  list(estimates = prob, logLik = logLik)
}
