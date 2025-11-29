mlpareto <- function(x, na.rm = FALSE, ...) {}


mlpareto_ <- function(x, ...) {
  m <- mean(log(x))
  b <- min(x)
  a <- 1 / (m - log(b))

  estimates <- c(a, b)
  logLik <- length(x) * (log(a) + a * log(b) - (a + 1) * m)
  list(estimates = estimates, logLik = logLik)
}
