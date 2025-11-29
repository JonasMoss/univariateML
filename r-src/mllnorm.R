mllnorm <- function(x, na.rm = FALSE, ...) {}


mllnorm_ <- function(x, ...) {
  y <- log(x)
  n <- length(x)
  meanlog <- mean(y)
  sdlog <- sqrt(stats::var(y) * (n - 1) / n)
  estimates <- c(meanlog = meanlog, sdlog = sdlog)
  logLik <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sdlog) + 2 * meanlog)
  list(estimates = estimates, logLik = logLik)
}
