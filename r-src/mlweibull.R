mlweibull <- function(x, na.rm = FALSE, ...) {}


mlweibull_ <- function(x, ...) {
  log_x <- -log(x)
  s <- -sum(log_x) / length(x)
  estimates <- mlgumbel_estimate(log_x + s, ...)
  scale <- exp(-estimates[1] + s)[1]
  shape <- 1 / estimates[2]
  logLik <- length(x) * (log(shape / scale) + (shape - 1) * (s - log(scale)) - 1)
  list(estimates = c(shape, scale), logLik = logLik)
}
