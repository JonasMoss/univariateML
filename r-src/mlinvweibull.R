mlinvweibull <- function(x, na.rm = FALSE, ...) {}


mlinvweibull_ <- function(x, ...) {

  log_x <- log(x)
  n <- length(x)
  s <- -sum(log_x) / n

  estimates <- mlgumbel_estimate(log_x + s, ...)
  rate_log <- -estimates[1] + s

  rate <- exp(rate_log)
  shape <- 1 / estimates[2]

  #r <- sum(exp(-log_x * shape)) / n
  #scale <- 1/rate
  #r <- (scale^shape)^2
  #r <- scale^shape * r
  # r <- 1

  logLik <- n * (log(shape) + shape * (-rate_log + s) - 1 + s)
  list(estimates = c(shape, rate), logLik = logLik)
}
