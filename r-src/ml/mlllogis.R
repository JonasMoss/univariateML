mlllogis_ <- function(x, ...) {
  y <- log(x)
  estimates <- mllogis_(y)$estimates
  estimates[1] <- exp(-estimates[1])
  estimates[2] <- 1 / estimates[2]
  estimates <- rev(estimates)
  logLik <- sum(actuar::dllogis(x, estimates[1], estimates[2], log = TRUE))
  list(estimates = estimates, logLik = logLik)
}
