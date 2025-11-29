mlsnorm <- function(x, na.rm = FALSE, ...) {}


mlsnorm_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::snormFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
