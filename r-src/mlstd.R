mlstd <- function(x, na.rm = FALSE, ...) {}


mlstd_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::stdFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
