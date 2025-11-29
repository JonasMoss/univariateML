mlsstd <- function(x, na.rm = FALSE, ...) {}


mlsstd_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::sstdFit(x))
  list(estimates = fit[["estimate"]], logLik = -fit$minimum)
}
