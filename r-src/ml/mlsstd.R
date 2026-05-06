mlsstd_ <- function(x, ...) {
  fit <- suppressWarnings(sstdFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
