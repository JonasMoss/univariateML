mlsged_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::sgedFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
