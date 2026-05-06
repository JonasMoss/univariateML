mlged_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::gedFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
