mlsged_ <- function(x, ...) {
  fit <- suppressWarnings(sgedFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
