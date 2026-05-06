mlged_ <- function(x, ...) {
  fit <- suppressWarnings(gedFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
