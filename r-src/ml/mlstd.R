mlstd_ <- function(x, ...) {
  fit <- suppressWarnings(stdFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
