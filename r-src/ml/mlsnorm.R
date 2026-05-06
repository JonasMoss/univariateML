mlsnorm_ <- function(x, ...) {
  fit <- suppressWarnings(snormFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}
