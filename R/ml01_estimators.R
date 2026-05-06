# This file is auto-generated from r-src via scripts/gen_estimators_from_rsrc.R
# Do not edit by hand.

mlged_ <- function(x, ...) {
  fit <- suppressWarnings(gedFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

mlsged_ <- function(x, ...) {
  fit <- suppressWarnings(sgedFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

mlsnorm_ <- function(x, ...) {
  fit <- suppressWarnings(snormFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

mlsstd_ <- function(x, ...) {
  fit <- suppressWarnings(sstdFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

