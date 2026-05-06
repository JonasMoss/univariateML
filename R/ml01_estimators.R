# This file is auto-generated from r-src via scripts/gen_estimators_from_rsrc.R
# Do not edit by hand.

mlsged_ <- function(x, ...) {
  fit <- suppressWarnings(sgedFit(x, ...))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

