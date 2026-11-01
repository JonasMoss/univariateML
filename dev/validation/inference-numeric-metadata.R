# Metadata shape tests; no inference eligibility claim is established here.
source("bindings/r/scripts/load_distributions.R")
check <- .umvl_validate_numeric_inference
ok <- list(regime="interior", available=TRUE, condition="positive scale", classification="report.md#classification")
open <- list(regime="boundary", available=FALSE, condition="scale zero", reason="classification open", classification="report.md#classification")
check(NULL, "test")
check(list(ok, open), "test")
reject <- function(x) inherits(tryCatch({check(x, "test"); NULL}, error=identity), "error")
stopifnot(reject(list(within(open, rm(reason)))),
          reject(list(within(ok, available <- "yes"))),
          reject(list(ok, ok)),
          reject(list(within(ok, condition <- ""))),
          reject(list(within(ok, classification <- "missing-anchor"))),
          reject(list(within(ok, surprise <- 1))))
cat("numeric inference metadata validation passed\n")
