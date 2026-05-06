#!/usr/bin/env Rscript
# Generate golden values for univariateML estimators.
#
# For every model in metadata/models.json this script samples from the
# configured sampler with default parameters and a deterministic seed,
# fits the corresponding ml<id>() function, and writes the parameter
# estimates, log-likelihood, and observation count to tests/golden/<id>.json.
#
# These goldens are the regression net for Workstreams 1-5 of the C++ port:
# any change that does NOT also change the algorithm must reproduce them
# bit-for-bit (or to within ~1e-12 floating-point noise).

suppressPackageStartupMessages({
  library(jsonlite)
  devtools::load_all(".", quiet = TRUE)
})

GOLDEN_N <- 500L

`%||%` <- function(a, b) if (is.null(a)) b else a

resolve_callable <- function(spec) {
  parts <- strsplit(spec, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L) {
    getExportedValue(parts[1], parts[2])
  } else {
    get(parts[1], mode = "function")
  }
}

seed_for <- function(id) sum(utf8ToInt(id)) + 12345L

set_seed_for <- function(id) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed_for(id))
}

default_args <- function(model) {
  setNames(
    lapply(model$params, function(p) p$default),
    vapply(model$params, function(p) p$name, character(1))
  )
}

sample_one <- function(model, n) {
  do.call(resolve_callable(model$sampler), c(list(n = n), default_args(model)))
}

fit_one <- function(model, data) {
  fn <- getExportedValue("univariateML", paste0("ml", model$id))
  fn(data)
}

fit_to_record <- function(model, fit, data) {
  estimates <- as.numeric(fit)
  param_names <- vapply(model$params, function(p) p$name, character(1))
  if (length(estimates) == length(param_names)) {
    names(estimates) <- param_names
  }
  list(
    id = model$id,
    name = model$name,
    n = length(data),
    seed = seed_for(model$id),
    sampler = model$sampler,
    sampler_args = default_args(model),
    estimates = as.list(estimates),
    logLik = unname(attr(fit, "logLik")),
    n_obs = unname(attr(fit, "n")) %||% length(data)
  )
}

main <- function() {
  json_path <- file.path("metadata", "models.json")
  out_dir <- file.path("tests", "golden")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  models <- jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE)$models

  results <- list()
  for (model in models) {
    set_seed_for(model$id)

    data <- tryCatch(sample_one(model, GOLDEN_N), error = function(e) e)
    if (inherits(data, "error")) {
      message(sprintf("[skip] %-10s sample: %s", model$id, conditionMessage(data)))
      next
    }

    fit <- tryCatch(fit_one(model, data), error = function(e) e)
    if (inherits(fit, "error")) {
      message(sprintf("[skip] %-10s fit:    %s", model$id, conditionMessage(fit)))
      next
    }

    record <- fit_to_record(model, fit, data)
    jsonlite::write_json(
      record,
      file.path(out_dir, paste0(model$id, ".json")),
      pretty = TRUE,
      auto_unbox = TRUE,
      digits = 17
    )
    results[[model$id]] <- record
    message(sprintf("[ok]   %-10s logLik=%g  n=%d", model$id, record$logLik, record$n))
  }

  message(sprintf("\nWrote %d goldens to %s", length(results), out_dir))
}

main()
