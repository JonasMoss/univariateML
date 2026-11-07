#!/usr/bin/env Rscript
# Create golden samples for univariateML families that have none.
#
# A golden sample is a frozen fixture (decided 2026-11-07): existing
# golden.json files are never resampled or refitted here, because their
# data are shared by the C++, R and JS tests. For a new family this script
# samples from the configured sampler with the README `simulate` values and a
# deterministic seed, fits ml<id>(), and writes data, provenance and the fitted
# values. The stored estimates are then replaced by an independent
# high-precision reference: `just golden-reference <id>`.
#
# UMVL_GOLDEN_IDS="hnorm lst" restricts the run to those families.
# UMVL_GOLDEN_RESAMPLE=1 deliberately replaces the samples of the selected
# families (requires UMVL_GOLDEN_IDS); rerun the reference afterwards.

suppressPackageStartupMessages({
  library(jsonlite)
  pkgload::load_all(Sys.getenv("UMVL_PKG_DIR", "../../univariateML-r"), quiet = TRUE)
})

GOLDEN_N <- 500L

`%||%` <- function(a, b) if (is.null(a)) b else a

resolve_callable <- function(spec) {
  parts <- strsplit(spec, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L) {
    getExportedValue(parts[1], parts[2])
  } else {
    get(parts[1], envir = asNamespace("univariateML"), inherits = FALSE)
  }
}

seed_for <- function(id) sum(utf8ToInt(id)) + 12345L

set_seed_for <- function(id) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed_for(id))
}

# Default parameters as sampler arguments: by coefficient name, or through
# the README's r.args map (sampler argument = coefficient).
default_args <- function(model) {
  values <- setNames(
    lapply(model$params, function(p) p$simulate),
    vapply(model$params, function(p) p$name, character(1))
  )
  if (is.null(model$args)) {
    if (!is.null(model$shift)) values[[model$shift]] <- NULL
    return(values)
  }
  setNames(lapply(unname(model$args), function(expr) eval(parse(text = expr), values)), names(model$args))
}

# Sample size by position: stats::rhyper calls it `nn`.
sample_one <- function(model, n) {
  rml(n, getExportedValue("univariateML", paste0("ml", model$id))())
}

# README fit_args (required design quantities) are passed to the fit.
fit_one <- function(model, data) {
  fn <- getExportedValue("univariateML", paste0("ml", model$id))
  do.call(fn, c(list(data), model$fit_args))
}

fit_to_record <- function(model, fit, data) {
  estimates <- as.numeric(fit)
  param_names <- vapply(model$params, function(p) p$name, character(1))
  if (length(estimates) == length(param_names)) {
    names(estimates) <- param_names
  }
  record <- list(
    id = model$id,
    name = model$name,
    n = length(data),
    seed = seed_for(model$id),
    sampler = model$sampler,
    sampler_args = default_args(model),
    estimates = as.list(estimates),
    fixed = I(as.character(attr(fit, "fixed"))),
    logLik = unname(attr(fit, "logLik")),
    n_obs = unname(attr(fit, "n")) %||% length(data),
    data = as.numeric(data)
  )
  if (length(model$fit_args)) record$fit_args <- model$fit_args
  record
}

main <- function() {
  dossier_dir <- Sys.getenv(
    "UMVL_DOSSIER_DIR",
    "../../univariateML-cpp/include/cppunivariate/distributions"
  )

  source(file.path("scripts", "load_distributions.R"))
  models <- load_distributions(dossier_dir)
  # UMVL_GOLDEN_IDS="hnorm lst" regenerates only those goldens.
  only <- strsplit(Sys.getenv("UMVL_GOLDEN_IDS"), "[ ,]+")[[1]]
  if (length(only)) models <- models[names(models) %in% only]

  resample <- identical(Sys.getenv("UMVL_GOLDEN_RESAMPLE"), "1")
  if (resample && !length(only)) stop("UMVL_GOLDEN_RESAMPLE=1 needs UMVL_GOLDEN_IDS")

  results <- list()
  for (model in models) {
    if (!resample && file.exists(file.path(dossier_dir, model$id, "golden.json"))) next
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
      file.path(dossier_dir, model$id, "golden.json"),
      pretty = TRUE,
      auto_unbox = TRUE,
      digits = 17
    )
    results[[model$id]] <- record
    message(sprintf("[ok]   %-10s logLik=%g  n=%d", model$id, record$logLik, record$n))
  }

  message(sprintf("\nWrote %d goldens to %s/<id>/golden.json; run just golden-reference on them", length(results), dossier_dir))
}

main()
