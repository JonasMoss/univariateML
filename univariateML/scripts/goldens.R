#!/usr/bin/env Rscript
# Generate golden values for univariateML estimators.
#
# For every dossier this script samples from the configured sampler
# with default parameters and a deterministic seed, fits the
# corresponding ml<id>() function, and writes the parameter estimates,
# log-likelihood, and observation count to distributions/<id>/golden.json.
#
# These goldens are the regression net for Workstreams 1-5 of the C++ port:
# any change that does NOT also change the algorithm must reproduce them
# bit-for-bit (or to within ~1e-12 floating-point noise).

suppressPackageStartupMessages({
  library(jsonlite)
  pkgload::load_all(Sys.getenv("UMVL_PKG_DIR", "package"), quiet = TRUE)
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
  draws <- do.call(resolve_callable(model$sampler), c(list(n), default_args(model)))
  if (!is.null(model$shift)) {
    shift <- Filter(function(p) identical(p$name, model$shift), model$params)[[1]]$simulate
    draws <- draws + shift
  }
  draws
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
    "../cppunivariate/include/cppunivariate/distributions"
  )

  source(file.path("scripts", "load_distributions.R"))
  models <- load_distributions(dossier_dir)
  # UMVL_GOLDEN_IDS="hnorm lst" regenerates only those goldens.
  only <- strsplit(Sys.getenv("UMVL_GOLDEN_IDS"), "[ ,]+")[[1]]
  if (length(only)) models <- models[names(models) %in% only]

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
      file.path(dossier_dir, model$id, "golden.json"),
      pretty = TRUE,
      auto_unbox = TRUE,
      digits = 17
    )
    results[[model$id]] <- record
    message(sprintf("[ok]   %-10s logLik=%g  n=%d", model$id, record$logLik, record$n))
  }

  message(sprintf("\nWrote %d goldens to %s/<id>/golden.json", length(results), dossier_dir))
}

main()
