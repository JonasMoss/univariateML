#!/usr/bin/env Rscript

# Generic benchmarking harness for Rcpp estimator variants.
# Usage:
#   Rscript scripts/bench_cpp.R --model burr --sizes 50,200,1000 --reps 5

suppressMessages({
  library(bench)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(jsonlite)
  library(univariateML)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) return(default)
  if (idx == length(args)) stop(sprintf("Missing value for %s", flag))
  args[idx + 1]
}

model_id <- get_arg("--model", "burr")
sizes    <- as.numeric(strsplit(get_arg("--sizes", "50,200,1000")[[1]], ",")[[1]])
reps     <- as.integer(get_arg("--reps", "5"))
out_path <- get_arg("--out", NA_character_)

resolve_fun <- function(qualified) {
  if (is.null(qualified) || is.na(qualified) || qualified == "") return(NULL)
  parts <- strsplit(qualified, "::")[[1]]
  if (length(parts) == 2) {
    getExportedValue(parts[1], parts[2])
  } else {
    get(qualified, mode = "function")
  }
}

extract_iterations <- function(res) {
  if (inherits(res, "error")) return(NA_real_)
  if (is.list(res) && "iterations" %in% names(res)) return(as.numeric(res$iterations))
  if (is.numeric(res) && "iterations" %in% names(res)) return(as.numeric(res["iterations"]))
  if (is.numeric(res) && "iter" %in% names(res)) return(as.numeric(res["iter"]))
  NA_real_
}

load_model_meta <- function(id) {
  meta <- jsonlite::fromJSON("metadata/models.json")
  models <- meta$models
  match <- models[models$id == id, ]
  if (nrow(match) == 0) stop(sprintf("Model id '%s' not found in metadata/models.json", id))
  match
}

mk_generator <- function(model_row) {
  sampler <- model_row$sampler
  if (is.null(sampler) || sampler == "") {
    # fallback: infer from density by swapping leading 'd' -> 'r'
    dens <- model_row$density
    if (is.null(dens) || dens == "") stop("No sampler or density provided in metadata")
    sampler <- sub("::d", "::r", dens)
  }
  fn <- resolve_fun(sampler)
  if (is.null(fn)) stop(sprintf("Sampler '%s' not found", sampler))
  params <- model_row$params[[1]]
  defaults <- params$default
  names(defaults) <- params$name
  function(n) {
    do.call(fn, c(list(n = n), as.list(defaults)))
  }
}

bench_candidates <- function(funcs, generator, sizes, reps, tol, maxit) {
  seeds <- seq_along(sizes)
  rows <- tibble(size = sizes, seed = seeds)

  run_once <- function(func_name, x) {
    fn <- get(func_name, envir = asNamespace("univariateML"))
    tryCatch(fn(x = x, tol = tol, maxit = maxit),
             error = function(e) e,
             warning = function(w) w)
  }

  rows %>%
    mutate(
      data = map2(size, seed, ~ {
        set.seed(.y)
        generator(.x)
      }),
      bench = map(data, ~ bench::mark(
        !!!setNames(
          lapply(funcs, function(f) {
            function() run_once(f, .x)
          }),
          funcs
        ),
        iterations = reps,
        check = FALSE
      ))
    ) %>%
    select(size, bench) %>%
    tidyr::unnest(bench) %>%
    mutate(
      iter = map_dbl(result, extract_iterations),
      error = map_chr(result, ~ if (inherits(.x, "error")) conditionMessage(.x) else NA_character_)
    )
}

model_row <- load_model_meta(model_id)
gen <- mk_generator(model_row)

detect_funcs <- function(model_id) {
  ns <- asNamespace("univariateML")
  base <- paste0("ml", model_id, "_rcpp")
  cands <- ls(ns, pattern = paste0("^", base, "(_.*)?$"))
  if (length(cands) == 0) stop(sprintf("No Rcpp functions found matching %s in univariateML namespace", base))
  cands
}

funcs <- detect_funcs(model_id)

tol_default <- 1.220703e-4
maxit_default <- 100

res <- bench_candidates(
  funcs = funcs,
  generator = gen,
  sizes = sizes,
  reps = reps,
  tol = tol_default,
  maxit = maxit_default
)

if (!is.na(out_path)) {
  readr::write_csv(res, out_path)
} else {
  print(res %>% select(expression, size, median, iter, error, mem_alloc))
}
