#!/usr/bin/env Rscript

# Generic benchmarking harness for Rcpp estimator variants (console output only).
# Usage:
#   Rscript scripts/bench_cpp.R --model burr --sizes 50,200,1000 --reps 5

suppressMessages({
  library(microbenchmark)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(jsonlite)
  library(univariateML)
  library(devtools)
})

devtools::load_all(Sys.getenv("UMVL_PKG_DIR", "package"), quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) {
    return(default)
  }
  if (idx == length(args)) stop(sprintf("Missing value for %s", flag))
  args[idx + 1]
}

strip_prefix <- function(val, flag) {
  if (is.null(val) || is.na(val)) return(val)
  sub(paste0("^", flag, "=?"), "", val)
}

model_id <- strip_prefix(get_arg("--model", "burr"), "model")
sizes <- as.numeric(strsplit(strip_prefix(get_arg("--sizes", "50,200,1000"), "sizes"), ",")[[1]])
reps <- as.integer(strip_prefix(get_arg("--reps", "5"), "reps"))

resolve_fun <- function(qualified) {
  if (is.null(qualified) || is.na(qualified) || qualified == "") {
    return(NULL)
  }
  parts <- strsplit(qualified, "::")[[1]]
  if (length(parts) == 2) {
    getExportedValue(parts[1], parts[2])
  } else {
    get(qualified, mode = "function")
  }
}

extract_iterations <- function(res) {
  if (inherits(res, "error")) {
    return(NA_real_)
  }
  if (is.list(res) && "iterations" %in% names(res)) {
    return(as.numeric(res$iterations))
  }
  if (is.numeric(res) && "iterations" %in% names(res)) {
    return(as.numeric(res["iterations"]))
  }
  if (is.numeric(res) && "iter" %in% names(res)) {
    return(as.numeric(res["iter"]))
  }
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
  function(params, n) {
    do.call(fn, c(list(n = n), params))
  }
}

bench_candidates <- function(funcs, generator, param_sets, sizes, reps, tol, maxit) {
  rows <- tidyr::crossing(tibble(size = sizes),
                          tibble(param_id = seq_along(param_sets))) %>%
    mutate(seed = row_number(),
           params = map(param_id, ~ param_sets[[.x]]))

  run_once <- function(func_name, x) {
    fn <- get(func_name, envir = asNamespace("univariateML"))
    tryCatch(fn(x = x, tol = tol, maxit = maxit),
             error = function(e) e,
             warning = function(w) w)
  }

  rows %>%
    mutate(
      data = pmap(list(size = size, seed = seed, params = params), function(size, seed, params) {
        set.seed(seed)
        generator(params, size)
      }),
      preflight = map(data, ~ {
        map(funcs, function(f) {
          res <- run_once(f, .x)
          tibble(func = f,
                 ok = !inherits(res, "error"),
                 iter = extract_iterations(res),
                 error = if (inherits(res, "error")) conditionMessage(res) else NA_character_)
        }) %>% bind_rows()
      }),
      bench = map2(data, preflight, ~ {
        ok_funcs <- .y %>% filter(ok) %>% pull(func)
        if (length(ok_funcs) == 0) return(NULL)
        exprs <- setNames(lapply(ok_funcs, function(f) {
          bquote(run_once(.(f), .x))
        }), ok_funcs)
        do.call(microbenchmark::microbenchmark,
                c(exprs, times = reps, unit = "ms"))
      })
    ) %>%
    select(size, param_id, preflight, bench) %>%
    mutate(summary = map2(bench, preflight, ~ {
      if (is.null(.x)) return(NULL)
      s <- summary(.x)
      s$expression <- as.character(s$expr)
      s$iter <- .y$iter[match(s$expression, .y$func)]
      s$error <- .y$error[match(s$expression, .y$func)]
      s
    })) %>%
    select(size, param_id, summary) %>%
    tidyr::unnest(summary)
}

model_row <- load_model_meta(model_id)
gen <- mk_generator(model_row)

param_sets <- {
  base <- model_row$params[[1]]
  defaults <- setNames(as.list(base$default), base$name)
  bp <- model_row$bench_params[[1]]
  if (is.null(bp) || length(bp) == 0) {
    list(defaults)
  } else {
    lapply(seq_len(nrow(bp)), function(i) as.list(bp[i, ]))
  }
}
params_df <- dplyr::bind_rows(
  lapply(seq_along(param_sets), function(i) {
    tibble::as_tibble_row(param_sets[[i]]) %>%
      mutate(param_id = i, .before = 1)
  })
)
params_df$param_label <- params_df %>%
  select(-param_id) %>%
  pmap_chr(function(...) {
    vals <- list(...)
    paste(paste(names(vals), signif(unlist(vals), 4), sep = "="), collapse = ", ")
  })

detect_funcs <- function(model_id) {
  ns <- asNamespace("univariateML")
  prefix <- paste0("ml", model_id, "_")
  patt <- paste0("^(", prefix, "rcpp.*|", prefix, "cpp.*)$")
  cands <- ls(ns, pattern = patt)
  if (length(cands) == 0) stop(sprintf("No Rcpp functions found matching %s in univariateML namespace", prefix))
  cands
}

funcs <- detect_funcs(model_id)

tol_default <- 1.220703e-4
maxit_default <- 100

res <- bench_candidates(
  funcs = funcs,
  generator = gen,
  param_sets = param_sets,
  sizes = sizes,
  reps = reps,
  tol = tol_default,
  maxit = maxit_default
)

if (nrow(res) == 0) {
  stop("No successful benchmarks to report.")
}

out <- res %>%
  select(expression, size, param_id, mean, median, lq, uq, iter, error) %>%
  left_join(params_df, by = "param_id") %>%
  arrange(param_id, size, expression)

cat("\nDetailed timings (ms):\n")
print(out %>% select(expression, size, param_label, mean, iter, error))

cat("\nSummary by parameter set:\n")
print(
  out %>%
    group_by(expression, param_label) %>%
    summarise(mean_mean = mean(mean), mean_iter = mean(iter, na.rm = TRUE), .groups = "drop") %>%
    arrange(param_label, expression)
)

cat("\nOverall summary (across parameter sets and sizes):\n")
print(
  out %>%
    group_by(expression) %>%
    summarise(mean_mean = mean(mean), mean_iter = mean(iter, na.rm = TRUE), .groups = "drop") %>%
    arrange(expression)
)
