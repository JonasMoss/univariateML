#!/usr/bin/env Rscript

suppressMessages({
  library(bench)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

bench_candidates <- function(funcs,
                             data_grid,
                             reps = 5,
                             control = list()) {
  rows <- expand.grid(data_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  run_once <- function(func_name, x, tol = NULL, maxit = NULL) {
    fn <- get(func_name, envir = asNamespace("univariateML"))
    args <- list(x = x)
    if (!is.null(tol)) args$tol <- tol
    if (!is.null(maxit)) args$maxit <- maxit
    res <- tryCatch(fn, error = function(e) e, warning = function(w) w)
    res
  }

  rows %>%
    mutate(seed = seq_len(n())) %>%
    rowwise() %>%
    mutate(
      x = {
        set.seed(seed)
        generator()
      },
      results = list(
        bench::mark(
          !!!setNames(
            lapply(funcs, function(f) {
              function() run_once(f, x, tol = control$tol, maxit = control$maxit)
            }),
            funcs
          ),
          iterations = reps,
          check = FALSE
        )
      )
    ) %>%
    ungroup() %>%
    tidyr::unnest(results)
}

if (interactive()) {
  # Example usage: compare burr variants on simple lognormal data
  data_grid <- list(
    generator = list(
      function() exp(rnorm(50)),
      function() exp(rnorm(500))
    )
  )
  res <- bench_candidates(
    funcs = c("mlburr_rcpp", "mlburr_cpp"),
    data_grid = data_grid,
    reps = 3,
    control = list(tol = 1.220703e-4, maxit = 100)
  )
  print(res)
}
