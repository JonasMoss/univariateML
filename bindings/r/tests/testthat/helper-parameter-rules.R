golden_fit_args <- local({
  # Each worker reads immutable fixtures. Held-subset checks reuse these
  # arguments while copying them into their own argument lists.
  cache <- new.env(parent = emptyenv())
  function(id) {
    root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
    if (!nzchar(root)) testthat::skip("no distribution goldens")
    path <- file.path(root, sub("^ml", "", id), "golden.json")
    if (!exists(path, envir = cache, inherits = FALSE)) {
      g <- jsonlite::fromJSON(path)
      cache[[path]] <- lapply(g$fit_args, function(x) if (is.null(x)) NULL else as.numeric(x))
    }
    cache[[path]]
  }
})
