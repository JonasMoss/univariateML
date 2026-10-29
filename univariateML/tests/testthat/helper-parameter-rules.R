golden_fit_args <- function(id) {
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  if (!nzchar(root)) testthat::skip("no distribution goldens")
  g <- jsonlite::fromJSON(file.path(root, sub("^ml", "", id), "golden.json"))
  lapply(g$fit_args, function(x) if(is.null(x)) NULL else as.numeric(x))
}
