# Concatenate estimator source files from r-src into a generated R file.
# Run via: Rscript scripts/gen_estimators_from_rsrc.R

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to run this generator.")
}

meta <- jsonlite::fromJSON("metadata/models.json", simplifyDataFrame = FALSE)$models
generated_names <- vapply(meta, function(m) paste0("ml", m$id), character(1))

sources <- list.files("r-src", pattern = "\\.R$", full.names = TRUE)
sources <- sort(sources)

strip_metadata_blocks <- function(lines) {
  out <- character(0)
  skip <- FALSE
  for (ln in lines) {
    if (grepl("^univariateML_metadata", ln)) {
      skip <- TRUE
      next
    }
    if (skip) {
      if (grepl("^\\)", trimws(ln))) {
        skip <- FALSE
      }
      next
    }
    out <- c(out, ln)
  }
  out
}

parts <- unlist(lapply(sources, function(f) {
  lines <- readLines(f)
  lines <- strip_metadata_blocks(lines)
  c(lines, "")
}))

header <- c(
  "# This file is auto-generated from r-src via scripts/gen_estimators_from_rsrc.R",
  "# Do not edit by hand."
)

out <- c(header, "", parts)
writeLines(out, "R/generated_estimators.R")
