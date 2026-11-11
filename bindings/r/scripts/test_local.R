# Run from the generated package directory, after `just regen`.
# The environment flag reaches testthat subprocesses as well as this process.
options(pkg.build_extra_flags = FALSE)
Sys.setenv(PKG_BUILD_EXTRA_FLAGS = "false")
families <- Sys.getenv("UMVL_TEST_FAMILIES")
filter <- NULL
if (nzchar(families)) {
  ids <- strsplit(families, ",", fixed = TRUE)[[1L]]
  available <- sub("^test-dist-(.*)\\.R$", "\\1",
                   list.files("tests/testthat", "^test-dist-.*\\.R$"))
  if (any(!ids %in% available)) stop("Unknown test families: ", paste(setdiff(ids, available), collapse = ","))
  shared <- c("conformance", "golden_regression", "generated-dpqr",
              "scale_equivariance", "frozen", "diagnostics", "audit-input",
              "input_checks", "restricted")
  filter <- paste0("^(", paste(c(paste0("dist-", ids), shared), collapse = "|"), ")$")
  message("R family filter: ", families, "; shared loops: ", paste(shared, collapse = ", "))
}
elapsed <- system.time(results <- testthat::test_local(".", filter = filter, stop_on_failure = TRUE))
rows <- as.data.frame(results)
# In parallel testthat, per-test time fields reflect parent event replay,
# not worker fit time. Use the external wall clock for parallel performance.
# The reporter also counts legacy top-level expectations absent from this table.
output <- Sys.getenv("UMVL_TEST_TIMINGS_FILE")
if (nzchar(output)) {
  write.csv(rows[, setdiff(names(rows), "result"), drop = FALSE], output,
            row.names = FALSE)
}
print(elapsed)

