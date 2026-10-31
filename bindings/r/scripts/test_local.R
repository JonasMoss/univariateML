# Run from the generated package directory, after `just regen`.
# The environment flag reaches testthat subprocesses as well as this process.
options(pkg.build_extra_flags = FALSE)
Sys.setenv(PKG_BUILD_EXTRA_FLAGS = "false")
elapsed <- system.time(results <- testthat::test_local(".", stop_on_failure = TRUE))
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

