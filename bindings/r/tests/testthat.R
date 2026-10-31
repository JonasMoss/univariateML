library(testthat)
library(univariateML)

# R CMD check is serial unless explicitly requested by its caller.
if (!nzchar(Sys.getenv("TESTTHAT_PARALLEL"))) Sys.setenv(TESTTHAT_PARALLEL = "false")
results <- test_check("univariateML")
if (identical(Sys.getenv("UMVL_TEST_TIMINGS"), "true")) {
  timings <- aggregate(real ~ file, as.data.frame(results), sum)
  timings <- timings[order(-timings$real), ]
  write.csv(timings, "testthat-timings.csv", row.names = FALSE)
}
