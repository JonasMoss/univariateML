library(testthat)
library(univariateML)

results <- test_check("univariateML")
if (identical(Sys.getenv("UMVL_TEST_TIMINGS"), "true")) {
  timings <- aggregate(real ~ file, as.data.frame(results), sum)
  timings <- timings[order(-timings$real), ]
  write.csv(timings, "testthat-timings.csv", row.names = FALSE)
}
