context("mllgser")

## Finds error with only 1s.
expect_error(
  mllgser(c(1, 1, 1)),
  "All observations equals 1; the maximum likelihood estimator does not exist\\."
)
