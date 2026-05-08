context("mlzipf")

## Warning when all observations are 1 (MLE not unique)
expect_warning(
  mlzipf(c(1, 1, 1)),
  "All observations are equal to 1\\. The maximum likelihood estimator is not unique\\."
)
