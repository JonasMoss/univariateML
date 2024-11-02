context("mlzipf")

## Warning when ml is not unique
expect_warning(mlzipf(c(1, 1, 1)))
