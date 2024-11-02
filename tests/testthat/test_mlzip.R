context("mlzip")

## Warning when ml is not unique
expect_warning(mlzip(c(0, 0, 0)))

set.seed(1)
expect_equal(unname(mlzip(rpois(10, 1))[2]), 0)
