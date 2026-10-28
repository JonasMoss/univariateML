context("mlnbinom")

## Error when data is Poisson.
expect_error(
  mlnbinom(c(1, 1, 1, 1)),
  "The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead\\."
)

## Non-unique maximum likelihood.
expect_warning(mlnbinom(c(0, 0, 0)))


## Size argument works
expect_equal(unname(mlnbinom(seq(20), size = 20))[1], 20)

test_that("fractional fixed size is preserved", {
  x <- c(0, 1, 2, 5)
  fit <- mlnbinom(x, size = 2.5)
  expect_equal(fit[["size"]], 2.5)
  expect_equal(attr(fit, "logLik"), sum(dnbinom(x, size = 2.5, prob = fit[["prob"]], log = TRUE)))
})
