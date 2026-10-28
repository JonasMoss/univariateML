context("mlgompertz errors")

test_that("mlgompertz errors on overdispersed data", {
  expect_error(
    mlgompertz(c(0.1, 0.2, 5)),
    "The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed\\. The exponential distribution will yield a better fit\\."
  )
})
