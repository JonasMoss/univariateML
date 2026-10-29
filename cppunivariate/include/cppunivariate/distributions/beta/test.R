context("mlbeta")

## Data generation.
set.seed(313)
small_data <- runif(100)
tiny_data <- rbeta(10, 2, 7)

## Checks if the ML is correct.
mle1 <- nlm(function(p) {
  -mean(dbeta(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(function(p) {
  -mean(dbeta(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

expect_equal(mle1, as.numeric(mlbeta(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlbeta(tiny_data)), tolerance = 1e-5)

## Finds errors with na and data out of bounds.
expect_error(mlbeta(c(0, tiny_data)))
expect_error(mlbeta(c(tiny_data, 1)))
expect_error(mlbeta(c(tiny_data, NA)))

test_that("constant data have no attained beta MLE", {
  expect_error(mlbeta(rep(.4, 5)), "does not exist")
  expect_error(mlbeta(.4), "does not exist")
})

test_that("beta fits data at both ends of the open support", {
  x <- c(rep(1e-10, 5), rep(1 - 1e-10, 5))
  f <- mlbeta(x)
  oracle <- optim(log(c(.05, .05)), function(p) -sum(dbeta(x, exp(p[1]), exp(p[2]), log = TRUE)), method = "BFGS", control = list(reltol = 1e-12))
  expect_equal(attr(f, "logLik"), -oracle$value, tolerance = 1e-8)
  expect_equal(attr(f, "logLik"), sum(dbeta(x, f[[1]], f[[2]], log = TRUE)), tolerance = 1e-10)
})
