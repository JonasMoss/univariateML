context("mlrayleigh")

## Data generation.
set.seed(313)
small_data <- extraDistr::rrayleigh(100, 2)
tiny_data <- extraDistr::rrayleigh(10, 3)

## Finds errors with na and data out of bounds.
expect_error(mlrayleigh(c(-.1, tiny_data)))
expect_error(mlrayleigh(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlrayleigh(small_data)),
  coef(mlrayleigh(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlrayleigh(small_data, na.rm = TRUE)
expect_equal(
  sum(extraDistr::drayleigh(small_data, est, log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Rayleigh")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("Rayleigh fits extreme scales and rejects all zero", {
  for (s in c(1e-300, 1e300)) {
    f <- mlrayleigh(c(1, 2, 3) * s)
    expect_equal(f[["sigma"]] / s, sqrt(7/3))
    expect_true(is.finite(attr(f, "logLik")))
  }
  expect_error(mlrayleigh(rep(0, 5)), "identically zero")
})

test_that("any zero makes the Rayleigh likelihood zero", {
  for (x in list(c(0,0,1,2), c(0,1), c(0,0)))
    expect_error(mlrayleigh(x), "identically zero")
  for (x in list(c(.1, .2, 1), c(1, 2, 5))) {
    fit <- mlrayleigh(x)
    oracle <- optimize(function(s) -sum(extraDistr::drayleigh(x,s,log=TRUE)), c(.01,10))
    expect_equal(as.numeric(logLik(fit)), -oracle$objective, tolerance=1e-8)
  }
})
