context("mlpois")

## Data generation.
set.seed(313)
small_data <- rpois(100, 20)
tiny_data <- rpois(10, 10)

## Finds errors with na and data out of bounds.
expect_error(mlpois(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlpois(small_data)),
  coef(mlpois(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlpois(small_data, na.rm = TRUE)
expect_equal(
  sum(dpois(small_data, est['lambda'], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Poisson")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support")), "numeric")
