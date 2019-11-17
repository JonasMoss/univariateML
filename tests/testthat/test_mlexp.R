context("mlexp")

## Data generation.
set.seed(313)
small_data <- rexp(100, 2)
tiny_data <- rexp(10, 3)

## Finds errors with na and data out of bounds.
expect_error(mlexp(c(-.1, tiny_data)))
expect_error(mlexp(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlexp(small_data)),
  coef(mlexp(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlexp(small_data, na.rm = TRUE)
expect_equal(
  sum(dexp(small_data, est, log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Exponential")
expect_equal(class(est), "univariateML")
