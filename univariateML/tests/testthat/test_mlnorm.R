context("mlnorm")

## Data generation.
set.seed(313)
small_data <- rnorm(100, 2, 3)
tiny_data <- rnorm(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mlnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlnorm(small_data)),
  coef(mlnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(dnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Normal")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")
