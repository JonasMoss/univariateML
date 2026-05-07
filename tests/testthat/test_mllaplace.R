context("mllaplace")

## Data generation.
set.seed(313)
small_data <- rlaplace(100, 2, 3)
tiny_data <- rlaplace(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mllaplace(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllaplace(small_data)),
  coef(mllaplace(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllaplace(small_data, na.rm = TRUE)
expect_equal(
  sum(dlaplace(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Laplace")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")
