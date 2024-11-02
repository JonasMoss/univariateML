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
