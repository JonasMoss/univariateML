context("mllnorm")

## Data generation.
set.seed(313)
small_data <- stats::rlnorm(100, 2, 3)
tiny_data <- stats::rlnorm(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mllnorm(c(tiny_data, NA)))
expect_error(mllnorm(c(tiny_data, -1)))
expect_error(mllnorm(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllnorm(small_data)),
  coef(mllnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(stats::dlnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Lognormal")
expect_equal(class(est), "univariateML")
