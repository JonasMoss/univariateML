context("mllogitnorm")

## Data generation.
set.seed(313)
small_data <- plogis(rnorm(100, 2, 3))
tiny_data <- plogis(rnorm(10, 0, 1))

## Finds errors with na and data out of bounds.
expect_error(mllogitnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllogitnorm(small_data)),
  coef(mllogitnorm(c(small_data, NA), na.rm = TRUE))
)

## Finds errors with na and data out of bounds.
expect_error(mllogitnorm(c(0, tiny_data)))
expect_error(mllogitnorm(c(tiny_data, 1)))

## Is the log-likelihood correct?
est <- mllogitnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(logitnorm::dlogitnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "LogitNormal")
expect_equal(class(est), c("univariateML"))


## Check support.
expect_equal(class(attr(est, "support")), "numeric")


