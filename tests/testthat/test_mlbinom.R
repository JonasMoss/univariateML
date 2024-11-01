context("mlbinom")

## Data generation.
set.seed(313)
small_data <- rbinom(100, 20, 0.5)
tiny_data <- rbinom(10, 10, 0.1)
poiss_data <- seq(20)

## Finds errors with na and data out of bounds.
expect_error(mlbinom(c(tiny_data, NA)))

## Expect error when fed with overdispersed data
expect_error(mlbinom(poiss_data))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlbinom(small_data)),
  coef(mlbinom(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlbinom(small_data, na.rm = TRUE)
expect_equal(
  sum(dbinom(small_data, est["size"],est["prob"], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Binomial")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")
