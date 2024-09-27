context("mllgamma")

## Data generation.
set.seed(313)
small_data <- actuar::rlgamma(100, 2, 3)
tiny_data <- actuar::rlgamma(10, 2, 1)

## Finds errors with na and data out of bounds.
expect_error(mllgamma(c(tiny_data, NA)))
expect_error(mllgamma(c(tiny_data, 0)))
expect_error(mllgamma(c(tiny_data, -1)))

# Check correctness
expect_equal(
  unname(mlgamma(log(small_data))[1]),
  unname(mllgamma(small_data)[1])
)


## Checks that na.rm works as intended.
expect_equal(
  coef(mllgamma(small_data)),
  coef(mllgamma(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllgamma(small_data, na.rm = TRUE)
expect_equal(
  sum(actuar::dlgamma(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Loggamma")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shapelog", "ratelog"))

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "Intervals")
