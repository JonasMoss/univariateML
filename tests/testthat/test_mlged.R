context("mlged")

## Data generation.
set.seed(313)
small_data <- fGarch::rged(100, 2, 3, 4)
tiny_data <- fGarch::rged(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlged(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlged(small_data)),
  coef(mlged(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlged(small_data, na.rm = TRUE)
expect_equal(
  sum(fGarch::dged(small_data, est[1], est[2], est[3], log = TRUE)),
  attr(est, "logLik")
)

## Are the fallback methods working correctly?
expect_equal(
  log(dml(small_data, est)),
  dml(small_data, est, log = TRUE)
)
expect_equal(
  log(pml(small_data, est)),
  pml(small_data, est, log.p = TRUE)
)
expect_equal(
  1 - pml(small_data, est),
  pml(small_data, est, lower.tail = FALSE)
)
expect_equal(
  qml(1:3 / 4, est),
  qml(log(1:3 / 4), est, log.p = TRUE)
)
expect_equal(
  qml(1 - 1:3 / 4, est),
  qml(1:3 / 4, est, lower.tail = FALSE)
)


## Check class.
expect_equal(attr(est, "model"), "Generalized Error")
expect_equal(class(est), "univariateML")



## Check support.
expect_equal(class(attr(est, "support"))[[1]], "Intervals")
