context("mlsged")

## Data generation.
set.seed(313)
small_data <- fGarch::rsged(100, 2, 3, 4)
tiny_data <- fGarch::rsged(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlsged(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsged(small_data)),
  coef(mlsged(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsged(small_data, na.rm = TRUE)
expect_equal(
  sum(fGarch::dsged(small_data, est[1], est[2], est[3], est[4], log = TRUE)),
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
expect_equal(attr(est, "model"), "Skew Generalized Error")
expect_equal(class(est), "univariateML")
