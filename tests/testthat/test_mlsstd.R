context("mlsstd")

## Data generation.
set.seed(313)
small_data <- fGarch::rsstd(100)
tiny_data <- fGarch::rsstd(100)

## Finds errors with na and data out of bounds.
expect_error(mlsstd(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsstd(small_data)),
  coef(mlsstd(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsstd(small_data, na.rm = TRUE)
expect_equal(
  sum(fGarch::dsstd(small_data, est[1], est[2], est[3], est[4], log = TRUE)),
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
expect_equal(attr(est, "model"), "Skew Student-t")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "Intervals")
