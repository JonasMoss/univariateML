context("mlt")

## Data generation.
set.seed(313)
small_data <- fGarch::rstd(100, 2, 3, 4)
tiny_data <- fGarch::rstd(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlt(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  suppressWarnings(coef(mlt(small_data))),
  suppressWarnings(coef(mlt(c(small_data, NA), na.rm = TRUE)))
)

## Is the log-likelihood correct?
est <- suppressWarnings(mlt(small_data, na.rm = TRUE))
expect_equal(
  sum(fGarch::dstd(small_data, est[1], est[2], est[3], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Student t")
expect_equal(class(est), "univariateML")
