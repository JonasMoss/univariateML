context("mlstd")

## Data generation.
set.seed(313)
small_data <- fGarch::rstd(500, 2, 3, 4)
tiny_data <- fGarch::rstd(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlstd(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlstd(small_data)),
  coef(mlstd(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlstd(small_data, na.rm = TRUE)
expect_equal(
  sum(fGarch::dstd(small_data, est[1], est[2], est[3], log = TRUE)),
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
expect_equal(attr(est, "model"), "Student-t")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("Student-t escapes name the limiting model", {
  set.seed(1)
  expect_error(mlstd(rnorm(200)), "Use `mlnorm`", fixed = TRUE)
  set.seed(313)
  expect_error(mlstd(fGarch::rstd(100, 2, 3, 4)),
               "nu -> 2 (infinite variance)", fixed = TRUE)
  x <- c(seq(-1e-4, 1e-4, length = 30),
         seq(0.9999, 1.0001, length = 10))
  expect_error(mlstd(x), "nu -> 2 (infinite variance)", fixed = TRUE)
})

test_that("Student-t interior fit and large-nu log likelihood agree with fGarch", {
  set.seed(313)
  x <- fGarch::rstd(500, nu = 4)
  fit <- mlstd(x)
  expect_equal(attr(fit, "logLik"),
               sum(fGarch::dstd(x, fit[1], fit[2], fit[3], log = TRUE)),
               tolerance = 1e-10)
  for (nu in c(1e4, 1e5, 1e6, 1e8)) {
    expect_equal(univariateML:::student_t_loglik_cpp(x, 0.3, 1.2, nu),
                 sum(fGarch::dstd(x, 0.3, 1.2, nu, log = TRUE)),
                 tolerance = 1e-10)
  }
})
