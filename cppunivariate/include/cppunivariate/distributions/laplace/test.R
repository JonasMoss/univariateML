context("mllaplace")

## Data generation.
set.seed(313)
small_data <- extraDistr::rlaplace(100, 2, 3)
tiny_data <- extraDistr::rlaplace(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mllaplace(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllaplace(small_data)),
  coef(mllaplace(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllaplace(small_data, na.rm = TRUE)
expect_equal(
  sum(extraDistr::dlaplace(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Laplace")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("laplace handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(extraDistr::rlaplace(199), 1e3)
  objective <- function(p) -sum(extraDistr::dlaplace(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  # For even n the location MLE is an interval; choose its upper endpoint.
  location <- sort(x)[length(x) / 2 + 1]
  expected <- c(location, mean(abs(x - location)))
  fit <- mllaplace(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})
