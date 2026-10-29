context("mlnorm")

## Data generation.
set.seed(313)
small_data <- rnorm(100, 2, 3)
tiny_data <- rnorm(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mlnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlnorm(small_data)),
  coef(mlnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(dnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Normal")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("norm handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(stats::rnorm(199), 1e3)
  objective <- function(p) -sum(stats::dnorm(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlnorm(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("constant data have no attained norm MLE", {
  expect_error(mlnorm(rep(2, 5)), "does not exist")
  expect_error(mlnorm(2), "does not exist")
})

test_that("normal moments retain tiny and huge scales and near ties", {
  for (s in c(1e-300, 1e300)) {
    x <- c(-2, -1, 0, 1, 2) * s
    f <- mlnorm(x)
    expect_equal(f[["sd"]] / s, sqrt(2))
    expect_equal(attr(f, "logLik"), sum(dnorm(x, f[[1]], f[[2]], log = TRUE)))
  }
  x <- 1 + (0:4)*.Machine$double.eps
  expect_equal(mlnorm(x)[["sd"]] / .Machine$double.eps, sqrt(2))
})
